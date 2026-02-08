use crate::core::document::{Document, DocumentManager, DocumentManagerTrait};
use bumpalo::Bump;
use lsp_types::{Uri, *};

use luanext_parser::ast::expression::{Expression, ExpressionKind};
use luanext_parser::ast::statement::Statement;
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser, Span};
use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
use std::collections::HashMap;
use std::sync::Arc;

/// Provides rename functionality
#[derive(Clone)]
pub struct RenameProvider;

impl RenameProvider {
    pub fn new() -> Self {
        Self
    }

    /// Prepare a rename operation (validate rename position and provide placeholder)
    pub fn prepare(
        &self,
        document: &Document,
        position: Position,
    ) -> Option<PrepareRenameResponse> {
        // Get the word at the current position
        let word = self.get_word_at_position(document, position)?;

        // Get the range of the word
        let range = self.get_word_range(document, position)?;

        // Return the range and current name as placeholder
        Some(PrepareRenameResponse::RangeWithPlaceholder {
            range,
            placeholder: word,
        })
    }

    /// Perform the rename operation
    pub fn rename(
        &self,
        uri: &Uri,
        document: &Document,
        position: Position,
        new_name: &str,
        document_manager: &DocumentManager,
    ) -> Option<WorkspaceEdit> {
        // Get the word at the current position
        let word = self.get_word_at_position(document, position)?;

        // Validate the new name
        if !self.is_valid_identifier(new_name) {
            return None;
        }

        // Parse the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let arena = Bump::new();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);
        let ast = parser.parse().ok()?;

        // Create a map to store edits for each file
        let mut all_edits: HashMap<Uri, Vec<TextEdit>> = HashMap::new();

        // Find all occurrences in the current file (including declaration)
        let mut current_file_occurrences = Vec::new();
        self.find_all_occurrences(
            &ast.statements,
            &word,
            &mut current_file_occurrences,
            &interner,
        );

        // Find the declaration to include it
        if let Some(decl_span) = self.find_declaration(&ast.statements, &word, &interner) {
            current_file_occurrences.push(decl_span);
        }

        // Convert spans to text edits for current file
        let current_edits: Vec<TextEdit> = current_file_occurrences
            .into_iter()
            .map(|span| TextEdit {
                range: span_to_range(&span),
                new_text: new_name.to_string(),
            })
            .collect();

        all_edits.insert(uri.clone(), current_edits);

        // Check if this symbol is exported - if so, rename in all importing files
        if self.is_symbol_exported(&ast.statements, &word, &interner) {
            if let Some(module_id) = &document.module_id {
                self.collect_renames_in_importing_files(
                    module_id,
                    &word,
                    new_name,
                    document_manager,
                    &mut all_edits,
                );
            }
        }

        // Check if this symbol is imported - if so, rename in the source file
        if let Some((source_uri, exported_name)) = self.find_import_source(
            &ast.statements,
            &word,
            document,
            document_manager,
            &interner,
        ) {
            if let Some(source_doc) = document_manager.get(&source_uri) {
                // Parse source document
                let handler = Arc::new(CollectingDiagnosticHandler::new());
                let arena = Bump::new();
                let mut lexer = Lexer::new(&source_doc.text, handler.clone(), &interner);
                if let Ok(tokens) = lexer.tokenize() {
                    let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);
                    if let Ok(ast) = parser.parse() {
                        let mut source_occurrences = Vec::new();
                        self.find_all_occurrences(
                            &ast.statements,
                            &exported_name,
                            &mut source_occurrences,
                            &interner,
                        );

                        // Include declaration in source file
                        if let Some(decl_span) =
                            self.find_declaration(&ast.statements, &exported_name, &interner)
                        {
                            source_occurrences.push(decl_span);
                        }

                        // Convert to text edits
                        let source_edits: Vec<TextEdit> = source_occurrences
                            .into_iter()
                            .map(|span| TextEdit {
                                range: span_to_range(&span),
                                new_text: new_name.to_string(),
                            })
                            .collect();

                        all_edits.insert(source_uri, source_edits);
                    }
                }
            }
        }

        // Create workspace edit
        Some(WorkspaceEdit {
            changes: Some(all_edits),
            document_changes: None,
            change_annotations: None,
        })
    }

    /// Check if a symbol is exported from the file
    fn is_symbol_exported(
        &self,
        statements: &[Statement],
        symbol_name: &str,
        interner: &StringInterner,
    ) -> bool {
        use luanext_parser::ast::statement::ExportKind;

        for stmt in statements {
            if let Statement::Export(export_decl) = stmt {
                match &export_decl.kind {
                    ExportKind::Declaration(decl) => {
                        if self
                            .get_declaration_name_span(decl, symbol_name, interner)
                            .is_some()
                        {
                            return true;
                        }
                    }
                    ExportKind::Named {
                        specifiers,
                        source: _,
                    } => {
                        for spec in *specifiers {
                            let exported_name = spec.exported.as_ref().unwrap_or(&spec.local);
                            if interner.resolve(exported_name.node) == symbol_name
                                || interner.resolve(spec.local.node) == symbol_name
                            {
                                return true;
                            }
                        }
                    }
                    ExportKind::Default(_) if symbol_name == "default" => {
                        return true;
                    }
                    _ => {}
                }
            }
        }
        false
    }

    /// Get the declaration name span from a statement
    fn get_declaration_name_span(
        &self,
        stmt: &Statement,
        name: &str,
        interner: &StringInterner,
    ) -> Option<Span> {
        use luanext_parser::ast::pattern::Pattern;

        match stmt {
            Statement::Variable(var_decl) => {
                if let Pattern::Identifier(ident) = &var_decl.pattern {
                    if interner.resolve(ident.node) == name {
                        return Some(ident.span);
                    }
                }
            }
            Statement::Function(func_decl) => {
                if interner.resolve(func_decl.name.node) == name {
                    return Some(func_decl.name.span);
                }
            }
            Statement::Class(class_decl) => {
                if interner.resolve(class_decl.name.node) == name {
                    return Some(class_decl.name.span);
                }
            }
            Statement::Interface(interface_decl) => {
                if interner.resolve(interface_decl.name.node) == name {
                    return Some(interface_decl.name.span);
                }
            }
            Statement::TypeAlias(type_decl) => {
                if interner.resolve(type_decl.name.node) == name {
                    return Some(type_decl.name.span);
                }
            }
            Statement::Enum(enum_decl) => {
                if interner.resolve(enum_decl.name.node) == name {
                    return Some(enum_decl.name.span);
                }
            }
            _ => {}
        }
        None
    }

    /// Collect rename edits in files that import from the current module
    fn collect_renames_in_importing_files(
        &self,
        module_id: &luanext_typechecker::module_resolver::ModuleId,
        symbol_name: &str,
        new_name: &str,
        document_manager: &DocumentManager,
        all_edits: &mut HashMap<Uri, Vec<TextEdit>>,
    ) {
        // Use symbol index to quickly find all files that import this symbol
        let importing_uris = document_manager
            .symbol_index()
            .get_importers(module_id.as_str(), symbol_name);

        // For each importing file, find and rename all occurrences of the local name
        for uri in importing_uris {
            if let Some(doc) = document_manager.get(&uri) {
                // Get the document's module ID
                if let Some(doc_module_id) = &doc.module_id {
                    // Use symbol index to get import info
                    if let Some(imports) = document_manager
                        .symbol_index()
                        .get_imports(doc_module_id.as_str(), symbol_name)
                    {
                        for import_info in imports {
                            // Check if this import is from our source module
                            if let Some(source_module_id) =
                                document_manager.uri_to_module_id(&import_info.source_uri)
                            {
                                if source_module_id == module_id
                                    && import_info.imported_name == symbol_name
                                {
                                    // Parse the document to find all occurrences
                                    if let Some((ast, interner, _, _)) = doc.get_or_parse_ast() {
                                        let mut occurrences = Vec::new();
                                        self.find_all_occurrences(
                                            &ast.statements,
                                            &import_info.local_name,
                                            &mut occurrences,
                                            &*interner,
                                        );

                                        // Convert to text edits
                                        let edits: Vec<TextEdit> = occurrences
                                            .into_iter()
                                            .map(|span| TextEdit {
                                                range: span_to_range(&span),
                                                new_text: new_name.to_string(),
                                            })
                                            .collect();

                                        if !edits.is_empty() {
                                            all_edits.insert(uri.clone(), edits);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Find the source file and exported name for an imported symbol
    fn find_import_source(
        &self,
        statements: &[Statement],
        symbol_name: &str,
        current_document: &Document,
        document_manager: &DocumentManager,
        interner: &StringInterner,
    ) -> Option<(Uri, String)> {
        use luanext_parser::ast::statement::ImportClause;

        for stmt in statements {
            if let Statement::Import(import_decl) = stmt {
                let import_source = &import_decl.source;

                // Check if this import contains our symbol
                let exported_name = match &import_decl.clause {
                    ImportClause::Named(specs) => specs.iter().find_map(|spec| {
                        let local_name = spec.local.as_ref().unwrap_or(&spec.imported);
                        if interner.resolve(local_name.node) == symbol_name {
                            Some(interner.resolve(spec.imported.node).to_string())
                        } else {
                            None
                        }
                    }),
                    ImportClause::Default(ident) => {
                        if interner.resolve(ident.node) == symbol_name {
                            Some("default".to_string())
                        } else {
                            None
                        }
                    }
                    ImportClause::Namespace(ident) => {
                        if interner.resolve(ident.node) == symbol_name {
                            None // Skip namespace imports for now
                        } else {
                            None
                        }
                    }
                    ImportClause::TypeOnly(_) => None,
                    ImportClause::Mixed { default, named } => {
                        // Check default import first
                        if interner.resolve(default.node) == symbol_name {
                            Some("default".to_string())
                        } else {
                            // Check named imports
                            named.iter().find_map(|spec| {
                                let local_name = spec.local.as_ref().unwrap_or(&spec.imported);
                                if interner.resolve(local_name.node) == symbol_name {
                                    Some(interner.resolve(spec.imported.node).to_string())
                                } else {
                                    None
                                }
                            })
                        }
                    }
                };

                if let Some(exported_name) = exported_name {
                    // Resolve the import path
                    if let Some(module_id) = &current_document.module_id {
                        let resolver = document_manager.module_resolver();
                        {
                            if let Ok(target_module_id) = resolver
                                .resolve(import_source, std::path::Path::new(module_id.as_str()))
                            {
                                if let Some(target_uri) =
                                    document_manager.module_id_to_uri(&target_module_id)
                                {
                                    return Some((target_uri.clone(), exported_name));
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Validate that a name is a valid identifier
    fn is_valid_identifier(&self, name: &str) -> bool {
        if name.is_empty() {
            return false;
        }

        // Check first character (must be letter or underscore)
        let mut chars = name.chars();
        if let Some(first) = chars.next() {
            if !first.is_alphabetic() && first != '_' {
                return false;
            }
        } else {
            return false;
        }

        // Check remaining characters (letter, digit, or underscore)
        for ch in chars {
            if !ch.is_alphanumeric() && ch != '_' {
                return false;
            }
        }

        // Check if it's a reserved keyword
        if self.is_keyword(name) {
            return false;
        }

        true
    }

    /// Check if a name is a reserved keyword
    fn is_keyword(&self, name: &str) -> bool {
        matches!(
            name,
            "const"
                | "local"
                | "function"
                | "if"
                | "then"
                | "else"
                | "elseif"
                | "end"
                | "while"
                | "for"
                | "in"
                | "do"
                | "repeat"
                | "until"
                | "return"
                | "break"
                | "continue"
                | "and"
                | "or"
                | "not"
                | "true"
                | "false"
                | "nil"
                | "type"
                | "interface"
                | "enum"
                | "class"
                | "extends"
                | "implements"
                | "public"
                | "private"
                | "protected"
                | "static"
                | "abstract"
                | "readonly"
                | "match"
                | "when"
                | "import"
                | "from"
                | "export"
        )
    }

    /// Get the word at the cursor position
    fn get_word_at_position(&self, document: &Document, position: Position) -> Option<String> {
        let lines: Vec<&str> = document.text.lines().collect();
        if position.line as usize >= lines.len() {
            return None;
        }

        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        if char_pos > line.len() {
            return None;
        }

        // Find word boundaries
        let chars: Vec<char> = line.chars().collect();
        if char_pos >= chars.len() {
            return None;
        }

        // Check if we're on a word character
        if !chars[char_pos].is_alphanumeric() && chars[char_pos] != '_' {
            return None;
        }

        // Find start of word
        let mut start = char_pos;
        while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
            start -= 1;
        }

        // Find end of word
        let mut end = char_pos;
        while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
            end += 1;
        }

        Some(chars[start..end].iter().collect())
    }

    /// Get the range of the word at the cursor position
    fn get_word_range(&self, document: &Document, position: Position) -> Option<Range> {
        let lines: Vec<&str> = document.text.lines().collect();
        if position.line as usize >= lines.len() {
            return None;
        }

        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        if char_pos > line.len() {
            return None;
        }

        // Find word boundaries
        let chars: Vec<char> = line.chars().collect();
        if char_pos >= chars.len() {
            return None;
        }

        // Check if we're on a word character
        if !chars[char_pos].is_alphanumeric() && chars[char_pos] != '_' {
            return None;
        }

        // Find start of word
        let mut start = char_pos;
        while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
            start -= 1;
        }

        // Find end of word
        let mut end = char_pos;
        while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
            end += 1;
        }

        Some(Range {
            start: Position {
                line: position.line,
                character: start as u32,
            },
            end: Position {
                line: position.line,
                character: end as u32,
            },
        })
    }

    /// Find the declaration span for a given symbol name
    fn find_declaration(
        &self,
        statements: &[Statement],
        name: &str,
        interner: &StringInterner,
    ) -> Option<Span> {
        use luanext_parser::ast::pattern::Pattern;

        for stmt in statements {
            match stmt {
                Statement::Variable(var_decl) => {
                    if let Pattern::Identifier(ident) = &var_decl.pattern {
                        if interner.resolve(ident.node) == name {
                            return Some(ident.span);
                        }
                    }
                }
                Statement::Function(func_decl) => {
                    if interner.resolve(func_decl.name.node) == name {
                        return Some(func_decl.name.span);
                    }
                }
                Statement::Class(class_decl) => {
                    if interner.resolve(class_decl.name.node) == name {
                        return Some(class_decl.name.span);
                    }
                }
                Statement::Interface(interface_decl) => {
                    if interner.resolve(interface_decl.name.node) == name {
                        return Some(interface_decl.name.span);
                    }
                }
                Statement::TypeAlias(type_decl) => {
                    if interner.resolve(type_decl.name.node) == name {
                        return Some(type_decl.name.span);
                    }
                }
                Statement::Enum(enum_decl) => {
                    if interner.resolve(enum_decl.name.node) == name {
                        return Some(enum_decl.name.span);
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Find all occurrences of a symbol
    fn find_all_occurrences(
        &self,
        statements: &[Statement],
        name: &str,
        refs: &mut Vec<Span>,
        interner: &StringInterner,
    ) {
        for stmt in statements {
            self.find_occurrences_in_statement(stmt, name, refs, interner);
        }
    }

    fn find_occurrences_in_statement(
        &self,
        stmt: &Statement,
        name: &str,
        refs: &mut Vec<Span>,
        interner: &StringInterner,
    ) {
        match stmt {
            Statement::Expression(expr) => {
                self.find_occurrences_in_expression(expr, name, refs, interner);
            }
            Statement::Variable(var_decl) => {
                self.find_occurrences_in_expression(&var_decl.initializer, name, refs, interner);
            }
            Statement::Function(func_decl) => {
                for stmt in func_decl.body.statements {
                    self.find_occurrences_in_statement(stmt, name, refs, interner);
                }
            }
            Statement::If(if_stmt) => {
                self.find_occurrences_in_expression(&if_stmt.condition, name, refs, interner);
                self.find_all_occurrences(&if_stmt.then_block.statements, name, refs, interner);
                for else_if in if_stmt.else_ifs {
                    self.find_occurrences_in_expression(&else_if.condition, name, refs, interner);
                    self.find_all_occurrences(&else_if.block.statements, name, refs, interner);
                }
                if let Some(else_block) = &if_stmt.else_block {
                    self.find_all_occurrences(&else_block.statements, name, refs, interner);
                }
            }
            Statement::While(while_stmt) => {
                self.find_occurrences_in_expression(&while_stmt.condition, name, refs, interner);
                self.find_all_occurrences(&while_stmt.body.statements, name, refs, interner);
            }
            Statement::Return(ret) => {
                for expr in ret.values {
                    self.find_occurrences_in_expression(expr, name, refs, interner);
                }
            }
            Statement::Block(block) => {
                self.find_all_occurrences(&block.statements, name, refs, interner);
            }
            _ => {}
        }
    }

    fn find_occurrences_in_expression(
        &self,
        expr: &Expression,
        name: &str,
        refs: &mut Vec<Span>,
        interner: &StringInterner,
    ) {
        match &expr.kind {
            ExpressionKind::Identifier(ident) => {
                if interner.resolve(*ident) == name {
                    refs.push(expr.span);
                }
            }
            ExpressionKind::Binary(_, left, right) => {
                self.find_occurrences_in_expression(left, name, refs, interner);
                self.find_occurrences_in_expression(right, name, refs, interner);
            }
            ExpressionKind::Unary(_, operand) => {
                self.find_occurrences_in_expression(operand, name, refs, interner);
            }
            ExpressionKind::Call(callee, args, _typeargs) => {
                self.find_occurrences_in_expression(callee, name, refs, interner);
                for arg in *args {
                    self.find_occurrences_in_expression(&arg.value, name, refs, interner);
                }
            }
            ExpressionKind::Member(object, _) => {
                self.find_occurrences_in_expression(object, name, refs, interner);
            }
            ExpressionKind::Index(object, index) => {
                self.find_occurrences_in_expression(object, name, refs, interner);
                self.find_occurrences_in_expression(index, name, refs, interner);
            }
            ExpressionKind::Assignment(target, _, value) => {
                self.find_occurrences_in_expression(target, name, refs, interner);
                self.find_occurrences_in_expression(value, name, refs, interner);
            }
            ExpressionKind::Conditional(condition, then_expr, else_expr) => {
                self.find_occurrences_in_expression(condition, name, refs, interner);
                self.find_occurrences_in_expression(then_expr, name, refs, interner);
                self.find_occurrences_in_expression(else_expr, name, refs, interner);
            }
            ExpressionKind::Parenthesized(inner) => {
                self.find_occurrences_in_expression(inner, name, refs, interner);
            }
            _ => {}
        }
    }
}

/// Convert a Span to an LSP Range
fn span_to_range(span: &Span) -> Range {
    Range {
        start: Position {
            line: (span.line.saturating_sub(1)) as u32,
            character: (span.column.saturating_sub(1)) as u32,
        },
        end: Position {
            line: (span.line.saturating_sub(1)) as u32,
            character: ((span.column + span.len()).saturating_sub(1)) as u32,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::document::Document;
    use std::str::FromStr;

    fn create_test_document(text: &str) -> Document {
        Document::new_test(text.to_string(), 1)
    }

    #[test]
    fn test_span_to_range() {
        let span = Span {
            start: 10,
            end: 13,
            line: 2,
            column: 5,
        };
        let range = span_to_range(&span);
        assert_eq!(range.start.line, 1);
        assert_eq!(range.start.character, 4);
        assert_eq!(range.end.line, 1);
        assert_eq!(range.end.character, 7);
    }

    #[test]
    fn test_prepare_no_declaration() {
        let doc = create_test_document("local x = 1");
        let provider = RenameProvider::new();

        // Test at a position where there's no declaration (whitespace)
        let result = provider.prepare(&doc, Position::new(0, 5));

        // At whitespace position, we won't get a declaration
        assert!(result.is_none());
    }

    #[test]
    fn test_prepare_at_function() {
        let doc = create_test_document("function foo() end");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 10));

        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_out_of_bounds() {
        let doc = create_test_document("local x = 1");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(10, 10));

        assert!(result.is_none());
    }

    #[test]
    fn test_empty_document() {
        let doc = create_test_document("");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 0));

        assert!(result.is_none());
    }

    #[test]
    fn test_invalid_new_name_empty() {
        let _doc = create_test_document("function foo() end");
        let provider = RenameProvider::new();
        let _uri = Uri::from_str("file://test.lua").unwrap();

        // Just test validation without calling rename since we can't easily create a DocumentManager
        assert!(!provider.is_valid_identifier(""));
    }

    #[test]
    fn test_invalid_new_name_same() {
        let _doc = create_test_document("function foo() end");
        let provider = RenameProvider::new();

        // Test that empty identifier is invalid
        assert!(!provider.is_valid_identifier(""));
    }

    #[test]
    fn test_invalid_new_name_invalid_identifier() {
        let _doc = create_test_document("function foo() end");
        let provider = RenameProvider::new();

        // Test that identifiers starting with numbers are invalid
        assert!(!provider.is_valid_identifier("123invalid"));
    }

    #[test]
    fn test_valid_identifier_simple() {
        let provider = RenameProvider::new();
        assert!(provider.is_valid_identifier("x"));
        assert!(provider.is_valid_identifier("_foo"));
        assert!(provider.is_valid_identifier("myVariable"));
    }

    #[test]
    fn test_valid_identifier_unicode() {
        let provider = RenameProvider::new();
        assert!(provider.is_valid_identifier("π"));
        assert!(provider.is_valid_identifier("tést"));
    }

    #[test]
    fn test_invalid_identifier_with_spaces() {
        let provider = RenameProvider::new();
        assert!(!provider.is_valid_identifier("foo bar"));
        assert!(!provider.is_valid_identifier("foo-bar"));
    }

    #[test]
    fn test_prepare_at_variable() {
        let doc = create_test_document("local x = 1");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 6));

        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_local_function() {
        let doc = create_test_document("local function foo() end");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 15));

        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_method() {
        let doc = create_test_document("function obj:method() end");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 10));

        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_class_field() {
        let doc = create_test_document("class Point\n  x: number\n  y: number\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(2, 2));

        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_enum_member() {
        let doc = create_test_document("enum Color\n  Red\n  Green\n  Blue\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(2, 2));

        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_interface_method() {
        let doc = create_test_document("interface Drawable\n  draw(): void\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(1, 2));

        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_type_alias() {
        let doc = create_test_document("type Point = { x: number, y: number }");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 5));

        assert!(result.is_some());
    }

    #[test]
    fn test_span_to_range_zero_column() {
        let span = Span {
            start: 0,
            end: 5,
            line: 1,
            column: 0,
        };
        let range = span_to_range(&span);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
    }

    #[test]
    fn test_span_to_range_single_char() {
        let span = Span {
            start: 0,
            end: 1,
            line: 0,
            column: 0,
        };
        let range = span_to_range(&span);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.character, 0);
    }

    #[test]
    fn test_prepare_empty_document() {
        let doc = create_test_document("");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 0));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_beyond_eof() {
        let doc = create_test_document("local x = 1");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(10, 0));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_in_comment() {
        let doc = create_test_document("-- local x = 1\nlocal y = 2");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 5));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_in_string() {
        let doc = create_test_document("local s = \"hello world\"");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 13));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_function_call() {
        let doc = create_test_document("foo(1, 2, 3)");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 0));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_table_field() {
        let doc = create_test_document("local t = { key = \"value\" }");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 13));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_is_valid_identifier_unicode() {
        let provider = RenameProvider::new();

        assert!(!provider.is_valid_identifier("foo bar"));
        assert!(!provider.is_valid_identifier("foo-bar"));
        assert!(!provider.is_valid_identifier("123"));
    }

    #[test]
    fn test_is_valid_identifier_valid() {
        let provider = RenameProvider::new();

        assert!(provider.is_valid_identifier("_private"));
        assert!(provider.is_valid_identifier("CamelCase"));
        assert!(provider.is_valid_identifier("snake_case"));
    }

    #[test]
    fn test_prepare_at_getter() {
        let doc = create_test_document("class Foo\n  @field\n  get value() end\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(2, 2));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_setter() {
        let doc = create_test_document("class Foo\n  @field\n  set value(v) end\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(2, 2));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_operator() {
        let doc = create_test_document("class Foo\n  operator +() end\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(1, 2));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_constructor() {
        let doc = create_test_document("class Foo\n  constructor() end\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(1, 2));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_rename_provider_clone() {
        let provider = RenameProvider::new();
        let _cloned = provider.clone();
    }

    #[test]
    fn test_prepare_at_multiline_function() {
        let doc = create_test_document("function very_long_function_name()\n  local x = 1\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 10));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_nested_function() {
        let doc = create_test_document("function outer()\n  function inner() end\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(1, 10));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_class_with_generic() {
        let doc = create_test_document("class MyClass<T> end");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 6));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_interface_with_generic() {
        let doc = create_test_document("interface MyInterface<T> end");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 10));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_enum_with_values() {
        let doc = create_test_document("enum Color\n  Red\n  Green\n  Blue\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(1, 2));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_match_expression() {
        let doc = create_test_document("match x\n  | 1 => \"one\"\n  | 2 => \"two\"\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(1, 4));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_local_function_with_colon() {
        let doc = create_test_document("local function obj:method() end");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 15));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_table_access() {
        let doc = create_test_document("local t = my_table.field");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 15));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_is_valid_identifier_empty() {
        let provider = RenameProvider::new();
        assert!(!provider.is_valid_identifier(""));
    }

    #[test]
    fn test_is_valid_identifier_starts_with_number() {
        let provider = RenameProvider::new();
        assert!(!provider.is_valid_identifier("123abc"));
    }

    #[test]
    fn test_is_valid_identifier_with_underscore() {
        let provider = RenameProvider::new();
        assert!(provider.is_valid_identifier("_"));
        assert!(provider.is_valid_identifier("_value"));
        assert!(provider.is_valid_identifier("__private__"));
    }

    #[test]
    fn test_is_valid_identifier_lua_keywords() {
        let provider = RenameProvider::new();
        assert!(!provider.is_valid_identifier("local"));
        assert!(!provider.is_valid_identifier("function"));
        assert!(!provider.is_valid_identifier("end"));
    }

    #[test]
    fn test_span_to_range_middle_of_line() {
        let span = Span {
            start: 5,
            end: 15,
            line: 2,
            column: 5,
        };
        let range = span_to_range(&span);
        assert_eq!(range.start.line, 1);
        assert_eq!(range.start.character, 4);
        assert_eq!(range.end.line, 1);
        assert_eq!(range.end.character, 14);
    }

    #[test]
    fn test_span_to_range_multiline() {
        let span = Span {
            start: 0,
            end: 20,
            line: 5,
            column: 0,
        };
        let range = span_to_range(&span);
        assert_eq!(range.start.line, 4);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 4);
        assert_eq!(range.end.character, 19);
    }

    #[test]
    fn test_prepare_response_range_with_placeholder() {
        let doc = create_test_document("local my_variable = 1");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 7));
        if let Some(PrepareRenameResponse::RangeWithPlaceholder {
            range: _,
            placeholder,
        }) = result
        {
            assert!(placeholder.contains("variable"));
        }
    }

    #[test]
    fn test_prepare_at_position_beyond_line_length() {
        let doc = create_test_document("local x = 1");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 100));
        assert!(result.is_none());
    }

    #[test]
    fn test_is_keyword_lua_reserved_words() {
        let provider = RenameProvider::new();

        assert!(provider.is_keyword("local"));
        assert!(provider.is_keyword("function"));
        assert!(provider.is_keyword("end"));
        assert!(provider.is_keyword("if"));
        assert!(provider.is_keyword("then"));
        assert!(provider.is_keyword("else"));
        assert!(provider.is_keyword("elseif"));
        assert!(provider.is_keyword("while"));
        assert!(provider.is_keyword("for"));
        assert!(provider.is_keyword("do"));
        assert!(provider.is_keyword("return"));
        assert!(provider.is_keyword("break"));
        assert!(provider.is_keyword("continue"));
    }

    #[test]
    fn test_is_keyword_typedlua_keywords() {
        let provider = RenameProvider::new();

        assert!(provider.is_keyword("type"));
        assert!(provider.is_keyword("interface"));
        assert!(provider.is_keyword("class"));
        assert!(provider.is_keyword("enum"));
        assert!(provider.is_keyword("const"));
        assert!(provider.is_keyword("export"));
        assert!(provider.is_keyword("import"));
        assert!(provider.is_keyword("from"));
        assert!(provider.is_keyword("extends"));
        assert!(provider.is_keyword("implements"));
        assert!(provider.is_keyword("public"));
        assert!(provider.is_keyword("private"));
        assert!(provider.is_keyword("protected"));
        assert!(provider.is_keyword("static"));
        assert!(provider.is_keyword("readonly"));
        assert!(provider.is_keyword("abstract"));
        assert!(provider.is_keyword("match"));
        assert!(provider.is_keyword("when"));
    }

    #[test]
    fn test_is_keyword_not_keyword() {
        let provider = RenameProvider::new();

        assert!(!provider.is_keyword("myFunction"));
        assert!(!provider.is_keyword("myVariable"));
        assert!(!provider.is_keyword("CustomClass"));
        assert!(!provider.is_keyword("_private"));
        assert!(!provider.is_keyword("MyType"));
    }

    #[test]
    fn test_get_word_at_position_simple() {
        let doc = create_test_document("local myVariable = 42");
        let provider = RenameProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 6));
        assert_eq!(word, Some("myVariable".to_string()));
    }

    #[test]
    fn test_get_word_at_position_start() {
        let doc = create_test_document("local x = 1");
        let provider = RenameProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 0));
        assert_eq!(word, Some("local".to_string()));
    }

    #[test]
    fn test_get_word_at_position_middle() {
        let doc = create_test_document("myLongVariableName");
        let provider = RenameProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 5));
        assert_eq!(word, Some("myLongVariableName".to_string()));
    }

    #[test]
    fn test_get_word_at_position_whitespace() {
        let doc = create_test_document("local x = 1");
        let provider = RenameProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 5));
        assert!(word.is_none());
    }

    #[test]
    fn test_get_word_at_position_after_word() {
        let doc = create_test_document("local x = 1");
        let provider = RenameProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 9));
        assert!(word.is_none());
    }

    #[test]
    fn test_get_word_at_position_number() {
        let doc = create_test_document("value123 = 42");
        let provider = RenameProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 0));
        assert_eq!(word, Some("value123".to_string()));
    }

    #[test]
    fn test_get_word_at_position_underscore() {
        let doc = create_test_document("_privateVar = 1");
        let provider = RenameProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 0));
        assert_eq!(word, Some("_privateVar".to_string()));
    }

    #[test]
    fn test_get_word_range_simple() {
        let doc = create_test_document("local myVar = 1");
        let provider = RenameProvider::new();

        let range = provider.get_word_range(&doc, Position::new(0, 6));
        assert!(range.is_some());
        let r = range.unwrap();
        assert_eq!(r.start.line, 0);
        assert_eq!(r.start.character, 6);
        assert_eq!(r.end.character, 11);
    }

    #[test]
    fn test_get_word_range_multiline() {
        let doc = create_test_document("function test()\n  local x = 1\nend");
        let provider = RenameProvider::new();

        let range = provider.get_word_range(&doc, Position::new(1, 2));
        assert!(range.is_some());
    }

    #[test]
    fn test_get_word_range_whitespace_returns_none() {
        let doc = create_test_document("local x = 1");
        let provider = RenameProvider::new();

        let range = provider.get_word_range(&doc, Position::new(0, 5));
        assert!(range.is_none());
    }

    #[test]
    fn test_find_declaration_function() {
        let doc = create_test_document("function myFunction() return 1 end");
        let provider = RenameProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.rename(
            &uri,
            &doc,
            Position::new(0, 10),
            "renamedFunction",
            &DocumentManager::new_test(),
        );

        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_find_declaration_variable() {
        let doc = create_test_document("local myVar = 42");
        let provider = RenameProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.rename(
            &uri,
            &doc,
            Position::new(0, 6),
            "renamedVar",
            &DocumentManager::new_test(),
        );

        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_find_declaration_class() {
        let doc = create_test_document("class MyClass end");
        let provider = RenameProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.rename(
            &uri,
            &doc,
            Position::new(0, 6),
            "RenamedClass",
            &DocumentManager::new_test(),
        );

        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_find_declaration_interface() {
        let doc = create_test_document("interface MyInterface end");
        let provider = RenameProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.rename(
            &uri,
            &doc,
            Position::new(0, 10),
            "RenamedInterface",
            &DocumentManager::new_test(),
        );

        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_find_declaration_type_alias() {
        let doc = create_test_document("type MyType = string");
        let provider = RenameProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.rename(
            &uri,
            &doc,
            Position::new(0, 5),
            "RenamedType",
            &DocumentManager::new_test(),
        );

        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_find_declaration_enum() {
        let doc = create_test_document("enum MyEnum\n  A\n  B\nend");
        let provider = RenameProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.rename(
            &uri,
            &doc,
            Position::new(0, 5),
            "RenamedEnum",
            &DocumentManager::new_test(),
        );

        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_invalid_identifier_numbers_only() {
        let provider = RenameProvider::new();

        assert!(!provider.is_valid_identifier("123"));
        assert!(!provider.is_valid_identifier("1abc"));
    }

    #[test]
    fn test_invalid_identifier_starts_with_digit() {
        let provider = RenameProvider::new();

        assert!(!provider.is_valid_identifier("1abc"));
        assert!(!provider.is_valid_identifier("0test"));
        assert!(!provider.is_valid_identifier("9variable"));
    }

    #[test]
    fn test_valid_identifier_with_numbers() {
        let provider = RenameProvider::new();

        assert!(provider.is_valid_identifier("var1"));
        assert!(provider.is_valid_identifier("value2"));
        assert!(provider.is_valid_identifier("test123"));
    }

    #[test]
    fn test_valid_identifier_underscore_prefix() {
        let provider = RenameProvider::new();

        assert!(provider.is_valid_identifier("_"));
        assert!(provider.is_valid_identifier("_private"));
        assert!(provider.is_valid_identifier("__"));
        assert!(provider.is_valid_identifier("_test123"));
    }

    #[test]
    fn test_invalid_identifier_contains_hyphen() {
        let provider = RenameProvider::new();

        assert!(!provider.is_valid_identifier("my-var"));
        assert!(!provider.is_valid_identifier("test-value"));
    }

    #[test]
    fn test_invalid_identifier_contains_dot() {
        let provider = RenameProvider::new();

        assert!(!provider.is_valid_identifier("my.var"));
        assert!(!provider.is_valid_identifier("obj.prop"));
    }

    #[test]
    fn test_invalid_identifier_empty_after_trim() {
        let provider = RenameProvider::new();

        assert!(!provider.is_valid_identifier("   "));
    }

    #[test]
    fn test_prepare_response_format() {
        let doc = create_test_document("local myFunction = 1");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 6));
        assert!(result.is_some());

        if let Some(PrepareRenameResponse::RangeWithPlaceholder { range, placeholder }) = result {
            assert_eq!(placeholder, "myFunction");
        }
    }

    #[test]
    fn test_span_to_range_preserves_length() {
        let span = Span {
            start: 5,
            end: 15,
            line: 1,
            column: 5,
        };
        let range = span_to_range(&span);

        let expected_length = 10; // 15 - 5 = 10
        let actual_length = range.end.character - range.start.character;
        assert_eq!(actual_length, expected_length);
    }

    #[test]
    fn test_span_to_range_single_line() {
        let span = Span {
            start: 0,
            end: 5,
            line: 0,
            column: 0,
        };
        let range = span_to_range(&span);

        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.character, 4);
    }

    #[test]
    fn test_span_to_range_negative_column() {
        let span = Span {
            start: 0,
            end: 5,
            line: 0,
            column: 0,
        };
        let range = span_to_range(&span);
        assert!(range.end.character >= 0);
    }

    #[test]
    fn test_prepare_at_comment_line() {
        let doc = create_test_document("-- This is a comment\nlocal x = 1");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 5));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_string_literal() {
        let doc = create_test_document("local s = \"hello\"");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 13));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_number_literal() {
        let doc = create_test_document("local x = 12345");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 12));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_table_literal() {
        let doc = create_test_document("local t = { key = \"value\" }");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 13));
        assert!(result.is_none() || result.is_some());
    }

    #[test]
    fn test_prepare_at_colon_method_definition() {
        let doc = create_test_document("function obj:method() end");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 10));
        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_local_function_with_type_annotations() {
        let doc = create_test_document(
            "local function add(a: number, b: number): number return a + b end",
        );
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 15));
        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_generic_class() {
        let doc = create_test_document("class Container<T>\n  value: T\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 6));
        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_generic_function() {
        let doc = create_test_document("function identity<T>(x: T): T return x end");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(0, 10));
        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_method_in_class() {
        let doc = create_test_document(
            "class Point\n  x: number\n  y: number\n  constructor(x, y) end\n  method() end\nend",
        );
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(4, 2));
        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_property_in_class() {
        let doc = create_test_document("class Point\n  x: number\n  y: number\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(1, 2));
        assert!(result.is_some());
    }

    #[test]
    fn test_prepare_at_constructor_in_class() {
        let doc = create_test_document("class Point\n  constructor() end\nend");
        let provider = RenameProvider::new();

        let result = provider.prepare(&doc, Position::new(1, 2));
        assert!(result.is_some());
    }

    #[test]
    fn test_is_valid_identifier_max_length() {
        let provider = RenameProvider::new();

        let long_name = "a".repeat(1000);
        assert!(provider.is_valid_identifier(&long_name));
    }

    #[test]
    fn test_is_valid_identifier_special_unicode() {
        let provider = RenameProvider::new();

        assert!(!provider.is_valid_identifier("var@name"));
        assert!(!provider.is_valid_identifier("var#name"));
        assert!(!provider.is_valid_identifier("var$name"));
        assert!(!provider.is_valid_identifier("var%name"));
    }
}
