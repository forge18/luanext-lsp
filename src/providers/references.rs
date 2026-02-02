use crate::document::{Document, DocumentManager};
use lsp_types::{Uri, *};

use std::sync::Arc;
use typedlua_parser::ast::{
    expression::{Expression, ExpressionKind},
    statement::Statement,
};
use typedlua_typechecker::diagnostics::CollectingDiagnosticHandler;
use typedlua_parser::string_interner::StringInterner;
use typedlua_parser::{Lexer, Parser, Span};

/// Provides find-references functionality
pub struct ReferencesProvider;

impl ReferencesProvider {
    pub fn new() -> Self {
        Self
    }

    /// Find all references to the symbol at the given position
    pub fn provide(
        &self,
        uri: &Uri,
        document: &Document,
        position: Position,
        include_declaration: bool,
        document_manager: &DocumentManager,
    ) -> Option<Vec<Location>> {
        // Get the word at the current position
        let word = self.get_word_at_position(document, position)?;

        // Parse the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids);
        let ast = parser.parse().ok()?;

        // Find all references in the current file
        let mut references = Vec::new();
        self.find_references_in_statements(&ast.statements, &word, &mut references, &interner);

        // Optionally include the declaration
        if include_declaration {
            if let Some(decl_span) = self.find_declaration(&ast.statements, &word, &interner) {
                references.push(decl_span);
            }
        }

        // Convert spans to locations (current file)
        let mut locations: Vec<Location> = references
            .into_iter()
            .map(|span| Location {
                uri: uri.clone(),
                range: span_to_range(&span),
            })
            .collect();

        // Check if this symbol is exported from current file
        // If so, search for references in files that import it
        if self.is_symbol_exported(&ast.statements, &word, &interner) {
            if let Some(module_id) = &document.module_id {
                self.search_references_in_importing_files(
                    module_id,
                    &word,
                    document_manager,
                    &mut locations,
                );
            }
        }

        // Check if this symbol is imported from another file
        // If so, also search for references in that file
        if let Some((source_uri, exported_name)) = self.find_import_source(
            &ast.statements,
            &word,
            document,
            document_manager,
            &interner,
        ) {
            // Search references in the source file
            if let Some(source_doc) = document_manager.get(&source_uri) {
                let mut source_refs = Vec::new();

                // Parse source document
                let handler = Arc::new(CollectingDiagnosticHandler::new());
                let mut lexer = Lexer::new(&source_doc.text, handler.clone(), &interner);
                if let Ok(tokens) = lexer.tokenize() {
                    let mut parser = Parser::new(tokens, handler, &interner, &common_ids);
                    if let Ok(ast) = parser.parse() {
                        self.find_references_in_statements(
                            &ast.statements,
                            &exported_name,
                            &mut source_refs,
                            &interner,
                        );

                        // Include declaration in source file
                        if include_declaration {
                            if let Some(decl_span) =
                                self.find_declaration(&ast.statements, &exported_name, &interner)
                            {
                                source_refs.push(decl_span);
                            }
                        }

                        // Convert to locations
                        for span in source_refs {
                            locations.push(Location {
                                uri: source_uri.clone(),
                                range: span_to_range(&span),
                            });
                        }
                    }
                }
            }
        }

        Some(locations)
    }

    /// Check if a symbol is exported from the file
    fn is_symbol_exported(
        &self,
        statements: &[Statement],
        symbol_name: &str,
        interner: &StringInterner,
    ) -> bool {
        use typedlua_parser::ast::statement::ExportKind;

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
                        for spec in specifiers {
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
        use typedlua_parser::ast::pattern::Pattern;

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

    /// Search for references in files that import from the current module
    fn search_references_in_importing_files(
        &self,
        module_id: &typedlua_typechecker::module_resolver::ModuleId,
        symbol_name: &str,
        document_manager: &DocumentManager,
        locations: &mut Vec<Location>,
    ) {
        // Use symbol index to quickly find all files that import this symbol
        let importing_uris = document_manager
            .symbol_index()
            .get_importers(module_id, symbol_name);

        // For each importing file, find references to the local name
        for uri in importing_uris {
            if let Some(doc) = document_manager.get(&uri) {
                // Get the document's module ID
                if let Some(doc_module_id) = &doc.module_id {
                    // Use symbol index to get import info
                    if let Some(imports) = document_manager
                        .symbol_index()
                        .get_imports(doc_module_id, symbol_name)
                    {
                        for import_info in imports {
                            // Check if this import is from our source module
                            if let Some(source_module_id) =
                                document_manager.uri_to_module_id(&import_info.source_uri)
                            {
                                if source_module_id == module_id
                                    && import_info.imported_name == symbol_name
                                {
                                    // Parse the document to find references
                                    if let Some((ast, interner, _)) = doc.get_or_parse_ast() {
                                        let mut refs = Vec::new();
                                        self.find_references_in_statements(
                                            &ast.statements,
                                            &import_info.local_name,
                                            &mut refs,
                                            &*interner,
                                        );

                                        // Convert to locations
                                        for span in refs {
                                            locations.push(Location {
                                                uri: uri.clone(),
                                                range: span_to_range(&span),
                                            });
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
        use typedlua_parser::ast::statement::ImportClause;

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
                        let resolver = document_manager.module_resolver(); { {
                            if let Ok(target_module_id) = resolver.resolve(
                                import_source,
                                std::path::Path::new(module_id.as_str()),
                            ) {
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

    /// Find the declaration span for a given symbol name
    fn find_declaration(
        &self,
        statements: &[Statement],
        name: &str,
        interner: &StringInterner,
    ) -> Option<Span> {
        use typedlua_parser::ast::pattern::Pattern;

        for stmt in statements {
            match stmt {
                Statement::Variable(var_decl) => {
                    // Check if the pattern contains this identifier
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

    /// Find all references to a symbol by traversing the AST
    fn find_references_in_statements(
        &self,
        statements: &[Statement],
        name: &str,
        refs: &mut Vec<Span>,
        interner: &StringInterner,
    ) {
        for stmt in statements {
            self.find_references_in_statement(stmt, name, refs, interner);
        }
    }

    fn find_references_in_statement(
        &self,
        stmt: &Statement,
        name: &str,
        refs: &mut Vec<Span>,
        interner: &StringInterner,
    ) {
        match stmt {
            Statement::Expression(expr) => {
                self.find_references_in_expression(expr, name, refs, interner);
            }
            Statement::Variable(var_decl) => {
                self.find_references_in_expression(&var_decl.initializer, name, refs, interner);
            }
            Statement::Function(func_decl) => {
                for stmt in &func_decl.body.statements {
                    self.find_references_in_statement(stmt, name, refs, interner);
                }
            }
            Statement::If(if_stmt) => {
                self.find_references_in_expression(&if_stmt.condition, name, refs, interner);
                self.find_references_in_statements(
                    &if_stmt.then_block.statements,
                    name,
                    refs,
                    interner,
                );
                for else_if in &if_stmt.else_ifs {
                    self.find_references_in_expression(&else_if.condition, name, refs, interner);
                    self.find_references_in_statements(
                        &else_if.block.statements,
                        name,
                        refs,
                        interner,
                    );
                }
                if let Some(else_block) = &if_stmt.else_block {
                    self.find_references_in_statements(
                        &else_block.statements,
                        name,
                        refs,
                        interner,
                    );
                }
            }
            Statement::While(while_stmt) => {
                self.find_references_in_expression(&while_stmt.condition, name, refs, interner);
                self.find_references_in_statements(
                    &while_stmt.body.statements,
                    name,
                    refs,
                    interner,
                );
            }
            Statement::Return(ret) => {
                for expr in &ret.values {
                    self.find_references_in_expression(expr, name, refs, interner);
                }
            }
            Statement::Block(block) => {
                self.find_references_in_statements(&block.statements, name, refs, interner);
            }
            _ => {}
        }
    }

    fn find_references_in_expression(
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
                self.find_references_in_expression(left, name, refs, interner);
                self.find_references_in_expression(right, name, refs, interner);
            }
            ExpressionKind::Unary(_, operand) => {
                self.find_references_in_expression(operand, name, refs, interner);
            }
            ExpressionKind::Call(callee, args, _type_args) => {
                self.find_references_in_expression(callee, name, refs, interner);
                for arg in args {
                    self.find_references_in_expression(&arg.value, name, refs, interner);
                }
            }
            ExpressionKind::Member(object, _) => {
                self.find_references_in_expression(object, name, refs, interner);
            }
            ExpressionKind::Index(object, index) => {
                self.find_references_in_expression(object, name, refs, interner);
                self.find_references_in_expression(index, name, refs, interner);
            }
            ExpressionKind::Assignment(target, _, value) => {
                self.find_references_in_expression(target, name, refs, interner);
                self.find_references_in_expression(value, name, refs, interner);
            }
            ExpressionKind::Array(elements) => {
                for elem in elements {
                    match elem {
                        typedlua_parser::ast::expression::ArrayElement::Expression(e) => {
                            self.find_references_in_expression(e, name, refs, interner);
                        }
                        typedlua_parser::ast::expression::ArrayElement::Spread(e) => {
                            self.find_references_in_expression(e, name, refs, interner);
                        }
                    }
                }
            }
            ExpressionKind::Object(properties) => {
                use typedlua_parser::ast::expression::ObjectProperty;
                for prop in properties {
                    match prop {
                        ObjectProperty::Property { value, .. } => {
                            self.find_references_in_expression(value, name, refs, interner);
                        }
                        ObjectProperty::Computed { key, value, .. } => {
                            self.find_references_in_expression(key, name, refs, interner);
                            self.find_references_in_expression(value, name, refs, interner);
                        }
                        ObjectProperty::Spread { value, .. } => {
                            self.find_references_in_expression(value, name, refs, interner);
                        }
                    }
                }
            }
            ExpressionKind::Conditional(condition, then_expr, else_expr) => {
                self.find_references_in_expression(condition, name, refs, interner);
                self.find_references_in_expression(then_expr, name, refs, interner);
                self.find_references_in_expression(else_expr, name, refs, interner);
            }
            ExpressionKind::Parenthesized(inner) => {
                self.find_references_in_expression(inner, name, refs, interner);
            }
            _ => {}
        }
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

    /// Provide document highlights for the symbol at the given position
    #[allow(dead_code)]
    pub fn provide_highlights(
        &self,
        document: &Document,
        position: Position,
    ) -> Option<Vec<DocumentHighlight>> {
        // Get the word at the current position
        let word = self.get_word_at_position(document, position)?;

        // Parse the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids);
        let ast = parser.parse().ok()?;

        // Find all references to this symbol (including declaration)
        let mut references = Vec::new();
        self.find_references_in_statements(&ast.statements, &word, &mut references, &interner);

        // Include the declaration
        if let Some(decl_span) = self.find_declaration(&ast.statements, &word, &interner) {
            references.push(decl_span);
        }

        // Convert spans to document highlights
        let highlights: Vec<DocumentHighlight> = references
            .into_iter()
            .map(|span| DocumentHighlight {
                range: span_to_range(&span),
                kind: Some(DocumentHighlightKind::TEXT),
            })
            .collect();

        if highlights.is_empty() {
            None
        } else {
            Some(highlights)
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
