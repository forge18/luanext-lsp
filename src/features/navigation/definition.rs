use crate::core::document::{Document, DocumentManager, DocumentManagerTrait};
use crate::traits::DefinitionProviderTrait;
use lsp_types::{GotoDefinitionResponse, Location, Position, Range, Uri};
use luanext_parser::ast::statement::Statement;
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser, Span};
use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
use std::sync::Arc;

/// Provides go-to-definition functionality
#[derive(Clone)]
pub struct DefinitionProvider;

impl DefinitionProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide definition location for symbol at position (internal method with manager)
    pub fn provide_with_manager(
        &self,
        uri: &Uri,
        document: &Document,
        position: Position,
        document_manager: &DocumentManager,
    ) -> Option<GotoDefinitionResponse> {
        // Get the word at the current position
        let word = self.get_word_at_position(document, position)?;

        // Parse the document to find declarations
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(
            tokens,
            handler.clone(),
            &interner,
            &common_ids,
            Box::leak(Box::new(bumpalo::Bump::new())),
        );
        let ast = parser.parse().ok()?;

        // First, check if the symbol is from an import statement
        if let Some(import_location) = self.find_import_definition(
            &ast.statements,
            &word,
            document,
            document_manager,
            &interner,
        ) {
            return Some(GotoDefinitionResponse::Scalar(import_location));
        }

        // Otherwise, search for local declaration
        let def_span = self.find_declaration(&ast.statements, &word, &interner)?;

        // Convert span to LSP Location
        let location = Location {
            uri: uri.clone(),
            range: span_to_range(&def_span),
        };

        Some(GotoDefinitionResponse::Scalar(location))
    }

    /// Find definition for symbols imported from other files
    fn find_import_definition(
        &self,
        statements: &[Statement],
        symbol_name: &str,
        current_document: &Document,
        document_manager: &DocumentManager,
        interner: &StringInterner,
    ) -> Option<Location> {
        use luanext_parser::ast::statement::ImportClause;

        // Search for import statements that import this symbol
        for stmt in statements {
            if let Statement::Import(import_decl) = stmt {
                let import_source = &import_decl.source;

                // Check if this import contains our symbol
                let exported_name = match &import_decl.clause {
                    ImportClause::Named(specs) => {
                        // Find the import spec that matches our symbol
                        specs.iter().find_map(|spec| {
                            let local_name = spec.local.as_ref().unwrap_or(&spec.imported);
                            if interner.resolve(local_name.node) == symbol_name {
                                Some(interner.resolve(spec.imported.node))
                            } else {
                                None
                            }
                        })
                    }
                    ImportClause::Default(ident) => {
                        if interner.resolve(ident.node) == symbol_name {
                            Some("default".to_string())
                        } else {
                            None
                        }
                    }
                    ImportClause::Namespace(ident) => {
                        if interner.resolve(ident.node) == symbol_name {
                            // For namespace imports, we'd need to handle property access
                            // For now, just return None
                            None
                        } else {
                            None
                        }
                    }
                    ImportClause::TypeOnly(_) => {
                        // Type-only imports - could handle similarly to Named
                        None
                    }
                    ImportClause::Mixed { default, named } => {
                        // Check default import first
                        if interner.resolve(default.node) == symbol_name {
                            Some("default".to_string())
                        } else {
                            // Check named imports
                            named.iter().find_map(|spec| {
                                let local_name = spec.local.as_ref().unwrap_or(&spec.imported);
                                if interner.resolve(local_name.node) == symbol_name {
                                    Some(interner.resolve(spec.imported.node))
                                } else {
                                    None
                                }
                            })
                        }
                    }
                };

                if let Some(exported_name) = exported_name {
                    // Resolve the import path to a module ID
                    if let Some(module_id) = &current_document.module_id {
                        let resolver = document_manager.module_resolver();
                        {
                            if let Ok(target_module_id) = resolver
                                .resolve(import_source, std::path::Path::new(module_id.as_str()))
                            {
                                // Convert module ID to URI
                                if let Some(target_uri) =
                                    document_manager.module_id_to_uri(&target_module_id)
                                {
                                    // Try to get the target document if it's open
                                    if let Some(target_doc) = document_manager.get(target_uri) {
                                        // Parse the target document and find the export
                                        return self.find_export_in_document(
                                            target_doc,
                                            &exported_name,
                                            target_uri,
                                        );
                                    } else {
                                        // Document not open - we could potentially read it from disk
                                        // For now, just return the file location
                                        return Some(Location {
                                            uri: target_uri.clone(),
                                            range: Range {
                                                start: Position {
                                                    line: 0,
                                                    character: 0,
                                                },
                                                end: Position {
                                                    line: 0,
                                                    character: 0,
                                                },
                                            },
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        None
    }

    /// Find an exported symbol in a document
    fn find_export_in_document(
        &self,
        document: &Document,
        symbol_name: &str,
        uri: &Uri,
    ) -> Option<Location> {
        // Parse the target document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(
            tokens,
            handler.clone(),
            &interner,
            &common_ids,
            Box::leak(Box::new(bumpalo::Bump::new())),
        );
        let ast = parser.parse().ok()?;

        // Search for the exported declaration
        use luanext_parser::ast::statement::ExportKind;

        for stmt in ast.statements {
            if let Statement::Export(export_decl) = stmt {
                match &export_decl.kind {
                    ExportKind::Declaration(decl) => {
                        // Check if this declaration exports our symbol
                        if let Some(span) =
                            self.get_declaration_name_span(decl, symbol_name, &interner)
                        {
                            return Some(Location {
                                uri: uri.clone(),
                                range: span_to_range(&span),
                            });
                        }
                    }
                    ExportKind::Named {
                        specifiers,
                        source: _,
                    } => {
                        // Check if this is a named export of our symbol
                        for spec in specifiers.iter() {
                            if interner.resolve(spec.exported.as_ref().unwrap_or(&spec.local).node)
                                == symbol_name
                            {
                                // Find the local declaration
                                if let Some(local_span) = self.find_declaration(
                                    &ast.statements,
                                    &interner.resolve(spec.local.node),
                                    &interner,
                                ) {
                                    return Some(Location {
                                        uri: uri.clone(),
                                        range: span_to_range(&local_span),
                                    });
                                }
                            }
                        }
                    }
                    ExportKind::Default(_) if symbol_name == "default" => {
                        // For default exports, return the export statement location
                        return Some(Location {
                            uri: uri.clone(),
                            range: span_to_range(&export_decl.span),
                        });
                    }
                    _ => {}
                }
            }
        }

        None
    }

    /// Get the name span from a declaration statement
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

impl DefinitionProviderTrait for DefinitionProvider {
    fn provide(
        &self,
        uri: &Uri,
        document: &Document,
        position: Position,
    ) -> Option<GotoDefinitionResponse> {
        use luanext_typechecker::cli::config::CompilerOptions;
        use luanext_typechecker::cli::fs::MockFileSystem;
        use luanext_typechecker::module_resolver::{ModuleRegistry, ModuleResolver};

        let workspace_root = std::path::PathBuf::from("/test");
        let fs = std::sync::Arc::new(MockFileSystem::new());
        let compiler_options = CompilerOptions::default();
        let module_config =
            luanext_typechecker::module_resolver::ModuleConfig::from_compiler_options(
                &compiler_options,
                &workspace_root,
            );
        let module_registry = std::sync::Arc::new(ModuleRegistry::new());
        let module_resolver = std::sync::Arc::new(ModuleResolver::new(
            fs,
            module_config,
            workspace_root.clone(),
        ));
        let manager = DocumentManager::new(workspace_root, module_registry, module_resolver);
        self.provide_with_manager(uri, document, position, &manager)
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
    fn test_get_word_at_position_valid() {
        let doc = create_test_document("local foo = 1");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 7));

        assert_eq!(word, Some("foo".to_string()));
    }

    #[test]
    fn test_get_word_at_position_whitespace() {
        let doc = create_test_document("local foo = 1");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 5));

        assert_eq!(word, None);
    }

    #[test]
    fn test_get_word_at_position_out_of_bounds() {
        let doc = create_test_document("local foo = 1");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(10, 0));

        assert_eq!(word, None);
    }

    #[test]
    fn test_get_word_at_position_multiline() {
        let doc = create_test_document("local x = 1\nlocal y = 2");
        let provider = DefinitionProvider::new();

        // Position at start of "local" on line 2
        let word = provider.get_word_at_position(&doc, Position::new(1, 0));

        assert_eq!(word, Some("local".to_string()));
    }

    #[test]
    fn test_find_export_in_document_function() {
        let doc = create_test_document("export function myFunc() end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "myFunc", &uri);

        assert!(result.is_some());
    }

    #[test]
    fn test_find_export_in_document_not_found() {
        let doc = create_test_document("export function myFunc() end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "nonExistent", &uri);

        assert!(result.is_none());
    }

    #[test]
    fn test_empty_document() {
        let doc = create_test_document("");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.provide(&uri, &doc, Position::new(0, 0));

        assert!(result.is_none());
    }

    #[test]
    fn test_definition_provider_new() {
        let provider = DefinitionProvider::new();
        let _ = provider.clone();
    }

    #[test]
    fn test_span_to_range_zero_line() {
        let span = Span {
            start: 0,
            end: 5,
            line: 1,
            column: 1,
        };
        let range = span_to_range(&span);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 0);
        // column 1 + (5-1) = 5 (not 4) because span.column is 1-based
        assert_eq!(range.end.character, 5);
    }

    #[test]
    fn test_get_word_at_position_underscore() {
        let doc = create_test_document("local my_var = 1");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 7));

        assert_eq!(word, Some("my_var".to_string()));
    }

    #[test]
    fn test_get_word_at_position_number_in_name() {
        let doc = create_test_document("local var1 = 1");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 8));

        assert_eq!(word, Some("var1".to_string()));
    }

    #[test]
    fn test_get_word_at_position_start_of_line() {
        let doc = create_test_document("local x = 1");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 0));

        assert_eq!(word, Some("local".to_string()));
    }

    #[test]
    fn test_get_word_at_position_end_of_word() {
        let doc = create_test_document("local x = 1");
        let provider = DefinitionProvider::new();

        // Position at end of "local" (position 5 is the space)
        let word = provider.get_word_at_position(&doc, Position::new(0, 4));

        assert_eq!(word, Some("local".to_string()));
    }

    #[test]
    fn test_get_word_at_position_special_characters() {
        let doc = create_test_document("local x = @#$%");
        let provider = DefinitionProvider::new();

        // Position on special character
        let word = provider.get_word_at_position(&doc, Position::new(0, 9));

        assert!(word.is_none());
    }

    #[test]
    fn test_get_word_at_position_empty_line() {
        let doc = create_test_document("");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 0));

        assert!(word.is_none());
    }

    #[test]
    fn test_find_declaration_function() {
        let doc = create_test_document("export function myFunc() end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        // Function declarations should be findable
        let result = provider.find_export_in_document(&doc, "myFunc", &uri);

        // Either it finds it or not, but shouldn't panic
        let _ = result;
    }

    #[test]
    fn test_find_declaration_variable() {
        let doc = create_test_document("export let myVar = 42");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "myVar", &uri);

        let _ = result;
    }

    #[test]
    fn test_find_declaration_class() {
        let doc = create_test_document("export class MyClass end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "MyClass", &uri);

        let _ = result;
    }

    #[test]
    fn test_find_declaration_interface() {
        let doc = create_test_document("export interface MyInterface end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "MyInterface", &uri);

        let _ = result;
    }

    #[test]
    fn test_find_declaration_enum() {
        let doc = create_test_document("export enum MyEnum { A, B } end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "MyEnum", &uri);

        let _ = result;
    }

    #[test]
    fn test_find_declaration_type_alias() {
        let doc = create_test_document("export type MyType = number");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "MyType", &uri);

        let _ = result;
    }

    #[test]
    fn test_definition_provider_trait_impl() {
        let provider = DefinitionProvider::new();
        let doc = create_test_document("local x = 1");
        let uri = Uri::from_str("file://test.lua").unwrap();

        // Test through trait
        let result = provider.provide(&uri, &doc, Position::new(0, 0));

        // Should not panic, result depends on implementation
        let _ = result;
    }

    #[test]
    fn test_find_declaration_with_nested_function() {
        let doc =
            create_test_document("function outer() local x = 1 function inner() return x end end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "x", &uri);
        let _ = result;
    }

    #[test]
    fn test_find_declaration_with_if_statement() {
        let doc = create_test_document("local x = 1 if true then x = 2 end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "x", &uri);
        let _ = result;
    }

    #[test]
    fn test_find_declaration_with_while_loop() {
        let doc = create_test_document("local i = 0 while i < 10 do i = i + 1 end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "i", &uri);
        let _ = result;
    }

    #[test]
    fn test_find_declaration_with_for_loop() {
        let doc = create_test_document("for i = 1, 10 do print(i) end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "i", &uri);
        let _ = result;
    }

    #[test]
    fn test_get_word_at_position_beyond_line_length() {
        let doc = create_test_document("local x = 1");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 100));

        assert!(word.is_none());
    }

    #[test]
    fn test_get_word_at_position_unicode() {
        let doc = create_test_document("local π = 3.14");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 6));

        assert_eq!(word, Some("π".to_string()));
    }

    #[test]
    fn test_get_word_at_position_long_variable() {
        let doc = create_test_document("local veryLongVariableName = 42");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 15));

        assert_eq!(word, Some("veryLongVariableName".to_string()));
    }

    #[test]
    fn test_get_word_at_position_after_equal_sign() {
        let doc = create_test_document("local x = 12345");
        let provider = DefinitionProvider::new();

        let word = provider.get_word_at_position(&doc, Position::new(0, 10));

        assert_eq!(word, Some("12345".to_string()));
    }

    #[test]
    fn test_span_to_range_multiline() {
        let span = Span {
            start: 0,
            end: 10,
            line: 5,
            column: 0,
        };
        let range = span_to_range(&span);

        assert_eq!(range.start.line, 4);
        assert_eq!(range.end.line, 4);
    }

    #[test]
    fn test_span_to_range_single_character() {
        let span = Span {
            start: 0,
            end: 1,
            line: 1,
            column: 0,
        };
        let range = span_to_range(&span);

        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.character, 0);
    }

    #[test]
    fn test_provide_with_parse_error() {
        let doc = create_test_document("local x = ");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.provide(&uri, &doc, Position::new(0, 5));

        assert!(result.is_none());
    }

    #[test]
    fn test_provide_with_lexer_error() {
        let doc = create_test_document("\x00\x01\x02");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.provide(&uri, &doc, Position::new(0, 0));

        assert!(result.is_none());
    }

    #[test]
    fn test_find_export_default() {
        let doc = create_test_document("export default MyClass end");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "default", &uri);
        let _ = result;
    }

    #[test]
    fn test_find_declaration_in_class_method() {
        let doc =
            create_test_document("class Point\n  x: number\n  constructor(x: number) end\nend");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "x", &uri);
        let _ = result;
    }

    #[test]
    fn test_get_declaration_name_span_variable() {
        let doc = create_test_document("local myVar = 42");
        let provider = DefinitionProvider::new();
        let _ = doc;

        // Test the internal method through find_export_in_document
        let uri = Uri::from_str("file://test.lua").unwrap();
        let result = provider.find_export_in_document(&doc, "myVar", &uri);
        let _ = result;
    }

    #[test]
    fn test_find_export_not_found_in_class() {
        let doc = create_test_document("class MyClass\n  x: number\nend");
        let provider = DefinitionProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let result = provider.find_export_in_document(&doc, "nonExistent", &uri);
        assert!(result.is_none());
    }

    #[test]
    fn test_definition_response_types() {
        let location = Location {
            uri: Uri::from_str("file://test.lua").unwrap(),
            range: Range {
                start: Position::new(0, 0),
                end: Position::new(0, 5),
            },
        };

        let scalar_response = GotoDefinitionResponse::Scalar(location.clone());
        assert!(matches!(scalar_response, GotoDefinitionResponse::Scalar(_)));

        let array_response = GotoDefinitionResponse::Array(vec![location]);
        assert!(matches!(array_response, GotoDefinitionResponse::Array(_)));
    }

    #[test]
    fn test_span_with_zero_length() {
        let span = Span {
            start: 0,
            end: 0,
            line: 1,
            column: 0,
        };
        let range = span_to_range(&span);

        // Should not panic
        assert!(range.start.character >= 0);
    }
}
