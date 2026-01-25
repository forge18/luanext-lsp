use crate::document::{Document, DocumentManager};
use lsp_types::{GotoDefinitionResponse, Location, Position, Range, Uri};
use std::sync::Arc;
use typedlua_core::ast::statement::Statement;
use typedlua_core::diagnostics::CollectingDiagnosticHandler;
use typedlua_core::string_interner::StringInterner;
use typedlua_core::{Lexer, Parser, Span};

/// Provides go-to-definition functionality
pub struct DefinitionProvider;

impl DefinitionProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide definition location for symbol at position
    pub fn provide(
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

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids);
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
        use typedlua_core::ast::statement::ImportClause;

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
                };

                if let Some(exported_name) = exported_name {
                    // Resolve the import path to a ModuleId
                    if let Some(module_id) = &current_document.module_id {
                        if let Ok(target_module_id) = document_manager
                            .module_resolver()
                            .resolve(import_source, module_id.path())
                        {
                            // Convert ModuleId to URI
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

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids);
        let ast = parser.parse().ok()?;

        // Search for the exported declaration
        use typedlua_core::ast::statement::ExportKind;

        for stmt in &ast.statements {
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
                        for spec in specifiers {
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
        use typedlua_core::ast::pattern::Pattern;

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
        use typedlua_core::ast::pattern::Pattern;

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
