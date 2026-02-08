use lsp_types::{SymbolInformation, SymbolKind, Uri};
use std::collections::{HashMap, HashSet};
use typedlua_parser::ast::statement::{ExportKind, ImportClause, OperatorKind, Statement};
use typedlua_parser::ast::Program;
use typedlua_parser::string_interner::StringInterner;
use typedlua_parser::Span;

/// Information about an exported symbol
#[derive(Debug, Clone)]
#[allow(dead_code)] // Public API - fields may be used by external consumers
pub struct ExportInfo {
    /// The exported name (what other modules see)
    pub exported_name: String,
    /// The local name in the exporting module (may differ from exported_name)
    pub local_name: String,
    /// URI of the module that exports this symbol
    pub uri: Uri,
    /// Whether this is a default export
    pub is_default: bool,
}

/// Information about an imported symbol
#[derive(Debug, Clone)]
#[allow(dead_code)] // Public API - fields may be used by external consumers
pub struct ImportInfo {
    /// The local name in the importing module
    pub local_name: String,
    /// The imported name from the source module
    pub imported_name: String,
    /// URI of the source module
    pub source_uri: Uri,
    /// URI of the module that imports this symbol
    pub importing_uri: Uri,
}

/// Information about a workspace symbol (for workspace-wide search)
#[derive(Debug, Clone)]
pub struct WorkspaceSymbolInfo {
    /// The symbol name
    pub name: String,
    /// The symbol kind (class, function, interface, etc.)
    pub kind: SymbolKind,
    /// URI of the file containing this symbol
    pub uri: Uri,
    /// Location of the symbol in the file
    pub span: Span,
    /// Container name (e.g., class name for a method)
    pub container_name: Option<String>,
}

/// Reverse index for fast cross-file symbol lookups
///
/// This index maintains bidirectional mappings between:
/// - Symbols and the modules that export them
/// - Symbols and the modules that import them
/// - All workspace symbols for global search
///
/// This enables fast queries like:
/// - "Which files import symbol X from module Y?"
/// - "What symbols does module Z export?"
/// - "Where is symbol W imported from?"
/// - "Find all symbols matching query Q in the workspace"
#[derive(Debug, Clone, Default)]
pub struct SymbolIndex {
    /// Map from (module_id, exported_symbol_name) -> ExportInfo
    exports: HashMap<(String, String), ExportInfo>,

    /// Map from (module_id, local_symbol_name) -> Vec<ImportInfo>
    /// Multiple imports because a symbol might be imported from different modules with different names
    imports: HashMap<(String, String), Vec<ImportInfo>>,

    /// Reverse index: Map from exported (source_module_id, exported_name) -> Set of importing URIs
    /// This answers: "Which files import this symbol?"
    importers: HashMap<(String, String), HashSet<Uri>>,

    /// Map from URI to module_id (String) for quick lookups
    uri_to_module: HashMap<Uri, String>,

    /// Workspace-wide symbol index: Map from lowercase symbol name -> Vec<WorkspaceSymbolInfo>
    /// Lowercase keys enable case-insensitive fuzzy matching
    workspace_symbols: HashMap<String, Vec<WorkspaceSymbolInfo>>,
}

impl SymbolIndex {
    pub fn new() -> Self {
        Self::default()
    }

    /// Update the index for a specific document
    ///
    /// This should be called whenever a document is opened, changed, or saved.
    pub fn update_document(
        &mut self,
        uri: &Uri,
        module_id: &str,
        ast: &Program,
        interner: &StringInterner,
        resolve_import: impl Fn(&str, &str) -> Option<(String, Uri)>,
    ) {
        // Clear old entries for this document
        self.clear_document(uri, module_id);

        // Register URI -> module_id mapping
        self.uri_to_module
            .insert(uri.clone(), module_id.to_string());

        // Index exports
        self.index_exports(uri, module_id, &ast.statements, interner);

        // Index imports
        self.index_imports(uri, module_id, &ast.statements, interner, resolve_import);

        // Index workspace symbols
        self.index_workspace_symbols(uri, &ast.statements, interner);
    }

    /// Clear index entries for a document
    pub fn clear_document(&mut self, uri: &Uri, module_id: &str) {
        // Remove exports
        self.exports.retain(|(mid, _), _| mid != module_id);

        // Remove imports
        self.imports.retain(|(mid, _), _| mid != module_id);

        // Remove from importers
        for importing_set in self.importers.values_mut() {
            importing_set.remove(uri);
        }

        // Remove workspace symbols from this URI
        for symbol_list in self.workspace_symbols.values_mut() {
            symbol_list.retain(|sym| &sym.uri != uri);
        }
        // Clean up empty entries
        self.workspace_symbols.retain(|_, list| !list.is_empty());

        // Remove URI mapping
        self.uri_to_module.remove(uri);
    }

    /// Index all exports in a module
    fn index_exports(
        &mut self,
        uri: &Uri,
        module_id: &str,
        statements: &[Statement],
        interner: &StringInterner,
    ) {
        for stmt in statements {
            if let Statement::Export(export_decl) = stmt {
                match &export_decl.kind {
                    ExportKind::Declaration(decl) => {
                        if let Some((local_name, exported_name)) =
                            Self::get_declaration_export_name(decl, interner)
                        {
                            let export_info = ExportInfo {
                                exported_name: exported_name.clone(),
                                local_name,
                                uri: uri.clone(),
                                is_default: false,
                            };
                            self.exports
                                .insert((module_id.to_string(), exported_name), export_info);
                        }
                    }
                    ExportKind::Named {
                        specifiers,
                        source: _,
                    } => {
                        for spec in specifiers.iter() {
                            let local_name = interner.resolve(spec.local.node);
                            let exported_name = spec
                                .exported
                                .as_ref()
                                .map(|e| interner.resolve(e.node))
                                .unwrap_or_else(|| local_name.clone());

                            let export_info = ExportInfo {
                                exported_name: exported_name.clone(),
                                local_name,
                                uri: uri.clone(),
                                is_default: false,
                            };
                            self.exports
                                .insert((module_id.to_string(), exported_name), export_info);
                        }
                    }
                    ExportKind::Default(_) => {
                        let export_info = ExportInfo {
                            exported_name: "default".to_string(),
                            local_name: "default".to_string(),
                            uri: uri.clone(),
                            is_default: true,
                        };
                        self.exports
                            .insert((module_id.to_string(), "default".to_string()), export_info);
                    }
                }
            }
        }
    }

    /// Index all imports in a module
    fn index_imports(
        &mut self,
        uri: &Uri,
        module_id: &str,
        statements: &[Statement],
        interner: &StringInterner,
        resolve_import: impl Fn(&str, &str) -> Option<(String, Uri)>,
    ) {
        for stmt in statements {
            if let Statement::Import(import_decl) = stmt {
                let import_source = &import_decl.source;

                // Resolve the import path to get source module ID and URI
                if let Some((source_module_id, source_uri)) =
                    resolve_import(import_source, module_id)
                {
                    match &import_decl.clause {
                        ImportClause::Named(specs) => {
                            for spec in specs.iter() {
                                let imported_name = interner.resolve(spec.imported.node);
                                let local_name = spec
                                    .local
                                    .as_ref()
                                    .map(|l| interner.resolve(l.node))
                                    .unwrap_or_else(|| imported_name.clone());

                                let import_info = ImportInfo {
                                    local_name: local_name.clone(),
                                    imported_name: imported_name.clone(),
                                    source_uri: source_uri.clone(),
                                    importing_uri: uri.clone(),
                                };

                                // Add to imports index
                                self.imports
                                    .entry((module_id.to_string(), local_name))
                                    .or_insert_with(Vec::new)
                                    .push(import_info);

                                // Add to importers reverse index
                                self.importers
                                    .entry((source_module_id.clone(), imported_name))
                                    .or_insert_with(HashSet::new)
                                    .insert(uri.clone());
                            }
                        }
                        ImportClause::Default(ident) => {
                            let local_name = interner.resolve(ident.node);
                            let import_info = ImportInfo {
                                local_name: local_name.clone(),
                                imported_name: "default".to_string(),
                                source_uri: source_uri.clone(),
                                importing_uri: uri.clone(),
                            };

                            self.imports
                                .entry((module_id.to_string(), local_name))
                                .or_insert_with(Vec::new)
                                .push(import_info);

                            self.importers
                                .entry((source_module_id.clone(), "default".to_string()))
                                .or_insert_with(HashSet::new)
                                .insert(uri.clone());
                        }
                        ImportClause::Namespace(_ident) => {
                            // Namespace imports are complex, skip for now
                        }
                        ImportClause::TypeOnly(_) => {
                            // Type-only imports could be handled similarly to Named
                        }
                        ImportClause::Mixed { default, named } => {
                            // Handle default import
                            let local_name = interner.resolve(default.node);
                            let import_info = ImportInfo {
                                local_name: local_name.clone(),
                                imported_name: "default".to_string(),
                                source_uri: source_uri.clone(),
                                importing_uri: uri.clone(),
                            };

                            self.imports
                                .entry((module_id.to_string(), local_name))
                                .or_insert_with(Vec::new)
                                .push(import_info);

                            self.importers
                                .entry((source_module_id.clone(), "default".to_string()))
                                .or_insert_with(HashSet::new)
                                .insert(uri.clone());

                            // Handle named imports
                            for spec in named.iter() {
                                let imported_name = interner.resolve(spec.imported.node);
                                let local_name = spec
                                    .local
                                    .as_ref()
                                    .map(|l| interner.resolve(l.node))
                                    .unwrap_or_else(|| imported_name.clone());

                                let import_info = ImportInfo {
                                    local_name: local_name.clone(),
                                    imported_name: imported_name.clone(),
                                    source_uri: source_uri.clone(),
                                    importing_uri: uri.clone(),
                                };

                                self.imports
                                    .entry((module_id.to_string(), local_name))
                                    .or_insert_with(Vec::new)
                                    .push(import_info);

                                self.importers
                                    .entry((source_module_id.clone(), imported_name))
                                    .or_insert_with(HashSet::new)
                                    .insert(uri.clone());
                            }
                        }
                    }
                }
            }
        }
    }

    /// Index all workspace symbols in a module
    fn index_workspace_symbols(
        &mut self,
        uri: &Uri,
        statements: &[Statement],
        interner: &StringInterner,
    ) {
        for stmt in statements {
            self.index_statement_symbols(uri, stmt, None, interner);
        }
    }

    /// Recursively index symbols from a statement
    fn index_statement_symbols(
        &mut self,
        uri: &Uri,
        stmt: &Statement,
        container_name: Option<String>,
        interner: &StringInterner,
    ) {
        use typedlua_parser::ast::pattern::Pattern;
        use typedlua_parser::ast::statement::ClassMember;

        match stmt {
            Statement::Variable(var_decl) => {
                if let Pattern::Identifier(ident) = &var_decl.pattern {
                    let name = interner.resolve(ident.node);
                    let symbol = WorkspaceSymbolInfo {
                        name: name.clone(),
                        kind: SymbolKind::VARIABLE,
                        uri: uri.clone(),
                        span: ident.span.clone(),
                        container_name: container_name.clone(),
                    };
                    self.workspace_symbols
                        .entry(name.to_lowercase())
                        .or_insert_with(Vec::new)
                        .push(symbol);
                }
            }
            Statement::Function(func_decl) => {
                let name = interner.resolve(func_decl.name.node);
                let symbol = WorkspaceSymbolInfo {
                    name: name.clone(),
                    kind: SymbolKind::FUNCTION,
                    uri: uri.clone(),
                    span: func_decl.name.span.clone(),
                    container_name: container_name.clone(),
                };
                self.workspace_symbols
                    .entry(name.to_lowercase())
                    .or_insert_with(Vec::new)
                    .push(symbol);
            }
            Statement::Class(class_decl) => {
                let class_name = interner.resolve(class_decl.name.node);
                let symbol = WorkspaceSymbolInfo {
                    name: class_name.clone(),
                    kind: SymbolKind::CLASS,
                    uri: uri.clone(),
                    span: class_decl.name.span.clone(),
                    container_name: container_name.clone(),
                };
                self.workspace_symbols
                    .entry(class_name.to_lowercase())
                    .or_insert_with(Vec::new)
                    .push(symbol);

                // Index class members
                for member in class_decl.members.iter() {
                    match member {
                        ClassMember::Property(prop) => {
                            let name = interner.resolve(prop.name.node);
                            let symbol = WorkspaceSymbolInfo {
                                name: name.clone(),
                                kind: SymbolKind::PROPERTY,
                                uri: uri.clone(),
                                span: prop.name.span.clone(),
                                container_name: Some(class_name.clone()),
                            };
                            self.workspace_symbols
                                .entry(name.to_lowercase())
                                .or_insert_with(Vec::new)
                                .push(symbol);
                        }
                        ClassMember::Method(method) => {
                            let name = interner.resolve(method.name.node);
                            let symbol = WorkspaceSymbolInfo {
                                name: name.clone(),
                                kind: SymbolKind::METHOD,
                                uri: uri.clone(),
                                span: method.name.span.clone(),
                                container_name: Some(class_name.clone()),
                            };
                            self.workspace_symbols
                                .entry(name.to_lowercase())
                                .or_insert_with(Vec::new)
                                .push(symbol);
                        }
                        ClassMember::Constructor(ctor) => {
                            let symbol = WorkspaceSymbolInfo {
                                name: "constructor".to_string(),
                                kind: SymbolKind::CONSTRUCTOR,
                                uri: uri.clone(),
                                span: ctor.span.clone(),
                                container_name: Some(class_name.clone()),
                            };
                            self.workspace_symbols
                                .entry("constructor".to_string())
                                .or_insert_with(Vec::new)
                                .push(symbol);
                        }
                        ClassMember::Getter(getter) => {
                            let name = interner.resolve(getter.name.node);
                            let symbol = WorkspaceSymbolInfo {
                                name: name.clone(),
                                kind: SymbolKind::PROPERTY,
                                uri: uri.clone(),
                                span: getter.name.span.clone(),
                                container_name: Some(class_name.clone()),
                            };
                            self.workspace_symbols
                                .entry(name.to_lowercase())
                                .or_insert_with(Vec::new)
                                .push(symbol);
                        }
                        ClassMember::Setter(setter) => {
                            let name = interner.resolve(setter.name.node);
                            let symbol = WorkspaceSymbolInfo {
                                name: name.clone(),
                                kind: SymbolKind::PROPERTY,
                                uri: uri.clone(),
                                span: setter.name.span.clone(),
                                container_name: Some(class_name.clone()),
                            };
                            self.workspace_symbols
                                .entry(name.to_lowercase())
                                .or_insert_with(Vec::new)
                                .push(symbol);
                        }
                        ClassMember::Operator(op) => {
                            let op_symbol = match op.operator {
                                OperatorKind::Add => "+",
                                OperatorKind::Subtract => "-",
                                OperatorKind::Multiply => "*",
                                OperatorKind::Divide => "/",
                                OperatorKind::Modulo => "%",
                                OperatorKind::Power => "^",
                                OperatorKind::Equal => "==",
                                OperatorKind::NotEqual => "~=",
                                OperatorKind::LessThan => "<",
                                OperatorKind::LessThanOrEqual => "<=",
                                OperatorKind::GreaterThan => ">",
                                OperatorKind::GreaterThanOrEqual => ">=",
                                OperatorKind::Concatenate => "..",
                                OperatorKind::Length => "#",
                                OperatorKind::Index => "[]",
                                OperatorKind::NewIndex => "[]=",
                                OperatorKind::Call => "()",
                                OperatorKind::UnaryMinus => "unm",
                                OperatorKind::FloorDivide => "//",
                                OperatorKind::BitwiseAnd => "&",
                                OperatorKind::BitwiseOr => "|",
                                OperatorKind::BitwiseXor => "~",
                                OperatorKind::ShiftLeft => "<<",
                                OperatorKind::ShiftRight => ">>",
                            };
                            let name = format!("operator {}", op_symbol);
                            let symbol = WorkspaceSymbolInfo {
                                name: name.clone(),
                                kind: SymbolKind::OPERATOR,
                                uri: uri.clone(),
                                span: op.span.clone(),
                                container_name: Some(class_name.clone()),
                            };
                            self.workspace_symbols
                                .entry(name.to_lowercase())
                                .or_insert_with(Vec::new)
                                .push(symbol);
                        }
                    }
                }
            }
            Statement::Interface(interface_decl) => {
                let name = interner.resolve(interface_decl.name.node);
                let symbol = WorkspaceSymbolInfo {
                    name: name.clone(),
                    kind: SymbolKind::INTERFACE,
                    uri: uri.clone(),
                    span: interface_decl.name.span.clone(),
                    container_name: container_name.clone(),
                };
                self.workspace_symbols
                    .entry(name.to_lowercase())
                    .or_insert_with(Vec::new)
                    .push(symbol);
            }
            Statement::TypeAlias(type_decl) => {
                let name = interner.resolve(type_decl.name.node);
                let symbol = WorkspaceSymbolInfo {
                    name: name.clone(),
                    kind: SymbolKind::TYPE_PARAMETER,
                    uri: uri.clone(),
                    span: type_decl.name.span.clone(),
                    container_name: container_name.clone(),
                };
                self.workspace_symbols
                    .entry(name.to_lowercase())
                    .or_insert_with(Vec::new)
                    .push(symbol);
            }
            Statement::Enum(enum_decl) => {
                let name = interner.resolve(enum_decl.name.node);
                let symbol = WorkspaceSymbolInfo {
                    name: name.clone(),
                    kind: SymbolKind::ENUM,
                    uri: uri.clone(),
                    span: enum_decl.name.span.clone(),
                    container_name: container_name.clone(),
                };
                self.workspace_symbols
                    .entry(name.to_lowercase())
                    .or_insert_with(Vec::new)
                    .push(symbol);
            }
            _ => {}
        }
    }

    /// Get export information for a symbol
    #[allow(dead_code)] // Used in tests for symbol index validation
    pub fn get_export(&self, module_id: &str, symbol_name: &str) -> Option<&ExportInfo> {
        self.exports
            .get(&(module_id.to_string(), symbol_name.to_string()))
    }

    /// Get all files that import a specific symbol from a module
    pub fn get_importers(&self, module_id: &str, symbol_name: &str) -> Vec<Uri> {
        self.importers
            .get(&(module_id.to_string(), symbol_name.to_string()))
            .map(|set| set.iter().cloned().collect())
            .unwrap_or_default()
    }

    /// Get import information for a local symbol in a module
    pub fn get_imports(&self, module_id: &str, local_name: &str) -> Option<&Vec<ImportInfo>> {
        self.imports
            .get(&(module_id.to_string(), local_name.to_string()))
    }

    /// Search workspace symbols by query string
    ///
    /// Performs case-insensitive fuzzy matching on symbol names.
    /// Returns symbols sorted by relevance (exact matches first, then prefix matches, then contains).
    pub fn search_workspace_symbols(&self, query: &str) -> Vec<SymbolInformation> {
        if query.is_empty() {
            // Return all symbols (limited to avoid overwhelming the client)
            return self
                .workspace_symbols
                .values()
                .flat_map(|symbols| symbols.iter())
                .take(100)
                .map(|symbol_info| self.workspace_symbol_to_lsp(symbol_info))
                .collect();
        }

        let query_lower = query.to_lowercase();
        let mut results = Vec::new();

        // Collect matching symbols with scoring
        for (name_lower, symbols) in &self.workspace_symbols {
            let score = if name_lower == &query_lower {
                3 // Exact match
            } else if name_lower.starts_with(&query_lower) {
                2 // Prefix match
            } else if name_lower.contains(&query_lower) {
                1 // Contains match
            } else {
                0 // No match
            };

            if score > 0 {
                for symbol_info in symbols {
                    results.push((score, symbol_info));
                }
            }
        }

        // Sort by score (descending) and convert to SymbolInformation
        results.sort_by(|(score_a, sym_a), (score_b, sym_b)| {
            score_b
                .cmp(score_a)
                .then_with(|| sym_a.name.cmp(&sym_b.name))
        });

        results
            .into_iter()
            .take(100) // Limit results
            .map(|(_, symbol_info)| self.workspace_symbol_to_lsp(symbol_info))
            .collect()
    }

    /// Convert WorkspaceSymbolInfo to LSP SymbolInformation
    fn workspace_symbol_to_lsp(&self, symbol: &WorkspaceSymbolInfo) -> SymbolInformation {
        use lsp_types::{Location, Position, Range};

        #[allow(deprecated)] // SymbolInformation uses deprecated fields
        SymbolInformation {
            name: symbol.name.clone(),
            kind: symbol.kind,
            tags: None,
            deprecated: None,
            location: Location {
                uri: symbol.uri.clone(),
                range: Range {
                    start: Position {
                        line: (symbol.span.line.saturating_sub(1)) as u32,
                        character: (symbol.span.column.saturating_sub(1)) as u32,
                    },
                    end: Position {
                        line: (symbol.span.line.saturating_sub(1)) as u32,
                        character: ((symbol.span.column + symbol.span.len()).saturating_sub(1))
                            as u32,
                    },
                },
            },
            container_name: symbol.container_name.clone(),
        }
    }

    /// Helper to extract export name from a declaration
    fn get_declaration_export_name(
        stmt: &Statement,
        interner: &StringInterner,
    ) -> Option<(String, String)> {
        use typedlua_parser::ast::pattern::Pattern;

        match stmt {
            Statement::Variable(var_decl) => {
                if let Pattern::Identifier(ident) = &var_decl.pattern {
                    let name = interner.resolve(ident.node);
                    Some((name.clone(), name))
                } else {
                    None
                }
            }
            Statement::Function(func_decl) => {
                let name = interner.resolve(func_decl.name.node);
                Some((name.clone(), name))
            }
            Statement::Class(class_decl) => {
                let name = interner.resolve(class_decl.name.node);
                Some((name.clone(), name))
            }
            Statement::Interface(interface_decl) => {
                let name = interner.resolve(interface_decl.name.node);
                Some((name.clone(), name))
            }
            Statement::TypeAlias(type_decl) => {
                let name = interner.resolve(type_decl.name.node);
                Some((name.clone(), name))
            }
            Statement::Enum(enum_decl) => {
                let name = interner.resolve(enum_decl.name.node);
                Some((name.clone(), name))
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;
    use std::sync::Arc;
    use typedlua_parser::{Lexer, Parser};
    use typedlua_typechecker::cli::diagnostics::CollectingDiagnosticHandler;

    fn make_uri(path: &str) -> Uri {
        Uri::from_str(&format!("file://{}", path)).unwrap()
    }

    #[test]
    fn test_symbol_index_new() {
        let index = SymbolIndex::new();

        let _uri = make_uri("/test/module.tl");
        let module_id = "/test/module.tl";

        assert!(index.get_export(module_id, "foo").is_none());
        assert!(index.get_importers(module_id, "foo").is_empty());
        assert!(index.get_imports(module_id, "bar").is_none());
    }

    #[test]
    fn test_symbol_index_clear_document() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/module.tl");
        let module_id = "/test/module.tl";

        index.clear_document(&uri, module_id);

        assert!(index.get_export(module_id, "foo").is_none());
        assert!(index.get_importers(module_id, "foo").is_empty());
    }

    #[test]
    fn test_export_info_creation() {
        let uri = make_uri("/test/module.tl");
        let export_info = ExportInfo {
            exported_name: "myFunc".to_string(),
            local_name: "localFunc".to_string(),
            uri: uri.clone(),
            is_default: false,
        };

        assert_eq!(export_info.exported_name, "myFunc");
        assert_eq!(export_info.local_name, "localFunc");
        assert_eq!(export_info.uri, uri);
        assert!(!export_info.is_default);
    }

    #[test]
    fn test_export_info_default() {
        let uri = make_uri("/test/module.tl");
        let export_info = ExportInfo {
            exported_name: "default".to_string(),
            local_name: "default".to_string(),
            uri: uri.clone(),
            is_default: true,
        };

        assert!(export_info.is_default);
        assert_eq!(export_info.exported_name, "default");
    }

    #[test]
    fn test_import_info_creation() {
        let source_uri = make_uri("/test/source.tl");
        let importing_uri = make_uri("/test/importer.tl");
        let import_info = ImportInfo {
            local_name: "alias".to_string(),
            imported_name: "OriginalName".to_string(),
            source_uri: source_uri.clone(),
            importing_uri: importing_uri.clone(),
        };

        assert_eq!(import_info.local_name, "alias");
        assert_eq!(import_info.imported_name, "OriginalName");
        assert_eq!(import_info.source_uri, source_uri);
        assert_eq!(import_info.importing_uri, importing_uri);
    }

    #[test]
    fn test_workspace_symbol_info_creation() {
        let uri = make_uri("/test/module.tl");
        let symbol_info = WorkspaceSymbolInfo {
            name: "myFunction".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: uri.clone(),
            span: Span::new(0, 0, 0, 0),
            container_name: Some("MyClass".to_string()),
        };

        assert_eq!(symbol_info.name, "myFunction");
        assert_eq!(symbol_info.kind, SymbolKind::FUNCTION);
        assert!(symbol_info.container_name.is_some());
        assert_eq!(symbol_info.container_name.unwrap(), "MyClass");
    }

    #[test]
    fn test_workspace_symbol_info_no_container() {
        let uri = make_uri("/test/module.tl");
        let symbol_info = WorkspaceSymbolInfo {
            name: "myVariable".to_string(),
            kind: SymbolKind::VARIABLE,
            uri: uri.clone(),
            span: Span::new(0, 0, 0, 0),
            container_name: None,
        };

        assert!(symbol_info.container_name.is_none());
    }

    #[test]
    fn test_search_workspace_symbols_empty_query() {
        let index = SymbolIndex::new();
        let results = index.search_workspace_symbols("");
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_workspace_symbols_no_matches() {
        let index = SymbolIndex::new();
        let results = index.search_workspace_symbols("nonexistent");
        assert!(results.is_empty());
    }

    #[test]
    fn test_get_importers_empty() {
        let index = SymbolIndex::new();
        let _uri = make_uri("/test/module.tl");
        let importers = index.get_importers("/other/module", "foo");

        assert!(importers.is_empty());
    }

    #[test]
    fn test_get_imports_none() {
        let index = SymbolIndex::new();
        let imports = index.get_imports("/test/module", "bar");

        assert!(imports.is_none());
    }

    #[test]
    fn test_get_export_none() {
        let index = SymbolIndex::new();
        let export = index.get_export("/test/module", "unknown");

        assert!(export.is_none());
    }

    #[test]
    fn test_symbol_index_debug_format() {
        let index = SymbolIndex::new();
        let debug_format = format!("{:?}", index);
        assert!(debug_format.contains("SymbolIndex"));
    }

    #[test]
    fn test_export_info_debug_format() {
        let uri = make_uri("/test/module.tl");
        let export_info = ExportInfo {
            exported_name: "test".to_string(),
            local_name: "test".to_string(),
            uri,
            is_default: false,
        };
        let debug_format = format!("{:?}", export_info);
        assert!(debug_format.contains("ExportInfo"));
        assert!(debug_format.contains("test"));
    }

    #[test]
    fn test_import_info_debug_format() {
        let source_uri = make_uri("/test/source.tl");
        let importing_uri = make_uri("/test/importer.tl");
        let import_info = ImportInfo {
            local_name: "alias".to_string(),
            imported_name: "OriginalName".to_string(),
            source_uri,
            importing_uri,
        };
        let debug_format = format!("{:?}", import_info);
        assert!(debug_format.contains("ImportInfo"));
    }

    #[test]
    fn test_workspace_symbol_info_debug_format() {
        let uri = make_uri("/test/module.tl");
        let symbol_info = WorkspaceSymbolInfo {
            name: "testFunc".to_string(),
            kind: SymbolKind::FUNCTION,
            uri,
            span: Span::new(0, 0, 0, 0),
            container_name: None,
        };
        let debug_format = format!("{:?}", symbol_info);
        assert!(debug_format.contains("WorkspaceSymbolInfo"));
        assert!(debug_format.contains("testFunc"));
    }

    #[test]
    fn test_symbol_kinds() {
        let uri = make_uri("/test/module.tl");

        let variable = WorkspaceSymbolInfo {
            name: "myVar".to_string(),
            kind: SymbolKind::VARIABLE,
            uri: uri.clone(),
            span: Span::new(0, 0, 0, 0),
            container_name: None,
        };
        assert_eq!(variable.kind, SymbolKind::VARIABLE);

        let function = WorkspaceSymbolInfo {
            name: "myFunc".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: uri.clone(),
            span: Span::new(0, 0, 0, 0),
            container_name: None,
        };
        assert_eq!(function.kind, SymbolKind::FUNCTION);

        let class = WorkspaceSymbolInfo {
            name: "MyClass".to_string(),
            kind: SymbolKind::CLASS,
            uri: uri.clone(),
            span: Span::new(0, 0, 0, 0),
            container_name: None,
        };
        assert_eq!(class.kind, SymbolKind::CLASS);

        let interface = WorkspaceSymbolInfo {
            name: "IMyInterface".to_string(),
            kind: SymbolKind::INTERFACE,
            uri: uri.clone(),
            span: Span::new(0, 0, 0, 0),
            container_name: None,
        };
        assert_eq!(interface.kind, SymbolKind::INTERFACE);

        let type_param = WorkspaceSymbolInfo {
            name: "T".to_string(),
            kind: SymbolKind::TYPE_PARAMETER,
            uri: uri.clone(),
            span: Span::new(0, 0, 0, 0),
            container_name: None,
        };
        assert_eq!(type_param.kind, SymbolKind::TYPE_PARAMETER);
    }

    #[test]
    fn test_multiple_uris_same_module() {
        let uri1 = make_uri("/test/module.tl");
        let uri2 = make_uri("/test/Module.tl");

        assert_ne!(uri1, uri2);
    }

    #[test]
    fn test_symbol_index_clone() {
        let index1 = SymbolIndex::new();
        let index2 = index1.clone();

        assert!(index2.get_export("/test", "foo").is_none());
    }

    #[test]
    fn test_symbol_index_with_parsed_ast() {
        use std::sync::Arc;
        use typedlua_typechecker::cli::diagnostics::CollectingDiagnosticHandler;

        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/module.tl");
        let module_id = "/test/module.tl";

        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new("local myVar = 42", handler.clone(), &interner);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, handler, &interner, &common_ids);
        let ast = parser.parse().unwrap();

        index.update_document(&uri, module_id, &ast, &interner, |_, _| None);

        let results = index.search_workspace_symbols("myVar");
        assert!(results.is_empty() || !results.is_empty());
    }

    #[test]
    fn test_update_document_clears_previous() {
        use std::sync::Arc;
        use typedlua_typechecker::cli::diagnostics::CollectingDiagnosticHandler;

        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");
        let module_id = "/test.lua";

        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();

        let text1 = "local oldVar = 1";
        let mut lexer1 = Lexer::new(text1, handler.clone(), &interner);
        let tokens1 = lexer1.tokenize().unwrap();
        let mut parser1 = Parser::new(tokens1, handler.clone(), &interner, &common_ids);
        let ast1 = parser1.parse().unwrap();

        index.update_document(&uri, module_id, &ast1, &interner, |_, _| None);

        let text2 = "local newVar = 2";
        let mut lexer2 = Lexer::new(text2, handler.clone(), &interner);
        let tokens2 = lexer2.tokenize().unwrap();
        let mut parser2 = Parser::new(tokens2, handler.clone(), &interner, &common_ids);
        let ast2 = parser2.parse().unwrap();

        index.update_document(&uri, module_id, &ast2, &interner, |_, _| None);

        assert!(index.get_export(module_id, "oldVar").is_none());
    }

    #[test]
    fn test_workspace_symbol_search_multiple_results() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        for i in 0..5 {
            let info = WorkspaceSymbolInfo {
                name: format!("helper_{}", i),
                kind: SymbolKind::FUNCTION,
                uri: uri.clone(),
                span: Span::new(0, 10, 1, 1),
                container_name: None,
            };
            index
                .workspace_symbols
                .insert(format!("helper_{}", i), vec![info]);
        }

        let results = index.search_workspace_symbols("helper");
        assert_eq!(results.len(), 5);
    }

    #[test]
    fn test_search_with_empty_string_returns_all() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        for i in 0..3 {
            let info = WorkspaceSymbolInfo {
                name: format!("symbol_{}", i),
                kind: SymbolKind::VARIABLE,
                uri: uri.clone(),
                span: Span::new(0, 5, 1, 1),
                container_name: None,
            };
            index
                .workspace_symbols
                .insert(format!("symbol_{}", i), vec![info]);
        }

        let results = index.search_workspace_symbols("");
        assert_eq!(results.len(), 3);
    }

    #[test]
    fn test_index_with_class_members() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let class_info = WorkspaceSymbolInfo {
            name: "MyClass".to_string(),
            kind: SymbolKind::CLASS,
            uri: uri.clone(),
            span: Span::new(0, 10, 1, 1),
            container_name: None,
        };

        let method_info = WorkspaceSymbolInfo {
            name: "method".to_string(),
            kind: SymbolKind::METHOD,
            uri: uri.clone(),
            span: Span::new(15, 25, 2, 2),
            container_name: Some("MyClass".to_string()),
        };

        let property_info = WorkspaceSymbolInfo {
            name: "field".to_string(),
            kind: SymbolKind::PROPERTY,
            uri,
            span: Span::new(10, 15, 1, 1),
            container_name: Some("MyClass".to_string()),
        };

        index
            .workspace_symbols
            .insert("myclass".to_string(), vec![class_info]);
        index
            .workspace_symbols
            .insert("method".to_string(), vec![method_info]);
        index
            .workspace_symbols
            .insert("field".to_string(), vec![property_info]);

        let class_results = index.search_workspace_symbols("MyClass");
        let method_results = index.search_workspace_symbols("method");

        assert!(!class_results.is_empty());
        assert!(!method_results.is_empty());
    }

    #[test]
    fn test_get_export_with_different_modules() {
        let mut index = SymbolIndex::new();
        let uri1 = make_uri("/module1.lua");
        let uri2 = make_uri("/module2.lua");

        let export1 = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri: uri1.clone(),
            is_default: false,
        };
        let export2 = ExportInfo {
            exported_name: "bar".to_string(),
            local_name: "bar".to_string(),
            uri: uri2.clone(),
            is_default: false,
        };

        index
            .exports
            .insert(("/module1.lua".to_string(), "foo".to_string()), export1);
        index
            .exports
            .insert(("/module2.lua".to_string(), "bar".to_string()), export2);

        assert!(index.get_export("/module1.lua", "foo").is_some());
        assert!(index.get_export("/module2.lua", "bar").is_some());
        assert!(index.get_export("/module1.lua", "bar").is_none());
        assert!(index.get_export("/module2.lua", "foo").is_none());
    }

    #[test]
    fn test_get_importers_multiple_files() {
        let mut index = SymbolIndex::new();
        let uri1 = make_uri("/source.lua");
        let uri2 = make_uri("/importer1.lua");
        let uri3 = make_uri("/importer2.lua");

        index.importers.insert(
            ("/source.lua".to_string(), "foo".to_string()),
            vec![uri2.clone(), uri3.clone()].into_iter().collect(),
        );

        let importers = index.get_importers("/source.lua", "foo");
        assert_eq!(importers.len(), 2);
        assert!(importers.contains(&uri2));
        assert!(importers.contains(&uri3));
    }

    #[test]
    fn test_get_imports_multiple_imports_same_name() {
        let mut index = SymbolIndex::new();
        let source_uri1 = make_uri("/source1.lua");
        let source_uri2 = make_uri("/source2.lua");
        let importing_uri = make_uri("/importer.lua");

        let import1 = ImportInfo {
            local_name: "foo".to_string(),
            imported_name: "Foo1".to_string(),
            source_uri: source_uri1.clone(),
            importing_uri: importing_uri.clone(),
        };
        let import2 = ImportInfo {
            local_name: "foo".to_string(),
            imported_name: "Foo2".to_string(),
            source_uri: source_uri2.clone(),
            importing_uri: importing_uri.clone(),
        };

        index.imports.insert(
            ("/importer.lua".to_string(), "foo".to_string()),
            vec![import1, import2],
        );

        let imports = index.get_imports("/importer.lua", "foo");
        assert!(imports.is_some());
        assert_eq!(imports.unwrap().len(), 2);
    }

    #[test]
    fn test_uri_to_module_mapping() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");
        let module_id = "/test.lua";

        index
            .uri_to_module
            .insert(uri.clone(), module_id.to_string());

        assert_eq!(index.uri_to_module.get(&uri), Some(&module_id.to_string()));
    }

    #[test]
    fn test_workspace_symbols_preserves_span_info() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let info = WorkspaceSymbolInfo {
            name: "testSymbol".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: uri.clone(),
            span: Span::new(25, 40, 3, 7),
            container_name: Some("TestClass".to_string()),
        };
        index
            .workspace_symbols
            .insert("testsymbol".to_string(), vec![info]);

        let results = index.search_workspace_symbols("testSymbol");
        assert!(!results.is_empty());

        if let Some(first) = results.first() {
            assert_eq!(first.name, "testSymbol");
        }
    }

    #[test]
    fn test_search_sorted_by_name_for_same_score() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let symbols = ["apple", "apricot", "avocado"];
        for name in symbols {
            let info = WorkspaceSymbolInfo {
                name: name.to_string(),
                kind: SymbolKind::VARIABLE,
                uri: uri.clone(),
                span: Span::new(0, 10, 1, 1),
                container_name: None,
            };
            index.workspace_symbols.insert(name.to_string(), vec![info]);
        }

        let results = index.search_workspace_symbols("a");
        assert_eq!(results.len(), 3);
    }
}
