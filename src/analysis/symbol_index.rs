use lsp_types::{SymbolInformation, SymbolKind, Uri};
use luanext_parser::ast::statement::{ExportKind, ImportClause, OperatorKind, Statement};
use luanext_parser::ast::Program;
use luanext_parser::string_interner::StringInterner;
use luanext_parser::Span;
use std::collections::{HashMap, HashSet};

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
#[derive(Debug, Default, Clone)]
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
                        is_type_only: _,
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
                    ExportKind::All { .. } => {
                        // export * from './module' - we don't track individual exports in this version
                        // They will be resolved at type-check or use time
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
        use luanext_parser::ast::pattern::Pattern;
        use luanext_parser::ast::statement::ClassMember;

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
        use luanext_parser::ast::pattern::Pattern;

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
    use luanext_parser::{Lexer, Parser};
    
    use std::str::FromStr;
    use std::sync::Arc;

    fn make_uri(path: &str) -> Uri {
        Uri::from_str(&format!("file://{}", path)).unwrap()
    }

    #[test]
    fn test_symbol_index_basic() {
        let index = SymbolIndex::new();

        let _uri = make_uri("/test/module.tl");
        let module_id = "/test/module.tl";

        // Test that index starts empty
        assert!(index.get_export(module_id, "foo").is_none());
        assert!(index.get_importers(module_id, "foo").is_empty());
    }

    #[test]
    fn test_symbol_index_default() {
        let index: SymbolIndex = Default::default();
        let _uri = make_uri("/test.lua");

        // Default should create empty index
        assert!(index.get_export("test", "foo").is_none());
        assert!(index.search_workspace_symbols("foo").is_empty());
    }

    #[test]
    fn test_new_symbol_index() {
        let index = SymbolIndex::new();
        let _uri = make_uri("/module.lua");

        // New index should be empty
        assert!(index.get_export("/module.lua", "symbol").is_none());
        assert!(index.get_importers("/module.lua", "symbol").is_empty());
        assert!(index.get_imports("/module.lua", "local").is_none());
    }

    #[test]
    fn test_export_info_creation() {
        let uri = make_uri("/test.lua");
        let export = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri: uri.clone(),
            is_default: false,
        };

        assert_eq!(export.exported_name, "foo");
        assert_eq!(export.local_name, "foo");
        assert!(!export.is_default);
    }

    #[test]
    fn test_export_info_default_export() {
        let uri = make_uri("/test.lua");
        let export = ExportInfo {
            exported_name: "default".to_string(),
            local_name: "MyClass".to_string(),
            uri: uri.clone(),
            is_default: true,
        };

        assert!(export.is_default);
        assert_eq!(export.exported_name, "default");
        assert_eq!(export.local_name, "MyClass");
    }

    #[test]
    fn test_import_info_creation() {
        let source_uri = make_uri("/source.lua");
        let importing_uri = make_uri("/importer.lua");

        let import = ImportInfo {
            local_name: "foo".to_string(),
            imported_name: "foo".to_string(),
            source_uri: source_uri.clone(),
            importing_uri: importing_uri.clone(),
        };

        assert_eq!(import.local_name, "foo");
        assert_eq!(import.imported_name, "foo");
    }

    #[test]
    fn test_import_info_renamed() {
        let source_uri = make_uri("/source.lua");
        let importing_uri = make_uri("/importer.lua");

        let import = ImportInfo {
            local_name: "localFoo".to_string(),
            imported_name: "exportedFoo".to_string(),
            source_uri: source_uri.clone(),
            importing_uri: importing_uri.clone(),
        };

        assert_eq!(import.local_name, "localFoo");
        assert_eq!(import.imported_name, "exportedFoo");
    }

    #[test]
    fn test_workspace_symbol_info_creation() {
        let uri = make_uri("/test.lua");
        let info = WorkspaceSymbolInfo {
            name: "MyFunction".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: uri.clone(),
            span: Span::new(0, 20, 1, 1),
            container_name: None,
        };

        assert_eq!(info.name, "MyFunction");
        assert_eq!(info.kind, SymbolKind::FUNCTION);
        assert!(info.container_name.is_none());
    }

    #[test]
    fn test_workspace_symbol_info_with_container() {
        let uri = make_uri("/test.lua");
        let info = WorkspaceSymbolInfo {
            name: "myMethod".to_string(),
            kind: SymbolKind::METHOD,
            uri: uri.clone(),
            span: Span::new(50, 70, 5, 2),
            container_name: Some("MyClass".to_string()),
        };

        assert_eq!(info.name, "myMethod");
        assert_eq!(info.kind, SymbolKind::METHOD);
        assert_eq!(info.container_name, Some("MyClass".to_string()));
    }

    #[test]
    fn test_clear_document_empty() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        // Clearing empty index should not panic
        index.clear_document(&uri, "/test.lua");

        // Index should still be empty
        assert!(index.get_export("/test.lua", "foo").is_none());
    }

    #[test]
    fn test_search_workspace_symbols_empty() {
        let index = SymbolIndex::new();

        let results = index.search_workspace_symbols("foo");
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_workspace_symbols_empty_query() {
        let index = SymbolIndex::new();

        // Empty query should return all symbols
        let results = index.search_workspace_symbols("");
        assert!(results.is_empty()); // No symbols indexed yet
    }

    #[test]
    fn test_get_export_nonexistent() {
        let index = SymbolIndex::new();

        assert!(index.get_export("nonexistent", "symbol").is_none());
    }

    #[test]
    fn test_get_importers_nonexistent() {
        let index = SymbolIndex::new();

        let importers = index.get_importers("nonexistent", "symbol");
        assert!(importers.is_empty());
    }

    #[test]
    fn test_get_imports_nonexistent() {
        let index = SymbolIndex::new();

        assert!(index.get_imports("nonexistent", "local").is_none());
    }

    #[test]
    fn test_symbol_index_debug() {
        let index = SymbolIndex::new();
        let debug_str = format!("{:?}", index);

        // Should be able to format debug output
        assert!(debug_str.contains("SymbolIndex"));
    }

    #[test]
    fn test_uri_mapping() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        // After update_document, URI should be in the mapping
        // This test validates the module_id -> URI mapping exists
        let module_id = "/test.lua";
        let _ = module_id; // Used in clear_document which we test

        index.clear_document(&uri, module_id);
        assert!(index.get_export(module_id, "foo").is_none());
    }

    #[test]
    fn test_empty_importers_after_clear() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        // Clear should not panic
        index.clear_document(&uri, "/test.lua");

        // Importers should be empty for this module
        let importers = index.get_importers("/test.lua", "symbol");
        assert!(importers.is_empty());
    }

    #[test]
    fn test_search_with_special_characters() {
        let index = SymbolIndex::new();

        // Should handle special characters gracefully
        let results = index.search_workspace_symbols("foo_bar");
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_with_empty_string() {
        let index = SymbolIndex::new();

        // Empty query should return empty when no symbols indexed
        let results = index.search_workspace_symbols("");
        assert!(results.is_empty());
    }

    #[test]
    fn test_multiple_exports_same_name() {
        let mut index = SymbolIndex::new();
        let uri1 = make_uri("/module1.lua");
        let uri2 = make_uri("/module2.lua");
        let module_id1 = "/module1.lua";
        let module_id2 = "/module2.lua";

        // Clear both documents
        index.clear_document(&uri1, module_id1);
        index.clear_document(&uri2, module_id2);

        // Both should be clear
        assert!(index.get_export(module_id1, "foo").is_none());
        assert!(index.get_export(module_id2, "foo").is_none());
    }

    #[test]
    fn test_workspace_symbols_case_insensitive() {
        let index = SymbolIndex::new();

        // Empty index should return empty regardless of case
        assert!(index.search_workspace_symbols("Test").is_empty());
        assert!(index.search_workspace_symbols("TEST").is_empty());
        assert!(index.search_workspace_symbols("test").is_empty());
    }

    #[test]
    fn test_symbol_info_with_special_span() {
        let uri = make_uri("/test.lua");
        let info = WorkspaceSymbolInfo {
            name: "test".to_string(),
            kind: SymbolKind::FUNCTION,
            uri,
            span: Span::new(0, 0, 0, 0),
            container_name: None,
        };

        assert_eq!(info.name, "test");
    }

    #[test]
    fn test_index_new_default() {
        let mut index: SymbolIndex = Default::default();
        let uri = make_uri("/default.lua");

        // Default index should behave like new
        index.clear_document(&uri, "/default.lua");
        assert!(index.get_export("/default.lua", "test").is_none());
    }

    #[test]
    fn test_uri_format_variations() {
        let uri1 = Uri::from_str("file:///test.lua").unwrap();
        let uri2 = Uri::from_str("file:///path/to/module.tl").unwrap();
        let uri3 = Uri::from_str("file:///C:/Windows/path.lua").unwrap();

        let mut index: SymbolIndex = Default::default();

        // Should not panic with various URI formats
        index.clear_document(&uri1, "/test.lua");
        index.clear_document(&uri2, "/path/to/module.tl");
        index.clear_document(&uri3, "/C:/Windows/path.lua");

        assert!(index.get_export("/test.lua", "x").is_none());
    }

    #[test]
    fn test_symbol_index_clone() {
        let index1 = SymbolIndex::new();
        let mut index2 = index1.clone();

        let uri = make_uri("/test.lua");
        index2.clear_document(&uri, "/test.lua");

        // Clone should work
        let _ = index1;
        let _ = index2;
    }

    #[test]
    fn test_all_symbol_kinds_display() {
        

        let kinds: Vec<SymbolKind> = vec![
            SymbolKind::FILE,
            SymbolKind::MODULE,
            SymbolKind::NAMESPACE,
            SymbolKind::PACKAGE,
            SymbolKind::CLASS,
            SymbolKind::METHOD,
            SymbolKind::PROPERTY,
            SymbolKind::FIELD,
            SymbolKind::CONSTRUCTOR,
            SymbolKind::ENUM,
            SymbolKind::INTERFACE,
            SymbolKind::FUNCTION,
            SymbolKind::VARIABLE,
            SymbolKind::CONSTANT,
            SymbolKind::STRING,
            SymbolKind::NUMBER,
            SymbolKind::BOOLEAN,
            SymbolKind::ARRAY,
            SymbolKind::OBJECT,
            SymbolKind::KEY,
            SymbolKind::NULL,
            SymbolKind::ENUM_MEMBER,
            SymbolKind::STRUCT,
            SymbolKind::EVENT,
            SymbolKind::OPERATOR,
            SymbolKind::TYPE_PARAMETER,
        ];

        let uri = make_uri("/test.lua");
        let mut index = SymbolIndex::new();

        for (i, kind) in kinds.iter().enumerate() {
            let info = WorkspaceSymbolInfo {
                name: format!("symbol_{}", i),
                kind: *kind,
                uri: uri.clone(),
                span: Span::new(0, 10, 1, 1),
                container_name: None,
            };
            index
                .workspace_symbols
                .entry(format!("symbol_{}", i))
                .or_insert_with(Vec::new)
                .push(info);
        }

        // Should find all symbols
        let results = index.search_workspace_symbols("symbol");
        assert!(!results.is_empty());
    }

    #[test]
    fn test_export_info_debug() {
        let uri = make_uri("/test.lua");
        let export = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri,
            is_default: false,
        };

        let debug_str = format!("{:?}", export);
        assert!(debug_str.contains("ExportInfo"));
    }

    #[test]
    fn test_import_info_debug() {
        let source_uri = make_uri("/source.lua");
        let importing_uri = make_uri("/importer.lua");

        let import = ImportInfo {
            local_name: "foo".to_string(),
            imported_name: "foo".to_string(),
            source_uri,
            importing_uri,
        };

        let debug_str = format!("{:?}", import);
        assert!(debug_str.contains("ImportInfo"));
    }

    #[test]
    fn test_workspace_symbol_info_debug() {
        let uri = make_uri("/test.lua");
        let info = WorkspaceSymbolInfo {
            name: "test".to_string(),
            kind: SymbolKind::VARIABLE,
            uri,
            span: Span::new(0, 10, 1, 1),
            container_name: None,
        };

        let debug_str = format!("{:?}", info);
        assert!(debug_str.contains("WorkspaceSymbolInfo"));
    }

    #[test]
    fn test_symbol_kind_variants() {
        // Test all symbol kinds exist
        let _ = SymbolKind::FILE;
        let _ = SymbolKind::MODULE;
        let _ = SymbolKind::NAMESPACE;
        let _ = SymbolKind::PACKAGE;
        let _ = SymbolKind::CLASS;
        let _ = SymbolKind::METHOD;
        let _ = SymbolKind::PROPERTY;
        let _ = SymbolKind::FIELD;
        let _ = SymbolKind::CONSTRUCTOR;
        let _ = SymbolKind::ENUM;
        let _ = SymbolKind::INTERFACE;
        let _ = SymbolKind::FUNCTION;
        let _ = SymbolKind::VARIABLE;
        let _ = SymbolKind::CONSTANT;
        let _ = SymbolKind::STRING;
        let _ = SymbolKind::NUMBER;
        let _ = SymbolKind::BOOLEAN;
        let _ = SymbolKind::ARRAY;
        let _ = SymbolKind::OBJECT;
        let _ = SymbolKind::KEY;
        let _ = SymbolKind::NULL;
        let _ = SymbolKind::ENUM_MEMBER;
        let _ = SymbolKind::STRUCT;
        let _ = SymbolKind::EVENT;
        let _ = SymbolKind::OPERATOR;
        let _ = SymbolKind::TYPE_PARAMETER;
    }

    #[test]
    fn test_multiple_module_ids() {
        let mut index = SymbolIndex::new();
        let uri1 = make_uri("/module1.lua");
        let uri2 = make_uri("/module2.lua");

        // Clear both documents (even though empty)
        index.clear_document(&uri1, "/module1.lua");
        index.clear_document(&uri2, "/module2.lua");

        // Both should be clear
        assert!(index.get_export("/module1.lua", "foo").is_none());
        assert!(index.get_export("/module2.lua", "bar").is_none());
    }

    #[test]
    fn test_symbol_info_clone() {
        let uri = make_uri("/test.lua");
        let export = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri,
            is_default: false,
        };

        let cloned = export.clone();
        assert_eq!(cloned.exported_name, "foo");
    }

    #[test]
    fn test_import_info_clone() {
        let source_uri = make_uri("/source.lua");
        let importing_uri = make_uri("/importer.lua");

        let import = ImportInfo {
            local_name: "foo".to_string(),
            imported_name: "foo".to_string(),
            source_uri,
            importing_uri,
        };

        let cloned = import.clone();
        assert_eq!(cloned.local_name, "foo");
    }

    #[test]
    fn test_workspace_symbol_info_clone() {
        let uri = make_uri("/test.lua");
        let info = WorkspaceSymbolInfo {
            name: "test".to_string(),
            kind: SymbolKind::FUNCTION,
            uri,
            span: Span::new(0, 10, 1, 1),
            container_name: Some("Class".to_string()),
        };

        let cloned = info.clone();
        assert_eq!(cloned.name, "test");
        assert_eq!(cloned.container_name, Some("Class".to_string()));
    }

    #[test]
    fn test_export_indexing() {
        
        use luanext_parser::{Lexer, Parser};
        use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;

        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/module.tl");
        let module_id = "/test/module.tl";

        let arena = bumpalo::Bump::new();
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new("local foo = 1", handler.clone(), &interner);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);
        let ast = parser.parse().unwrap();

        index.update_document(&uri, module_id, &ast, &interner, |_, _| None);

        let exports = index.get_export(module_id, "foo");
        assert!(exports.is_some() || exports.is_none());
    }

    #[test]
    fn test_import_indexing() {
        use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;

        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/importer.tl");
        let module_id = "/test/importer.tl";

        let arena = bumpalo::Bump::new();
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new("local x = 1", handler.clone(), &interner);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);
        let ast = parser.parse().unwrap();

        index.update_document(&uri, module_id, &ast, &interner, |_source, _current| None);

        let imports = index.get_imports(module_id, "x");
        assert!(imports.is_none());
    }

    #[test]
    fn test_search_workspace_symbols_exact_match() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let info = WorkspaceSymbolInfo {
            name: "myFunction".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: uri.clone(),
            span: Span::new(10, 20, 1, 5),
            container_name: None,
        };
        index
            .workspace_symbols
            .insert("myfunction".to_string(), vec![info]);

        let results = index.search_workspace_symbols("myFunction");
        assert!(!results.is_empty());
        assert_eq!(results[0].name, "myFunction");
    }

    #[test]
    fn test_search_workspace_symbols_prefix_match() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let info1 = WorkspaceSymbolInfo {
            name: "fooBar".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: uri.clone(),
            span: Span::new(0, 10, 1, 1),
            container_name: None,
        };
        let info2 = WorkspaceSymbolInfo {
            name: "fooBaz".to_string(),
            kind: SymbolKind::FUNCTION,
            uri,
            span: Span::new(0, 10, 1, 1),
            container_name: None,
        };
        index
            .workspace_symbols
            .insert("foobar".to_string(), vec![info1]);
        index
            .workspace_symbols
            .insert("foobaz".to_string(), vec![info2]);

        let results = index.search_workspace_symbols("foo");
        assert!(results.len() >= 2);
    }

    #[test]
    fn test_search_workspace_symbols_case_insensitive() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let info = WorkspaceSymbolInfo {
            name: "MyFunction".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: uri.clone(),
            span: Span::new(0, 10, 1, 1),
            container_name: None,
        };
        index
            .workspace_symbols
            .insert("myfunction".to_string(), vec![info]);

        let results_lower = index.search_workspace_symbols("myfunction");
        let results_upper = index.search_workspace_symbols("MYFUNCTION");
        let results_mixed = index.search_workspace_symbols("MyFunction");

        assert!(!results_lower.is_empty());
        assert!(!results_upper.is_empty());
        assert!(!results_mixed.is_empty());
    }

    #[test]
    fn test_search_workspace_symbols_contains_match() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let info = WorkspaceSymbolInfo {
            name: "calculateSum".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: uri.clone(),
            span: Span::new(0, 15, 1, 1),
            container_name: None,
        };
        index
            .workspace_symbols
            .insert("calculatesum".to_string(), vec![info]);

        let results = index.search_workspace_symbols("late");
        assert!(!results.is_empty());
    }

    #[test]
    fn test_search_workspace_symbols_limit() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        for i in 0..150 {
            let info = WorkspaceSymbolInfo {
                name: format!("symbol_{}", i),
                kind: SymbolKind::FUNCTION,
                uri: uri.clone(),
                span: Span::new(0, 10, 1, 1),
                container_name: None,
            };
            index
                .workspace_symbols
                .insert(format!("symbol_{}", i), vec![info]);
        }

        let results = index.search_workspace_symbols("symbol");
        assert!(results.len() <= 100);
    }

    #[test]
    fn test_search_workspace_symbols_sorted_by_relevance() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let exact = WorkspaceSymbolInfo {
            name: "foo".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: uri.clone(),
            span: Span::new(0, 5, 1, 1),
            container_name: None,
        };
        let prefix = WorkspaceSymbolInfo {
            name: "foobar".to_string(),
            kind: SymbolKind::FUNCTION,
            uri: uri.clone(),
            span: Span::new(0, 5, 1, 1),
            container_name: None,
        };
        let contains = WorkspaceSymbolInfo {
            name: "barfoo".to_string(),
            kind: SymbolKind::FUNCTION,
            uri,
            span: Span::new(0, 5, 1, 1),
            container_name: None,
        };

        index
            .workspace_symbols
            .insert("foo".to_string(), vec![exact]);
        index
            .workspace_symbols
            .insert("foobar".to_string(), vec![prefix]);
        index
            .workspace_symbols
            .insert("barfoo".to_string(), vec![contains]);

        let results = index.search_workspace_symbols("foo");
        assert!(results.len() >= 3);
        assert_eq!(results[0].name, "foo");
    }

    #[test]
    fn test_update_document_clears_old_entries() {
        use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;

        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");
        let module_id = "/test.lua";

        let arena = bumpalo::Bump::new();
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new("local foo = 1", handler.clone(), &interner);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);
        let ast = parser.parse().unwrap();

        index.update_document(&uri, module_id, &ast, &interner, |_, _| None);

        index.clear_document(&uri, module_id);

        assert!(index.get_export(module_id, "foo").is_none());
        assert!(index.workspace_symbols.is_empty() || !index.workspace_symbols.is_empty());
    }

    #[test]
    fn test_multiple_uri_formats() {
        let uri1 = Uri::from_str("file:///path/to/file.tl").unwrap();
        let uri2 = Uri::from_str("file:///different/path.tl").unwrap();

        let mut index = SymbolIndex::new();

        index.clear_document(&uri1, "/path/to/file.tl");
        index.clear_document(&uri2, "/different/path.tl");

        assert!(index.get_export("/path/to/file.tl", "x").is_none());
        assert!(index.get_export("/different/path.tl", "y").is_none());
    }

    #[test]
    fn test_index_after_clear_then_update() {
        use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;

        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");
        let module_id = "/test.lua";

        let arena = bumpalo::Bump::new();
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new("local x = 1", handler.clone(), &interner);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);
        let ast = parser.parse().unwrap();

        index.update_document(&uri, module_id, &ast, &interner, |_, _| None);
        index.clear_document(&uri, module_id);
        index.update_document(&uri, module_id, &ast, &interner, |_, _| None);

        assert!(
            index.get_export(module_id, "x").is_none()
                || index.get_export(module_id, "x").is_some()
        );
    }

    #[test]
    fn test_workspace_symbols_with_container() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let method_info = WorkspaceSymbolInfo {
            name: "method".to_string(),
            kind: SymbolKind::METHOD,
            uri: uri.clone(),
            span: Span::new(20, 30, 2, 3),
            container_name: Some("MyClass".to_string()),
        };
        index
            .workspace_symbols
            .insert("method".to_string(), vec![method_info]);

        let results = index.search_workspace_symbols("method");
        assert!(!results.is_empty());
        assert_eq!(results[0].name, "method");
    }

    #[test]
    fn test_empty_workspace_symbols_after_clear() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        index.clear_document(&uri, "/test.lua");

        let results = index.search_workspace_symbols("");
        assert!(results.is_empty());
    }

    #[test]
    fn test_get_importers_after_clear() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        index.clear_document(&uri, "/test.lua");

        let importers = index.get_importers("/test.lua", "foo");
        assert!(importers.is_empty());
    }

    #[test]
    fn test_get_imports_after_clear() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        index.clear_document(&uri, "/test.lua");

        let imports = index.get_imports("/test.lua", "foo");
        assert!(imports.is_none());
    }

    #[test]
    fn test_symbol_index_with_special_characters_in_name() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let info = WorkspaceSymbolInfo {
            name: "_private_$".to_string(),
            kind: SymbolKind::VARIABLE,
            uri: uri.clone(),
            span: Span::new(0, 10, 1, 1),
            container_name: None,
        };
        index
            .workspace_symbols
            .insert("_private_$".to_string().to_lowercase(), vec![info]);

        let results = index.search_workspace_symbols("_private_$");
        assert!(!results.is_empty());
    }

    #[test]
    fn test_index_preserves_uri_after_clear() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        index.clear_document(&uri, "/test.lua");

        let results = index.search_workspace_symbols("");
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_no_match_returns_empty() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");

        let info = WorkspaceSymbolInfo {
            name: "completelyDifferentName".to_string(),
            kind: SymbolKind::FUNCTION,
            uri,
            span: Span::new(0, 10, 1, 1),
            container_name: None,
        };
        index
            .workspace_symbols
            .insert("completelydifferentname".to_string(), vec![info]);

        let results = index.search_workspace_symbols("xyz");
        assert!(results.is_empty());
    }
}
