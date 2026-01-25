use lsp_types::{SymbolInformation, SymbolKind, Uri};
use std::collections::{HashMap, HashSet};
use typedlua_core::ast::statement::{ExportKind, ImportClause, OperatorKind, Statement};
use typedlua_core::ast::Program;
use typedlua_core::module_resolver::ModuleId;
use typedlua_core::string_interner::StringInterner;
use typedlua_core::Span;

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
#[derive(Debug, Default)]
pub struct SymbolIndex {
    /// Map from (module_id, exported_symbol_name) -> ExportInfo
    exports: HashMap<(ModuleId, String), ExportInfo>,

    /// Map from (module_id, local_symbol_name) -> Vec<ImportInfo>
    /// Multiple imports because a symbol might be imported from different modules with different names
    imports: HashMap<(ModuleId, String), Vec<ImportInfo>>,

    /// Reverse index: Map from exported (source_module_id, exported_name) -> Set of importing URIs
    /// This answers: "Which files import this symbol?"
    importers: HashMap<(ModuleId, String), HashSet<Uri>>,

    /// Map from URI to ModuleId for quick lookups
    uri_to_module: HashMap<Uri, ModuleId>,

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
        module_id: &ModuleId,
        ast: &Program,
        interner: &StringInterner,
        resolve_import: impl Fn(&str, &ModuleId) -> Option<(ModuleId, Uri)>,
    ) {
        // Clear old entries for this document
        self.clear_document(uri, module_id);

        // Register URI -> ModuleId mapping
        self.uri_to_module.insert(uri.clone(), module_id.clone());

        // Index exports
        self.index_exports(uri, module_id, &ast.statements, interner);

        // Index imports
        self.index_imports(uri, module_id, &ast.statements, interner, resolve_import);

        // Index workspace symbols
        self.index_workspace_symbols(uri, &ast.statements, interner);
    }

    /// Clear index entries for a document
    pub fn clear_document(&mut self, uri: &Uri, module_id: &ModuleId) {
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
        module_id: &ModuleId,
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
                                .insert((module_id.clone(), exported_name), export_info);
                        }
                    }
                    ExportKind::Named {
                        specifiers,
                        source: _,
                    } => {
                        for spec in specifiers {
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
                                .insert((module_id.clone(), exported_name), export_info);
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
                            .insert((module_id.clone(), "default".to_string()), export_info);
                    }
                }
            }
        }
    }

    /// Index all imports in a module
    fn index_imports(
        &mut self,
        uri: &Uri,
        module_id: &ModuleId,
        statements: &[Statement],
        interner: &StringInterner,
        resolve_import: impl Fn(&str, &ModuleId) -> Option<(ModuleId, Uri)>,
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
                            for spec in specs {
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
                                    .entry((module_id.clone(), local_name))
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
                                .entry((module_id.clone(), local_name))
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
        use typedlua_core::ast::pattern::Pattern;
        use typedlua_core::ast::statement::ClassMember;

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
                for member in &class_decl.members {
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
    pub fn get_export(&self, module_id: &ModuleId, symbol_name: &str) -> Option<&ExportInfo> {
        self.exports
            .get(&(module_id.clone(), symbol_name.to_string()))
    }

    /// Get all files that import a specific symbol from a module
    pub fn get_importers(&self, module_id: &ModuleId, symbol_name: &str) -> Vec<Uri> {
        self.importers
            .get(&(module_id.clone(), symbol_name.to_string()))
            .map(|set| set.iter().cloned().collect())
            .unwrap_or_default()
    }

    /// Get import information for a local symbol in a module
    pub fn get_imports(&self, module_id: &ModuleId, local_name: &str) -> Option<&Vec<ImportInfo>> {
        self.imports
            .get(&(module_id.clone(), local_name.to_string()))
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
        use typedlua_core::ast::pattern::Pattern;

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

    fn make_uri(path: &str) -> Uri {
        Uri::from_str(&format!("file://{}", path)).unwrap()
    }

    fn make_module_id(path: &str) -> ModuleId {
        ModuleId::new(std::path::PathBuf::from(path))
    }

    #[test]
    fn test_symbol_index_basic() {
        let index = SymbolIndex::new();

        let _uri = make_uri("/test/module.tl");
        let module_id = make_module_id("/test/module.tl");

        // Test that index starts empty
        assert!(index.get_export(&module_id, "foo").is_none());
        assert!(index.get_importers(&module_id, "foo").is_empty());
    }

    #[test]
    fn test_export_indexing() {
        // This would require parsing actual TypedLua code
        // Skipping for now as it requires full parser setup
    }

    #[test]
    fn test_import_indexing() {
        // This would require parsing actual TypedLua code
        // Skipping for now as it requires full parser setup
    }
}
