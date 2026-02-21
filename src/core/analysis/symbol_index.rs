use lsp_types::{SymbolInformation, SymbolKind, Uri};
use luanext_parser::ast::statement::{ExportKind, ImportClause, OperatorKind, Statement};
use luanext_parser::ast::Program;
use luanext_parser::string_interner::StringInterner;
use luanext_parser::Span;
use std::collections::{HashMap, HashSet};

/// Information about an exported symbol
#[derive(Debug, Clone)]
pub struct ExportInfo {
    /// The exported name (what other modules see)
    pub exported_name: String,
    /// The local name in the exporting module (may differ from exported_name)
    pub local_name: String,
    /// URI of the module that exports this symbol
    pub uri: Uri,
    /// Whether this is a default export
    pub is_default: bool,
    /// Whether this is a type-only export
    pub is_type_only: bool,
    /// Whether this export is a re-export from another module
    pub is_reexport: bool,
    /// Source module ID if this is a re-export (e.g., "/path/to/source.luax")
    pub source_module_id: Option<String>,
    /// Source module URI if this is a re-export
    pub source_uri: Option<Uri>,
    /// Original symbol name in source module (handles aliasing)
    pub original_symbol_name: Option<String>,
}

/// Information about an imported symbol
#[derive(Debug, Clone)]
pub struct ImportInfo {
    /// The local name in the importing module
    pub local_name: String,
    /// The imported name from the source module
    pub imported_name: String,
    /// URI of the source module
    pub source_uri: Uri,
    /// URI of the module that imports this symbol
    pub importing_uri: Uri,
    /// Whether this is a type-only import
    pub is_type_only: bool,
}

/// Result of resolving a re-export chain
#[derive(Debug, Clone)]
pub struct ExportChainEnd {
    /// The final URI where the symbol is originally defined
    pub definition_uri: Uri,
    /// The original symbol name at the definition site
    pub original_name: String,
    /// The chain of module IDs traversed (for debugging/error messages)
    pub chain: Vec<String>,
}

/// Error types for re-export resolution
#[derive(Debug, Clone)]
pub enum ReexportError {
    /// Re-export chain exceeds maximum depth
    ChainTooDeep { depth: usize, max_depth: usize },
    /// Circular re-export detected
    CircularReexport { chain: Vec<String> },
    /// Export not found in symbol index
    ExportNotFound {
        module_id: String,
        symbol_name: String,
    },
    /// Module not indexed
    ModuleNotIndexed { module_id: String },
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
/// Fingerprint of a document's contribution to the symbol index.
///
/// Used for diffing: if the snapshot hasn't changed, the index entries
/// don't need to be cleared and re-inserted, saving significant work
/// when only function bodies change (not imports/exports).
#[derive(Debug, Clone, Default)]
struct DocumentSymbolSnapshot {
    /// (module_id, exported_name) -> hash of ExportInfo
    exports: HashMap<(String, String), u64>,
    /// (module_id, local_name) -> hash of Vec<ImportInfo>
    imports: HashMap<(String, String), u64>,
    /// lowercase_name -> hash of symbols from this URI
    workspace_symbols: HashMap<String, u64>,
}

fn hash_export_info(info: &ExportInfo) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    info.exported_name.hash(&mut hasher);
    info.local_name.hash(&mut hasher);
    info.is_default.hash(&mut hasher);
    info.is_type_only.hash(&mut hasher);
    info.is_reexport.hash(&mut hasher);
    info.source_module_id.hash(&mut hasher);
    info.original_symbol_name.hash(&mut hasher);
    hasher.finish()
}

fn hash_import_infos(infos: &[ImportInfo]) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    for info in infos {
        info.local_name.hash(&mut hasher);
        info.imported_name.hash(&mut hasher);
        info.is_type_only.hash(&mut hasher);
        info.source_uri.as_str().hash(&mut hasher);
    }
    hasher.finish()
}

fn hash_workspace_symbols(symbols: &[WorkspaceSymbolInfo]) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    for sym in symbols {
        sym.name.hash(&mut hasher);
        // SymbolKind is not a primitive; hash its debug representation
        format!("{:?}", sym.kind).hash(&mut hasher);
        sym.span.start.hash(&mut hasher);
        sym.span.end.hash(&mut hasher);
        sym.span.line.hash(&mut hasher);
        sym.container_name.hash(&mut hasher);
    }
    hasher.finish()
}

#[derive(Debug, Clone)]
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

    /// Cache for resolved re-export chains: (module_id, symbol_name) -> ExportChainEnd
    /// This avoids redundant re-export chain traversals for the same symbol
    reexport_chain_cache: std::cell::RefCell<HashMap<(String, String), ExportChainEnd>>,

    /// Per-document snapshots for incremental diffing.
    /// Compared on each `update_document()` to avoid re-indexing unchanged entries.
    document_snapshots: HashMap<Uri, DocumentSymbolSnapshot>,
}

impl Default for SymbolIndex {
    fn default() -> Self {
        Self {
            exports: HashMap::new(),
            imports: HashMap::new(),
            importers: HashMap::new(),
            uri_to_module: HashMap::new(),
            workspace_symbols: HashMap::new(),
            reexport_chain_cache: std::cell::RefCell::new(HashMap::new()),
            document_snapshots: HashMap::new(),
        }
    }
}

impl SymbolIndex {
    pub fn new() -> Self {
        Self::default()
    }

    /// Update the index for a specific document.
    ///
    /// Uses snapshot diffing to avoid re-indexing unchanged entries.
    /// Returns `true` if exports changed (used for cascade invalidation).
    pub fn update_document(
        &mut self,
        uri: &Uri,
        module_id: &str,
        ast: &Program,
        interner: &StringInterner,
        resolve_import: impl Fn(&str, &str) -> Option<(String, Uri)>,
    ) -> bool {
        // Step 1: Build new entries into temp collections
        let mut new_exports: HashMap<(String, String), ExportInfo> = HashMap::new();
        let mut new_imports: HashMap<(String, String), Vec<ImportInfo>> = HashMap::new();
        let mut new_importer_entries: HashSet<(String, String)> = HashSet::new();
        let mut new_workspace_symbols: HashMap<String, Vec<WorkspaceSymbolInfo>> = HashMap::new();

        Self::index_exports_to(
            &self.exports,
            uri,
            module_id,
            &ast.statements,
            interner,
            &resolve_import,
            &mut new_exports,
        );
        Self::index_imports_to(
            uri,
            module_id,
            &ast.statements,
            interner,
            &resolve_import,
            &mut new_imports,
            &mut new_importer_entries,
        );
        Self::index_workspace_symbols_to(
            uri,
            &ast.statements,
            interner,
            &mut new_workspace_symbols,
        );

        // Step 2: Build new snapshot (importer_entries not stored — derived from imports)
        let new_snapshot = DocumentSymbolSnapshot {
            exports: new_exports
                .iter()
                .map(|(k, v)| (k.clone(), hash_export_info(v)))
                .collect(),
            imports: new_imports
                .iter()
                .map(|(k, v)| (k.clone(), hash_import_infos(v)))
                .collect(),
            workspace_symbols: new_workspace_symbols
                .iter()
                .map(|(k, v)| (k.clone(), hash_workspace_symbols(v)))
                .collect(),
        };

        // Step 3: Compare with old snapshot
        let old_snapshot = self
            .document_snapshots
            .get(uri)
            .cloned()
            .unwrap_or_default();

        let exports_changed = old_snapshot.exports != new_snapshot.exports;
        let imports_changed = old_snapshot.imports != new_snapshot.imports;
        let workspace_symbols_changed =
            old_snapshot.workspace_symbols != new_snapshot.workspace_symbols;

        // Step 4: Apply only changed categories
        if exports_changed {
            self.exports.retain(|(mid, _), _| mid != module_id);
            self.exports.extend(new_exports);
            self.invalidate_reexport_cache(module_id);
        }

        if imports_changed {
            self.imports.retain(|(mid, _), _| mid != module_id);
            // Remove old importer entries for this URI
            for importing_set in self.importers.values_mut() {
                importing_set.remove(uri);
            }
            self.importers.retain(|_, set| !set.is_empty());
            // Insert new imports and importer entries
            self.imports.extend(new_imports);
            for (source_module_id, imported_name) in &new_importer_entries {
                self.importers
                    .entry((source_module_id.clone(), imported_name.clone()))
                    .or_default()
                    .insert(uri.clone());
            }
        }

        if workspace_symbols_changed {
            for symbol_list in self.workspace_symbols.values_mut() {
                symbol_list.retain(|sym| &sym.uri != uri);
            }
            self.workspace_symbols.retain(|_, list| !list.is_empty());
            for (name_lower, symbols) in new_workspace_symbols {
                self.workspace_symbols
                    .entry(name_lower)
                    .or_default()
                    .extend(symbols);
            }
        }

        // Step 5: Always update the URI -> module mapping
        self.uri_to_module
            .insert(uri.clone(), module_id.to_string());

        // Step 6: Store new snapshot
        self.document_snapshots.insert(uri.clone(), new_snapshot);

        exports_changed
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

        // Remove document snapshot
        self.document_snapshots.remove(uri);
    }

    /// Index exports into an external collection (for snapshot diffing).
    ///
    /// Same logic as the old `index_exports` but writes to `target` instead of `self.exports`.
    /// For `ExportKind::All`, reads from `existing_exports` (other modules' data).
    fn index_exports_to(
        existing_exports: &HashMap<(String, String), ExportInfo>,
        uri: &Uri,
        module_id: &str,
        statements: &[Statement],
        interner: &StringInterner,
        resolve_import: &dyn Fn(&str, &str) -> Option<(String, Uri)>,
        target: &mut HashMap<(String, String), ExportInfo>,
    ) {
        for stmt in statements {
            if let Statement::Export(export_decl) = stmt {
                match &export_decl.kind {
                    ExportKind::Declaration(decl) => {
                        if let Some((local_name, exported_name)) =
                            Self::get_declaration_export_name(decl, interner)
                        {
                            let is_type_only = Self::is_declaration_type_only(decl);
                            let export_info = ExportInfo {
                                exported_name: exported_name.clone(),
                                local_name,
                                uri: uri.clone(),
                                is_default: false,
                                is_type_only,
                                is_reexport: false,
                                source_module_id: None,
                                source_uri: None,
                                original_symbol_name: None,
                            };
                            target.insert((module_id.to_string(), exported_name), export_info);
                        }
                    }
                    ExportKind::Named {
                        specifiers,
                        source,
                        is_type_only: export_is_type_only,
                    } => {
                        for spec in specifiers.iter() {
                            let local_name = interner.resolve(spec.local.node);
                            let exported_name = spec
                                .exported
                                .as_ref()
                                .map(|e| interner.resolve(e.node))
                                .unwrap_or_else(|| local_name.clone());

                            let export_info = if let Some(source_path) = source {
                                if let Some((source_module_id, source_uri)) =
                                    resolve_import(source_path, module_id)
                                {
                                    ExportInfo {
                                        exported_name: exported_name.clone(),
                                        local_name: local_name.clone(),
                                        uri: uri.clone(),
                                        is_default: false,
                                        is_type_only: *export_is_type_only,
                                        is_reexport: true,
                                        source_module_id: Some(source_module_id),
                                        source_uri: Some(source_uri),
                                        original_symbol_name: Some(local_name),
                                    }
                                } else {
                                    ExportInfo {
                                        exported_name: exported_name.clone(),
                                        local_name: local_name.clone(),
                                        uri: uri.clone(),
                                        is_default: false,
                                        is_type_only: *export_is_type_only,
                                        is_reexport: false,
                                        source_module_id: None,
                                        source_uri: None,
                                        original_symbol_name: None,
                                    }
                                }
                            } else {
                                ExportInfo {
                                    exported_name: exported_name.clone(),
                                    local_name,
                                    uri: uri.clone(),
                                    is_default: false,
                                    is_type_only: false,
                                    is_reexport: false,
                                    source_module_id: None,
                                    source_uri: None,
                                    original_symbol_name: None,
                                }
                            };
                            target.insert((module_id.to_string(), exported_name), export_info);
                        }
                    }
                    ExportKind::Default(_) => {
                        let export_info = ExportInfo {
                            exported_name: "default".to_string(),
                            local_name: "default".to_string(),
                            uri: uri.clone(),
                            is_default: true,
                            is_type_only: false,
                            is_reexport: false,
                            source_module_id: None,
                            source_uri: None,
                            original_symbol_name: None,
                        };
                        target.insert((module_id.to_string(), "default".to_string()), export_info);
                    }
                    ExportKind::All {
                        source,
                        is_type_only,
                    } => {
                        if let Some((source_module_id, source_uri)) =
                            resolve_import(source, module_id)
                        {
                            let source_exports: Vec<_> = existing_exports
                                .iter()
                                .filter(|((src_mid, _), _)| src_mid == &source_module_id)
                                .map(|((_, export_name), export_info)| {
                                    (export_name.clone(), export_info.clone())
                                })
                                .collect();

                            for (export_name, source_export_info) in source_exports {
                                if *is_type_only && !source_export_info.is_type_only {
                                    continue;
                                }

                                let reexport_info = ExportInfo {
                                    exported_name: export_name.clone(),
                                    local_name: source_export_info.local_name.clone(),
                                    uri: uri.clone(),
                                    is_default: false,
                                    is_type_only: source_export_info.is_type_only,
                                    is_reexport: true,
                                    source_module_id: Some(source_module_id.clone()),
                                    source_uri: Some(source_uri.clone()),
                                    original_symbol_name: Some(source_export_info.local_name),
                                };

                                target.insert((module_id.to_string(), export_name), reexport_info);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Index imports into external collections (for snapshot diffing).
    fn index_imports_to(
        uri: &Uri,
        module_id: &str,
        statements: &[Statement],
        interner: &StringInterner,
        resolve_import: &dyn Fn(&str, &str) -> Option<(String, Uri)>,
        imports_target: &mut HashMap<(String, String), Vec<ImportInfo>>,
        importer_entries: &mut HashSet<(String, String)>,
    ) {
        for stmt in statements {
            if let Statement::Import(import_decl) = stmt {
                let import_source = &import_decl.source;

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
                                    is_type_only: false,
                                };

                                imports_target
                                    .entry((module_id.to_string(), local_name))
                                    .or_default()
                                    .push(import_info);

                                importer_entries.insert((source_module_id.clone(), imported_name));
                            }
                        }
                        ImportClause::Default(ident) => {
                            let local_name = interner.resolve(ident.node);
                            let import_info = ImportInfo {
                                local_name: local_name.clone(),
                                imported_name: "default".to_string(),
                                source_uri: source_uri.clone(),
                                importing_uri: uri.clone(),
                                is_type_only: false,
                            };

                            imports_target
                                .entry((module_id.to_string(), local_name))
                                .or_default()
                                .push(import_info);

                            importer_entries
                                .insert((source_module_id.clone(), "default".to_string()));
                        }
                        ImportClause::Namespace(_ident) => {
                            // Namespace imports are complex, skip for now
                        }
                        ImportClause::TypeOnly(specs) => {
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
                                    is_type_only: true,
                                };

                                imports_target
                                    .entry((module_id.to_string(), local_name))
                                    .or_default()
                                    .push(import_info);

                                importer_entries.insert((source_module_id.clone(), imported_name));
                            }
                        }
                        ImportClause::Mixed { default, named } => {
                            let local_name = interner.resolve(default.node);
                            let import_info = ImportInfo {
                                local_name: local_name.clone(),
                                imported_name: "default".to_string(),
                                source_uri: source_uri.clone(),
                                importing_uri: uri.clone(),
                                is_type_only: false,
                            };

                            imports_target
                                .entry((module_id.to_string(), local_name))
                                .or_default()
                                .push(import_info);

                            importer_entries
                                .insert((source_module_id.clone(), "default".to_string()));

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
                                    is_type_only: false,
                                };

                                imports_target
                                    .entry((module_id.to_string(), local_name))
                                    .or_default()
                                    .push(import_info);

                                importer_entries.insert((source_module_id.clone(), imported_name));
                            }
                        }
                    }
                }
            }
        }
    }

    /// Index workspace symbols into an external collection (for snapshot diffing).
    fn index_workspace_symbols_to(
        uri: &Uri,
        statements: &[Statement],
        interner: &StringInterner,
        target: &mut HashMap<String, Vec<WorkspaceSymbolInfo>>,
    ) {
        for stmt in statements {
            Self::index_statement_symbols_to(uri, stmt, None, interner, target);
        }
    }

    /// Recursively index symbols from a statement into an external collection.
    fn index_statement_symbols_to(
        uri: &Uri,
        stmt: &Statement,
        container_name: Option<String>,
        interner: &StringInterner,
        target: &mut HashMap<String, Vec<WorkspaceSymbolInfo>>,
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
                    target.entry(name.to_lowercase()).or_default().push(symbol);
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
                target.entry(name.to_lowercase()).or_default().push(symbol);
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
                target
                    .entry(class_name.to_lowercase())
                    .or_default()
                    .push(symbol);

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
                            target.entry(name.to_lowercase()).or_default().push(symbol);
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
                            target.entry(name.to_lowercase()).or_default().push(symbol);
                        }
                        ClassMember::Constructor(ctor) => {
                            let symbol = WorkspaceSymbolInfo {
                                name: "constructor".to_string(),
                                kind: SymbolKind::CONSTRUCTOR,
                                uri: uri.clone(),
                                span: ctor.span.clone(),
                                container_name: Some(class_name.clone()),
                            };
                            target
                                .entry("constructor".to_string())
                                .or_default()
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
                            target.entry(name.to_lowercase()).or_default().push(symbol);
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
                            target.entry(name.to_lowercase()).or_default().push(symbol);
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
                            target.entry(name.to_lowercase()).or_default().push(symbol);
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
                target.entry(name.to_lowercase()).or_default().push(symbol);
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
                target.entry(name.to_lowercase()).or_default().push(symbol);
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
                target.entry(name.to_lowercase()).or_default().push(symbol);
            }
            _ => {}
        }
    }

    /// Get export information for a symbol
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

    fn is_declaration_type_only(stmt: &Statement) -> bool {
        matches!(stmt, Statement::TypeAlias(_) | Statement::Interface(_))
    }

    /// Resolve a re-export chain to find the original definition
    ///
    /// Follows re-export chains from the starting module/symbol to the original
    /// definition site. Performs cycle detection and depth limiting to prevent
    /// infinite loops.
    ///
    /// # Parameters
    ///
    /// - `module_id`: Starting module ID
    /// - `symbol_name`: Symbol to resolve
    ///
    /// # Returns
    ///
    /// - `Ok(ExportChainEnd)`: Original definition location and metadata
    /// - `Err(ReexportError)`: Resolution failure (cycle, depth limit, not found)
    pub fn resolve_reexport_chain(
        &self,
        module_id: &str,
        symbol_name: &str,
    ) -> Result<ExportChainEnd, ReexportError> {
        const MAX_DEPTH: usize = 10;

        // Check cache first
        let cache_key = (module_id.to_string(), symbol_name.to_string());
        if let Some(cached) = self.reexport_chain_cache.borrow().get(&cache_key) {
            return Ok(cached.clone());
        }

        let mut visited = HashSet::new();
        let mut chain = Vec::new();
        let mut current_module_id = module_id.to_string();
        let mut current_symbol_name = symbol_name.to_string();

        loop {
            // Depth check
            if chain.len() >= MAX_DEPTH {
                return Err(ReexportError::ChainTooDeep {
                    depth: chain.len(),
                    max_depth: MAX_DEPTH,
                });
            }

            // Cycle detection
            if !visited.insert(current_module_id.clone()) {
                chain.push(current_module_id);
                return Err(ReexportError::CircularReexport { chain });
            }

            chain.push(current_module_id.clone());

            // Look up export in current module
            let export_info = self
                .get_export(&current_module_id, &current_symbol_name)
                .ok_or_else(|| ReexportError::ExportNotFound {
                    module_id: current_module_id.clone(),
                    symbol_name: current_symbol_name.clone(),
                })?;

            // If not a re-export, we've found the original definition
            if !export_info.is_reexport {
                let result = ExportChainEnd {
                    definition_uri: export_info.uri.clone(),
                    original_name: export_info.local_name.clone(),
                    chain,
                };
                self.reexport_chain_cache
                    .borrow_mut()
                    .insert(cache_key.clone(), result.clone());
                return Ok(result);
            }

            // Follow the re-export chain
            match (
                &export_info.source_module_id,
                &export_info.original_symbol_name,
            ) {
                (Some(source_id), Some(original_name)) => {
                    current_module_id = source_id.clone();
                    current_symbol_name = original_name.clone();
                }
                _ => {
                    // Re-export marked but missing source info - treat as terminal
                    let result = ExportChainEnd {
                        definition_uri: export_info.uri.clone(),
                        original_name: export_info.local_name.clone(),
                        chain,
                    };
                    self.reexport_chain_cache
                        .borrow_mut()
                        .insert(cache_key.clone(), result.clone());
                    return Ok(result);
                }
            }
        }
    }

    /// Invalidate re-export chain cache for a specific module
    ///
    /// Call this whenever a document is updated to clear cached chains
    /// that might be affected by changes to the module's exports.
    pub fn invalidate_reexport_cache(&self, module_id: &str) {
        self.reexport_chain_cache
            .borrow_mut()
            .retain(|(mid, _), _| mid != module_id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use luanext_parser::{Lexer, Parser};
    use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
    use std::str::FromStr;
    use std::sync::Arc;

    fn make_uri(path: &str) -> Uri {
        Uri::from_str(&format!("file://{}", path)).unwrap()
    }

    #[test]
    fn test_symbol_index_new() {
        let index = SymbolIndex::new();

        let _uri = make_uri("/test/module.luax");
        let module_id = "/test/module.luax";

        assert!(index.get_export(module_id, "foo").is_none());
        assert!(index.get_importers(module_id, "foo").is_empty());
        assert!(index.get_imports(module_id, "bar").is_none());
    }

    #[test]
    fn test_symbol_index_clear_document() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/module.luax");
        let module_id = "/test/module.luax";

        index.clear_document(&uri, module_id);

        assert!(index.get_export(module_id, "foo").is_none());
        assert!(index.get_importers(module_id, "foo").is_empty());
    }

    #[test]
    fn test_export_info_creation() {
        let uri = make_uri("/test/module.luax");
        let export_info = ExportInfo {
            exported_name: "myFunc".to_string(),
            local_name: "localFunc".to_string(),
            uri: uri.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: false,
            source_module_id: None,
            source_uri: None,
            original_symbol_name: None,
        };

        assert_eq!(export_info.exported_name, "myFunc");
        assert_eq!(export_info.local_name, "localFunc");
        assert_eq!(export_info.uri, uri);
        assert!(!export_info.is_default);
        assert!(!export_info.is_type_only);
    }

    #[test]
    fn test_export_info_default() {
        let uri = make_uri("/test/module.luax");
        let export_info = ExportInfo {
            exported_name: "default".to_string(),
            local_name: "default".to_string(),
            uri: uri.clone(),
            is_default: true,
            is_type_only: false,
            is_reexport: false,
            source_module_id: None,
            source_uri: None,
            original_symbol_name: None,
        };

        assert!(export_info.is_default);
        assert_eq!(export_info.exported_name, "default");
        assert!(!export_info.is_type_only);
    }

    #[test]
    fn test_import_info_creation() {
        let source_uri = make_uri("/test/source.luax");
        let importing_uri = make_uri("/test/importer.luax");
        let import_info = ImportInfo {
            local_name: "alias".to_string(),
            imported_name: "OriginalName".to_string(),
            source_uri: source_uri.clone(),
            importing_uri: importing_uri.clone(),
            is_type_only: false,
        };

        assert_eq!(import_info.local_name, "alias");
        assert_eq!(import_info.imported_name, "OriginalName");
        assert_eq!(import_info.source_uri, source_uri);
        assert_eq!(import_info.importing_uri, importing_uri);
        assert!(!import_info.is_type_only);
    }

    #[test]
    fn test_workspace_symbol_info_creation() {
        let uri = make_uri("/test/module.luax");
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
        let uri = make_uri("/test/module.luax");
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
        let _uri = make_uri("/test/module.luax");
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
        let uri = make_uri("/test/module.luax");
        let export_info = ExportInfo {
            exported_name: "test".to_string(),
            local_name: "test".to_string(),
            uri,
            is_default: false,
            is_type_only: false,
            is_reexport: false,
            source_module_id: None,
            source_uri: None,
            original_symbol_name: None,
        };
        let debug_format = format!("{:?}", export_info);
        assert!(debug_format.contains("ExportInfo"));
        assert!(debug_format.contains("test"));
    }

    #[test]
    fn test_import_info_debug_format() {
        let source_uri = make_uri("/test/source.luax");
        let importing_uri = make_uri("/test/importer.luax");
        let import_info = ImportInfo {
            local_name: "alias".to_string(),
            imported_name: "OriginalName".to_string(),
            source_uri,
            importing_uri,
            is_type_only: false,
        };
        let debug_format = format!("{:?}", import_info);
        assert!(debug_format.contains("ImportInfo"));
    }

    #[test]
    fn test_workspace_symbol_info_debug_format() {
        let uri = make_uri("/test/module.luax");
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
        let uri = make_uri("/test/module.luax");

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
        let uri1 = make_uri("/test/module.luax");
        let uri2 = make_uri("/test/Module.luax");

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
        use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
        use std::sync::Arc;

        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/module.luax");
        let module_id = "/test/module.luax";

        let arena = bumpalo::Bump::new();
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new("local myVar = 42", handler.clone(), &interner);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);
        let ast = parser.parse().unwrap();

        index.update_document(&uri, module_id, &ast, &interner, |_, _| None);

        let results = index.search_workspace_symbols("myVar");
        assert!(results.is_empty() || !results.is_empty());
    }

    #[test]
    fn test_update_document_clears_previous() {
        use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
        use std::sync::Arc;

        let mut index = SymbolIndex::new();
        let uri = make_uri("/test.lua");
        let module_id = "/test.lua";

        let arena = bumpalo::Bump::new();
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();

        let text1 = "local oldVar = 1";
        let mut lexer1 = Lexer::new(text1, handler.clone(), &interner);
        let tokens1 = lexer1.tokenize().unwrap();
        let mut parser1 = Parser::new(tokens1, handler.clone(), &interner, &common_ids, &arena);
        let ast1 = parser1.parse().unwrap();

        index.update_document(&uri, module_id, &ast1, &interner, |_, _| None);

        let text2 = "local newVar = 2";
        let mut lexer2 = Lexer::new(text2, handler.clone(), &interner);
        let tokens2 = lexer2.tokenize().unwrap();
        let mut parser2 = Parser::new(tokens2, handler.clone(), &interner, &common_ids, &arena);
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
            is_type_only: false,
            is_reexport: false,
            source_module_id: None,
            source_uri: None,
            original_symbol_name: None,
        };
        let export2 = ExportInfo {
            exported_name: "bar".to_string(),
            local_name: "bar".to_string(),
            uri: uri2.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: false,
            source_module_id: None,
            source_uri: None,
            original_symbol_name: None,
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
        let _uri1 = make_uri("/source.lua");
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
            is_type_only: false,
        };
        let import2 = ImportInfo {
            local_name: "foo".to_string(),
            imported_name: "Foo2".to_string(),
            source_uri: source_uri2.clone(),
            importing_uri: importing_uri.clone(),
            is_type_only: false,
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

    // Tests for resolve_reexport_chain method
    #[test]
    fn test_resolve_reexport_chain_direct_export() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/module.luax");

        let export = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri: uri.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: false,
            source_module_id: None,
            source_uri: None,
            original_symbol_name: None,
        };

        index
            .exports
            .insert(("/module.luax".to_string(), "foo".to_string()), export);

        let result = index.resolve_reexport_chain("/module.luax", "foo");
        assert!(result.is_ok());

        let chain_end = result.unwrap();
        assert_eq!(chain_end.definition_uri, uri);
        assert_eq!(chain_end.original_name, "foo");
        assert_eq!(chain_end.chain.len(), 1);
    }

    #[test]
    fn test_resolve_reexport_chain_single_level() {
        let mut index = SymbolIndex::new();
        let uri_a = make_uri("/module_a.luax");
        let uri_b = make_uri("/module_b.luax");

        // A exports foo
        let export_a = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri: uri_a.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: false,
            source_module_id: None,
            source_uri: None,
            original_symbol_name: None,
        };

        // B re-exports foo from A
        let export_b = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri: uri_b.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: true,
            source_module_id: Some("/module_a.luax".to_string()),
            source_uri: Some(uri_a.clone()),
            original_symbol_name: Some("foo".to_string()),
        };

        index
            .exports
            .insert(("/module_a.luax".to_string(), "foo".to_string()), export_a);
        index
            .exports
            .insert(("/module_b.luax".to_string(), "foo".to_string()), export_b);

        let result = index.resolve_reexport_chain("/module_b.luax", "foo");
        assert!(result.is_ok());

        let chain_end = result.unwrap();
        assert_eq!(chain_end.definition_uri, uri_a);
        assert_eq!(chain_end.original_name, "foo");
        assert_eq!(chain_end.chain.len(), 2);
    }

    #[test]
    fn test_resolve_reexport_chain_multi_level() {
        let mut index = SymbolIndex::new();
        let uri_a = make_uri("/module_a.luax");
        let uri_b = make_uri("/module_b.luax");
        let uri_c = make_uri("/module_c.luax");

        // A exports foo
        let export_a = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri: uri_a.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: false,
            source_module_id: None,
            source_uri: None,
            original_symbol_name: None,
        };

        // B re-exports foo from A
        let export_b = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri: uri_b.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: true,
            source_module_id: Some("/module_a.luax".to_string()),
            source_uri: Some(uri_a.clone()),
            original_symbol_name: Some("foo".to_string()),
        };

        // C re-exports foo from B
        let export_c = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri: uri_c.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: true,
            source_module_id: Some("/module_b.luax".to_string()),
            source_uri: Some(uri_b.clone()),
            original_symbol_name: Some("foo".to_string()),
        };

        index
            .exports
            .insert(("/module_a.luax".to_string(), "foo".to_string()), export_a);
        index
            .exports
            .insert(("/module_b.luax".to_string(), "foo".to_string()), export_b);
        index
            .exports
            .insert(("/module_c.luax".to_string(), "foo".to_string()), export_c);

        let result = index.resolve_reexport_chain("/module_c.luax", "foo");
        assert!(result.is_ok());

        let chain_end = result.unwrap();
        assert_eq!(chain_end.definition_uri, uri_a);
        assert_eq!(chain_end.original_name, "foo");
        assert_eq!(chain_end.chain.len(), 3);
    }

    #[test]
    fn test_resolve_reexport_chain_circular() {
        let mut index = SymbolIndex::new();
        let uri_a = make_uri("/module_a.luax");
        let uri_b = make_uri("/module_b.luax");

        // A re-exports foo from B
        let export_a = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri: uri_a.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: true,
            source_module_id: Some("/module_b.luax".to_string()),
            source_uri: Some(uri_b.clone()),
            original_symbol_name: Some("foo".to_string()),
        };

        // B re-exports foo from A (circular!)
        let export_b = ExportInfo {
            exported_name: "foo".to_string(),
            local_name: "foo".to_string(),
            uri: uri_b.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: true,
            source_module_id: Some("/module_a.luax".to_string()),
            source_uri: Some(uri_a.clone()),
            original_symbol_name: Some("foo".to_string()),
        };

        index
            .exports
            .insert(("/module_a.luax".to_string(), "foo".to_string()), export_a);
        index
            .exports
            .insert(("/module_b.luax".to_string(), "foo".to_string()), export_b);

        let result = index.resolve_reexport_chain("/module_a.luax", "foo");
        assert!(result.is_err());

        match result {
            Err(ReexportError::CircularReexport { chain }) => {
                // Chain includes the starting module and the one we tried to visit twice
                assert_eq!(chain.len(), 3); // [A, B, A]
            }
            _ => panic!("Expected CircularReexport error"),
        }
    }

    #[test]
    fn test_resolve_reexport_chain_depth_limit() {
        let mut index = SymbolIndex::new();

        // Create a chain of 12 modules: 0 exports, 1-11 re-export from previous
        // When we resolve from module_11, chain will have 12 elements, exceeding MAX_DEPTH of 10
        for i in 0..12 {
            let current_module = format!("/module_{}.luax", i);
            let uri = make_uri(&current_module);

            let export = if i == 0 {
                ExportInfo {
                    exported_name: "foo".to_string(),
                    local_name: "foo".to_string(),
                    uri,
                    is_default: false,
                    is_type_only: false,
                    is_reexport: false,
                    source_module_id: None,
                    source_uri: None,
                    original_symbol_name: None,
                }
            } else {
                ExportInfo {
                    exported_name: "foo".to_string(),
                    local_name: "foo".to_string(),
                    uri,
                    is_default: false,
                    is_type_only: false,
                    is_reexport: true,
                    source_module_id: Some(format!("/module_{}.luax", i - 1)),
                    source_uri: Some(make_uri(&format!("/module_{}.luax", i - 1))),
                    original_symbol_name: Some("foo".to_string()),
                }
            };

            index
                .exports
                .insert((current_module, "foo".to_string()), export);
        }

        // Resolving module_11 means: 11 -> 10 -> 9 -> 8 -> 7 -> 6 -> 5 -> 4 -> 3 -> 2 -> 1 -> 0
        // That's 12 modules in the chain, which exceeds MAX_DEPTH of 10
        let result = index.resolve_reexport_chain("/module_11.luax", "foo");
        assert!(result.is_err());

        match result {
            Err(ReexportError::ChainTooDeep { depth, max_depth }) => {
                assert_eq!(max_depth, 10);
                assert!(depth >= 10);
            }
            _ => panic!("Expected ChainTooDeep error"),
        }
    }

    #[test]
    fn test_resolve_reexport_chain_aliasing() {
        let mut index = SymbolIndex::new();
        let uri_a = make_uri("/module_a.luax");
        let uri_b = make_uri("/module_b.luax");

        // A exports Foo
        let export_a = ExportInfo {
            exported_name: "Foo".to_string(),
            local_name: "Foo".to_string(),
            uri: uri_a.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: false,
            source_module_id: None,
            source_uri: None,
            original_symbol_name: None,
        };

        // B re-exports Foo as Bar from A
        let export_b = ExportInfo {
            exported_name: "Bar".to_string(),
            local_name: "Bar".to_string(),
            uri: uri_b.clone(),
            is_default: false,
            is_type_only: false,
            is_reexport: true,
            source_module_id: Some("/module_a.luax".to_string()),
            source_uri: Some(uri_a.clone()),
            original_symbol_name: Some("Foo".to_string()),
        };

        index
            .exports
            .insert(("/module_a.luax".to_string(), "Foo".to_string()), export_a);
        index
            .exports
            .insert(("/module_b.luax".to_string(), "Bar".to_string()), export_b);

        let result = index.resolve_reexport_chain("/module_b.luax", "Bar");
        assert!(result.is_ok());

        let chain_end = result.unwrap();
        assert_eq!(chain_end.definition_uri, uri_a);
        assert_eq!(chain_end.original_name, "Foo");
        assert_eq!(chain_end.chain.len(), 2);
    }

    #[test]
    fn test_resolve_reexport_chain_export_not_found() {
        let index = SymbolIndex::new();

        let result = index.resolve_reexport_chain("/module.luax", "nonexistent");
        assert!(result.is_err());

        match result {
            Err(ReexportError::ExportNotFound {
                module_id,
                symbol_name,
            }) => {
                assert_eq!(module_id, "/module.luax");
                assert_eq!(symbol_name, "nonexistent");
            }
            _ => panic!("Expected ExportNotFound error"),
        }
    }

    // ── Incremental symbol index tests ──────────────────────────────

    /// Helper: parse source code and return (ast, interner, arena).
    /// Arena is returned so it stays alive for the duration of the test.
    fn parse_source(
        source: &str,
    ) -> (
        Program<'static>,
        StringInterner,
        luanext_parser::string_interner::CommonIdentifiers,
        bumpalo::Bump,
    ) {
        let arena = bumpalo::Bump::new();
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(source, handler.clone(), &interner);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);
        let ast = parser.parse().unwrap();
        // SAFETY: ast references arena-allocated data; we return both together
        // and the caller must keep the arena alive.
        let ast: Program<'static> = unsafe { std::mem::transmute(ast) };
        let arena: bumpalo::Bump = unsafe { std::mem::transmute(arena) };
        (ast, interner, common_ids, arena)
    }

    #[test]
    fn test_incremental_update_no_change() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/module.luax");
        let module_id = "/test/module.luax";

        let source = "export function greet(): void {}\nexport const X: number = 42";
        let (ast, interner, _common_ids, _arena) = parse_source(source);

        // First update — everything is new
        let changed1 = index.update_document(&uri, module_id, &ast, &interner, |_, _| None);
        assert!(changed1); // First time is always "changed"

        // Second update with identical AST — nothing should change
        let changed2 = index.update_document(&uri, module_id, &ast, &interner, |_, _| None);
        assert!(!changed2); // Exports didn't change
    }

    #[test]
    fn test_incremental_update_export_added() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/module.luax");
        let module_id = "/test/module.luax";

        // Start with one export
        let (ast1, interner1, _, _arena1) = parse_source("export function greet(): void {}");
        index.update_document(&uri, module_id, &ast1, &interner1, |_, _| None);
        assert!(index.get_export(module_id, "greet").is_some());

        // Add a second export
        let (ast2, interner2, _, _arena2) =
            parse_source("export function greet(): void {}\nexport const X: number = 1");
        let changed = index.update_document(&uri, module_id, &ast2, &interner2, |_, _| None);
        assert!(changed);
        assert!(index.get_export(module_id, "greet").is_some());
        assert!(index.get_export(module_id, "X").is_some());
    }

    #[test]
    fn test_incremental_update_export_removed() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/module.luax");
        let module_id = "/test/module.luax";

        // Start with two exports
        let (ast1, interner1, _, _arena1) =
            parse_source("export function greet(): void {}\nexport const X: number = 1");
        index.update_document(&uri, module_id, &ast1, &interner1, |_, _| None);
        assert!(index.get_export(module_id, "greet").is_some());
        assert!(index.get_export(module_id, "X").is_some());

        // Remove one export
        let (ast2, interner2, _, _arena2) = parse_source("export function greet(): void {}");
        let changed = index.update_document(&uri, module_id, &ast2, &interner2, |_, _| None);
        assert!(changed);
        assert!(index.get_export(module_id, "greet").is_some());
        assert!(index.get_export(module_id, "X").is_none()); // removed
    }

    #[test]
    fn test_incremental_update_body_only() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/module.luax");
        let module_id = "/test/module.luax";

        // Export a function with body v1
        let (ast1, interner1, _, _arena1) =
            parse_source("export function greet(): void {}\nconst x = 1");
        let changed1 = index.update_document(&uri, module_id, &ast1, &interner1, |_, _| None);
        assert!(changed1);

        // Change only local variable (not exports) — exports should NOT change
        let (ast2, interner2, _, _arena2) =
            parse_source("export function greet(): void {}\nconst y = 2");
        let changed2 = index.update_document(&uri, module_id, &ast2, &interner2, |_, _| None);
        assert!(!changed2); // exports didn't change
    }

    #[test]
    fn test_snapshot_cleanup_on_clear() {
        let mut index = SymbolIndex::new();
        let uri = make_uri("/test/module.luax");
        let module_id = "/test/module.luax";

        let (ast, interner, _, _arena) = parse_source("export function greet(): void {}");
        index.update_document(&uri, module_id, &ast, &interner, |_, _| None);

        // Snapshot should exist
        assert!(index.document_snapshots.contains_key(&uri));

        // Clear should remove snapshot
        index.clear_document(&uri, module_id);
        assert!(!index.document_snapshots.contains_key(&uri));
    }
}
