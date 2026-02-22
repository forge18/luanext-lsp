use super::analysis::SymbolIndex;
use super::cache::{GlobalCacheLimiter, ModuleDependencyGraph, ModuleExportsEntry};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, Position, Uri,
};
use luanext_parser::ast::statement::{ExportKind, Statement};
use luanext_parser::ast::Program;
use luanext_parser::diagnostics::CollectingDiagnosticHandler;
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser};
use luanext_typechecker::module_resolver::{ModuleId, ModuleRegistry, ModuleResolver};
use luanext_typechecker::SymbolTable;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Abstraction for document management operations.
///
/// This trait allows for injecting mock document managers in tests
/// and potential future substitution of different document storage backends.
/// Currently implemented by [`DocumentManager`] but used directly
/// rather than via trait objects.
pub trait DocumentManagerTrait {
    fn get(&self, uri: &Uri) -> Option<&Document>;
    fn symbol_index(&self) -> &SymbolIndex;
    fn module_id_to_uri(&self, module_id: &ModuleId) -> Option<&Uri>;
    fn uri_to_module_id(&self, uri: &Uri) -> Option<&ModuleId>;
}

impl DocumentManagerTrait for DocumentManager {
    fn get(&self, uri: &Uri) -> Option<&Document> {
        self.documents.get(uri)
    }

    fn symbol_index(&self) -> &SymbolIndex {
        &self.symbol_index
    }

    fn module_id_to_uri(&self, module_id: &ModuleId) -> Option<&Uri> {
        self.module_id_to_uri.get(module_id)
    }

    fn uri_to_module_id(&self, uri: &Uri) -> Option<&ModuleId> {
        self.uri_to_module_id.get(uri)
    }
}

/// Parsed AST along with its string interner for resolving StringId values
pub type ParsedAst = (
    &'static Program<'static>,
    Arc<StringInterner>,
    Arc<luanext_parser::string_interner::CommonIdentifiers>,
    Arc<bumpalo::Bump>,
);

/// Manages open documents and their cached analysis results
#[derive(Debug)]
pub struct DocumentManager {
    documents: HashMap<Uri, Document>,
    /// Module resolver for import path resolution
    module_resolver: Arc<ModuleResolver>,
    /// Bidirectional mapping between URIs and ModuleIds
    uri_to_module_id: HashMap<Uri, ModuleId>,
    module_id_to_uri: HashMap<ModuleId, Uri>,
    /// Workspace root path
    workspace_root: PathBuf,
    /// Reverse index for fast cross-file symbol lookups
    symbol_index: SymbolIndex,
    /// Strategy analyzer for incremental parsing heuristics
    strategy_analyzer: crate::core::heuristics::ParseStrategyAnalyzer,
    /// Cached module type exports for cross-file queries
    module_exports_cache: HashMap<String, ModuleExportsEntry>,
    /// Dependency graph for cascade invalidation
    dependency_graph: ModuleDependencyGraph,
    /// Global cache memory limiter with LRU eviction
    cache_limiter: GlobalCacheLimiter,
}

/// Represents a single document with cached analysis
pub struct Document {
    pub text: String,
    pub version: i32,
    /// Cached parsed AST with its interner (invalidated on change)
    ast: RefCell<Option<ParsedAst>>,
    /// Cached symbol table (invalidated on change)
    pub symbol_table: Option<Arc<SymbolTable<'static>>>,
    /// Module ID for this document (used for cross-file symbol resolution)
    pub module_id: Option<ModuleId>,
    /// Incremental parse tree for efficient re-parsing (invalidated on change)
    #[cfg(feature = "incremental-parsing")]
    incremental_tree: RefCell<Option<luanext_parser::incremental::IncrementalParseTree<'static>>>,
    /// Performance metrics for incremental vs full parsing
    pub metrics: Arc<crate::core::metrics::ParseMetrics>,
    /// Per-document cache for analysis results (semantic tokens, hover, etc.)
    cache: RefCell<crate::core::cache::DocumentCache>,
}

impl std::fmt::Debug for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Document")
            .field(
                "text",
                &format!("{}...", &self.text.chars().take(50).collect::<String>()),
            )
            .field("version", &self.version)
            .field("ast", &"<cached>")
            .field(
                "symbol_table",
                &self.symbol_table.as_ref().map(|_| "<cached>"),
            )
            .field("module_id", &self.module_id)
            .field("cache", &"<DocumentCache>")
            .finish()
    }
}

impl Document {
    /// Create a test document with minimal setup
    /// Available in both test and non-test builds for integration testing
    pub fn new_test(text: String, version: i32) -> Self {
        Self {
            text,
            version,
            ast: RefCell::new(None),
            symbol_table: None,
            module_id: None,
            #[cfg(feature = "incremental-parsing")]
            incremental_tree: RefCell::new(None),
            metrics: Arc::new(crate::core::metrics::ParseMetrics::new()),
            cache: RefCell::new(crate::core::cache::DocumentCache::new()),
        }
    }

    pub fn get_or_parse_ast(&self) -> Option<ParsedAst> {
        if let Some(cached) = self.ast.borrow().as_ref() {
            return Some((
                cached.0,
                Arc::clone(&cached.1),
                Arc::clone(&cached.2),
                Arc::clone(&cached.3),
            ));
        }

        // No cached AST, do full parse
        #[cfg(feature = "incremental-parsing")]
        {
            self.parse_with_edits(&[])
        }
        #[cfg(not(feature = "incremental-parsing"))]
        {
            self.parse_full()
        }
    }

    /// Parse with optional incremental support
    /// If edits is empty, does full parse. Otherwise attempts incremental parse.
    #[cfg(feature = "incremental-parsing")]
    pub(crate) fn parse_with_edits(
        &self,
        edits: &[luanext_parser::incremental::TextEdit],
    ) -> Option<ParsedAst> {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();

        // Create arena and wrap in Arc immediately to avoid borrow issues
        let arena = Arc::new(bumpalo::Bump::new());

        let mut lexer = Lexer::new(&self.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);

        // Try incremental parsing if we have a previous tree and edits
        let (program, new_tree) = if !edits.is_empty() {
            let prev_tree = self.incremental_tree.borrow();
            match parser.parse_incremental(prev_tree.as_ref(), edits, &self.text) {
                Ok((prog, tree)) => (prog, Some(tree)),
                Err(e) => {
                    // Fallback to full parse on error
                    tracing::warn!(
                        "Incremental parse failed, falling back to full parse: {:?}",
                        e
                    );
                    (parser.parse().ok()?, None)
                }
            }
        } else {
            // No edits or no previous tree - do full parse
            (parser.parse().ok()?, None)
        };

        // Allocate the Program in the arena so the reference remains valid
        // as long as the Arc<Bump> is alive. Taking &program would be a
        // use-after-free since program is a stack local.
        let program_ref = arena.alloc(program);
        let leaked_program: &'static Program<'static> =
            unsafe { &*(program_ref as *const Program<'_> as *const Program<'static>) };

        let interner_arc = Arc::new(interner);
        let common_ids_arc = Arc::new(common_ids);

        *self.ast.borrow_mut() = Some((
            leaked_program,
            Arc::clone(&interner_arc),
            Arc::clone(&common_ids_arc),
            Arc::clone(&arena),
        ));

        // Store the incremental tree if we got one
        if let Some(tree) = new_tree {
            let leaked_tree: luanext_parser::incremental::IncrementalParseTree<'static> =
                unsafe { std::mem::transmute(tree) };
            *self.incremental_tree.borrow_mut() = Some(leaked_tree);
        }

        Some((leaked_program, interner_arc, common_ids_arc, arena))
    }

    /// Full parse without incremental support (used when incremental-parsing feature is disabled)
    #[cfg(not(feature = "incremental-parsing"))]
    pub(crate) fn parse_full(&self) -> Option<ParsedAst> {
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let arena = Arc::new(bumpalo::Bump::new());

        let mut lexer = Lexer::new(&self.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);
        let program = parser.parse().ok()?;

        // Allocate the Program in the arena so the reference remains valid
        // as long as the Arc<Bump> is alive. Taking &program would be a
        // use-after-free since program is a stack local.
        let program_ref = arena.alloc(program);
        let leaked_program: &'static Program<'static> =
            unsafe { &*(program_ref as *const Program<'_> as *const Program<'static>) };

        let interner_arc = Arc::new(interner);
        let common_ids_arc = Arc::new(common_ids);

        *self.ast.borrow_mut() = Some((
            leaked_program,
            Arc::clone(&interner_arc),
            Arc::clone(&common_ids_arc),
            Arc::clone(&arena),
        ));

        Some((leaked_program, interner_arc, common_ids_arc, arena))
    }

    /// Get a reference to the document cache.
    pub fn cache(&self) -> std::cell::Ref<'_, crate::core::cache::DocumentCache> {
        self.cache.borrow()
    }

    /// Get a mutable reference to the document cache.
    pub fn cache_mut(&self) -> std::cell::RefMut<'_, crate::core::cache::DocumentCache> {
        self.cache.borrow_mut()
    }

    #[cfg(test)]
    pub(crate) fn clear_cache(&self) {
        *self.ast.borrow_mut() = None;
        #[cfg(feature = "incremental-parsing")]
        {
            *self.incremental_tree.borrow_mut() = None;
        }
        self.cache.borrow_mut().invalidate_all();
    }
}

impl DocumentManager {
    pub fn new(
        workspace_root: PathBuf,
        _module_registry: Arc<ModuleRegistry>,
        module_resolver: Arc<ModuleResolver>,
    ) -> Self {
        Self {
            documents: HashMap::new(),
            module_resolver,
            uri_to_module_id: HashMap::new(),
            module_id_to_uri: HashMap::new(),
            workspace_root,
            symbol_index: SymbolIndex::new(),
            strategy_analyzer: crate::core::heuristics::ParseStrategyAnalyzer::new(
                crate::core::heuristics::IncrementalConfig::from_env(),
            ),
            module_exports_cache: HashMap::new(),
            dependency_graph: ModuleDependencyGraph::new(),
            cache_limiter: GlobalCacheLimiter::new(),
        }
    }

    /// Get the workspace root path
    pub fn workspace_root(&self) -> &Path {
        &self.workspace_root
    }

    /// Create a test document manager with mock module system
    pub fn new_test() -> Self {
        use luanext_typechecker::cli::fs::MockFileSystem;
        Self::new_test_with_fs(MockFileSystem::new())
    }

    /// Create a test document manager with a pre-populated mock file system.
    ///
    /// Use `MockFileSystem::add_file()` to populate files before calling this.
    pub fn new_test_with_fs(fs: luanext_typechecker::cli::fs::MockFileSystem) -> Self {
        use luanext_typechecker::cli::config::CompilerOptions;

        let workspace_root = PathBuf::from("/test");
        let fs = Arc::new(fs);
        let compiler_options = CompilerOptions::default();
        let module_config =
            luanext_typechecker::module_resolver::ModuleConfig::from_compiler_options(
                &compiler_options,
                &workspace_root,
            );
        let module_registry = Arc::new(ModuleRegistry::new());
        let module_resolver = Arc::new(ModuleResolver::new(
            fs,
            module_config,
            workspace_root.clone(),
        ));

        Self::new(workspace_root, module_registry, module_resolver)
    }

    /// Add a document for testing with explicit module_id (no canonicalization)
    pub fn add_test_document(&mut self, uri: &Uri, module_id_str: &str, text: &str) {
        let module_id = ModuleId::new(PathBuf::from(module_id_str));
        let mut document = Document::new_test(text.to_string(), 1);
        document.module_id = Some(module_id.clone());
        self.uri_to_module_id.insert(uri.clone(), module_id.clone());
        self.module_id_to_uri.insert(module_id, uri.clone());
        self.documents.insert(uri.clone(), document);
    }

    /// Get mutable access to the symbol index for test setup
    pub fn symbol_index_mut(&mut self) -> &mut SymbolIndex {
        &mut self.symbol_index
    }

    pub fn open(&mut self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();

        let module_id = uri
            .as_str()
            .strip_prefix("file://")
            .map(PathBuf::from)
            .and_then(|path| path.canonicalize().ok())
            .map(ModuleId::from);

        let document = Document {
            text: params.text_document.text,
            version: params.text_document.version,
            ast: RefCell::new(None),
            symbol_table: None,
            module_id: module_id.clone(),
            #[cfg(feature = "incremental-parsing")]
            incremental_tree: RefCell::new(None),
            metrics: Arc::new(crate::core::metrics::ParseMetrics::new()),
            cache: RefCell::new(crate::core::cache::DocumentCache::new()),
        };

        if let Some(ref mid) = module_id {
            self.uri_to_module_id
                .insert(uri.clone(), mid.clone() as ModuleId);
            self.module_id_to_uri
                .insert(mid.clone() as ModuleId, uri.clone());
        }

        self.documents.insert(uri, document);
    }

    pub fn change(&mut self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();

        // Cascade invalidation info collected during the mutable borrow,
        // applied after the borrow is released to avoid aliasing.
        let mut cascade_dependents: Option<std::collections::HashSet<String>> = None;

        if let Some(doc) = self.documents.get_mut(&uri) {
            let start_time = std::time::Instant::now();
            doc.version = params.text_document.version;

            // Analyze parse strategy BEFORE building TextEdits
            #[cfg(feature = "incremental-parsing")]
            let statement_count = doc
                .incremental_tree
                .borrow()
                .as_ref()
                .map(|t| t.statements.len());
            #[cfg(not(feature = "incremental-parsing"))]
            let statement_count: Option<usize> = None;

            let strategy = self.strategy_analyzer.analyze_lsp_changes(
                &params.content_changes,
                &doc.text,
                statement_count,
            );

            let use_incremental =
                !matches!(strategy, crate::core::heuristics::ParseStrategy::FullParse);

            // Build TextEdits for incremental parsing
            #[cfg(feature = "incremental-parsing")]
            let mut text_edits = Vec::new();

            // Collect token edits for incremental semantic token updates
            let mut token_edits: Vec<crate::features::semantic::incremental::TokenTextEdit> =
                Vec::new();

            for change in params.content_changes {
                if let Some(range) = change.range {
                    let start_offset = Self::position_to_offset(&doc.text, range.start);
                    let end_offset = Self::position_to_offset(&doc.text, range.end);

                    // Create TextEdit only if using incremental
                    #[cfg(feature = "incremental-parsing")]
                    if use_incremental {
                        text_edits.push(luanext_parser::incremental::TextEdit {
                            range: (start_offset as u32, end_offset as u32),
                            new_text: change.text.clone(),
                        });
                    }

                    // Collect semantic token edit info
                    let new_lines = change.text.matches('\n').count() as u32;
                    let new_last_line_length = if new_lines > 0 {
                        change.text.rsplit('\n').next().map_or(0, |s| s.len()) as u32
                    } else {
                        change.text.len() as u32
                    };
                    token_edits.push(crate::features::semantic::incremental::TokenTextEdit {
                        start_line: range.start.line,
                        start_char: range.start.character,
                        end_line: range.end.line,
                        end_char: range.end.character,
                        new_line_count: new_lines,
                        new_last_line_length,
                    });

                    // Apply the edit to the document text
                    let mut new_text = String::new();
                    new_text.push_str(&doc.text[..start_offset]);
                    new_text.push_str(&change.text);
                    new_text.push_str(&doc.text[end_offset..]);
                    doc.text = new_text;
                } else {
                    // Full document replacement
                    doc.text = change.text;
                    token_edits.clear();
                    #[cfg(feature = "incremental-parsing")]
                    text_edits.clear();
                }
            }

            // Clear AST cache
            *doc.ast.borrow_mut() = None;

            // Try incremental semantic token update for single-line edits
            let mut semantic_tokens_updated = false;
            if token_edits.len() == 1 && token_edits[0].is_single_line() {
                let new_version = doc.version;
                let mut cache = doc.cache.borrow_mut();
                // Get old version before we check - cache stores the version it was computed at
                let old_version = cache.semantic_tokens.version();
                if let Some(prev) = cache.semantic_tokens.get_if_valid(old_version) {
                    let prev_clone = prev.clone();
                    if let Some(updated) =
                        crate::features::semantic::incremental::update_semantic_tokens(
                            &prev_clone,
                            &token_edits[0],
                            format!("{}", new_version),
                        )
                    {
                        cache.semantic_tokens.set(updated, new_version);
                        semantic_tokens_updated = true;
                    }
                }
            }

            if !semantic_tokens_updated {
                // Full invalidation of semantic token cache
                doc.cache.borrow_mut().semantic_tokens.invalidate();
            }

            // Invalidate other caches (hover, completion, type-check result)
            // Note: last_published_diagnostics is intentionally NOT cleared here
            {
                let mut cache = doc.cache.borrow_mut();
                cache.hover_cache.invalidate_all();
                cache.completion_cache.invalidate_all();
                cache.type_check_result.invalidate();
            }

            // Clear incremental tree if forcing full parse
            #[cfg(feature = "incremental-parsing")]
            if !use_incremental {
                *doc.incremental_tree.borrow_mut() = None;
            }

            // Re-parse with incremental support
            if let Some(module_id) = &doc.module_id {
                #[cfg(feature = "incremental-parsing")]
                let parse_result = doc.parse_with_edits(&text_edits);
                #[cfg(not(feature = "incremental-parsing"))]
                let parse_result = doc.parse_full();

                if let Some((ast, interner, _common_ids, _arena)) = parse_result {
                    let elapsed = start_time.elapsed();

                    // Record metrics
                    if use_incremental {
                        doc.metrics.record_incremental_parse(elapsed);
                    } else {
                        doc.metrics.record_full_parse(elapsed);
                    }

                    let resolve_import = |import_path: &str, from_module_id: &str| {
                        self.module_resolver
                            .resolve(import_path, std::path::Path::new(from_module_id))
                            .ok()
                            .and_then(|resolved_module_id| {
                                self.module_id_to_uri.get(&resolved_module_id).map(|uri| {
                                    (resolved_module_id.as_str().to_string(), uri.clone())
                                })
                            })
                    };

                    let exports_changed = self.symbol_index.update_document(
                        &uri,
                        module_id.as_str(),
                        ast,
                        &interner,
                        &resolve_import,
                    );

                    // Update dependency graph
                    let mid_str = module_id.as_str().to_string();
                    self.dependency_graph.clear_module(&mid_str);
                    let deps = Self::extract_dependencies(ast, &mid_str, &resolve_import);
                    for dep in &deps {
                        self.dependency_graph.add_dependency(&mid_str, dep);
                    }

                    // Collect cascade invalidation info (applied after mutable borrow ends)
                    if exports_changed {
                        self.module_exports_cache.remove(&mid_str);
                        let dependents = self.dependency_graph.get_transitive_dependents(&mid_str);
                        for dep in &dependents {
                            self.module_exports_cache.remove(dep);
                        }
                        cascade_dependents = Some(dependents);
                    }

                    // Log metrics every 100 parses
                    if doc.version % 100 == 0 {
                        doc.metrics.maybe_log_stats();
                        let cache = doc.cache();
                        cache.maybe_log_all_stats();
                        if std::env::var("LUANEXT_LSP_CACHE_STATS").is_ok() {
                            tracing::info!(
                                "Cache memory: {} bytes estimated",
                                cache.estimated_bytes()
                            );
                        }
                    }
                }
            }
        }

        // Apply cascade invalidation to dependent documents (outside mutable borrow)
        if let Some(dependents) = cascade_dependents {
            for dep in &dependents {
                if let Some(dep_uri) = self
                    .module_id_to_uri
                    .get(&ModuleId::from(PathBuf::from(dep.as_str())))
                {
                    if let Some(dep_doc) = self.documents.get(dep_uri) {
                        dep_doc.cache.borrow_mut().type_check_result.invalidate();
                    }
                }
            }
        }

        // Update global cache limiter and evict LRU documents if over budget
        let uri_str = uri.to_string();
        if let Some(doc) = self.documents.get(&uri) {
            let bytes = doc.cache().estimated_bytes();
            self.cache_limiter.record_access(&uri_str, bytes);
        }
        let to_evict = self.cache_limiter.documents_to_evict();
        for evict_uri_str in &to_evict {
            for (doc_uri, doc) in &self.documents {
                if doc_uri.to_string() == *evict_uri_str {
                    doc.cache.borrow_mut().invalidate_all();
                    break;
                }
            }
        }
    }

    pub fn save(&mut self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(doc) = self.documents.get_mut(&uri) {
            // If the saved text differs from current text (e.g., formatter ran on save),
            // update the text and invalidate all caches. Otherwise keep caches valid.
            if let Some(saved_text) = params.text {
                if saved_text != doc.text {
                    doc.text = saved_text;
                    *doc.ast.borrow_mut() = None;
                    doc.cache.borrow_mut().invalidate_all();
                }
            }
        }
    }

    pub fn close(&mut self, params: DidCloseTextDocumentParams) {
        let uri = &params.text_document.uri;

        if let Some(module_id) = self.uri_to_module_id.get(uri) {
            let mid_str = module_id.as_str().to_string();
            self.symbol_index.clear_document(uri, &mid_str);
            self.module_exports_cache.remove(&mid_str);

            // Cascade invalidate dependents
            let dependents = self.dependency_graph.get_transitive_dependents(&mid_str);
            for dep in &dependents {
                self.module_exports_cache.remove(dep);
            }

            self.dependency_graph.clear_module(&mid_str);
        }

        self.cache_limiter.remove_document(&uri.to_string());

        if let Some(module_id) = self.uri_to_module_id.remove(uri) {
            self.module_id_to_uri.remove(&module_id);
        }
        self.documents.remove(uri);
    }

    pub fn get(&self, uri: &Uri) -> Option<&Document> {
        self.documents.get(uri)
    }

    pub fn module_id_to_uri(&self, module_id: &ModuleId) -> Option<&Uri> {
        self.module_id_to_uri.get(module_id)
    }

    pub fn module_resolver(&self) -> &Arc<ModuleResolver> {
        &self.module_resolver
    }

    /// Load a module from disk that isn't currently open in the editor.
    ///
    /// Creates a temporary `Document` by reading the file via the module
    /// resolver's filesystem. The document is NOT added to this manager's
    /// tracking maps — it is a read-only snapshot for cross-file queries.
    pub fn load_unopened_module(&self, module_id: &ModuleId) -> Option<Document> {
        let text = self.module_resolver.read_file(module_id.path()).ok()?;
        let mut doc = Document::new_test(text, 0);
        doc.module_id = Some(module_id.clone());
        Some(doc)
    }

    pub fn symbol_index(&self) -> &SymbolIndex {
        &self.symbol_index
    }

    pub fn uri_to_module_id(&self, uri: &Uri) -> Option<&ModuleId> {
        self.uri_to_module_id.get(uri)
    }

    /// Get cached module exports if valid for current document version.
    pub fn get_module_exports(&self, module_id: &str) -> Option<&ModuleExportsEntry> {
        if let Some(entry) = self.module_exports_cache.get(module_id) {
            if let Some(uri) = self
                .module_id_to_uri
                .get(&ModuleId::from(PathBuf::from(module_id)))
            {
                if let Some(doc) = self.documents.get(uri) {
                    if entry.version == doc.version {
                        return Some(entry);
                    }
                }
            }
        }
        None
    }

    /// Compute and cache module exports if not already cached at current version.
    pub fn ensure_module_exports_cached(&mut self, module_id: &str) {
        // Skip if already cached at current version
        if self.get_module_exports(module_id).is_some() {
            return;
        }

        let uri = self
            .module_id_to_uri
            .get(&ModuleId::from(PathBuf::from(module_id)))
            .cloned();
        if let Some(uri) = uri {
            if let Some(doc) = self.documents.get(&uri) {
                let result =
                    crate::core::diagnostics::DiagnosticsProvider::ensure_type_checked(doc);
                let entry = ModuleExportsEntry {
                    symbols: result.symbols,
                    version: doc.version,
                    content_hash: super::cache::content_hash(&doc.text),
                };
                self.module_exports_cache
                    .insert(module_id.to_string(), entry);
            }
        }
    }

    /// Extract module dependency edges from an AST's import and re-export statements.
    fn extract_dependencies(
        ast: &Program,
        module_id: &str,
        resolve_import: &dyn Fn(&str, &str) -> Option<(String, Uri)>,
    ) -> Vec<String> {
        let mut deps = Vec::new();
        for stmt in ast.statements {
            match stmt {
                Statement::Import(import_decl) => {
                    if let Some((dep_module_id, _)) = resolve_import(&import_decl.source, module_id)
                    {
                        deps.push(dep_module_id);
                    }
                }
                Statement::Export(export_decl) => match &export_decl.kind {
                    ExportKind::Named {
                        source: Some(src), ..
                    } => {
                        if let Some((dep_module_id, _)) = resolve_import(src, module_id) {
                            deps.push(dep_module_id);
                        }
                    }
                    ExportKind::All { source, .. } => {
                        if let Some((dep_module_id, _)) = resolve_import(source, module_id) {
                            deps.push(dep_module_id);
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }
        deps
    }

    fn position_to_offset(text: &str, position: Position) -> usize {
        let mut offset = 0;
        let mut current_line = 0;

        for (idx, ch) in text.char_indices() {
            if current_line == position.line {
                if offset == position.character as usize {
                    return idx;
                }
                if ch != '\n' && ch != '\r' {
                    offset += 1;
                }
            }

            if ch == '\n' {
                current_line += 1;
                offset = 0;
            }
        }

        text.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_new_document() {
        let doc = Document::new_test("local x = 1".to_string(), 1);

        assert_eq!(doc.text, "local x = 1");
        assert_eq!(doc.version, 1);
    }

    #[test]
    fn test_empty_document() {
        let doc = Document::new_test("".to_string(), 1);

        assert_eq!(doc.text, "");
        assert_eq!(doc.version, 1);
    }

    #[test]
    fn test_multiline_document() {
        let text = "local x = 1\nlocal y = 2\nreturn x + y";
        let doc = Document::new_test(text.to_string(), 1);

        assert_eq!(doc.text, text);
        assert_eq!(doc.text.lines().count(), 3);
    }

    #[test]
    fn test_position_to_offset_line_0() {
        let doc = Document::new_test("hello world".to_string(), 1);

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(0, 0));
        assert_eq!(offset, 0);

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(0, 5));
        assert_eq!(offset, 5);
    }

    #[test]
    fn test_position_to_offset_multiline() {
        let doc = Document::new_test("line 1\nline 2\nline 3".to_string(), 1);

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(1, 0));
        assert_eq!(offset, 7); // After "line 1\n"

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(2, 0));
        assert_eq!(offset, 14); // After "line 1\nline 2\n"
    }

    #[test]
    fn test_position_to_offset_out_of_bounds() {
        let doc = Document::new_test("hello".to_string(), 1);

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(100, 100));
        assert_eq!(offset, 5); // Returns text.len()
    }

    #[test]
    fn test_get_document_attributes() {
        let doc = Document::new_test("local x = 1".to_string(), 5);

        assert_eq!(doc.text, "local x = 1");
        assert_eq!(doc.version, 5);
    }

    #[test]
    fn test_document_debug() {
        let doc = Document::new_test("local x = 1".to_string(), 1);
        let debug_str = format!("{:?}", doc);

        // Debug should include field names
        assert!(debug_str.contains("Document"));
        assert!(debug_str.contains("local x = 1"));
    }

    #[test]
    fn test_document_debug_truncation() {
        let long_text = "a".repeat(100);
        let doc = Document::new_test(long_text.clone(), 1);
        let debug_str = format!("{:?}", doc);

        // Debug should truncate long text
        assert!(debug_str.contains("..."));
    }

    #[test]
    fn test_position_to_offset_exact_positions() {
        let doc = Document::new_test("abc".to_string(), 1);

        assert_eq!(
            DocumentManager::position_to_offset(&doc.text, Position::new(0, 0)),
            0
        );
        assert_eq!(
            DocumentManager::position_to_offset(&doc.text, Position::new(0, 1)),
            1
        );
        assert_eq!(
            DocumentManager::position_to_offset(&doc.text, Position::new(0, 2)),
            2
        );
        assert_eq!(
            DocumentManager::position_to_offset(&doc.text, Position::new(0, 3)),
            3
        );
    }

    #[test]
    fn test_position_to_offset_with_crlf() {
        let doc = Document::new_test("line1\r\nline2".to_string(), 1);

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(1, 0));
        // Should handle CRLF correctly
        assert!(offset > 5);
    }

    #[test]
    fn test_position_to_offset_past_line_end() {
        let doc = Document::new_test("hi".to_string(), 1);

        // Position past end of line should return text length
        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(0, 100));
        assert_eq!(offset, 2);
    }

    #[test]
    fn test_document_version_increment() {
        let doc1 = Document::new_test("x".to_string(), 1);
        let doc2 = Document::new_test("x".to_string(), 2);
        let doc3 = Document::new_test("x".to_string(), 100);

        assert_eq!(doc1.version, 1);
        assert_eq!(doc2.version, 2);
        assert_eq!(doc3.version, 100);
    }

    #[test]
    fn test_document_empty_lines() {
        let doc = Document::new_test("\n\n\n".to_string(), 1);

        assert_eq!(doc.text.lines().count(), 3);
        assert_eq!(doc.text, "\n\n\n");
    }

    #[test]
    fn test_document_with_tabs() {
        let doc = Document::new_test("\tlocal\tx\t=\t1".to_string(), 1);

        assert_eq!(doc.text, "\tlocal\tx\t=\t1");
    }

    #[test]
    fn test_document_unicode() {
        let doc = Document::new_test("local π = 3.14".to_string(), 1);

        assert_eq!(doc.text, "local π = 3.14");

        // Test position calculation with unicode
        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(0, 6));
        // "local " is 6 bytes, "π" is 2 bytes in UTF-8
        assert_eq!(offset, 6);
    }

    #[test]
    fn test_position_to_offset_line_beyond_end() {
        let doc = Document::new_test("single line".to_string(), 1);

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(100, 0));
        assert_eq!(offset, 11); // Returns text length
    }

    #[test]
    fn test_position_to_offset_at_newline() {
        let doc = Document::new_test("line1\nline2".to_string(), 1);

        // Position at the newline character
        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(0, 5));
        assert_eq!(offset, 5);

        // First position of second line
        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(1, 0));
        assert_eq!(offset, 6);
    }

    #[test]
    fn test_document_whitespace_only() {
        let doc = Document::new_test("   \n  \n ".to_string(), 1);

        assert_eq!(doc.text, "   \n  \n ");
        assert_eq!(doc.version, 1);
    }

    #[test]
    fn test_document_special_characters() {
        let text = "local x = '🎉' -- emoji!\nprint(x)";
        let doc = Document::new_test(text.to_string(), 1);

        assert_eq!(doc.text, text);
    }

    #[test]
    fn test_document_large_version() {
        let doc = Document::new_test("x".to_string(), i32::MAX);

        assert_eq!(doc.version, i32::MAX);
    }

    #[test]
    fn test_document_negative_version() {
        // Even negative versions should work
        let doc = Document::new_test("x".to_string(), -1);

        assert_eq!(doc.version, -1);
    }

    #[test]
    fn test_position_to_offset_single_character() {
        let doc = Document::new_test("x".to_string(), 1);

        assert_eq!(
            DocumentManager::position_to_offset(&doc.text, Position::new(0, 0)),
            0
        );
        assert_eq!(
            DocumentManager::position_to_offset(&doc.text, Position::new(0, 1)),
            1
        );
    }

    #[test]
    fn test_document_multiple_newline_types() {
        let doc = Document::new_test("unix\nwindows\r\nclassic\r".to_string(), 1);

        // Should handle all types of newlines
        let lines: Vec<&str> = doc.text.lines().collect();
        assert_eq!(lines.len(), 3);
    }

    #[test]
    fn test_position_to_offset_empty_line() {
        let doc = Document::new_test("line1\n\nline3".to_string(), 1);

        // Position at start of empty line
        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(1, 0));
        assert_eq!(offset, 6); // After "line1\n"

        // Position at start of third line
        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(2, 0));
        assert_eq!(offset, 7); // After "line1\n\n"
    }

    #[test]
    fn test_document_trait_implementation() {
        // Test that Document implements Debug
        fn check_traits<T: std::fmt::Debug>() {}
        check_traits::<Document>();
    }

    #[test]
    fn test_document_manager_new() {
        let dm = DocumentManager::new_test();
        assert!(dm.documents.is_empty());
    }

    #[test]
    fn test_document_manager_open_document() {
        let mut dm = DocumentManager::new_test();

        let params = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 1,
                text: "local x = 1".to_string(),
            },
        };

        dm.open(params);

        let uri = Uri::from_str("file:///test.lua").unwrap();
        assert!(dm.get(&uri).is_some());
    }

    #[test]
    fn test_document_manager_close_document() {
        let mut dm = DocumentManager::new_test();

        let open_params = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 1,
                text: "local x = 1".to_string(),
            },
        };

        dm.open(open_params);

        let close_params = DidCloseTextDocumentParams {
            text_document: lsp_types::TextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
            },
        };

        dm.close(close_params);

        let uri = Uri::from_str("file:///test.lua").unwrap();
        assert!(dm.get(&uri).is_none());
    }

    #[test]
    fn test_document_manager_change_document() {
        let mut dm = DocumentManager::new_test();

        let open_params = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 1,
                text: "local x = 1".to_string(),
            },
        };

        dm.open(open_params.clone());

        let change_params = DidChangeTextDocumentParams {
            text_document: lsp_types::VersionedTextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                version: 2,
            },
            content_changes: vec![lsp_types::TextDocumentContentChangeEvent {
                range: Some(lsp_types::Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 9,
                    },
                }),
                range_length: Some(9),
                text: "local y = 2".to_string(),
            }],
        };

        dm.change(change_params);

        let uri = Uri::from_str("file:///test.lua").unwrap();
        let doc = dm.get(&uri).unwrap();
        assert!(doc.text.contains("local y = 2"));
        assert_eq!(doc.version, 2);
    }

    #[test]
    fn test_document_manager_change_document_full() {
        let mut dm = DocumentManager::new_test();

        let open_params = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 1,
                text: "local x = 1".to_string(),
            },
        };

        dm.open(open_params.clone());

        let change_params = DidChangeTextDocumentParams {
            text_document: lsp_types::VersionedTextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                version: 2,
            },
            content_changes: vec![lsp_types::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "completely new content".to_string(),
            }],
        };

        dm.change(change_params);

        let uri = Uri::from_str("file:///test.lua").unwrap();
        let doc = dm.get(&uri).unwrap();
        assert_eq!(doc.text, "completely new content");
    }

    #[test]
    fn test_document_manager_save_document() {
        let mut dm = DocumentManager::new_test();

        let open_params = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 1,
                text: "local x = 1".to_string(),
            },
        };

        dm.open(open_params);

        let save_params = DidSaveTextDocumentParams {
            text_document: lsp_types::TextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
            },
            text: Some("local x = 1".to_string()),
        };

        dm.save(save_params);

        // Save should not remove the document
        let uri = Uri::from_str("file:///test.lua").unwrap();
        assert!(dm.get(&uri).is_some());
    }

    #[test]
    fn test_document_manager_multiple_documents() {
        let mut dm = DocumentManager::new_test();

        let params1 = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test1.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 1,
                text: "local x = 1".to_string(),
            },
        };

        let params2 = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test2.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 1,
                text: "local y = 2".to_string(),
            },
        };

        dm.open(params1);
        dm.open(params2);

        let uri1 = Uri::from_str("file:///test1.lua").unwrap();
        let uri2 = Uri::from_str("file:///test2.lua").unwrap();

        assert!(dm.get(&uri1).is_some());
        assert!(dm.get(&uri2).is_some());
        assert_eq!(dm.documents.len(), 2);
    }

    #[test]
    fn test_document_manager_get_nonexistent() {
        let dm = DocumentManager::new_test();

        let uri = Uri::from_str("file:///nonexistent.lua").unwrap();
        assert!(dm.get(&uri).is_none());
    }

    #[test]
    fn test_document_symbol_index_access() {
        let dm = DocumentManager::new_test();
        let _ = dm.symbol_index();
    }

    #[test]
    fn test_document_module_resolver_access() {
        let dm = DocumentManager::new_test();
        let _ = dm.module_resolver();
    }

    #[test]
    fn test_document_get_or_parse_ast_valid() {
        let doc = Document::new_test("local x = 1".to_string(), 1);
        let result = doc.get_or_parse_ast();

        assert!(result.is_some());
    }

    #[test]
    fn test_document_get_or_parse_ast_empty() {
        let doc = Document::new_test("".to_string(), 1);
        let result = doc.get_or_parse_ast();

        assert!(result.is_some());
    }

    #[test]
    fn test_document_get_or_parse_ast_function() {
        let doc = Document::new_test("function foo() return 1 end".to_string(), 1);
        let result = doc.get_or_parse_ast();

        assert!(result.is_some());
    }

    #[test]
    fn test_document_get_or_parse_ast_caching() {
        let doc = Document::new_test("local x = 1".to_string(), 1);

        let result1 = doc.get_or_parse_ast();
        let result2 = doc.get_or_parse_ast();

        assert!(result1.is_some());
        assert!(result2.is_some());
    }

    #[test]
    fn test_document_clear_cache() {
        let doc = Document::new_test("local x = 1".to_string(), 1);

        let _ = doc.get_or_parse_ast();
        doc.clear_cache();

        // After clearing cache, AST should still be parseable
        let result = doc.get_or_parse_ast();
        assert!(result.is_some());
    }

    #[test]
    fn test_document_manager_uri_to_module_id() {
        let dm = DocumentManager::new_test();
        let uri = Uri::from_str("file:///test.lua").unwrap();

        let result = dm.uri_to_module_id(&uri);
        // Non-opened document should return None
        assert!(result.is_none());
    }

    #[test]
    fn test_document_manager_module_id_to_uri() {
        let dm = DocumentManager::new_test();

        // Create a dummy module id
        let module_id = luanext_typechecker::module_resolver::ModuleId::new("test".into());

        let result = dm.module_id_to_uri(&module_id);
        // Non-opened document should return None
        assert!(result.is_none());
    }

    #[test]
    fn test_document_manager_trait_impl() {
        let dm = DocumentManager::new_test();
        let uri = Uri::from_str("file:///test.lua").unwrap();

        // Test trait implementation
        let result: Option<&Document> = dm.get(&uri);
        assert!(result.is_none());

        let _ = dm.symbol_index();
        let _ = dm.module_id_to_uri(&luanext_typechecker::module_resolver::ModuleId::new(
            "test".into(),
        ));
        let _ = dm.uri_to_module_id(&uri);
    }

    #[test]
    fn test_position_to_offset_long_line() {
        let doc = Document::new_test(
            "this is a very long line of text that needs testing".to_string(),
            1,
        );

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(0, 10));
        assert_eq!(offset, 10);

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(0, 20));
        assert_eq!(offset, 20);
    }

    #[test]
    fn test_position_to_offset_special_chars() {
        let doc = Document::new_test("a!@#$%^&*()b".to_string(), 1);

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(0, 0));
        assert_eq!(offset, 0);

        let offset = DocumentManager::position_to_offset(&doc.text, Position::new(0, 1));
        assert_eq!(offset, 1);
    }

    #[test]
    fn test_document_contains_unicode_emoji() {
        let text = "local message = \"Hello 🌍\"";
        let doc = Document::new_test(text.to_string(), 1);

        assert!(doc.text.contains("🌍"));
    }

    #[test]
    fn test_document_manager_change_clears_cache() {
        let mut dm = DocumentManager::new_test();

        let open_params = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 1,
                text: "local x = 1".to_string(),
            },
        };

        dm.open(open_params);

        let uri = Uri::from_str("file:///test.lua").unwrap();
        {
            let doc = dm.get(&uri).unwrap();
            let _ = doc.get_or_parse_ast();
        }

        let change_params = DidChangeTextDocumentParams {
            text_document: lsp_types::VersionedTextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                version: 2,
            },
            content_changes: vec![lsp_types::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "local x = 2".to_string(),
            }],
        };

        dm.change(change_params);

        let doc = dm.get(&uri).unwrap();
        assert!(doc.text.contains("local x = 2"));
    }

    #[test]
    fn test_document_manager_open_twice_same_uri() {
        let mut dm = DocumentManager::new_test();

        let params1 = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 1,
                text: "version 1".to_string(),
            },
        };

        dm.open(params1);

        let params2 = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 2,
                text: "version 2".to_string(),
            },
        };

        dm.open(params2);

        let uri = Uri::from_str("file:///test.lua").unwrap();
        let doc = dm.get(&uri).unwrap();
        assert!(doc.text.contains("version 2"));
    }

    #[test]
    fn test_document_with_long_unicode() {
        let text = "日本語テスト文字列".repeat(10);
        let doc = Document::new_test(text.clone(), 1);

        assert_eq!(doc.text, text);
    }

    #[test]
    fn test_position_to_offset_with_multibyte_chars() {
        let doc = Document::new_test("αβγδ".to_string(), 1);

        // Each Greek letter is 2 bytes in UTF-8
        // Position is character offset, returns corresponding byte offset
        assert_eq!(
            DocumentManager::position_to_offset(&doc.text, Position::new(0, 0)),
            0
        );
        assert_eq!(
            DocumentManager::position_to_offset(&doc.text, Position::new(0, 1)),
            2
        );
        assert_eq!(
            DocumentManager::position_to_offset(&doc.text, Position::new(0, 2)),
            4
        );
        assert_eq!(
            DocumentManager::position_to_offset(&doc.text, Position::new(0, 3)),
            6
        );
    }

    #[test]
    fn test_document_version_zero() {
        let doc = Document::new_test("x".to_string(), 0);
        assert_eq!(doc.version, 0);
    }

    #[test]
    fn test_document_manager_with_real_path() {
        let dm = DocumentManager::new_test();
        assert!(dm.workspace_root.to_string_lossy().contains("test"));
    }

    #[test]
    fn test_document_cache_accessible() {
        let doc = Document::new_test("local x = 1".to_string(), 1);

        // Cache should be accessible and initially empty
        let cache = doc.cache();
        assert!(!cache.semantic_tokens.is_valid(1));
        assert!(cache.hover_cache.is_empty());
        drop(cache);

        // Mutable access should work too
        let mut cache = doc.cache_mut();
        cache.semantic_tokens.set(
            lsp_types::SemanticTokens {
                result_id: None,
                data: vec![],
            },
            1,
        );
        assert!(cache.semantic_tokens.is_valid(1));
    }

    #[test]
    fn test_document_change_invalidates_cache() {
        let mut dm = DocumentManager::new_test();

        let open_params = DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                language_id: "lua".to_string(),
                version: 1,
                text: "local x = 1".to_string(),
            },
        };
        dm.open(open_params);

        let uri = Uri::from_str("file:///test.lua").unwrap();

        // Populate cache
        {
            let doc = dm.get(&uri).unwrap();
            doc.cache_mut().semantic_tokens.set(
                lsp_types::SemanticTokens {
                    result_id: None,
                    data: vec![],
                },
                1,
            );
            assert!(doc.cache().semantic_tokens.is_valid(1));
        }

        // Change document - should invalidate cache
        let change_params = DidChangeTextDocumentParams {
            text_document: lsp_types::VersionedTextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                version: 2,
            },
            content_changes: vec![lsp_types::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "local x = 2".to_string(),
            }],
        };
        dm.change(change_params);

        // Cache should be invalidated
        let doc = dm.get(&uri).unwrap();
        assert!(!doc.cache().semantic_tokens.is_valid(1));
        assert!(!doc.cache().semantic_tokens.is_valid(2));
    }

    #[test]
    fn test_load_unopened_module_success() {
        use luanext_typechecker::cli::config::CompilerOptions;
        use luanext_typechecker::cli::fs::MockFileSystem;

        let workspace_root = PathBuf::from("/test");
        let mut mock_fs = MockFileSystem::new();
        mock_fs.add_file(
            PathBuf::from("/test/math.luax"),
            "export function add(a: number, b: number): number\n    return a + b\nend",
        );
        let fs = Arc::new(mock_fs);
        let compiler_options = CompilerOptions::default();
        let module_config =
            luanext_typechecker::module_resolver::ModuleConfig::from_compiler_options(
                &compiler_options,
                &workspace_root,
            );
        let module_registry = Arc::new(ModuleRegistry::new());
        let module_resolver = Arc::new(ModuleResolver::new(
            fs,
            module_config,
            workspace_root.clone(),
        ));

        let dm = DocumentManager::new(workspace_root, module_registry, module_resolver);
        let module_id = ModuleId::new(PathBuf::from("/test/math.luax"));

        let doc = dm.load_unopened_module(&module_id);
        assert!(doc.is_some());
        let doc = doc.unwrap();
        assert!(doc.text.contains("export function add"));
        assert_eq!(doc.version, 0);
        assert_eq!(doc.module_id.as_ref().unwrap(), &module_id);
    }

    #[test]
    fn test_load_unopened_module_file_not_found() {
        let dm = DocumentManager::new_test();
        let module_id = ModuleId::new(PathBuf::from("/nonexistent/file.luax"));

        let doc = dm.load_unopened_module(&module_id);
        assert!(doc.is_none());
    }
}
