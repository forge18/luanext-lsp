use crate::symbol_index::SymbolIndex;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, Position, Uri,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;
use typedlua_core::ast::Program;
use typedlua_core::diagnostics::CollectingDiagnosticHandler;
use typedlua_core::module_resolver::{ModuleId, ModuleRegistry, ModuleResolver};
use typedlua_core::string_interner::StringInterner;
use typedlua_core::typechecker::SymbolTable;
use typedlua_core::{Lexer, Parser};

/// Parsed AST along with its string interner for resolving StringId values
pub type ParsedAst = (
    Arc<Program>,
    Arc<StringInterner>,
    Arc<typedlua_core::string_interner::CommonIdentifiers>,
);

/// Manages open documents and their cached analysis results
#[derive(Debug)]
pub struct DocumentManager {
    documents: HashMap<Uri, Document>,
    /// Module registry for cross-file symbol tracking
    #[allow(dead_code)] // Reserved for future cross-file features
    module_registry: Arc<ModuleRegistry>,
    /// Module resolver for import path resolution
    module_resolver: Arc<ModuleResolver>,
    /// Bidirectional mapping between URIs and ModuleIds
    uri_to_module_id: HashMap<Uri, ModuleId>,
    module_id_to_uri: HashMap<ModuleId, Uri>,
    /// Workspace root path
    #[allow(dead_code)] // Reserved for future workspace-relative path operations
    workspace_root: PathBuf,
    /// Reverse index for fast cross-file symbol lookups
    symbol_index: SymbolIndex,
}

/// Represents a single document with cached analysis
///
/// # Caching Strategy
///
/// The `ast` field uses interior mutability (`RefCell`) to enable transparent caching
/// without requiring mutable access. The cache is populated on first access via
/// `get_or_parse_ast()` and invalidated when document text changes.
///
/// This caching strategy improves performance by avoiding redundant parsing when
/// multiple LSP features (hover, completion, references) need the AST in quick succession.
pub struct Document {
    pub text: String,
    pub version: i32,
    /// Cached parsed AST with its interner (invalidated on change)
    ast: RefCell<Option<ParsedAst>>,
    /// Cached symbol table (invalidated on change) - reserved for future optimization
    pub symbol_table: Option<Arc<SymbolTable>>,
    /// Module ID for this document (used for cross-file symbol resolution)
    pub module_id: Option<ModuleId>,
}

impl std::fmt::Debug for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Document")
            .field(
                "text",
                &format!(
                    "{}... ({} bytes)",
                    &self.text.chars().take(50).collect::<String>(),
                    self.text.len()
                ),
            )
            .field("version", &self.version)
            .field("ast", &"<cached>")
            .field(
                "symbol_table",
                &self.symbol_table.as_ref().map(|_| "<cached>"),
            )
            .field("module_id", &self.module_id)
            .finish()
    }
}

impl Document {
    /// Create a new document for testing
    ///
    /// This constructor is public to allow both unit tests and integration tests
    /// to create test documents without needing to access private fields.
    #[cfg_attr(not(test), allow(dead_code))]
    pub fn new_test(text: String, version: i32) -> Self {
        Self {
            text,
            version,
            ast: RefCell::new(None),
            symbol_table: None,
            module_id: None,
        }
    }

    /// Get or parse the AST for this document, using cached result if available
    ///
    /// This method transparently caches the parsed AST on first access and reuses
    /// it for subsequent calls until the document changes (cache invalidation happens
    /// in `DocumentManager::change()`).
    ///
    /// # Returns
    /// - `Some((Arc<Program>, Arc<StringInterner>))` if parsing succeeds (either from cache or fresh parse)
    /// - `None` if parsing fails
    ///
    /// The returned interner is required to resolve StringId values in the AST.
    pub fn get_or_parse_ast(&self) -> Option<ParsedAst> {
        // Check if we have a cached AST
        if let Some(cached) = self.ast.borrow().as_ref() {
            return Some((
                Arc::clone(&cached.0),
                Arc::clone(&cached.1),
                Arc::clone(&cached.2),
            ));
        }

        // Parse the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (mut interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&self.text, handler.clone(), &mut interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler, &mut interner, &common_ids);
        let program = parser.parse().ok()?;

        let ast_arc = Arc::new(program);
        let interner_arc = Arc::new(interner);
        let common_ids_arc = Arc::new(common_ids);
        *self.ast.borrow_mut() = Some((
            Arc::clone(&ast_arc),
            Arc::clone(&interner_arc),
            Arc::clone(&common_ids_arc),
        ));

        Some((ast_arc, interner_arc, common_ids_arc))
    }

    /// Clear the cached AST (called when document changes)
    pub(crate) fn clear_cache(&self) {
        *self.ast.borrow_mut() = None;
    }
}

#[allow(dead_code)]
impl DocumentManager {
    /// Create a test document manager with mock module system
    /// This is exposed for testing purposes
    pub fn new_test() -> Self {
        use typedlua_core::config::CompilerOptions;
        use typedlua_core::fs::MockFileSystem;

        let workspace_root = PathBuf::from("/test");
        let fs = Arc::new(MockFileSystem::new());
        let compiler_options = CompilerOptions::default();
        let module_config = typedlua_core::module_resolver::ModuleConfig::from_compiler_options(
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
}

impl DocumentManager {
    pub fn new(
        workspace_root: PathBuf,
        module_registry: Arc<ModuleRegistry>,
        module_resolver: Arc<ModuleResolver>,
    ) -> Self {
        Self {
            documents: HashMap::new(),
            module_registry,
            module_resolver,
            uri_to_module_id: HashMap::new(),
            module_id_to_uri: HashMap::new(),
            workspace_root,
            symbol_index: SymbolIndex::new(),
        }
    }

    /// Open a new document
    pub fn open(&mut self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();

        // Convert URI to file path and create ModuleId
        // Uri string format: "file:///path/to/file"
        let module_id = uri
            .as_str()
            .strip_prefix("file://")
            .map(PathBuf::from)
            .and_then(|path| path.canonicalize().ok().map(ModuleId::new));

        let document = Document {
            text: params.text_document.text,
            version: params.text_document.version,
            ast: RefCell::new(None),
            symbol_table: None,
            module_id: module_id.clone(),
        };

        // Update bidirectional mapping
        if let Some(ref mid) = module_id {
            self.uri_to_module_id.insert(uri.clone(), mid.clone());
            self.module_id_to_uri.insert(mid.clone(), uri.clone());
        }

        self.documents.insert(uri.clone(), document);

        // Eagerly parse the document to warm the cache
        if let Some(doc) = self.documents.get(&uri) {
            // Pre-parse to populate the AST cache
            if let Some((ast, interner, _common_ids)) = doc.get_or_parse_ast() {
                // Update symbol index if this document has a module ID
                if let Some(mid) = module_id.as_ref() {
                    let module_resolver = Arc::clone(&self.module_resolver);
                    self.symbol_index.update_document(
                        &uri,
                        mid,
                        &ast,
                        &interner,
                        |import_path, current_module_id| {
                            module_resolver
                                .resolve(import_path, current_module_id.path())
                                .ok()
                                .and_then(|resolved_module_id| {
                                    let resolved_path = resolved_module_id.path();
                                    let resolved_uri = Uri::from_str(&format!(
                                        "file://{}",
                                        resolved_path.display()
                                    ))
                                    .ok()?;
                                    Some((resolved_module_id, resolved_uri))
                                })
                        },
                    );
                }
            }
        }
    }

    /// Handle document changes (incremental)
    pub fn change(&mut self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();

        if let Some(doc) = self.documents.get_mut(&uri) {
            doc.version = params.text_document.version;

            for change in params.content_changes {
                if let Some(range) = change.range {
                    // Apply incremental change
                    let start_offset = Self::position_to_offset(&doc.text, range.start);
                    let end_offset = Self::position_to_offset(&doc.text, range.end);

                    let mut new_text = String::new();
                    new_text.push_str(&doc.text[..start_offset]);
                    new_text.push_str(&change.text);
                    new_text.push_str(&doc.text[end_offset..]);

                    doc.text = new_text;
                } else {
                    // Full document sync
                    doc.text = change.text;
                }
            }

            // Invalidate cached AST and symbol table on change
            doc.clear_cache();
            doc.symbol_table = None;
        }

        // Eagerly re-parse the changed document to warm the cache
        if let Some(doc) = self.documents.get(&uri) {
            // Pre-parse to populate the AST cache
            if let Some((ast, interner, _common_ids)) = doc.get_or_parse_ast() {
                // Update symbol index if this document has a module ID
                if let Some(module_id) = self.uri_to_module_id.get(&uri).cloned() {
                    let module_resolver = Arc::clone(&self.module_resolver);
                    self.symbol_index.update_document(
                        &uri,
                        &module_id,
                        &ast,
                        &interner,
                        |import_path, current_module_id| {
                            module_resolver
                                .resolve(import_path, current_module_id.path())
                                .ok()
                                .and_then(|resolved_module_id| {
                                    let resolved_path = resolved_module_id.path();
                                    let resolved_uri = Uri::from_str(&format!(
                                        "file://{}",
                                        resolved_path.display()
                                    ))
                                    .ok()?;
                                    Some((resolved_module_id, resolved_uri))
                                })
                        },
                    );
                }
            }
        }
    }

    /// Handle document save
    pub fn save(&mut self, _params: DidSaveTextDocumentParams) {
        // Nothing special to do on save for now
        // The document is already up to date from didChange events
    }

    /// Close a document
    pub fn close(&mut self, params: DidCloseTextDocumentParams) {
        let uri = &params.text_document.uri;

        // Clean up symbol index
        if let Some(module_id) = self.uri_to_module_id.get(uri) {
            self.symbol_index.clear_document(uri, module_id);
        }

        // Clean up bidirectional mapping
        if let Some(module_id) = self.uri_to_module_id.remove(uri) {
            self.module_id_to_uri.remove(&module_id);
        }
        self.documents.remove(uri);
    }

    /// Get a document by URI
    pub fn get(&self, uri: &Uri) -> Option<&Document> {
        self.documents.get(uri)
    }

    /// Get mutable reference to a document
    #[allow(dead_code)] // Public API for document management
    pub fn get_mut(&mut self, uri: &Uri) -> Option<&mut Document> {
        self.documents.get_mut(uri)
    }

    /// Get the module registry
    #[allow(dead_code)] // Public API for cross-file features
    pub fn module_registry(&self) -> &Arc<ModuleRegistry> {
        &self.module_registry
    }

    /// Get the module resolver
    pub fn module_resolver(&self) -> &Arc<ModuleResolver> {
        &self.module_resolver
    }

    /// Get workspace root
    #[allow(dead_code)] // Public API for workspace operations
    pub fn workspace_root(&self) -> &PathBuf {
        &self.workspace_root
    }

    /// Get the symbol index
    pub fn symbol_index(&self) -> &SymbolIndex {
        &self.symbol_index
    }

    /// Convert URI to ModuleId
    pub fn uri_to_module_id(&self, uri: &Uri) -> Option<&ModuleId> {
        self.uri_to_module_id.get(uri)
    }

    /// Convert ModuleId to URI
    pub fn module_id_to_uri(&self, module_id: &ModuleId) -> Option<&Uri> {
        self.module_id_to_uri.get(module_id)
    }

    /// Get all open documents
    #[allow(dead_code)] // Public API for document iteration
    pub fn all_documents(&self) -> impl Iterator<Item = (&Uri, &Document)> {
        self.documents.iter()
    }

    /// Get or parse the AST for a document by URI, using cached result if available
    ///
    /// This is a convenience method that combines document lookup with AST parsing/caching.
    ///
    /// # Returns
    /// - `Some((Arc<Program>, Arc<StringInterner>))` if document exists and parsing succeeds
    /// - `None` if document doesn't exist or parsing fails
    #[allow(dead_code)] // Public API for AST access with caching
    pub fn get_or_parse_ast(&self, uri: &Uri) -> Option<ParsedAst> {
        self.get(uri).and_then(|doc| doc.get_or_parse_ast())
    }

    /// Convert LSP Position to byte offset in text
    fn position_to_offset(text: &str, position: Position) -> usize {
        let mut offset = 0;
        let mut current_line = 0;
        let mut current_char = 0;

        for ch in text.chars() {
            if current_line == position.line && current_char == position.character {
                return offset;
            }

            if ch == '\n' {
                current_line += 1;
                current_char = 0;
            } else {
                current_char += 1;
            }

            offset += ch.len_utf8();
        }

        offset
    }

    /// Convert byte offset to LSP Position
    #[allow(dead_code)] // Used by selection_range provider
    pub fn offset_to_position(text: &str, offset: usize) -> Position {
        let mut current_line = 0;
        let mut current_char = 0;
        let mut current_offset = 0;

        for ch in text.chars() {
            if current_offset >= offset {
                break;
            }

            if ch == '\n' {
                current_line += 1;
                current_char = 0;
            } else {
                current_char += 1;
            }

            current_offset += ch.len_utf8();
        }

        Position {
            line: current_line,
            character: current_char,
        }
    }
}
