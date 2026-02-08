use crate::symbol_index::SymbolIndex;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, Position, Uri,
};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use typedlua_parser::ast::Program;
use typedlua_parser::diagnostics::CollectingDiagnosticHandler;
use typedlua_parser::string_interner::StringInterner;
use typedlua_parser::{Lexer, Parser};
use typedlua_typechecker::module_resolver::{ModuleId, ModuleRegistry, ModuleResolver};

/// Abstraction for document management operations.
///
/// This trait allows for injecting mock document managers in tests
/// and potential future substitution of different document storage backends.
/// Currently implemented by [`DocumentManager`] but used directly
/// rather than via trait objects.
#[allow(dead_code)]
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

/// Parsed AST along with its string interner for resolving StringId values.
/// Note: AST has arena lifetime so we use references instead of Arc.
pub type ParsedAst<'arena> = (
    &'arena Program<'arena>,
    Arc<StringInterner>,
    Arc<typedlua_parser::string_interner::CommonIdentifiers>,
);

/// Manages open documents and their cached analysis results
#[derive(Debug)]
pub struct DocumentManager {
    documents: HashMap<Uri, Document>,
    /// Module registry for cross-file symbol tracking
    #[allow(dead_code)]
    module_registry: Arc<ModuleRegistry>,
    /// Module resolver for import path resolution
    module_resolver: Arc<ModuleResolver>,
    /// Bidirectional mapping between URIs and ModuleIds
    uri_to_module_id: HashMap<Uri, ModuleId>,
    module_id_to_uri: HashMap<ModuleId, Uri>,
    /// Workspace root path
    #[allow(dead_code)]
    workspace_root: PathBuf,
    /// Reverse index for fast cross-file symbol lookups
    symbol_index: SymbolIndex,
}

/// Represents a single document with cached analysis
pub struct Document {
    pub text: String,
    pub version: i32,
    /// Cached symbol table (invalidated on change)
    /// Note: SymbolTable has arena lifetime, so we can't cache it in Arc
    /// We'll need to reparse/recheck on each request (acceptable - it's fast)
    #[allow(dead_code)]
    pub symbol_table: Option<()>,  // Placeholder - actual symbol table would need arena
    /// Module ID for this document (used for cross-file symbol resolution)
    pub module_id: Option<ModuleId>,
}

impl std::fmt::Debug for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Document")
            .field(
                "text",
                &format!("{}...", &self.text.chars().take(50).collect::<String>()),
            )
            .field("version", &self.version)
            .field("module_id", &self.module_id)
            .finish()
    }
}

impl Document {
    #[cfg(test)]
    pub fn new_test(text: String, version: i32) -> Self {
        Self {
            text,
            version,
            symbol_table: None,
            module_id: None,
        }
    }

    /// Parse the document's AST.
    ///
    /// Note: This doesn't cache the AST anymore (arena lifetime prevents caching in Arc).
    /// Parsing is fast enough that reparsing on each request is acceptable.
    pub fn get_or_parse_ast(&self) -> Option<ParsedAst<'static>> {
        // Create a static arena that leaks memory (acceptable for LSP - long-lived process)
        // Alternative would be to use Box::leak but this is cleaner
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let interner_rc = std::rc::Rc::new(interner);

        let mut lexer = Lexer::new(&self.text, handler.clone(), &interner_rc);
        let tokens = lexer.tokenize().ok()?;

        // Use Box::leak to get 'static lifetime (memory leak but acceptable for LSP)
        let arena: &'static bumpalo::Bump = Box::leak(Box::new(bumpalo::Bump::new()));

        let mut parser = Parser::new(tokens, handler, &interner_rc, &common_ids, arena);
        let program = parser.parse().ok()?;

        Some((
            &*arena.alloc(program),  // Allocate in arena and return ref
            Arc::new(std::rc::Rc::unwrap_or_clone(interner_rc)),
            Arc::new(common_ids),
        ))
    }

    pub(crate) fn clear_cache(&self) {
        // No-op now that we don't cache
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

    /// Create a test document manager with mock module system
    #[cfg(test)]
    pub fn new_test() -> Self {
        use typedlua_typechecker::cli::config::CompilerOptions;
        use typedlua_typechecker::cli::fs::MockFileSystem;

        let workspace_root = PathBuf::from("/test");
        let fs = Arc::new(MockFileSystem::new());
        let compiler_options = CompilerOptions::default();
        let module_config =
            typedlua_typechecker::module_resolver::ModuleConfig::from_compiler_options(
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
            symbol_table: None,
            module_id: module_id.clone(),
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

        if let Some(doc) = self.documents.get_mut(&uri) {
            doc.version = params.text_document.version;

            for change in params.content_changes {
                if let Some(range) = change.range {
                    let start_offset = Self::position_to_offset(&doc.text, range.start);
                    let end_offset = Self::position_to_offset(&doc.text, range.end);
                    let mut new_text = String::new();
                    new_text.push_str(&doc.text[..start_offset]);
                    new_text.push_str(&change.text);
                    new_text.push_str(&doc.text[end_offset..]);
                    doc.text = new_text;
                } else {
                    doc.text = change.text;
                }
            }

            doc.clear_cache();

            if let Some(module_id) = &doc.module_id {
                if let Some((ast, interner, _common_ids)) = doc.get_or_parse_ast() {
                    self.symbol_index.update_document(
                        &uri,
                        module_id.as_str(),
                        &ast,
                        &interner,
                        |import_path, from_module_id: &str| {
                            self.module_resolver
                                .resolve(import_path, std::path::Path::new(from_module_id))
                                .ok()
                                .and_then(|resolved_module_id| {
                                    self.module_id_to_uri.get(&resolved_module_id).map(|uri| {
                                        (resolved_module_id.as_str().to_string(), uri.clone())
                                    })
                                })
                        },
                    );
                }
            }
        }
    }

    pub fn save(&mut self, _params: DidSaveTextDocumentParams) {}

    pub fn close(&mut self, params: DidCloseTextDocumentParams) {
        let uri = &params.text_document.uri;

        if let Some(module_id) = self.uri_to_module_id.get(uri) {
            self.symbol_index.clear_document(uri, module_id.as_str());
        }

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

    pub fn symbol_index(&self) -> &SymbolIndex {
        &self.symbol_index
    }

    pub fn uri_to_module_id(&self, uri: &Uri) -> Option<&ModuleId> {
        self.uri_to_module_id.get(uri)
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
