use super::analysis::SymbolIndex;
use bumpalo::Bump;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, Position, Uri,
};
use luanext_parser::ast::Program;
use luanext_parser::diagnostics::CollectingDiagnosticHandler;
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser};
use luanext_typechecker::module_resolver::{ModuleId, ModuleRegistry, ModuleResolver};
use luanext_typechecker::SymbolTable;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

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
    fn module_resolver(&self) -> &ModuleResolver;
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

    fn module_resolver(&self) -> &ModuleResolver {
        &self.module_resolver
    }
}

/// Parsed AST along with its string interner for resolving StringId values
pub type ParsedAst = (
    Arc<Program<'static>>,
    Arc<StringInterner>,
    Arc<luanext_parser::string_interner::CommonIdentifiers>,
    Arc<Bump>,
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
            .finish()
    }
}

impl Document {
    /// Create a test document with minimal setup
    /// Available in both test and non-test builds for integration testing
    #[allow(dead_code)]
    pub fn new_test(text: String, version: i32) -> Self {
        Self {
            text,
            version,
            ast: RefCell::new(None),
            symbol_table: None,
            module_id: None,
        }
    }

    pub fn get_or_parse_ast(&self) -> Option<ParsedAst> {
        if let Some(cached) = self.ast.borrow().as_ref() {
            return Some((
                Arc::clone(&cached.0),
                Arc::clone(&cached.1),
                Arc::clone(&cached.2),
                Arc::clone(&cached.3),
            ));
        }

        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (mut interner, common_ids) = StringInterner::new_with_common_identifiers();
        let arena = Bump::new();
        let mut lexer = Lexer::new(&self.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids, &arena);
        let program = parser.parse().ok()?;

        // Leak the arena to make the lifetime 'static
        // This is safe because the data is stored in an Arc and will live as long as needed
        let leaked_program: &'static Program<'static> = unsafe {
            let program_ptr = &program as *const Program<'_>;
            &*(program_ptr as *const Program<'static>)
        };

        let ast_arc = Arc::new(*leaked_program);
        let interner_arc = Arc::new(interner);
        let common_ids_arc = Arc::new(common_ids);
        let arena_arc = Arc::new(arena);

        *self.ast.borrow_mut() = Some((
            Arc::clone(&ast_arc),
            Arc::clone(&interner_arc),
            Arc::clone(&common_ids_arc),
            Arc::clone(&arena_arc),
        ));

        Some((ast_arc, interner_arc, common_ids_arc, arena_arc))
    }
}
