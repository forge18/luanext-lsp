use crate::symbol_index::SymbolIndex;
use crate::traits::{ModuleRegistry, ModuleResolver, SymbolStore};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, Position, Uri,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
<<<<<<< HEAD

#[cfg(feature = "compiler")]
use typedlua_core::diagnostics::CollectingDiagnosticHandler;
#[cfg(feature = "compiler")]
use typedlua_parser::ast::Program;
#[cfg(feature = "compiler")]
use typedlua_parser::lexer::Lexer;
#[cfg(feature = "compiler")]
use typedlua_parser::parser::Parser;
#[cfg(feature = "compiler")]
use typedlua_parser::string_interner::StringInterner;
=======
use typedlua_parser::ast::Program;
use typedlua_parser::diagnostics::CollectingDiagnosticHandler;
use typedlua_typechecker::module_resolver::{ModuleId, ModuleRegistry, ModuleResolver};
use typedlua_parser::string_interner::StringInterner;
use typedlua_typechecker::SymbolTable;
use typedlua_parser::{Lexer, Parser};
>>>>>>> b9886cd (Refactor dependencies and update imports to use typedlua_parser and typedlua_typechecker)

/// Parsed AST along with its string interner for resolving StringId values
#[cfg(feature = "compiler")]
pub type ParsedAst = (
    Arc<Program>,
    Arc<StringInterner>,
    Arc<typedlua_parser::string_interner::CommonIdentifiers>,
);

#[cfg(not(feature = "compiler"))]
pub type ParsedAst = ();

/// Manages open documents and their cached analysis results
#[derive(Debug)]
pub struct DocumentManager {
    documents: HashMap<Uri, Document>,
    /// Module registry for cross-file symbol tracking (optional)
    module_registry: Option<Arc<dyn ModuleRegistry>>,
    /// Module resolver for import path resolution (optional)
    module_resolver: Option<Arc<dyn ModuleResolver>>,
    /// Bidirectional mapping between URIs and ModuleIds
    uri_to_module_id: HashMap<Uri, String>,
    module_id_to_uri: HashMap<String, Uri>,
    /// Workspace root path
    workspace_root: PathBuf,
    /// Reverse index for fast cross-file symbol lookups
    symbol_index: SymbolIndex,
}

/// Represents a single document with cached analysis
pub struct Document {
    pub text: String,
    pub version: i32,
    /// Cached parsed AST with its interner (invalidated on change)
    ast: RefCell<Option<ParsedAst>>,
    /// Cached symbol store (invalidated on change)
    symbol_store: Option<Box<dyn SymbolStore>>,
    /// Module ID for this document (used for cross-file symbol resolution)
    pub module_id: Option<String>,
}

impl std::fmt::Debug for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Document")
            .field("text", &format!("{}...", &self.text.chars().take(50).collect::<String>()))
            .field("version", &self.version)
            .field("ast", &"<cached>")
            .field("symbol_store", &self.symbol_store.as_ref().map(|_| "<cached>"))
            .field("module_id", &self.module_id)
            .finish()
    }
}

impl Document {
    pub fn new_test(text: String, version: i32) -> Self {
        Self {
            text,
            version,
            ast: RefCell::new(None),
            symbol_store: None,
            module_id: None,
        }
    }

    #[cfg(feature = "compiler")]
    pub fn get_or_parse_ast(&self) -> Option<ParsedAst> {
        if let Some(cached) = self.ast.borrow().as_ref() {
            return Some((Arc::clone(&cached.0), Arc::clone(&cached.1), Arc::clone(&cached.2)));
        }

        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (mut interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&self.text, handler.clone(), &mut interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler, &mut interner, &common_ids);
        let program = parser.parse().ok()?;

        let ast_arc = Arc::new(program);
        let interner_arc = Arc::new(interner);
        let common_ids_arc = Arc::new(common_ids);
        *self.ast.borrow_mut() = Some((Arc::clone(&ast_arc), Arc::clone(&interner_arc), Arc::clone(&common_ids_arc)));

        Some((ast_arc, interner_arc, common_ids_arc))
    }

    #[cfg(not(feature = "compiler"))]
    pub fn get_or_parse_ast(&self) -> Option<ParsedAst> {
        None
    }

    pub(crate) fn clear_cache(&self) {
        *self.ast.borrow_mut() = None;
    }
}

impl DocumentManager {
<<<<<<< HEAD
    /// Create a basic document manager without compiler features
    pub fn new_basic(workspace_root: PathBuf) -> Self {
=======
    /// Create a test document manager with mock module system
    /// This is exposed for testing purposes
    pub fn new_test() -> Self {
        use typedlua_typechecker::config::CompilerOptions;
        use typedlua_typechecker::fs::MockFileSystem;

        let workspace_root = PathBuf::from("/test");
        let fs = Arc::new(MockFileSystem::new());
        let compiler_options = CompilerOptions::default();
        let module_config = typedlua_typechecker::module_resolver::ModuleConfig::from_compiler_options(
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
>>>>>>> b9886cd (Refactor dependencies and update imports to use typedlua_parser and typedlua_typechecker)
        Self {
            documents: HashMap::new(),
            module_registry: None,
            module_resolver: None,
            uri_to_module_id: HashMap::new(),
            module_id_to_uri: HashMap::new(),
            workspace_root,
            symbol_index: SymbolIndex::new(),
        }
    }

    /// Create a document manager with compiler support
    #[cfg(feature = "compiler")]
    pub fn new(
        workspace_root: PathBuf,
        module_registry: Arc<dyn ModuleRegistry>,
        module_resolver: Arc<dyn ModuleResolver>,
    ) -> Self {
        Self {
            documents: HashMap::new(),
            module_registry: Some(module_registry),
            module_resolver: Some(module_resolver),
            uri_to_module_id: HashMap::new(),
            module_id_to_uri: HashMap::new(),
            workspace_root,
            symbol_index: SymbolIndex::new(),
        }
    }

    #[cfg(feature = "compiler")]
    pub fn new_test() -> Self {
        use crate::impls::{CoreModuleRegistry, CoreModuleResolver};
        use typedlua_core::config::CompilerOptions;
        use typedlua_core::fs::MockFileSystem;
        use typedlua_core::module_resolver::{ModuleRegistry as CoreModuleRegistryType, ModuleResolver as CoreModuleResolverType};

        let workspace_root = PathBuf::from("/test");
        let fs = Arc::new(MockFileSystem::new());
        let compiler_options = CompilerOptions::default();
        let module_config = typedlua_core::module_resolver::ModuleConfig::from_compiler_options(&compiler_options, &workspace_root);
        let core_registry = Arc::new(CoreModuleRegistryType::new());
        let core_resolver = Arc::new(CoreModuleResolverType::new(fs, module_config, workspace_root.clone()));

        let module_registry = Arc::new(CoreModuleRegistry::new(core_registry)) as Arc<dyn ModuleRegistry>;
        let module_resolver = Arc::new(CoreModuleResolver::new(core_resolver)) as Arc<dyn ModuleResolver>;

        Self::new(workspace_root, module_registry, module_resolver)
    }

    pub fn open(&mut self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();

        #[cfg(feature = "compiler")]
        let module_id = uri.as_str().strip_prefix("file://")
            .map(PathBuf::from)
            .and_then(|path| path.canonicalize().ok())
            .map(|path| path.to_string_lossy().into_owned());

        #[cfg(not(feature = "compiler"))]
        let module_id: Option<String> = None;

        let document = Document {
            text: params.text_document.text,
            version: params.text_document.version,
            ast: RefCell::new(None),
            symbol_store: None,
            module_id: module_id.clone(),
        };

        if let Some(ref mid) = module_id {
            self.uri_to_module_id.insert(uri.clone(), mid.clone());
            self.module_id_to_uri.insert(mid.clone(), uri.clone());
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
            doc.symbol_store = None;
        }
    }

    pub fn save(&mut self, _params: DidSaveTextDocumentParams) {}

    pub fn close(&mut self, params: DidCloseTextDocumentParams) {
        let uri = &params.text_document.uri;

        #[cfg(feature = "compiler")]
        if let Some(module_id) = self.uri_to_module_id.get(uri) {
            self.symbol_index.clear_document(uri, module_id);
        }

        if let Some(module_id) = self.uri_to_module_id.remove(uri) {
            self.module_id_to_uri.remove(&module_id);
        }
        self.documents.remove(uri);
    }

    pub fn get(&self, uri: &Uri) -> Option<&Document> {
        self.documents.get(uri)
    }

    pub fn get_mut(&mut self, uri: &Uri) -> Option<&mut Document> {
        self.documents.get_mut(uri)
    }

    pub fn module_registry(&self) -> Option<&Arc<dyn ModuleRegistry>> {
        self.module_registry.as_ref()
    }

    pub fn module_resolver(&self) -> Option<&Arc<dyn ModuleResolver>> {
        self.module_resolver.as_ref()
    }

    pub fn workspace_root(&self) -> &PathBuf {
        &self.workspace_root
    }

    pub fn symbol_index(&self) -> &SymbolIndex {
        &self.symbol_index
    }

    pub fn uri_to_module_id(&self, uri: &Uri) -> Option<&String> {
        self.uri_to_module_id.get(uri)
    }

    pub fn module_id_to_uri(&self, module_id: &str) -> Option<&Uri> {
        self.module_id_to_uri.get(module_id)
    }

    pub fn all_documents(&self) -> impl Iterator<Item = (&Uri, &Document)> {
        self.documents.iter()
    }

    pub fn get_or_parse_ast(&self, uri: &Uri) -> Option<ParsedAst> {
        self.get(uri).and_then(|doc| doc.get_or_parse_ast())
    }

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
