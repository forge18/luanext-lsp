//! Bridge implementations connecting typedlua-typechecker types to LSP traits

#![allow(dead_code)]

use crate::traits::{
    diagnostics::RelatedInformation,
    type_analysis::{Span, SymbolInfo, SymbolKind, SymbolStore, TypeCheckResult, TypeChecker},
    Diagnostic, DiagnosticCollector, DiagnosticLevel, ModuleIdentifier, ModuleRegistry,
    ModuleResolver,
};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use typedlua_parser::lexer::Lexer;
use typedlua_parser::parser::Parser;
use typedlua_parser::string_interner::StringInterner;
use typedlua_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
use typedlua_typechecker::module_resolver::{
    ModuleId as CoreModuleId, ModuleRegistry as CoreModuleRegistryType,
    ModuleResolver as CoreModuleResolverType,
};
use typedlua_typechecker::{Symbol, SymbolTable, TypeChecker as CoreTypeCheckerType};

// ============================================================================
// Module Resolution Bridges
// ============================================================================

/// Bridge implementation wrapping typedlua_typechecker::module_resolver::ModuleId
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CoreModuleIdentifier {
    inner: CoreModuleId,
}

impl CoreModuleIdentifier {
    pub fn new(inner: CoreModuleId) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> &CoreModuleId {
        &self.inner
    }

    pub fn into_inner(self) -> CoreModuleId {
        self.inner
    }
}

impl ModuleIdentifier for CoreModuleIdentifier {
    fn from_path(path: PathBuf) -> Self {
        Self {
            inner: CoreModuleId::new(path),
        }
    }

    fn path(&self) -> &Path {
        self.inner.path()
    }

    fn as_str(&self) -> &str {
        self.inner.as_str()
    }
}

/// Bridge implementation wrapping typedlua_core::ModuleResolver
#[derive(Debug)]
pub struct CoreModuleResolver {
    inner: Arc<CoreModuleResolverType>,
}

impl CoreModuleResolver {
    pub fn new(inner: Arc<CoreModuleResolverType>) -> Self {
        Self { inner }
    }
}

impl ModuleResolver for CoreModuleResolver {
    fn resolve(&self, source: &str, from_file: &Path) -> Result<String, String> {
        self.inner
            .resolve(source, from_file)
            .map(|module_id| module_id.as_str().to_string())
            .map_err(|e: typedlua_typechecker::module_resolver::ModuleError| e.to_string())
    }
}

/// Bridge implementation wrapping typedlua_core::ModuleRegistry
#[derive(Debug)]
pub struct CoreModuleRegistry {
    inner: Arc<CoreModuleRegistryType>,
}

impl CoreModuleRegistry {
    pub fn new(inner: Arc<CoreModuleRegistryType>) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> &Arc<CoreModuleRegistryType> {
        &self.inner
    }
}

impl ModuleRegistry for CoreModuleRegistry {
    fn has_module(&self, module_id: &str) -> bool {
        // Convert string to ModuleId and check if module exists
        let path = PathBuf::from(module_id);
        let id = CoreModuleId::new(path);
        self.inner.get_module(&id).is_ok()
    }

    fn all_modules(&self) -> Vec<String> {
        // The core ModuleRegistry doesn't expose all modules
        // This is a limitation of the current abstraction
        vec![]
    }
}

// ============================================================================
// Symbol Store Bridge
// ============================================================================

/// Bridge implementation wrapping typedlua_core::SymbolTable
///
/// Symbols are converted eagerly at construction time to avoid
/// storing StringInterner which is not Sync.
pub struct CoreSymbolStore {
    symbols: Vec<SymbolInfo>,
}

impl CoreSymbolStore {
    pub fn new(symbol_table: &SymbolTable, interner: &StringInterner) -> Self {
        let symbols = symbol_table
            .all_visible_symbols()
            .values()
            .map(|symbol| Self::convert_symbol(symbol, interner))
            .collect();

        Self { symbols }
    }

    fn convert_symbol(symbol: &Symbol, _interner: &StringInterner) -> SymbolInfo {
        // symbol.name is already a String, no need to resolve
        SymbolInfo {
            name: symbol.name.clone(),
            kind: convert_symbol_kind(&symbol.kind),
            type_annotation: Some(format!("{:?}", symbol.typ)),
            span: convert_span(&symbol.span),
            is_exported: symbol.is_exported,
            references: symbol.references.iter().map(convert_span).collect(),
        }
    }
}

impl SymbolStore for CoreSymbolStore {
    fn get_symbol_at_position(&self, line: usize, column: usize) -> Option<SymbolInfo> {
        // Find symbol whose span contains the position
        self.symbols
            .iter()
            .find(|symbol| {
                let span = &symbol.span;
                span.line == line && span.column <= column && column < span.column + span.len()
            })
            .cloned()
    }

    fn all_symbols(&self) -> Vec<SymbolInfo> {
        self.symbols.clone()
    }
}

// ============================================================================
// Type Checker Bridge
// ============================================================================

/// Bridge implementation wrapping typedlua_core::TypeChecker
pub struct CoreTypeChecker {
    _marker: std::marker::PhantomData<()>,
}

impl CoreTypeChecker {
    pub fn new() -> Self {
        Self {
            _marker: std::marker::PhantomData,
        }
    }
}

impl Default for CoreTypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker for CoreTypeChecker {
    fn check_document(&self, text: &str) -> TypeCheckResult {
        // Create string interner and common identifiers
        let (interner, common) = StringInterner::new_with_common_identifiers();

        // Create diagnostic handler
        let diagnostic_handler = Arc::new(CollectingDiagnosticHandler::new());

        // Lex the source
        let mut lexer = Lexer::new(
            text,
            diagnostic_handler.clone() as Arc<dyn typedlua_parser::DiagnosticHandler>,
            &interner,
        );
        let tokens = match lexer.tokenize() {
            Ok(tokens) => tokens,
            Err(_) => {
                // Lex failed, return diagnostics
                use typedlua_typechecker::cli::diagnostics::DiagnosticHandler;
                let diagnostics = diagnostic_handler
                    .get_diagnostics()
                    .iter()
                    .map(convert_diagnostic)
                    .collect();

                return TypeCheckResult {
                    diagnostics,
                    symbol_info: None,
                };
            }
        };

        // Parse the tokens
        let mut parser = Parser::new(
            tokens,
            diagnostic_handler.clone() as Arc<dyn typedlua_parser::DiagnosticHandler>,
            &interner,
            &common,
        );

        let mut ast = match parser.parse() {
            Ok(ast) => ast,
            Err(_) => {
                // Parse failed, return diagnostics
                use typedlua_typechecker::cli::diagnostics::DiagnosticHandler;
                let diagnostics = diagnostic_handler
                    .get_diagnostics()
                    .iter()
                    .map(convert_diagnostic)
                    .collect();

                return TypeCheckResult {
                    diagnostics,
                    symbol_info: None,
                };
            }
        };

        // Type check the AST
        let mut type_checker = CoreTypeCheckerType::new(
            diagnostic_handler.clone()
                as Arc<dyn typedlua_typechecker::cli::diagnostics::DiagnosticHandler>,
            &interner,
            &common,
        );
        match type_checker.check_program(&mut ast) {
            Ok(_) => {
                use typedlua_typechecker::cli::diagnostics::DiagnosticHandler;
                let diagnostics = diagnostic_handler
                    .get_diagnostics()
                    .iter()
                    .map(convert_diagnostic)
                    .collect();

                let symbol_store =
                    Box::new(CoreSymbolStore::new(type_checker.symbol_table(), &interner));

                TypeCheckResult {
                    diagnostics,
                    symbol_info: Some(symbol_store),
                }
            }
            Err(_) => {
                use typedlua_typechecker::cli::diagnostics::DiagnosticHandler;
                let diagnostics = diagnostic_handler
                    .get_diagnostics()
                    .iter()
                    .map(convert_diagnostic)
                    .collect();

                TypeCheckResult {
                    diagnostics,
                    symbol_info: None,
                }
            }
        }
    }
}

// ============================================================================
// Diagnostic Collector Bridge
// ============================================================================

/// Bridge implementation for diagnostic collection
pub struct CoreDiagnosticCollector {
    _marker: std::marker::PhantomData<()>,
}

impl CoreDiagnosticCollector {
    pub fn new() -> Self {
        Self {
            _marker: std::marker::PhantomData,
        }
    }
}

impl Default for CoreDiagnosticCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl DiagnosticCollector for CoreDiagnosticCollector {
    fn collect_diagnostics(&self, text: &str) -> Vec<Diagnostic> {
        let (interner, common) = StringInterner::new_with_common_identifiers();
        let diagnostic_handler = Arc::new(CollectingDiagnosticHandler::new());

        let mut lexer = Lexer::new(
            text,
            diagnostic_handler.clone() as Arc<dyn typedlua_parser::DiagnosticHandler>,
            &interner,
        );
        let tokens = match lexer.tokenize() {
            Ok(tokens) => tokens,
            Err(_) => {
                // Return lex diagnostics
                use typedlua_typechecker::cli::diagnostics::DiagnosticHandler;
                return diagnostic_handler
                    .get_diagnostics()
                    .iter()
                    .map(convert_diagnostic)
                    .collect();
            }
        };

        let mut parser = Parser::new(
            tokens,
            diagnostic_handler.clone() as Arc<dyn typedlua_parser::DiagnosticHandler>,
            &interner,
            &common,
        );
        let _ast = parser.parse();

        use typedlua_typechecker::cli::diagnostics::DiagnosticHandler;
        diagnostic_handler
            .get_diagnostics()
            .iter()
            .map(convert_diagnostic)
            .collect()
    }
}

// ============================================================================
// Conversion Helpers
// ============================================================================

fn convert_span(span: &typedlua_parser::Span) -> Span {
    Span {
        start: span.start as usize,
        end: span.end as usize,
        line: span.line as usize,
        column: span.column as usize,
    }
}

fn convert_symbol_kind(kind: &typedlua_typechecker::SymbolKind) -> SymbolKind {
    use typedlua_typechecker::SymbolKind as CoreKind;
    match kind {
        CoreKind::Variable => SymbolKind::Variable,
        CoreKind::Const => SymbolKind::Const,
        CoreKind::Function => SymbolKind::Function,
        CoreKind::Class => SymbolKind::Class,
        CoreKind::Interface => SymbolKind::Interface,
        CoreKind::TypeAlias => SymbolKind::Type,
        CoreKind::Enum => SymbolKind::Enum,
        CoreKind::Parameter => SymbolKind::Parameter,
        CoreKind::Namespace => SymbolKind::Namespace,
    }
}

fn convert_diagnostic(diag: &typedlua_typechecker::cli::diagnostics::Diagnostic) -> Diagnostic {
    use typedlua_typechecker::cli::diagnostics::DiagnosticLevel as CoreLevel;

    let level = match diag.level {
        CoreLevel::Error => DiagnosticLevel::Error,
        CoreLevel::Warning => DiagnosticLevel::Warning,
        CoreLevel::Info => DiagnosticLevel::Info,
    };

    let span = convert_span(&diag.span);

    let mut diagnostic = Diagnostic::new(span, level, diag.message.clone());

    if let Some(code) = &diag.code {
        diagnostic = diagnostic.with_code(code.as_str());
    }

    for related in &diag.related_information {
        diagnostic = diagnostic.with_related(RelatedInformation {
            span: convert_span(&related.span),
            message: related.message.clone(),
        });
    }

    diagnostic
}
