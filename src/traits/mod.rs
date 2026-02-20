//! Trait abstractions for decoupling LSP from type checker implementation
//!
//! This module provides trait-based abstractions that allow the LSP
//! to work with different implementations (typechecker-based, mock, etc.)
//! without hard dependencies on specific type checker implementations.
//!
//! ## Traits
//!
//! These traits are used for dependency injection in the LSP server.
//! They allow for:
//! - Easier testing with mock implementations
//! - Potential future substitution of different type checker backends
//! - Better separation of concerns between LSP protocol and type checking

use crate::core::document::Document;
use lsp_types::*;

pub mod diagnostics;
pub mod module_resolution;
pub mod type_analysis;

pub use diagnostics::{Diagnostic, DiagnosticCollector, DiagnosticLevel};
pub use module_resolution::{ModuleIdentifier, ModuleRegistry, ModuleResolver};

/// Trait for providing code completion items.
///
/// Used by the LSP completion feature to suggest code completions
/// at a given cursor position.
pub trait CompletionProviderTrait {
    fn provide(&self, document: &Document, position: Position) -> Vec<CompletionItem>;
    fn resolve(&self, item: CompletionItem) -> CompletionItem;
}

/// Trait for providing hover information.
///
/// Used by the LSP hover feature to show type information
/// and documentation when hovering over symbols.
pub trait HoverProviderTrait {
    fn provide(&self, document: &Document, position: Position) -> Option<Hover>;
}

/// Trait for providing goto definition functionality.
///
/// Used by the LSP definition feature to navigate from a symbol
/// reference to its declaration.
pub trait DefinitionProviderTrait {
    fn provide(
        &self,
        uri: &Uri,
        document: &Document,
        position: Position,
    ) -> Option<GotoDefinitionResponse>;
}

/// Trait for providing find references functionality.
///
/// Used by the LSP references feature to find all references
/// to a symbol at a given position.
pub trait ReferencesProviderTrait {
    fn provide(
        &self,
        uri: &Uri,
        document: &Document,
        position: Position,
        include_declaration: bool,
    ) -> Vec<Location>;
}

/// Trait for providing rename refactoring functionality.
///
/// Used by the LSP rename feature to safely rename symbols
/// across the entire project.
pub trait RenameProviderTrait {
    fn prepare(&self, document: &Document, position: Position) -> Option<Range>;
    fn rename(
        &self,
        uri: &Uri,
        document: &Document,
        position: Position,
        new_name: &str,
    ) -> Option<WorkspaceEdit>;
}

/// Trait for providing document symbols outline.
///
/// Used by the LSP document symbols feature to show the
/// structure of a file (classes, functions, variables, etc.).
pub trait SymbolsProviderTrait {
    fn provide(&self, document: &Document) -> DocumentSymbolResponse;
}

/// Trait for providing document formatting.
///
/// Used by the LSP formatting features:
/// - Document formatting (whole file)
/// - Range formatting (selection)
/// - On-type formatting (after typing specific characters)
pub trait FormattingProviderTrait {
    fn format_document(&self, document: &Document, options: FormattingOptions) -> Option<String>;
    fn format_range(
        &self,
        document: &Document,
        range: Range,
        options: FormattingOptions,
    ) -> Option<String>;
    fn format_on_type(
        &self,
        document: &Document,
        position: Position,
        ch: &str,
        options: FormattingOptions,
    ) -> Option<String>;
}

/// Trait for providing code actions and quick fixes.
///
/// Used by the LSP code actions feature to suggest and apply
/// refactorings, fixes, and refactorings at a given position.
pub trait CodeActionsProviderTrait {
    fn provide(
        &self,
        uri: &Uri,
        document: &Document,
        range: Range,
        context: CodeActionContext,
    ) -> Option<CodeActionResponse>;
    fn resolve(&self, item: CodeAction) -> Option<CodeAction>;
}

/// Trait for providing signature help.
///
/// Used by the LSP signature help feature to show function
/// parameter information when typing opening parenthesis.
pub trait SignatureHelpProviderTrait {
    fn provide(&self, document: &Document, position: Position) -> Option<SignatureHelp>;
}

/// Trait for providing inlay hints.
///
/// Used by the LSP inlay hints feature to show inline hints
/// for types, parameter names, etc.
pub trait InlayHintsProviderTrait {
    fn provide(&self, document: &Document, range: Range) -> Option<Vec<InlayHint>>;
    fn resolve(&self, item: InlayHint) -> Option<InlayHint>;
}

/// Trait for providing selection ranges.
///
/// Used by the LSP selection ranges feature to support
/// smart selection expansion.
pub trait SelectionRangeProviderTrait {
    fn provide(&self, document: &Document, positions: Vec<Position>) -> Vec<SelectionRange>;
}

/// Trait for providing folding ranges.
///
/// Used by the LSP folding ranges feature to show
/// collapsible regions in the editor.
pub trait FoldingRangeProviderTrait {
    fn provide(&self, document: &Document) -> Vec<FoldingRange>;
}

/// Trait for providing semantic tokens.
///
/// Used by the LSP semantic tokens feature for syntax
/// highlighting of identifiers, types, etc.
pub trait SemanticTokensProviderTrait {
    fn provide_full(&self, document: &Document) -> Option<Vec<SemanticToken>>;
    fn provide_range(&self, document: &Document, range: Range) -> Option<Vec<SemanticToken>>;
    fn provide_full_delta(
        &self,
        document: &Document,
        previous_result_id: Option<String>,
    ) -> Option<SemanticTokensDelta>;
}

/// Trait for providing diagnostics.
///
/// Used by the LSP diagnostics feature to report type errors,
/// warnings, and other issues to the editor.
pub trait DiagnosticsProviderTrait {
    fn provide(&self, document: &Document) -> Vec<lsp_types::Diagnostic>;
}
