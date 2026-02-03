//! Trait abstractions for decoupling LSP from type checker implementation
//!
//! This module provides trait-based abstractions that allow the LSP
//! to work with different implementations (typechecker-based, mock, etc.)
//! without hard dependencies on specific type checker implementations.

use crate::document::Document;
use lsp_types::*;

pub mod diagnostics;
pub mod module_resolution;
pub mod type_analysis;

#[allow(unused_imports)]
pub use diagnostics::{Diagnostic, DiagnosticCollector, DiagnosticLevel};
#[allow(unused_imports)]
pub use module_resolution::{ModuleIdentifier, ModuleRegistry, ModuleResolver};
#[allow(unused_imports)]
pub use type_analysis::{SymbolInfo, SymbolKind, SymbolStore, TypeCheckResult, TypeChecker};

pub trait CompletionProviderTrait {
    fn provide(&self, document: &Document, position: Position) -> Vec<CompletionItem>;
    fn resolve(&self, item: CompletionItem) -> CompletionItem;
}

pub trait HoverProviderTrait {
    fn provide(&self, document: &Document, position: Position) -> Option<Hover>;
}

pub trait DefinitionProviderTrait {
    fn provide(&self, uri: &Uri, document: &Document, position: Position) -> Option<Location>;
}

pub trait ReferencesProviderTrait {
    fn provide(
        &self,
        uri: &Uri,
        document: &Document,
        position: Position,
        include_declaration: bool,
    ) -> Vec<Location>;
}

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

pub trait SymbolsProviderTrait {
    fn provide(&self, document: &Document) -> DocumentSymbolResponse;
}

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

pub trait SignatureHelpProviderTrait {
    fn provide(&self, document: &Document, position: Position) -> Option<SignatureHelp>;
}

pub trait InlayHintsProviderTrait {
    fn provide(&self, document: &Document, range: Range) -> Option<Vec<InlayHint>>;
    fn resolve(&self, item: InlayHint) -> Option<InlayHint>;
}

pub trait SelectionRangeProviderTrait {
    fn provide(&self, document: &Document, positions: Vec<Position>) -> Vec<SelectionRange>;
}

pub trait FoldingRangeProviderTrait {
    fn provide(&self, document: &Document) -> Vec<FoldingRange>;
}

pub trait SemanticTokensProviderTrait {
    fn provide_full(&self, document: &Document) -> Option<Vec<SemanticToken>>;
    fn provide_range(&self, document: &Document, range: Range) -> Option<Vec<SemanticToken>>;
    fn provide_full_delta(
        &self,
        document: &Document,
        previous_result_id: Option<String>,
    ) -> Option<SemanticTokensDelta>;
}

pub trait DiagnosticsProviderTrait {
    fn provide(&self, document: &Document) -> Vec<lsp_types::Diagnostic>;
}
