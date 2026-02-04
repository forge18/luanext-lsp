//! LSP feature providers organized by category

pub mod edit;
pub mod formatting;
pub mod hints;
pub mod navigation;
pub mod semantic;
pub mod structure;

pub use edit::{CodeActionsProvider, CompletionProvider, RenameProvider, SignatureHelpProvider};
pub use formatting::FormattingProvider;
pub use hints::InlayHintsProvider;
pub use navigation::{DefinitionProvider, HoverProvider, ReferencesProvider};
pub use semantic::SemanticTokensProvider;
pub use structure::{FoldingRangeProvider, SelectionRangeProvider, SymbolsProvider};
