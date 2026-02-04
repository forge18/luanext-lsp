//! Core document management for the LSP server

pub mod diagnostics;
pub mod document;

pub use diagnostics::DiagnosticsProvider;
pub use document::{Document, DocumentManager, DocumentManagerTrait};
