#![allow(unused_imports)]
//! Core document management for the LSP server

pub mod analysis;
pub mod diagnostics;
pub mod document;

pub use diagnostics::DiagnosticsProvider;
pub use document::{Document, DocumentManager, DocumentManagerTrait};
