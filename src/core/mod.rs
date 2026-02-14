#![allow(unused_imports)]
//! Core document management for the LSP server

pub mod analysis;
pub mod cache;
pub mod diagnostics;
pub mod document;
pub mod heuristics;
pub mod metrics;

pub use diagnostics::DiagnosticsProvider;
pub use document::{Document, DocumentManager, DocumentManagerTrait};
