//! Trait abstractions for decoupling LSP from compiler core
//!
//! This module provides trait-based abstractions that allow the LSP
//! to work with different implementations (compiler-based, mock, etc.)
//! without hard dependencies on typedlua-core.

pub mod diagnostics;
pub mod module_resolution;
pub mod type_analysis;

pub use diagnostics::{Diagnostic, DiagnosticCollector, DiagnosticLevel};
pub use module_resolution::{ModuleIdentifier, ModuleRegistry, ModuleResolver};
pub use type_analysis::{SymbolInfo, SymbolKind, SymbolStore, TypeCheckResult, TypeChecker};
