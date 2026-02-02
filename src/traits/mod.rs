//! Trait abstractions for decoupling LSP from type checker implementation
//!
//! This module provides trait-based abstractions that allow the LSP
//! to work with different implementations (typechecker-based, mock, etc.)
//! without hard dependencies on specific type checker implementations.

pub mod diagnostics;
pub mod module_resolution;
pub mod type_analysis;

#[allow(unused_imports)]
pub use diagnostics::{Diagnostic, DiagnosticCollector, DiagnosticLevel};
#[allow(unused_imports)]
pub use module_resolution::{ModuleIdentifier, ModuleRegistry, ModuleResolver};
#[allow(unused_imports)]
pub use type_analysis::{SymbolInfo, SymbolKind, SymbolStore, TypeCheckResult, TypeChecker};
