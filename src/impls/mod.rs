//! Bridge implementations for typedlua-core types
//!
//! These implementations adapt concrete types from typedlua-core
//! to implement the trait abstractions defined in the traits module.
//!
//! All implementations are feature-gated with `#[cfg(feature = "compiler")]`

#[cfg(feature = "compiler")]
pub mod compiler_bridge;

#[cfg(feature = "compiler")]
pub use compiler_bridge::{
    CoreDiagnosticCollector, CoreModuleIdentifier, CoreModuleRegistry, CoreModuleResolver,
    CoreSymbolStore, CoreTypeChecker,
};
