//! Bridge implementations for typedlua-typechecker types
//!
//! These implementations adapt concrete types from typedlua-typechecker
//! to implement the trait abstractions defined in the traits module.

#[allow(unused_imports)]
pub mod compiler_bridge;

#[allow(unused_imports)]
pub use compiler_bridge::{
    CoreDiagnosticCollector, CoreModuleIdentifier, CoreModuleRegistry, CoreModuleResolver,
    CoreSymbolStore, CoreTypeChecker,
};
