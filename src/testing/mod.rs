//! Testing utilities with mock providers
//!
//! These mocks don't require the typechecker git dependency
//! and can be used in unit tests for faster iteration.

pub mod mocks;
pub use mocks::*;
