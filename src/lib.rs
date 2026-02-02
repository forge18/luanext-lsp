// Library interface for LSP server
// This allows the LSP components to be tested

#![allow(clippy::all)]
#![allow(deprecated)]

// Trait abstractions for decoupling from typedlua-core

// Bridge implementations for typedlua-core types (feature-gated)

pub mod document;
pub mod message_handler;
pub mod providers;
pub mod symbol_index;

// Re-export commonly used types
pub use document::{Document, DocumentManager};
pub use message_handler::{BasicMessageHandler, LspConnection};
