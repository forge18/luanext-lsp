// Library interface for LSP server
// This allows the LSP components to be tested

#![allow(clippy::all)]
#![allow(deprecated)]

// Trait abstractions for decoupling from typedlua-core
pub mod traits;

// Bridge implementations for typedlua-core types (feature-gated)
pub mod impls;

pub mod document;
pub mod message_handler;
#[cfg(feature = "compiler")]
pub mod providers;
pub mod symbol_index;

// Re-export commonly used types
pub use document::{Document, DocumentManager};
pub use message_handler::{BasicMessageHandler, LspConnection};
#[cfg(feature = "compiler")]
pub use message_handler::MessageHandler;
