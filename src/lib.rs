// Library interface for LSP server
// This allows the LSP components to be tested

#![allow(clippy::all)]
#![allow(deprecated)]

// Protocol layer
pub mod protocol;

// Core document management
pub mod core;

// Analysis features
pub mod analysis;

// Feature providers organized by category
pub mod features;

// Trait abstractions for type system components
pub mod traits;

// Bridge implementations for typedlua-typechecker types
pub mod impls;

// Dependency injection
pub mod di;

// Message handler (LSP protocol routing)
pub mod message_handler;

// Testing utilities with mock providers
pub mod testing;

// Re-export commonly used types
pub use crate::protocol::LspConnection;
pub use core::{DiagnosticsProvider, Document, DocumentManager};
pub use message_handler::BasicMessageHandler;
