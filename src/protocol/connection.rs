//! LSP protocol connection abstractions

use anyhow::Result;
use lsp_server::{Notification, Response};

/// Trait for sending LSP messages - allows mocking for tests
pub trait LspConnection {
    fn send_response(&self, response: Response) -> Result<()>;
    fn send_notification(&self, notification: Notification) -> Result<()>;
}
