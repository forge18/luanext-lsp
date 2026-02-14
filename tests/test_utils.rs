//! Test utilities for LSP multi-file integration tests
//!
//! Provides infrastructure for testing LSP features across multiple files,
//! including workspace simulation, document management, and file URIs.

use lsp_types::Uri;
use luanext_lsp::core::document::Document;
use std::collections::HashMap;
use std::path::PathBuf;
use std::str::FromStr;

/// Simulates an LSP workspace with multiple open documents
///
/// Provides convenient methods for managing a multi-file workspace,
/// creating documents, and working with file URIs in tests.
pub struct LspTestWorkspace {
    documents: HashMap<Uri, Document>,
    workspace_root: PathBuf,
}

impl LspTestWorkspace {
    /// Create a new LSP test workspace
    ///
    /// # Arguments
    /// * `workspace_root` - Path to the workspace root (e.g., "/test-workspace")
    pub fn new(workspace_root: &str) -> Self {
        Self {
            documents: HashMap::new(),
            workspace_root: PathBuf::from(workspace_root),
        }
    }

    /// Add a document to the workspace
    ///
    /// # Arguments
    /// * `path` - Relative path from workspace root (e.g., "src/main.luax")
    /// * `content` - Document content (LuaNext source code)
    pub fn add_document(&mut self, path: &str, content: &str) {
        let uri = self.make_uri(path);
        let document = Document::new_test(content.to_string(), 1);
        self.documents.insert(uri, document);
    }

    /// Update an existing document's content
    ///
    /// # Arguments
    /// * `path` - Relative path from workspace root
    /// * `content` - New document content
    /// * `version` - New version number (for LSP change tracking)
    pub fn update_document(&mut self, path: &str, content: &str, version: i32) {
        let uri = self.make_uri(path);
        let document = Document::new_test(content.to_string(), version);
        self.documents.insert(uri, document);
    }

    /// Get a reference to a document
    ///
    /// # Arguments
    /// * `path` - Relative path from workspace root
    pub fn get_document(&self, path: &str) -> Option<&Document> {
        let uri = self.make_uri(path);
        self.documents.get(&uri)
    }

    /// Get a mutable reference to a document
    pub fn get_document_mut(&mut self, path: &str) -> Option<&mut Document> {
        let uri = self.make_uri(path);
        self.documents.get_mut(&uri)
    }

    /// Get the URI for a relative path
    ///
    /// # Arguments
    /// * `path` - Relative path from workspace root (e.g., "src/main.luax")
    ///
    /// Returns a properly formatted file:// URI
    pub fn uri(&self, path: &str) -> Uri {
        self.make_uri(path)
    }

    /// Get the workspace root as a URI
    pub fn workspace_uri(&self) -> Uri {
        Uri::from_str(&format!("file://{}", self.workspace_root.display())).unwrap()
    }

    /// Get the number of open documents
    pub fn document_count(&self) -> usize {
        self.documents.len()
    }

    /// Get all open documents
    #[allow(clippy::mutable_key_type)]
    pub fn documents(&self) -> &HashMap<Uri, Document> {
        &self.documents
    }

    /// Get all open documents (mutable)
    #[allow(clippy::mutable_key_type)]
    pub fn documents_mut(&mut self) -> &mut HashMap<Uri, Document> {
        &mut self.documents
    }

    /// Clear all documents from the workspace
    pub fn clear(&mut self) {
        self.documents.clear();
    }

    /// Helper to create a file:// URI from a relative path
    fn make_uri(&self, path: &str) -> Uri {
        let full_path = self.workspace_root.join(path);
        Uri::from_str(&format!("file://{}", full_path.display())).unwrap()
    }
}

impl Default for LspTestWorkspace {
    fn default() -> Self {
        Self::new("/test-workspace")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workspace_creation() {
        let workspace = LspTestWorkspace::new("/test");
        assert_eq!(workspace.document_count(), 0);
    }

    #[test]
    fn test_add_single_document() {
        let mut workspace = LspTestWorkspace::new("/test");
        workspace.add_document("main.luax", "print('hello')");
        assert_eq!(workspace.document_count(), 1);
    }

    #[test]
    fn test_add_multiple_documents() {
        let mut workspace = LspTestWorkspace::new("/test");
        workspace.add_document("main.luax", "print('hello')");
        workspace.add_document("lib.luax", "function foo() end");
        workspace.add_document("types.luax", "interface User { id: string }");
        assert_eq!(workspace.document_count(), 3);
    }

    #[test]
    fn test_get_document() {
        let mut workspace = LspTestWorkspace::new("/test");
        workspace.add_document("main.luax", "local x = 1");
        let doc = workspace.get_document("main.luax");
        assert!(doc.is_some());
        assert_eq!(doc.unwrap().text, "local x = 1");
    }

    #[test]
    fn test_get_nonexistent_document() {
        let workspace = LspTestWorkspace::new("/test");
        let doc = workspace.get_document("nonexistent.luax");
        assert!(doc.is_none());
    }

    #[test]
    fn test_update_document() {
        let mut workspace = LspTestWorkspace::new("/test");
        workspace.add_document("main.luax", "local x = 1");
        assert_eq!(workspace.get_document("main.luax").unwrap().version, 1);

        workspace.update_document("main.luax", "local x = 2", 2);
        let doc = workspace.get_document("main.luax").unwrap();
        assert_eq!(doc.text, "local x = 2");
        assert_eq!(doc.version, 2);
    }

    #[test]
    fn test_uri_generation() {
        let workspace = LspTestWorkspace::new("/home/user/project");
        let uri = workspace.uri("src/main.luax");
        // Verify URI contains the full path
        assert!(uri.to_string().contains("src/main.luax"));
    }

    #[test]
    fn test_workspace_default() {
        let workspace = LspTestWorkspace::default();
        assert_eq!(workspace.document_count(), 0);
        // Default workspace root should be /test-workspace
        assert!(workspace
            .workspace_uri()
            .to_string()
            .contains("test-workspace"));
    }

    #[test]
    fn test_clear_workspace() {
        let mut workspace = LspTestWorkspace::new("/test");
        workspace.add_document("main.luax", "print('hello')");
        workspace.add_document("lib.luax", "function foo() end");
        assert_eq!(workspace.document_count(), 2);

        workspace.clear();
        assert_eq!(workspace.document_count(), 0);
    }

    #[test]
    fn test_nested_paths() {
        let mut workspace = LspTestWorkspace::new("/test");
        workspace.add_document("src/main.luax", "print('hello')");
        workspace.add_document("src/api/handlers.luax", "function handle() end");
        workspace.add_document("types/models.luax", "interface Model {}");

        assert_eq!(workspace.document_count(), 3);
        assert!(workspace.get_document("src/main.luax").is_some());
        assert!(workspace.get_document("src/api/handlers.luax").is_some());
        assert!(workspace.get_document("types/models.luax").is_some());
    }
}
