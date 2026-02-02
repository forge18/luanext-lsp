//! Module resolution traits for decoupling from typedlua-core module system

#![allow(dead_code)]

use std::fmt;
use std::hash::Hash;
use std::path::{Path, PathBuf};

/// Trait for resolving module imports to file paths
pub trait ModuleResolver: Send + Sync + fmt::Debug {
    /// Resolve an import specifier to an absolute module path
    ///
    /// # Arguments
    /// * `source` - The import specifier (e.g., "./foo", "@/bar")
    /// * `from_file` - The file containing the import
    ///
    /// # Returns
    /// The resolved absolute path as a string, or error message
    fn resolve(&self, source: &str, from_file: &Path) -> Result<String, String>;
}

/// Trait for module identification
///
/// Provides a language-agnostic way to identify modules without
/// depending on concrete ModuleId implementation from typedlua-core.
pub trait ModuleIdentifier: Clone + fmt::Debug + Hash + Eq + Send + Sync {
    /// Create a module identifier from a file path
    fn from_path(path: PathBuf) -> Self;

    /// Get the file path for this module
    fn path(&self) -> &Path;

    /// Get a string representation of this module ID
    fn as_str(&self) -> &str;
}

/// Trait for module registry operations
///
/// The module registry tracks cross-file symbols and dependencies.
/// Currently minimal as most functionality is reserved for future
/// cross-file type checking features.
pub trait ModuleRegistry: Send + Sync + fmt::Debug {
    /// Check if a module is registered
    fn has_module(&self, module_id: &str) -> bool;

    /// Get all registered module IDs
    fn all_modules(&self) -> Vec<String>;
}
