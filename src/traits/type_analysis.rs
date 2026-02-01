//! Type analysis traits for decoupling from typedlua-core type checker

use super::diagnostics::Diagnostic;

/// Trait for type checking documents
pub trait TypeChecker: Send + Sync {
    /// Type check a document and return diagnostics and symbol information
    ///
    /// # Arguments
    /// * `text` - The source code to type check
    ///
    /// # Returns
    /// Type checking result with diagnostics and optional symbol store
    fn check_document(&self, text: &str) -> TypeCheckResult;
}

/// Result of type checking a document
pub struct TypeCheckResult {
    /// Diagnostics (errors, warnings) from type checking
    pub diagnostics: Vec<Diagnostic>,

    /// Symbol information extracted during type checking
    pub symbol_info: Option<Box<dyn SymbolStore>>,
}

/// Trait for symbol table storage
///
/// Provides access to symbol information without depending on
/// concrete SymbolTable implementation from typedlua-core.
pub trait SymbolStore: Send + Sync {
    /// Get symbol at a specific position
    ///
    /// # Arguments
    /// * `line` - Zero-indexed line number
    /// * `column` - Zero-indexed column number
    ///
    /// # Returns
    /// Symbol information if a symbol exists at that position
    fn get_symbol_at_position(&self, line: usize, column: usize) -> Option<SymbolInfo>;

    /// Get all symbols in the document
    fn all_symbols(&self) -> Vec<SymbolInfo>;

    /// Get all symbols visible in a given scope
    fn visible_symbols(&self) -> Vec<SymbolInfo> {
        self.all_symbols()
    }
}

/// Language-agnostic symbol information
///
/// Represents a symbol (variable, function, class, etc.) without
/// depending on typedlua-core types.
#[derive(Debug, Clone)]
pub struct SymbolInfo {
    /// Symbol name
    pub name: String,

    /// Symbol kind (variable, function, etc.)
    pub kind: SymbolKind,

    /// Type annotation if available
    pub type_annotation: Option<String>,

    /// Source location span
    pub span: Span,

    /// Whether this symbol is exported from the module
    pub is_exported: bool,

    /// References to this symbol
    pub references: Vec<Span>,
}

/// Symbol kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Variable,
    Const,
    Function,
    Class,
    Interface,
    Type,
    Enum,
    Property,
    Method,
    Parameter,
    Namespace,
}

/// Source code location span
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// Starting byte offset
    pub start: usize,

    /// Ending byte offset
    pub end: usize,

    /// Starting line (zero-indexed)
    pub line: usize,

    /// Starting column (zero-indexed)
    pub column: usize,
}

impl Span {
    /// Create a new span
    pub fn new(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    /// Get the length of this span
    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    /// Check if this span is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
