//! Type analysis traits for decoupling from typedlua-core type checker

#![allow(dead_code)]

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_new() {
        let span = Span::new(10, 20, 5, 3);
        assert_eq!(span.start, 10);
        assert_eq!(span.end, 20);
        assert_eq!(span.line, 5);
        assert_eq!(span.column, 3);
    }

    #[test]
    fn test_span_len() {
        let span = Span::new(10, 20, 0, 0);
        assert_eq!(span.len(), 10);
    }

    #[test]
    fn test_span_len_zero() {
        let span = Span::new(10, 10, 0, 0);
        assert_eq!(span.len(), 0);
    }

    #[test]
    fn test_span_is_empty_true() {
        let span = Span::new(10, 10, 0, 0);
        assert!(span.is_empty());
    }

    #[test]
    fn test_span_is_empty_false() {
        let span = Span::new(10, 11, 0, 0);
        assert!(!span.is_empty());
    }

    #[test]
    fn test_symbol_kind_variants() {
        // Ensure all variants can be created
        let _ = SymbolKind::Variable;
        let _ = SymbolKind::Const;
        let _ = SymbolKind::Function;
        let _ = SymbolKind::Class;
        let _ = SymbolKind::Interface;
        let _ = SymbolKind::Type;
        let _ = SymbolKind::Enum;
        let _ = SymbolKind::Property;
        let _ = SymbolKind::Method;
        let _ = SymbolKind::Parameter;
        let _ = SymbolKind::Namespace;
    }

    #[test]
    fn test_symbol_kind_equality() {
        assert_eq!(SymbolKind::Variable, SymbolKind::Variable);
        assert_ne!(SymbolKind::Variable, SymbolKind::Function);
    }

    #[test]
    fn test_symbol_info_creation() {
        let info = SymbolInfo {
            name: "test".to_string(),
            kind: SymbolKind::Variable,
            type_annotation: Some("number".to_string()),
            span: Span::new(0, 4, 0, 0),
            is_exported: false,
            references: vec![],
        };

        assert_eq!(info.name, "test");
        assert_eq!(info.kind, SymbolKind::Variable);
        assert_eq!(info.type_annotation, Some("number".to_string()));
        assert!(!info.is_exported);
        assert!(info.references.is_empty());
    }

    #[test]
    fn test_symbol_info_with_references() {
        let info = SymbolInfo {
            name: "x".to_string(),
            kind: SymbolKind::Variable,
            type_annotation: None,
            span: Span::new(0, 1, 0, 0),
            is_exported: true,
            references: vec![Span::new(10, 11, 1, 0), Span::new(20, 21, 2, 0)],
        };

        assert!(info.is_exported);
        assert_eq!(info.references.len(), 2);
    }

    #[test]
    fn test_type_check_result_creation() {
        let result = TypeCheckResult {
            diagnostics: vec![],
            symbol_info: None,
        };

        assert!(result.diagnostics.is_empty());
        assert!(result.symbol_info.is_none());
    }
}
