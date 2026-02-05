//! Diagnostic traits for decoupling from typedlua-core diagnostics

#![allow(dead_code)]

use super::type_analysis::Span;

/// Trait for collecting diagnostics during parsing/type checking
pub trait DiagnosticCollector: Send + Sync {
    /// Collect all diagnostics from parsing and checking a document
    ///
    /// # Arguments
    /// * `text` - The source code to analyze
    ///
    /// # Returns
    /// Vector of diagnostics (errors, warnings, info)
    fn collect_diagnostics(&self, text: &str) -> Vec<Diagnostic>;
}

/// LSP-compatible diagnostic
///
/// Represents an error, warning, or informational message without
/// depending on typedlua-core diagnostic types.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// Source location of the diagnostic
    pub span: Span,

    /// Severity level
    pub level: DiagnosticLevel,

    /// Human-readable message
    pub message: String,

    /// Optional diagnostic code
    pub code: Option<String>,

    /// Optional related information
    pub related: Vec<RelatedInformation>,
}

impl Diagnostic {
    /// Create a new diagnostic
    pub fn new(span: Span, level: DiagnosticLevel, message: String) -> Self {
        Self {
            span,
            level,
            message,
            code: None,
            related: Vec::new(),
        }
    }

    /// Create an error diagnostic
    pub fn error(span: Span, message: String) -> Self {
        Self::new(span, DiagnosticLevel::Error, message)
    }

    /// Create a warning diagnostic
    pub fn warning(span: Span, message: String) -> Self {
        Self::new(span, DiagnosticLevel::Warning, message)
    }

    /// Create an info diagnostic
    pub fn info(span: Span, message: String) -> Self {
        Self::new(span, DiagnosticLevel::Info, message)
    }

    /// Add a diagnostic code
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Add related information
    pub fn with_related(mut self, related: RelatedInformation) -> Self {
        self.related.push(related);
        self
    }
}

/// Diagnostic severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticLevel {
    /// Error that prevents compilation
    Error,

    /// Warning that should be addressed
    Warning,

    /// Informational message
    Info,

    /// Hint for improvement
    Hint,
}

/// Related information for a diagnostic
#[derive(Debug, Clone)]
pub struct RelatedInformation {
    /// Location of the related information
    pub span: Span,

    /// Message explaining the relationship
    pub message: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diagnostic_new() {
        let span = Span::new(0, 10, 1, 2);
        let diag = Diagnostic::new(span, DiagnosticLevel::Error, "test error".to_string());

        assert_eq!(diag.span.start, 0);
        assert_eq!(diag.span.end, 10);
        assert_eq!(diag.level, DiagnosticLevel::Error);
        assert_eq!(diag.message, "test error");
        assert!(diag.code.is_none());
        assert!(diag.related.is_empty());
    }

    #[test]
    fn test_diagnostic_error() {
        let span = Span::new(0, 5, 0, 0);
        let diag = Diagnostic::error(span, "syntax error".to_string());

        assert_eq!(diag.level, DiagnosticLevel::Error);
        assert_eq!(diag.message, "syntax error");
    }

    #[test]
    fn test_diagnostic_warning() {
        let span = Span::new(10, 15, 1, 0);
        let diag = Diagnostic::warning(span, "unused variable".to_string());

        assert_eq!(diag.level, DiagnosticLevel::Warning);
        assert_eq!(diag.message, "unused variable");
    }

    #[test]
    fn test_diagnostic_info() {
        let span = Span::new(20, 25, 2, 0);
        let diag = Diagnostic::info(span, "information".to_string());

        assert_eq!(diag.level, DiagnosticLevel::Info);
        assert_eq!(diag.message, "information");
    }

    #[test]
    fn test_diagnostic_with_code() {
        let span = Span::new(0, 5, 0, 0);
        let diag = Diagnostic::error(span, "error".to_string()).with_code("E001");

        assert_eq!(diag.code, Some("E001".to_string()));
    }

    #[test]
    fn test_diagnostic_with_related() {
        let span = Span::new(0, 5, 0, 0);
        let related = RelatedInformation {
            span: Span::new(10, 15, 1, 0),
            message: "related info".to_string(),
        };

        let diag = Diagnostic::error(span, "error".to_string()).with_related(related);

        assert_eq!(diag.related.len(), 1);
        assert_eq!(diag.related[0].message, "related info");
    }

    #[test]
    fn test_diagnostic_with_multiple_related() {
        let span = Span::new(0, 5, 0, 0);
        let diag = Diagnostic::error(span, "error".to_string())
            .with_related(RelatedInformation {
                span: Span::new(10, 15, 1, 0),
                message: "first".to_string(),
            })
            .with_related(RelatedInformation {
                span: Span::new(20, 25, 2, 0),
                message: "second".to_string(),
            });

        assert_eq!(diag.related.len(), 2);
    }

    #[test]
    fn test_diagnostic_level_variants() {
        let _ = DiagnosticLevel::Error;
        let _ = DiagnosticLevel::Warning;
        let _ = DiagnosticLevel::Info;
        let _ = DiagnosticLevel::Hint;
    }

    #[test]
    fn test_diagnostic_level_equality() {
        assert_eq!(DiagnosticLevel::Error, DiagnosticLevel::Error);
        assert_ne!(DiagnosticLevel::Error, DiagnosticLevel::Warning);
    }

    #[test]
    fn test_related_information_creation() {
        let related = RelatedInformation {
            span: Span::new(10, 20, 5, 3),
            message: "see here".to_string(),
        };

        assert_eq!(related.span.start, 10);
        assert_eq!(related.span.end, 20);
        assert_eq!(related.message, "see here");
    }

    #[test]
    fn test_diagnostic_builder_pattern() {
        let diag = Diagnostic::error(Span::new(0, 5, 0, 0), "main error".to_string())
            .with_code("E123")
            .with_related(RelatedInformation {
                span: Span::new(10, 15, 1, 0),
                message: "context".to_string(),
            });

        assert_eq!(diag.level, DiagnosticLevel::Error);
        assert_eq!(diag.code, Some("E123".to_string()));
        assert_eq!(diag.related.len(), 1);
    }
}
