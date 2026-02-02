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
