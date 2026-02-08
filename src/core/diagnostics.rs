use crate::core::document::Document;
use crate::traits::DiagnosticsProviderTrait;
use lsp_types::*;
use std::sync::Arc;
use typedlua_parser::string_interner::StringInterner;
use typedlua_parser::{Lexer, Parser, Span};
use typedlua_typechecker::cli::diagnostics::{
    CollectingDiagnosticHandler, DiagnosticHandler, DiagnosticLevel,
};
use typedlua_typechecker::TypeChecker;

/// Provides diagnostics (errors and warnings) for documents
#[derive(Clone)]
pub struct DiagnosticsProvider;

impl DiagnosticsProvider {
    pub fn new() -> Self {
        Self
    }

    /// Analyze a document and return diagnostics (internal method)
    pub fn provide_impl(&self, document: &Document) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Create a diagnostic handler to collect errors
        let handler = Arc::new(CollectingDiagnosticHandler::new());

        // Create interner
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();

        // Lex the document
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(_) => {
                // Collect lexer diagnostics
                return Self::convert_diagnostics(handler);
            }
        };

        // Parse the document
        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common_ids);
        let mut ast = match parser.parse() {
            Ok(a) => a,
            Err(_) => {
                // Collect parser diagnostics
                return Self::convert_diagnostics(handler);
            }
        };

        // Type check the document
        let mut type_checker = TypeChecker::new(handler.clone(), &interner, &common_ids);
        if let Err(_) = type_checker.check_program(&mut ast) {
            return Self::convert_diagnostics(handler);
        }

        // If we get here and there are still diagnostics (warnings), include them
        diagnostics.extend(Self::convert_diagnostics(handler));
        diagnostics
    }

    /// Convert core diagnostics to LSP diagnostics
    fn convert_diagnostics(handler: Arc<CollectingDiagnosticHandler>) -> Vec<Diagnostic> {
        handler
            .get_diagnostics()
            .into_iter()
            .map(|d| Diagnostic {
                range: span_to_range(&d.span),
                severity: Some(match d.level {
                    DiagnosticLevel::Error => DiagnosticSeverity::ERROR,
                    DiagnosticLevel::Warning => DiagnosticSeverity::WARNING,
                    DiagnosticLevel::Info => DiagnosticSeverity::INFORMATION,
                }),
                code: None, // Core diagnostics don't have error codes yet
                source: Some("typedlua".to_string()),
                message: d.message,
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            })
            .collect()
    }
}

/// Convert a Span to an LSP Range
fn span_to_range(span: &Span) -> Range {
    Range {
        start: Position {
            line: (span.line.saturating_sub(1)) as u32,
            character: (span.column.saturating_sub(1)) as u32,
        },
        end: Position {
            // Span only has start position, so end is start + length
            line: (span.line.saturating_sub(1)) as u32,
            character: ((span.column + span.len()).saturating_sub(1)) as u32,
        },
    }
}

impl DiagnosticsProviderTrait for DiagnosticsProvider {
    fn provide(&self, document: &Document) -> Vec<Diagnostic> {
        self.provide_impl(document)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::document::Document;

    #[test]
    fn test_diagnostics_provider_new() {
        let provider = DiagnosticsProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_diagnostics_empty_document() {
        let doc = Document::new_test("".to_string(), 1);
        let provider = DiagnosticsProvider::new();

        let diagnostics = provider.provide(&doc);

        // Empty document should have no diagnostics
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostics_valid_code() {
        let doc = Document::new_test("local x = 1".to_string(), 1);
        let provider = DiagnosticsProvider::new();

        let diagnostics = provider.provide(&doc);

        // Valid code should have no diagnostics
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostics_function_declaration() {
        let doc = Document::new_test("function foo() end".to_string(), 1);
        let provider = DiagnosticsProvider::new();

        let diagnostics = provider.provide(&doc);

        // Valid function should have no diagnostics
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostics_with_type_annotation() {
        let doc = Document::new_test("local x: number = 1".to_string(), 1);
        let provider = DiagnosticsProvider::new();

        let diagnostics = provider.provide(&doc);

        // Correctly typed code should have no diagnostics
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostics_multiple_statements() {
        let code = "local x = 1\nlocal y = 2\nlocal z = x + y";
        let doc = Document::new_test(code.to_string(), 1);
        let provider = DiagnosticsProvider::new();

        let diagnostics = provider.provide(&doc);

        // Valid multi-statement code should have no diagnostics
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_span_to_range_basic() {
        let span = Span {
            start: 0,
            end: 10,
            line: 1,
            column: 1,
        };

        let range = span_to_range(&span);

        // Line and column are 0-indexed in LSP
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, 10);
    }

    #[test]
    fn test_span_to_range_multiline() {
        let span = Span {
            start: 0,
            end: 5,
            line: 5,
            column: 3,
        };

        let range = span_to_range(&span);

        assert_eq!(range.start.line, 4);
        assert_eq!(range.start.character, 2);
        assert_eq!(range.end.line, 4);
        assert_eq!(range.end.character, 7);
    }

    #[test]
    fn test_span_to_range_at_position_zero() {
        let span = Span {
            start: 0,
            end: 0,
            line: 0,
            column: 0,
        };

        let range = span_to_range(&span);

        // Should handle zero values gracefully (saturating_sub)
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
    }

    #[test]
    fn test_diagnostics_provider_trait() {
        let doc = Document::new_test("local x = 1".to_string(), 1);
        let provider: Box<dyn DiagnosticsProviderTrait> = Box::new(DiagnosticsProvider::new());

        let diagnostics = provider.provide(&doc);

        // Should work through trait
        assert!(diagnostics.is_empty());
    }
}
