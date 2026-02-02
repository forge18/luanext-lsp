use crate::document::Document;
use lsp_types::*;
use std::sync::Arc;
<<<<<<< HEAD
use typedlua_core::diagnostics::{CollectingDiagnosticHandler, DiagnosticHandler, DiagnosticLevel};
use typedlua_core::typechecker::TypeChecker;
use typedlua_parser::lexer::Lexer;
use typedlua_parser::parser::Parser;
use typedlua_parser::span::Span;
use typedlua_parser::string_interner::StringInterner;
=======
use typedlua_parser::diagnostics::{CollectingDiagnosticHandler, DiagnosticLevel};
use typedlua_parser::string_interner::StringInterner;
use typedlua_typechecker::TypeChecker;
use typedlua_parser::{DiagnosticHandler, Lexer, Parser, Span};
>>>>>>> b9886cd (Refactor dependencies and update imports to use typedlua_parser and typedlua_typechecker)

/// Provides diagnostics (errors and warnings) for documents
pub struct DiagnosticsProvider;

impl DiagnosticsProvider {
    pub fn new() -> Self {
        Self
    }

    /// Analyze a document and return diagnostics
    pub fn provide(&self, document: &Document) -> Vec<Diagnostic> {
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
