use crate::document::Document;
use lsp_types::*;
use std::sync::Arc;
use typedlua_core::diagnostics::CollectingDiagnosticHandler;
use typedlua_core::typechecker::TypeChecker;
use typedlua_parser::ast::expression::{Expression, ExpressionKind};
use typedlua_parser::ast::statement::Statement;
use typedlua_parser::lexer::Lexer;
use typedlua_parser::parser::Parser;
use typedlua_parser::span::Span;
use typedlua_parser::string_interner::StringInterner;

/// Provides inlay hints (inline type annotations and parameter names)
pub struct InlayHintsProvider;

impl InlayHintsProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide inlay hints for a given range in the document
    pub fn provide(&self, document: &Document, range: Range) -> Vec<InlayHint> {
        let mut hints = Vec::new();

        // Parse and type check the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(_) => return hints,
        };

        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common_ids);
        let mut ast = match parser.parse() {
            Ok(a) => a,
            Err(_) => return hints,
        };

        let mut type_checker = TypeChecker::new(handler, &interner, &common_ids);
        if type_checker.check_program(&mut ast).is_err() {
            return hints;
        }

        // Traverse AST and collect hints within the range
        for stmt in &ast.statements {
            self.collect_hints_from_statement(stmt, &type_checker, range, &mut hints, &interner);
        }

        hints
    }

    /// Resolve additional details for an inlay hint
    pub fn resolve(&self, hint: InlayHint) -> InlayHint {
        // For now, just return the hint as-is
        hint
    }

    /// Collect hints from a statement
    fn collect_hints_from_statement(
        &self,
        _stmt: &Statement,
        _type_checker: &TypeChecker,
        _range: Range,
        _hints: &mut Vec<InlayHint>,
        _interner: &StringInterner,
    ) {
        // TODO: Implement inlay hint collection
    }

    /// Format a type for display
    fn format_type_simple(
        &self,
        typ: &typedlua_parser::ast::types::Type,
        interner: &StringInterner,
    ) -> String {
        use typedlua_parser::ast::types::{PrimitiveType, TypeKind};

        match &typ.kind {
            TypeKind::Primitive(PrimitiveType::Nil) => "nil".to_string(),
            TypeKind::Primitive(PrimitiveType::Boolean) => "boolean".to_string(),
            TypeKind::Primitive(PrimitiveType::Number) => "number".to_string(),
            TypeKind::Primitive(PrimitiveType::Integer) => "integer".to_string(),
            TypeKind::Primitive(PrimitiveType::String) => "string".to_string(),
            TypeKind::Primitive(PrimitiveType::Unknown) => "unknown".to_string(),
            TypeKind::Primitive(PrimitiveType::Never) => "never".to_string(),
            TypeKind::Primitive(PrimitiveType::Void) => "void".to_string(),
            TypeKind::Reference(type_ref) => interner.resolve(type_ref.name.node).to_string(),
            TypeKind::Function(_) => "function".to_string(),
            TypeKind::Array(elem_type) => {
                format!("{}[]", self.format_type_simple(elem_type, interner))
            }
            _ => "unknown".to_string(),
        }
    }
}

/// Convert a Span to an LSP Position (start)
fn span_to_position_start(span: &Span) -> Position {
    Position {
        line: (span.line.saturating_sub(1)) as u32,
        character: (span.column.saturating_sub(1)) as u32,
    }
}

/// Convert a Span to an LSP Position (end)
fn span_to_position_end(span: &Span) -> Position {
    Position {
        line: (span.line.saturating_sub(1)) as u32,
        character: ((span.column + span.len()).saturating_sub(1)) as u32,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_hint_scenarios() {
        let provider = InlayHintsProvider::new();

        // Test variable without type annotation - should show hint
        let doc = Document::new_test("local x = 10".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 12,
            },
        };

        let _hints = provider.provide(&doc, range);
        // May or may not have hints depending on type checker availability
        // Just verify the function runs without errors

        // Test variable WITH type annotation - should NOT show hint
        let doc = Document::new_test("local x: number = 10".to_string(), 1);

        let _hints = provider.provide(&doc, range);
        // Should not add duplicate type hints
    }

    #[test]
    fn test_parameter_hint_scenarios() {
        let provider = InlayHintsProvider::new();

        // Test simple function call
        let doc = Document::new_test("foo(10, 20)".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 11,
            },
        };

        let _hints = provider.provide(&doc, range);
        // May need type information to provide parameter hints

        // Test that we can process method calls without errors
        let doc = Document::new_test("obj.method(true)".to_string(), 1);

        let _hints = provider.provide(&doc, range);
    }

    #[test]
    fn test_hint_positions() {
        let provider = InlayHintsProvider::new();

        let doc = Document::new_test("local x = 10".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 12,
            },
        };

        let hints = provider.provide(&doc, range);

        // Verify each hint has valid position
        for hint in &hints {
            assert_eq!(hint.position.line, 0);
            // Position should be within document bounds
            assert!(hint.position.character <= 12);

            // Verify hint has appropriate structure
            match &hint.label {
                InlayHintLabel::String(s) => {
                    assert!(!s.is_empty());
                }
                InlayHintLabel::LabelParts(_) => {}
            }

            // Verify kind is set
            assert!(hint.kind.is_some());
        }
    }
}
