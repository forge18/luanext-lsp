use crate::document::Document;
use lsp_types::*;
use std::sync::Arc;
use typedlua_parser::ast::expression::{Expression, ExpressionKind};
use typedlua_parser::ast::statement::Statement;
use typedlua_typechecker::diagnostics::CollectingDiagnosticHandler;
use typedlua_parser::string_interner::StringInterner;
use typedlua_typechecker::TypeChecker;
use typedlua_parser::{Lexer, Parser, Span};

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
        stmt: &Statement,
        type_checker: &TypeChecker,
        range: Range,
        hints: &mut Vec<InlayHint>,
        interner: &StringInterner,
    ) {
        use typedlua_parser::ast::pattern::Pattern;

        match stmt {
            Statement::Variable(decl) => {
                if decl.type_annotation.is_none() {
                    let stmt_start = span_to_position_start(&decl.span);
                    let stmt_end = span_to_position_end(&decl.span);

                    if stmt_start.line >= range.start.line && stmt_end.line <= range.end.line {
                        let pattern_end = span_to_position_end(&decl.pattern.span());

                        hints.push(InlayHint {
                            position: pattern_end,
                            label: InlayHintLabel::String(": unknown".to_string()),
                            kind: Some(InlayHintKind::TYPE),
                            text_edits: None,
                            tooltip: None,
                            padding_left: Some(true),
                            padding_right: Some(false),
                            data: None,
                        });
                    }
                }
            }
            Statement::Block(block) => {
                for inner_stmt in &block.statements {
                    self.collect_hints_from_statement(
                        inner_stmt,
                        type_checker,
                        range,
                        hints,
                        interner,
                    );
                }
            }
            _ => {}
        }
    }

    /// Collect parameter name hints from function calls
    fn collect_hints_from_expression(
        &self,
        expr: &Expression,
        type_checker: &TypeChecker,
        range: Range,
        hints: &mut Vec<InlayHint>,
        interner: &StringInterner,
    ) {
        match &expr.kind {
            ExpressionKind::Call(callee, args, _type_args) => {
                // Try to get the function name for parameter hints
                if let ExpressionKind::Identifier(func_name) = &callee.kind {
                    let func_name_str = interner.resolve(*func_name);
                    if let Some(symbol) = type_checker.lookup_symbol(&func_name_str) {
                        // Get function type parameters
                        use typedlua_parser::ast::types::TypeKind;
                        if let TypeKind::Function(func_type) = &symbol.typ.kind {
                            // Show parameter name hints for function arguments
                            for (i, arg) in args.iter().enumerate() {
                                if i < func_type.parameters.len() {
                                    let param = &func_type.parameters[i];
                                    if let typedlua_parser::ast::pattern::Pattern::Identifier(ident) =
                                        &param.pattern
                                    {
                                        if self.span_in_range(&arg.span, range) {
                                            let position = span_to_position_start(&arg.span);

                                            hints.push(InlayHint {
                                                position,
                                                label: InlayHintLabel::String(format!(
                                                    "{}: ",
                                                    ident.node
                                                )),
                                                kind: Some(InlayHintKind::PARAMETER),
                                                text_edits: None,
                                                tooltip: None,
                                                padding_left: Some(false),
                                                padding_right: Some(false),
                                                data: None,
                                            });
                                        }
                                    }
                                }

                                // Recursively check nested expressions
                                self.collect_hints_from_expression(
                                    &arg.value,
                                    type_checker,
                                    range,
                                    hints,
                                    interner,
                                );
                            }
                        }
                    }
                }

                // Also check the callee for nested calls
                self.collect_hints_from_expression(callee, type_checker, range, hints, interner);
            }
            ExpressionKind::Binary(_, left, right) => {
                self.collect_hints_from_expression(left, type_checker, range, hints, interner);
                self.collect_hints_from_expression(right, type_checker, range, hints, interner);
            }
            ExpressionKind::Unary(_, operand) => {
                self.collect_hints_from_expression(operand, type_checker, range, hints, interner);
            }
            ExpressionKind::Member(object, _) => {
                self.collect_hints_from_expression(object, type_checker, range, hints, interner);
            }
            ExpressionKind::Index(object, index) => {
                self.collect_hints_from_expression(object, type_checker, range, hints, interner);
                self.collect_hints_from_expression(index, type_checker, range, hints, interner);
            }
            ExpressionKind::Assignment(target, _, value) => {
                self.collect_hints_from_expression(target, type_checker, range, hints, interner);
                self.collect_hints_from_expression(value, type_checker, range, hints, interner);
            }
            ExpressionKind::Conditional(condition, then_expr, else_expr) => {
                self.collect_hints_from_expression(condition, type_checker, range, hints, interner);
                self.collect_hints_from_expression(then_expr, type_checker, range, hints, interner);
                self.collect_hints_from_expression(else_expr, type_checker, range, hints, interner);
            }
            ExpressionKind::Parenthesized(inner) => {
                self.collect_hints_from_expression(inner, type_checker, range, hints, interner);
            }
            _ => {}
        }
    }

    /// Check if a span is within the requested range
    fn span_in_range(&self, span: &Span, range: Range) -> bool {
        let span_line = (span.line.saturating_sub(1)) as u32;
        span_line >= range.start.line && span_line <= range.end.line
    }

    /// Format a type for display
    #[allow(dead_code)]
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
#[allow(dead_code)]
fn span_to_position_start(span: &Span) -> Position {
    Position {
        line: (span.line.saturating_sub(1)) as u32,
        character: (span.column.saturating_sub(1)) as u32,
    }
}

/// Convert a Span to an LSP Position (end)
#[allow(dead_code)]
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
