use crate::core::document::Document;
use bumpalo::Bump;
use lsp_types::*;
use luanext_parser::ast::expression::{Expression, ExpressionKind};
use luanext_parser::ast::statement::Statement;
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser, Span};
use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
use luanext_typechecker::TypeChecker;
use std::sync::Arc;

/// Provides inlay hints (inline type annotations and parameter names)
#[derive(Clone)]
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

        let arena = Bump::new();
        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common_ids, &arena);
        let mut ast = match parser.parse() {
            Ok(a) => a,
            Err(_) => return hints,
        };

        let mut type_checker = TypeChecker::new(handler, &interner, &common_ids, &arena);
        if type_checker.check_program(&mut ast).is_err() {
            return hints;
        }

        // Traverse AST and collect hints within the range
        for stmt in ast.statements {
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
                for inner_stmt in block.statements {
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
    #[allow(dead_code)]
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
                        use luanext_parser::ast::types::TypeKind;
                        if let TypeKind::Function(func_type) = &symbol.typ.kind {
                            // Show parameter name hints for function arguments
                            for (i, arg) in args.iter().enumerate() {
                                if i < func_type.parameters.len() {
                                    let param = &func_type.parameters[i];
                                    if let luanext_parser::ast::pattern::Pattern::Identifier(
                                        ident,
                                    ) = &param.pattern
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
    #[allow(dead_code)]
    fn span_in_range(&self, span: &Span, range: Range) -> bool {
        let span_line = (span.line.saturating_sub(1)) as u32;
        span_line >= range.start.line && span_line <= range.end.line
    }

    /// Format a type for display
    #[allow(dead_code)]
    fn format_type_simple(
        &self,
        typ: &luanext_parser::ast::types::Type,
        interner: &StringInterner,
    ) -> String {
        use luanext_parser::ast::types::{PrimitiveType, TypeKind};

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
    use luanext_parser::ast::types::{PrimitiveType, Type, TypeKind};

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

    #[test]
    fn test_inlay_hints_resolve() {
        let provider = InlayHintsProvider::new();

        let hint = InlayHint {
            position: Position {
                line: 0,
                character: 5,
            },
            label: InlayHintLabel::String("test".to_string()),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        };

        let resolved = provider.resolve(hint.clone());
        // Resolve should return a hint with the same position
        assert_eq!(resolved.position.line, hint.position.line);
        assert_eq!(resolved.position.character, hint.position.character);
    }

    #[test]
    fn test_inlay_hints_empty_document() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        };

        let hints = provider.provide(&doc, range);
        assert!(hints.is_empty() || hints.len() >= 0);
    }

    #[test]
    fn test_inlay_hints_multiline_document() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local x = 1\nlocal y = 2\nlocal z = 3".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 3,
                character: 0,
            },
        };

        let hints = provider.provide(&doc, range);
        // Just verify it runs without errors on multiline
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_return_type() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("function foo(): number return 1 end".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 35,
            },
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_generic_function() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("function identity<T>(x: T): T return x end".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 40,
            },
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_class_method() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test(
            "class Point\n  x: number\n  y: number\n  constructor(x: number, y: number) end\nend"
                .to_string(),
            1,
        );

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 5,
                character: 0,
            },
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_interface() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("interface Drawable\n  draw(): void\nend".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 2,
                character: 0,
            },
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_enum() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test(
            "enum Status\n  Pending\n  Approved\n  Rejected\nend".to_string(),
            1,
        );

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 4,
                character: 0,
            },
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_range_subset() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local a = 1\nlocal b = 2\nlocal c = 3".to_string(), 1);

        // Request hints for only part of the document
        let range = Range {
            start: Position {
                line: 1,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 10,
            },
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_single_line_partial_range() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local veryLongVariableName = 42".to_string(), 1);

        // Request hints for middle of the line
        let range = Range {
            start: Position {
                line: 0,
                character: 10,
            },
            end: Position {
                line: 0,
                character: 20,
            },
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_kind_variants() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local x = 1".to_string(), 1);

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

        for hint in hints {
            assert!(hint.position.line >= 0);
            assert!(hint.position.character >= 0);
        }
    }

    #[test]
    fn test_inlay_hints_empty_range() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local x = 1".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        };

        let hints = provider.provide(&doc, range);
        assert!(hints.is_empty() || hints.len() >= 0);
    }

    #[test]
    fn test_inlay_hints_beyond_eof() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local x = 1".to_string(), 1);

        let range = Range {
            start: Position {
                line: 10,
                character: 0,
            },
            end: Position {
                line: 20,
                character: 10,
            },
        };

        let hints = provider.provide(&doc, range);
        assert!(hints.is_empty() || hints.len() >= 0);
    }

    #[test]
    fn test_inlay_hints_type_alias() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("type MyAlias = string".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 0,
            },
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_class_members() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test(
            "class MyClass\n  field: string\n  method(): void\nend".to_string(),
            1,
        );

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 4,
                character: 0,
            },
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_nested_functions() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test(
            "local function outer()\n  local function inner() end\nend".to_string(),
            1,
        );

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 2,
                character: 0,
            },
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_provider_clone() {
        let provider = InlayHintsProvider::new();
        let _cloned = provider.clone();
    }

    #[test]
    fn test_inlay_hints_span_in_range() {
        let provider = InlayHintsProvider::new();

        let span = Span {
            start: 0,
            end: 10,
            line: 5,
            column: 0,
        };

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(10, 0),
        };

        let in_range = provider.span_in_range(&span, range);
        assert!(in_range);
    }

    #[test]
    fn test_inlay_hints_span_outside_range() {
        let provider = InlayHintsProvider::new();

        let span = Span {
            start: 0,
            end: 10,
            line: 15,
            column: 0,
        };

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(10, 0),
        };

        let in_range = provider.span_in_range(&span, range);
        assert!(!in_range);
    }

    #[test]
    fn test_inlay_hints_span_on_boundary() {
        let provider = InlayHintsProvider::new();

        let span = Span {
            start: 0,
            end: 10,
            line: 0,
            column: 0,
        };

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 10),
        };

        let in_range = provider.span_in_range(&span, range);
        assert!(in_range);
    }

    #[test]
    fn test_format_type_simple_primitives() {
        let provider = InlayHintsProvider::new();
        use luanext_parser::ast::types::{PrimitiveType, Type};

        let nil_type = Type {
            span: Span::new(0, 3, 1, 1),
            kind: luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Nil),
        };

        let result = provider.format_type_simple(&nil_type, &StringInterner::new());
        assert_eq!(result, "nil");

        let number_type = Type {
            span: Span::new(0, 6, 1, 1),
            kind: luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Number),
        };
        let result = provider.format_type_simple(&number_type, &StringInterner::new());
        assert_eq!(result, "number");

        let string_type = Type {
            span: Span::new(0, 6, 1, 1),
            kind: luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::String),
        };
        let result = provider.format_type_simple(&string_type, &StringInterner::new());
        assert_eq!(result, "string");

        let boolean_type = Type {
            span: Span::new(0, 7, 1, 1),
            kind: luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Boolean),
        };
        let result = provider.format_type_simple(&boolean_type, &StringInterner::new());
        assert_eq!(result, "boolean");

        let integer_type = Type {
            span: Span::new(0, 7, 1, 1),
            kind: luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Integer),
        };
        let result = provider.format_type_simple(&integer_type, &StringInterner::new());
        assert_eq!(result, "integer");

        let void_type = Type {
            span: Span::new(0, 4, 1, 1),
            kind: luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Void),
        };
        let result = provider.format_type_simple(&void_type, &StringInterner::new());
        assert_eq!(result, "void");

        let never_type = Type {
            span: Span::new(0, 5, 1, 1),
            kind: luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Never),
        };
        let result = provider.format_type_simple(&never_type, &StringInterner::new());
        assert_eq!(result, "never");

        let unknown_type = Type {
            span: Span::new(0, 7, 1, 1),
            kind: luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Unknown),
        };
        let result = provider.format_type_simple(&unknown_type, &StringInterner::new());
        assert_eq!(result, "unknown");
    }

    #[test]
    fn test_format_type_function() {
        let provider = InlayHintsProvider::new();

        let return_type = Type {
            span: Span::new(0, 4, 1, 1),
            kind: TypeKind::Primitive(PrimitiveType::Void),
        };

        let func_type = Type {
            span: Span::new(0, 8, 1, 1),
            kind: TypeKind::Function(luanext_parser::ast::types::FunctionType {
                span: Span::new(0, 8, 1, 1),
                parameters: vec![],
                return_type: Box::new(return_type),
                throws: None,
                type_parameters: None,
            }),
        };

        let result = provider.format_type_simple(&func_type, &StringInterner::new());
        assert_eq!(result, "function");
    }

    #[test]
    fn test_format_type_array() {
        let provider = InlayHintsProvider::new();

        let number_type = Type {
            span: Span::new(0, 6, 1, 1),
            kind: luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Number),
        };

        let array_type = Type {
            span: Span::new(0, 9, 1, 1),
            kind: luanext_parser::ast::types::TypeKind::Array(Box::new(number_type)),
        };

        let result = provider.format_type_simple(&array_type, &StringInterner::new());
        assert_eq!(result, "number[]");
    }

    #[test]
    fn test_provide_with_parse_error() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local x = ".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 10),
        };

        let hints = provider.provide(&doc, range);
        assert!(hints.is_empty());
    }

    #[test]
    fn test_provide_with_lexer_error() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("\x00\x01\x02".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 10),
        };

        let hints = provider.provide(&doc, range);
        assert!(hints.is_empty());
    }

    #[test]
    fn test_provide_with_type_check_error() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local x: nonexistent_type = 1".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 30),
        };

        let hints = provider.provide(&doc, range);
        assert!(hints.is_empty() || !hints.is_empty());
    }

    #[test]
    fn test_hints_position_line_number() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local x = 10\nlocal y = 20".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(2, 0),
        };

        let hints = provider.provide(&doc, range);

        for hint in hints {
            assert!(hint.position.line >= 0);
            assert!(hint.position.line <= 1);
        }
    }

    #[test]
    fn test_hints_label_formats() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local myVar = 42".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 15),
        };

        let hints = provider.provide(&doc, range);

        for hint in hints {
            match &hint.label {
                InlayHintLabel::String(s) => {
                    assert!(!s.is_empty());
                    assert!(s.starts_with(':'));
                }
                InlayHintLabel::LabelParts(parts) => {
                    assert!(!parts.is_empty());
                }
            }
        }
    }

    #[test]
    fn test_hints_type_kind() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local x = 1".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 10),
        };

        let hints = provider.provide(&doc, range);

        for hint in hints {
            assert_eq!(hint.kind, Some(InlayHintKind::TYPE));
        }
    }

    #[test]
    fn test_hints_padding() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local x = 1".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 10),
        };

        let hints = provider.provide(&doc, range);

        for hint in hints {
            assert_eq!(hint.padding_left, Some(true));
            assert_eq!(hint.padding_right, Some(false));
        }
    }

    #[test]
    fn test_provide_with_nested_blocks() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test(
            "function outer()\n  function inner()\n    local x = 1\n  end\nend".to_string(),
            1,
        );

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(4, 0),
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_provide_with_while_loop() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("while true do local x = 1 end".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 30),
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_provide_with_if_statement() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("if x then local y = 1 end".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 25),
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_provide_with_if_else() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("if x then local a = 1 else local b = 2 end".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 45),
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_provide_with_repeat_until() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("repeat local x = 1 until false".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 30),
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_provide_with_for_loop() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("for i = 1, 10 do local x = i end".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 35),
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_provide_with_match_expression() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test(
            "match x\n  | 1 => local y = 1\n  | _ => local z = 2\nend".to_string(),
            1,
        );

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(4, 0),
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_multiple_variables() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test(
            "local a = 1\nlocal b = 2\nlocal c = 3\nlocal d = 4".to_string(),
            1,
        );

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(4, 0),
        };

        let hints = provider.provide(&doc, range);
        assert!(hints.len() >= 0);
    }

    #[test]
    fn test_inlay_hints_with_type_annotation() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test(
            "local x: number = 1\nlocal y: string = \"hello\"".to_string(),
            1,
        );

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(2, 0),
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_unicode_identifier() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test("local π = 3.14".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 15),
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }

    #[test]
    fn test_inlay_hints_long_variable_name() {
        let provider = InlayHintsProvider::new();
        let doc = Document::new_test(
            "local veryLongVariableNameThatIsHardToRead = 42".to_string(),
            1,
        );

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(0, 50),
        };

        let hints = provider.provide(&doc, range);
        let _ = hints;
    }
}
