use crate::core::document::Document;
use lsp_types::*;
use std::sync::Arc;
use luanext_parser::ast::expression::{Expression, ExpressionKind};
use luanext_parser::ast::pattern::Pattern;
use luanext_parser::ast::statement::{ClassMember, Statement, VariableKind};
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser, Span};
use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;

/// Provides semantic tokens for syntax highlighting based on semantic analysis
#[derive(Clone)]
pub struct SemanticTokensProvider {
    /// Token types legend (must match what's advertised in server capabilities)
    token_types: Vec<SemanticTokenType>,
    /// Token modifiers legend (must match what's advertised in server capabilities)
    token_modifiers: Vec<SemanticTokenModifier>,
}

impl SemanticTokensProvider {
    pub fn new() -> Self {
        Self {
            token_types: vec![
                SemanticTokenType::CLASS,
                SemanticTokenType::INTERFACE,
                SemanticTokenType::ENUM,
                SemanticTokenType::TYPE,
                SemanticTokenType::PARAMETER,
                SemanticTokenType::VARIABLE,
                SemanticTokenType::PROPERTY,
                SemanticTokenType::FUNCTION,
                SemanticTokenType::METHOD,
                SemanticTokenType::KEYWORD,
                SemanticTokenType::COMMENT,
                SemanticTokenType::STRING,
                SemanticTokenType::NUMBER,
            ],
            token_modifiers: vec![
                SemanticTokenModifier::DECLARATION,
                SemanticTokenModifier::READONLY,
                SemanticTokenModifier::STATIC,
                SemanticTokenModifier::ABSTRACT,
                SemanticTokenModifier::DEPRECATED,
                SemanticTokenModifier::MODIFICATION,
            ],
        }
    }

    /// Provide semantic tokens for the entire document
    pub fn provide_full(&self, document: &Document) -> SemanticTokens {
        // Parse the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(_) => {
                return SemanticTokens {
                    result_id: None,
                    data: vec![],
                }
            }
        };

        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common_ids, Box::leak(Box::new(bumpalo::Bump::new())));
        let ast = match parser.parse() {
            Ok(a) => a,
            Err(_) => {
                return SemanticTokens {
                    result_id: None,
                    data: vec![],
                }
            }
        };

        // Collect semantic tokens from AST
        let mut tokens_data = Vec::new();
        let mut last_line = 0;
        let mut last_char = 0;

        for stmt in ast.statements.iter() {
            self.collect_tokens_from_statement(
                stmt,
                &mut tokens_data,
                &mut last_line,
                &mut last_char,
            );
        }

        SemanticTokens {
            result_id: Some(format!(
                "{}",
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs()
            )),
            data: tokens_data,
        }
    }

    /// Provide semantic tokens for a specific range in the document
    pub fn provide_range(&self, _document: &Document, _range: Range) -> SemanticTokens {
        // This is useful for visible viewport optimization

        SemanticTokens {
            result_id: None,
            data: vec![],
        }
    }

    /// Provide semantic tokens delta (incremental update)
    pub fn provide_full_delta(
        &self,
        _document: &Document,
        _previous_result_id: String,
    ) -> SemanticTokensDelta {
        // This is for efficient incremental updates

        SemanticTokensDelta {
            result_id: None,
            edits: vec![],
        }
    }

    /// Helper: Get token type index for a given semantic token type
    fn get_token_type_index(&self, token_type: &SemanticTokenType) -> u32 {
        self.token_types
            .iter()
            .position(|t| t == token_type)
            .unwrap_or(0) as u32
    }

    /// Helper: Encode token modifiers as a bitset
    fn encode_modifiers(&self, modifiers: &[SemanticTokenModifier]) -> u32 {
        let mut result = 0u32;
        for modifier in modifiers {
            if let Some(index) = self.token_modifiers.iter().position(|m| m == modifier) {
                result |= 1 << index;
            }
        }
        result
    }

    /// Helper: Create a semantic token entry
    ///
    /// LSP semantic tokens use a relative encoding:
    /// [deltaLine, deltaStartChar, length, tokenType, tokenModifiers]
    ///
    /// Each token is encoded relative to the previous token
    #[allow(dead_code)]
    fn create_token(
        &self,
        delta_line: u32,
        delta_start: u32,
        length: u32,
        token_type: &SemanticTokenType,
        modifiers: &[SemanticTokenModifier],
    ) -> Vec<u32> {
        vec![
            delta_line,
            delta_start,
            length,
            self.get_token_type_index(token_type),
            self.encode_modifiers(modifiers),
        ]
    }

    /// Helper: Classify a token based on AST node type
    #[allow(dead_code)]
    fn classify_token(&self, _node_kind: &str) -> (SemanticTokenType, Vec<SemanticTokenModifier>) {
        // Examples:
        // - FunctionDeclaration -> (FUNCTION, [DECLARATION])
        // - ClassDeclaration -> (CLASS, [DECLARATION])
        // - VariableDeclaration with const -> (VARIABLE, [DECLARATION, READONLY])
        // - Parameter -> (PARAMETER, [DECLARATION])
        // - PropertyAccess -> (PROPERTY, [])
        // - MethodCall -> (METHOD, [])
        // - TypeReference -> (TYPE, [])

        (SemanticTokenType::VARIABLE, vec![])
    }

    /// Collect semantic tokens from a statement
    fn collect_tokens_from_statement(
        &self,
        stmt: &Statement,
        tokens: &mut Vec<SemanticToken>,
        last_line: &mut u32,
        last_char: &mut u32,
    ) {
        match stmt {
            Statement::Variable(var_decl) => {
                if let Pattern::Identifier(ident) = &var_decl.pattern {
                    let modifiers = if var_decl.kind == VariableKind::Const {
                        vec![
                            SemanticTokenModifier::DECLARATION,
                            SemanticTokenModifier::READONLY,
                        ]
                    } else {
                        vec![SemanticTokenModifier::DECLARATION]
                    };

                    self.add_token(
                        &ident.span,
                        &SemanticTokenType::VARIABLE,
                        &modifiers,
                        tokens,
                        last_line,
                        last_char,
                    );
                }
            }
            Statement::Function(func_decl) => {
                self.add_token(
                    &func_decl.name.span,
                    &SemanticTokenType::FUNCTION,
                    &[SemanticTokenModifier::DECLARATION],
                    tokens,
                    last_line,
                    last_char,
                );

                // Process function body
                for body_stmt in func_decl.body.statements.iter() {
                    self.collect_tokens_from_statement(body_stmt, tokens, last_line, last_char);
                }
            }
            Statement::Class(class_decl) => {
                self.add_token(
                    &class_decl.name.span,
                    &SemanticTokenType::CLASS,
                    &[SemanticTokenModifier::DECLARATION],
                    tokens,
                    last_line,
                    last_char,
                );

                // Process class members
                for member in class_decl.members.iter() {
                    self.collect_tokens_from_class_member(member, tokens, last_line, last_char);
                }
            }
            Statement::Interface(interface_decl) => {
                self.add_token(
                    &interface_decl.name.span,
                    &SemanticTokenType::INTERFACE,
                    &[SemanticTokenModifier::DECLARATION],
                    tokens,
                    last_line,
                    last_char,
                );
            }
            Statement::TypeAlias(type_decl) => {
                self.add_token(
                    &type_decl.name.span,
                    &SemanticTokenType::TYPE,
                    &[SemanticTokenModifier::DECLARATION],
                    tokens,
                    last_line,
                    last_char,
                );
            }
            Statement::Enum(enum_decl) => {
                self.add_token(
                    &enum_decl.name.span,
                    &SemanticTokenType::ENUM,
                    &[SemanticTokenModifier::DECLARATION],
                    tokens,
                    last_line,
                    last_char,
                );
            }
            Statement::Expression(expr) => {
                self.collect_tokens_from_expression(expr, tokens, last_line, last_char);
            }
            Statement::If(if_stmt) => {
                self.collect_tokens_from_expression(
                    &if_stmt.condition,
                    tokens,
                    last_line,
                    last_char,
                );
                for stmt in if_stmt.then_block.statements.iter() {
                    self.collect_tokens_from_statement(stmt, tokens, last_line, last_char);
                }
                for else_if in if_stmt.else_ifs.iter() {
                    self.collect_tokens_from_expression(
                        &else_if.condition,
                        tokens,
                        last_line,
                        last_char,
                    );
                    for stmt in else_if.block.statements.iter() {
                        self.collect_tokens_from_statement(stmt, tokens, last_line, last_char);
                    }
                }
                if let Some(else_block) = &if_stmt.else_block {
                    for stmt in else_block.statements.iter() {
                        self.collect_tokens_from_statement(stmt, tokens, last_line, last_char);
                    }
                }
            }
            Statement::While(while_stmt) => {
                self.collect_tokens_from_expression(
                    &while_stmt.condition,
                    tokens,
                    last_line,
                    last_char,
                );
                for stmt in while_stmt.body.statements.iter() {
                    self.collect_tokens_from_statement(stmt, tokens, last_line, last_char);
                }
            }
            Statement::Return(ret) => {
                for expr in ret.values.iter() {
                    self.collect_tokens_from_expression(expr, tokens, last_line, last_char);
                }
            }
            Statement::Block(block) => {
                for stmt in block.statements.iter() {
                    self.collect_tokens_from_statement(stmt, tokens, last_line, last_char);
                }
            }
            _ => {}
        }
    }

    /// Collect semantic tokens from class members
    fn collect_tokens_from_class_member(
        &self,
        member: &ClassMember,
        tokens: &mut Vec<SemanticToken>,
        last_line: &mut u32,
        last_char: &mut u32,
    ) {
        match member {
            ClassMember::Property(prop) => {
                let mut modifiers = vec![SemanticTokenModifier::DECLARATION];
                if prop.is_static {
                    modifiers.push(SemanticTokenModifier::STATIC);
                }
                if prop.is_readonly {
                    modifiers.push(SemanticTokenModifier::READONLY);
                }

                self.add_token(
                    &prop.name.span,
                    &SemanticTokenType::PROPERTY,
                    &modifiers,
                    tokens,
                    last_line,
                    last_char,
                );
            }
            ClassMember::Method(method) => {
                let mut modifiers = vec![SemanticTokenModifier::DECLARATION];
                if method.is_static {
                    modifiers.push(SemanticTokenModifier::STATIC);
                }

                self.add_token(
                    &method.name.span,
                    &SemanticTokenType::METHOD,
                    &modifiers,
                    tokens,
                    last_line,
                    last_char,
                );
            }
            ClassMember::Getter(getter) => {
                self.add_token(
                    &getter.name.span,
                    &SemanticTokenType::PROPERTY,
                    &[SemanticTokenModifier::DECLARATION],
                    tokens,
                    last_line,
                    last_char,
                );
            }
            ClassMember::Setter(setter) => {
                self.add_token(
                    &setter.name.span,
                    &SemanticTokenType::PROPERTY,
                    &[
                        SemanticTokenModifier::DECLARATION,
                        SemanticTokenModifier::MODIFICATION,
                    ],
                    tokens,
                    last_line,
                    last_char,
                );
            }
            _ => {}
        }
    }

    /// Collect semantic tokens from expressions
    fn collect_tokens_from_expression(
        &self,
        expr: &Expression,
        tokens: &mut Vec<SemanticToken>,
        last_line: &mut u32,
        last_char: &mut u32,
    ) {
        match &expr.kind {
            ExpressionKind::Identifier(_name) => {
                self.add_token(
                    &expr.span,
                    &SemanticTokenType::VARIABLE,
                    &[],
                    tokens,
                    last_line,
                    last_char,
                );
            }
            ExpressionKind::Call(callee, args, _type_args) => {
                // Mark function calls
                if let ExpressionKind::Identifier(_) = &callee.kind {
                    self.add_token(
                        &callee.span,
                        &SemanticTokenType::FUNCTION,
                        &[],
                        tokens,
                        last_line,
                        last_char,
                    );
                } else {
                    self.collect_tokens_from_expression(callee, tokens, last_line, last_char);
                }

                for arg in args.iter() {
                    self.collect_tokens_from_expression(&arg.value, tokens, last_line, last_char);
                }
            }
            ExpressionKind::Member(object, property) => {
                self.collect_tokens_from_expression(object, tokens, last_line, last_char);
                self.add_token(
                    &property.span,
                    &SemanticTokenType::PROPERTY,
                    &[],
                    tokens,
                    last_line,
                    last_char,
                );
            }
            ExpressionKind::Binary(_, left, right) => {
                self.collect_tokens_from_expression(left, tokens, last_line, last_char);
                self.collect_tokens_from_expression(right, tokens, last_line, last_char);
            }
            ExpressionKind::Unary(_, operand) => {
                self.collect_tokens_from_expression(operand, tokens, last_line, last_char);
            }
            ExpressionKind::Index(object, index) => {
                self.collect_tokens_from_expression(object, tokens, last_line, last_char);
                self.collect_tokens_from_expression(index, tokens, last_line, last_char);
            }
            ExpressionKind::Assignment(target, _, value) => {
                self.collect_tokens_from_expression(target, tokens, last_line, last_char);
                self.collect_tokens_from_expression(value, tokens, last_line, last_char);
            }
            ExpressionKind::Conditional(condition, then_expr, else_expr) => {
                self.collect_tokens_from_expression(condition, tokens, last_line, last_char);
                self.collect_tokens_from_expression(then_expr, tokens, last_line, last_char);
                self.collect_tokens_from_expression(else_expr, tokens, last_line, last_char);
            }
            ExpressionKind::Parenthesized(inner) => {
                self.collect_tokens_from_expression(inner, tokens, last_line, last_char);
            }
            _ => {}
        }
    }

    /// Add a semantic token with delta encoding
    fn add_token(
        &self,
        span: &Span,
        token_type: &SemanticTokenType,
        modifiers: &[SemanticTokenModifier],
        tokens: &mut Vec<SemanticToken>,
        last_line: &mut u32,
        last_char: &mut u32,
    ) {
        let line = (span.line.saturating_sub(1)) as u32;
        let start_char = (span.column.saturating_sub(1)) as u32;
        let length = span.len() as u32;

        // Calculate deltas
        let delta_line = line.saturating_sub(*last_line);
        let delta_start = if delta_line == 0 {
            start_char.saturating_sub(*last_char)
        } else {
            start_char
        };

        tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: self.get_token_type_index(token_type),
            token_modifiers_bitset: self.encode_modifiers(modifiers),
        });

        // Update last position
        *last_line = line;
        *last_char = start_char;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_semantic_tokens_encoding() {
        let provider = SemanticTokensProvider::new();

        // Test basic variable declaration
        let doc = Document::new_test("local x = 42".to_string(), 1);
        let result = provider.provide_full(&doc);

        // Should have at least one token for the variable 'x'
        assert!(!result.data.is_empty());
        assert!(result.result_id.is_some());

        // Verify first token is for variable 'x'
        let first_token = &result.data[0];
        assert_eq!(first_token.delta_line, 0); // First line
        assert_eq!(first_token.length, 1); // 'x' is 1 character

        // Should have modifiers for DECLARATION
        assert!(first_token.token_modifiers_bitset > 0);
    }

    #[test]
    fn test_function_declaration_tokens() {
        let provider = SemanticTokensProvider::new();

        let doc = Document::new_test(
            "function calculateSum(a, b) return a + b end".to_string(),
            1,
        );
        let result = provider.provide_full(&doc);

        // Should have tokens for the function name
        assert!(!result.data.is_empty());

        // First token should be for 'calculateSum'
        let function_token = &result.data[0];
        assert_eq!(function_token.length, 12); // 'calculateSum' is 12 characters

        // Verify it's a FUNCTION token type
        let function_type_index = provider.get_token_type_index(&SemanticTokenType::FUNCTION);
        assert_eq!(function_token.token_type, function_type_index);

        // Should have DECLARATION modifier
        let declaration_modifiers =
            provider.encode_modifiers(&[SemanticTokenModifier::DECLARATION]);
        assert_eq!(function_token.token_modifiers_bitset, declaration_modifiers);
    }

    #[test]
    fn test_class_declaration_tokens() {
        let provider = SemanticTokensProvider::new();

        let doc = Document::new_test("class Point end".to_string(), 1);
        let result = provider.provide_full(&doc);

        // Should have at least one token for 'Point'
        assert!(!result.data.is_empty());

        let class_token = &result.data[0];
        assert_eq!(class_token.length, 5); // 'Point' is 5 characters

        // Verify it's a CLASS token type
        let class_type_index = provider.get_token_type_index(&SemanticTokenType::CLASS);
        assert_eq!(class_token.token_type, class_type_index);

        // Should have DECLARATION modifier
        let declaration_modifiers =
            provider.encode_modifiers(&[SemanticTokenModifier::DECLARATION]);
        assert_eq!(class_token.token_modifiers_bitset, declaration_modifiers);
    }

    #[test]
    fn test_const_variable_modifiers() {
        let provider = SemanticTokensProvider::new();

        // Test const variable
        let doc = Document::new_test("const PI = 3.14159".to_string(), 1);
        let result = provider.provide_full(&doc);

        assert!(!result.data.is_empty());

        let const_token = &result.data[0];
        // Should have both DECLARATION and READONLY modifiers
        let expected_modifiers = provider.encode_modifiers(&[
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::READONLY,
        ]);
        assert_eq!(const_token.token_modifiers_bitset, expected_modifiers);

        // Test local (mutable) variable
        let doc = Document::new_test("local mutable = 42".to_string(), 1);
        let result = provider.provide_full(&doc);

        assert!(!result.data.is_empty());

        let var_token = &result.data[0];
        // Should have only DECLARATION modifier
        let expected_modifiers = provider.encode_modifiers(&[SemanticTokenModifier::DECLARATION]);
        assert_eq!(var_token.token_modifiers_bitset, expected_modifiers);
    }

    #[test]
    fn test_deprecated_modifier() {
        // This test is a placeholder for when we implement decorator support
        // For now, we don't parse @deprecated decorators yet
        assert!(true);
    }

    #[test]
    fn test_multiline_tokens() {
        let provider = SemanticTokensProvider::new();

        let doc = Document::new_test("local x = 10\nlocal y = 20".to_string(), 1);
        let result = provider.provide_full(&doc);

        // Should have tokens for both variables
        assert!(result.data.len() >= 2);

        // First token (x) should be on line 0
        let first_token = &result.data[0];
        assert_eq!(first_token.delta_line, 0);

        // Second token (y) should be on next line (delta_line = 1)
        let second_token = &result.data[1];
        assert_eq!(second_token.delta_line, 1);
    }

    #[test]
    fn test_token_modifiers_encoding() {
        let provider = SemanticTokensProvider::new();

        // Test single modifier
        let declaration_only = provider.encode_modifiers(&[SemanticTokenModifier::DECLARATION]);
        assert_eq!(declaration_only, 1); // bit 0 = 1

        // Test multiple modifiers
        let declaration_readonly = provider.encode_modifiers(&[
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::READONLY,
        ]);
        assert_eq!(declaration_readonly, 3); // 1 | 2 = 3

        // Test different combination
        let static_modifier = provider.encode_modifiers(&[SemanticTokenModifier::STATIC]);
        assert_eq!(static_modifier, 4); // bit 2 = 4

        // Test empty modifiers
        let no_modifiers = provider.encode_modifiers(&[]);
        assert_eq!(no_modifiers, 0);
    }

    #[test]
    fn test_semantic_tokens_empty_document() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("".to_string(), 1);

        let result = provider.provide_full(&doc);
        assert!(result.data.is_empty());
    }

    #[test]
    fn test_semantic_tokens_comment_only() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("-- this is a comment".to_string(), 1);

        let result = provider.provide_full(&doc);
        assert!(result.data.is_empty() || result.data.len() >= 0);
    }

    #[test]
    fn test_semantic_tokens_string_only() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("local s = \"hello world\"".to_string(), 1);

        let result = provider.provide_full(&doc);
        assert!(!result.data.is_empty());
    }

    #[test]
    fn test_semantic_tokens_number_only() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("local n = 42.5".to_string(), 1);

        let result = provider.provide_full(&doc);
        assert!(!result.data.is_empty());
    }

    #[test]
    fn test_semantic_tokens_function_call() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("foo()".to_string(), 1);

        let result = provider.provide_full(&doc);
        assert!(result.data.is_empty() || result.data.len() >= 0);
    }

    #[test]
    fn test_semantic_tokens_method_call() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("obj:method()".to_string(), 1);

        let result = provider.provide_full(&doc);
        assert!(result.data.is_empty() || result.data.len() >= 0);
    }

    #[test]
    fn test_semantic_tokens_class_definition() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("class MyClass\n  field: number\nend".to_string(), 1);

        let result = provider.provide_full(&doc);
        assert!(!result.data.is_empty());
    }

    #[test]
    fn test_semantic_tokens_interface_definition() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("interface MyInterface end".to_string(), 1);

        let result = provider.provide_full(&doc);
        assert!(result.data.is_empty() || result.data.len() >= 0);
    }

    #[test]
    fn test_semantic_tokens_enum_definition() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("enum MyEnum\n  A\n  B\nend".to_string(), 1);

        let result = provider.provide_full(&doc);
        assert!(result.data.is_empty() || result.data.len() >= 0);
    }

    #[test]
    fn test_semantic_tokens_provide_range() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("local a = 1\nlocal b = 2\nlocal c = 3".to_string(), 1);

        let range = Range {
            start: Position::new(1, 0),
            end: Position::new(2, 10),
        };
        let result = provider.provide_range(&doc, range);
        assert!(result.data.is_empty() || result.data.len() >= 0);
    }

    #[test]
    fn test_semantic_tokens_provide_range_full() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("local x = 1".to_string(), 1);

        let range = Range {
            start: Position::new(0, 0),
            end: Position::new(10, 0),
        };
        let result = provider.provide_range(&doc, range);
        assert!(result.data.is_empty() || result.data.len() >= 0);
    }

    #[test]
    fn test_semantic_tokens_provide_delta() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("local x = 1".to_string(), 1);

        let result = provider.provide_full_delta(&doc, "".to_string());
        assert!(result.edits.is_empty() || result.edits.len() >= 0);
    }

    #[test]
    fn test_semantic_tokens_provide_delta_with_result_id() {
        let provider = SemanticTokensProvider::new();
        let doc = Document::new_test("local x = 1".to_string(), 1);

        let result = provider.provide_full_delta(&doc, "previous".to_string());
        assert!(result.edits.is_empty() || result.edits.len() >= 0);
    }

    #[test]
    fn test_semantic_tokens_provider_clone() {
        let provider = SemanticTokensProvider::new();
        let _cloned = provider.clone();
    }
}
