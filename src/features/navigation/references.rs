use crate::core::document::{Document, DocumentManager};
use crate::traits::ReferencesProviderTrait;
use lsp_types::{Uri, *};

use std::str::FromStr;
use std::sync::Arc;
use typedlua_parser::ast::statement::{Block, Statement};
use typedlua_parser::ast::{
    expression::{Argument, Expression, ExpressionKind, ObjectProperty},
    pattern::Pattern,
};
use typedlua_parser::string_interner::StringInterner;
use typedlua_parser::{Lexer, Parser, Span};
use typedlua_typechecker::cli::diagnostics::CollectingDiagnosticHandler;

/// Provides find-references functionality
#[derive(Clone)]
pub struct ReferencesProvider;

impl ReferencesProvider {
    pub fn new() -> Self {
        Self
    }

    /// Find all references to the symbol at the given position (internal method)
    pub fn provide_impl(
        &self,
        uri: &Uri,
        document: &Document,
        position: Position,
        include_declaration: bool,
        _document_manager: Option<&DocumentManager>,
    ) -> Option<Vec<Location>> {
        let word = self.get_word_at_position(document, position)?;

        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids);
        let ast = parser.parse().ok()?;

        let mut references = Vec::new();
        self.find_references_in_statements(&ast.statements, &word, &mut references, &interner);

        if include_declaration {
            if let Some(decl_span) = self.find_declaration(&ast.statements, &word, &interner) {
                references.insert(
                    0,
                    Location {
                        uri: uri.clone(),
                        range: span_to_range(&decl_span),
                    },
                );
            }
        }

        Some(references)
    }

    fn find_references_in_statements(
        &self,
        statements: &[Statement],
        word: &str,
        references: &mut Vec<Location>,
        interner: &StringInterner,
    ) {
        for stmt in statements {
            self.find_references_in_statement(stmt, word, references, interner);
        }
    }

    fn find_references_in_statement(
        &self,
        stmt: &Statement,
        word: &str,
        references: &mut Vec<Location>,
        interner: &StringInterner,
    ) {
        match stmt {
            Statement::Variable(var_decl) => {
                if let Pattern::Identifier(ident) = &var_decl.pattern {
                    if interner.resolve(ident.node) == word {
                        references.push(Location {
                            uri: Uri::from_str("file://current").unwrap(),
                            range: span_to_range(&ident.span),
                        });
                    }
                }
            }
            Statement::Function(func_decl) => {
                if interner.resolve(func_decl.name.node) == word {
                    references.push(Location {
                        uri: Uri::from_str("file://current").unwrap(),
                        range: span_to_range(&func_decl.name.span),
                    });
                }
                self.find_references_in_function(func_decl, word, references, interner);
            }
            Statement::Expression(expr) => {
                self.find_references_in_expression(expr, word, references, interner);
            }
            _ => {}
        }
    }

    fn find_references_in_function(
        &self,
        func: &typedlua_parser::ast::statement::FunctionDeclaration,
        word: &str,
        references: &mut Vec<Location>,
        interner: &StringInterner,
    ) {
        for param in &func.parameters {
            if let Pattern::Identifier(ident) = &param.pattern {
                if interner.resolve(ident.node) == word {
                    references.push(Location {
                        uri: Uri::from_str("file://current").unwrap(),
                        range: span_to_range(&ident.span),
                    });
                }
            }
        }
        self.find_references_in_block(&func.body, word, references, interner);
    }

    fn find_references_in_block(
        &self,
        block: &Block,
        word: &str,
        references: &mut Vec<Location>,
        interner: &StringInterner,
    ) {
        for stmt in &block.statements {
            self.find_references_in_statement(stmt, word, references, interner);
        }
    }

    fn find_references_in_arguments(
        &self,
        arguments: &[Argument],
        word: &str,
        references: &mut Vec<Location>,
        interner: &StringInterner,
    ) {
        for arg in arguments {
            self.find_references_in_expression(&arg.value, word, references, interner);
        }
    }

    fn find_references_in_expression(
        &self,
        expr: &Expression,
        word: &str,
        references: &mut Vec<Location>,
        interner: &StringInterner,
    ) {
        match &expr.kind {
            ExpressionKind::Identifier(ident) => {
                if interner.resolve(*ident) == word {
                    references.push(Location {
                        uri: Uri::from_str("file://current").unwrap(),
                        range: span_to_range(&expr.span),
                    });
                }
            }
            ExpressionKind::Call(callee, args, _types) => {
                self.find_references_in_expression(callee, word, references, interner);
                self.find_references_in_arguments(args, word, references, interner);
            }
            ExpressionKind::MethodCall(expr, _name, args, _types) => {
                self.find_references_in_expression(expr, word, references, interner);
                self.find_references_in_arguments(args, word, references, interner);
            }
            ExpressionKind::Object(properties) => {
                for prop in properties {
                    match prop {
                        ObjectProperty::Property { key, value, .. } => {
                            if interner.resolve(key.node) == word {
                                references.push(Location {
                                    uri: Uri::from_str("file://current").unwrap(),
                                    range: span_to_range(&key.span),
                                });
                            }
                            self.find_references_in_expression(value, word, references, interner);
                        }
                        ObjectProperty::Computed { key, value, .. } => {
                            self.find_references_in_expression(key, word, references, interner);
                            self.find_references_in_expression(value, word, references, interner);
                        }
                        ObjectProperty::Spread { value, .. } => {
                            self.find_references_in_expression(value, word, references, interner);
                        }
                    }
                }
            }
            ExpressionKind::Binary(_binop, left, right) => {
                self.find_references_in_expression(left, word, references, interner);
                self.find_references_in_expression(right, word, references, interner);
            }
            ExpressionKind::Unary(_unop, operand) => {
                self.find_references_in_expression(operand, word, references, interner);
            }
            _ => {}
        }
    }

    fn get_word_at_position(&self, document: &Document, position: Position) -> Option<String> {
        let lines: Vec<&str> = document.text.lines().collect();
        if position.line as usize >= lines.len() {
            return None;
        }

        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        if char_pos > line.len() {
            return None;
        }

        let chars: Vec<char> = line.chars().collect();
        if char_pos >= chars.len() {
            return None;
        }

        if !chars[char_pos].is_alphanumeric() && chars[char_pos] != '_' {
            return None;
        }

        let mut start = char_pos;
        while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
            start -= 1;
        }

        let mut end = char_pos;
        while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
            end += 1;
        }

        Some(chars[start..end].iter().collect())
    }

    fn find_declaration(
        &self,
        statements: &[Statement],
        name: &str,
        interner: &StringInterner,
    ) -> Option<Span> {
        for stmt in statements {
            match stmt {
                Statement::Variable(var_decl) => {
                    if let Pattern::Identifier(ident) = &var_decl.pattern {
                        if interner.resolve(ident.node) == name {
                            return Some(ident.span);
                        }
                    }
                }
                Statement::Function(func_decl) => {
                    if interner.resolve(func_decl.name.node) == name {
                        return Some(func_decl.name.span);
                    }
                }
                _ => {}
            }
        }
        None
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
            line: (span.line.saturating_sub(1)) as u32,
            character: ((span.column + span.len()).saturating_sub(1)) as u32,
        },
    }
}

impl ReferencesProviderTrait for ReferencesProvider {
    fn provide(
        &self,
        uri: &Uri,
        document: &Document,
        position: Position,
        include_declaration: bool,
    ) -> Vec<Location> {
        let _document_manager = None;
        self.provide_impl(
            uri,
            document,
            position,
            include_declaration,
            _document_manager,
        )
        .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::document::Document;
    use std::str::FromStr;

    fn create_test_document(text: &str) -> Document {
        Document::new_test(text.to_string(), 1)
    }

    #[test]
    fn test_find_variable_reference() {
        let doc = create_test_document("local x = 1\nlocal y = x + 1");
        let provider = ReferencesProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let references = provider.provide(&uri, &doc, Position::new(1, 11), true);

        assert!(!references.is_empty());
    }

    #[test]
    fn test_find_function_reference() {
        let doc = create_test_document("function foo() end\nfoo()");
        let provider = ReferencesProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let references = provider.provide(&uri, &doc, Position::new(1, 0), true);

        assert!(!references.is_empty());
    }

    #[test]
    fn test_no_references_for_unknown_symbol() {
        let doc = create_test_document("local x = 1");
        let provider = ReferencesProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let references = provider.provide(&uri, &doc, Position::new(0, 0), true);

        assert!(references.is_empty());
    }

    #[test]
    fn test_span_to_range() {
        let span = Span {
            start: 10,
            end: 13,
            line: 2,
            column: 5,
        };
        let range = span_to_range(&span);
        assert_eq!(range.start.line, 1);
        assert_eq!(range.start.character, 4);
        assert_eq!(range.end.line, 1);
        assert_eq!(range.end.character, 7);
    }

    #[test]
    fn test_empty_document() {
        let doc = create_test_document("");
        let provider = ReferencesProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let references = provider.provide(&uri, &doc, Position::new(0, 0), true);

        assert!(references.is_empty());
    }

    #[test]
    fn test_out_of_bounds_position() {
        let doc = create_test_document("local x = 1");
        let provider = ReferencesProvider::new();
        let uri = Uri::from_str("file://test.lua").unwrap();

        let references = provider.provide(&uri, &doc, Position::new(10, 10), true);

        assert!(references.is_empty());
    }
}
