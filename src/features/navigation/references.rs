use crate::core::document::{Document, DocumentManager};
use crate::traits::ReferencesProviderTrait;
use lsp_types::{Uri, *};

use std::sync::Arc;
use typedlua_parser::ast::{
    expression::{Expression, ExpressionKind},
    statement::Statement,
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
        document_manager: &DocumentManager,
    ) -> Option<Vec<Location>> {
        // Get the word at the current position
        let word = self.get_word_at_position(document, position)?;

        // Parse the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids);
        let ast = parser.parse().ok()?;

        // Find all references in the current file
        let mut references = Vec::new();
        self.find_references_in_statements(&ast.statements, &word, &mut references, &interner);

        // Optionally include the declaration
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
        use typedlua_parser::ast::pattern::Pattern;

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
            Statement::LocalAssignment(assignment) => {
                for pattern in &assignment.patterns {
                    if let Pattern::Identifier(ident) = pattern {
                        if interner.resolve(ident.node) == word {
                            references.push(Location {
                                uri: Uri::from_str("file://current").unwrap(),
                                range: span_to_range(&ident.span),
                            });
                        }
                    }
                }
                self.find_references_in_expressions(
                    &assignment.expressions,
                    word,
                    references,
                    interner,
                );
            }
            Statement::Call(call) => {
                self.find_references_in_expression(call, word, references, interner);
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
        for param in &func.signature.params {
            if let Pattern::Identifier(ident) = param {
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
        block: &typedlua_parser::ast::Block,
        word: &str,
        references: &mut Vec<Location>,
        interner: &StringInterner,
    ) {
        for stmt in &block.statements {
            self.find_references_in_statement(stmt, word, references, interner);
        }
    }

    fn find_references_in_expressions(
        &self,
        expressions: &[Expression],
        word: &str,
        references: &mut Vec<Location>,
        interner: &StringInterner,
    ) {
        for expr in expressions {
            self.find_references_in_expression(expr, word, references, interner);
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
                if interner.resolve(ident.node) == word {
                    references.push(Location {
                        uri: Uri::from_str("file://current").unwrap(),
                        range: span_to_range(&ident.span),
                    });
                }
            }
            ExpressionKind::FunctionCall(call) => {
                self.find_references_in_expression(&call.expr, word, references, interner);
                self.find_references_in_expressions(&call.args, word, references, interner);
            }
            ExpressionKind::MethodCall(call) => {
                self.find_references_in_expression(&call.expr, word, references, interner);
                self.find_references_in_expressions(&call.args, word, references, interner);
            }
            ExpressionKind::TableConstructor(table) => {
                for field in &table.fields {
                    match field {
                        typedlua_parser::ast::expression::TableField::ExpressionKey(key, value) => {
                            self.find_references_in_expression(key, word, references, interner);
                            self.find_references_in_expression(value, word, references, interner);
                        }
                        typedlua_parser::ast::expression::TableField::IdentifierKey(key, value) => {
                            if interner.resolve(key.node) == word {
                                references.push(Location {
                                    uri: Uri::from_str("file://current").unwrap(),
                                    range: span_to_range(&key.span),
                                });
                            }
                            self.find_references_in_expression(value, word, references, interner);
                        }
                    }
                }
            }
            ExpressionKind::BinaryOp(binop) => {
                self.find_references_in_expression(&binop.left, word, references, interner);
                self.find_references_in_expression(&binop.right, word, references, interner);
            }
            ExpressionKind::UnaryOp(unop) => {
                self.find_references_in_expression(&unop.expr, word, references, interner);
            }
            _ => {}
        }
    }

    /// Get the word at the cursor position
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

    /// Find the declaration span for a given symbol name
    fn find_declaration(
        &self,
        statements: &[Statement],
        name: &str,
        interner: &StringInterner,
    ) -> Option<Span> {
        use typedlua_parser::ast::pattern::Pattern;

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
                Statement::LocalAssignment(assignment) => {
                    for pattern in &assignment.patterns {
                        if let Pattern::Identifier(ident) = pattern {
                            if interner.resolve(ident.node) == name {
                                return Some(ident.span);
                            }
                        }
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
        self.provide_impl(
            uri,
            document,
            position,
            include_declaration,
            &DocumentManager::new_test(),
        )
        .unwrap_or_default()
    }
}
