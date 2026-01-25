use crate::document::Document;
use lsp_types::*;

use std::collections::HashMap;
use std::sync::Arc;
use typedlua_core::ast::pattern::Pattern;
use typedlua_core::ast::statement::Statement;
use typedlua_core::diagnostics::CollectingDiagnosticHandler;
use typedlua_core::string_interner::StringInterner;
use typedlua_core::{Lexer, Parser};

/// Provides code actions (quick fixes, refactorings, source actions)
pub struct CodeActionsProvider;

impl CodeActionsProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide code actions for a given range in the document
    pub fn provide(
        &self,
        uri: &Uri,
        document: &Document,
        range: Range,
        context: CodeActionContext,
    ) -> Vec<CodeActionOrCommand> {
        let mut actions = Vec::new();

        // Parse the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(_) => return actions,
        };

        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common_ids);
        let ast = match parser.parse() {
            Ok(a) => a,
            Err(_) => return actions,
        };

        // Get diagnostics from context
        for diagnostic in &context.diagnostics {
            // Quick fix: Add type annotation for variables without types
            if diagnostic.message.contains("type annotation") {
                if let Some(action) = self.quick_fix_add_type_annotation(uri, diagnostic) {
                    actions.push(CodeActionOrCommand::CodeAction(action));
                }
            }

            // Quick fix: Remove unused variable
            if diagnostic.message.contains("unused") {
                if let Some(action) = self.quick_fix_unused_variable(uri, diagnostic) {
                    actions.push(CodeActionOrCommand::CodeAction(action));
                }
            }
        }

        // Refactoring: Extract variable (if there's a selection)
        if range.start != range.end {
            if let Some(action) = self.refactor_extract_variable(uri, document, range, &ast) {
                actions.push(CodeActionOrCommand::CodeAction(action));
            }
        }

        // Source action: Add missing type annotations
        if let Some(action) = self.source_action_add_type_annotations(uri, &ast) {
            actions.push(CodeActionOrCommand::CodeAction(action));
        }

        actions
    }

    /// Resolve additional details for a code action
    pub fn resolve(&self, action: CodeAction) -> CodeAction {
        // For now, return the action as-is
        // In the future, we could lazy-load expensive edits here
        action
    }

    // Quick fix implementations

    /// Generate a quick fix to add type annotation
    fn quick_fix_add_type_annotation(
        &self,
        uri: &Uri,
        diagnostic: &Diagnostic,
    ) -> Option<CodeAction> {
        // For now, return a simple code action that inserts ": unknown"
        // In the future, we could infer the actual type from the type checker
        let insert_pos = diagnostic.range.end;

        let mut changes = HashMap::new();
        changes.insert(
            uri.clone(),
            vec![TextEdit {
                range: Range {
                    start: insert_pos,
                    end: insert_pos,
                },
                new_text: ": unknown".to_string(),
            }],
        );

        Some(CodeAction {
            title: "Add type annotation".to_string(),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic.clone()]),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }),
            command: None,
            is_preferred: Some(true),
            disabled: None,
            data: None,
        })
    }

    /// Generate a quick fix for unused variable
    fn quick_fix_unused_variable(&self, uri: &Uri, diagnostic: &Diagnostic) -> Option<CodeAction> {
        // Prefix the variable name with underscore
        let mut changes = HashMap::new();
        changes.insert(
            uri.clone(),
            vec![TextEdit {
                range: Range {
                    start: diagnostic.range.start,
                    end: diagnostic.range.start,
                },
                new_text: "_".to_string(),
            }],
        );

        Some(CodeAction {
            title: "Prefix with underscore".to_string(),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic.clone()]),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }),
            command: None,
            is_preferred: Some(false),
            disabled: None,
            data: None,
        })
    }

    // Refactoring implementations

    /// Generate a refactoring to extract a variable
    fn refactor_extract_variable(
        &self,
        uri: &Uri,
        document: &Document,
        range: Range,
        _ast: &typedlua_core::ast::Program,
    ) -> Option<CodeAction> {
        // Get the selected text
        let selected_text = self.get_text_in_range(document, range)?;

        // Check if the selection is a valid expression
        if selected_text.trim().is_empty() {
            return None;
        }

        // Generate the variable name
        let var_name = "extracted";

        // Create the extraction edits
        let mut changes = HashMap::new();

        // Insert variable declaration before the statement
        let insert_line = range.start.line;
        let indent = self.get_line_indent(document, insert_line);

        let edits = vec![
            // Insert the variable declaration
            TextEdit {
                range: Range {
                    start: Position {
                        line: insert_line,
                        character: 0,
                    },
                    end: Position {
                        line: insert_line,
                        character: 0,
                    },
                },
                new_text: format!(
                    "{}local {}: unknown = {}\n",
                    indent,
                    var_name,
                    selected_text.trim()
                ),
            },
            // Replace the selection with the variable name
            TextEdit {
                range,
                new_text: var_name.to_string(),
            },
        ];

        changes.insert(uri.clone(), edits);

        Some(CodeAction {
            title: "Extract to variable".to_string(),
            kind: Some(CodeActionKind::REFACTOR_EXTRACT),
            diagnostics: None,
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }),
            command: None,
            is_preferred: Some(false),
            disabled: None,
            data: None,
        })
    }

    // Source action implementations

    /// Generate a source action to add missing type annotations
    fn source_action_add_type_annotations(
        &self,
        uri: &Uri,
        ast: &typedlua_core::ast::Program,
    ) -> Option<CodeAction> {
        let mut edits = Vec::new();

        // Find all variables without type annotations
        for stmt in &ast.statements {
            self.collect_missing_type_annotations(stmt, &mut edits);
        }

        if edits.is_empty() {
            return None;
        }

        let mut changes = HashMap::new();
        changes.insert(uri.clone(), edits);

        Some(CodeAction {
            title: "Add type annotations to all variables".to_string(),
            kind: Some(CodeActionKind::SOURCE),
            diagnostics: None,
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }),
            command: None,
            is_preferred: Some(false),
            disabled: None,
            data: None,
        })
    }

    // Helper methods

    /// Get text within a range
    fn get_text_in_range(&self, document: &Document, range: Range) -> Option<String> {
        let lines: Vec<&str> = document.text.lines().collect();

        if range.start.line == range.end.line {
            // Single line selection
            let line_idx = range.start.line as usize;
            if line_idx >= lines.len() {
                return None;
            }

            let line = lines[line_idx];
            let start = range.start.character as usize;
            let end = range.end.character as usize;

            if start >= line.len() || end > line.len() {
                return None;
            }

            Some(line[start..end].to_string())
        } else {
            // Multi-line selection
            let mut result = String::new();

            for line_idx in range.start.line..=range.end.line {
                let line_idx = line_idx as usize;
                if line_idx >= lines.len() {
                    break;
                }

                let line = lines[line_idx];

                if line_idx == range.start.line as usize {
                    let start = range.start.character as usize;
                    if start < line.len() {
                        result.push_str(&line[start..]);
                    }
                } else if line_idx == range.end.line as usize {
                    let end = range.end.character as usize;
                    if end <= line.len() {
                        result.push_str(&line[..end]);
                    }
                } else {
                    result.push_str(line);
                }

                if line_idx < range.end.line as usize {
                    result.push('\n');
                }
            }

            Some(result)
        }
    }

    /// Get indentation of a line
    fn get_line_indent(&self, document: &Document, line: u32) -> String {
        let lines: Vec<&str> = document.text.lines().collect();
        let line_idx = line as usize;

        if line_idx >= lines.len() {
            return String::new();
        }

        let line = lines[line_idx];
        line.chars().take_while(|c| c.is_whitespace()).collect()
    }

    /// Find statement containing a range
    /// Collect missing type annotations from statements
    fn collect_missing_type_annotations(&self, stmt: &Statement, edits: &mut Vec<TextEdit>) {
        match stmt {
            Statement::Variable(var_decl) => {
                if var_decl.type_annotation.is_none() {
                    if let Pattern::Identifier(ident) = &var_decl.pattern {
                        // Add type annotation after the identifier
                        let position = Position {
                            line: (ident.span.line.saturating_sub(1)) as u32,
                            character: ((ident.span.column + ident.span.len()).saturating_sub(1))
                                as u32,
                        };

                        edits.push(TextEdit {
                            range: Range {
                                start: position,
                                end: position,
                            },
                            new_text: ": unknown".to_string(),
                        });
                    }
                }
            }
            Statement::Function(func_decl) => {
                // Recursively check function body
                for body_stmt in &func_decl.body.statements {
                    self.collect_missing_type_annotations(body_stmt, edits);
                }
            }
            Statement::If(if_stmt) => {
                for then_stmt in &if_stmt.then_block.statements {
                    self.collect_missing_type_annotations(then_stmt, edits);
                }
                for else_if in &if_stmt.else_ifs {
                    for stmt in &else_if.block.statements {
                        self.collect_missing_type_annotations(stmt, edits);
                    }
                }
                if let Some(else_block) = &if_stmt.else_block {
                    for stmt in &else_block.statements {
                        self.collect_missing_type_annotations(stmt, edits);
                    }
                }
            }
            Statement::While(while_stmt) => {
                for body_stmt in &while_stmt.body.statements {
                    self.collect_missing_type_annotations(body_stmt, edits);
                }
            }
            Statement::Block(block) => {
                for body_stmt in &block.statements {
                    self.collect_missing_type_annotations(body_stmt, edits);
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code_action_kinds() {
        let provider = CodeActionsProvider::new();

        // Test that we can create different kinds of code actions
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

        let context = CodeActionContext {
            diagnostics: vec![],
            only: None,
            trigger_kind: None,
        };

        let uri = "file:///test.lua".parse::<Uri>().unwrap();
        let _actions = provider.provide(&uri, &doc, range, context);

        // Should provide at least source actions
    }

    #[test]
    fn test_quick_fix_scenarios() {
        let provider = CodeActionsProvider::new();

        let doc = Document::new_test("local unused_var = 10".to_string(), 1);

        let range = Range {
            start: Position {
                line: 0,
                character: 6,
            },
            end: Position {
                line: 0,
                character: 16,
            },
        };

        // Create a diagnostic for unused variable
        let diagnostic = Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::WARNING),
            code: None,
            code_description: None,
            source: Some("typedlua".to_string()),
            message: "unused variable".to_string(),
            related_information: None,
            tags: None,
            data: None,
        };

        let context = CodeActionContext {
            diagnostics: vec![diagnostic],
            only: None,
            trigger_kind: None,
        };

        let uri = "file:///test.lua".parse::<Uri>().unwrap();
        let actions = provider.provide(&uri, &doc, range, context);

        // Should provide quick fix for unused variable
        assert!(actions.len() > 0);

        // Verify at least one action is a quick fix
        let has_quickfix = actions.iter().any(|action| {
            if let CodeActionOrCommand::CodeAction(a) = action {
                a.kind == Some(CodeActionKind::QUICKFIX)
            } else {
                false
            }
        });
        assert!(has_quickfix);
    }

    #[test]
    fn test_refactoring_scenarios() {
        let provider = CodeActionsProvider::new();

        let doc = Document::new_test("local x = 10 + 20".to_string(), 1);

        // Select the expression "10 + 20"
        let range = Range {
            start: Position {
                line: 0,
                character: 10,
            },
            end: Position {
                line: 0,
                character: 17,
            },
        };

        let context = CodeActionContext {
            diagnostics: vec![],
            only: None,
            trigger_kind: None,
        };

        let uri = "file:///test.lua".parse::<Uri>().unwrap();
        let actions = provider.provide(&uri, &doc, range, context);

        // Should provide extract variable refactoring
        let has_extract = actions.iter().any(|action| {
            if let CodeActionOrCommand::CodeAction(a) = action {
                a.kind == Some(CodeActionKind::REFACTOR_EXTRACT)
            } else {
                false
            }
        });
        assert!(has_extract);
    }
}
