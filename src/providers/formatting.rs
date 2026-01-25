use crate::document::Document;
use lsp_types::*;

use std::sync::Arc;
use typedlua_core::diagnostics::CollectingDiagnosticHandler;
use typedlua_core::string_interner::StringInterner;
use typedlua_core::{Lexer, Parser};

/// Provides code formatting functionality
pub struct FormattingProvider;

impl FormattingProvider {
    pub fn new() -> Self {
        Self
    }

    /// Format the entire document
    pub fn format_document(
        &self,
        document: &Document,
        options: FormattingOptions,
    ) -> Vec<TextEdit> {
        // Parse the document to ensure it's valid
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(_) => return Vec::new(), // Don't format invalid code
        };

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids);
        let _ast = match parser.parse() {
            Ok(a) => a,
            Err(_) => return Vec::new(), // Don't format invalid code
        };

        // Apply basic formatting fixes
        let mut edits = Vec::new();

        // Fix indentation and trailing whitespace
        self.fix_indentation_and_whitespace(document, options, &mut edits);

        edits
    }

    /// Format a specific range in the document
    pub fn format_range(
        &self,
        document: &Document,
        range: Range,
        options: FormattingOptions,
    ) -> Vec<TextEdit> {
        // Parse the document to ensure it's valid
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(_) => return Vec::new(),
        };

        let mut parser = Parser::new(tokens, handler, &interner, &common_ids);
        let _ast = match parser.parse() {
            Ok(a) => a,
            Err(_) => return Vec::new(),
        };

        // Apply formatting only within the specified range
        let mut edits = Vec::new();
        self.fix_indentation_in_range(document, range, options, &mut edits);

        edits
    }

    /// Format text as the user types (on-type formatting)
    pub fn format_on_type(
        &self,
        document: &Document,
        position: Position,
        ch: &str,
        options: FormattingOptions,
    ) -> Vec<TextEdit> {
        let mut edits = Vec::new();

        // When user types 'end', auto-indent the line
        if ch == "d" {
            let lines: Vec<&str> = document.text.lines().collect();
            if position.line as usize >= lines.len() {
                return edits;
            }

            let line = lines[position.line as usize];
            let trimmed = line.trim();

            if trimmed == "end" {
                // Calculate correct indentation for 'end'
                if let Some(indent_level) =
                    self.calculate_end_indent(document, position.line as usize)
                {
                    let indent = self.make_indent(indent_level, &options);

                    edits.push(TextEdit {
                        range: Range {
                            start: Position {
                                line: position.line,
                                character: 0,
                            },
                            end: Position {
                                line: position.line,
                                character: line.len() as u32,
                            },
                        },
                        new_text: format!("{}end", indent),
                    });
                }
            }
        }

        edits
    }

    // Helper methods

    /// Fix indentation and trailing whitespace
    fn fix_indentation_and_whitespace(
        &self,
        document: &Document,
        options: FormattingOptions,
        edits: &mut Vec<TextEdit>,
    ) {
        let lines: Vec<&str> = document.text.lines().collect();
        let mut indent_level: usize = 0;

        for (line_num, line) in lines.iter().enumerate() {
            let trimmed = line.trim();

            // Skip empty lines
            if trimmed.is_empty() {
                // Remove trailing whitespace from empty lines
                if !line.is_empty() {
                    edits.push(TextEdit {
                        range: Range {
                            start: Position {
                                line: line_num as u32,
                                character: 0,
                            },
                            end: Position {
                                line: line_num as u32,
                                character: line.len() as u32,
                            },
                        },
                        new_text: String::new(),
                    });
                }
                continue;
            }

            // Decrease indent for closing keywords
            if trimmed.starts_with("end")
                || trimmed.starts_with("else")
                || trimmed.starts_with("elseif")
            {
                indent_level = indent_level.saturating_sub(1);
            }

            // Calculate expected indentation
            let expected_indent = self.make_indent(indent_level, &options);
            let current_indent: String = line.chars().take_while(|c| c.is_whitespace()).collect();

            // Only add edit if indentation is wrong
            if expected_indent != current_indent {
                edits.push(TextEdit {
                    range: Range {
                        start: Position {
                            line: line_num as u32,
                            character: 0,
                        },
                        end: Position {
                            line: line_num as u32,
                            character: current_indent.len() as u32,
                        },
                    },
                    new_text: expected_indent.clone(),
                });
            }

            // Remove trailing whitespace
            if let Some(trailing_ws_start) = line.rfind(|c: char| !c.is_whitespace()) {
                let trailing_ws_start = trailing_ws_start + 1;
                if trailing_ws_start < line.len() {
                    edits.push(TextEdit {
                        range: Range {
                            start: Position {
                                line: line_num as u32,
                                character: trailing_ws_start as u32,
                            },
                            end: Position {
                                line: line_num as u32,
                                character: line.len() as u32,
                            },
                        },
                        new_text: String::new(),
                    });
                }
            }

            // Increase indent for opening keywords
            if trimmed.starts_with("function")
                || trimmed.starts_with("if")
                || trimmed.starts_with("while")
                || trimmed.starts_with("for")
                || trimmed.starts_with("class")
                || trimmed.starts_with("interface")
                || (trimmed.starts_with("else") && !trimmed.starts_with("elseif"))
            {
                indent_level += 1;
            }
        }
    }

    /// Fix indentation within a specific range
    fn fix_indentation_in_range(
        &self,
        document: &Document,
        range: Range,
        options: FormattingOptions,
        edits: &mut Vec<TextEdit>,
    ) {
        let lines: Vec<&str> = document.text.lines().collect();

        // Calculate the indent level at the start of the range
        let mut indent_level = self.calculate_indent_level(document, range.start.line as usize);

        for line_num in range.start.line..=range.end.line {
            let line_num = line_num as usize;
            if line_num >= lines.len() {
                break;
            }

            let line = lines[line_num];
            let trimmed = line.trim();

            if trimmed.is_empty() {
                continue;
            }

            // Decrease indent for closing keywords
            if trimmed.starts_with("end")
                || trimmed.starts_with("else")
                || trimmed.starts_with("elseif")
            {
                indent_level = indent_level.saturating_sub(1);
            }

            // Calculate expected indentation
            let expected_indent = self.make_indent(indent_level, &options);
            let current_indent: String = line.chars().take_while(|c| c.is_whitespace()).collect();

            // Only add edit if indentation is wrong
            if expected_indent != current_indent {
                edits.push(TextEdit {
                    range: Range {
                        start: Position {
                            line: line_num as u32,
                            character: 0,
                        },
                        end: Position {
                            line: line_num as u32,
                            character: current_indent.len() as u32,
                        },
                    },
                    new_text: expected_indent.clone(),
                });
            }

            // Increase indent for opening keywords
            if trimmed.starts_with("function")
                || trimmed.starts_with("if")
                || trimmed.starts_with("while")
                || trimmed.starts_with("for")
                || trimmed.starts_with("class")
                || trimmed.starts_with("interface")
                || (trimmed.starts_with("else") && !trimmed.starts_with("elseif"))
            {
                indent_level += 1;
            }
        }
    }

    /// Calculate the indent level at a given line
    fn calculate_indent_level(&self, document: &Document, target_line: usize) -> usize {
        let lines: Vec<&str> = document.text.lines().collect();
        let mut level: usize = 0;

        for line_num in 0..=target_line.min(lines.len().saturating_sub(1)) {
            let trimmed = lines[line_num].trim();

            if trimmed.is_empty() {
                continue;
            }

            // Decrease for closing keywords
            if trimmed.starts_with("end")
                || trimmed.starts_with("else")
                || trimmed.starts_with("elseif")
            {
                level = level.saturating_sub(1);
            }

            // Don't count the target line for increment
            if line_num < target_line {
                // Increase for opening keywords
                if trimmed.starts_with("function")
                    || trimmed.starts_with("if")
                    || trimmed.starts_with("while")
                    || trimmed.starts_with("for")
                    || trimmed.starts_with("class")
                    || trimmed.starts_with("interface")
                    || (trimmed.starts_with("else") && !trimmed.starts_with("elseif"))
                {
                    level += 1;
                }
            }
        }

        level
    }

    /// Calculate the correct indent level for 'end' keyword
    fn calculate_end_indent(&self, document: &Document, line: usize) -> Option<usize> {
        let indent_level = self.calculate_indent_level(document, line);
        Some(indent_level.saturating_sub(1))
    }

    /// Create indentation string
    fn make_indent(&self, level: usize, options: &FormattingOptions) -> String {
        if options.insert_spaces {
            " ".repeat(level * options.tab_size as usize)
        } else {
            "\t".repeat(level)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_formatting_options() {
        let provider = FormattingProvider::new();

        // Test with spaces
        let doc = Document::new_test("function foo()\nlocal x = 1\nend".to_string(), 1);

        let options = FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            properties: Default::default(),
            trim_trailing_whitespace: Some(true),
            insert_final_newline: Some(false),
            trim_final_newlines: Some(false),
        };

        let edits = provider.format_document(&doc, options);
        // Should produce edits for indentation
        assert!(edits.len() > 0);

        // Test with tabs
        let options = FormattingOptions {
            tab_size: 1,
            insert_spaces: false,
            properties: Default::default(),
            trim_trailing_whitespace: Some(true),
            insert_final_newline: Some(false),
            trim_final_newlines: Some(false),
        };

        let edits = provider.format_document(&doc, options.clone());
        assert!(edits.len() > 0);
    }

    #[test]
    fn test_formatting_cases() {
        let provider = FormattingProvider::new();

        // Test indentation normalization
        let doc = Document::new_test(
            "function foo()\n        local x = 1\n    end".to_string(),
            1,
        );

        let options = FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            properties: Default::default(),
            trim_trailing_whitespace: Some(true),
            insert_final_newline: Some(false),
            trim_final_newlines: Some(false),
        };

        let edits = provider.format_document(&doc, options);
        // Should fix misaligned indentation
        assert!(edits.len() > 0);

        // Test trailing whitespace removal
        let doc = Document::new_test("local x = 1   \nlocal y = 2  ".to_string(), 1);

        let options2 = FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            properties: Default::default(),
            trim_trailing_whitespace: Some(true),
            insert_final_newline: Some(false),
            trim_final_newlines: Some(false),
        };

        let edits = provider.format_document(&doc, options2);
        // Should remove trailing spaces
        assert!(edits.len() > 0);

        // Test on-type formatting for 'end' keyword
        let doc = Document::new_test(
            "function foo()\n    local x = 1\n        end".to_string(),
            1,
        );

        let options3 = FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            properties: Default::default(),
            trim_trailing_whitespace: Some(true),
            insert_final_newline: Some(false),
            trim_final_newlines: Some(false),
        };

        let position = Position {
            line: 2,
            character: 11,
        }; // After 'end'
        let _edits = provider.format_on_type(&doc, position, "d", options3);
        // Should auto-indent 'end' to correct level
        // May or may not produce edits depending on state
    }
}
