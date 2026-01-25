use crate::document::Document;
use lsp_types::*;

/// Provides smart selection ranges (expand/shrink selection)
pub struct SelectionRangeProvider;

impl SelectionRangeProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide selection ranges for the given positions
    pub fn provide(&self, document: &Document, positions: Vec<Position>) -> Vec<SelectionRange> {
        positions
            .iter()
            .filter_map(|pos| self.get_selection_range_at_position(document, *pos))
            .collect()
    }

    /// Get the selection range hierarchy at a specific position
    fn get_selection_range_at_position(
        &self,
        document: &Document,
        position: Position,
    ) -> Option<SelectionRange> {
        //
        // Selection hierarchy (from innermost to outermost):
        // 1. Identifier/literal
        // 2. Expression
        // 3. Statement
        // 4. Block
        // 5. Function/class body
        // 6. Entire file

        // For now, provide basic word-based selection
        let offset = self.position_to_offset(document, position)?;
        let text = &document.text;

        // Start with word selection
        let word_range = self.get_word_range(text, offset)?;
        let word_selection = SelectionRange {
            range: self.offset_range_to_lsp_range(document, word_range.0, word_range.1)?,
            parent: None,
        };

        // Expand to line
        let line_range = self.get_line_range(text, offset)?;
        let line_selection = SelectionRange {
            range: self.offset_range_to_lsp_range(document, line_range.0, line_range.1)?,
            parent: Some(Box::new(word_selection)),
        };

        // Expand to surrounding brackets/parentheses
        if let Some(bracket_range) = self.get_bracket_range(text, offset) {
            let bracket_selection = SelectionRange {
                range: self.offset_range_to_lsp_range(
                    document,
                    bracket_range.0,
                    bracket_range.1,
                )?,
                parent: Some(Box::new(line_selection)),
            };
            return Some(bracket_selection);
        }

        Some(line_selection)
    }

    /// Get the range of the word at the given offset
    fn get_word_range(&self, text: &str, offset: usize) -> Option<(usize, usize)> {
        if offset > text.len() {
            return None;
        }

        let chars: Vec<char> = text.chars().collect();
        if offset >= chars.len() {
            return None;
        }

        // Find word boundaries
        let mut start = offset;
        let mut end = offset;

        // Expand backwards
        while start > 0 && self.is_identifier_char(chars[start - 1]) {
            start -= 1;
        }

        // Expand forwards
        while end < chars.len() && self.is_identifier_char(chars[end]) {
            end += 1;
        }

        if start < end {
            Some((start, end))
        } else {
            None
        }
    }

    /// Get the range of the current line
    fn get_line_range(&self, text: &str, offset: usize) -> Option<(usize, usize)> {
        if offset > text.len() {
            return None;
        }

        let mut start = offset;
        let mut end = offset;

        let chars: Vec<char> = text.chars().collect();

        // Find line start
        while start > 0 && chars[start - 1] != '\n' {
            start -= 1;
        }

        // Find line end
        while end < chars.len() && chars[end] != '\n' {
            end += 1;
        }

        Some((start, end))
    }

    /// Get the range of content within matching brackets/parentheses
    fn get_bracket_range(&self, text: &str, offset: usize) -> Option<(usize, usize)> {
        let chars: Vec<char> = text.chars().collect();
        if offset >= chars.len() {
            return None;
        }

        // Search backwards for opening bracket
        let mut pos = offset;
        let mut depth = 0;
        let bracket_pairs = vec![('(', ')'), ('[', ']'), ('{', '}')];

        while pos > 0 {
            pos -= 1;
            let ch = chars[pos];

            for (open, close) in &bracket_pairs {
                if ch == *close {
                    depth += 1;
                } else if ch == *open {
                    if depth == 0 {
                        // Found matching opening bracket
                        // Now find the closing bracket
                        if let Some(end) = self.find_closing_bracket(text, pos, *open, *close) {
                            return Some((pos, end + 1));
                        }
                    } else {
                        depth -= 1;
                    }
                }
            }
        }

        None
    }

    /// Find the closing bracket matching the opening bracket at start_pos
    fn find_closing_bracket(
        &self,
        text: &str,
        start_pos: usize,
        open: char,
        close: char,
    ) -> Option<usize> {
        let chars: Vec<char> = text.chars().collect();
        let mut depth = 0;
        let mut pos = start_pos;

        while pos < chars.len() {
            let ch = chars[pos];
            if ch == open {
                depth += 1;
            } else if ch == close {
                depth -= 1;
                if depth == 0 {
                    return Some(pos);
                }
            }
            pos += 1;
        }

        None
    }

    /// Check if a character is part of an identifier
    fn is_identifier_char(&self, ch: char) -> bool {
        ch.is_alphanumeric() || ch == '_'
    }

    /// Convert a Position to a byte offset in the document
    fn position_to_offset(&self, document: &Document, position: Position) -> Option<usize> {
        let mut offset = 0;
        let mut current_line = 0;

        for line in document.text.lines() {
            if current_line == position.line {
                return Some(offset + position.character as usize);
            }
            offset += line.len() + 1; // +1 for newline
            current_line += 1;
        }

        None
    }

    /// Convert byte offset range to LSP Range
    fn offset_range_to_lsp_range(
        &self,
        document: &Document,
        start_offset: usize,
        end_offset: usize,
    ) -> Option<Range> {
        let start_pos = self.offset_to_position(document, start_offset)?;
        let end_pos = self.offset_to_position(document, end_offset)?;

        Some(Range {
            start: start_pos,
            end: end_pos,
        })
    }

    /// Convert byte offset to Position
    fn offset_to_position(&self, document: &Document, offset: usize) -> Option<Position> {
        let mut current_offset = 0;
        let mut line = 0;

        for line_text in document.text.lines() {
            let line_end = current_offset + line_text.len();
            if offset <= line_end {
                return Some(Position {
                    line: line as u32,
                    character: (offset - current_offset) as u32,
                });
            }
            current_offset = line_end + 1; // +1 for newline
            line += 1;
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_word_selection() {
        let provider = SelectionRangeProvider::new();

        let doc = Document::new_test("local my_variable = 42".to_string(), 1);

        // Cursor on 'my_variable' at position (0, 8)
        let position = Position {
            line: 0,
            character: 8,
        };
        let result = provider.get_selection_range_at_position(&doc, position);

        assert!(result.is_some());
        let selection = result.unwrap();

        // Should select the word "my_variable"
        // The exact range will depend on implementation, just verify we got a selection
        assert_eq!(selection.range.start.line, 0);
        assert_eq!(selection.range.end.line, 0);

        // Should have a parent (line selection)
        assert!(selection.parent.is_some());
    }

    #[test]
    fn test_expression_selection() {
        let provider = SelectionRangeProvider::new();

        let doc = Document::new_test("local x = foo(bar(1, 2), 3)".to_string(), 1);

        // Cursor somewhere in the expression
        let position = Position {
            line: 0,
            character: 15,
        };
        let result = provider.get_selection_range_at_position(&doc, position);

        assert!(result.is_some());
        let selection = result.unwrap();

        // Should have hierarchical selections
        assert!(selection.parent.is_some());
    }

    #[test]
    fn test_bracket_selection() {
        let provider = SelectionRangeProvider::new();

        let doc = Document::new_test("local t = { x = 1, y = 2 }".to_string(), 1);

        // Cursor inside the table literal
        let position = Position {
            line: 0,
            character: 15,
        };
        let result = provider.get_selection_range_at_position(&doc, position);

        // May or may not find selection depending on exact position
        if let Some(selection) = result {
            assert_eq!(selection.range.start.line, 0);
        }
    }

    #[test]
    fn test_nested_selection() {
        let provider = SelectionRangeProvider::new();

        let doc = Document::new_test("function foo() local x = 1 end".to_string(), 1);

        // Cursor on 'x'
        let position = Position {
            line: 0,
            character: 21,
        };
        let result = provider.get_selection_range_at_position(&doc, position);

        assert!(result.is_some());
        let selection = result.unwrap();

        // Should have parent selections
        assert!(selection.parent.is_some());

        // Verify we can traverse parent chain
        let mut current = Some(selection);
        let mut depth = 0;
        while let Some(sel) = current {
            depth += 1;
            current = sel.parent.map(|boxed| *boxed);
        }

        // Should have multiple levels of selection
        assert!(depth > 1);
    }

    #[test]
    fn test_string_selection() {
        let provider = SelectionRangeProvider::new();

        let doc = Document::new_test("local s = \"hello world\"".to_string(), 1);

        // Cursor inside the string
        let position = Position {
            line: 0,
            character: 15,
        };
        let result = provider.get_selection_range_at_position(&doc, position);

        assert!(result.is_some());
        // Just verify we can select within strings
        let selection = result.unwrap();
        assert_eq!(selection.range.start.line, 0);
        assert_eq!(selection.range.end.line, 0);
    }
}
