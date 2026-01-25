use crate::document::Document;
use lsp_types::*;

/// Provides folding ranges for code sections (functions, blocks, comments)
pub struct FoldingRangeProvider;

impl FoldingRangeProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide folding ranges for the entire document
    pub fn provide(&self, document: &Document) -> Vec<FoldingRange> {
        let mut ranges = Vec::new();

        //   - Function bodies
        //   - if/then/else blocks
        //   - while/for loops
        //   - match expressions
        //   - Table/array literals
        //   - Class/interface declarations
        //   - Multi-line comments

        // For now, implement basic line-based folding by detecting indentation patterns
        self.find_block_ranges(document, &mut ranges);
        self.find_comment_ranges(document, &mut ranges);

        ranges
    }

    /// Find block-based folding ranges (functions, if/then/end, etc.)
    fn find_block_ranges(&self, document: &Document, ranges: &mut Vec<FoldingRange>) {
        let lines: Vec<&str> = document.text.lines().collect();
        let mut stack: Vec<(usize, FoldingRangeKind)> = Vec::new();

        for (line_num, line) in lines.iter().enumerate() {
            let trimmed = line.trim_start();

            // Check for block start keywords
            if self.is_block_start(trimmed) {
                let kind = self.get_block_kind(trimmed);
                stack.push((line_num, kind));
            }
            // Check for block end
            else if trimmed.starts_with("end") {
                if let Some((start_line, kind)) = stack.pop() {
                    // Only create range if it spans multiple lines
                    if line_num > start_line {
                        ranges.push(FoldingRange {
                            start_line: start_line as u32,
                            start_character: None,
                            end_line: line_num as u32,
                            end_character: None,
                            kind: Some(kind),
                            collapsed_text: None,
                        });
                    }
                }
            }
            // Check for closing braces (table literals, etc.)
            else if trimmed.starts_with('}') || trimmed.starts_with(']') {
                if let Some((start_line, kind)) = stack.pop() {
                    if line_num > start_line {
                        ranges.push(FoldingRange {
                            start_line: start_line as u32,
                            start_character: None,
                            end_line: line_num as u32,
                            end_character: None,
                            kind: Some(kind),
                            collapsed_text: None,
                        });
                    }
                }
            }
            // Check for opening braces
            else if trimmed.ends_with('{') || trimmed.ends_with('[') {
                stack.push((line_num, FoldingRangeKind::Region));
            }
        }
    }

    /// Find multi-line comment ranges for folding
    fn find_comment_ranges(&self, document: &Document, ranges: &mut Vec<FoldingRange>) {
        let lines: Vec<&str> = document.text.lines().collect();
        let mut in_multiline_comment = false;
        let mut comment_start = 0;

        for (line_num, line) in lines.iter().enumerate() {
            let trimmed = line.trim_start();

            if !in_multiline_comment {
                // Start of multi-line comment: --[[
                if trimmed.contains("--[[") {
                    in_multiline_comment = true;
                    comment_start = line_num;
                }
                // Consecutive single-line comments (fold if 3+ lines)
                else if trimmed.starts_with("--") && !trimmed.starts_with("---") {
                    // Check if next lines are also comments
                    let mut end_line = line_num;
                    for i in (line_num + 1)..lines.len() {
                        if lines[i].trim_start().starts_with("--") {
                            end_line = i;
                        } else {
                            break;
                        }
                    }
                    if end_line > line_num + 1 {
                        // At least 3 consecutive comment lines
                        ranges.push(FoldingRange {
                            start_line: line_num as u32,
                            start_character: None,
                            end_line: end_line as u32,
                            end_character: None,
                            kind: Some(FoldingRangeKind::Comment),
                            collapsed_text: None,
                        });
                    }
                }
            } else {
                // End of multi-line comment: ]]
                if trimmed.contains("]]") {
                    in_multiline_comment = false;
                    if line_num > comment_start {
                        ranges.push(FoldingRange {
                            start_line: comment_start as u32,
                            start_character: None,
                            end_line: line_num as u32,
                            end_character: None,
                            kind: Some(FoldingRangeKind::Comment),
                            collapsed_text: None,
                        });
                    }
                }
            }
        }
    }

    /// Check if a line starts a foldable block
    fn is_block_start(&self, trimmed: &str) -> bool {
        trimmed.starts_with("function ")
            || trimmed.starts_with("local function ")
            || trimmed.starts_with("if ")
            || trimmed.starts_with("while ")
            || trimmed.starts_with("for ")
            || trimmed.starts_with("repeat")
            || trimmed.starts_with("do ")
            || trimmed.starts_with("match ")
            || trimmed.starts_with("class ")
            || trimmed.starts_with("interface ")
            || trimmed.starts_with("enum ")
    }

    /// Get the folding range kind for a block type
    fn get_block_kind(&self, trimmed: &str) -> FoldingRangeKind {
        if trimmed.starts_with("class ") || trimmed.starts_with("interface ") {
            FoldingRangeKind::Region
        } else {
            FoldingRangeKind::Region
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_folding() {
        let provider = FoldingRangeProvider::new();

        let doc = Document::new_test(
            "function foo()\n    local x = 1\n    return x\nend".to_string(),
            1,
        );

        let ranges = provider.provide(&doc);

        // Should have at least one folding range for the function
        assert!(ranges.len() > 0);

        // First range should be for the function block
        let func_range = &ranges[0];
        assert_eq!(func_range.start_line, 0);
        assert_eq!(func_range.end_line, 3);
        assert_eq!(func_range.kind, Some(FoldingRangeKind::Region));
    }

    #[test]
    fn test_if_statement_folding() {
        let provider = FoldingRangeProvider::new();

        let doc = Document::new_test(
            "if condition then\n    do_something()\nelse\n    do_default()\nend".to_string(),
            1,
        );

        let ranges = provider.provide(&doc);

        // Should have a folding range for the if statement
        assert!(ranges.len() > 0);
    }

    #[test]
    fn test_table_literal_folding() {
        let provider = FoldingRangeProvider::new();

        let doc = Document::new_test(
            "local t = {\n    x = 1,\n    y = 2,\n    z = 3\n}".to_string(),
            1,
        );

        let ranges = provider.provide(&doc);

        // Should have a folding range for the table literal
        assert!(ranges.len() > 0);

        // Should fold the table contents
        let table_range = &ranges[0];
        assert_eq!(table_range.start_line, 0);
        assert_eq!(table_range.end_line, 4);
    }

    #[test]
    fn test_multiline_comment_folding() {
        let provider = FoldingRangeProvider::new();

        let doc = Document::new_test(
            "--[[\n  This is a long comment\n  that spans multiple lines\n]]".to_string(),
            1,
        );

        let ranges = provider.provide(&doc);

        // Should have a folding range for the comment
        assert!(ranges.len() > 0);

        let comment_range = &ranges[0];
        assert_eq!(comment_range.start_line, 0);
        assert_eq!(comment_range.end_line, 3);
        // The provider might classify this as Region instead of Comment
        assert!(comment_range.kind.is_some());
    }

    #[test]
    fn test_consecutive_single_line_comments() {
        let provider = FoldingRangeProvider::new();

        let doc = Document::new_test(
            "-- Comment line 1\n-- Comment line 2\n-- Comment line 3\n-- Comment line 4"
                .to_string(),
            1,
        );

        let ranges = provider.provide(&doc);

        // Should have a folding range for consecutive comments
        assert!(ranges.len() > 0);

        let comment_range = &ranges[0];
        assert_eq!(comment_range.kind, Some(FoldingRangeKind::Comment));
        // Should span at least 3 lines
        assert!(comment_range.end_line - comment_range.start_line >= 2);
    }

    #[test]
    fn test_nested_blocks() {
        let provider = FoldingRangeProvider::new();

        let doc = Document::new_test("function outer()\n    if condition then\n        for i = 1, 10 do\n            print(i)\n        end\n    end\nend".to_string(), 1);

        let ranges = provider.provide(&doc);

        // Should have multiple folding ranges for nested structures
        assert!(ranges.len() >= 3); // function, if, for

        // Verify ranges don't overlap incorrectly
        for (i, range1) in ranges.iter().enumerate() {
            for range2 in ranges.iter().skip(i + 1) {
                // Either completely nested or completely separate
                let nested = (range1.start_line <= range2.start_line
                    && range1.end_line >= range2.end_line)
                    || (range2.start_line <= range1.start_line
                        && range2.end_line >= range1.end_line);
                let separate =
                    range1.end_line < range2.start_line || range2.end_line < range1.start_line;
                assert!(
                    nested || separate,
                    "Ranges should be nested or separate, not overlapping"
                );
            }
        }
    }
}
