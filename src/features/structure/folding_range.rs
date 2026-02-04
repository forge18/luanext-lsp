use crate::core::document::Document;
use lsp_types::*;

/// Provides folding ranges for code sections (functions, blocks, comments)
#[derive(Clone)]
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
                }
            } else {
                // End of multi-line comment: ]]
                if trimmed.contains("]]") {
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
                    in_multiline_comment = false;
                }
            }
        }
    }

    fn is_block_start(&self, line: &str) -> bool {
        line.starts_with("function")
            || line.starts_with("if")
            || line.starts_with("while")
            || line.starts_with("for")
            || line.starts_with("repeat")
            || line.starts_with("do")
            || line.starts_with("class")
            || line.starts_with("interface")
            || line.starts_with("enum")
            || line.starts_with("match")
    }

    fn get_block_kind(&self, line: &str) -> FoldingRangeKind {
        if line.starts_with("function") {
            FoldingRangeKind::Function
        } else if line.starts_with("if") || line.starts_with("else") {
            FoldingRangeKind::Annotation
        } else if line.starts_with("while") || line.starts_with("for") {
            FoldingRangeKind::Iteration
        } else if line.starts_with("class") {
            FoldingRangeKind::Class
        } else if line.starts_with("interface") {
            FoldingRangeKind::Interface
        } else {
            FoldingRangeKind::Region
        }
    }
}
