//! Incremental semantic token updates.
//!
//! For single-line edits, adjusts token positions in the cached semantic tokens
//! rather than recomputing from scratch. Falls back to full recompute when the
//! edit spans multiple lines or changes document structure.

use lsp_types::{SemanticToken, SemanticTokens};

/// Describes a text edit for incremental token adjustment.
#[derive(Debug, Clone)]
pub struct TokenTextEdit {
    /// The line the edit starts on (0-indexed).
    pub start_line: u32,
    /// The character offset where the edit starts on start_line (0-indexed).
    pub start_char: u32,
    /// The line the edit ends on (0-indexed).
    pub end_line: u32,
    /// The character offset where the edit ends on end_line (0-indexed).
    pub end_char: u32,
    /// Number of lines in the new text.
    pub new_line_count: u32,
    /// Length of the last line of the new text (in characters).
    pub new_last_line_length: u32,
}

impl TokenTextEdit {
    /// Whether this edit is confined to a single line.
    pub fn is_single_line(&self) -> bool {
        self.start_line == self.end_line && self.new_line_count == 0
    }

    /// Character delta for tokens on the same line as a single-line edit.
    /// Positive means characters shifted right, negative means shifted left.
    fn char_delta(&self) -> i32 {
        debug_assert!(self.is_single_line());
        self.new_last_line_length as i32 - (self.end_char as i32 - self.start_char as i32)
    }
}

/// Attempt to update semantic tokens incrementally for a single-line edit.
///
/// Returns `Some(updated_tokens)` if the edit can be applied incrementally,
/// or `None` if a full recompute is needed (multi-line edit, structural change).
///
/// # How it works
///
/// Semantic tokens use delta encoding: each token stores `delta_line` and
/// `delta_start` relative to the previous token. A single-line edit only
/// affects tokens on the edited line - their `delta_start` values need
/// adjustment if they come after the edit position.
pub fn update_semantic_tokens(
    prev: &SemanticTokens,
    edit: &TokenTextEdit,
    new_result_id: String,
) -> Option<SemanticTokens> {
    if !edit.is_single_line() {
        return None;
    }

    let char_delta = edit.char_delta();
    if char_delta == 0 {
        // No position shift needed, but content changed - need full recompute
        // since token types/modifiers might have changed.
        return None;
    }

    let edit_line = edit.start_line;
    let edit_start_char = edit.start_char;

    // Reconstruct absolute positions, adjust, and re-encode
    let mut adjusted = Vec::with_capacity(prev.data.len());
    let mut abs_line: u32 = 0;
    let mut abs_char: u32 = 0;

    // Collect absolute positions first
    let mut absolute_tokens: Vec<(u32, u32, u32, u32, u32)> = Vec::with_capacity(prev.data.len());
    for token in &prev.data {
        if token.delta_line > 0 {
            abs_line += token.delta_line;
            abs_char = token.delta_start;
        } else {
            abs_char += token.delta_start;
        }
        absolute_tokens.push((
            abs_line,
            abs_char,
            token.length,
            token.token_type,
            token.token_modifiers_bitset,
        ));
    }

    // Adjust positions for tokens on the edited line that start after the edit
    for entry in &mut absolute_tokens {
        if entry.0 == edit_line && entry.1 >= edit_start_char {
            // Token starts at or after edit position - shift it
            let new_char = entry.1 as i32 + char_delta;
            if new_char < 0 {
                // Edit caused token to have negative position - need full recompute
                return None;
            }
            entry.1 = new_char as u32;
        }
    }

    // Re-encode as deltas
    let mut prev_line: u32 = 0;
    let mut prev_char: u32 = 0;
    for &(line, char_pos, length, token_type, modifiers) in &absolute_tokens {
        let delta_line = line - prev_line;
        let delta_start = if delta_line > 0 {
            char_pos
        } else {
            char_pos - prev_char
        };

        adjusted.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: modifiers,
        });

        prev_line = line;
        prev_char = char_pos;
    }

    Some(SemanticTokens {
        result_id: Some(new_result_id),
        data: adjusted,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_token(delta_line: u32, delta_start: u32, length: u32) -> SemanticToken {
        SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: 5, // VARIABLE
            token_modifiers_bitset: 0,
        }
    }

    fn make_tokens(data: Vec<SemanticToken>) -> SemanticTokens {
        SemanticTokens {
            result_id: Some("1".to_string()),
            data,
        }
    }

    #[test]
    fn test_single_line_edit_shifts_tokens_right() {
        // Tokens: "x" at (0,6) and "y" at (0,14)
        // Simulates typing 3 chars at position 4 on line 0
        let prev = make_tokens(vec![
            make_token(0, 6, 1), // x at col 6
            make_token(0, 8, 1), // y at col 14
        ]);

        let edit = TokenTextEdit {
            start_line: 0,
            start_char: 4,
            end_line: 0,
            end_char: 4,
            new_line_count: 0,
            new_last_line_length: 3, // inserted 3 chars
        };

        let result = update_semantic_tokens(&prev, &edit, "2".to_string());
        assert!(result.is_some());
        let updated = result.unwrap();

        // Both tokens should be shifted right by 3
        assert_eq!(updated.data[0].delta_line, 0);
        assert_eq!(updated.data[0].delta_start, 9); // 6 + 3
        assert_eq!(updated.data[1].delta_line, 0);
        assert_eq!(updated.data[1].delta_start, 8); // delta unchanged (both shifted equally)
    }

    #[test]
    fn test_single_line_edit_shifts_tokens_left() {
        // Delete 2 chars at position 4-6 on line 0
        let prev = make_tokens(vec![
            make_token(0, 10, 1), // token at col 10
        ]);

        let edit = TokenTextEdit {
            start_line: 0,
            start_char: 4,
            end_line: 0,
            end_char: 6,
            new_line_count: 0,
            new_last_line_length: 0, // deleted 2 chars, inserted 0
        };

        let result = update_semantic_tokens(&prev, &edit, "2".to_string());
        assert!(result.is_some());
        let updated = result.unwrap();

        // Token shifted left by 2
        assert_eq!(updated.data[0].delta_start, 8); // 10 - 2
    }

    #[test]
    fn test_multiline_edit_returns_none() {
        let prev = make_tokens(vec![make_token(0, 5, 1)]);

        let edit = TokenTextEdit {
            start_line: 0,
            start_char: 5,
            end_line: 1,
            end_char: 0,
            new_line_count: 0,
            new_last_line_length: 0,
        };

        assert!(update_semantic_tokens(&prev, &edit, "2".to_string()).is_none());
    }

    #[test]
    fn test_new_lines_inserted_returns_none() {
        let prev = make_tokens(vec![make_token(0, 5, 1)]);

        let edit = TokenTextEdit {
            start_line: 0,
            start_char: 5,
            end_line: 0,
            end_char: 5,
            new_line_count: 1,
            new_last_line_length: 0,
        };

        assert!(update_semantic_tokens(&prev, &edit, "2".to_string()).is_none());
    }

    #[test]
    fn test_tokens_before_edit_unchanged() {
        // Token at col 2, edit at col 10 - token should not move
        let prev = make_tokens(vec![
            make_token(0, 2, 1),  // before edit
            make_token(0, 10, 1), // after edit (at col 12)
        ]);

        let edit = TokenTextEdit {
            start_line: 0,
            start_char: 10,
            end_line: 0,
            end_char: 10,
            new_line_count: 0,
            new_last_line_length: 5, // insert 5 chars
        };

        let result = update_semantic_tokens(&prev, &edit, "2".to_string()).unwrap();

        // First token unchanged
        assert_eq!(result.data[0].delta_start, 2);
        // Second token shifted: was at col 12, now at col 17
        // delta from first token was 10, now should be 15
        assert_eq!(result.data[1].delta_start, 15);
    }

    #[test]
    fn test_tokens_on_other_lines_unchanged() {
        let prev = make_tokens(vec![
            make_token(0, 5, 1),  // line 0, col 5
            make_token(1, 10, 1), // line 1, col 10
            make_token(1, 3, 1),  // line 2, col 3
        ]);

        // Edit on line 0
        let edit = TokenTextEdit {
            start_line: 0,
            start_char: 3,
            end_line: 0,
            end_char: 3,
            new_line_count: 0,
            new_last_line_length: 2,
        };

        let result = update_semantic_tokens(&prev, &edit, "2".to_string()).unwrap();

        // Line 0 token shifted: 5 + 2 = 7
        assert_eq!(result.data[0].delta_start, 7);
        // Line 1 token unchanged
        assert_eq!(result.data[1].delta_line, 1);
        assert_eq!(result.data[1].delta_start, 10);
        // Line 2 token unchanged
        assert_eq!(result.data[2].delta_line, 1);
        assert_eq!(result.data[2].delta_start, 3);
    }

    #[test]
    fn test_empty_tokens() {
        let prev = make_tokens(vec![]);

        let edit = TokenTextEdit {
            start_line: 0,
            start_char: 0,
            end_line: 0,
            end_char: 0,
            new_line_count: 0,
            new_last_line_length: 5,
        };

        let result = update_semantic_tokens(&prev, &edit, "2".to_string()).unwrap();
        assert!(result.data.is_empty());
    }

    #[test]
    fn test_zero_char_delta_returns_none() {
        // Replace 3 chars with 3 chars at same position - content changed but length didn't
        let prev = make_tokens(vec![make_token(0, 5, 3)]);

        let edit = TokenTextEdit {
            start_line: 0,
            start_char: 5,
            end_line: 0,
            end_char: 8,
            new_line_count: 0,
            new_last_line_length: 3,
        };

        // Should return None because positions didn't change but content might have
        assert!(update_semantic_tokens(&prev, &edit, "2".to_string()).is_none());
    }

    #[test]
    fn test_result_id_updated() {
        let prev = make_tokens(vec![make_token(0, 10, 1)]);

        let edit = TokenTextEdit {
            start_line: 0,
            start_char: 5,
            end_line: 0,
            end_char: 5,
            new_line_count: 0,
            new_last_line_length: 1,
        };

        let result = update_semantic_tokens(&prev, &edit, "42".to_string()).unwrap();
        assert_eq!(result.result_id, Some("42".to_string()));
    }
}
