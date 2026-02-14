//! Heuristics for intelligent incremental parsing decisions
//!
//! This module provides edit pattern detection and parse strategy selection
//! to decide when incremental parsing is beneficial vs when a full reparse
//! would be faster.

use lsp_types::{Position, Range, TextDocumentContentChangeEvent};

/// Configuration for incremental parsing heuristics
///
/// These thresholds determine when to use incremental vs full parsing.
/// Values can be tuned via environment variables for performance optimization.
#[derive(Debug, Clone)]
pub struct IncrementalConfig {
    /// Maximum edit size in bytes before forcing full reparse
    /// Default: 1000 (large pastes typically exceed this)
    pub max_edit_size: usize,

    /// Maximum number of dirty regions before forcing full reparse
    /// Default: 10 (too many scattered edits = fragmented parsing)
    pub max_dirty_regions: usize,

    /// Maximum ratio of affected statements (0.0-1.0)
    /// Default: 0.5 (if >50% of statements dirty, full parse is faster)
    pub max_affected_ratio: f64,

    /// Enable single-line edit optimization
    /// Default: true (common case, worth detecting)
    pub optimize_single_line: bool,

    /// Enable append-only edit optimization
    /// Default: true (typing at end of file is common)
    pub optimize_append_only: bool,
}

impl Default for IncrementalConfig {
    fn default() -> Self {
        Self {
            max_edit_size: 1000,
            max_dirty_regions: 10,
            max_affected_ratio: 0.5,
            optimize_single_line: true,
            optimize_append_only: true,
        }
    }
}

impl IncrementalConfig {
    /// Load configuration from environment variables
    ///
    /// Supported environment variables:
    /// - `LUANEXT_MAX_EDIT_SIZE`: Maximum edit size in bytes
    /// - `LUANEXT_MAX_DIRTY_REGIONS`: Maximum dirty regions
    /// - `LUANEXT_MAX_AFFECTED_RATIO`: Maximum affected ratio (0.0-1.0)
    /// - `LUANEXT_DISABLE_INCREMENTAL`: Set to disable incremental parsing
    pub fn from_env() -> Self {
        let mut config = Self::default();

        if let Ok(val) = std::env::var("LUANEXT_MAX_EDIT_SIZE") {
            if let Ok(size) = val.parse() {
                config.max_edit_size = size;
            }
        }

        if let Ok(val) = std::env::var("LUANEXT_MAX_DIRTY_REGIONS") {
            if let Ok(regions) = val.parse() {
                config.max_dirty_regions = regions;
            }
        }

        if let Ok(val) = std::env::var("LUANEXT_MAX_AFFECTED_RATIO") {
            if let Ok(ratio) = val.parse() {
                config.max_affected_ratio = ratio;
            }
        }

        // Check for disable flag - force full parse always
        if std::env::var("LUANEXT_DISABLE_INCREMENTAL").is_ok() {
            config.max_edit_size = 0; // Any edit exceeds threshold
        }

        config
    }
}

/// Parse strategy recommendation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseStrategy {
    /// Use full reparse (incremental not beneficial)
    FullParse,
    /// Use standard incremental parsing
    Incremental,
    /// Optimized for single-line edits (1-2 statements affected)
    SingleLineOptimized,
    /// Optimized for append-only edits (typing at end of file)
    AppendOnlyOptimized,
}

/// Analyzer for edit patterns and parse strategy selection
#[derive(Debug)]
pub struct ParseStrategyAnalyzer {
    config: IncrementalConfig,
}

impl ParseStrategyAnalyzer {
    /// Create a new analyzer with the given configuration
    pub fn new(config: IncrementalConfig) -> Self {
        Self { config }
    }

    /// Analyze LSP content changes and recommend parse strategy
    ///
    /// # Arguments
    /// * `changes` - LSP text document content change events
    /// * `doc_text` - Current document text (before changes)
    /// * `statement_count` - Optional number of statements in document
    ///
    /// # Returns
    /// Recommended parse strategy based on edit patterns
    pub fn analyze_lsp_changes(
        &self,
        changes: &[TextDocumentContentChangeEvent],
        doc_text: &str,
        _statement_count: Option<usize>,
    ) -> ParseStrategy {
        // Check for full document replacement
        if changes.iter().any(|c| c.range.is_none()) {
            return ParseStrategy::FullParse;
        }

        // Check for large edits (likely paste of large code block)
        let total_edit_size: usize = changes.iter().map(|c| c.text.len()).sum();
        if total_edit_size > self.config.max_edit_size {
            return ParseStrategy::FullParse;
        }

        // Check for too many edits (fragmented changes)
        if changes.len() > self.config.max_dirty_regions {
            return ParseStrategy::FullParse;
        }

        // Check for append-only edit FIRST (typing at end of file)
        // This must come before single-line check since append-only edits are also single-line
        if self.config.optimize_append_only && changes.len() == 1 {
            let change = &changes[0];
            if let Some(range) = &change.range {
                // Check if edit is at end of document
                let lines: Vec<_> = doc_text.lines().collect();
                if lines.is_empty() {
                    // Empty document, any insertion is append
                    if range.start == range.end {
                        return ParseStrategy::AppendOnlyOptimized;
                    }
                } else {
                    let last_line_idx = lines.len().saturating_sub(1);
                    let last_line = lines.last().unwrap_or(&"");
                    let is_at_end = range.start.line as usize >= last_line_idx
                        && range.start.character as usize >= last_line.len();

                    if is_at_end && range.start == range.end {
                        return ParseStrategy::AppendOnlyOptimized;
                    }
                }
            }
        }

        // Check for single-line edit optimization
        if self.config.optimize_single_line && changes.len() == 1 {
            let change = &changes[0];
            if let Some(range) = &change.range {
                if range.start.line == range.end.line && change.text.len() < 100 {
                    return ParseStrategy::SingleLineOptimized;
                }
            }
        }

        // Default to standard incremental parsing
        ParseStrategy::Incremental
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_change(
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
        text: String,
    ) -> TextDocumentContentChangeEvent {
        TextDocumentContentChangeEvent {
            range: Some(Range {
                start: Position {
                    line: start_line,
                    character: start_char,
                },
                end: Position {
                    line: end_line,
                    character: end_char,
                },
            }),
            text,
            range_length: None,
        }
    }

    #[test]
    fn test_full_document_replacement() {
        let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

        let change = TextDocumentContentChangeEvent {
            range: None,
            text: "completely new content".to_string(),
            range_length: None,
        };

        let strategy = analyzer.analyze_lsp_changes(&[change], "", None);
        assert_eq!(strategy, ParseStrategy::FullParse);
    }

    #[test]
    fn test_large_edit_forces_full_parse() {
        let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

        let large_text = "x".repeat(2000);
        let change = make_change(0, 0, 0, 5, large_text);

        let strategy = analyzer.analyze_lsp_changes(&[change], "const x = 1", None);
        assert_eq!(strategy, ParseStrategy::FullParse);
    }

    #[test]
    fn test_many_edits_forces_full_parse() {
        let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

        let changes: Vec<_> = (0..15)
            .map(|i| make_change(i, 0, i, 1, "x".to_string()))
            .collect();

        let strategy = analyzer.analyze_lsp_changes(&changes, "", None);
        assert_eq!(strategy, ParseStrategy::FullParse);
    }

    #[test]
    fn test_single_line_edit_detection() {
        let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

        let change = make_change(10, 5, 10, 6, "x".to_string());

        let strategy =
            analyzer.analyze_lsp_changes(&[change], "local x = 1\nlocal y = 2\n", Some(2));
        assert_eq!(strategy, ParseStrategy::SingleLineOptimized);
    }

    #[test]
    fn test_append_only_detection_empty_document() {
        let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

        let change = make_change(0, 0, 0, 0, "local x = 1".to_string());

        let strategy = analyzer.analyze_lsp_changes(&[change], "", None);
        assert_eq!(strategy, ParseStrategy::AppendOnlyOptimized);
    }

    #[test]
    fn test_append_only_detection_at_end() {
        let text = "local x = 1\nlocal y = 2";
        let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

        // Append at end of last line
        let change = make_change(1, 11, 1, 11, "\nlocal z = 3".to_string());

        let strategy = analyzer.analyze_lsp_changes(&[change], text, Some(2));
        assert_eq!(strategy, ParseStrategy::AppendOnlyOptimized);
    }

    #[test]
    fn test_multiline_edit_uses_standard_incremental() {
        let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

        let change = make_change(0, 0, 2, 0, "replaced text".to_string());

        let strategy = analyzer.analyze_lsp_changes(&[change], "line1\nline2\nline3", None);
        assert_eq!(strategy, ParseStrategy::Incremental);
    }

    #[test]
    fn test_config_from_env() {
        std::env::set_var("LUANEXT_MAX_EDIT_SIZE", "500");
        std::env::set_var("LUANEXT_MAX_DIRTY_REGIONS", "5");
        std::env::set_var("LUANEXT_MAX_AFFECTED_RATIO", "0.3");

        let config = IncrementalConfig::from_env();
        assert_eq!(config.max_edit_size, 500);
        assert_eq!(config.max_dirty_regions, 5);
        assert_eq!(config.max_affected_ratio, 0.3);

        std::env::remove_var("LUANEXT_MAX_EDIT_SIZE");
        std::env::remove_var("LUANEXT_MAX_DIRTY_REGIONS");
        std::env::remove_var("LUANEXT_MAX_AFFECTED_RATIO");
    }

    #[test]
    fn test_disable_incremental_env_var() {
        std::env::set_var("LUANEXT_DISABLE_INCREMENTAL", "1");

        let config = IncrementalConfig::from_env();
        assert_eq!(config.max_edit_size, 0); // Forces all edits to full parse

        std::env::remove_var("LUANEXT_DISABLE_INCREMENTAL");
    }

    #[test]
    fn test_medium_edit_under_threshold() {
        let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

        // 500 bytes - under 1000 threshold
        let medium_text = "x".repeat(500);
        let change = make_change(0, 0, 0, 5, medium_text);

        let strategy = analyzer.analyze_lsp_changes(&[change], "const x = 1", None);
        // Should use incremental because it's under threshold and multiline
        assert_eq!(strategy, ParseStrategy::Incremental);
    }
}
