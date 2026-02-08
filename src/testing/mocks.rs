//! Mock implementations for testing without the typechecker
//!
//! These provide simple, fast implementations that don't require
//! the git dependency on typedlua-typechecker.

use crate::core::document::Document;
use crate::traits::*;
use lsp_types::*;

#[derive(Clone)]
pub struct MockCompletionProvider;

impl MockCompletionProvider {
    pub fn new() -> Self {
        Self
    }
}

impl CompletionProviderTrait for MockCompletionProvider {
    fn provide(&self, document: &Document, position: Position) -> Vec<CompletionItem> {
        let line = document
            .text
            .lines()
            .nth(position.line as usize)
            .unwrap_or("");
        let prefix: String = line.chars().take(position.character as usize).collect();

        let completions = vec![
            CompletionItem {
                label: "local".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            CompletionItem {
                label: "function".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            CompletionItem {
                label: "if".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            CompletionItem {
                label: "print".to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                ..Default::default()
            },
        ];

        completions
            .into_iter()
            .filter(|c| c.label.starts_with(&prefix) || prefix.is_empty())
            .collect()
    }

    fn resolve(&self, item: CompletionItem) -> CompletionItem {
        item
    }
}

#[derive(Clone)]
pub struct MockHoverProvider;

impl MockHoverProvider {
    pub fn new() -> Self {
        Self
    }
}

impl HoverProviderTrait for MockHoverProvider {
    fn provide(&self, document: &Document, position: Position) -> Option<Hover> {
        let line = document
            .text
            .lines()
            .nth(position.line as usize)
            .unwrap_or("");
        let word = extract_word_at_position(line, position.character as usize);

        let detail = match word.as_str() {
            "local" => Some("Local variable declaration"),
            "function" => Some("Function declaration"),
            "if" => Some("Conditional statement"),
            "print" => Some("Built-in print function"),
            "return" => Some("Return statement"),
            "end" => Some("End of block"),
            _ => None,
        };

        detail.map(|d| Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("**{}**", d),
            }),
            range: None,
        })
    }
}

#[derive(Clone)]
pub struct MockDefinitionProvider;

impl MockDefinitionProvider {
    pub fn new() -> Self {
        Self
    }
}

impl DefinitionProviderTrait for MockDefinitionProvider {
    fn provide(
        &self,
        uri: &Uri,
        _document: &Document,
        position: Position,
    ) -> Option<GotoDefinitionResponse> {
        let location = Location {
            uri: uri.clone(),
            range: Range {
                start: position,
                end: position,
            },
        };
        Some(GotoDefinitionResponse::Scalar(location))
    }
}

#[derive(Clone)]
pub struct MockReferencesProvider;

impl MockReferencesProvider {
    pub fn new() -> Self {
        Self
    }
}

impl ReferencesProviderTrait for MockReferencesProvider {
    fn provide(
        &self,
        _uri: &Uri,
        _document: &Document,
        _position: Position,
        _include_declaration: bool,
    ) -> Vec<Location> {
        vec![]
    }
}

#[derive(Clone)]
pub struct MockRenameProvider;

impl MockRenameProvider {
    pub fn new() -> Self {
        Self
    }
}

impl RenameProviderTrait for MockRenameProvider {
    fn prepare(&self, _document: &Document, position: Position) -> Option<Range> {
        Some(Range {
            start: position,
            end: position,
        })
    }

    fn rename(
        &self,
        _uri: &Uri,
        _document: &Document,
        _position: Position,
        _new_name: &str,
    ) -> Option<WorkspaceEdit> {
        Some(WorkspaceEdit::default())
    }
}

#[derive(Clone)]
pub struct MockSymbolsProvider;

impl MockSymbolsProvider {
    pub fn new() -> Self {
        Self
    }
}

impl SymbolsProviderTrait for MockSymbolsProvider {
    fn provide(&self, _document: &Document) -> DocumentSymbolResponse {
        DocumentSymbolResponse::Flat(vec![])
    }
}

#[derive(Clone)]
pub struct MockFormattingProvider;

impl MockFormattingProvider {
    pub fn new() -> Self {
        Self
    }
}

impl FormattingProviderTrait for MockFormattingProvider {
    fn format_document(&self, document: &Document, _options: FormattingOptions) -> Option<String> {
        Some(document.text.clone())
    }

    fn format_range(
        &self,
        document: &Document,
        range: Range,
        _options: FormattingOptions,
    ) -> Option<String> {
        let lines: Vec<&str> = document.text.lines().collect();
        let start_line = range.start.line as usize;
        let end_line = (range.end.line as usize).min(lines.len().saturating_sub(1));
        let result = lines[start_line..=end_line].join("\n");
        Some(result)
    }

    fn format_on_type(
        &self,
        document: &Document,
        _position: Position,
        _ch: &str,
        _options: FormattingOptions,
    ) -> Option<String> {
        Some(document.text.clone())
    }
}

#[derive(Clone)]
pub struct MockCodeActionsProvider;

impl MockCodeActionsProvider {
    pub fn new() -> Self {
        Self
    }
}

impl CodeActionsProviderTrait for MockCodeActionsProvider {
    fn provide(
        &self,
        _uri: &Uri,
        _document: &Document,
        _range: Range,
        _context: CodeActionContext,
    ) -> Option<CodeActionResponse> {
        Some(vec![])
    }

    fn resolve(&self, item: CodeAction) -> Option<CodeAction> {
        Some(item)
    }
}

#[derive(Clone)]
pub struct MockSignatureHelpProvider;

impl MockSignatureHelpProvider {
    pub fn new() -> Self {
        Self
    }
}

impl SignatureHelpProviderTrait for MockSignatureHelpProvider {
    fn provide(&self, _document: &Document, _position: Position) -> Option<SignatureHelp> {
        Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label: "function(...)".to_string(),
                documentation: None,
                parameters: None,
                active_parameter: None,
            }],
            active_signature: Some(0),
            active_parameter: Some(0),
        })
    }
}

#[derive(Clone)]
pub struct MockInlayHintsProvider;

impl MockInlayHintsProvider {
    pub fn new() -> Self {
        Self
    }
}

impl InlayHintsProviderTrait for MockInlayHintsProvider {
    fn provide(&self, _document: &Document, _range: Range) -> Option<Vec<InlayHint>> {
        Some(vec![])
    }

    fn resolve(&self, item: InlayHint) -> Option<InlayHint> {
        Some(item)
    }
}

#[derive(Clone)]
pub struct MockSelectionRangeProvider;

impl MockSelectionRangeProvider {
    pub fn new() -> Self {
        Self
    }
}

impl SelectionRangeProviderTrait for MockSelectionRangeProvider {
    fn provide(&self, _document: &Document, positions: Vec<Position>) -> Vec<SelectionRange> {
        positions
            .into_iter()
            .map(|pos| SelectionRange {
                range: Range {
                    start: pos,
                    end: pos,
                },
                parent: None,
            })
            .collect()
    }
}

#[derive(Clone)]
pub struct MockFoldingRangeProvider;

impl MockFoldingRangeProvider {
    pub fn new() -> Self {
        Self
    }
}

impl FoldingRangeProviderTrait for MockFoldingRangeProvider {
    fn provide(&self, _document: &Document) -> Vec<FoldingRange> {
        vec![FoldingRange {
            start_line: 0,
            start_character: Some(0),
            end_line: 0,
            end_character: Some(19),
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: None,
        }]
    }
}

#[derive(Clone)]
pub struct MockSemanticTokensProvider;

impl MockSemanticTokensProvider {
    pub fn new() -> Self {
        Self
    }
}

impl SemanticTokensProviderTrait for MockSemanticTokensProvider {
    fn provide_full(&self, _document: &Document) -> Option<Vec<SemanticToken>> {
        Some(vec![])
    }

    fn provide_range(&self, _document: &Document, _range: Range) -> Option<Vec<SemanticToken>> {
        Some(vec![])
    }

    fn provide_full_delta(
        &self,
        _document: &Document,
        _previous_result_id: Option<String>,
    ) -> Option<SemanticTokensDelta> {
        Some(SemanticTokensDelta {
            edits: vec![],
            result_id: None,
        })
    }
}

#[derive(Clone)]
pub struct MockDiagnosticsProvider;

impl MockDiagnosticsProvider {
    pub fn new() -> Self {
        Self
    }
}

impl DiagnosticsProviderTrait for MockDiagnosticsProvider {
    fn provide(&self, _document: &Document) -> Vec<lsp_types::Diagnostic> {
        vec![]
    }
}

use crate::core::analysis::SymbolIndex;
use lsp_types::Uri;
use std::collections::HashMap;
use luanext_typechecker::module_resolver::ModuleId;

#[derive(Clone)]
pub struct MockDocumentManager {
    documents: HashMap<Uri, String>,
    symbol_index: SymbolIndex,
}

impl MockDocumentManager {
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            symbol_index: SymbolIndex::new(),
        }
    }

    pub fn add_document(&mut self, uri: Uri, text: String) {
        self.documents.insert(uri, text);
    }

    pub fn get_text(&self, uri: &Uri) -> Option<&String> {
        self.documents.get(uri)
    }
}

impl crate::core::document::DocumentManagerTrait for MockDocumentManager {
    fn get(&self, _uri: &Uri) -> Option<&crate::core::document::Document> {
        None
    }

    fn symbol_index(&self) -> &SymbolIndex {
        &self.symbol_index
    }

    fn module_id_to_uri(&self, _module_id: &ModuleId) -> Option<&Uri> {
        None
    }

    fn uri_to_module_id(&self, _uri: &Uri) -> Option<&ModuleId> {
        None
    }
}

fn extract_word_at_position(line: &str, char_pos: usize) -> String {
    let chars: Vec<char> = line.chars().collect();
    if char_pos > chars.len() {
        return String::new();
    }

    let mut start = char_pos;
    while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
        start -= 1;
    }

    let mut end = char_pos;
    while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
        end += 1;
    }

    chars[start..end].iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::document::Document;
    use lsp_types::Position;
    use std::str::FromStr;

    fn create_test_doc(text: &str) -> Document {
        Document::new_test(text.to_string(), 1)
    }

    #[test]
    fn test_mock_completion_provider_new() {
        let provider = MockCompletionProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_completion_provide_keywords() {
        let provider = MockCompletionProvider::new();
        let doc = create_test_doc("lo");
        let result = provider.provide(&doc, Position::new(0, 2));
        assert!(result.iter().any(|c| c.label == "local"));
    }

    #[test]
    fn test_mock_hover_provider_new() {
        let provider = MockHoverProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_hover_local_keyword() {
        let provider = MockHoverProvider::new();
        let doc = create_test_doc("local x = 1");
        let result = provider.provide(&doc, Position::new(0, 1));
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_definition_provider_new() {
        let provider = MockDefinitionProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_definition_provide() {
        let provider = MockDefinitionProvider::new();
        let doc = create_test_doc("local x = 1");
        let uri = Uri::from_str("file:///test.lua").unwrap();
        let result = provider.provide(&uri, &doc, Position::new(0, 6));
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_references_provider_new() {
        let provider = MockReferencesProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_rename_provider_new() {
        let provider = MockRenameProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_rename_prepare() {
        let provider = MockRenameProvider::new();
        let doc = create_test_doc("local x = 1");
        let result = provider.prepare(&doc, Position::new(0, 6));
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_rename_execute() {
        let provider = MockRenameProvider::new();
        let doc = create_test_doc("local x = 1");
        let uri = Uri::from_str("file:///test.lua").unwrap();
        let result = provider.rename(&uri, &doc, Position::new(0, 6), "new_x");
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_symbols_provider_new() {
        let provider = MockSymbolsProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_symbols_provide() {
        let provider = MockSymbolsProvider::new();
        let doc = create_test_doc("local x = 1");
        let result = provider.provide(&doc);
        // DocumentSymbolResponse is a type alias
        let _ = result;
    }

    #[test]
    fn test_mock_folding_range_provide() {
        let provider = MockFoldingRangeProvider::new();
        let doc = create_test_doc("function foo() end");
        let result = provider.provide(&doc);
        assert!(!result.is_empty());
    }

    #[test]
    fn test_mock_formatting_provider_new() {
        let provider = MockFormattingProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_formatting_document() {
        let provider = MockFormattingProvider::new();
        let doc = create_test_doc("local x = 1");
        let result = provider.format_document(&doc, FormattingOptions::default());
        assert!(result.is_some());
        assert_eq!(result.unwrap(), "local x = 1");
    }

    #[test]
    fn test_mock_formatting_range() {
        let provider = MockFormattingProvider::new();
        let doc = create_test_doc("local x = 1\nlocal y = 2");
        let result = provider.format_range(
            &doc,
            Range {
                start: Position::new(1, 0),
                end: Position::new(1, 11),
            },
            FormattingOptions::default(),
        );
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_code_actions_provider_new() {
        let provider = MockCodeActionsProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_code_actions_provide() {
        let provider = MockCodeActionsProvider::new();
        let doc = create_test_doc("local x = 1");
        let uri = Uri::from_str("file:///test.lua").unwrap();
        let result = provider.provide(
            &uri,
            &doc,
            Range {
                start: Position::new(0, 0),
                end: Position::new(0, 11),
            },
            CodeActionContext::default(),
        );
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_signature_help_provider_new() {
        let provider = MockSignatureHelpProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_signature_help_provide() {
        let provider = MockSignatureHelpProvider::new();
        let doc = create_test_doc("foo(");
        let result = provider.provide(&doc, Position::new(0, 4));
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_inlay_hints_provider_new() {
        let provider = MockInlayHintsProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_inlay_hints_provide() {
        let provider = MockInlayHintsProvider::new();
        let doc = create_test_doc("local x = 1");
        let result = provider.provide(
            &doc,
            Range {
                start: Position::new(0, 0),
                end: Position::new(0, 12),
            },
        );
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_selection_range_provider_new() {
        let provider = MockSelectionRangeProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_selection_range_provide() {
        let provider = MockSelectionRangeProvider::new();
        let doc = create_test_doc("local x = 1");
        let result = provider.provide(&doc, vec![Position::new(0, 6)]);
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn test_mock_semantic_tokens_provider_new() {
        let provider = MockSemanticTokensProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_semantic_tokens_full() {
        let provider = MockSemanticTokensProvider::new();
        let doc = create_test_doc("local x = 1");
        let result = provider.provide_full(&doc);
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_semantic_tokens_range() {
        let provider = MockSemanticTokensProvider::new();
        let doc = create_test_doc("local x = 1");
        let result = provider.provide_range(
            &doc,
            Range {
                start: Position::new(0, 0),
                end: Position::new(0, 12),
            },
        );
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_semantic_tokens_delta() {
        let provider = MockSemanticTokensProvider::new();
        let doc = create_test_doc("local x = 1");
        let result = provider.provide_full_delta(&doc, Some("prev".to_string()));
        assert!(result.is_some());
    }

    #[test]
    fn test_mock_diagnostics_provider_new() {
        let provider = MockDiagnosticsProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_mock_diagnostics_provide() {
        let provider = MockDiagnosticsProvider::new();
        let doc = create_test_doc("local x = 1");
        let result = provider.provide(&doc);
        assert!(result.is_empty());
    }

    #[test]
    fn test_extract_word_at_position() {
        let result = extract_word_at_position("local x = 1", 6);
        assert_eq!(result, "x");
    }

    #[test]
    fn test_extract_word_at_position_empty() {
        let result = extract_word_at_position("", 0);
        assert_eq!(result, "");
    }

    #[test]
    fn test_extract_word_at_position_start() {
        let result = extract_word_at_position("local x = 1", 0);
        assert_eq!(result, "local");
    }

    #[test]
    fn test_extract_word_at_position_in_word() {
        let result = extract_word_at_position("local x = 1", 2);
        assert_eq!(result, "local");
    }
}
