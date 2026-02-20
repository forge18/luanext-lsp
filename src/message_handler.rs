use crate::core::diagnostics::DiagnosticsProvider;
use crate::core::DocumentManager;
use crate::di::{DiContainer, ServiceLifetime};
use crate::features::*;
use crate::protocol::LspConnection;
use crate::traits::{
    DefinitionProviderTrait, DiagnosticsProviderTrait, HoverProviderTrait, ReferencesProviderTrait,
    SymbolsProviderTrait,
};
use anyhow::Result;
use lsp_server::{Notification, Response};
use tracing;

use lsp_server::{Request, RequestId};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
    PublishDiagnostics,
};

use lsp_types::request::{
    CodeActionRequest, CodeActionResolveRequest, Completion, DocumentSymbolRequest,
    FoldingRangeRequest, Formatting, GotoDefinition, HoverRequest, InlayHintRequest,
    InlayHintResolveRequest, OnTypeFormatting, PrepareRenameRequest, RangeFormatting, References,
    Rename, ResolveCompletionItem, SelectionRangeRequest, SemanticTokensFullDeltaRequest,
    SemanticTokensFullRequest, SemanticTokensRangeRequest, SignatureHelpRequest,
    WorkspaceSymbolRequest,
};
use lsp_types::*;
use serde::{de::DeserializeOwned, Serialize};

/// Basic message handler for document lifecycle and diagnostics (no type-aware features)
///
/// This handler provides core LSP functionality without requiring typedlua-core:
/// - Document lifecycle (open, change, save, close)
/// - Publishing diagnostics
///
/// Use this when you need LSP document management without full type checking capabilities.
/// For complete IDE features (completion, hover, goto-definition, etc.), use `MessageHandler`.
pub struct BasicMessageHandler;

impl BasicMessageHandler {
    pub fn new() -> Self {
        Self
    }

    /// Handle LSP notifications (document lifecycle events)
    pub fn handle_notification<C: LspConnection>(
        &self,
        connection: &C,
        not: Notification,
        document_manager: &mut DocumentManager,
    ) -> Result<()> {
        match Self::cast_notification::<DidOpenTextDocument>(not.clone()) {
            Ok(params) => {
                document_manager.open(params);
                return Ok(());
            }
            Err(not) => not,
        };

        match Self::cast_notification::<DidChangeTextDocument>(not.clone()) {
            Ok(params) => {
                document_manager.change(params);
                return Ok(());
            }
            Err(not) => not,
        };

        match Self::cast_notification::<DidSaveTextDocument>(not.clone()) {
            Ok(params) => {
                document_manager.save(params);
                return Ok(());
            }
            Err(not) => not,
        };

        match Self::cast_notification::<DidCloseTextDocument>(not.clone()) {
            Ok(params) => {
                let uri = params.text_document.uri.clone();
                document_manager.close(params);
                // Clear diagnostics on close
                Self::send_notification::<PublishDiagnostics>(
                    connection,
                    PublishDiagnosticsParams {
                        uri,
                        diagnostics: vec![],
                        version: None,
                    },
                )?;
                return Ok(());
            }
            Err(_not) => {
                // Unknown notification, ignore
            }
        };

        Ok(())
    }

    /// Publish diagnostics for a document
    ///
    /// This allows external diagnostic providers (like linters) to publish their diagnostics
    /// through the LSP connection without needing type checking functionality.
    pub fn publish_diagnostics<C: LspConnection>(
        &self,
        connection: &C,
        uri: &Uri,
        diagnostics: Vec<lsp_types::Diagnostic>,
    ) -> Result<()> {
        Self::send_notification::<PublishDiagnostics>(
            connection,
            PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics,
                version: None,
            },
        )?;
        Ok(())
    }

    fn cast_notification<N>(not: Notification) -> std::result::Result<N::Params, Notification>
    where
        N: lsp_types::notification::Notification,
        N::Params: DeserializeOwned,
    {
        match not.extract(N::METHOD) {
            Ok(params) => Ok(params),
            Err(lsp_server::ExtractError::MethodMismatch(not)) => Err(not),
            Err(lsp_server::ExtractError::JsonError { method, error }) => {
                tracing::error!("Failed to deserialize notification {}: {}", method, error);
                Err(Notification::new(
                    method.to_string(),
                    serde_json::Value::Null,
                ))
            }
        }
    }

    fn send_notification<N>(connection: &impl LspConnection, params: N::Params) -> Result<()>
    where
        N: lsp_types::notification::Notification,
        N::Params: Serialize,
    {
        let not = Notification::new(N::METHOD.to_string(), params);
        connection.send_notification(not)?;
        Ok(())
    }
}

impl Default for BasicMessageHandler {
    fn default() -> Self {
        Self::new()
    }
}

/// Full-featured message handler with type-aware LSP capabilities (requires compiler feature)
///
/// This handler provides complete IDE functionality including:
/// - All features from `BasicMessageHandler`
/// - Type-aware completion
/// - Hover information
/// - Go to definition
/// - Find references
/// - Rename
/// - Document symbols
/// - And more...
///
/// Requires the `compiler` feature flag and pulls in typedlua-core as a dependency.

pub struct MessageHandler {
    container: DiContainer,
}

impl MessageHandler {
    pub fn new() -> Self {
        let mut container = DiContainer::new();
        Self::register_services(&mut container);
        Self { container }
    }

    pub fn with_container(container: DiContainer) -> Self {
        Self { container }
    }

    fn register_services(container: &mut DiContainer) {
        container.register(|_| DiagnosticsProvider::new(), ServiceLifetime::Transient);
        container.register(|_| CompletionProvider::new(), ServiceLifetime::Transient);
        container.register(|_| HoverProvider::new(), ServiceLifetime::Transient);
        container.register(|_| DefinitionProvider::new(), ServiceLifetime::Transient);
        container.register(|_| ReferencesProvider::new(), ServiceLifetime::Transient);
        container.register(|_| RenameProvider::new(), ServiceLifetime::Transient);
        container.register(|_| SymbolsProvider::new(), ServiceLifetime::Transient);
        container.register(|_| FormattingProvider::new(), ServiceLifetime::Transient);
        container.register(|_| CodeActionsProvider::new(), ServiceLifetime::Transient);
        container.register(|_| SignatureHelpProvider::new(), ServiceLifetime::Transient);
        container.register(|_| InlayHintsProvider::new(), ServiceLifetime::Transient);
        container.register(
            |_| SelectionRangeProvider::new(),
            ServiceLifetime::Transient,
        );
        container.register(
            |_| SemanticTokensProvider::new(),
            ServiceLifetime::Transient,
        );
        container.register(|_| FoldingRangeProvider::new(), ServiceLifetime::Transient);
    }

    #[allow(clippy::too_many_arguments)]
    pub fn handle_request<C: LspConnection>(
        &mut self,
        connection: &C,
        req: Request,
        document_manager: &DocumentManager,
    ) -> Result<()> {
        match Self::cast_request::<Completion>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document_position.text_document.uri;
                let position = params.text_document_position.position;

                let completion_provider = self.container.resolve::<CompletionProvider>().unwrap();
                let workspace_root = document_manager.workspace_root();
                let result = document_manager
                    .get(uri)
                    .map(|doc| {
                        completion_provider.provide_with_workspace(
                            doc,
                            position,
                            Some(workspace_root),
                        )
                    })
                    .map(CompletionResponse::Array);

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<ResolveCompletionItem>(req.clone()) {
            Ok((id, params)) => {
                let completion_provider = self.container.resolve::<CompletionProvider>().unwrap();
                let result = completion_provider.resolve(params);
                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<HoverRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document_position_params.text_document.uri;
                let position = params.text_document_position_params.position;

                let hover_provider = self.container.resolve::<HoverProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .and_then(|doc| hover_provider.provide(doc, position));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<GotoDefinition>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document_position_params.text_document.uri;
                let position = params.text_document_position_params.position;

                let definition_provider = self.container.resolve::<DefinitionProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .and_then(|doc| definition_provider.provide(uri, doc, position));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<References>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document_position.text_document.uri;
                let position = params.text_document_position.position;
                let include_declaration = params.context.include_declaration;

                let references_provider = self.container.resolve::<ReferencesProvider>().unwrap();
                let result = document_manager.get(uri).map(|doc| {
                    references_provider.provide(uri, doc, position, include_declaration)
                });

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<PrepareRenameRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;
                let position = params.position;

                let rename_provider = self.container.resolve::<RenameProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .and_then(|doc| rename_provider.prepare(doc, position));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<Rename>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document_position.text_document.uri;
                let position = params.text_document_position.position;
                let new_name = &params.new_name;

                let rename_provider = self.container.resolve::<RenameProvider>().unwrap();
                let result = document_manager.get(uri).and_then(|doc| {
                    rename_provider.rename(uri, doc, position, new_name, document_manager)
                });

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<DocumentSymbolRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;

                let symbols_provider = self.container.resolve::<SymbolsProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| symbols_provider.provide(doc));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<WorkspaceSymbolRequest>(req.clone()) {
            Ok((id, params)) => {
                let symbols = document_manager
                    .symbol_index()
                    .search_workspace_symbols(&params.query);
                let response = Response::new_ok(id, Some(symbols));
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<Formatting>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;
                let options = params.options;

                let formatting_provider = self.container.resolve::<FormattingProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| formatting_provider.format_document(doc, options));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<RangeFormatting>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;
                let range = params.range;
                let options = params.options;

                let formatting_provider = self.container.resolve::<FormattingProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| formatting_provider.format_range(doc, range, options));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<CodeActionRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;
                let range = params.range;
                let context = params.context;

                let code_actions_provider =
                    self.container.resolve::<CodeActionsProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| code_actions_provider.provide(uri, doc, range, context));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<SignatureHelpRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document_position_params.text_document.uri;
                let position = params.text_document_position_params.position;

                let signature_help_provider =
                    self.container.resolve::<SignatureHelpProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .and_then(|doc| signature_help_provider.provide(doc, position));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<InlayHintRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;
                let range = params.range;

                let inlay_hints_provider = self.container.resolve::<InlayHintsProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| inlay_hints_provider.provide(doc, range));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<SelectionRangeRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;
                let positions = params.positions;

                let selection_range_provider =
                    self.container.resolve::<SelectionRangeProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| selection_range_provider.provide(doc, positions));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<FoldingRangeRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;

                let folding_range_provider =
                    self.container.resolve::<FoldingRangeProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| folding_range_provider.provide(doc));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<SemanticTokensFullRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;

                let semantic_tokens_provider =
                    self.container.resolve::<SemanticTokensProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| semantic_tokens_provider.provide_full(doc))
                    .map(SemanticTokensResult::Tokens);

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<SemanticTokensRangeRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;
                let range = params.range;

                let semantic_tokens_provider =
                    self.container.resolve::<SemanticTokensProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| semantic_tokens_provider.provide_range(doc, range))
                    .map(SemanticTokensRangeResult::Tokens);

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<SemanticTokensFullDeltaRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;
                let previous_result_id = params.previous_result_id;

                let semantic_tokens_provider =
                    self.container.resolve::<SemanticTokensProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| semantic_tokens_provider.provide_full_delta(doc, previous_result_id))
                    .map(SemanticTokensFullDeltaResult::TokensDelta);

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<CodeActionResolveRequest>(req.clone()) {
            Ok((id, params)) => {
                let code_actions_provider =
                    self.container.resolve::<CodeActionsProvider>().unwrap();
                let result = code_actions_provider.resolve(params);
                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<InlayHintResolveRequest>(req.clone()) {
            Ok((id, params)) => {
                let inlay_hints_provider = self.container.resolve::<InlayHintsProvider>().unwrap();
                let result = inlay_hints_provider.resolve(params);
                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<OnTypeFormatting>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document_position.text_document.uri;
                let position = params.text_document_position.position;
                let ch = &params.ch;
                let options = params.options;

                let formatting_provider = self.container.resolve::<FormattingProvider>().unwrap();
                let result = document_manager
                    .get(uri)
                    .map(|doc| formatting_provider.format_on_type(doc, position, ch, options));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(_req) => {
                // Unknown request, ignore
            }
        };

        Ok(())
    }

    pub fn handle_notification<C: LspConnection>(
        &mut self,
        connection: &C,
        not: Notification,
        document_manager: &mut DocumentManager,
    ) -> Result<()> {
        match Self::cast_notification::<DidOpenTextDocument>(not.clone()) {
            Ok(params) => {
                let uri = params.text_document.uri.clone();
                document_manager.open(params);
                self.publish_diagnostics(connection, &uri, document_manager)?;
                return Ok(());
            }
            Err(not) => not,
        };

        match Self::cast_notification::<DidChangeTextDocument>(not.clone()) {
            Ok(params) => {
                let uri = params.text_document.uri.clone();
                document_manager.change(params);
                self.publish_diagnostics(connection, &uri, document_manager)?;
                return Ok(());
            }
            Err(not) => not,
        };

        match Self::cast_notification::<DidSaveTextDocument>(not.clone()) {
            Ok(params) => {
                let uri = params.text_document.uri.clone();
                document_manager.save(params);
                self.publish_diagnostics(connection, &uri, document_manager)?;
                return Ok(());
            }
            Err(not) => not,
        };

        match Self::cast_notification::<DidCloseTextDocument>(not.clone()) {
            Ok(params) => {
                let uri = params.text_document.uri.clone();
                document_manager.close(params);
                // Clear diagnostics on close
                Self::send_notification::<PublishDiagnostics>(
                    connection,
                    PublishDiagnosticsParams {
                        uri,
                        diagnostics: vec![],
                        version: None,
                    },
                )?;
                return Ok(());
            }
            Err(_not) => {
                // Unknown notification, ignore
            }
        };

        Ok(())
    }

    fn publish_diagnostics<C: LspConnection>(
        &mut self,
        connection: &C,
        uri: &Uri,
        document_manager: &DocumentManager,
    ) -> Result<()> {
        if let Some(document) = document_manager.get(uri) {
            let diagnostics_provider = self.container.resolve::<DiagnosticsProvider>().unwrap();
            let new_diagnostics = diagnostics_provider.provide(document);

            // Deduplication: skip sending if diagnostics are identical to what the client last saw
            let should_send = {
                let cache = document.cache();
                match &cache.last_published_diagnostics {
                    Some(prev) => *prev != new_diagnostics,
                    None => true,
                }
            };

            if should_send {
                document.cache_mut().last_published_diagnostics = Some(new_diagnostics.clone());
                Self::send_notification::<PublishDiagnostics>(
                    connection,
                    PublishDiagnosticsParams {
                        uri: uri.clone(),
                        diagnostics: new_diagnostics,
                        version: None,
                    },
                )?;
            }
        }
        Ok(())
    }

    fn cast_request<R>(req: Request) -> std::result::Result<(RequestId, R::Params), Request>
    where
        R: lsp_types::request::Request,
        R::Params: DeserializeOwned,
    {
        match req.extract(R::METHOD) {
            Ok(params) => Ok(params),
            Err(lsp_server::ExtractError::MethodMismatch(req)) => Err(req),
            Err(lsp_server::ExtractError::JsonError { method, error }) => {
                tracing::error!("Failed to deserialize request {}: {}", method, error);
                Err(Request::new(
                    RequestId::from(0),
                    method.to_string(),
                    serde_json::Value::Null,
                ))
            }
        }
    }

    fn cast_notification<N>(not: Notification) -> std::result::Result<N::Params, Notification>
    where
        N: lsp_types::notification::Notification,
        N::Params: DeserializeOwned,
    {
        match not.extract(N::METHOD) {
            Ok(params) => Ok(params),
            Err(lsp_server::ExtractError::MethodMismatch(not)) => Err(not),
            Err(lsp_server::ExtractError::JsonError { method, error }) => {
                tracing::error!("Failed to deserialize notification {}: {}", method, error);
                Err(Notification::new(
                    method.to_string(),
                    serde_json::Value::Null,
                ))
            }
        }
    }

    fn send_notification<N>(connection: &impl LspConnection, params: N::Params) -> Result<()>
    where
        N: lsp_types::notification::Notification,
        N::Params: Serialize,
    {
        let not = Notification::new(N::METHOD.to_string(), params);
        connection.send_notification(not)?;
        Ok(())
    }
}

impl Default for MessageHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::protocol::LspConnection;
    use lsp_server::{Notification, Request, RequestId};
    use std::str::FromStr;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    #[derive(Clone)]
    struct MockConnection {
        notification_count: Arc<AtomicUsize>,
        response_count: Arc<AtomicUsize>,
    }

    impl MockConnection {
        fn new() -> (Self, Arc<AtomicUsize>, Arc<AtomicUsize>) {
            let notification_count = Arc::new(AtomicUsize::new(0));
            let response_count = Arc::new(AtomicUsize::new(0));
            (
                Self {
                    notification_count: notification_count.clone(),
                    response_count: response_count.clone(),
                },
                notification_count,
                response_count,
            )
        }
    }

    impl LspConnection for MockConnection {
        fn send_notification(&self, _not: Notification) -> anyhow::Result<()> {
            self.notification_count.fetch_add(1, Ordering::SeqCst);
            Ok(())
        }

        fn send_response(&self, _response: lsp_server::Response) -> anyhow::Result<()> {
            self.response_count.fetch_add(1, Ordering::SeqCst);
            Ok(())
        }
    }

    fn create_did_open_notification(text: &str, version: i32) -> Notification {
        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: Uri::from_str("file:///test.lua").unwrap(),
                language_id: "lua".to_string(),
                version,
                text: text.to_string(),
            },
        };
        Notification::new(
            "textDocument/didOpen".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_completion_request() -> Request {
        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str("file:///test.lua").unwrap(),
                },
                position: Position {
                    line: 0,
                    character: 5,
                },
            },
            context: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        Request::new(
            RequestId::from(1),
            "textDocument/completion".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_hover_request() -> Request {
        let params = HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str("file:///test.lua").unwrap(),
                },
                position: Position {
                    line: 0,
                    character: 5,
                },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        };
        Request::new(
            RequestId::from(2),
            "textDocument/hover".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_definition_request() -> Request {
        let params = GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str("file:///test.lua").unwrap(),
                },
                position: Position {
                    line: 0,
                    character: 5,
                },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        Request::new(
            RequestId::from(3),
            "textDocument/definition".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_references_request() -> Request {
        let params = ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str("file:///test.lua").unwrap(),
                },
                position: Position {
                    line: 0,
                    character: 5,
                },
            },
            context: ReferenceContext {
                include_declaration: true,
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        Request::new(
            RequestId::from(4),
            "textDocument/references".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_document_symbols_request() -> Request {
        let params = DocumentSymbolParams {
            text_document: TextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        Request::new(
            RequestId::from(5),
            "textDocument/documentSymbol".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_workspace_symbol_request() -> Request {
        let params = WorkspaceSymbolParams {
            query: "test".to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        Request::new(
            RequestId::from(6),
            "workspace/symbol".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_formatting_request() -> Request {
        let params = DocumentFormattingParams {
            text_document: TextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
            },
            options: FormattingOptions {
                tab_size: 4,
                insert_spaces: true,
                ..Default::default()
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        };
        Request::new(
            RequestId::from(7),
            "textDocument/formatting".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_signature_help_request() -> Request {
        let params = SignatureHelpParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str("file:///test.lua").unwrap(),
                },
                position: Position {
                    line: 0,
                    character: 5,
                },
            },
            context: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        };
        Request::new(
            RequestId::from(8),
            "textDocument/signatureHelp".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_inlay_hints_request() -> Request {
        let params = InlayHintParams {
            text_document: TextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
            },
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 10,
                    character: 0,
                },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        };
        Request::new(
            RequestId::from(9),
            "textDocument/inlayHint".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_selection_range_request() -> Request {
        let params = SelectionRangeParams {
            text_document: TextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
            },
            positions: vec![
                Position {
                    line: 0,
                    character: 5,
                },
                Position {
                    line: 1,
                    character: 10,
                },
            ],
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        Request::new(
            RequestId::from(10),
            "textDocument/selectionRange".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_folding_range_request() -> Request {
        let params = FoldingRangeParams {
            text_document: TextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        Request::new(
            RequestId::from(11),
            "textDocument/foldingRange".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_semantic_tokens_request() -> Request {
        let params = SemanticTokensParams {
            text_document: TextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        Request::new(
            RequestId::from(12),
            "textDocument/semanticTokens/full".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    fn create_code_action_request() -> Request {
        let params = CodeActionParams {
            text_document: TextDocumentIdentifier {
                uri: Uri::from_str("file:///test.lua").unwrap(),
            },
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 10,
                },
            },
            context: CodeActionContext {
                diagnostics: vec![],
                only: None,
                trigger_kind: Some(CodeActionTriggerKind::INVOKED),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };
        Request::new(
            RequestId::from(13),
            "textDocument/codeAction".to_string(),
            serde_json::to_value(params).unwrap(),
        )
    }

    #[test]
    fn test_basic_message_handler_new() {
        let handler = BasicMessageHandler::new();
        let _ = handler;
    }

    #[test]
    fn test_basic_message_handler_default() {
        let handler = BasicMessageHandler::default();
        let _ = handler;
    }

    #[test]
    fn test_message_handler_new() {
        let handler = MessageHandler::new();
        let _ = handler;
    }

    #[test]
    fn test_message_handler_default() {
        let handler = MessageHandler::default();
        let _ = handler;
    }

    #[test]
    fn test_message_handler_with_container() {
        let container = DiContainer::new();
        let handler = MessageHandler::with_container(container);
        let _ = handler;
    }

    #[test]
    fn test_basic_message_handler_publish_diagnostics() {
        let handler = BasicMessageHandler::new();
        let (conn, notification_count, _) = MockConnection::new();
        let uri = Uri::from_str("file:///test.lua").unwrap();

        let diagnostics = vec![lsp_types::Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 5,
                },
            },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            message: "Test error".to_string(),
            ..Default::default()
        }];

        let result = handler.publish_diagnostics(&conn, &uri, diagnostics);
        assert!(result.is_ok());
        assert_eq!(notification_count.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_basic_message_handler_unknown_notification() {
        let handler = BasicMessageHandler::new();
        let (conn, _, _) = MockConnection::new();

        let unknown_not =
            Notification::new("unknown/notification".to_string(), serde_json::json!({}));

        let result =
            handler.handle_notification(&conn, unknown_not, &mut DocumentManager::new_test());
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_completion_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_completion_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_hover_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_hover_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_definition_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_definition_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_references_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_references_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_document_symbols_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_document_symbols_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_workspace_symbol_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_workspace_symbol_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_formatting_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_formatting_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_signature_help_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_signature_help_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_inlay_hints_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_inlay_hints_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_selection_range_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_selection_range_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_folding_range_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_folding_range_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_semantic_tokens_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_semantic_tokens_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_code_action_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = create_code_action_request();

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_unknown_request() {
        let mut handler = MessageHandler::new();
        let document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let req = Request::new(
            RequestId::from(99),
            "unknown/request".to_string(),
            serde_json::json!({}),
        );

        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }

    #[test]
    fn test_message_handler_notification_open() {
        let mut handler = MessageHandler::new();
        let mut document_manager = DocumentManager::new_test();
        let (conn, notification_count, _) = MockConnection::new();

        let not = create_did_open_notification("local x = 1", 1);

        let result = handler.handle_notification(&conn, not, &mut document_manager);
        assert!(result.is_ok());
        assert!(notification_count.load(Ordering::SeqCst) > 0);
    }

    #[test]
    fn test_message_handler_notification_open_and_request() {
        let mut handler = MessageHandler::new();
        let mut document_manager = DocumentManager::new_test();
        let (conn, _, _) = MockConnection::new();

        let not = create_did_open_notification("local foo = 1", 1);
        handler
            .handle_notification(&conn, not, &mut document_manager)
            .unwrap();

        let req = create_hover_request();
        let result = handler.handle_request(&conn, req, &document_manager);
        assert!(result.is_ok());
    }
}
