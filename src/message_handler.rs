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
    Rename, SelectionRangeRequest, SemanticTokensFullDeltaRequest, SemanticTokensFullRequest,
    SemanticTokensRangeRequest, SignatureHelpRequest, WorkspaceSymbolRequest,
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
#[allow(dead_code)]
pub struct BasicMessageHandler;

#[allow(dead_code)]
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
                let result = document_manager
                    .get(uri)
                    .map(|doc| completion_provider.provide(doc, position))
                    .map(CompletionResponse::Array);

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
                    .map(|doc| symbols_provider.provide(doc))
                    .map(DocumentSymbolResponse::Nested);

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
            let diagnostics = diagnostics_provider.provide(document);
            Self::send_notification::<PublishDiagnostics>(
                connection,
                PublishDiagnosticsParams {
                    uri: uri.clone(),
                    diagnostics,
                    version: None,
                },
            )?;
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
