use crate::document::DocumentManager;
use crate::providers::*;
use anyhow::Result;
use lsp_server::{Notification, Request, RequestId, Response};
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

/// Trait for sending LSP messages - allows mocking for tests
pub trait LspConnection {
    fn send_response(&self, response: Response) -> Result<()>;
    fn send_notification(&self, notification: Notification) -> Result<()>;
}

/// Message handler containing all LSP request/notification handling logic
pub struct MessageHandler {
    diagnostics_provider: DiagnosticsProvider,
    completion_provider: CompletionProvider,
    hover_provider: HoverProvider,
    definition_provider: DefinitionProvider,
    references_provider: ReferencesProvider,
    rename_provider: RenameProvider,
    symbols_provider: SymbolsProvider,
    formatting_provider: FormattingProvider,
    code_actions_provider: CodeActionsProvider,
    signature_help_provider: SignatureHelpProvider,
    inlay_hints_provider: InlayHintsProvider,
    selection_range_provider: SelectionRangeProvider,
    semantic_tokens_provider: SemanticTokensProvider,
    folding_range_provider: FoldingRangeProvider,
}

impl MessageHandler {
    pub fn new() -> Self {
        Self {
            diagnostics_provider: DiagnosticsProvider::new(),
            completion_provider: CompletionProvider::new(),
            hover_provider: HoverProvider::new(),
            definition_provider: DefinitionProvider::new(),
            references_provider: ReferencesProvider::new(),
            rename_provider: RenameProvider::new(),
            symbols_provider: SymbolsProvider::new(),
            formatting_provider: FormattingProvider::new(),
            code_actions_provider: CodeActionsProvider::new(),
            signature_help_provider: SignatureHelpProvider::new(),
            inlay_hints_provider: InlayHintsProvider::new(),
            selection_range_provider: SelectionRangeProvider::new(),
            semantic_tokens_provider: SemanticTokensProvider::new(),
            folding_range_provider: FoldingRangeProvider::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn handle_request<C: LspConnection>(
        &self,
        connection: &C,
        req: Request,
        document_manager: &DocumentManager,
    ) -> Result<()> {
        match Self::cast_request::<Completion>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document_position.text_document.uri;
                let position = params.text_document_position.position;

                let result = document_manager
                    .get(uri)
                    .map(|doc| self.completion_provider.provide(doc, position))
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

                let result = document_manager
                    .get(uri)
                    .and_then(|doc| self.hover_provider.provide(doc, position));

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

                let result = document_manager.get(uri).and_then(|doc| {
                    self.definition_provider
                        .provide(uri, doc, position, document_manager)
                });

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

                let result = document_manager.get(uri).and_then(|doc| {
                    self.references_provider.provide(
                        uri,
                        doc,
                        position,
                        include_declaration,
                        document_manager,
                    )
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

                let result = document_manager
                    .get(uri)
                    .and_then(|doc| self.rename_provider.prepare(doc, position));

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

                let result = document_manager.get(uri).and_then(|doc| {
                    self.rename_provider
                        .rename(uri, doc, position, new_name, document_manager)
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

                let result = document_manager
                    .get(uri)
                    .map(|doc| self.symbols_provider.provide(doc))
                    .map(DocumentSymbolResponse::Nested);

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<WorkspaceSymbolRequest>(req.clone()) {
            Ok((id, params)) => {
                // Use the symbol index for workspace-wide symbol search
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

                let result = document_manager
                    .get(uri)
                    .map(|doc| self.formatting_provider.format_document(doc, options));

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

                let result = document_manager
                    .get(uri)
                    .map(|doc| self.formatting_provider.format_range(doc, range, options));

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

                let result = document_manager
                    .get(uri)
                    .map(|doc| self.code_actions_provider.provide(uri, doc, range, context));

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

                let result = document_manager
                    .get(uri)
                    .and_then(|doc| self.signature_help_provider.provide(doc, position));

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

                let result = document_manager
                    .get(uri)
                    .map(|doc| self.inlay_hints_provider.provide(doc, range));

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

                let result = document_manager
                    .get(uri)
                    .map(|doc| self.selection_range_provider.provide(doc, positions));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<FoldingRangeRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;

                let result = document_manager
                    .get(uri)
                    .map(|doc| self.folding_range_provider.provide(doc));

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<SemanticTokensFullRequest>(req.clone()) {
            Ok((id, params)) => {
                let uri = &params.text_document.uri;

                let result = document_manager
                    .get(uri)
                    .map(|doc| self.semantic_tokens_provider.provide_full(doc))
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

                let result = document_manager
                    .get(uri)
                    .map(|doc| self.semantic_tokens_provider.provide_range(doc, range))
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

                let result = document_manager
                    .get(uri)
                    .map(|doc| {
                        self.semantic_tokens_provider
                            .provide_full_delta(doc, previous_result_id)
                    })
                    .map(SemanticTokensFullDeltaResult::TokensDelta);

                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<CodeActionResolveRequest>(req.clone()) {
            Ok((id, params)) => {
                let result = self.code_actions_provider.resolve(params);
                let response = Response::new_ok(id, result);
                connection.send_response(response)?;
                return Ok(());
            }
            Err(req) => req,
        };

        match Self::cast_request::<InlayHintResolveRequest>(req.clone()) {
            Ok((id, params)) => {
                let result = self.inlay_hints_provider.resolve(params);
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

                let result = document_manager.get(uri).map(|doc| {
                    self.formatting_provider
                        .format_on_type(doc, position, ch, options)
                });

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
        &self,
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
        &self,
        connection: &C,
        uri: &Uri,
        document_manager: &DocumentManager,
    ) -> Result<()> {
        if let Some(document) = document_manager.get(uri) {
            let diagnostics = self.diagnostics_provider.provide(document);
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
