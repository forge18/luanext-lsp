#![allow(clippy::all)]
#![allow(deprecated)]

mod core;
mod di;
mod features;
mod impls;
mod message_handler;
mod protocol;
mod traits;

use crate::protocol::LspConnection;
use anyhow::Result;
use core::DocumentManager;
use lsp_server::{Connection, Message, Notification, Response};
use lsp_types::*;
use message_handler::MessageHandler;
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;
use tracing_subscriber::EnvFilter;
use typedlua_typechecker::cli::config::CompilerOptions;
use typedlua_typechecker::cli::fs::RealFileSystem;
use typedlua_typechecker::module_resolver::{ModuleConfig, ModuleRegistry, ModuleResolver};

// Implement LspConnection for the real lsp_server::Connection
struct ConnectionWrapper<'a>(&'a Connection);

impl LspConnection for ConnectionWrapper<'_> {
    fn send_response(&self, response: Response) -> Result<()> {
        self.0.sender.send(Message::Response(response))?;
        Ok(())
    }

    fn send_notification(&self, notification: Notification) -> Result<()> {
        self.0.sender.send(Message::Notification(notification))?;
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::INFO.into()))
        .with_writer(std::io::stderr)
        .init();

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        )),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![
                ".".to_string(),
                ":".to_string(),
                "@".to_string(),
                "<".to_string(),
                "{".to_string(),
                "(".to_string(),
            ]),
            resolve_provider: Some(true),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        document_highlight_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
            code_action_kinds: Some(vec![
                CodeActionKind::QUICKFIX,
                CodeActionKind::REFACTOR,
                CodeActionKind::SOURCE_ORGANIZE_IMPORTS,
            ]),
            resolve_provider: Some(true),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        })),
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        })),
        document_formatting_provider: Some(OneOf::Left(true)),
        document_range_formatting_provider: Some(OneOf::Left(true)),
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: vec![
                        SemanticTokenType::CLASS,
                        SemanticTokenType::INTERFACE,
                        SemanticTokenType::ENUM,
                        SemanticTokenType::TYPE,
                        SemanticTokenType::PARAMETER,
                        SemanticTokenType::VARIABLE,
                        SemanticTokenType::PROPERTY,
                        SemanticTokenType::FUNCTION,
                        SemanticTokenType::METHOD,
                        SemanticTokenType::KEYWORD,
                        SemanticTokenType::COMMENT,
                        SemanticTokenType::STRING,
                        SemanticTokenType::NUMBER,
                    ],
                    token_modifiers: vec![
                        SemanticTokenModifier::DECLARATION,
                        SemanticTokenModifier::READONLY,
                        SemanticTokenModifier::STATIC,
                        SemanticTokenModifier::ABSTRACT,
                        SemanticTokenModifier::DEPRECATED,
                        SemanticTokenModifier::MODIFICATION,
                    ],
                },
                range: Some(true),
                full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                ..Default::default()
            },
        )),
        inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
            InlayHintOptions {
                resolve_provider: Some(true),
                work_done_progress_options: WorkDoneProgressOptions::default(),
            },
        ))),
        document_on_type_formatting_provider: Some(DocumentOnTypeFormattingOptions {
            first_trigger_character: "d".to_string(),
            more_trigger_character: Some(vec![]),
        }),
        folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
        ..Default::default()
    })
    .unwrap();

    let initialization_params = connection.initialize(server_capabilities)?;
    let _params: InitializeParams = serde_json::from_value(initialization_params)?;

    main_loop(connection, _params)?;

    io_threads.join()?;

    Ok(())
}

fn main_loop(connection: Connection, params: InitializeParams) -> Result<()> {
    #[allow(deprecated)]
    let workspace_root = params
        .root_uri
        .and_then(|uri| uri.as_str().strip_prefix("file://").map(PathBuf::from))
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

    tracing::info!("LSP workspace root: {:?}", workspace_root);

    let fs = Arc::new(RealFileSystem);
    let compiler_options = CompilerOptions::default();
    let module_config = ModuleConfig::from_compiler_options(&compiler_options, &workspace_root);

    let module_registry = Arc::new(ModuleRegistry::new());
    let module_resolver = Arc::new(ModuleResolver::new(
        fs,
        module_config,
        workspace_root.clone(),
    ));

    let mut document_manager =
        DocumentManager::new(workspace_root, module_registry, module_resolver);
    let mut message_handler = MessageHandler::new();
    let connection_wrapper = ConnectionWrapper(&connection);

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                message_handler.handle_request(&connection_wrapper, req, &document_manager)?;
            }
            Message::Notification(not) => {
                message_handler.handle_notification(
                    &connection_wrapper,
                    not,
                    &mut document_manager,
                )?;
            }
            Message::Response(_resp) => {}
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_server::{Request, RequestId};
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
        fn send_response(&self, _response: Response) -> Result<()> {
            self.notification_count.fetch_add(1, Ordering::SeqCst);
            Ok(())
        }

        fn send_notification(&self, _notification: Notification) -> Result<()> {
            self.response_count.fetch_add(1, Ordering::SeqCst);
            Ok(())
        }
    }

    #[test]
    fn test_mock_connection_new() {
        let (conn, not_count, resp_count) = MockConnection::new();
        assert_eq!(conn.notification_count.load(Ordering::SeqCst), 0);
        assert_eq!(conn.response_count.load(Ordering::SeqCst), 0);
        assert_eq!(not_count.load(Ordering::SeqCst), 0);
        assert_eq!(resp_count.load(Ordering::SeqCst), 0);
    }

    #[test]
    fn test_mock_connection_send_response() {
        let (conn, _, _) = MockConnection::new();
        let response = Response {
            id: RequestId::from(1),
            result: Some(serde_json::json!({"test": true})),
            error: None,
        };
        let result = conn.send_response(response);
        assert!(result.is_ok());
    }

    #[test]
    fn test_mock_connection_send_notification() {
        let (conn, _, _) = MockConnection::new();
        let notification = Notification::new("test".to_string(), serde_json::json!({"data": 42}));
        let result = conn.send_notification(notification);
        assert!(result.is_ok());
    }
}
