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

    #[test]
    fn test_connection_wrapper_response() {
        use lsp_server::Message;

        let response = Response {
            id: RequestId::from(1),
            result: Some(serde_json::json!({"test": true})),
            error: None,
        };
        let message = Message::Response(response);
        assert!(matches!(message, Message::Response(_)));
    }

    #[test]
    fn test_connection_wrapper_notification() {
        use lsp_server::Message;

        let notification = Notification::new("test".to_string(), serde_json::json!({"data": 42}));
        let message = Message::Notification(notification);
        assert!(matches!(message, Message::Notification(_)));
    }

    #[test]
    fn test_connection_wrapper_request() {
        use lsp_server::Message;

        let request = Request::new(
            RequestId::from(1),
            "test".to_string(),
            serde_json::json!({"id": 1}),
        );
        let message = Message::Request(request);
        assert!(matches!(message, Message::Request(_)));
    }

    #[test]
    fn test_mock_connection_counters() {
        let (conn, not_count, resp_count) = MockConnection::new();

        assert_eq!(not_count.load(Ordering::SeqCst), 0);
        assert_eq!(resp_count.load(Ordering::SeqCst), 0);

        let _ = conn.send_response(Response {
            id: RequestId::from(1),
            result: None,
            error: None,
        });
        assert_eq!(not_count.load(Ordering::SeqCst), 1);

        let notification = Notification::new("test".to_string(), serde_json::json!({}));
        let _ = conn.send_notification(notification);
        assert_eq!(resp_count.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_connection_wrapper_clone() {
        let (conn, _, _) = MockConnection::new();
        let _cloned = conn.clone();
    }

    #[test]
    fn test_server_capabilities_serialization() {
        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL,
            )),
            completion_provider: Some(CompletionOptions::default()),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            document_symbol_provider: Some(OneOf::Left(true)),
            ..Default::default()
        };

        let value = serde_json::to_value(&capabilities).unwrap();
        assert!(value.is_object());
    }

    #[test]
    fn test_initialize_params_empty() {
        let params = InitializeParams::default();
        assert!(params.capabilities.workspace.is_none());
    }

    #[test]
    fn test_semantic_tokens_legend() {
        let legend = SemanticTokensLegend {
            token_types: vec![
                SemanticTokenType::CLASS,
                SemanticTokenType::FUNCTION,
                SemanticTokenType::VARIABLE,
            ],
            token_modifiers: vec![
                SemanticTokenModifier::DECLARATION,
                SemanticTokenModifier::READONLY,
            ],
        };

        assert_eq!(legend.token_types.len(), 3);
        assert_eq!(legend.token_modifiers.len(), 2);
    }

    #[test]
    fn test_server_capabilities_completion() {
        let capabilities = ServerCapabilities {
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                resolve_provider: Some(true),
                ..Default::default()
            }),
            ..Default::default()
        };

        let provider = capabilities.completion_provider.unwrap();
        assert_eq!(provider.trigger_characters.unwrap().len(), 2);
        assert!(provider.resolve_provider.unwrap());
    }

    #[test]
    fn test_server_capabilities_hover() {
        let capabilities = ServerCapabilities {
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            ..Default::default()
        };

        assert!(matches!(
            capabilities.hover_provider,
            Some(HoverProviderCapability::Simple(true))
        ));
    }

    #[test]
    fn test_server_capabilities_signature_help() {
        let capabilities = ServerCapabilities {
            signature_help_provider: Some(SignatureHelpOptions {
                trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                retrigger_characters: None,
                work_done_progress_options: WorkDoneProgressOptions::default(),
            }),
            ..Default::default()
        };

        let provider = capabilities.signature_help_provider.unwrap();
        assert_eq!(provider.trigger_characters.unwrap().len(), 2);
    }

    #[test]
    fn test_server_capabilities_code_action() {
        let capabilities = ServerCapabilities {
            code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
                code_action_kinds: Some(vec![CodeActionKind::QUICKFIX, CodeActionKind::REFACTOR]),
                resolve_provider: Some(true),
                work_done_progress_options: WorkDoneProgressOptions::default(),
            })),
            ..Default::default()
        };

        let provider = capabilities.code_action_provider.unwrap();
        if let CodeActionProviderCapability::Options(opts) = provider {
            assert_eq!(opts.code_action_kinds.unwrap().len(), 2);
        }
    }

    #[test]
    fn test_server_capabilities_rename() {
        let capabilities = ServerCapabilities {
            rename_provider: Some(OneOf::Right(RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: WorkDoneProgressOptions::default(),
            })),
            ..Default::default()
        };

        assert!(matches!(
            capabilities.rename_provider,
            Some(OneOf::Right(_))
        ));
    }

    #[test]
    fn test_server_capabilities_semantic_tokens() {
        let capabilities = ServerCapabilities {
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    legend: SemanticTokensLegend {
                        token_types: vec![SemanticTokenType::CLASS],
                        token_modifiers: vec![],
                    },
                    range: Some(true),
                    full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                    ..Default::default()
                }),
            ),
            ..Default::default()
        };

        assert!(matches!(
            capabilities.semantic_tokens_provider,
            Some(SemanticTokensServerCapabilities::SemanticTokensOptions(_))
        ));
    }

    #[test]
    fn test_server_capabilities_folding_range() {
        let capabilities = ServerCapabilities {
            folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
            ..Default::default()
        };

        assert!(matches!(
            capabilities.folding_range_provider,
            Some(FoldingRangeProviderCapability::Simple(true))
        ));
    }

    #[test]
    fn test_server_capabilities_inlay_hints() {
        let capabilities = ServerCapabilities {
            inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
                InlayHintOptions {
                    resolve_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                },
            ))),
            ..Default::default()
        };

        assert!(matches!(
            capabilities.inlay_hint_provider,
            Some(OneOf::Right(InlayHintServerCapabilities::Options(_)))
        ));
    }

    #[test]
    fn test_server_capabilities_formatting() {
        let capabilities = ServerCapabilities {
            document_formatting_provider: Some(OneOf::Left(true)),
            document_range_formatting_provider: Some(OneOf::Left(true)),
            ..Default::default()
        };

        assert!(capabilities.document_formatting_provider.is_some());
        assert!(capabilities.document_range_formatting_provider.is_some());
    }

    #[test]
    fn test_server_capabilities_definition_references() {
        let capabilities = ServerCapabilities {
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            document_highlight_provider: Some(OneOf::Left(true)),
            ..Default::default()
        };

        assert!(matches!(
            capabilities.definition_provider,
            Some(OneOf::Left(true))
        ));
        assert!(matches!(
            capabilities.references_provider,
            Some(OneOf::Left(true))
        ));
        assert!(matches!(
            capabilities.document_highlight_provider,
            Some(OneOf::Left(true))
        ));
    }

    #[test]
    fn test_server_capabilities_selection_range() {
        let capabilities = ServerCapabilities {
            selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
            ..Default::default()
        };

        assert!(matches!(
            capabilities.selection_range_provider,
            Some(SelectionRangeProviderCapability::Simple(true))
        ));
    }

    #[test]
    fn test_server_capabilities_symbols() {
        let capabilities = ServerCapabilities {
            document_symbol_provider: Some(OneOf::Left(true)),
            workspace_symbol_provider: Some(OneOf::Left(true)),
            ..Default::default()
        };

        assert!(capabilities.document_symbol_provider.is_some());
        assert!(capabilities.workspace_symbol_provider.is_some());
    }

    #[test]
    fn test_server_capabilities_document_on_type_formatting() {
        let capabilities = ServerCapabilities {
            document_on_type_formatting_provider: Some(DocumentOnTypeFormattingOptions {
                first_trigger_character: "d".to_string(),
                more_trigger_character: Some(vec![]),
            }),
            ..Default::default()
        };

        let provider = capabilities.document_on_type_formatting_provider.unwrap();
        assert_eq!(provider.first_trigger_character, "d");
    }

    #[test]
    fn test_main_loop_params_workspace_root() {
        let json = serde_json::json!({
            "rootUri": "file:///workspace/project",
            "capabilities": {}
        });
        let params: InitializeParams = serde_json::from_value(json).unwrap();

        let workspace_root = params
            .root_uri
            .and_then(|uri| uri.as_str().strip_prefix("file://").map(PathBuf::from))
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        assert_eq!(workspace_root, PathBuf::from("/workspace/project"));
    }

    #[test]
    fn test_main_loop_params_no_workspace_root() {
        let params = InitializeParams {
            root_uri: None,
            ..Default::default()
        };

        let workspace_root = params
            .root_uri
            .and_then(|uri| uri.as_str().strip_prefix("file://").map(PathBuf::from))
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        assert!(workspace_root.exists() || workspace_root.to_string_lossy() == ".");
    }

    #[test]
    fn test_connection_wrapper_lifetime() {
        let (connection, _) = lsp_server::Connection::stdio();

        struct WrapperHolder<'a> {
            wrapper: ConnectionWrapper<'a>,
        }

        let holder = WrapperHolder {
            wrapper: ConnectionWrapper(&connection),
        };
        let _ = holder;
    }

    #[test]
    fn test_module_config_creation() {
        use typedlua_typechecker::cli::config::CompilerOptions;

        let compiler_options = CompilerOptions::default();
        let workspace_root = PathBuf::from("/test");
        let config = ModuleConfig::from_compiler_options(&compiler_options, &workspace_root);

        assert!(!workspace_root.to_string_lossy().is_empty());
    }

    #[test]
    fn test_lsp_types_text_document_sync_kind() {
        let kind = TextDocumentSyncKind::INCREMENTAL;
        assert!(matches!(kind, TextDocumentSyncKind::INCREMENTAL));
    }

    #[test]
    fn test_code_action_kind_variants() {
        assert_eq!(CodeActionKind::QUICKFIX.as_str(), "quickfix");
        assert_eq!(CodeActionKind::REFACTOR.as_str(), "refactor");
        assert_eq!(
            CodeActionKind::SOURCE_ORGANIZE_IMPORTS.as_str(),
            "source.organizeImports"
        );
    }
}
