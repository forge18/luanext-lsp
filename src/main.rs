#![allow(clippy::all)]
#![allow(deprecated)]

mod document;
mod message_handler;
mod providers;
mod symbol_index;

use anyhow::Result;
use document::DocumentManager;
use lsp_server::{Connection, Message, Notification, Response};
use lsp_types::*;
use message_handler::{LspConnection, MessageHandler};
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;
use tracing_subscriber::EnvFilter;
use typedlua_core::config::CompilerOptions;
use typedlua_core::fs::RealFileSystem;
use typedlua_core::module_resolver::{ModuleConfig, ModuleRegistry, ModuleResolver};

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
    // Initialize tracing for LSP server
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive(tracing::Level::INFO.into()))
        .with_writer(std::io::stderr) // LSP uses stdout for protocol, log to stderr
        .init();

    // Create the LSP connection
    let (connection, io_threads) = Connection::stdio();

    // Server capabilities
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
            first_trigger_character: "d".to_string(), // Trigger when typing 'end'
            more_trigger_character: Some(vec![]),
        }),
        folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
        ..Default::default()
    })
    .unwrap();

    // Initialize the connection
    let initialization_params = connection.initialize(server_capabilities)?;
    let _params: InitializeParams = serde_json::from_value(initialization_params)?;

    // Run the main loop
    main_loop(connection, _params)?;

    // Wait for IO threads to finish
    io_threads.join()?;

    Ok(())
}

fn main_loop(connection: Connection, params: InitializeParams) -> Result<()> {
    // Get workspace root from initialization params
    #[allow(deprecated)] // root_uri is deprecated but still widely used
    let workspace_root = params
        .root_uri
        .and_then(|uri| uri.as_str().strip_prefix("file://").map(PathBuf::from))
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

    tracing::info!("LSP workspace root: {:?}", workspace_root);

    // Initialize module system infrastructure
    let fs = Arc::new(RealFileSystem);
    let compiler_options = CompilerOptions::default();
    let module_config = ModuleConfig::from_compiler_options(&compiler_options, &workspace_root);
    let module_registry = Arc::new(ModuleRegistry::new());
    let module_resolver = Arc::new(ModuleResolver::new(
        fs,
        module_config,
        workspace_root.clone(),
    ));

    // Create document manager with module system support
    let mut document_manager =
        DocumentManager::new(workspace_root, module_registry, module_resolver);
    let message_handler = MessageHandler::new();
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
            Message::Response(_resp) => {
                // Client responses to our requests - we don't currently send any
            }
        }
    }

    Ok(())
}
