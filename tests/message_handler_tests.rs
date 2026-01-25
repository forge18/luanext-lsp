use lsp_server::{Notification, Request, RequestId, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
    Notification as NotificationTrait, PublishDiagnostics,
};
use lsp_types::request::{
    CodeActionRequest, Completion, DocumentSymbolRequest, FoldingRangeRequest, Formatting,
    GotoDefinition, HoverRequest, InlayHintRequest, PrepareRenameRequest, RangeFormatting,
    References, Rename, Request as RequestTrait, SelectionRangeRequest,
    SemanticTokensFullDeltaRequest, SemanticTokensFullRequest, SemanticTokensRangeRequest,
    SignatureHelpRequest, WorkspaceSymbolRequest,
};
use lsp_types::*;
use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;
use typedlua_lsp::document::DocumentManager;
use typedlua_lsp::message_handler::{LspConnection, MessageHandler};

// Mock connection that captures sent messages for testing
#[derive(Clone)]
struct MockConnection {
    responses: Rc<RefCell<Vec<Response>>>,
    notifications: Rc<RefCell<Vec<Notification>>>,
}

impl MockConnection {
    fn new() -> Self {
        Self {
            responses: Rc::new(RefCell::new(Vec::new())),
            notifications: Rc::new(RefCell::new(Vec::new())),
        }
    }

    fn get_last_response(&self) -> Option<Response> {
        self.responses.borrow().last().cloned()
    }

    fn get_last_notification(&self) -> Option<Notification> {
        self.notifications.borrow().last().cloned()
    }

    fn response_count(&self) -> usize {
        self.responses.borrow().len()
    }

    fn notification_count(&self) -> usize {
        self.notifications.borrow().len()
    }
}

impl LspConnection for MockConnection {
    fn send_response(&self, response: Response) -> anyhow::Result<()> {
        self.responses.borrow_mut().push(response);
        Ok(())
    }

    fn send_notification(&self, notification: Notification) -> anyhow::Result<()> {
        self.notifications.borrow_mut().push(notification);
        Ok(())
    }
}

fn create_test_uri() -> Uri {
    Uri::from_str("file:///test.lua").unwrap()
}

fn create_test_document(text: &str) -> (DocumentManager, Uri) {
    let mut manager = DocumentManager::new_test();
    let uri = create_test_uri();

    let open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "lua".to_string(),
            version: 1,
            text: text.to_string(),
        },
    };
    manager.open(open_params);

    (manager, uri)
}

#[test]
fn test_completion_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("local x = 1");

    let request = Request::new(
        RequestId::from(1),
        Completion::METHOD.to_string(),
        CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(0, 5),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: None,
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
    let response = connection.get_last_response().unwrap();
    assert_eq!(response.id, RequestId::from(1));
}

#[test]
fn test_hover_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test() end");

    let request = Request::new(
        RequestId::from(2),
        HoverRequest::METHOD.to_string(),
        HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(0, 10),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_goto_definition_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("local x = 1\nprint(x)");

    let request = Request::new(
        RequestId::from(3),
        GotoDefinition::METHOD.to_string(),
        GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(1, 6),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_references_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("local x = 1\nprint(x)");

    let request = Request::new(
        RequestId::from(4),
        References::METHOD.to_string(),
        ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(0, 6),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: ReferenceContext {
                include_declaration: true,
            },
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_prepare_rename_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("local myVar = 1");

    let request = Request::new(
        RequestId::from(5),
        PrepareRenameRequest::METHOD.to_string(),
        TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: Position::new(0, 8),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_rename_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("local myVar = 1");

    let request = Request::new(
        RequestId::from(6),
        Rename::METHOD.to_string(),
        RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(0, 8),
            },
            new_name: "newVar".to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_document_symbol_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) =
        create_test_document("function test() end\nclass MyClass end\ninterface ITest end");

    let request = Request::new(
        RequestId::from(7),
        DocumentSymbolRequest::METHOD.to_string(),
        DocumentSymbolParams {
            text_document: TextDocumentIdentifier { uri },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_workspace_symbol_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let doc_manager = DocumentManager::new_test();

    let request = Request::new(
        RequestId::from(8),
        WorkspaceSymbolRequest::METHOD.to_string(),
        WorkspaceSymbolParams {
            query: "test".to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_formatting_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test()end");

    let request = Request::new(
        RequestId::from(9),
        Formatting::METHOD.to_string(),
        DocumentFormattingParams {
            text_document: TextDocumentIdentifier { uri },
            options: FormattingOptions {
                tab_size: 4,
                insert_spaces: true,
                ..Default::default()
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_range_formatting_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test()end\nlocal x=1");

    let request = Request::new(
        RequestId::from(10),
        RangeFormatting::METHOD.to_string(),
        DocumentRangeFormattingParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range::new(Position::new(0, 0), Position::new(1, 0)),
            options: FormattingOptions {
                tab_size: 4,
                insert_spaces: true,
                ..Default::default()
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_code_action_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("local x = 1");

    let request = Request::new(
        RequestId::from(11),
        CodeActionRequest::METHOD.to_string(),
        CodeActionParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range::new(Position::new(0, 0), Position::new(0, 11)),
            context: CodeActionContext {
                diagnostics: vec![],
                only: None,
                trigger_kind: None,
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_signature_help_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test(a, b) end\ntest(");

    let request = Request::new(
        RequestId::from(12),
        SignatureHelpRequest::METHOD.to_string(),
        SignatureHelpParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(1, 5),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            context: None,
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_inlay_hint_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test(param) end");

    let request = Request::new(
        RequestId::from(13),
        InlayHintRequest::METHOD.to_string(),
        InlayHintParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range::new(Position::new(0, 0), Position::new(0, 24)),
            work_done_progress_params: WorkDoneProgressParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_selection_range_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test() local x = 1 end");

    let request = Request::new(
        RequestId::from(14),
        SelectionRangeRequest::METHOD.to_string(),
        SelectionRangeParams {
            text_document: TextDocumentIdentifier { uri },
            positions: vec![Position::new(0, 15)],
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_folding_range_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test()\n    local x = 1\nend");

    let request = Request::new(
        RequestId::from(15),
        FoldingRangeRequest::METHOD.to_string(),
        FoldingRangeParams {
            text_document: TextDocumentIdentifier { uri },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_semantic_tokens_full_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test() end");

    let request = Request::new(
        RequestId::from(16),
        SemanticTokensFullRequest::METHOD.to_string(),
        SemanticTokensParams {
            text_document: TextDocumentIdentifier { uri },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_semantic_tokens_range_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test() end\nlocal x = 1");

    let request = Request::new(
        RequestId::from(17),
        SemanticTokensRangeRequest::METHOD.to_string(),
        SemanticTokensRangeParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range::new(Position::new(0, 0), Position::new(1, 0)),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_semantic_tokens_delta_request() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test() end");

    let request = Request::new(
        RequestId::from(18),
        SemanticTokensFullDeltaRequest::METHOD.to_string(),
        SemanticTokensDeltaParams {
            text_document: TextDocumentIdentifier { uri },
            previous_result_id: "test_id".to_string(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        },
    );

    handler
        .handle_request(&connection, request, &doc_manager)
        .unwrap();

    assert_eq!(connection.response_count(), 1);
}

#[test]
fn test_did_open_notification() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let mut doc_manager = DocumentManager::new_test();
    let uri = create_test_uri();

    let notification = Notification::new(
        DidOpenTextDocument::METHOD.to_string(),
        DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri,
                language_id: "lua".to_string(),
                version: 1,
                text: "local x = 1".to_string(),
            },
        },
    );

    handler
        .handle_notification(&connection, notification, &mut doc_manager)
        .unwrap();

    // Should publish diagnostics
    assert_eq!(connection.notification_count(), 1);
    let notification = connection.get_last_notification().unwrap();
    assert_eq!(notification.method, PublishDiagnostics::METHOD);
}

#[test]
fn test_did_change_notification() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (mut doc_manager, uri) = create_test_document("local x = 1");

    let notification = Notification::new(
        DidChangeTextDocument::METHOD.to_string(),
        DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier { uri, version: 2 },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "local y = 2".to_string(),
            }],
        },
    );

    handler
        .handle_notification(&connection, notification, &mut doc_manager)
        .unwrap();

    // Should publish diagnostics
    assert!(connection.notification_count() >= 1);
}

#[test]
fn test_did_save_notification() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (mut doc_manager, uri) = create_test_document("local x = 1");

    let notification = Notification::new(
        DidSaveTextDocument::METHOD.to_string(),
        DidSaveTextDocumentParams {
            text_document: TextDocumentIdentifier { uri },
            text: None,
        },
    );

    handler
        .handle_notification(&connection, notification, &mut doc_manager)
        .unwrap();

    // Should publish diagnostics
    assert!(connection.notification_count() >= 1);
}

#[test]
fn test_did_close_notification() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (mut doc_manager, uri) = create_test_document("local x = 1");

    let notification = Notification::new(
        DidCloseTextDocument::METHOD.to_string(),
        DidCloseTextDocumentParams {
            text_document: TextDocumentIdentifier { uri },
        },
    );

    handler
        .handle_notification(&connection, notification, &mut doc_manager)
        .unwrap();

    // Should clear diagnostics
    assert_eq!(connection.notification_count(), 1);
    let notification = connection.get_last_notification().unwrap();
    assert_eq!(notification.method, PublishDiagnostics::METHOD);
}

#[test]
fn test_multiple_requests_in_sequence() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let (doc_manager, uri) = create_test_document("function test() end");

    // Send multiple different requests
    let requests = vec![
        Request::new(
            RequestId::from(1),
            Completion::METHOD.to_string(),
            CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(0, 5),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            },
        ),
        Request::new(
            RequestId::from(2),
            HoverRequest::METHOD.to_string(),
            HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(0, 10),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            },
        ),
        Request::new(
            RequestId::from(3),
            DocumentSymbolRequest::METHOD.to_string(),
            DocumentSymbolParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            },
        ),
    ];

    for request in requests {
        handler
            .handle_request(&connection, request, &doc_manager)
            .unwrap();
    }

    assert_eq!(connection.response_count(), 3);
}

#[test]
fn test_notification_sequence() {
    let handler = MessageHandler::new();
    let connection = MockConnection::new();
    let mut doc_manager = DocumentManager::new_test();
    let uri = create_test_uri();

    // Open, change, save, close sequence
    let open_notif = Notification::new(
        DidOpenTextDocument::METHOD.to_string(),
        DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "lua".to_string(),
                version: 1,
                text: "local x = 1".to_string(),
            },
        },
    );

    let change_notif = Notification::new(
        DidChangeTextDocument::METHOD.to_string(),
        DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: 2,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "local y = 2".to_string(),
            }],
        },
    );

    let save_notif = Notification::new(
        DidSaveTextDocument::METHOD.to_string(),
        DidSaveTextDocumentParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            text: None,
        },
    );

    let close_notif = Notification::new(
        DidCloseTextDocument::METHOD.to_string(),
        DidCloseTextDocumentParams {
            text_document: TextDocumentIdentifier { uri },
        },
    );

    handler
        .handle_notification(&connection, open_notif, &mut doc_manager)
        .unwrap();
    handler
        .handle_notification(&connection, change_notif, &mut doc_manager)
        .unwrap();
    handler
        .handle_notification(&connection, save_notif, &mut doc_manager)
        .unwrap();
    handler
        .handle_notification(&connection, close_notif, &mut doc_manager)
        .unwrap();

    // Should have received 4 diagnostic notifications
    assert_eq!(connection.notification_count(), 4);
}
