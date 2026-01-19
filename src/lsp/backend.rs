//! LSP backend implementation using tower-lsp.

use std::sync::Arc;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use super::analysis::Analyzer;
use super::document::DocumentManager;
use super::handlers::{
    get_legend, handle_completion, handle_goto_definition, handle_hover, handle_references,
    handle_semantic_tokens_full, publish_diagnostics,
};

/// The Dream language server.
pub struct DreamLanguageServer {
    /// LSP client for sending notifications
    client: Client,
    /// Document manager for open files
    documents: DocumentManager,
    /// Analyzer for parsing and type checking
    analyzer: Arc<Analyzer>,
}

impl DreamLanguageServer {
    /// Create a new language server.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DocumentManager::new(),
            analyzer: Arc::new(Analyzer::new()),
        }
    }

    /// Analyze a document and publish diagnostics.
    async fn analyze_and_publish(&self, uri: Url) {
        let doc = match self.documents.get(&uri) {
            Some(doc) => doc,
            None => return,
        };

        let result = self.analyzer.analyze(&doc.content, doc.path.as_deref());

        let diagnostics = publish_diagnostics(
            &doc.line_index,
            &result.parse_errors,
            &result.type_errors,
            &result.warnings,
        );

        self.client
            .publish_diagnostics(uri, diagnostics, Some(doc.version))
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for DreamLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![":".to_string(), ".".to_string()]),
                    ..Default::default()
                }),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: get_legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        },
                    ),
                ),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "dream-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Dream LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        let version = params.text_document.version;

        self.documents.open(uri.clone(), content, version);
        self.analyze_and_publish(uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        // With FULL sync, we get the entire document content
        if let Some(change) = params.content_changes.into_iter().last() {
            self.documents.change(&uri, change.text, version);
            self.analyze_and_publish(uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents.close(&uri);

        // Clear diagnostics for closed document
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let result = self.analyzer.analyze(&doc.content, doc.path.as_deref());
        let stdlib = self.analyzer.stdlib_modules();

        let hover = result
            .module
            .as_ref()
            .and_then(|module| handle_hover(module, &doc.line_index, position, stdlib));

        Ok(hover)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let result = self.analyzer.analyze(&doc.content, doc.path.as_deref());
        let stdlib = self.analyzer.stdlib_modules();

        let definition = result.module.as_ref().and_then(|module| {
            handle_goto_definition(module, &doc.line_index, position, uri, stdlib)
        });

        Ok(definition)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;

        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let result = self.analyzer.analyze(&doc.content, doc.path.as_deref());
        let stdlib = self.analyzer.stdlib_modules();

        let completions = result.module.as_ref().and_then(|module| {
            handle_completion(module, stdlib, &doc.line_index, &params, &doc.content)
        });

        Ok(completions)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let result = self.analyzer.analyze(&doc.content, doc.path.as_deref());

        let references = result.module.as_ref().and_then(|module| {
            handle_references(module, &doc.line_index, position, uri, include_declaration)
        });

        Ok(references)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        let doc = match self.documents.get(uri) {
            Some(doc) => doc,
            None => return Ok(None),
        };

        let result = self.analyzer.analyze(&doc.content, doc.path.as_deref());

        let tokens = result
            .module
            .as_ref()
            .and_then(|module| handle_semantic_tokens_full(module, &doc.line_index, &doc.content));

        Ok(tokens)
    }
}
