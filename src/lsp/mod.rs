//! Language Server Protocol (LSP) implementation for Dream.
//!
//! This module provides IDE features like diagnostics, hover, go-to-definition,
//! and completions through the standard LSP interface.

mod analysis;
mod backend;
mod document;
mod handlers;
mod position;

pub use backend::DreamLanguageServer;

use tokio::io::{stdin, stdout};
use tower_lsp::{LspService, Server};

/// Run the LSP server on stdin/stdout.
pub async fn run_server() {
    let (service, socket) = LspService::new(|client| DreamLanguageServer::new(client));
    Server::new(stdin(), stdout(), socket).serve(service).await;
}
