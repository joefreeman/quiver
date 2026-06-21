//! Quiver language server. Speaks LSP over stdio.

mod analysis;
mod backend;
mod convert;
mod diagnostics;
mod documents;
mod effect;
mod loader;
mod symbols;

use backend::Backend;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
