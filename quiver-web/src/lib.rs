mod repl_web;
mod types;
mod web_transport;
mod worker_entry;

// Re-export types for use in TypeScript
pub use types::*;

// Re-export main API
pub use repl_web::Repl;
pub use worker_entry::worker_main;

use wasm_bindgen::prelude::*;

/// Returns the version of the quiver-web package
#[wasm_bindgen]
pub fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}
