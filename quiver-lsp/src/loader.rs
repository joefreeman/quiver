//! Module resolution for the language server.
//!
//! The Quiver standard library is bundled into `quiver-compiler` and exposed through the
//! built-in `std` package, so `%num`, `%list`, etc. always resolve. A document that lives on
//! disk additionally gets its project's own modules: the resolver discovers the nearest
//! `quiver.toml` and reads filesystem packages through the editor `overlay`, so imports resolve
//! to unsaved buffer content rather than stale on-disk source.

use quiver_compiler::{Overlay, PackageResolver};
use std::sync::Arc;
use tower_lsp::lsp_types::Url;

/// Build a resolver for the document at `uri`. File-backed documents resolve their project's
/// modules (via the nearest `quiver.toml`) through `overlay`; documents with no file path
/// (untitled buffers) fall back to the standard library only.
pub fn resolver_for(uri: &Url, overlay: Arc<Overlay>) -> PackageResolver {
    match uri.to_file_path() {
        Ok(path) => PackageResolver::for_entry_file_with_overlay(&path, overlay),
        Err(()) => PackageResolver::inline(),
    }
}
