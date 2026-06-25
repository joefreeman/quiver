//! A no-op effect for the language server.
//!
//! The LSP type-checks but never executes code, so it needs a `BuiltinRegistry` (to
//! type-check builtin calls like `__integer_add__`) without depending on a real I/O backend.
//! `quiver-io`'s `NativeEffect` pulls in io-uring (Linux-only), which an editor-hosted
//! server can't require — so we supply our own trivial effect instead.

use quiver_core::effects::Effect;
use quiver_core::value::ResourceId;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NoEffect;

impl Effect for NoEffect {
    fn resource_id(&self) -> Option<ResourceId> {
        None
    }
}
