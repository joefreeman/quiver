use quiver_core::effects::Effect;
use quiver_core::value::ResourceId;
use serde::{Deserialize, Serialize};

/// What a browser can ask its host to do.
///
/// Exactly one thing, because a browser's io floor is HTTP: it cannot open a socket, so the
/// whole exchange is the primitive rather than something built over one. Everything else the
/// web host offers — clocks, entropy — is a synchronous host read that never becomes an
/// effect at all.
///
/// Method and headers cross as bytes (a raw CRLF block) rather than as structured values: an
/// effect is serialized to JSON across the worker boundary, and the backend has no type
/// registry to rebuild tuples with. `%http` owns the header grammar on the Quiver side.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WebEffect {
    Fetch {
        method: Vec<u8>,
        url: Vec<u8>,
        headers: Vec<u8>,
        body: Vec<u8>,
    },
}

impl Effect for WebEffect {
    fn resource_id(&self) -> Option<ResourceId> {
        // Fetch creates the body stream rather than operating on one, so it needs no
        // ownership check. Reads of that stream go through `arm_stream`, not an effect.
        match self {
            WebEffect::Fetch { .. } => None,
        }
    }
}
