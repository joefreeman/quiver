use quiver_core::effects::Effect;
use quiver_core::value::ResourceId;
use serde::{Deserialize, Serialize};

/// Native platform effects (I/O, network, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NativeEffect {
    // File operations
    OpenFile {
        path: Vec<u8>,
        flags: i32,
        mode: u32,
        buffer_size: usize,
    },

    // Network operations
    TcpConnect {
        host: Vec<u8>,
        port: u16,
        buffer_size: usize,
    },
    TcpListen {
        port: u16,
        backlog: i32,
    },
    Accept {
        resource_id: ResourceId,
    },

    // Resource operations
    Read {
        resource_id: ResourceId,
    },
    Write {
        resource_id: ResourceId,
        data: Vec<u8>,
    },
    Flush {
        resource_id: ResourceId,
    },
    Close {
        resource_id: ResourceId,
    },
}

impl Effect for NativeEffect {
    fn resource_id(&self) -> Option<ResourceId> {
        match self {
            // Resource-creating effects
            NativeEffect::OpenFile { .. }
            | NativeEffect::TcpConnect { .. }
            | NativeEffect::TcpListen { .. } => None,

            // Resource-using effects
            NativeEffect::Accept { resource_id }
            | NativeEffect::Read { resource_id }
            | NativeEffect::Write { resource_id, .. }
            | NativeEffect::Flush { resource_id }
            | NativeEffect::Close { resource_id } => Some(*resource_id),
        }
    }
}
