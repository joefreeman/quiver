use quiver_core::effects::Effect;
use quiver_core::value::ResourceId;
use serde::{Deserialize, Serialize};

/// Native platform effects (I/O, network, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NativeEffect {
    // File operations
    FileOpen {
        path: Vec<u8>,
        flags: i32,
        mode: u32,
    },
    FileRead {
        resource_id: ResourceId,
        offset: u64,
        length: usize,
    },
    FileWrite {
        resource_id: ResourceId,
        offset: u64,
        data: Vec<u8>,
    },
    FileFlush {
        resource_id: ResourceId,
    },
    FileClose {
        resource_id: ResourceId,
    },

    // Network operations
    TcpConnect {
        host: Vec<u8>,
        port: u16,
    },
    TcpListen {
        port: u16,
        backlog: i32,
    },
    TcpListenerAccept {
        resource_id: ResourceId,
    },
    TcpListenerClose {
        resource_id: ResourceId,
    },
    TcpSocketRead {
        resource_id: ResourceId,
        length: usize,
    },
    TcpSocketWrite {
        resource_id: ResourceId,
        data: Vec<u8>,
    },
    TcpSocketClose {
        resource_id: ResourceId,
    },
}

impl Effect for NativeEffect {
    fn resource_id(&self) -> Option<ResourceId> {
        match self {
            // Resource-creating effects
            NativeEffect::FileOpen { .. }
            | NativeEffect::TcpConnect { .. }
            | NativeEffect::TcpListen { .. } => None,

            // Resource-using effects
            NativeEffect::FileRead { resource_id, .. }
            | NativeEffect::FileWrite { resource_id, .. }
            | NativeEffect::FileFlush { resource_id }
            | NativeEffect::FileClose { resource_id }
            | NativeEffect::TcpListenerAccept { resource_id }
            | NativeEffect::TcpListenerClose { resource_id }
            | NativeEffect::TcpSocketRead { resource_id, .. }
            | NativeEffect::TcpSocketWrite { resource_id, .. }
            | NativeEffect::TcpSocketClose { resource_id } => Some(*resource_id),
        }
    }
}
