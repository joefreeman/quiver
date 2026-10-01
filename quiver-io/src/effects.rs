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

    // Filesystem metadata
    Stat {
        path: Vec<u8>,
        /// `stat` when set, `lstat` (describe a symlink itself) otherwise.
        follow: bool,
    },

    // Filesystem queries and mutation. Each runs on the blocking pool; path arguments are
    // the raw bytes the OS is handed.
    CreateDir {
        path: Vec<u8>,
        /// Create missing parents too, and accept an existing directory (`mkdir -p`).
        all: bool,
    },
    Remove {
        path: Vec<u8>,
        /// Remove a directory's contents first. Never follows symlinks.
        recursive: bool,
    },
    Rename {
        from: Vec<u8>,
        to: Vec<u8>,
    },
    Copy {
        from: Vec<u8>,
        to: Vec<u8>,
        /// Overwrite an existing destination rather than failing with `AlreadyExists`.
        replace: bool,
    },
    Symlink {
        link: Vec<u8>,
        target: Vec<u8>,
    },
    ReadLink {
        path: Vec<u8>,
    },
    SetPerm {
        path: Vec<u8>,
        perm: u32,
    },
    Canonical {
        path: Vec<u8>,
    },
    Cwd,
    Temp,

    // Directory operations
    ReadDirOpen {
        path: Vec<u8>,
    },
    ReadDirNext {
        resource_id: ResourceId,
    },

    // DNS operations
    DnsResolve {
        hostname: Vec<u8>,
    },
    DnsNext {
        resource_id: ResourceId,
    },

    // Network operations
    TcpConnect {
        ip: Vec<u8>,
        port: u16,
    },
    TcpListen {
        port: u16,
        backlog: i32,
    },

    // Byte I/O: the operations every byte resource shares (`__io_write__`, `__io_close__`),
    // dispatched on the resource's kind by the backend.
    /// Write every byte of `data`, finishing any short write before answering `Ok`.
    Write {
        resource_id: ResourceId,
        data: Vec<u8>,
    },
    /// Close any resource.
    Close {
        resource_id: ResourceId,
    },

    // TLS upgrades a connected socket *in place* (client and server side respectively) —
    // encryption becomes a property of the socket, and the ordinary socket operations then
    // speak plaintext through it. There are no TLS read/write/close effects for that reason.
    TlsAttach {
        resource_id: ResourceId,
        hostname: Vec<u8>,
        roots: Vec<u8>,
    },
    TlsAccept {
        resource_id: ResourceId,
        cert: Vec<u8>,
        key: Vec<u8>,
    },

    /// A whole HTTP exchange as one effect — the native backing of `__http_request__`,
    /// driven by the backend over its own socket (see `native_backend`'s HTTP section).
    HttpRequest {
        method: Vec<u8>,
        url: Vec<u8>,
        headers: Vec<u8>,
        body: Vec<u8>,
    },
}

impl Effect for NativeEffect {
    fn resource_id(&self) -> Option<ResourceId> {
        match self {
            // Resource-creating effects
            NativeEffect::FileOpen { .. }
            | NativeEffect::Stat { .. }
            | NativeEffect::CreateDir { .. }
            | NativeEffect::Remove { .. }
            | NativeEffect::Rename { .. }
            | NativeEffect::Copy { .. }
            | NativeEffect::Symlink { .. }
            | NativeEffect::ReadLink { .. }
            | NativeEffect::SetPerm { .. }
            | NativeEffect::Canonical { .. }
            | NativeEffect::Cwd
            | NativeEffect::Temp
            | NativeEffect::ReadDirOpen { .. }
            | NativeEffect::DnsResolve { .. }
            | NativeEffect::TcpConnect { .. }
            | NativeEffect::TcpListen { .. }
            | NativeEffect::HttpRequest { .. } => None,

            // Resource-using effects
            NativeEffect::FileRead { resource_id, .. }
            | NativeEffect::FileWrite { resource_id, .. }
            | NativeEffect::FileFlush { resource_id }
            | NativeEffect::ReadDirNext { resource_id }
            | NativeEffect::DnsNext { resource_id }
            | NativeEffect::Write { resource_id, .. }
            | NativeEffect::Close { resource_id }
            | NativeEffect::TlsAttach { resource_id, .. }
            | NativeEffect::TlsAccept { resource_id, .. } => Some(*resource_id),
        }
    }
}
