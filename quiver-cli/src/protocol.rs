//! The client protocol: an HTTP API over a unix socket whose sole resource is the
//! process. The server is a process host — the only code-bearing verb is `resume`,
//! which carries compiled bytecode; results come back as rendered text (data notation
//! for data values). Compilation, session state and display policy are client
//! concerns.
//!
//! Compatibility is fingerprint-checked per request (the [`FINGERPRINT_HEADER`]):
//! a server built from different compiler sources answers 426, since bytecode
//! shapes, artifacts and std are all fingerprint-scoped. `GET /status` and
//! `POST /shutdown` are exempt — which is what lets a newer client discover and
//! stop a stale server (takeover).
//!
//! ```text
//! GET    /status                        → StatusResponse
//! POST   /processes                     → CreateResponse     (pure allocation)
//! POST   /processes/{id}/resume         ResumeRequest → Outcome; 409 while busy,
//!                                       424 + MissingModules when a named module is
//!                                       neither held nor attached
//! POST   /processes/{id}/compact        CompactRequest
//! POST   /processes/{id}/cancel
//! DELETE /processes/{id}                stop + ownership cascade
//! GET    /processes[/{id}]              text/plain inspection
//! GET    /workers[/{id}]                text/plain inspection
//! POST   /shutdown
//! ```

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

pub const PROTOCOL_VERSION: u32 = 2;

/// The compiler-fingerprint header checked on every request except `/status` and
/// `/shutdown`.
pub const FINGERPRINT_HEADER: &str = "x-quiver-fingerprint";

/// Where the server listens: `$XDG_RUNTIME_DIR/quiv` (tmpfs, per-user) or
/// `/tmp/quiv-<uid>`. The directory is created mode 0700 — socket reachability is the
/// authentication model, which is also why the endpoint is a unix socket and never
/// TCP: it evaluates arbitrary bytecode.
pub fn default_socket_dir() -> PathBuf {
    match std::env::var_os("XDG_RUNTIME_DIR") {
        Some(dir) => PathBuf::from(dir).join("quiv"),
        // SAFETY-free: getuid never fails.
        None => PathBuf::from(format!("/tmp/quiv-{}", unsafe { libc::getuid() })),
    }
}

/// The socket clients and `quiv server` agree on: `QUIV_SOCKET` when set (isolated
/// test/CI environments), the per-user default otherwise.
pub fn default_socket_path() -> PathBuf {
    match std::env::var_os("QUIV_SOCKET") {
        Some(path) => PathBuf::from(path),
        None => default_socket_dir().join("server.sock"),
    }
}

/// The pidfile beside the socket: the SIGTERM fallback for stopping a server too old
/// to speak the protocol.
pub fn pidfile_path(socket: &std::path::Path) -> PathBuf {
    socket.with_extension("pid")
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StatusResponse {
    pub protocol_version: u32,
    pub fingerprint: String,
    pub pid: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateResponse {
    pub id: u64,
}

/// What a resume hands the server to run: the line's own relocatable code, plus the
/// units of any modules it imports that this client has not already sent to this
/// server — dependency-first, in link order. The server links each once and the line
/// names them by key thereafter. A module the client could not supply is already
/// inlined into the unit, so the payload is complete by construction.
#[derive(Debug, Serialize, Deserialize)]
pub struct ResumePayload {
    pub unit: quiver_compiler::CompiledUnit,
    pub modules: Vec<(u64, quiver_compiler::CompiledUnit)>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ResumeRequest {
    pub payload: ResumePayload,
    /// Local slots to keep when the result is delivered (a REPL line's bindings);
    /// `None` keeps everything.
    #[serde(default)]
    pub keep: Option<Vec<usize>>,
}

/// The `424` body: module keys the unit names that the server does not hold and the
/// request did not carry — the client's record of what it has sent is stale, most
/// likely because a code sweep reclaimed a module nothing was using. Resend those
/// modules and retry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MissingModules {
    pub missing: Vec<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompactRequest {
    pub keep: Vec<usize>,
}

/// What a resume answered.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Outcome {
    /// A value: its rendered form (data notation for data values); its rendered type
    /// where the REPL shows one (functions, builtins, pids); its failure-provenance
    /// origin for stamped nils; and whether it was nil (a run's exit code).
    Value {
        rendered: String,
        type_rendered: Option<String>,
        origin: Option<String>,
        is_nil: bool,
    },
    /// The process crashed evaluating this resume.
    Error { message: String },
    /// A cancel stopped the process before the resume completed.
    Interrupted,
}
