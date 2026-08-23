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
//! POST   /processes[?lease_ms=N]        → CreateResponse     (pure allocation)
//! POST   /processes/{id}/resume         ResumeRequest → Outcome; 409 while busy,
//!                                       424 + MissingModules when a named module is
//!                                       neither held nor attached
//! POST   /processes/{id}/compact        CompactRequest
//! POST   /processes/{id}/cancel
//! POST   /processes/{id}/heartbeat      renew a lease
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
/// authentication model, and the endpoint evaluates arbitrary bytecode, so this is
/// never plain TCP. The one exception is deliberate and opt-in: `quiv server --listen`
/// adds a **loopback-only** TCP listener for browser clients (the web REPL compiling
/// in wasm, executing here), where every request — `/status` and `/shutdown` included,
/// so a hostile page cannot even probe — must carry `Authorization: Bearer <token>`,
/// the token living beside the socket ([`token_path`], mode 0600) and persisting
/// across restarts so a rebuilt server does not strand connected browsers. CORS is
/// answered only for allowed origins, and non-loopback binds are refused outright.
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

/// Ready the directory holding the socket: create it 0700 when it is absent, and when
/// it is already there, check it is still this user's alone.
///
/// Reachability of the socket *is* the authentication model for an endpoint that
/// evaluates arbitrary bytecode, and everything beside it — the token, the pidfile, the
/// log — inherits the directory's protection. Creating it locked down is not enough on
/// its own: the `/tmp/quiv-<uid>` fallback ([`default_socket_dir`], taken whenever
/// `XDG_RUNTIME_DIR` is unset, as under cron, a plain ssh session or a container) can be
/// pre-created world-writable by anyone, and a directory another user owns is refused
/// rather than served from. Only that per-user default is vouched for: an explicit
/// `--socket` elsewhere is the operator's own arrangement.
pub fn prepare_socket_dir(socket: &std::path::Path) -> std::io::Result<()> {
    use std::os::unix::fs::{MetadataExt as _, PermissionsExt as _};
    let Some(dir) = socket.parent() else {
        return Ok(());
    };
    if !dir.exists() {
        std::fs::create_dir_all(dir)?;
        return std::fs::set_permissions(dir, std::fs::Permissions::from_mode(0o700));
    }
    if dir != default_socket_dir() {
        return Ok(());
    }
    let metadata = std::fs::symlink_metadata(dir)?;
    // SAFETY-free: geteuid never fails.
    let euid = unsafe { libc::geteuid() };
    if !metadata.is_dir() || metadata.uid() != euid {
        return Err(std::io::Error::other(format!(
            "{} is not a directory owned by this user (uid {})",
            dir.display(),
            metadata.uid()
        )));
    }
    // Ours, but reachable by others: put it back the way it is created. Everything in
    // it is this user's, so tightening the mode can lose nobody anything.
    if metadata.mode() & 0o077 != 0 {
        std::fs::set_permissions(dir, std::fs::Permissions::from_mode(0o700))?;
    }
    Ok(())
}

/// The pidfile beside the socket: the SIGTERM fallback for stopping a server too old
/// to speak the protocol.
pub fn pidfile_path(socket: &std::path::Path) -> PathBuf {
    socket.with_extension("pid")
}

/// The bearer token for the TCP listener, beside the socket (created mode 0600,
/// reused across restarts).
pub fn token_path(socket: &std::path::Path) -> PathBuf {
    socket.with_extension("token")
}

/// Where a listening server records its bound TCP endpoint (`http://127.0.0.1:<port>`),
/// beside the socket — how tooling (and tests) discover the port when `--listen` bound
/// port 0. Removed on shutdown.
pub fn http_endpoint_path(socket: &std::path::Path) -> PathBuf {
    socket.with_extension("http")
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

/// `POST /processes` — a root's **lease**, in milliseconds. A leased root must be kept
/// alive by its client (any request naming it renews the lease, and
/// `POST …/heartbeat` renews nothing else), and the server stops one whose lease has
/// run out. This is how an abruptly-killed client's root is reclaimed: the transport is
/// one short-lived connection per request, so nothing else tells the server that the
/// process on the other end is gone, and a root left running holds its subtree and
/// whatever resources it owns — a bound listening port among them.
///
/// Omitted means no lease: the root lives until an explicit `DELETE`, which is what a
/// client that cannot beat (a browser tab, a one-shot script) gets.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CreateParams {
    pub lease_ms: Option<u64>,
}

/// The lease a `quiv` client takes on its root, and how often it beats: often enough
/// that a beat lost to a busy machine is not fatal, rare enough to be invisible beside
/// the traffic a session already makes.
pub const CLIENT_LEASE: std::time::Duration = std::time::Duration::from_secs(30);
pub const CLIENT_HEARTBEAT: std::time::Duration = std::time::Duration::from_secs(8);

/// What a resume hands the server to run: the line's own relocatable code, plus the
/// units of any modules it imports that this client has not already sent to this
/// server — dependency-first, in link order. The server links each once and the line
/// names them by key thereafter. A module the client could not supply is already
/// inlined into the unit, so the payload is complete by construction. The shape is the
/// shared [`quiver_environment::WirePayload`] — the same payload every remote driver
/// speaks.
pub type ResumePayload = quiver_environment::WirePayload;

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
    pub missing: Vec<quiver_compiler::UnitKey>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompactRequest {
    pub keep: Vec<usize>,
}

/// `GET /events` — the realtime inspection stream (SSE). The query names the interest
/// set, one connection carries everything it names, and closing the connection is the
/// unsubscribe: no control protocol, and a client whose interest changes (a different
/// panel, a different selected process) simply reconnects with new parameters. That is
/// safe by construction — every subscription pushes an initial snapshot and updates
/// are last-wins, so a reconnect starts complete and missed intermediates are
/// meaningless. One connection per client also respects the browser's per-origin
/// connection budget, which per-subscription streams would spend.
///
/// Events are named `processes`, `workers` and `process`; each `data:` line is the
/// JSON of the corresponding `*Event` type below.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EventsParams {
    /// Stream every process's status (`event: processes`).
    #[serde(default)]
    pub processes: bool,
    /// Stream worker executor snapshots (`event: workers`).
    #[serde(default)]
    pub workers: bool,
    /// Stream one process's detail (`event: process`).
    pub process: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessesEvent {
    pub processes: Vec<ProcessSummary>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessSummary {
    pub id: u64,
    pub status: quiver_core::process::ProcessStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkersEvent {
    pub workers: Vec<quiver_core::process::WorkerInfo>,
}

/// The selected process's detail, rendered server-side: its type and result value
/// only mean something next to the session program, which never leaves the server.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessEvent {
    pub process: Option<ProcessDetail>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessDetail {
    pub id: u64,
    pub status: quiver_core::process::ProcessStatus,
    pub process_type: Option<String>,
    pub stack_size: usize,
    pub locals_count: usize,
    pub frames_count: usize,
    pub mailbox_size: usize,
    pub persistent: bool,
    pub result: Option<Outcome>,
    pub heap: quiver_core::process::ProcessHeapUsage,
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
