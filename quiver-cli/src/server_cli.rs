//! `quiv server` — the persistent process host. One runtime core (workers,
//! environment, io backend) behind a small HTTP API on a unix socket: processes are
//! created empty, resumed with compiled bytecode (the only code-bearing verb), and
//! deleted explicitly — the ownership cascade takes each root's subtree, and
//! `%proc.detach` is the escape hatch that lets a service outlive the client that
//! started it. The server holds no compiler and no session state: what remains per
//! root is a cancel flag, a busy marker and — for a client that asked for one — a
//! lease, which is what reclaims the root of a client that died without deleting it.
//!
//! Async edge, sync core: axum handlers never touch the environment — every
//! operation crosses into the existing blocking machinery (the env mutex, the
//! `Progress` waits) via `spawn_blocking`.

// Handler plumbing threads `axum::Response` (128 bytes) through `Err` — a size that
// is meaningless at per-HTTP-request frequency, so the lint's boxing would be noise.
#![allow(clippy::result_large_err)]

use axum::extract::{Path as AxumPath, State};
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use quiver_cli::native_transport::Progress;
use quiver_cli::protocol::{
    CompactRequest, CreateParams, CreateResponse, EventsParams, FINGERPRINT_HEADER, MissingModules,
    Outcome, PROTOCOL_VERSION, ProcessDetail, ProcessEvent, ProcessSummary, ProcessesEvent,
    ResumePayload, ResumeRequest, StatusResponse, WorkersEvent, http_endpoint_path, pidfile_path,
    token_path,
};
use quiver_cli::spawn_worker;
use quiver_core::process::ProcessId;
use quiver_core::wire::WireValue;
use quiver_environment::EnvironmentError;
use quiver_environment::{Environment, RequestResult, WorkerHandle};
use quiver_io::NativeEffect;
use std::collections::HashMap;
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

/// Per-root control state: the busy marker serializes resumes (they are sequential
/// and non-idempotent — an overlapping one answers 409), the cancel flag is how
/// `POST …/cancel` reaches a resume in flight, and the lease is how a root outlives its
/// client for a bounded time rather than forever.
struct RootControl {
    cancel: AtomicBool,
    busy: AtomicBool,
    /// How long this root may go unheard-from before the server stops it, and when that
    /// runs out. `None` is no lease — the root lives until an explicit `DELETE`, which
    /// is what a client that cannot beat gets.
    lease: Option<std::time::Duration>,
    expiry: Mutex<std::time::Instant>,
}

impl RootControl {
    fn new(lease: Option<std::time::Duration>) -> RootControl {
        RootControl {
            cancel: AtomicBool::new(false),
            busy: AtomicBool::new(false),
            lease,
            expiry: Mutex::new(std::time::Instant::now() + lease.unwrap_or_default()),
        }
    }

    /// Renew the lease: any request naming this root is its client saying it is there.
    fn touch(&self) {
        if let Some(lease) = self.lease {
            *lock(&self.expiry, "lease") = std::time::Instant::now() + lease;
        }
    }

    fn expired(&self, now: std::time::Instant) -> bool {
        self.lease.is_some() && *lock(&self.expiry, "lease") <= now
    }
}

/// How often the reaper looks for roots whose lease has run out. Coarse beside the
/// lease itself, which is what actually bounds how long an orphan lives.
const REAP_INTERVAL: std::time::Duration = std::time::Duration::from_secs(1);

/// The `/events` fan-out: each environment subscription id maps to its SSE event name
/// and the connection's channel. The stepping thread renders updates into here; a
/// dropped connection removes its entries and unsubscribes.
type Subscribers =
    Arc<Mutex<HashMap<u64, (&'static str, tokio::sync::mpsc::UnboundedSender<SseEvent>)>>>;
type SseEvent = axum::response::sse::Event;

struct ServerState {
    environment: Arc<Mutex<Environment<NativeEffect>>>,
    progress: Arc<Progress>,
    roots: Mutex<HashMap<ProcessId, Arc<RootControl>>>,
    shutdown: tokio::sync::Notify,
    subscribers: Subscribers,
}

type Shared = Arc<ServerState>;

/// Take one of this server's locks, treating poisoning as fatal. A panic under a lock
/// leaves the state behind it — the environment above all, which every session shares —
/// part-way through an update, and answering from it, or stepping over it, would serve
/// that middle to sessions that had nothing to do with the fault. Nothing here is
/// honestly recoverable, so the host stops instead of degrading.
fn lock<'a, T>(mutex: &'a Mutex<T>, what: &str) -> std::sync::MutexGuard<'a, T> {
    mutex.lock().unwrap_or_else(|_| {
        eprintln!("quiv server: the {what} lock is poisoned; aborting");
        std::process::abort()
    })
}

pub fn server_command(
    socket: Option<String>,
    listen: Option<String>,
    allow_origins: Vec<String>,
    code_collection_threshold: Option<usize>,
) -> Result<(), Box<dyn std::error::Error>> {
    let socket_path = resolve_socket(socket);
    // The address is parsed before anything is created, so a bad one fails the start
    // rather than leaving a bound socket and a minted token behind. The token itself is
    // read (or minted) only once the TCP listener is up, at the bottom of this function.
    let listen = listen
        .map(|address| parse_listen_address(&address))
        .transpose()?;
    let listener = bind_socket(&socket_path)?;
    let pidfile = pidfile_path(&socket_path);
    std::fs::write(&pidfile, std::process::id().to_string())?;

    // The runtime core, exactly as the in-process drivers build it.
    let builtins = quiver_cli::build_builtin_registry();
    let (waker, wake) = quiver_cli::native_transport::wake_channel();
    let num_workers = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(2);
    let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
    for i in 0..num_workers {
        workers.push(Box::new(spawn_worker(
            quiver_cli::native_transport::SystemClock,
            builtins.clone(),
            i as u16,
            waker.clone(),
        )));
    }
    let mut environment = Environment::<NativeEffect>::new(workers);
    environment.set_runtime_declarations(builtins.runtime_declarations().clone());
    if let Some(backend) = quiver_cli::create_effect_backend() {
        environment.set_effect_backend(backend);
    }
    if let Some(threshold) = code_collection_threshold {
        environment.set_code_collection_threshold(threshold);
    }
    let environment = Arc::new(Mutex::new(environment));
    let stepping_shutdown = Arc::new(AtomicBool::new(false));
    let progress = Arc::new(Progress::new());

    let subscribers: Subscribers = Arc::default();

    let env_clone = Arc::clone(&environment);
    let shutdown_clone = Arc::clone(&stepping_shutdown);
    let progress_clone = Arc::clone(&progress);
    let subscribers_clone = Arc::clone(&subscribers);
    let stepping = thread::spawn(move || {
        while !shutdown_clone.load(Ordering::Relaxed) {
            let did_work = {
                let mut env = lock(&env_clone, "environment");
                let did_work = env.step().unwrap_or(false);
                // Fan subscription updates out to their `/events` connections,
                // rendered under the same lock (types and values need the
                // environment). An update for a dropped connection is discarded.
                let updates = env.take_subscription_updates();
                if !updates.is_empty() {
                    let subscribers = lock(&subscribers_clone, "subscribers");
                    for (id, result) in updates {
                        if let Some((name, sender)) = subscribers.get(&id)
                            && let Some(event) = render_event(&mut env, name, result)
                        {
                            let _ = sender.send(event);
                        }
                    }
                }
                did_work
            };
            if did_work {
                progress_clone.notify();
            } else {
                let in_flight = lock(&env_clone, "environment").io_in_flight();
                wake.wait(in_flight);
            }
        }
    });

    println!("quiv server listening on {}", socket_path.display());

    let state: Shared = Arc::new(ServerState {
        environment,
        progress,
        roots: Mutex::new(HashMap::new()),
        shutdown: tokio::sync::Notify::new(),
        subscribers,
    });

    // Fingerprint-checked API, plus the two exempt endpoints that make takeover
    // possible: a stale server must still answer `/status` and honour `/shutdown`.
    let protected = axum::Router::new()
        .route("/processes", post(create_process).get(list_processes))
        .route(
            "/processes/{id}",
            get(inspect_process).delete(delete_process),
        )
        .route("/processes/{id}/resume", post(resume_process))
        .route("/processes/{id}/compact", post(compact_process))
        .route("/processes/{id}/cancel", post(cancel_process))
        .route("/processes/{id}/heartbeat", post(heartbeat_process))
        .route("/workers", get(list_workers))
        .route("/workers/{id}", get(inspect_worker))
        .route("/events", get(events))
        .layer(axum::middleware::from_fn(check_fingerprint));
    let router = axum::Router::new()
        .route("/status", get(status))
        .route("/shutdown", post(shutdown))
        .merge(protected)
        .with_state(Arc::clone(&state));

    let runtime = tokio::runtime::Runtime::new()?;
    let served = runtime.block_on(async {
        tokio::spawn(reap_expired_roots(Arc::clone(&state)));
        listener.set_nonblocking(true)?;
        let listener = tokio::net::UnixListener::from_std(listener)?;
        let socket_server = axum::serve(listener, router.clone()).with_graceful_shutdown({
            let state = Arc::clone(&state);
            async move { state.shutdown.notified().await }
        });
        // Graceful shutdown waits for open connections, and browsers hold idle
        // keep-alive connections to the TCP listener for minutes — so a shutdown gets
        // a short grace period and then wins regardless. The socket path never needed
        // this (its clients close per request), but one deadline serves both.
        let deadline = {
            let state = Arc::clone(&state);
            async move {
                state.shutdown.notified().await;
                tokio::time::sleep(std::time::Duration::from_secs(2)).await;
            }
        };
        let serve = {
            let socket_path = socket_path.clone();
            async move {
                match listen {
                    None => socket_server.await,
                    Some(address) => {
                        let tcp_listener = tokio::net::TcpListener::bind(address).await?;
                        // The credential is minted only now that the endpoint it opens
                        // exists: a token file left behind by a start that never got
                        // this far would be adopted by the next server, which is not
                        // what "persists across restarts" is meant to mean.
                        let token = load_or_create_token(&token_path(&socket_path))?;
                        // The browser endpoint: the same router, wrapped in the
                        // bearer-token gate (innermost, so it guards every route
                        // including the two the socket exempts — a hostile page cannot
                        // even probe) and CORS (outermost, so preflights — which carry
                        // no credentials by design — are answered before the token
                        // check).
                        let expected: Arc<str> = Arc::from(format!("Bearer {token}"));
                        let tcp_router = router
                            .layer(axum::middleware::from_fn(
                                move |request: axum::extract::Request,
                                      next: axum::middleware::Next| {
                                    let expected = Arc::clone(&expected);
                                    async move {
                                        let presented = request
                                            .headers()
                                            .get(axum::http::header::AUTHORIZATION)
                                            .and_then(|value| value.to_str().ok());
                                        match presented {
                                            Some(header) if secret_eq(header, &expected) => {
                                                next.run(request).await
                                            }
                                            _ => StatusCode::UNAUTHORIZED.into_response(),
                                        }
                                    }
                                },
                            ))
                            .layer(cors_layer(&allow_origins));
                        let endpoint = format!("http://{}", tcp_listener.local_addr()?);
                        // Recorded beside the socket so tooling can discover a port-0 bind.
                        std::fs::write(http_endpoint_path(&socket_path), &endpoint)?;
                        println!(
                            "quiv server listening on {endpoint} (token in {})",
                            token_path(&socket_path).display()
                        );
                        let tcp_server = axum::serve(tcp_listener, tcp_router)
                            .with_graceful_shutdown({
                                let state = Arc::clone(&state);
                                async move { state.shutdown.notified().await }
                            });
                        tokio::try_join!(socket_server, tcp_server).map(|_| ())
                    }
                }
            }
        };
        tokio::select! {
            result = serve => result,
            () = deadline => Ok(()),
        }
    });

    // Root processes die with the server (the documented blast radius); what matters
    // is releasing the endpoints — on the error path too, where a failed TCP bind must
    // not leave a stale socket file shadowing the next start.
    let _ = std::fs::remove_file(&socket_path);
    let _ = std::fs::remove_file(&pidfile);
    let _ = std::fs::remove_file(http_endpoint_path(&socket_path));
    stepping_shutdown.store(true, Ordering::Relaxed);
    waker.wake();
    let _ = stepping.join();
    served?;
    Ok(())
}

/// Parse `--listen`'s value: `host:port` as given, or a bare port on 127.0.0.1 — the
/// host carries almost no information when only loopback is accepted, so it may be
/// left off. The loopback check lives here too: this endpoint evaluates arbitrary
/// bytecode, and serving it beyond the machine is out of scope.
fn parse_listen_address(address: &str) -> Result<std::net::SocketAddr, String> {
    let parsed = address
        .parse::<std::net::SocketAddr>()
        .or_else(|_| {
            address
                .parse::<u16>()
                .map(|port| std::net::SocketAddr::new(std::net::Ipv4Addr::LOCALHOST.into(), port))
        })
        .map_err(|_| format!("--listen takes host:port or a bare port, not {address:?}"))?;
    if !parsed.ip().is_loopback() {
        return Err(format!(
            "--listen must bind a loopback address (got {parsed}): this endpoint evaluates \
             arbitrary bytecode, and serving it beyond the machine is out of scope"
        ));
    }
    Ok(parsed)
}

/// Read the TCP bearer token, or mint one (32 random bytes, hex) at mode 0600. It
/// persists across restarts deliberately: the dev loop replaces the server on every
/// rebuild, and a per-boot token would strand every connected browser each time.
///
/// An existing file is adopted only if it is a plain file this user owns and nobody
/// else can read or write. The token is the entire authentication of an endpoint that
/// evaluates arbitrary bytecode, so one that was planted — or left readable — is
/// refused rather than trusted: replacing it silently would be no better, since the
/// planter would then simply plant another.
fn load_or_create_token(path: &Path) -> std::io::Result<String> {
    use std::os::unix::fs::MetadataExt as _;
    match std::fs::symlink_metadata(path) {
        Ok(metadata) => {
            // SAFETY-free: geteuid never fails.
            let euid = unsafe { libc::geteuid() };
            if !metadata.is_file() || metadata.uid() != euid {
                return Err(std::io::Error::other(format!(
                    "{} is not a plain file owned by this user (uid {})",
                    path.display(),
                    metadata.uid()
                )));
            }
            let mode = metadata.mode() & 0o777;
            if mode & 0o077 != 0 {
                return Err(std::io::Error::other(format!(
                    "{} is mode {mode:04o}: the token must be readable by its owner alone",
                    path.display()
                )));
            }
            let existing = std::fs::read_to_string(path)?.trim().to_string();
            if !existing.is_empty() {
                return Ok(existing);
            }
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => return Err(e),
    }
    let mut bytes = [0u8; 32];
    getrandom::fill(&mut bytes).map_err(|e| std::io::Error::other(e.to_string()))?;
    let token: String = bytes.iter().map(|byte| format!("{byte:02x}")).collect();
    use std::io::Write as _;
    use std::os::unix::fs::OpenOptionsExt as _;
    std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .mode(0o600)
        .open(path)?
        .write_all(token.as_bytes())?;
    Ok(token)
}

/// Compare a presented credential against the expected one without an early exit, so
/// that how long the answer takes says nothing about how much of the token was right.
/// Lengths are compared first and are not secret: the token's is fixed and published
/// with the format.
fn secret_eq(presented: &str, expected: &str) -> bool {
    let (presented, expected) = (presented.as_bytes(), expected.as_bytes());
    presented.len() == expected.len()
        && presented
            .iter()
            .zip(expected)
            .fold(0u8, |differing, (a, b)| differing | (a ^ b))
            == 0
}

/// CORS for browser clients: the built-in allowlist (quiver.run, plus any localhost
/// origin for locally served frontends) extended by `--allow-origin`, the headers the
/// protocol uses, and the Private Network Access answer newer Chrome requires for a
/// public page reaching loopback. The token remains the actual gate — Origin is
/// defense-in-depth.
fn cors_layer(allow_origins: &[String]) -> tower_http::cors::CorsLayer {
    let extra: Vec<String> = allow_origins.to_vec();
    tower_http::cors::CorsLayer::new()
        .allow_origin(tower_http::cors::AllowOrigin::predicate(
            move |origin, _request| {
                origin
                    .to_str()
                    .is_ok_and(|origin| origin_allowed(origin, &extra))
            },
        ))
        .allow_methods([
            axum::http::Method::GET,
            axum::http::Method::POST,
            axum::http::Method::DELETE,
        ])
        .allow_headers([
            axum::http::header::AUTHORIZATION,
            axum::http::header::CONTENT_TYPE,
            axum::http::HeaderName::from_static(FINGERPRINT_HEADER),
        ])
        .allow_private_network(true)
}

fn origin_allowed(origin: &str, extra: &[String]) -> bool {
    if origin == "https://quiver.run" || extra.iter().any(|allowed| allowed == origin) {
        return true;
    }
    // A locally served frontend, on any port and either scheme.
    ["http://localhost", "https://localhost", "http://127.0.0.1"]
        .iter()
        .any(|local| {
            origin == *local
                || origin
                    .strip_prefix(local)
                    .is_some_and(|rest| rest.starts_with(':'))
        })
}

async fn check_fingerprint(
    request: axum::extract::Request,
    next: axum::middleware::Next,
) -> Response {
    let expected = quiver_compiler::compiler_fingerprint();
    match request
        .headers()
        .get(FINGERPRINT_HEADER)
        .and_then(|value| value.to_str().ok())
    {
        Some(fingerprint) if fingerprint == expected => next.run(request).await,
        _ => (
            StatusCode::UPGRADE_REQUIRED,
            format!("fingerprint mismatch: this server is build {expected}"),
        )
            .into_response(),
    }
}

async fn status() -> axum::Json<StatusResponse> {
    axum::Json(StatusResponse {
        protocol_version: PROTOCOL_VERSION,
        fingerprint: quiver_compiler::compiler_fingerprint().to_string(),
        pid: std::process::id(),
    })
}

async fn shutdown(State(state): State<Shared>) -> StatusCode {
    // Waiters, plural: with `--listen` two serve loops wait on this.
    state.shutdown.notify_waiters();
    StatusCode::OK
}

/// A blocking-section error, mapped onto a response.
fn internal(e: impl std::fmt::Debug) -> Response {
    (
        StatusCode::INTERNAL_SERVER_ERROR,
        format!("internal error: {e:?}"),
    )
        .into_response()
}

/// Run a closure on the blocking pool — the only way handlers touch the environment.
async fn blocking<T: Send + 'static>(
    work: impl FnOnce() -> T + Send + 'static,
) -> Result<T, Response> {
    tokio::task::spawn_blocking(work).await.map_err(internal)
}

async fn create_process(
    State(state): State<Shared>,
    axum::extract::Query(params): axum::extract::Query<CreateParams>,
) -> Result<axum::Json<CreateResponse>, Response> {
    let lease = params.lease_ms.map(std::time::Duration::from_millis);
    let shared = Arc::clone(&state);
    let pid = blocking(move || lock(&shared.environment, "environment").start_process())
        .await?
        .map_err(internal)?;
    lock(&state.roots, "roots").insert(pid, Arc::new(RootControl::new(lease)));
    Ok(axum::Json(CreateResponse { id: pid as u64 }))
}

async fn delete_process(
    State(state): State<Shared>,
    AxumPath(id): AxumPath<u64>,
) -> Result<StatusCode, Response> {
    let pid = id as ProcessId;
    if lock(&state.roots, "roots").remove(&pid).is_none() {
        return Err(StatusCode::NOT_FOUND.into_response());
    }
    stop_root(&state, pid).await?.map_err(internal)?;
    Ok(StatusCode::NO_CONTENT)
}

async fn cancel_process(
    State(state): State<Shared>,
    AxumPath(id): AxumPath<u64>,
) -> Result<StatusCode, Response> {
    let control = root_control(&state, id)?;
    control.cancel.store(true, Ordering::Relaxed);
    Ok(StatusCode::OK)
}

/// `POST …/heartbeat` — renew the root's lease and nothing else. A client with a long
/// resume in flight sends nothing else for as long as it runs, so this is what tells the
/// server the far end is still there.
async fn heartbeat_process(
    State(state): State<Shared>,
    AxumPath(id): AxumPath<u64>,
) -> Result<StatusCode, Response> {
    root_control(&state, id)?;
    Ok(StatusCode::OK)
}

/// Stop a root and its owned subtree, off the async threads like every other
/// environment operation.
async fn stop_root(
    state: &Shared,
    pid: ProcessId,
) -> Result<Result<(), EnvironmentError>, Response> {
    let shared = Arc::clone(state);
    blocking(move || lock(&shared.environment, "environment").stop_process(pid)).await
}

/// Stop the roots whose leases have run out.
///
/// A leased root is one whose client undertook to keep beating. The transport is a
/// short-lived connection per request, so a client that is killed says nothing at all —
/// and what it leaves is a root that goes on running, holding its owned subtree and
/// every resource in it, a bound listening port included. Roots created without a lease
/// are untouched: they are the ones whose client cannot beat, and an explicit `DELETE`
/// remains their only end.
async fn reap_expired_roots(state: Shared) {
    let mut ticker = tokio::time::interval(REAP_INTERVAL);
    loop {
        ticker.tick().await;
        let now = std::time::Instant::now();
        let mut expired: Vec<ProcessId> = Vec::new();
        lock(&state.roots, "roots").retain(|&pid, control| {
            let alive = !control.expired(now);
            if !alive {
                expired.push(pid);
            }
            alive
        });
        for pid in expired {
            eprintln!("quiv server: stopping root {pid}, whose client stopped answering");
            if let Ok(Err(error)) = stop_root(&state, pid).await {
                eprintln!("quiv server: root {pid} would not stop: {error:?}");
            }
        }
    }
}

async fn compact_process(
    State(state): State<Shared>,
    AxumPath(id): AxumPath<u64>,
    axum::Json(request): axum::Json<CompactRequest>,
) -> Result<StatusCode, Response> {
    root_control(&state, id)?;
    let shared = Arc::clone(&state);
    blocking(move || {
        shared
            .environment
            .lock()
            .unwrap()
            .compact_locals(id as ProcessId, request.keep)
    })
    .await?
    .map_err(internal)?;
    Ok(StatusCode::OK)
}

async fn resume_process(
    State(state): State<Shared>,
    AxumPath(id): AxumPath<u64>,
    axum::Json(request): axum::Json<ResumeRequest>,
) -> Result<axum::Json<Outcome>, Response> {
    let control = root_control(&state, id)?;
    let shared = Arc::clone(&state);
    let outcome = blocking(move || {
        // Serialize resumes per process: they are sequential and non-idempotent.
        if control.busy.swap(true, Ordering::Acquire) {
            return Err((
                StatusCode::CONFLICT,
                "a resume is already in flight for this process".to_string(),
            )
                .into_response());
        }
        let outcome = run_resume(&shared, id as ProcessId, &control, request);
        control.busy.store(false, Ordering::Release);
        outcome
    })
    .await??;
    Ok(axum::Json(outcome))
}

/// The blocking heart of `resume`: hand the unit to the process, wait on the
/// progress signal (stopping the root once if a cancel lands), render the outcome.
fn run_resume(
    state: &Shared,
    pid: ProcessId,
    control: &RootControl,
    request: ResumeRequest,
) -> Result<Outcome, Response> {
    control.cancel.store(false, Ordering::Relaxed);
    let request_id = {
        let mut env = lock(&state.environment, "environment");
        let ResumePayload { unit, modules } = request.payload;
        let builtins = quiver_cli::build_builtin_registry();
        // Link what the client attached; a stale sent-record answers 424 with the keys
        // to resend, while an *invalid* unit — content that does not hash to its key,
        // or malformed structure — is the request's own defect: refused outright,
        // since no resend of anything could repair it.
        let missing = match env.link_payload_modules(&modules, &unit, &builtins) {
            Ok(missing) => missing,
            Err(EnvironmentError::InvalidUnit(message)) => {
                return Err((StatusCode::UNPROCESSABLE_ENTITY, message).into_response());
            }
            Err(e) => return Err(internal(e)),
        };
        if !missing.is_empty() {
            return Err((
                StatusCode::FAILED_DEPENDENCY,
                axum::Json(MissingModules { missing }),
            )
                .into_response());
        }
        match env.resume_process_unit(pid, &unit, &builtins) {
            Ok(()) => {}
            Err(EnvironmentError::InvalidUnit(message)) => {
                return Err((StatusCode::UNPROCESSABLE_ENTITY, message).into_response());
            }
            Err(e) => return Err(internal(e)),
        }
        env.request_result(pid, request.keep).map_err(internal)?
    };
    match wait_for(state, request_id, Some((pid, &control.cancel))) {
        Ok(RequestResult::Result(Ok(value))) => {
            if control.cancel.load(Ordering::Relaxed) {
                // The cancel raced completion; the process was stopped either way,
                // so report the interruption deterministically.
                Ok(Outcome::Interrupted)
            } else {
                Ok(render(state, &value))
            }
        }
        Ok(RequestResult::Result(Err(e))) => {
            if control.cancel.load(Ordering::Relaxed) {
                Ok(Outcome::Interrupted)
            } else {
                Ok(Outcome::Error {
                    message: e.to_string(),
                })
            }
        }
        Ok(_) => Err(internal("unexpected result for resume")),
        Err(e) => Err(internal(e)),
    }
}

/// Wait for a request, sleeping on the progress signal. With a root given, a cancel
/// stops it (once) and keeps waiting — the request then resolves with the kill.
fn wait_for(
    state: &Shared,
    request: u64,
    root: Option<(ProcessId, &AtomicBool)>,
) -> Result<RequestResult, quiver_environment::EnvironmentError> {
    let mut stopped = false;
    loop {
        if let Some((pid, cancel)) = root
            && !stopped
            && cancel.load(Ordering::Relaxed)
        {
            let _ = lock(&state.environment, "environment").stop_process(pid);
            stopped = true;
        }
        let seen = state.progress.generation();
        // Bound to a local so the environment guard drops before the wait below.
        let polled = lock(&state.environment, "environment").poll_request(request);
        match polled {
            Ok(Some(result)) => return Ok(result),
            Ok(None) => state
                .progress
                .wait_past(seen, std::time::Duration::from_millis(50)),
            Err(e) => return Err(e),
        }
    }
}

/// Render a result value the way the REPL displays one: the value (data notation for
/// data values); its type for functions, builtins and pids; its failure origin for
/// stamped nils.
fn render(state: &Shared, value: &WireValue) -> Outcome {
    let mut env = lock(&state.environment, "environment");
    render_value(&mut env, value)
}

fn render_value(env: &mut Environment<NativeEffect>, value: &WireValue) -> Outcome {
    let rendered = env.format_value(value);
    let type_rendered = match value {
        WireValue::Function(..) | WireValue::Builtin(..) | WireValue::Process(..) => {
            let value_type = env.value_to_type(&value.for_display().0);
            Some(env.format_type(&value_type))
        }
        _ => None,
    };
    let origin = if value.is_nil() {
        env.describe_failure(value)
    } else {
        None
    };
    Outcome::Value {
        rendered,
        type_rendered,
        origin,
        is_nil: value.is_nil(),
    }
}

/// The realtime inspection stream (see [`EventsParams`]): register the requested
/// environment subscriptions, route their rendered updates into this connection's
/// channel, and undo all of it when the connection drops. Each subscription pushes an
/// initial snapshot, so the stream is complete from its first events.
async fn events(
    State(state): State<Shared>,
    axum::extract::Query(params): axum::extract::Query<EventsParams>,
) -> Result<
    axum::response::sse::Sse<
        impl tokio_stream::Stream<Item = Result<SseEvent, std::convert::Infallible>>,
    >,
    Response,
> {
    let mut ids: Vec<u64> = Vec::new();
    let register = |ids: &mut Vec<u64>| -> Result<Vec<(u64, &'static str)>, EnvironmentError> {
        let mut env = lock(&state.environment, "environment");
        let mut named = Vec::new();
        if params.processes {
            let id = env.subscribe_process_statuses()?;
            ids.push(id);
            named.push((id, "processes"));
        }
        if params.workers {
            let id = env.subscribe_worker_info()?;
            ids.push(id);
            named.push((id, "workers"));
        }
        if let Some(pid) = params.process {
            let id = env.subscribe_process_info(pid as ProcessId)?;
            ids.push(id);
            named.push((id, "process"));
        }
        Ok(named)
    };
    let named = match register(&mut ids) {
        Ok(named) => named,
        Err(error) => {
            // Roll back whatever did register before failing the request.
            let mut env = lock(&state.environment, "environment");
            for id in ids {
                let _ = env.unsubscribe(id);
            }
            return Err(internal(error));
        }
    };
    if named.is_empty() {
        return Err((
            StatusCode::BAD_REQUEST,
            "nothing to stream: name processes, workers and/or a process id".to_string(),
        )
            .into_response());
    }

    let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
    {
        let mut subscribers = lock(&state.subscribers, "subscribers");
        for (id, name) in &named {
            subscribers.insert(*id, (name, sender.clone()));
        }
    }
    // Dropping the stream drops the guard, which is the unsubscribe.
    let guard = Unsubscriber {
        state: Arc::clone(&state),
        ids: named.into_iter().map(|(id, _)| id).collect(),
    };
    let stream = tokio_stream::StreamExt::map(
        tokio_stream::wrappers::UnboundedReceiverStream::new(receiver),
        move |event| {
            let _ = &guard;
            Ok(event)
        },
    );
    Ok(axum::response::sse::Sse::new(stream).keep_alive(axum::response::sse::KeepAlive::default()))
}

/// Undoes an `/events` connection's registrations when its stream drops.
struct Unsubscriber {
    state: Shared,
    ids: Vec<u64>,
}

impl Drop for Unsubscriber {
    fn drop(&mut self) {
        {
            let mut subscribers = lock(&self.state.subscribers, "subscribers");
            for id in &self.ids {
                subscribers.remove(id);
            }
        }
        let mut env = lock(&self.state.environment, "environment");
        for id in &self.ids {
            let _ = env.unsubscribe(*id);
        }
    }
}

/// A subscription update as an SSE event, rendered under the environment lock: types
/// and result values only mean something next to the session program.
fn render_event(
    env: &mut Environment<NativeEffect>,
    name: &str,
    result: RequestResult,
) -> Option<SseEvent> {
    let payload = match result {
        RequestResult::Statuses(statuses) => {
            let mut processes: Vec<ProcessSummary> = statuses
                .into_iter()
                .map(|(id, status)| ProcessSummary {
                    id: id as u64,
                    status,
                })
                .collect();
            processes.sort_by_key(|process| process.id);
            serde_json::to_string(&ProcessesEvent { processes })
        }
        RequestResult::WorkerInfo(workers) => serde_json::to_string(&WorkersEvent { workers }),
        RequestResult::ProcessInfo(info) => {
            let process = info.map(|info| ProcessDetail {
                id: info.id as u64,
                status: info.status,
                process_type: info
                    .function_index
                    .and_then(|index| env.format_process_type(index)),
                stack_size: info.stack_size,
                locals_count: info.locals_count,
                frames_count: info.frames_count,
                mailbox_size: info.mailbox_size,
                persistent: info.persistent,
                result: info.result.map(|result| match result {
                    Ok(value) => render_value(env, &value),
                    Err(error) => Outcome::Error {
                        message: error.to_string(),
                    },
                }),
                heap: info.heap,
            });
            serde_json::to_string(&ProcessEvent { process })
        }
        _ => return None,
    }
    .ok()?;
    Some(SseEvent::default().event(name).data(payload))
}

/// The control block for a root, renewing its lease on the way: a request naming a root
/// is its client saying it is still there.
fn root_control(state: &Shared, id: u64) -> Result<Arc<RootControl>, Response> {
    let control = lock(&state.roots, "roots")
        .get(&(id as ProcessId))
        .cloned()
        .ok_or_else(|| StatusCode::NOT_FOUND.into_response())?;
    control.touch();
    Ok(control)
}

// --- Inspection (text/plain) -------------------------------------------------------

async fn list_processes(State(state): State<Shared>) -> Result<String, Response> {
    let shared = Arc::clone(&state);
    blocking(move || {
        let request = shared
            .environment
            .lock()
            .unwrap()
            .request_statuses()
            .map_err(internal)?;
        match wait_for(&shared, request, None) {
            Ok(RequestResult::Statuses(statuses)) if statuses.is_empty() => {
                Ok("No processes".to_string())
            }
            Ok(RequestResult::Statuses(statuses)) => {
                let mut processes: Vec<_> = statuses.into_iter().collect();
                processes.sort_by_key(|(id, _)| *id);
                let mut lines = vec!["Processes:".to_string()];
                lines.extend(
                    processes
                        .into_iter()
                        .map(|(id, status)| format!("  {id}: {status:?}")),
                );
                Ok(lines.join("\n"))
            }
            Ok(_) => Err(internal("unexpected result for statuses")),
            Err(e) => Err(internal(e)),
        }
    })
    .await?
}

async fn inspect_process(
    State(state): State<Shared>,
    AxumPath(id): AxumPath<u64>,
) -> Result<String, Response> {
    let shared = Arc::clone(&state);
    blocking(move || {
        let request = shared
            .environment
            .lock()
            .unwrap()
            .request_process_info(id as ProcessId)
            .map_err(|e| (StatusCode::NOT_FOUND, format!("{e:?}")).into_response())?;
        match wait_for(&shared, request, None) {
            Ok(RequestResult::ProcessInfo(Some(info))) => {
                let mut env = lock(&shared.environment, "environment");
                Ok(render_process_info(id as usize, &info, &mut env))
            }
            Ok(RequestResult::ProcessInfo(None)) => Err(StatusCode::NOT_FOUND.into_response()),
            Ok(_) => Err(internal("unexpected result for process info")),
            Err(e) => Err(internal(e)),
        }
    })
    .await?
}

async fn list_workers(State(state): State<Shared>) -> Result<String, Response> {
    let workers = worker_info(&state).await?;
    let mut lines = vec![format!("Workers ({}):", workers.len())];
    for worker in workers {
        let count = worker.process_ids.len();
        let mut line = format!(
            "  Worker {}: {} proc{} · {} binar{} · {}",
            worker.worker_id,
            count,
            if count == 1 { "" } else { "s" },
            worker.live_binaries,
            if worker.live_binaries == 1 {
                "y"
            } else {
                "ies"
            },
            format_bytes(worker.live_bytes)
        );
        if worker.shared_bytes > 0 {
            line.push_str(&format!(" ({} shared)", format_bytes(worker.shared_bytes)));
        }
        lines.push(line);
    }
    Ok(lines.join("\n"))
}

async fn inspect_worker(
    State(state): State<Shared>,
    AxumPath(id): AxumPath<u64>,
) -> Result<String, Response> {
    let workers = worker_info(&state).await?;
    match workers.iter().find(|w| w.worker_id as u64 == id) {
        Some(worker) => Ok(render_worker_info(worker)),
        None => Err(StatusCode::NOT_FOUND.into_response()),
    }
}

async fn worker_info(state: &Shared) -> Result<Vec<quiver_core::process::WorkerInfo>, Response> {
    let shared = Arc::clone(state);
    blocking(move || {
        let request = shared
            .environment
            .lock()
            .unwrap()
            .request_worker_info()
            .map_err(internal)?;
        match wait_for(&shared, request, None) {
            Ok(RequestResult::WorkerInfo(workers)) => Ok(workers),
            Ok(_) => Err(internal("unexpected result for worker info")),
            Err(e) => Err(internal(e)),
        }
    })
    .await?
}

fn render_process_info(
    id: usize,
    info: &quiver_core::process::ProcessInfo,
    env: &mut Environment<NativeEffect>,
) -> String {
    let mut lines = vec![format!("Process {id}:")];
    lines.push(if info.persistent {
        format!("  Status: {:?} (persistent)", info.status)
    } else {
        format!("  Status: {:?}", info.status)
    });
    lines.push(format!(
        "  Stack: {} ({})",
        info.stack_size,
        format_bytes(info.heap.stack.bytes)
    ));
    lines.push(format!(
        "  Locals: {} ({})",
        info.locals_count,
        format_bytes(info.heap.locals.bytes)
    ));
    lines.push(format!("  Frames: {}", info.frames_count));
    lines.push(format!(
        "  Mailbox: {} ({})",
        info.mailbox_size,
        format_bytes(info.heap.mailbox.bytes)
    ));
    // Distinct across all roots, so it is ≤ the sum of the per-root figures above
    // (a binary referenced from two roots is counted once here).
    lines.push(format!(
        "  Binaries: {} · {}",
        info.heap.total.binaries,
        format_bytes(info.heap.total.bytes)
    ));
    let type_str = info
        .function_index
        .and_then(|idx| env.format_process_type(idx))
        .unwrap_or_else(|| "―".to_string());
    lines.push(format!("  Type: {type_str}"));
    match &info.result {
        Some(Ok(value)) => lines.push(format!("  Result: {}", env.format_value(value))),
        Some(Err(err)) => lines.push(format!("  Result: Error({err:?})")),
        None => lines.push("  Result: ―".to_string()),
    }
    lines.join("\n")
}

fn render_worker_info(worker: &quiver_core::process::WorkerInfo) -> String {
    let mut lines = vec![format!("Worker {}:", worker.worker_id)];
    let pids: Vec<String> = worker.process_ids.iter().map(|p| p.to_string()).collect();
    lines.push(format!(
        "  Processes: {}  [{}]",
        worker.process_ids.len(),
        pids.join(", ")
    ));
    // Distinct buffers, counted by identity: one shared between two processes — or
    // two workers — appears once.
    lines.push(format!(
        "  Binaries: {} · {}",
        worker.live_binaries,
        format_bytes(worker.live_bytes)
    ));
    // Bytes whose allocation has another holder — another value here, or one on
    // another worker, since a send passes the handle.
    lines.push(format!("  Shared: {}", format_bytes(worker.shared_bytes)));
    // Unrealised ropes. Every read realises one and nothing caches the result, so a
    // deep rope read repeatedly redoes the work each time.
    if worker.rope_binaries > 0 {
        lines.push(format!(
            "  Ropes: {} unrealised · max depth {}",
            worker.rope_binaries, worker.max_rope_depth
        ));
    }
    lines.push(format!(
        "  Constants: {} · {}",
        worker.constant_binaries,
        format_bytes(worker.constant_bytes)
    ));
    lines.join("\n")
}

fn format_bytes(bytes: usize) -> String {
    const KB: f64 = 1024.0;
    const MB: f64 = 1024.0 * 1024.0;
    if bytes < 1024 {
        format!("{bytes} B")
    } else if (bytes as f64) < MB {
        format!("{:.1} KB", bytes as f64 / KB)
    } else {
        format!("{:.1} MB", bytes as f64 / MB)
    }
}

// --- Socket plumbing and the status/stop subcommands -------------------------------

fn resolve_socket(socket: Option<String>) -> PathBuf {
    match socket {
        Some(path) => PathBuf::from(path),
        None => quiver_cli::protocol::default_socket_path(),
    }
}

/// Bind the listener, readying the (0700) socket directory, and replacing a stale
/// socket file if no server answers on it.
fn bind_socket(path: &Path) -> Result<UnixListener, Box<dyn std::error::Error>> {
    quiver_cli::protocol::prepare_socket_dir(path)?;
    match UnixListener::bind(path) {
        Ok(listener) => Ok(listener),
        Err(e) if e.kind() == std::io::ErrorKind::AddrInUse => {
            if UnixStream::connect(path).is_ok() {
                return Err(format!("a server is already listening on {}", path.display()).into());
            }
            // Stale socket, no listener. Racing spawns may both reach here; the one
            // that loses the rebind exits and its client connects to the winner, so a
            // missing file is fine and a failed rebind is the loss, not an anomaly.
            match std::fs::remove_file(path) {
                Ok(()) => {}
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
                Err(e) => return Err(e.into()),
            }
            Ok(UnixListener::bind(path)?)
        }
        Err(e) => Err(e.into()),
    }
}

/// `quiv server status`: whether a server is listening, and which build.
pub fn status_command(socket: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let socket = resolve_socket(socket);
    match quiver_cli::client::Client::new(socket.clone()).status() {
        Ok(status) => {
            if status.fingerprint == quiver_compiler::compiler_fingerprint() {
                println!(
                    "running: pid {}, build {}, socket {}",
                    status.pid,
                    status.fingerprint,
                    socket.display()
                );
            } else {
                println!(
                    "running but stale: build {} (this binary is {}); \
                     the next quiv run/repl will replace it",
                    status.fingerprint,
                    quiver_compiler::compiler_fingerprint()
                );
            }
        }
        Err(quiver_cli::client::ConnectError::Absent) => {
            println!("not running (socket {})", socket.display());
        }
        Err(e) => return Err(e.into()),
    }
    Ok(())
}

/// `quiv server stop`: stop a listening server, however old.
pub fn stop_command(socket: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let socket = resolve_socket(socket);
    if UnixStream::connect(&socket).is_err() {
        println!("not running (socket {})", socket.display());
        return Ok(());
    }
    quiver_cli::client::stop_server(&socket)?;
    println!("stopped");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::parse_listen_address;

    #[test]
    fn listen_addresses_parse_with_an_optional_host() {
        assert_eq!(
            parse_listen_address("2192").unwrap().to_string(),
            "127.0.0.1:2192"
        );
        assert_eq!(
            parse_listen_address("0").unwrap().to_string(),
            "127.0.0.1:0"
        );
        assert_eq!(
            parse_listen_address("127.0.0.1:8500").unwrap().to_string(),
            "127.0.0.1:8500"
        );
        assert_eq!(
            parse_listen_address("[::1]:2192").unwrap().to_string(),
            "[::1]:2192"
        );
        assert!(
            parse_listen_address("0.0.0.0:2192")
                .unwrap_err()
                .contains("loopback")
        );
        assert!(
            parse_listen_address("nonsense")
                .unwrap_err()
                .contains("bare port")
        );
    }
}
