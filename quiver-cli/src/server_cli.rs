//! `quiv server` — the persistent process host. One runtime core (workers,
//! environment, io backend) behind a small HTTP API on a unix socket: processes are
//! created empty, resumed with compiled bytecode (the only code-bearing verb), and
//! deleted explicitly — the ownership cascade takes each root's subtree, and
//! `%proc.detach` is the escape hatch that lets a service outlive the client that
//! started it. The server holds no compiler and no session state: what remains per
//! root is a cancel flag and a busy marker.
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
    CompactRequest, CreateResponse, FINGERPRINT_HEADER, Outcome, PROTOCOL_VERSION, ResumeRequest,
    StatusResponse, pidfile_path,
};
use quiver_cli::spawn_worker;
use quiver_core::process::ProcessId;
use quiver_core::wire::WireValue;
use quiver_environment::{Environment, RequestResult, WorkerHandle};
use quiver_io::NativeEffect;
use std::collections::HashMap;
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

/// Per-root control state: the busy marker serializes resumes (they are sequential
/// and non-idempotent — an overlapping one answers 409), and the cancel flag is how
/// `POST …/cancel` reaches a resume in flight.
struct RootControl {
    cancel: AtomicBool,
    busy: AtomicBool,
}

struct ServerState {
    environment: Arc<Mutex<Environment<NativeEffect>>>,
    progress: Arc<Progress>,
    roots: Mutex<HashMap<ProcessId, Arc<RootControl>>>,
    shutdown: tokio::sync::Notify,
}

type Shared = Arc<ServerState>;

pub fn server_command(
    socket: Option<String>,
    code_collection_threshold: Option<usize>,
) -> Result<(), Box<dyn std::error::Error>> {
    let socket_path = resolve_socket(socket);
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
            false,
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

    let env_clone = Arc::clone(&environment);
    let shutdown_clone = Arc::clone(&stepping_shutdown);
    let progress_clone = Arc::clone(&progress);
    let stepping = thread::spawn(move || {
        while !shutdown_clone.load(Ordering::Relaxed) {
            let did_work = env_clone
                .lock()
                .map(|mut env| env.step().unwrap_or(false))
                .unwrap_or(false);
            if did_work {
                progress_clone.notify();
            } else {
                let in_flight = env_clone
                    .lock()
                    .map(|env| env.io_in_flight())
                    .unwrap_or(true);
                wake.wait(in_flight);
            }
        }
    });

    // Warm the artifact store with std in the background: clients compile, so this
    // serves *their* first imports — through the shared cache directory.
    thread::spawn(|| {
        let store = std::rc::Rc::new(quiver_compiler::ArtifactStore::cache());
        quiver_compiler::warm_std_store(
            &store,
            &quiver_cli::build_builtin_registry(),
            quiver_compiler::compiler::CompileOptions {
                debug: true,
                source_name: "std".to_string(),
                ..Default::default()
            },
        );
    });

    println!("quiv server listening on {}", socket_path.display());

    let state: Shared = Arc::new(ServerState {
        environment,
        progress,
        roots: Mutex::new(HashMap::new()),
        shutdown: tokio::sync::Notify::new(),
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
        .route("/workers", get(list_workers))
        .route("/workers/{id}", get(inspect_worker))
        .layer(axum::middleware::from_fn(check_fingerprint));
    let router = axum::Router::new()
        .route("/status", get(status))
        .route("/shutdown", post(shutdown))
        .merge(protected)
        .with_state(Arc::clone(&state));

    let runtime = tokio::runtime::Runtime::new()?;
    runtime.block_on(async {
        listener.set_nonblocking(true)?;
        let listener = tokio::net::UnixListener::from_std(listener)?;
        axum::serve(listener, router)
            .with_graceful_shutdown(async move { state.shutdown.notified().await })
            .await
    })?;

    // Root processes die with the server (the documented blast radius); what matters
    // is releasing the endpoint.
    let _ = std::fs::remove_file(&socket_path);
    let _ = std::fs::remove_file(&pidfile);
    stepping_shutdown.store(true, Ordering::Relaxed);
    waker.wake();
    let _ = stepping.join();
    Ok(())
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
    state.shutdown.notify_one();
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
) -> Result<axum::Json<CreateResponse>, Response> {
    let shared = Arc::clone(&state);
    let pid = blocking(move || shared.environment.lock().unwrap().start_process(None))
        .await?
        .map_err(internal)?;
    state.roots.lock().unwrap().insert(
        pid,
        Arc::new(RootControl {
            cancel: AtomicBool::new(false),
            busy: AtomicBool::new(false),
        }),
    );
    Ok(axum::Json(CreateResponse { id: pid as u64 }))
}

async fn delete_process(
    State(state): State<Shared>,
    AxumPath(id): AxumPath<u64>,
) -> Result<StatusCode, Response> {
    let pid = id as ProcessId;
    if state.roots.lock().unwrap().remove(&pid).is_none() {
        return Err(StatusCode::NOT_FOUND.into_response());
    }
    let shared = Arc::clone(&state);
    blocking(move || shared.environment.lock().unwrap().stop_process(pid))
        .await?
        .map_err(internal)?;
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

/// The blocking heart of `resume`: hand the bytecode to the process, wait on the
/// progress signal (stopping the root once if a cancel lands), render the outcome.
fn run_resume(
    state: &Shared,
    pid: ProcessId,
    control: &RootControl,
    request: ResumeRequest,
) -> Result<Outcome, Response> {
    control.cancel.store(false, Ordering::Relaxed);
    let request_id = {
        let mut env = state.environment.lock().unwrap();
        env.resume_process(pid, request.bytecode)
            .map_err(internal)?;
        env.request_result(pid, request.keep).map_err(internal)?
    };
    match wait_for(state, request_id, Some((pid, &control.cancel))) {
        Ok(RequestResult::Result(Ok(value), _)) => {
            if control.cancel.load(Ordering::Relaxed) {
                // The cancel raced completion; the process was stopped either way,
                // so report the interruption deterministically.
                Ok(Outcome::Interrupted)
            } else {
                Ok(render(state, &value))
            }
        }
        Ok(RequestResult::Result(Err(e), _)) => {
            if control.cancel.load(Ordering::Relaxed) {
                Ok(Outcome::Interrupted)
            } else {
                Ok(Outcome::Error {
                    message: e.crash_message(),
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
            let _ = state.environment.lock().unwrap().stop_process(pid);
            stopped = true;
        }
        let seen = state.progress.generation();
        // Bound to a local so the environment guard drops before the wait below.
        let polled = state.environment.lock().unwrap().poll_request(request);
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
    let mut env = state.environment.lock().unwrap();
    let rendered = env.format_value(value);
    let type_rendered = match value {
        WireValue::Function(..) | WireValue::Builtin(..) | WireValue::Process(..) => {
            let value_type = env.value_to_type(&value.for_display().0);
            Some(env.format_type(&value_type))
        }
        _ => None,
    };
    let origin = if value.is_nil() {
        env.describe_origin(value)
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

fn root_control(state: &Shared, id: u64) -> Result<Arc<RootControl>, Response> {
    state
        .roots
        .lock()
        .unwrap()
        .get(&(id as ProcessId))
        .cloned()
        .ok_or_else(|| StatusCode::NOT_FOUND.into_response())
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
                let mut env = shared.environment.lock().unwrap();
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

/// Bind the listener, creating the (0700) socket directory, and replacing a stale
/// socket file if no server answers on it.
fn bind_socket(path: &Path) -> Result<UnixListener, Box<dyn std::error::Error>> {
    if let Some(dir) = path.parent()
        && !dir.exists()
    {
        // Only a directory this server creates is locked down; a pre-existing one
        // (an explicit --socket in /tmp, a re-used runtime dir) is left alone.
        std::fs::create_dir_all(dir)?;
        std::fs::set_permissions(dir, std::os::unix::fs::PermissionsExt::from_mode(0o700))?;
    }
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
