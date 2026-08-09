//! The `quiv server` acceptance gate (docs plan, phases 2–4.5): concurrent sessions
//! compiling client-side and resuming over the HTTP API, concurrent std imports (the
//! artifact-directory write race), cancels, abandoned processes staying visible and
//! collectable, per-process resume serialization (409), code sweeps under load,
//! fingerprint rejection, connect-else-spawn, and takeover of a stale server.
//!
//! Tests are serialized (each spawns its own server process with a full worker set;
//! running several at once is needless load on the machine).

use quiver_cli::client::Client;
use quiver_cli::protocol::Outcome;
use quiver_cli::protocol::{MissingModules, ResumePayload};
use quiver_environment::LineCompiler;
use quiver_io::NativeEffect;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::process::{Child, Command};
use std::sync::Mutex;
use std::time::{Duration, Instant};

static SERIAL: Mutex<()> = Mutex::new(());

struct Server {
    child: Child,
    socket: PathBuf,
}

fn scratch_socket(prefix: &str) -> PathBuf {
    std::env::temp_dir().join(format!(
        "{prefix}-{}-{:x}.sock",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ))
}

impl Server {
    fn start(args: &[&str]) -> Self {
        let socket = scratch_socket("quiv-test");
        let child = Command::new(env!("CARGO_BIN_EXE_quiv"))
            .arg("server")
            .arg("--socket")
            .arg(&socket)
            .args(args)
            .spawn()
            .expect("failed to spawn quiv server");
        let deadline = Instant::now() + Duration::from_secs(20);
        loop {
            if std::os::unix::net::UnixStream::connect(&socket).is_ok() {
                break;
            }
            assert!(Instant::now() < deadline, "server never started listening");
            std::thread::sleep(Duration::from_millis(50));
        }
        Server { child, socket }
    }

    fn client(&self) -> Client {
        Client::new(self.socket.clone())
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
        let _ = std::fs::remove_file(&self.socket);
        let _ = std::fs::remove_file(self.socket.with_extension("pid"));
        let _ = std::fs::remove_file(self.socket.with_extension("log"));
    }
}

/// A client-side session: the CLI's own shape — a `LineCompiler` plus a server
/// process — exercised as a library.
struct Session {
    client: Client,
    compiler: LineCompiler<NativeEffect>,
    pid: u64,
}

impl Session {
    fn open(server: &Server) -> Self {
        let client = server.client();
        let pid = client.create_process().expect("create failed");
        Session {
            client,
            compiler: LineCompiler::new(
                Box::new(quiver_compiler::PackageResolver::inline()),
                quiver_cli::build_builtin_registry(),
            ),
            pid,
        }
    }

    fn evaluate(&mut self, source: &str) -> Outcome {
        let prepared = self.compiler.prepare(source).expect("prepare failed");
        self.client
            .compact(self.pid, prepared.compact_keep().to_vec())
            .expect("compact failed");
        let compiled = self.compiler.compile(prepared).expect("compile failed");
        let committed = self
            .compiler
            .commit_line(compiled)
            .expect("expected executable code");
        // This harness attaches no store, so the payload arrives fully inlined.
        let quiver_environment::LinePayload { unit, modules } = committed.payload;
        self.client
            .resume(
                self.pid,
                ResumePayload {
                    unit,
                    modules: modules
                        .iter()
                        .map(|(key, artifact)| (*key, artifact.unit.clone()))
                        .collect(),
                },
                Some(committed.keep_indices),
            )
            .expect("resume failed")
    }

    fn evaluate_value(&mut self, source: &str) -> String {
        match self.evaluate(source) {
            Outcome::Value { rendered, .. } => rendered,
            other => panic!("expected a value, got {other:?}"),
        }
    }
}

fn program(source: &str) -> quiver_compiler::CompiledUnit {
    let ast = quiver_compiler::parse(source).expect("parse failed");
    let resolver = quiver_compiler::PackageResolver::inline();
    let (program, module_cache, entry) = quiver_cli::compile::compile_entry(
        ast,
        &resolver,
        &quiver_cli::build_builtin_registry(),
        Default::default(),
        None,
    )
    .expect("compile failed");
    quiver_compiler::extract_unit(
        &program,
        &module_cache,
        Some(entry),
        0,
        quiver_compiler::Imports::Inline,
    )
}

#[test]
fn fingerprint_is_checked_on_protected_routes() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let server = Server::start(&[]);
    // A raw request with no fingerprint header: protected routes answer 426.
    let mut stream = std::os::unix::net::UnixStream::connect(&server.socket).unwrap();
    stream
        .write_all(b"POST /processes HTTP/1.1\r\nHost: quiv\r\nConnection: close\r\nContent-Length: 0\r\n\r\n")
        .unwrap();
    let mut response = String::new();
    stream.read_to_string(&mut response).unwrap();
    assert!(
        response.starts_with("HTTP/1.1 426"),
        "expected 426, got: {response}"
    );
    // /status stays exempt, which is what makes takeover possible.
    let status = server.client().status().expect("status failed");
    assert_eq!(status.fingerprint, quiver_compiler::compiler_fingerprint());
}

#[test]
fn concurrent_sessions_evaluate_and_run_independently() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let server = Server::start(&[]);
    std::thread::scope(|scope| {
        for n in 0..8u64 {
            let server = &server;
            scope.spawn(move || {
                let mut session = Session::open(server);
                session.evaluate_value(&format!("x = {n}; x"));
                for step in 0..10u64 {
                    let value = session.evaluate_value("x = [x, 1] ~> __integer_add__; x");
                    assert_eq!(value, (n + step + 1).to_string(), "session {n}");
                }
                // A run beside the session: its own root, one resume, delete.
                let client = server.client();
                let pid = client.create_process().expect("create failed");
                let outcome = client
                    .resume(
                        pid,
                        ResumePayload {
                            unit: program(&format!("#{{ [{n}, 1] ~> __integer_add__ }}")),
                            modules: Vec::new(),
                        },
                        None,
                    )
                    .expect("resume failed");
                match outcome {
                    Outcome::Value { rendered, .. } => {
                        assert_eq!(rendered, (n + 1).to_string(), "run in session {n}")
                    }
                    other => panic!("expected a value, got {other:?}"),
                }
                client.delete_process(pid).expect("delete failed");
            });
        }
    });
}

#[test]
fn concurrent_std_imports_share_the_cache() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let server = Server::start(&[]);
    std::thread::scope(|scope| {
        for n in 1..=4i64 {
            let server = &server;
            scope.spawn(move || {
                let mut session = Session::open(server);
                let value = session.evaluate_value(&format!("%num.add [{n}, {n}]"));
                assert_eq!(value, (2 * n).to_string(), "session {n}");
            });
        }
    });
}

#[test]
fn cancel_interrupts_and_a_fresh_session_recovers() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let server = Server::start(&[]);
    let client = server.client();
    let pid = client.create_process().expect("create failed");
    let spinner = std::thread::spawn({
        let client = client.clone();
        move || {
            client.resume(
                pid,
                ResumePayload {
                    unit: program("#{ f = #[] { ^ [] }; f [] }"),
                    modules: Vec::new(),
                },
                None,
            )
        }
    });
    std::thread::sleep(Duration::from_millis(300));
    client.cancel(pid).expect("cancel failed");
    match spinner.join().unwrap().expect("resume errored") {
        Outcome::Interrupted => {}
        other => panic!("expected Interrupted, got {other:?}"),
    }
    client.delete_process(pid).expect("delete failed");
    // The server is healthy; a fresh session evaluates.
    let mut session = Session::open(&server);
    assert_eq!(session.evaluate_value("7"), "7");
}

#[test]
fn concurrent_resumes_answer_conflict() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let server = Server::start(&[]);
    let client = server.client();
    let pid = client.create_process().expect("create failed");
    let spinner = std::thread::spawn({
        let client = client.clone();
        move || {
            client.resume(
                pid,
                ResumePayload {
                    unit: program("#{ f = #[] { ^ [] }; f [] }"),
                    modules: Vec::new(),
                },
                None,
            )
        }
    });
    std::thread::sleep(Duration::from_millis(300));
    // Resumes are serialized per process: an overlapping one is refused.
    let overlap = client.resume(
        pid,
        ResumePayload {
            unit: program("#{ 1 }"),
            modules: Vec::new(),
        },
        None,
    );
    assert!(
        overlap.as_ref().is_err_and(|e| e.is_conflict()),
        "expected 409, got {overlap:?}"
    );
    client.cancel(pid).expect("cancel failed");
    match spinner.join().unwrap().expect("resume errored") {
        Outcome::Interrupted => {}
        other => panic!("expected Interrupted, got {other:?}"),
    }
    client.delete_process(pid).expect("delete failed");
}

#[test]
fn abandoned_processes_stay_visible_and_collectable() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let server = Server::start(&[]);
    let client = server.client();
    // An abandoned spinner: no cancel, no delete — the crashed-client shape. Detach
    // the resume into a thread nothing joins.
    let pid = client.create_process().expect("create failed");
    let _abandoned = std::thread::spawn({
        let client = client.clone();
        move || {
            client.resume(
                pid,
                ResumePayload {
                    unit: program("#{ f = #[] { ^ [] }; f [] }"),
                    modules: Vec::new(),
                },
                None,
            )
        }
    });
    std::thread::sleep(Duration::from_millis(300));

    // The server stays fully usable beside it...
    let mut session = Session::open(&server);
    assert_eq!(session.evaluate_value("[40, 2] ~> __integer_add__"), "42");

    // ...the orphan is visible...
    let listing = client.inspect("/processes").expect("inspect failed");
    assert!(
        listing.contains(&format!("{pid}: Active")),
        "expected process {pid} running in:\n{listing}"
    );

    // ...and manually collectable.
    client.cancel(pid).expect("cancel failed");
    client.delete_process(pid).expect("delete failed");
    let listing = client.inspect("/processes").expect("inspect failed");
    assert!(
        !listing.contains(&format!("{pid}: Active")),
        "expected process {pid} stopped in:\n{listing}"
    );
}

#[test]
fn code_sweeps_fire_under_concurrent_load() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let server = Server::start(&["--code-collection-threshold", "64"]);
    std::thread::scope(|scope| {
        for n in 0..4u64 {
            let server = &server;
            scope.spawn(move || {
                let mut session = Session::open(server);
                for round in 0..15u64 {
                    // Fresh function content per line, then a rebind killing the
                    // previous one — churn that crosses the sweep threshold
                    // constantly while other sessions do the same.
                    let offset = n * 1000 + round;
                    session.evaluate(&format!("f = #'int {{ [$, {offset}] ~> __integer_add__ }}"));
                    let value = session.evaluate_value("f 1");
                    assert_eq!(value, (offset + 1).to_string(), "session {n} round {round}");
                    session.evaluate("f = 0");
                }
            });
        }
    });
}

#[test]
fn shutdown_stops_the_server() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let mut server = Server::start(&[]);
    let mut session = Session::open(&server);
    assert_eq!(session.evaluate_value("1"), "1");
    // Fingerprint-exempt by design: any client can stop the server.
    server.client().shutdown().expect("shutdown failed");
    wait_for_exit(&mut server.child);
}

fn wait_for_exit(child: &mut Child) {
    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
        if let Some(status) = child.try_wait().expect("wait failed") {
            assert!(status.success(), "server exited with {status}");
            break;
        }
        assert!(Instant::now() < deadline, "server did not exit on shutdown");
        std::thread::sleep(Duration::from_millis(100));
    }
}

// --- Connect-else-spawn and takeover -----------------------------------------------

/// A socket path for a server no test guard owns; the guard kills via the pidfile.
struct SpawnedServer {
    socket: PathBuf,
}

impl SpawnedServer {
    fn fresh() -> Self {
        SpawnedServer {
            socket: scratch_socket("quiv-spawn"),
        }
    }
}

impl Drop for SpawnedServer {
    fn drop(&mut self) {
        if let Ok(pid_text) =
            std::fs::read_to_string(quiver_cli::protocol::pidfile_path(&self.socket))
            && let Ok(pid) = pid_text.trim().parse::<i32>()
        {
            unsafe { libc::kill(pid, libc::SIGTERM) };
        }
        let _ = std::fs::remove_file(&self.socket);
        let _ = std::fs::remove_file(self.socket.with_extension("log"));
        let _ = std::fs::remove_file(quiver_cli::protocol::pidfile_path(&self.socket));
    }
}

fn exe() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_quiv"))
}

fn quick_value(client: &Client, source: &str) -> String {
    let pid = client.create_process().expect("create failed");
    let outcome = client
        .resume(
            pid,
            ResumePayload {
                unit: program(source),
                modules: Vec::new(),
            },
            None,
        )
        .expect("resume failed");
    client.delete_process(pid).expect("delete failed");
    match outcome {
        Outcome::Value { rendered, .. } => rendered,
        other => panic!("expected a value, got {other:?}"),
    }
}

#[test]
fn connect_or_spawn_starts_a_server_when_absent() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let spawned = SpawnedServer::fresh();
    let client = quiver_cli::client::connect_or_spawn(&spawned.socket, &exe())
        .expect("connect_or_spawn failed");
    assert_eq!(
        quick_value(&client, "#{ [40, 2] ~> __integer_add__ }"),
        "42"
    );
    client.shutdown().expect("shutdown failed");
}

#[test]
fn connect_or_spawn_reuses_a_running_server() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let server = Server::start(&[]);
    let pid_before =
        std::fs::read_to_string(quiver_cli::protocol::pidfile_path(&server.socket)).unwrap();
    let client = quiver_cli::client::connect_or_spawn(&server.socket, &exe())
        .expect("connect_or_spawn failed");
    assert_eq!(quick_value(&client, "#{ 1 }"), "1");
    let pid_after =
        std::fs::read_to_string(quiver_cli::protocol::pidfile_path(&server.socket)).unwrap();
    assert_eq!(pid_before, pid_after, "a second server was spawned");
}

#[test]
fn racing_spawns_converge_on_one_server() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let spawned = SpawnedServer::fresh();
    std::thread::scope(|scope| {
        for n in 0..4u64 {
            let socket = &spawned.socket;
            scope.spawn(move || {
                let client = quiver_cli::client::connect_or_spawn(socket, &exe())
                    .expect("connect_or_spawn failed");
                assert_eq!(quick_value(&client, &format!("#{{ {n} }}")), n.to_string());
            });
        }
    });
    // Exactly one server holds the socket; its pidfile names a live process.
    let pid_text =
        std::fs::read_to_string(quiver_cli::protocol::pidfile_path(&spawned.socket)).unwrap();
    let pid: i32 = pid_text.trim().parse().unwrap();
    assert_eq!(unsafe { libc::kill(pid, 0) }, 0, "pidfile names a dead pid");
}

#[test]
fn takeover_replaces_an_incompatible_server() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let spawned = SpawnedServer::fresh();

    // A mock stale server: answers `/status` with an old fingerprint and honours
    // `POST /shutdown` by unlinking its socket and exiting — the contract a real old
    // server satisfies.
    let socket = spawned.socket.clone();
    let listener = std::os::unix::net::UnixListener::bind(&socket).unwrap();
    let mock = std::thread::spawn(move || {
        for stream in listener.incoming() {
            let Ok(mut stream) = stream else { break };
            let mut buffer = [0u8; 1024];
            let read = stream.read(&mut buffer).unwrap_or(0);
            let request = String::from_utf8_lossy(&buffer[..read]);
            if request.starts_with("POST /shutdown") {
                let _ = stream.write_all(
                    b"HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n",
                );
                let _ = std::fs::remove_file(&socket);
                return;
            }
            let body = r#"{"protocol_version":2,"fingerprint":"old-build","pid":0}"#;
            let _ = stream.write_all(
                format!(
                    "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
                    body.len()
                )
                .as_bytes(),
            );
        }
    });

    let client =
        quiver_cli::client::connect_or_spawn(&spawned.socket, &exe()).expect("takeover failed");
    assert_eq!(
        quick_value(&client, "#{ [40, 2] ~> __integer_add__ }"),
        "42"
    );
    client.shutdown().expect("shutdown failed");
    mock.join().unwrap();
}

/// A session that ships units, mirroring `ReplCli::send_line`: attach only what this
/// server has not been given, and correct the record from a `424`.
struct UnitSession {
    client: Client,
    compiler: LineCompiler<NativeEffect>,
    pid: u64,
    sent: std::collections::HashSet<u64>,
}

impl UnitSession {
    fn open(server: &Server) -> Self {
        let client = server.client();
        let pid = client.create_process().expect("create failed");
        let mut compiler = LineCompiler::new(
            Box::new(quiver_compiler::PackageResolver::inline()),
            quiver_cli::build_builtin_registry(),
        );
        compiler.set_artifact_store(std::rc::Rc::new(quiver_compiler::ArtifactStore::cache()));
        UnitSession {
            client,
            compiler,
            pid,
            sent: std::collections::HashSet::new(),
        }
    }

    /// Compile a line and answer its unit payload, without sending it.
    fn compile(
        &mut self,
        source: &str,
    ) -> (
        quiver_compiler::CompiledUnit,
        Vec<(u64, quiver_compiler::CompiledUnit)>,
        Vec<usize>,
    ) {
        let prepared = self.compiler.prepare(source).expect("prepare failed");
        self.client
            .compact(self.pid, prepared.compact_keep().to_vec())
            .expect("compact failed");
        let compiled = self.compiler.compile(prepared).expect("compile failed");
        let committed = self
            .compiler
            .commit_line(compiled)
            .expect("expected executable code");
        let quiver_environment::LinePayload { unit, modules } = committed.payload;
        (
            unit,
            modules
                .iter()
                .map(|(key, artifact)| (*key, artifact.unit.clone()))
                .collect(),
            committed.keep_indices,
        )
    }
}

#[test]
fn a_unit_resume_links_its_modules_and_names_them_thereafter() {
    let server = Server::start(&[]);
    let mut session = UnitSession::open(&server);

    // First line: the server holds nothing, so everything it needs rides along.
    let (unit, modules, keep) = session.compile("%num.mul [7, 6]");
    assert!(!modules.is_empty(), "a first line must carry its modules");
    let outcome = session
        .client
        .resume(
            session.pid,
            ResumePayload {
                unit,
                modules: modules.clone(),
            },
            Some(keep),
        )
        .expect("resume failed");
    match outcome {
        Outcome::Value { rendered, .. } => assert_eq!(rendered, "42"),
        other => panic!("expected 42, got {other:?}"),
    }
    session.sent.extend(modules.iter().map(|(key, _)| *key));

    // Second line: the same modules are already there, so it names them and sends none.
    let (unit, modules, keep) = session.compile("%num.mul [6, 6]");
    let attach: Vec<_> = modules
        .iter()
        .filter(|(key, _)| !session.sent.contains(key))
        .cloned()
        .collect();
    assert!(
        attach.is_empty(),
        "the second line must not resend modules the server holds"
    );
    let outcome = session
        .client
        .resume(
            session.pid,
            ResumePayload {
                unit,
                modules: attach,
            },
            Some(keep),
        )
        .expect("resume failed");
    match outcome {
        Outcome::Value { rendered, .. } => assert_eq!(rendered, "36"),
        other => panic!("expected 36, got {other:?}"),
    }
}

#[test]
fn a_stale_sent_record_is_answered_with_the_missing_keys() {
    // A client that believes the server holds modules it does not — the state a code
    // sweep leaves behind — must be told exactly which, and succeed on the retry.
    let server = Server::start(&[]);
    let mut session = UnitSession::open(&server);
    let (unit, modules, keep) = session.compile("%num.mul [7, 6]");

    let error = session
        .client
        .resume(
            session.pid,
            ResumePayload {
                unit: unit.clone(),
                modules: Vec::new(),
            },
            Some(keep.clone()),
        )
        .expect_err("a unit naming unheld modules must be refused");
    let quiver_cli::client::RequestError::Http { status, body } = error else {
        panic!("expected an HTTP error, got {error}");
    };
    assert_eq!(status, 424);
    let missing: MissingModules = serde_json::from_str(&body).expect("a MissingModules body");
    let expected: std::collections::HashSet<u64> = modules.iter().map(|(key, _)| *key).collect();
    assert!(
        missing.missing.iter().all(|key| expected.contains(key)),
        "the server must name keys the line actually imports"
    );
    assert!(!missing.missing.is_empty());

    // The retry carries them and succeeds.
    let outcome = session
        .client
        .resume(session.pid, ResumePayload { unit, modules }, Some(keep))
        .expect("retry failed");
    match outcome {
        Outcome::Value { rendered, .. } => assert_eq!(rendered, "42"),
        other => panic!("expected 42, got {other:?}"),
    }
}
