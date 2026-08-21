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

impl Server {
    /// Start with the TCP listener on an ephemeral port (the bare-port `--listen`
    /// form, exercising the host default), answering the endpoint the server recorded
    /// beside the socket and the bearer token it minted.
    fn start_listening() -> (Self, String, String) {
        let server = Self::start(&["--listen", "0"]);
        let endpoint_file = server.socket.with_extension("http");
        let deadline = Instant::now() + Duration::from_secs(20);
        let endpoint = loop {
            if let Ok(endpoint) = std::fs::read_to_string(&endpoint_file) {
                break endpoint;
            }
            assert!(Instant::now() < deadline, "endpoint file never appeared");
            std::thread::sleep(Duration::from_millis(50));
        };
        let token = std::fs::read_to_string(server.socket.with_extension("token"))
            .expect("token file beside the socket");
        (server, endpoint, token)
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
        let _ = std::fs::remove_file(&self.socket);
        let _ = std::fs::remove_file(self.socket.with_extension("pid"));
        let _ = std::fs::remove_file(self.socket.with_extension("log"));
        let _ = std::fs::remove_file(self.socket.with_extension("http"));
        // The token file stays: persistence across restarts is its contract, and each
        // test's socket path is unique, so nothing accumulates beyond the scratch dir.
    }
}

/// Minimal HTTP/1.1 over TCP for listener tests — the client library speaks only the
/// unix socket. Answers `(status, body, lowercased headers)`.
fn http_request(
    endpoint: &str,
    method: &str,
    path: &str,
    headers: &[(&str, &str)],
    body: Option<&str>,
) -> (u16, String, Vec<(String, String)>) {
    let address = endpoint.strip_prefix("http://").expect("an http endpoint");
    let mut stream = std::net::TcpStream::connect(address).expect("connect");
    let mut request =
        format!("{method} {path} HTTP/1.1\r\nHost: {address}\r\nConnection: close\r\n");
    for (name, value) in headers {
        request.push_str(&format!("{name}: {value}\r\n"));
    }
    if let Some(body) = body {
        request.push_str(&format!(
            "Content-Type: application/json\r\nContent-Length: {}\r\n",
            body.len()
        ));
    }
    request.push_str("\r\n");
    if let Some(body) = body {
        request.push_str(body);
    }
    stream.write_all(request.as_bytes()).expect("write request");
    let mut response = Vec::new();
    stream.read_to_end(&mut response).expect("read response");
    let response = String::from_utf8_lossy(&response).into_owned();
    let (head, body) = response.split_once("\r\n\r\n").unwrap_or((&response, ""));
    let mut lines = head.lines();
    let status: u16 = lines
        .next()
        .and_then(|line| line.split_whitespace().nth(1))
        .and_then(|code| code.parse().ok())
        .expect("a status line");
    let headers = lines
        .filter_map(|line| line.split_once(": "))
        .map(|(name, value)| (name.to_ascii_lowercase(), value.to_string()))
        .collect();
    (status, body.to_string(), headers)
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
                    let value = session.evaluate_value("x = [x, 1] ~> __integer_add__ ~; x");
                    assert_eq!(value, (n + step + 1).to_string(), "session {n}");
                }
                // A run beside the session: its own root, one resume, delete.
                let client = server.client();
                let pid = client.create_process().expect("create failed");
                let outcome = client
                    .resume(
                        pid,
                        ResumePayload {
                            unit: program(&format!("#[] {{ [{n}, 1] ~> __integer_add__ ~ }}")),
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
                    unit: program("#[] { f = #[] { ^ [] }; f [] }"),
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
                    unit: program("#[] { f = #[] { ^ [] }; f [] }"),
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
            unit: program("#[] { 1 }"),
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
                    unit: program("#[] { f = #[] { ^ [] }; f [] }"),
                    modules: Vec::new(),
                },
                None,
            )
        }
    });
    std::thread::sleep(Duration::from_millis(300));

    // The server stays fully usable beside it...
    let mut session = Session::open(&server);
    assert_eq!(session.evaluate_value("[40, 2] ~> __integer_add__ ~"), "42");

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
                    session.evaluate(&format!(
                        "f = #'int {{ [$, {offset}] ~> __integer_add__ ~ }}"
                    ));
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
        quick_value(&client, "#[] { [40, 2] ~> __integer_add__ ~ }"),
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
    assert_eq!(quick_value(&client, "#[] { 1 }"), "1");
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
                assert_eq!(
                    quick_value(&client, &format!("#[] {{ {n} }}")),
                    n.to_string()
                );
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
        quick_value(&client, "#[] { [40, 2] ~> __integer_add__ ~ }"),
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
    sent: std::collections::HashSet<quiver_compiler::UnitKey>,
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
        Vec<(quiver_compiler::UnitKey, quiver_compiler::CompiledUnit)>,
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
fn an_invalid_unit_is_refused_and_the_server_survives() {
    // The trust boundary over the wire: a module whose content does not hash to the key
    // it was sent under is a defect of the request, refused as 422 — and the refusal
    // must leave the server serviceable, where the pre-validation era panicked under
    // the environment lock and poisoned it for every request after.
    let server = Server::start(&[]);
    let mut session = UnitSession::open(&server);
    let (unit, modules, keep) = session.compile("%num.mul [7, 6]");
    assert!(!modules.is_empty(), "the line must carry modules to tamper");

    let mut tampered = modules.clone();
    tampered[0].1.field_names.push("tampered".to_string());
    let error = session
        .client
        .resume(
            session.pid,
            ResumePayload {
                unit: unit.clone(),
                modules: tampered,
            },
            Some(keep.clone()),
        )
        .expect_err("a tampered module must be refused");
    let quiver_cli::client::RequestError::Http { status, body } = error else {
        panic!("expected an HTTP error, got {error}");
    };
    assert_eq!(status, 422);
    assert!(
        body.contains("hashes to"),
        "the refusal names the mismatch: {body}"
    );

    // The honest payload still succeeds on the same server.
    let outcome = session
        .client
        .resume(session.pid, ResumePayload { unit, modules }, Some(keep))
        .expect("the server must survive a refused request");
    match outcome {
        Outcome::Value { rendered, .. } => assert_eq!(rendered, "42"),
        other => panic!("expected 42, got {other:?}"),
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
    let expected: std::collections::HashSet<quiver_compiler::UnitKey> =
        modules.iter().map(|(key, _)| *key).collect();
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

#[test]
fn the_tcp_listener_requires_the_token_on_every_route() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_server, endpoint, token) = Server::start_listening();

    // No token: refused — including /status, which the socket exempts. A hostile page
    // must not even learn a server exists.
    let (status, ..) = http_request(&endpoint, "GET", "/status", &[], None);
    assert_eq!(status, 401);
    let (status, ..) = http_request(
        &endpoint,
        "GET",
        "/status",
        &[("Authorization", "Bearer wrong")],
        None,
    );
    assert_eq!(status, 401);

    // The minted token opens it; /status stays fingerprint-exempt as on the socket.
    let bearer = format!("Bearer {token}");
    let (status, body, _) = http_request(
        &endpoint,
        "GET",
        "/status",
        &[("Authorization", &bearer)],
        None,
    );
    assert_eq!(status, 200, "{body}");
    assert!(
        body.contains(quiver_compiler::compiler_fingerprint()),
        "{body}"
    );
}

#[test]
fn a_browser_preflight_is_answered_with_cors_and_private_network_headers() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_server, endpoint, _token) = Server::start_listening();
    let preflight = |origin: &str| {
        http_request(
            &endpoint,
            "OPTIONS",
            "/processes",
            &[
                ("Origin", origin),
                ("Access-Control-Request-Method", "POST"),
                (
                    "Access-Control-Request-Headers",
                    "authorization,content-type,x-quiver-fingerprint",
                ),
                ("Access-Control-Request-Private-Network", "true"),
            ],
            None,
        )
    };

    // Allowed origins are echoed, with the private-network answer newer Chrome
    // requires for a public page reaching loopback — and no token, since preflights
    // carry no credentials by design.
    for origin in ["https://quiver.run", "http://localhost:3000"] {
        let (status, _, headers) = preflight(origin);
        assert!(status < 300, "preflight for {origin} answered {status}");
        assert!(
            headers
                .iter()
                .any(|(name, value)| name == "access-control-allow-origin" && value == origin),
            "{origin}: {headers:?}"
        );
        assert!(
            headers.iter().any(|(name, value)| {
                name == "access-control-allow-private-network" && value == "true"
            }),
            "{origin}: {headers:?}"
        );
    }

    // A foreign origin gets no CORS grant.
    let (_, _, headers) = preflight("https://evil.example");
    assert!(
        !headers
            .iter()
            .any(|(name, _)| name == "access-control-allow-origin"),
        "{headers:?}"
    );
}

#[test]
fn an_evaluation_runs_end_to_end_over_tcp() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_server, endpoint, token) = Server::start_listening();
    let bearer = format!("Bearer {token}");
    let auth: Vec<(&str, &str)> = vec![
        ("Authorization", &bearer),
        (
            "x-quiver-fingerprint",
            quiver_compiler::compiler_fingerprint(),
        ),
    ];

    let (status, body, _) = http_request(&endpoint, "POST", "/processes", &auth, Some("{}"));
    assert_eq!(status, 200, "{body}");
    let created: quiver_cli::protocol::CreateResponse =
        serde_json::from_str(&body).expect("a CreateResponse");

    // Compile client-side exactly as a browser's compiler worker would; no store, so
    // the payload arrives fully inlined.
    let mut compiler = LineCompiler::new(
        Box::new(quiver_compiler::PackageResolver::inline()),
        quiver_cli::build_builtin_registry(),
    );
    let prepared = compiler.prepare("%num.mul [7, 6]").expect("prepare");
    let compiled = compiler.compile(prepared).expect("compile");
    let committed = compiler.commit_line(compiled).expect("executable code");
    let request = quiver_cli::protocol::ResumeRequest {
        payload: committed.payload.to_wire(),
        keep: Some(committed.keep_indices),
    };
    let (status, body, _) = http_request(
        &endpoint,
        "POST",
        &format!("/processes/{}/resume", created.id),
        &auth,
        Some(&serde_json::to_string(&request).expect("serialize")),
    );
    assert_eq!(status, 200, "{body}");
    let outcome: Outcome = serde_json::from_str(&body).expect("an Outcome");
    match outcome {
        Outcome::Value { rendered, .. } => assert_eq!(rendered, "42"),
        other => panic!("expected 42, got {other:?}"),
    }
}

#[test]
fn the_token_persists_across_restarts() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (server, _endpoint, first) = Server::start_listening();
    let socket = server.socket.clone();
    drop(server);
    // Same socket path, fresh server: the browser's stored token must still open it.
    let second = {
        let child = Command::new(env!("CARGO_BIN_EXE_quiv"))
            .arg("server")
            .arg("--socket")
            .arg(&socket)
            .args(["--listen", "127.0.0.1:0"])
            .spawn()
            .expect("failed to respawn quiv server");
        let mut server = Server { child, socket };
        let deadline = Instant::now() + Duration::from_secs(20);
        loop {
            if std::os::unix::net::UnixStream::connect(&server.socket).is_ok() {
                break;
            }
            assert!(Instant::now() < deadline, "server never restarted");
            std::thread::sleep(Duration::from_millis(50));
        }
        let token =
            std::fs::read_to_string(server.socket.with_extension("token")).expect("token file");
        server.child.kill().ok();
        let _ = std::fs::remove_file(server.socket.with_extension("token"));
        token
    };
    assert_eq!(first, second);
}

#[test]
fn a_non_loopback_listen_is_refused() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let socket = scratch_socket("quiv-test-nonloop");
    let output = Command::new(env!("CARGO_BIN_EXE_quiv"))
        .arg("server")
        .arg("--socket")
        .arg(&socket)
        .args(["--listen", "0.0.0.0:0"])
        .output()
        .expect("run quiv server");
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("loopback"),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let _ = std::fs::remove_file(&socket);
}

/// Read from an open SSE connection until `needle` appears in the accumulated text
/// (chunked-framing noise included, which substring search tolerates).
fn read_until(stream: &mut std::net::TcpStream, received: &mut String, needle: &str) {
    let deadline = Instant::now() + Duration::from_secs(10);
    let mut buffer = [0u8; 4096];
    while !received.contains(needle) {
        assert!(
            Instant::now() < deadline,
            "timed out waiting for {needle}; received so far: {received}"
        );
        match stream.read(&mut buffer) {
            Ok(0) => panic!("stream closed early; received: {received}"),
            Ok(read) => received.push_str(&String::from_utf8_lossy(&buffer[..read])),
            Err(e)
                if e.kind() == std::io::ErrorKind::WouldBlock
                    || e.kind() == std::io::ErrorKind::TimedOut => {}
            Err(e) => panic!("read error: {e}"),
        }
    }
}

/// Open `/events` with the interest set in `query`, answering the connected stream
/// (headers already sent, response not yet read).
fn open_events(endpoint: &str, token: &str, query: &str) -> std::net::TcpStream {
    let address = endpoint.strip_prefix("http://").expect("an http endpoint");
    let mut stream = std::net::TcpStream::connect(address).expect("connect");
    stream
        .set_read_timeout(Some(Duration::from_millis(300)))
        .expect("set timeout");
    write!(
        stream,
        "GET /events?{query} HTTP/1.1\r\nHost: {address}\r\nAuthorization: Bearer {token}\r\nx-quiver-fingerprint: {fingerprint}\r\n\r\n",
        fingerprint = quiver_compiler::compiler_fingerprint()
    )
    .expect("write request");
    stream
}

#[test]
fn the_events_stream_pushes_process_updates() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let (_server, endpoint, token) = Server::start_listening();
    let bearer = format!("Bearer {token}");
    let auth: Vec<(&str, &str)> = vec![
        ("Authorization", &bearer),
        (
            "x-quiver-fingerprint",
            quiver_compiler::compiler_fingerprint(),
        ),
    ];

    // The statuses stream opens with an initial snapshot — the subscription pushes one
    // on registration, so a (re)connecting client is complete from its first event.
    let mut statuses = open_events(&endpoint, &token, "processes=true");
    let mut received = String::new();
    read_until(&mut statuses, &mut received, "event: processes");

    // A process created through the ordinary API appears on the already-open stream.
    let (status, body, _) = http_request(&endpoint, "POST", "/processes", &auth, Some("{}"));
    assert_eq!(status, 200, "{body}");
    let created: quiver_cli::protocol::CreateResponse =
        serde_json::from_str(&body).expect("a CreateResponse");
    read_until(
        &mut statuses,
        &mut received,
        &format!("\"id\":{}", created.id),
    );

    // A second connection watches that process's detail, rendered server-side.
    let mut detail = open_events(&endpoint, &token, &format!("process={}", created.id));
    let mut received = String::new();
    read_until(&mut detail, &mut received, "event: process");
    read_until(&mut detail, &mut received, "\"persistent\":true");

    // An empty interest set is refused rather than held open silently.
    let (status, ..) = http_request(&endpoint, "GET", "/events", &auth, None);
    assert_eq!(status, 400);
}

#[test]
fn registry_rendezvous_across_sessions() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let server = Server::start(&[]);

    // Session A starts a service, detaches it (so it outlives A's teardown), and
    // registers it under a shared name.
    let mut a = Session::open(&server);
    assert_eq!(
        a.evaluate_value(
            "svc = @#[] { !'int ~> %num.mul [~, 2] } []; %proc.detach svc; \
             %registry.register [Shared, svc]"
        ),
        "Ok"
    );

    // Session B shares no bindings with A, but reaches the service by name — at the
    // process type it states, checked at lookup.
    let mut b = Session::open(&server);
    assert_eq!(
        b.evaluate_value("%registry.lookup<@'bin> Shared ~> =[]"),
        "Ok",
        "a lookup at the wrong send type must answer nil"
    );
    // The name is taken environment-wide: B's own registration answers nil.
    assert_eq!(
        b.evaluate_value("p = @#[] { !'int } []; %registry.register [Shared, p] ~> =[]"),
        "Ok"
    );

    // B reaches the (one-shot) service by name, serves an exchange — and the
    // service's termination frees the name for every session, deterministically
    // before B's await answers.
    assert_eq!(
        b.evaluate_value("%registry.lookup<@'int !'int> Shared ~> =(@'int !'int)q; q 21; !q"),
        "42"
    );
    assert_eq!(
        b.evaluate_value("%registry.lookup<@'int> Shared ~> =[]"),
        "Ok"
    );
}

#[test]
fn registry_names_free_when_session_teardown_kills_the_service() {
    let _serial = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let server = Server::start(&[]);

    // Session A registers an *owned* (not detached) service: deleting A's root tears
    // down its subtree, and the cascade's death must free the name for everyone.
    let mut a = Session::open(&server);
    assert_eq!(
        a.evaluate_value("svc = @#[] { !'int } []; %registry.register [Owned, svc]"),
        "Ok"
    );
    let mut b = Session::open(&server);
    assert_eq!(
        b.evaluate_value("%registry.lookup<@'int> Owned ~> { =[] => Missing | Found }"),
        "Found"
    );

    a.client.delete_process(a.pid).expect("delete failed");

    // Teardown is asynchronous: poll until the cascade's expiry frees the name.
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    loop {
        let value = b.evaluate_value("%registry.lookup<@'int> Owned ~> { =[] => Missing | Found }");
        if value == "Missing" {
            break;
        }
        assert!(
            std::time::Instant::now() < deadline,
            "the name was never freed (last: {value})"
        );
        std::thread::sleep(Duration::from_millis(50));
    }

    // The freed name is immediately reusable, from any session.
    assert_eq!(
        b.evaluate_value("mine = @#[] { !'int } []; %registry.register [Owned, mine]"),
        "Ok"
    );
}
