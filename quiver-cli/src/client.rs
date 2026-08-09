//! The client side of the server API: a hand-rolled HTTP/1.1 client over the unix
//! socket (one short-lived connection per request — `Connection: close` — which is
//! all a local API needs), plus connect-else-spawn and takeover.
//!
//! The spawn race needs no lock: the listener bind is the arbiter. Two clients that
//! both spawn a server race the bind; the loser server exits on `AddrInUse` and its
//! client's retries land on the winner.
//!
//! Takeover is deliberately warn-not-ask: a fingerprint mismatch means this binary
//! was rebuilt, everything the old server holds is fingerprint-scoped anyway, and
//! stopping it kills the processes it hosts — the client says so and moves on.
//! `POST /shutdown` is fingerprint-exempt precisely so a newer client can stop a
//! stale server; SIGTERM via the pidfile is the fallback for one too old to speak
//! HTTP at all.

use crate::protocol::{
    CompactRequest, CreateResponse, FINGERPRINT_HEADER, Outcome, ResumeRequest, StatusResponse,
    pidfile_path,
};
use std::io::{BufRead, BufReader, Read, Write};
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

/// How long to keep retrying after spawning a server before giving up.
const SPAWN_DEADLINE: Duration = Duration::from_secs(10);
/// How long a stale server gets to exit after `Shutdown` before SIGTERM, and after
/// SIGTERM before giving up.
const STOP_DEADLINE: Duration = Duration::from_secs(5);

#[derive(Debug)]
pub enum ConnectError {
    /// No server: the socket is missing or nothing is listening on it.
    Absent,
    /// A server answered, but with a different build's fingerprint.
    Incompatible {
        fingerprint: String,
    },
    Io(std::io::Error),
}

impl std::fmt::Display for ConnectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConnectError::Absent => write!(f, "no server is listening"),
            ConnectError::Incompatible { fingerprint } => {
                write!(
                    f,
                    "server has a different build (fingerprint {fingerprint})"
                )
            }
            ConnectError::Io(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for ConnectError {}

impl From<std::io::Error> for ConnectError {
    fn from(e: std::io::Error) -> Self {
        match e.kind() {
            std::io::ErrorKind::NotFound | std::io::ErrorKind::ConnectionRefused => {
                ConnectError::Absent
            }
            _ => ConnectError::Io(e),
        }
    }
}

/// Wire tracing, off unless `QUIV_TRACE` is set: `1` for a line per request with its
/// body size, `full` to dump the bodies as well. The only way to see what a resume
/// actually carries — a line's own code, or that plus the units of modules this server
/// has not been given yet.
#[derive(Clone, Copy, PartialEq)]
enum Trace {
    Off,
    Sizes,
    Full,
}

impl Trace {
    fn current() -> Self {
        match std::env::var("QUIV_TRACE").as_deref() {
            Ok("full") => Trace::Full,
            Ok("" | "0") | Err(_) => Trace::Off,
            Ok(_) => Trace::Sizes,
        }
    }

    fn request(self, method: &str, path: &str, body: Option<&[u8]>) {
        if self == Trace::Off {
            return;
        }
        let body = body.unwrap_or(&[]);
        eprintln!("→ {method} {path} {} B{}", body.len(), summarise(body));
        if self == Trace::Full && !body.is_empty() {
            eprintln!("{}", String::from_utf8_lossy(body));
        }
    }

    fn response(self, status: u16, body: &[u8]) {
        if self == Trace::Off {
            return;
        }
        eprintln!("← {status} {} B", body.len());
        if self == Trace::Full && !body.is_empty() {
            eprintln!("{}", String::from_utf8_lossy(body));
        }
    }
}

/// What a resume body is, without deserialising it: which payload arm, and how many
/// module units ride along.
fn summarise(body: &[u8]) -> String {
    let Ok(request) = serde_json::from_slice::<ResumeRequest>(body) else {
        return String::new();
    };
    match request.payload {
        crate::protocol::ResumePayload::Bytecode(bytecode) => {
            format!(
                "  bytecode: {} functions, {} types",
                bytecode.functions.len(),
                bytecode.types.len()
            )
        }
        crate::protocol::ResumePayload::Unit { unit, modules } => format!(
            "  unit: {} own functions, {} imports; {} module unit(s) attached",
            unit.functions.len(),
            unit.imports.iter().map(|(_, _, i)| i.len()).sum::<usize>(),
            modules.len()
        ),
    }
}

/// A non-2xx answer, or the transport failing under a request.
#[derive(Debug)]
pub enum RequestError {
    Http { status: u16, body: String },
    Io(std::io::Error),
}

impl std::fmt::Display for RequestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RequestError::Http { status, body } => write!(f, "server answered {status}: {body}"),
            RequestError::Io(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for RequestError {}

impl From<std::io::Error> for RequestError {
    fn from(e: std::io::Error) -> Self {
        RequestError::Io(e)
    }
}

impl RequestError {
    /// Whether this is the per-process resume serialization answering "busy".
    pub fn is_conflict(&self) -> bool {
        matches!(self, RequestError::Http { status: 409, .. })
    }
}

/// A handle on a server's API — just the socket path; every request is its own
/// connection, so the handle is freely cloneable across threads (a cancel comes from
/// a signal thread while the main thread blocks in a resume).
#[derive(Clone)]
pub struct Client {
    socket: PathBuf,
}

impl Client {
    pub fn new(socket: PathBuf) -> Self {
        Client { socket }
    }

    fn request(
        &self,
        method: &str,
        path: &str,
        body: Option<&[u8]>,
        fingerprint: bool,
    ) -> std::io::Result<(u16, Vec<u8>)> {
        let trace = Trace::current();
        trace.request(method, path, body);
        let mut stream = UnixStream::connect(&self.socket)?;
        let mut head = format!("{method} {path} HTTP/1.1\r\nHost: quiv\r\nConnection: close\r\n");
        if fingerprint {
            head.push_str(&format!(
                "{FINGERPRINT_HEADER}: {}\r\n",
                quiver_compiler::compiler_fingerprint()
            ));
        }
        match body {
            Some(body) => {
                head.push_str(&format!(
                    "Content-Type: application/json\r\nContent-Length: {}\r\n\r\n",
                    body.len()
                ));
                stream.write_all(head.as_bytes())?;
                stream.write_all(body)?;
            }
            None => {
                head.push_str("Content-Length: 0\r\n\r\n");
                stream.write_all(head.as_bytes())?;
            }
        }
        stream.flush()?;

        let mut reader = BufReader::new(stream);
        let mut status_line = String::new();
        reader.read_line(&mut status_line)?;
        let status: u16 = status_line
            .split_whitespace()
            .nth(1)
            .and_then(|code| code.parse().ok())
            .ok_or_else(|| std::io::Error::other(format!("bad status line: {status_line:?}")))?;
        // Headers: only content-length matters (the server answers ours with
        // `Connection: close`, so draining to EOF is also always correct).
        let mut content_length: Option<usize> = None;
        loop {
            let mut line = String::new();
            reader.read_line(&mut line)?;
            let line = line.trim_end();
            if line.is_empty() {
                break;
            }
            if let Some((name, value)) = line.split_once(':')
                && name.eq_ignore_ascii_case("content-length")
            {
                content_length = value.trim().parse().ok();
            }
        }
        let body = match content_length {
            Some(length) => {
                let mut body = vec![0; length];
                reader.read_exact(&mut body)?;
                body
            }
            None => {
                let mut body = Vec::new();
                reader.read_to_end(&mut body)?;
                body
            }
        };
        trace.response(status, &body);
        Ok((status, body))
    }

    fn json<T: for<'de> serde::Deserialize<'de>>(
        &self,
        method: &str,
        path: &str,
        body: Option<&[u8]>,
    ) -> Result<T, RequestError> {
        let (status, body) = self.request(method, path, body, true)?;
        if !(200..300).contains(&status) {
            return Err(RequestError::Http {
                status,
                body: String::from_utf8_lossy(&body).into_owned(),
            });
        }
        serde_json::from_slice(&body).map_err(|e| RequestError::Io(std::io::Error::other(e)))
    }

    fn expect_ok(&self, method: &str, path: &str, body: Option<&[u8]>) -> Result<(), RequestError> {
        let (status, body) = self.request(method, path, body, true)?;
        if !(200..300).contains(&status) {
            return Err(RequestError::Http {
                status,
                body: String::from_utf8_lossy(&body).into_owned(),
            });
        }
        Ok(())
    }

    /// `GET /status` — fingerprint-exempt, so any server answers.
    pub fn status(&self) -> Result<StatusResponse, ConnectError> {
        let (status, body) = self.request("GET", "/status", None, false)?;
        if status != 200 {
            return Err(ConnectError::Io(std::io::Error::other(format!(
                "status answered {status}"
            ))));
        }
        serde_json::from_slice(&body).map_err(|e| ConnectError::Io(std::io::Error::other(e)))
    }

    pub fn create_process(&self) -> Result<u64, RequestError> {
        let response: CreateResponse = self.json("POST", "/processes", None)?;
        Ok(response.id)
    }

    pub fn resume(
        &self,
        id: u64,
        payload: crate::protocol::ResumePayload,
        keep: Option<Vec<usize>>,
    ) -> Result<Outcome, RequestError> {
        let body = serde_json::to_vec(&ResumeRequest { payload, keep })
            .map_err(|e| RequestError::Io(std::io::Error::other(e)))?;
        self.json("POST", &format!("/processes/{id}/resume"), Some(&body))
    }

    pub fn compact(&self, id: u64, keep: Vec<usize>) -> Result<(), RequestError> {
        let body = serde_json::to_vec(&CompactRequest { keep })
            .map_err(|e| RequestError::Io(std::io::Error::other(e)))?;
        self.expect_ok("POST", &format!("/processes/{id}/compact"), Some(&body))
    }

    pub fn cancel(&self, id: u64) -> Result<(), RequestError> {
        self.expect_ok("POST", &format!("/processes/{id}/cancel"), None)
    }

    pub fn delete_process(&self, id: u64) -> Result<(), RequestError> {
        self.expect_ok("DELETE", &format!("/processes/{id}"), None)
    }

    /// Inspection text: `GET /processes`, `/processes/{id}`, `/workers`, `/workers/{id}`.
    pub fn inspect(&self, path: &str) -> Result<String, RequestError> {
        let (status, body) = self.request("GET", path, None, true)?;
        if !(200..300).contains(&status) {
            return Err(RequestError::Http {
                status,
                body: String::from_utf8_lossy(&body).into_owned(),
            });
        }
        Ok(String::from_utf8_lossy(&body).into_owned())
    }

    /// `POST /shutdown` — fingerprint-exempt, so a newer client can stop a stale
    /// server.
    pub fn shutdown(&self) -> std::io::Result<()> {
        let _ = self.request("POST", "/shutdown", None, false)?;
        Ok(())
    }
}

/// Connect and verify the build matches. Never spawns.
pub fn connect(socket: &Path) -> Result<Client, ConnectError> {
    let client = Client::new(socket.to_path_buf());
    let status = client.status()?;
    if status.fingerprint != quiver_compiler::compiler_fingerprint() {
        return Err(ConnectError::Incompatible {
            fingerprint: status.fingerprint,
        });
    }
    Ok(client)
}

/// Connect, spawning (or taking over) a server as needed. `exe` is the `quiv` binary
/// to spawn — the caller's own (`std::env::current_exe()`), so client and server can
/// never skew.
pub fn connect_or_spawn(socket: &Path, exe: &Path) -> Result<Client, ConnectError> {
    match connect(socket) {
        Ok(client) => return Ok(client),
        Err(ConnectError::Absent) => {}
        Err(ConnectError::Incompatible { fingerprint }) => {
            eprintln!(
                "quiv: restarting stale server (its build {fingerprint} does not match; \
                 its processes were lost)"
            );
            stop_server(socket)?;
        }
        Err(e) => return Err(e),
    }

    // Spawn failures are IO errors, never `Absent` — `Absent` would misread as "the
    // server just needs a moment".
    spawn_server(exe, socket).map_err(ConnectError::Io)?;

    // Retry until the spawned (or race-winning) server answers.
    let deadline = Instant::now() + SPAWN_DEADLINE;
    loop {
        match connect(socket) {
            Ok(client) => return Ok(client),
            Err(ConnectError::Absent) if Instant::now() < deadline => {
                std::thread::sleep(Duration::from_millis(50));
            }
            Err(ConnectError::Absent) => {
                return Err(ConnectError::Io(std::io::Error::other(
                    "spawned server never started listening (see server.log beside the socket)",
                )));
            }
            // A freshly spawned server shares this binary; skew here means something
            // else won the race and is itself stale — give up rather than loop.
            Err(e) => return Err(e),
        }
    }
}

/// Stop whatever is listening: `POST /shutdown`, then SIGTERM via the pidfile for a
/// server too old to understand it, waiting for the socket to go quiet.
pub fn stop_server(socket: &Path) -> Result<(), ConnectError> {
    let _ = Client::new(socket.to_path_buf()).shutdown();
    if wait_until_gone(socket, STOP_DEADLINE) {
        return Ok(());
    }
    if let Ok(pid_text) = std::fs::read_to_string(pidfile_path(socket))
        && let Ok(pid) = pid_text.trim().parse::<i32>()
    {
        unsafe { libc::kill(pid, libc::SIGTERM) };
        if wait_until_gone(socket, STOP_DEADLINE) {
            // SIGTERM gave the old server no chance to unlink its socket.
            let _ = std::fs::remove_file(socket);
            return Ok(());
        }
    }
    Err(ConnectError::Io(std::io::Error::other(
        "a stale server is listening and would not stop",
    )))
}

/// Whether the socket stopped answering within `deadline`.
fn wait_until_gone(socket: &Path, deadline: Duration) -> bool {
    let until = Instant::now() + deadline;
    while Instant::now() < until {
        if UnixStream::connect(socket).is_err() {
            return true;
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    false
}

/// Spawn a detached server on this socket, logging beside it. Losing a spawn race is
/// fine: the loser exits on the bind and the connect retries reach the winner.
fn spawn_server(exe: &Path, socket: &Path) -> std::io::Result<()> {
    // The log lives beside the socket, whose directory may not exist yet on first
    // use. Same rule as the server's own bind: lock down only a directory we create.
    if let Some(dir) = socket.parent()
        && !dir.exists()
    {
        std::fs::create_dir_all(dir)?;
        std::fs::set_permissions(dir, std::os::unix::fs::PermissionsExt::from_mode(0o700))?;
    }
    let log = socket.with_extension("log");
    let stdout = std::fs::File::create(&log)?;
    let stderr = stdout.try_clone()?;
    use std::os::unix::process::CommandExt;
    std::process::Command::new(exe)
        .arg("server")
        .arg("--socket")
        .arg(socket)
        .stdin(std::process::Stdio::null())
        .stdout(stdout)
        .stderr(stderr)
        // Its own process group: a Ctrl-C aimed at the spawning client's terminal
        // must not take the server with it.
        .process_group(0)
        .spawn()?;
    Ok(())
}
