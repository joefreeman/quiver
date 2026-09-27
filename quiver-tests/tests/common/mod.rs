use quiver::spawn_worker;
use quiver_compiler::{ArtifactStore, PackageResolver};
use quiver_core::wire::WireValue;
use quiver_environment::{Environment, Repl, ReplError, WorkerHandle};
use quiver_io::NativeEffect;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

type ReplResult = Result<Option<WireValue>, ReplError>;

thread_local! {
    /// The artifact store for every test session on this thread: content-addressed and
    /// disk-backed, so a module compiled by one test links everywhere else — across tests,
    /// binaries, and runs (keys carry the debug flag, so both compile modes share one
    /// store). Warming is organic: the first test to import a module compiles and saves
    /// it. Capability shapes don't split the store: every registry carries the same
    /// universal signatures, so compilation is identical whatever a host attaches.
    ///
    /// Per-thread because an artifact holds a compile-time `Value`, whose payload is `Rc`.
    /// Sharing is through the cache *directory*, which is what made it shareable across
    /// test binaries and runs in the first place; only the in-memory map is now per-thread.
    static ARTIFACTS: Rc<ArtifactStore> = Rc::new(ArtifactStore::cache());
}

thread_local! {
    /// One environment per host shape, reused by every test on this thread that can share it.
    /// Building an environment is cheap; *filling* one is not — linking the standard library
    /// into a fresh environment costs tens of milliseconds per module closure, and a
    /// per-test environment pays that on every test that imports anything. A pooled
    /// environment already holds the modules, and `link_module_unit` skips a key it has, so
    /// the second test to import `%json` does none of the work the first one did.
    ///
    /// Per-thread for the same reason `ARTIFACTS` is: cargo runs tests on several threads,
    /// and an `Environment` is not `Sync`.
    static ENVIRONMENTS: RefCell<HashMap<HostShape, Pooled>> = RefCell::new(HashMap::new());
}

/// A pooled environment, with the clock its workers were spawned against.
struct Pooled {
    environment: Environment<NativeEffect>,
    virtual_time: Arc<AtomicU64>,
}

/// What a test needs from its host, and so which pooled environment can serve it. Only the
/// two things baked into the workers at spawn — the capability set they carry and whether io
/// is attached — separate one pool slot from another.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct HostShape {
    capabilities: Capabilities,
    with_io: bool,
}

/// The environment a test runs against, together with how it must be given back. Sharing is
/// the default; `Drop` stops the test's session process, which is what keeps one test's
/// leftovers — spawned processes, registry names, open resources — out of the next one.
struct Session {
    /// Taken only by `Drop`, handing the environment back to the pool.
    environment: Option<Environment<NativeEffect>>,
    repl: Repl<NativeEffect>,
    virtual_time: Arc<AtomicU64>,
    /// The pool slot to return to, or `None` for a private environment — which is simply
    /// dropped, shutting its workers down with it.
    shape: Option<HostShape>,
}

impl Session {
    fn environment(&self) -> &Environment<NativeEffect> {
        self.environment
            .as_ref()
            .expect("session environment taken")
    }

    fn environment_mut(&mut self) -> &mut Environment<NativeEffect> {
        self.environment
            .as_mut()
            .expect("session environment taken")
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        let (Some(mut environment), Some(shape)) = (self.environment.take(), self.shape) else {
            return;
        };
        // Ownership teardown cascades from the session process to everything the test
        // spawned, which is what frees its registry names and closes its resources. The stop
        // is a worker command and is deliberately not awaited: the next test's process-type
        // fetch travels the same channel behind it, so the worker has already done the
        // teardown by the time anything can observe the environment again.
        let _ = environment.stop_process(self.repl.process_id());
        let virtual_time = Arc::clone(&self.virtual_time);
        ENVIRONMENTS.with_borrow_mut(|pool| {
            pool.insert(
                shape,
                Pooled {
                    environment,
                    virtual_time,
                },
            );
        });
    }
}

/// The builtin registry for a host shape. Every shape carries the same universal signatures —
/// compilation is host-independent — and the shape decides only which implementations attach.
fn build_builtins(shape: HostShape) -> quiver_core::builtins::BuiltinRegistry<NativeEffect> {
    let mut builtins = quiver_core::builtins::BuiltinRegistry::<NativeEffect>::with_modules(
        &quiver_core::builtins::universal_modules(),
    );
    match shape.capabilities {
        // Full attaches via `with_io` below; None attaches nothing at all.
        Capabilities::Full | Capabilities::None => {}
        // The system builtins are synchronous host reads, so implementations alone make them
        // runnable — no effect backend. This is a browser's shape.
        Capabilities::SystemOnly | Capabilities::Web => {
            quiver_io::attach_system_builtins(&mut builtins);
        }
    }
    if shape.with_io {
        quiver_io::attach_network_builtins(&mut builtins);
        quiver_io::attach_file_builtins(&mut builtins);
        quiver_io::attach_system_builtins(&mut builtins);
        quiver_io::attach_tls_builtins(&mut builtins);
        quiver_io::attach_http_builtins(&mut builtins);
    }
    builtins
}

/// Build a host of the given shape: its workers, its effect backend, and the environment
/// binding them. `real_time` and `mock_io` are exactly the two things a pooled environment
/// cannot vary, because a clock is fixed when the workers spawn and a backend when it
/// attaches — which is why either one forces a private environment.
fn build_environment(
    shape: HostShape,
    real_time: bool,
    mock_io: Option<MockIo>,
) -> (Environment<NativeEffect>, Arc<AtomicU64>) {
    let virtual_time_ms = Arc::new(AtomicU64::new(0));
    let builtins = build_builtins(shape);

    // The harness drives the environment itself (advancing the stepped clock as it idles), so
    // it never waits on the wake signal — the workers still poke it, and its capacity of 1
    // keeps that harmless.
    let (waker, _wake) = quiver::native_transport::wake_channel();
    let num_workers = 2;
    let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
    for i in 0..num_workers {
        let builtins_clone = builtins.clone();

        if real_time {
            workers.push(Box::new(spawn_worker(
                quiver::native_transport::SystemClock,
                builtins_clone,
                i as u16,
                waker.clone(),
            )));
        } else {
            workers.push(Box::new(spawn_worker(
                quiver::native_transport::SteppedClock::new(virtual_time_ms.clone()),
                builtins_clone,
                i as u16,
                waker.clone(),
            )));
        }
    }

    let effect_backend: Option<Box<dyn quiver_core::effects::EffectBackend<E = NativeEffect>>> =
        match (mock_io, shape.with_io) {
            (Some(behaviour), _) => Some(Box::new(mock_io::MockBackend::new(behaviour))),
            (None, true) => quiver_io::NativeEffectBackend::new(256)
                .ok()
                .map(|backend| Box::new(backend) as Box<_>),
            (None, false) => None,
        };

    let mut environment = Environment::<NativeEffect>::new(workers);
    environment.set_runtime_declarations(builtins.runtime_declarations().clone());
    if let Some(backend) = effect_backend {
        environment.set_effect_backend(backend);
    }

    (environment, virtual_time_ms)
}

/// Evaluate source and return a TestResult
fn evaluate(mut session: Session, source: &str, timeout: std::time::Duration) -> TestResult {
    let Session {
        environment,
        repl,
        virtual_time,
        ..
    } = &mut session;
    let environment = environment.as_mut().expect("session environment taken");

    // Fetch process types
    let types_request_id = environment
        .request_process_types()
        .expect("Failed to request process types");

    let process_types = loop {
        environment.step().ok();
        match environment.poll_request(types_request_id) {
            Ok(Some(quiver_environment::RequestResult::ProcessTypes(types))) => break types,
            Ok(Some(_)) => panic!("Unexpected result type for process types request"),
            Ok(None) => continue,
            Err(e) => panic!("Failed to get process types: {:?}", e),
        }
    };

    // Evaluate and poll for result
    let result = match repl.evaluate(environment, source, process_types) {
        Ok(Some(request_id)) => {
            let start = std::time::Instant::now();

            loop {
                let did_work = environment.step().unwrap_or(false);

                if !did_work {
                    virtual_time.fetch_add(1, Ordering::Relaxed);
                }

                match environment.poll_request(request_id) {
                    Ok(Some(quiver_environment::RequestResult::Result(Ok(value)))) => {
                        break Ok(Some(value));
                    }
                    Ok(Some(quiver_environment::RequestResult::Result(Err(e)))) => {
                        break Err(ReplError::Runtime(e));
                    }
                    Ok(Some(_)) => {
                        panic!("Unexpected result type for evaluation request");
                    }
                    Ok(None) => {
                        if start.elapsed() > timeout {
                            break Err(ReplError::Environment(
                                quiver_environment::EnvironmentError::Timeout(timeout),
                            ));
                        }
                        std::thread::sleep(std::time::Duration::from_micros(10));
                    }
                    Err(e) => break Err(ReplError::Environment(e)),
                }
            }
        }
        Ok(None) => Ok(None),
        Err(e) => Err(e),
    };

    let last_result_type = repl.get_last_result_type().clone();

    TestResult {
        result,
        source: source.to_string(),
        session,
        last_result_type,
    }
}

// Dead-code allowances as for the builder itself: `common` is compiled into every test
// binary, and each binary uses only a slice of it — most construct no MockIo variant and
// start no TLS server.
#[allow(dead_code)]
pub mod mock_io;
pub use mock_io::MockIo;
#[allow(dead_code)]
pub mod tls_server;

// The standard library is a built-in package (embedded in quiver-compiler), so tests start
// with no in-memory modules — only those a test adds via `with_modules`.
/// Which IO implementations the host attaches — a host's capability set, mirroring the
/// real ones. `Full` is the native CLI's; `SystemOnly`/`Web` are the browser's (clocks and
/// entropy, which need no effect backend, but no filesystem or sockets); `None` attaches
/// nothing. Signatures are universal either way: a call outside the attached set errors at
/// runtime, never at compile time.
#[allow(dead_code)]
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Capabilities {
    #[default]
    Full,
    SystemOnly,
    /// The browser's: the system builtins plus `fetch`, and no sockets or filesystem.
    Web,
    None,
}

#[allow(dead_code)]
#[derive(Default)]
pub struct TestBuilder {
    modules: HashMap<Vec<String>, String>,
    with_io: bool,
    mock_io: Option<MockIo>,
    files: Option<HashMap<String, String>>,
    capabilities: Capabilities,
    debug: bool,
    compile_fuel: Option<u64>,
    compile_cancel: Option<Arc<std::sync::atomic::AtomicBool>>,
    collection_threshold: Option<usize>,
    code_collection_threshold: Option<usize>,
    timeout: Option<std::time::Duration>,
    real_time: bool,
    isolated: bool,
}

#[allow(dead_code)]
impl TestBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_modules(mut self, modules: HashMap<Vec<String>, String>) -> Self {
        self.modules = modules;
        self
    }

    pub fn with_io(mut self) -> Self {
        self.with_io = true;
        self
    }

    /// Real io builtins over a *faked OS*: the client, the codecs and the effect
    /// plumbing all run for real, and only the syscalls are canned. This is how the
    /// cases a live socket cannot reach reliably get tested — awkward read boundaries, a peer
    /// that never answers, a refused connect, an aborting operation.
    pub fn with_mock_io(mut self, behaviour: MockIo) -> Self {
        self.with_io = true;
        self.mock_io = Some(behaviour);
        self
    }

    /// An in-memory package addressed by *file path* rather than module name — the shape
    /// the web host uses.
    pub fn with_files(mut self, files: &[(&str, &str)]) -> Self {
        self.files = Some(
            files
                .iter()
                .map(|(path, source)| (path.to_string(), source.to_string()))
                .collect(),
        );
        self
    }

    /// A host with no io implementations attached at all — io-referencing code compiles
    /// (signatures are universal) and errors only when a call is actually reached.
    pub fn scoped_no_io(mut self) -> Self {
        self.capabilities = Capabilities::None;
        self
    }

    /// The browser's capability shape — the system builtins attached, no sockets or
    /// filesystem.
    pub fn scoped_web(mut self) -> Self {
        self.capabilities = Capabilities::Web;
        self
    }

    /// The web host's capability set: the system builtins (clocks, entropy) with real
    /// implementations attached, and nothing else. They are synchronous host reads, so they need
    /// no effect backend — which is exactly why a browser can serve them.
    pub fn scoped_system_only(mut self) -> Self {
        self.capabilities = Capabilities::SystemOnly;
        self
    }

    /// Compile in debug mode: nil results carry failure-provenance `origin` stamps.
    pub fn debug(mut self) -> Self {
        self.debug = true;
        self
    }

    /// Cap compile-time evaluation (module bodies, dialect expansion) at `fuel` step
    /// units, so a test can exercise the budget without burning the generous default.
    pub fn with_compile_fuel(mut self, fuel: u64) -> Self {
        self.compile_fuel = Some(fuel);
        self
    }

    /// Attach a cancellation flag to compile-time evaluation, polled between execution
    /// slices.
    pub fn with_compile_cancel(mut self, cancel: Arc<std::sync::atomic::AtomicBool>) -> Self {
        self.compile_cancel = Some(cancel);
        self
    }

    /// Auto-trigger a process-reclamation round every `n` spawns.
    /// Lets a test exercise reclamation under load without a huge spawn count.
    pub fn with_collection_threshold(mut self, n: usize) -> Self {
        self.collection_threshold = Some(n);
        self
    }

    /// Include a code phase in a reclamation round once `n` functions/constants have
    /// been registered since the last sweep — the auto-trigger, at test scale.
    pub fn with_code_collection_threshold(mut self, n: usize) -> Self {
        self.code_collection_threshold = Some(n);
        self
    }

    /// Raise the evaluation timeout (default 5s) for tests with real waits — wall-clock
    /// sleeps, socket round-trips — that can exceed it under a loaded test machine.
    pub fn with_timeout(mut self, timeout: std::time::Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    /// Run on the real clock instead of the free-running virtual one. The virtual
    /// clock advances ~1 virtual ms per idle step (orders of magnitude faster than
    /// wall time), which turns every timed wait in a test doing REAL I/O into a race
    /// between two clocks — a `![100]` drop-observation wait elapses in ~1ms of real
    /// time. Real-I/O integration tests should use this; pure tests keep the virtual
    /// clock (it is what makes timeout-heavy tests instant).
    pub fn with_real_time(mut self) -> Self {
        self.real_time = true;
        self
    }

    /// Give this test an environment of its own rather than the pooled one. Needed by tests
    /// that read environment-wide counters (reclaimed processes, code sweeps, the virtual
    /// clock), which a neighbouring test sharing the environment would perturb.
    pub fn isolated(mut self) -> Self {
        self.isolated = true;
        self
    }

    /// The pool slot this test can share, or `None` if it must have the environment to
    /// itself. A clock is fixed when the workers spawn and an effect backend when it
    /// attaches, so `real_time` and `mock_io` can never share; reclamation settings and the
    /// counters that go with them are environment-wide state, so they must not.
    fn shape(&self) -> Option<HostShape> {
        let shareable = self.mock_io.is_none()
            && !self.real_time
            && !self.isolated
            && self.collection_threshold.is_none()
            && self.code_collection_threshold.is_none();
        shareable.then_some(HostShape {
            capabilities: self.capabilities,
            with_io: self.with_io,
        })
    }

    pub fn evaluate(self, source: &str) -> TestResult {
        let shape = self.shape();
        let (mut environment, virtual_time_ms) = match shape {
            // A pooled environment arrives with the standard library already linked, which is
            // the whole saving; an empty slot builds one and the next test inherits it.
            Some(shape) => match ENVIRONMENTS.with_borrow_mut(|pool| pool.remove(&shape)) {
                Some(pooled) => (pooled.environment, pooled.virtual_time),
                None => build_environment(shape, false, None),
            },
            None => build_environment(
                HostShape {
                    capabilities: self.capabilities,
                    with_io: self.with_io,
                },
                self.real_time,
                self.mock_io.clone(),
            ),
        };

        // Only ever a private environment: setting either forces `shape()` to `None`.
        if let Some(threshold) = self.collection_threshold {
            environment.set_collection_threshold(threshold);
        }
        if let Some(threshold) = self.code_collection_threshold {
            environment.set_code_collection_threshold(threshold);
        }

        let builtins = build_builtins(HostShape {
            capabilities: self.capabilities,
            with_io: self.with_io,
        });
        let resolver = match self.files {
            Some(files) => {
                PackageResolver::memory_files(files).expect("in-memory files must be valid")
            }
            None => PackageResolver::memory(self.modules),
        };
        let resolver = Box::new(resolver);
        let mut repl =
            Repl::new(&mut environment, resolver, builtins).expect("Failed to create REPL");
        // Every capability shape compiles against the same universal signatures, so all
        // of them may share the artifact store — implementations don't reach compilation.
        repl.set_artifact_store(ARTIFACTS.with(Rc::clone));
        if self.debug || self.compile_fuel.is_some() || self.compile_cancel.is_some() {
            let mut options = quiver_compiler::compiler::CompileOptions {
                debug: self.debug,
                source_name: "test".to_string(),
                ..Default::default()
            };
            if let Some(fuel) = self.compile_fuel {
                options.fuel = fuel;
            }
            options.cancel = self.compile_cancel.clone();
            repl.set_compile_options(options);
        }

        let timeout = self
            .timeout
            .unwrap_or_else(|| std::time::Duration::from_secs(5));
        let session = Session {
            environment: Some(environment),
            repl,
            virtual_time: virtual_time_ms,
            shape,
        };
        evaluate(session, source, timeout)
    }
}

#[allow(dead_code)]
pub struct TestResult {
    result: ReplResult,
    source: String,
    session: Session,
    last_result_type: quiver_core::types::Type,
}

#[allow(dead_code)]
impl TestResult {
    /// The formatted result, for comparing two evaluations against each other rather than
    /// against a literal — e.g. asserting two hosts agree.
    pub fn value_string(&self) -> String {
        match &self.result {
            Ok(Some(value)) => self.session.environment().format_value(value),
            Ok(None) => String::new(),
            Err(e) => panic!("expected a value, got {:?} for source: {}", e, self.source),
        }
    }

    /// Expect a value matching the given Quiver syntax string representation
    pub fn expect(self, expected: &str) -> Self {
        match self.result {
            Ok(Some(ref value)) => {
                let actual = self.session.environment().format_value(value);
                assert_eq!(
                    actual, expected,
                    "Expected '{}', got '{}' for source: {}",
                    expected, actual, self.source
                );
            }
            Ok(None) => {
                // No executable code (e.g., only type definitions)
                assert_eq!(
                    "", expected,
                    "Expected '{}', got no result (type definitions only) for source: {}",
                    expected, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Expected value '{}', got error: {:?} for source: {}",
                    expected, e, self.source
                );
            }
        }
        self
    }

    /// Expect the result to carry a failure-provenance origin rendering as `expected`
    /// (e.g. `"match failed at test:1:8"`). Debug builds only.
    pub fn expect_origin(self, expected: &str) -> Self {
        match self.result {
            Ok(Some(ref value)) => {
                let actual = self.session.environment().describe_failure(value);
                assert_eq!(
                    actual.as_deref(),
                    Some(expected),
                    "for source: {}",
                    self.source
                );
            }
            ref other => panic!(
                "Expected origin '{}', got {:?} for source: {}",
                expected, other, self.source
            ),
        }
        self
    }

    /// Expect the result to carry no failure-provenance origin (release builds, or nil
    /// in a non-result position).
    pub fn expect_no_origin(self) -> Self {
        if let Ok(Some(ref value)) = self.result {
            let actual = self.session.environment().describe_failure(value);
            assert_eq!(actual, None, "for source: {}", self.source);
        }
        self
    }

    pub fn expect_runtime_error(self, expected: quiver_core::error::Error) {
        match self.result {
            Ok(result) => {
                panic!(
                    "Expected runtime error {:?}, but evaluation succeeded with result: {:?} for source: {}",
                    expected, result, self.source
                );
            }
            Err(ReplError::Runtime(actual)) => {
                assert_eq!(
                    actual, expected,
                    "Expected runtime error {:?}, but got {:?} for source: {}",
                    expected, actual, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Expected runtime error {:?}, but got {:?} for source: {}",
                    expected, e, self.source
                );
            }
        }
    }

    pub fn expect_compile_error(self, expected: quiver_compiler::compiler::Error) {
        match self.result {
            Ok(result) => {
                panic!(
                    "Expected compile error {:?}, but evaluation succeeded with result: {:?} for source: {}",
                    expected, result, self.source
                );
            }
            Err(ReplError::Compiler(actual)) => {
                let actual = actual.error;
                assert_eq!(
                    actual, expected,
                    "Expected compile error {:?}, but got {:?} for source: {}",
                    expected, actual, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Expected compile error {:?}, but got {:?} for source: {}",
                    expected, e, self.source
                );
            }
        }
    }

    /// Assert that compilation fails, handing back the located error for assertions on
    /// where it is.
    pub fn expect_located_compile_error(self) -> quiver_compiler::compiler::LocatedError {
        match self.result {
            Err(ReplError::Compiler(error)) => error,
            Ok(result) => panic!(
                "Expected a compile error, but evaluation succeeded with: {:?} for source: {}",
                result, self.source
            ),
            Err(e) => panic!(
                "Expected a compile error, but got {:?} for source: {}",
                e, self.source
            ),
        }
    }

    /// Assert that compilation fails with a `TypeMismatch`, without pinning the rendered
    /// type strings (useful when they include large inferred unions).
    #[allow(dead_code)]
    pub fn expect_type_mismatch(self) {
        match self.result {
            Err(ReplError::Compiler(e))
                if matches!(
                    e.error,
                    quiver_compiler::compiler::Error::TypeMismatch { .. }
                ) => {}
            Ok(result) => panic!(
                "Expected a type mismatch, but evaluation succeeded with: {:?} for source: {}",
                result, self.source
            ),
            Err(e) => panic!(
                "Expected a type mismatch, but got {:?} for source: {}",
                e, self.source
            ),
        }
    }

    /// Assert that compilation fails with an error whose rendering contains `needle` —
    /// for asserting notes/hints without pinning the full error value.
    pub fn expect_error_containing(self, needle: &str) {
        match self.result {
            Err(ReplError::Compiler(e)) => {
                let rendered = format!("{e}");
                assert!(
                    rendered.contains(needle),
                    "Expected a compile error containing {needle:?}, but got: {rendered} for source: {}",
                    self.source
                );
            }
            Ok(result) => panic!(
                "Expected a compile error containing {needle:?}, but evaluation succeeded with: {:?} for source: {}",
                result, self.source
            ),
            Err(e) => panic!(
                "Expected a compile error containing {needle:?}, but got {:?} for source: {}",
                e, self.source
            ),
        }
    }

    /// Assert that the source fails to parse (any parse error), without pinning the
    /// exact error value.
    pub fn expect_parse_failure(self) {
        match self.result {
            Err(ReplError::Parser(_)) => {}
            Ok(result) => panic!(
                "Expected a parse error, but evaluation succeeded with: {:?} for source: {}",
                result, self.source
            ),
            Err(e) => panic!(
                "Expected a parse error, but got {:?} for source: {}",
                e, self.source
            ),
        }
    }

    pub fn expect_parse_error(self, expected: quiver_compiler::parser::Error) {
        match self.result {
            Ok(result) => {
                panic!(
                    "Expected parse error {:?}, but evaluation succeeded with result: {:?} for source: {}",
                    expected, result, self.source
                );
            }
            Err(ReplError::Parser(boxed_actual)) => {
                let actual = *boxed_actual;
                assert_eq!(
                    actual, expected,
                    "Expected parse error {:?}, but got {:?} for source: {}",
                    expected, actual, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Expected parse error {:?}, but got {:?} for source: {}",
                    expected, e, self.source
                );
            }
        }
    }

    pub fn expect_variable(self, variable_name: &str, expected: &str) -> Self {
        // Get variable type from the repl
        let variables = self.session.repl.get_variables();
        let variable_type = variables
            .iter()
            .find(|(name, _)| name == variable_name)
            .map(|(_, ty)| ty);

        match variable_type {
            Some(actual) => {
                assert_eq!(
                    actual, expected,
                    "Expected variable '{}' to have type '{}', but got '{}' for source: {}",
                    variable_name, expected, actual, self.source
                );
            }
            None => {
                panic!(
                    "Variable '{}' not found. Available variables: {:?} for source: {}",
                    variable_name,
                    variables.iter().map(|(n, _)| n).collect::<Vec<_>>(),
                    self.source
                );
            }
        }
        self
    }

    pub fn expect_alias(mut self, alias_name: &str, expected: &str) -> Self {
        let Session {
            environment, repl, ..
        } = &mut self.session;
        let environment = environment.as_mut().expect("session environment taken");
        match repl.resolve_type_alias(environment, alias_name) {
            Ok(type_id) => {
                // Use REPL's format_type_by_id since type IDs from TypeAliasDef::Resolved
                // are registered in the REPL's program, not the Environment's
                let actual = self.session.repl.format_type_by_id(type_id);
                assert_eq!(
                    actual, expected,
                    "Expected type alias '{}' to resolve to '{}', but got '{}' for source: {}",
                    alias_name, expected, actual, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Failed to resolve type alias '{}': {} for source: {}",
                    alias_name, e, self.source
                );
            }
        }
        self
    }

    pub fn expect_type(self, expected: &str) -> Self {
        match self.result {
            Ok(Some(_)) => {
                if expected.is_empty() {
                    panic!(
                        "Expected no executable code (type definitions only), but got a result for source: {}",
                        self.source
                    );
                } else {
                    // Check the inferred type (a compiler-side type — REPL id space)
                    let actual = self.session.repl.format_type(&self.last_result_type);
                    assert_eq!(
                        actual, expected,
                        "Expected result type '{}', got '{}' for source: {}",
                        expected, actual, self.source
                    );
                }
            }
            Ok(None) => {
                if expected.is_empty() {
                    // Success - no executable code as expected
                } else {
                    panic!(
                        "Expected result type '{}', but got no result (type definitions only) for source: {}",
                        expected, self.source
                    );
                }
            }
            Err(e) => {
                panic!(
                    "Expected result type '{}', got error: {:?} for source: {}",
                    expected, e, self.source
                );
            }
        }
        self
    }

    pub fn expect_duration(self, min_ms: u64, max_ms: u64) -> Self {
        let time = self.session.virtual_time.load(Ordering::Relaxed);
        assert!(
            time >= min_ms && time <= max_ms,
            "Expected virtual time between {}ms and {}ms, but got {}ms for source: {}",
            min_ms,
            max_ms,
            time,
            self.source
        );
        self
    }

    /// Evaluate another expression, chaining from the previous evaluation
    pub fn then_evaluate(self, source: &str) -> Self {
        evaluate(self.session, source, std::time::Duration::from_secs(5))
    }

    /// Run a full process-reclamation round to completion. Drives
    /// the pause/snapshot/sweep handshake across `step()`s until it settles. If an
    /// auto-triggered round is already in flight, this simply pumps it to completion.
    pub fn force_collection(mut self) -> Self {
        let source = self.source.clone();
        let environment = self.session.environment_mut();
        environment
            .start_collection()
            .expect("failed to start collection");
        pump_collection(environment, &source);
        self
    }

    /// Run a reclamation round *with a code phase* to completion. If a round is
    /// already in flight it is pumped first (the code request then applies to a fresh
    /// round), so this always sweeps.
    pub fn force_code_collection(mut self) -> Self {
        let source = self.source.clone();
        let environment = self.session.environment_mut();
        loop {
            let started = environment
                .start_code_collection()
                .expect("failed to start code collection");
            pump_collection(environment, &source);
            if started {
                break;
            }
        }
        self
    }

    /// Assert at least `functions` function slots have been reclaimed by code sweeps
    /// so far.
    pub fn expect_code_reclaimed_at_least(self, functions: usize) -> Self {
        let (actual, _constants) = self.session.environment().code_reclaimed_totals();
        assert!(
            actual >= functions,
            "expected at least {functions} reclaimed functions, got {actual} for source: {}",
            self.source
        );
        self
    }

    /// Assert no code has been reclaimed by any sweep so far.
    pub fn expect_no_code_reclaimed(self) -> Self {
        let totals = self.session.environment().code_reclaimed_totals();
        assert_eq!(
            totals,
            (0, 0),
            "expected no reclaimed code, got {totals:?} for source: {}",
            self.source
        );
        self
    }

    /// Assert exactly `n` tombstones have been reclaimed in total across all rounds so far.
    pub fn expect_reclaimed(self, n: usize) -> Self {
        let actual = self.session.environment().reclaimed_total();
        assert_eq!(
            actual, n,
            "expected {n} reclaimed processes, got {actual} for source: {}",
            self.source
        );
        self
    }

    /// Assert at least `n` tombstones have been reclaimed in total across all rounds so far.
    pub fn expect_reclaimed_at_least(self, n: usize) -> Self {
        let actual = self.session.environment().reclaimed_total();
        assert!(
            actual >= n,
            "expected at least {n} reclaimed processes, got {actual} for source: {}",
            self.source
        );
        self
    }

    /// Assert the environment tracks fewer than `n` processes (live plus unreclaimed
    /// tombstones) — i.e. reclamation kept the population bounded.
    pub fn expect_process_count_below(self, n: usize) -> Self {
        let actual = self.session.environment().process_count();
        assert!(
            actual < n,
            "expected fewer than {n} tracked processes, got {actual} for source: {}",
            self.source
        );
        self
    }
}

/// Drive an in-flight reclamation round to completion.
fn pump_collection(environment: &mut Environment<NativeEffect>, source: &str) {
    let start = std::time::Instant::now();
    while environment.is_collecting() {
        let did_work = environment.step().unwrap_or(false);
        if !did_work {
            if start.elapsed() > std::time::Duration::from_secs(5) {
                panic!("collection did not complete within 5s for source: {source}");
            }
            std::thread::sleep(std::time::Duration::from_micros(10));
        }
    }
}

#[allow(dead_code)]
pub fn quiver() -> TestBuilder {
    TestBuilder::new()
}
