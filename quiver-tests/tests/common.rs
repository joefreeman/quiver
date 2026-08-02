use quiver::spawn_worker;
use quiver_compiler::{ArtifactStore, PackageResolver};
use quiver_core::wire::WireValue;
use quiver_environment::{Environment, Repl, ReplError, WorkerHandle};
use quiver_io::NativeEffect;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

type ReplResult = Result<Option<WireValue>, ReplError>;

thread_local! {
    /// The artifact store for every non-scoped test session on this thread:
    /// content-addressed and disk-backed, so a module compiled by one test links
    /// everywhere else — across tests, binaries, and runs (keys carry the debug flag, so
    /// both compile modes share one store). Warming is organic: the first test to import a
    /// module compiles and saves it. Scoped (no-io) tests never carry a store: their
    /// capability model depends on io-referencing std modules failing to compile, which
    /// pre-built artifacts would defeat.
    ///
    /// Per-thread because an artifact holds a compile-time `Value`, whose payload is `Rc`.
    /// Sharing is through the cache *directory*, which is what made it shareable across
    /// test binaries and runs in the first place; only the in-memory map is now per-thread.
    static ARTIFACTS: Rc<ArtifactStore> = Rc::new(ArtifactStore::cache());
}

/// Evaluate source and return a TestResult
fn evaluate(
    mut environment: Environment<NativeEffect>,
    mut repl: Repl<NativeEffect>,
    virtual_time: Arc<AtomicU64>,
    source: &str,
    timeout: std::time::Duration,
) -> TestResult {
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
    let result = match repl.evaluate(&mut environment, source, process_types) {
        Ok(Some(request_id)) => {
            let start = std::time::Instant::now();

            loop {
                let did_work = environment.step().unwrap_or(false);

                if !did_work {
                    virtual_time.fetch_add(1, Ordering::Relaxed);
                }

                match environment.poll_request(request_id) {
                    Ok(Some(quiver_environment::RequestResult::Result(Ok(value), _))) => {
                        break Ok(Some(value));
                    }
                    Ok(Some(quiver_environment::RequestResult::Result(Err(e), _))) => {
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
        environment,
        repl,
        virtual_time,
        last_result_type,
    }
}

// The standard library is a built-in package (embedded in quiver-compiler), so tests start
// with no in-memory modules — only those a test adds via `with_modules`.
/// Which IO signature groups the host registers — a host's capability set, mirroring the real
/// ones. `Full` is the native CLI's; `SystemOnly` is the browser's (clocks and entropy, which
/// need no effect backend, but no filesystem or sockets); `None` is a host with no IO vocabulary
/// at all. Referencing a builtin outside the set is a compile error.
#[allow(dead_code)]
#[derive(Default, Clone, Copy, PartialEq)]
pub enum Capabilities {
    #[default]
    Full,
    SystemOnly,
    None,
}

#[allow(dead_code)]
#[derive(Default)]
pub struct TestBuilder {
    modules: HashMap<Vec<String>, String>,
    with_io: bool,
    capabilities: Capabilities,
    debug: bool,
    collection_threshold: Option<usize>,
    timeout: Option<std::time::Duration>,
    real_time: bool,
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

    /// A capability-scoped host with no io signatures at all — io-referencing code fails at
    /// compile time.
    pub fn scoped_no_io(mut self) -> Self {
        self.capabilities = Capabilities::None;
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

    /// Auto-trigger a process-reclamation round every `n` spawns.
    /// Lets a test exercise reclamation under load without a huge spawn count.
    pub fn with_collection_threshold(mut self, n: usize) -> Self {
        self.collection_threshold = Some(n);
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

    pub fn evaluate(self, source: &str) -> TestResult {
        // Initialize virtual time for testing
        let virtual_time_ms = Arc::new(AtomicU64::new(0));

        // Build builtin registry: the always-set plus the io signature union — the
        // harness compiles std modules that reference io builtins even without io;
        // `with_io` only attaches the native implementations.
        let mut builtins = quiver_core::builtins::BuiltinRegistry::<NativeEffect>::with_modules(
            &quiver_core::builtins::core_modules(),
        );
        match self.capabilities {
            Capabilities::Full => {
                for module in quiver_core::builtins::io_modules() {
                    module(&mut builtins);
                }
            }
            Capabilities::SystemOnly => {
                for module in quiver_core::builtins::system_modules() {
                    module(&mut builtins);
                }
                // The point of the group: implementations alone make it runnable.
                quiver_io::attach_system_builtins(&mut builtins);
            }
            Capabilities::None => {}
        }

        // Add I/O builtins if I/O is enabled
        if self.with_io {
            quiver_io::attach_network_builtins(&mut builtins);
            quiver_io::attach_file_builtins(&mut builtins);
            quiver_io::attach_system_builtins(&mut builtins);
        }

        // Create workers with virtual time function. The harness drives the environment itself
        // (advancing the stepped clock as it idles), so it never waits on the wake signal — the
        // workers still poke it, and its capacity of 1 keeps that harmless.
        let (waker, _wake) = quiver::native_transport::wake_channel();
        let num_workers = 2;
        let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
        for i in 0..num_workers {
            let builtins_clone = builtins.clone();

            if self.real_time {
                workers.push(Box::new(spawn_worker(
                    quiver::native_transport::SystemClock,
                    builtins_clone,
                    false, // Don't enable profiling in tests
                    i as u16,
                    waker.clone(),
                )));
            } else {
                workers.push(Box::new(spawn_worker(
                    quiver::native_transport::SteppedClock::new(virtual_time_ms.clone()),
                    builtins_clone,
                    false, // Don't enable profiling in tests
                    i as u16,
                    waker.clone(),
                )));
            }
        }

        // Create shared effect backend if enabled
        let effect_backend = if self.with_io {
            quiver_io::NativeEffectBackend::new(256)
                .ok()
                .map(|backend| {
                    Box::new(backend)
                        as Box<dyn quiver_core::effects::EffectBackend<E = NativeEffect>>
                })
        } else {
            None
        };

        // Create environment and REPL
        let mut environment = Environment::<NativeEffect>::new(workers);
        environment.set_runtime_declarations(builtins.runtime_declarations().clone());

        if let Some(threshold) = self.collection_threshold {
            environment.set_collection_threshold(threshold);
        }

        // Set the effect backend
        if let Some(backend) = effect_backend {
            environment.set_effect_backend(backend);
        }
        let resolver = Box::new(PackageResolver::memory(self.modules));
        let mut repl =
            Repl::new(&mut environment, resolver, builtins).expect("Failed to create REPL");
        // The shared store is keyed on a fingerprint that doesn't cover the registry, so only the
        // full-capability shape (what every other test builds) may use it.
        if self.capabilities == Capabilities::Full {
            repl.set_artifact_store(ARTIFACTS.with(Rc::clone));
        }
        if self.debug {
            repl.set_compile_options(quiver_compiler::compiler::CompileOptions {
                debug: true,
                source_name: "test".to_string(),
            });
        }

        let timeout = self
            .timeout
            .unwrap_or_else(|| std::time::Duration::from_secs(5));
        evaluate(environment, repl, virtual_time_ms, source, timeout)
    }
}

#[allow(dead_code)]
pub struct TestResult {
    result: ReplResult,
    source: String,
    environment: Environment<NativeEffect>,
    repl: Repl<NativeEffect>,
    virtual_time: Arc<AtomicU64>,
    last_result_type: quiver_core::types::Type,
}

#[allow(dead_code)]
impl TestResult {
    /// Expect a value matching the given Quiver syntax string representation
    pub fn expect(self, expected: &str) -> Self {
        match self.result {
            Ok(Some(ref value)) => {
                let actual = self.environment.format_value(value);
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
                let actual = self.environment.describe_origin(value);
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
            let actual = self.environment.describe_origin(value);
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

    /// Assert that compilation fails with a `TypeMismatch`, without pinning the rendered
    /// type strings (useful when they include large inferred unions).
    #[allow(dead_code)]
    pub fn expect_type_mismatch(self) {
        match self.result {
            Err(ReplError::Compiler(quiver_compiler::compiler::Error::TypeMismatch { .. })) => {}
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
        let variables = self.repl.get_variables();
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
        match self
            .repl
            .resolve_type_alias(&mut self.environment, alias_name)
        {
            Ok(type_id) => {
                // Use REPL's format_type_by_id since type IDs from TypeAliasDef::Resolved
                // are registered in the REPL's program, not the Environment's
                let actual = self.repl.format_type_by_id(type_id);
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
                    let actual = self.repl.format_type(&self.last_result_type);
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
        let time = self.virtual_time.load(Ordering::Relaxed);
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
        evaluate(
            self.environment,
            self.repl,
            self.virtual_time,
            source,
            std::time::Duration::from_secs(5),
        )
    }

    /// Run a full process-reclamation round to completion. Drives
    /// the pause/snapshot/sweep handshake across `step()`s until it settles. If an
    /// auto-triggered round is already in flight, this simply pumps it to completion.
    pub fn force_collection(mut self) -> Self {
        self.environment
            .start_collection()
            .expect("failed to start collection");
        let start = std::time::Instant::now();
        while self.environment.is_collecting() {
            let did_work = self.environment.step().unwrap_or(false);
            if !did_work {
                if start.elapsed() > std::time::Duration::from_secs(5) {
                    panic!(
                        "collection did not complete within 5s for source: {}",
                        self.source
                    );
                }
                std::thread::sleep(std::time::Duration::from_micros(10));
            }
        }
        self
    }

    /// Assert exactly `n` tombstones have been reclaimed in total across all rounds so far.
    pub fn expect_reclaimed(self, n: usize) -> Self {
        let actual = self.environment.reclaimed_total();
        assert_eq!(
            actual, n,
            "expected {n} reclaimed processes, got {actual} for source: {}",
            self.source
        );
        self
    }

    /// Assert at least `n` tombstones have been reclaimed in total across all rounds so far.
    pub fn expect_reclaimed_at_least(self, n: usize) -> Self {
        let actual = self.environment.reclaimed_total();
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
        let actual = self.environment.process_count();
        assert!(
            actual < n,
            "expected fewer than {n} tracked processes, got {actual} for source: {}",
            self.source
        );
        self
    }
}

#[allow(dead_code)]
pub fn quiver() -> TestBuilder {
    TestBuilder::new()
}
