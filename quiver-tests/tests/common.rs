use quiver::spawn_worker;
use quiver_compiler::PackageResolver;
use quiver_core::value::Value;
use quiver_environment::{Environment, Repl, ReplError, WorkerHandle};
use quiver_io::NativeEffect;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

type ReplResult = Result<Option<(Value, Vec<Vec<u8>>)>, ReplError>;

/// Evaluate source and return a TestResult
fn evaluate(
    mut environment: Environment<NativeEffect>,
    mut repl: Repl<NativeEffect>,
    virtual_time: Arc<AtomicU64>,
    source: &str,
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
            let timeout = std::time::Duration::from_secs(5);

            loop {
                let did_work = environment.step().unwrap_or(false);

                if !did_work {
                    virtual_time.fetch_add(1, Ordering::Relaxed);
                }

                match environment.poll_request(request_id) {
                    Ok(Some(quiver_environment::RequestResult::Result(Ok((value, heap)), _))) => {
                        break Ok(Some((value, heap)));
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

#[allow(dead_code)]
pub struct TestBuilder {
    modules: HashMap<Vec<String>, String>,
    with_io: bool,
}

#[allow(dead_code)]
impl Default for TestBuilder {
    fn default() -> Self {
        // The standard library is a built-in package (embedded in quiver-compiler), so tests
        // start with no in-memory modules — only those a test adds via `with_modules`.
        Self {
            modules: HashMap::new(),
            with_io: false,
        }
    }
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

    pub fn evaluate(self, source: &str) -> TestResult {
        // Initialize virtual time for testing
        let virtual_time_ms = Arc::new(AtomicU64::new(0));

        // Build builtin registry from core modules
        let mut builtins = quiver_core::builtins::BuiltinRegistry::<NativeEffect>::with_modules(
            &quiver_core::builtins::core_modules(),
        );

        // Add I/O builtins if I/O is enabled
        if self.with_io {
            quiver_io::register_network_builtins(&mut builtins);
            quiver_io::register_file_builtins(&mut builtins);
        }

        // Create workers with virtual time function
        let num_workers = 2;
        let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
        for i in 0..num_workers {
            let time = virtual_time_ms.clone();
            let builtins_clone = builtins.clone();

            workers.push(Box::new(spawn_worker(
                move || time.load(Ordering::Relaxed),
                builtins_clone,
                false, // Don't enable profiling in tests
                i as u16,
            )));
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

        // Set the effect backend
        if let Some(backend) = effect_backend {
            environment.set_effect_backend(backend);
        }
        let resolver = Box::new(PackageResolver::memory(self.modules));
        let repl = Repl::new(&mut environment, resolver, builtins).expect("Failed to create REPL");

        evaluate(environment, repl, virtual_time_ms, source)
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
            Ok(Some((ref value, ref heap_data))) => {
                let actual = self.environment.format_value(value, heap_data);
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
                    // Check the inferred type
                    let actual = self.environment.format_type(&self.last_result_type);
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
        evaluate(self.environment, self.repl, self.virtual_time, source)
    }
}

#[allow(dead_code)]
pub fn quiver() -> TestBuilder {
    TestBuilder::new()
}
