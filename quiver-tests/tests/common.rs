use quiver::spawn_worker;
use quiver_compiler::modules::InMemoryModuleLoader;
use quiver_core::program::Program;
use quiver_core::value::Value;
use quiver_environment::{Repl, ReplError, WorkerHandle};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

type ReplResult = Result<Option<(Value, Vec<Vec<u8>>)>, ReplError>;

macro_rules! load_stdlib_modules {
    ($($name:literal),* $(,)?) => {{
        let mut modules = HashMap::new();
        $(
            modules.insert(
                $name.to_string(),
                include_str!(concat!("../../std/", $name, ".qv")).to_string()
            );
        )*
        modules
    }};
}

#[allow(dead_code)]
pub struct TestBuilder {
    modules: HashMap<String, String>,
}

#[allow(dead_code)]
impl Default for TestBuilder {
    fn default() -> Self {
        Self {
            modules: load_stdlib_modules!("math", "list"),
        }
    }
}

#[allow(dead_code)]
impl TestBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_modules(mut self, mut modules: HashMap<String, String>) -> Self {
        // Merge additional modules with standard library modules
        for (key, value) in self.modules {
            modules.entry(key).or_insert(value);
        }
        self.modules = modules;
        self
    }

    pub fn evaluate(self, source: &str) -> TestResult {
        // Initialize virtual time for testing
        let virtual_time_ms = Arc::new(AtomicU64::new(0));

        // Create workers with virtual time function
        let num_workers = 2;
        let mut workers: Vec<Box<dyn WorkerHandle>> = Vec::new();
        for _ in 0..num_workers {
            let time = virtual_time_ms.clone();
            workers.push(Box::new(spawn_worker(move || time.load(Ordering::Relaxed))));
        }

        // Create program and module loader
        let program = Program::new();
        let module_loader = Box::new(InMemoryModuleLoader::new(self.modules));

        // Create REPL
        let mut repl = Repl::new(workers, program, module_loader).expect("Failed to create REPL");

        // Evaluate source
        let result = match repl.evaluate(source) {
            Ok(Some(request_id)) => {
                // Poll for result with timeout
                let start = std::time::Instant::now();
                let timeout = std::time::Duration::from_secs(5);

                loop {
                    let did_work = repl.step().unwrap_or(false);

                    // If idle, advance virtual time
                    if !did_work {
                        virtual_time_ms.fetch_add(1, Ordering::Relaxed);
                    }

                    match repl.poll_request(request_id) {
                        Ok(Some(quiver_environment::RequestResult::Result(Ok((value, heap))))) => {
                            break Ok(Some((value, heap)));
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
            Ok(None) => {
                // No executable code (e.g., only type definitions)
                Ok(None)
            }
            Err(e) => Err(e),
        };

        TestResult {
            result,
            source: source.to_string(),
            repl,
            virtual_time_ms: virtual_time_ms.load(Ordering::Relaxed),
        }
    }
}

#[allow(dead_code)]
pub struct TestResult {
    result: ReplResult,
    source: String,
    repl: Repl,
    virtual_time_ms: u64,
}

#[allow(dead_code)]
impl TestResult {
    /// Expect a value matching the given Quiver syntax string representation
    pub fn expect(self, expected: &str) -> Self {
        match self.result {
            Ok(Some((ref value, ref heap_data))) => {
                let actual = self.repl.format_value(value, heap_data);
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

    pub fn expect_type(self, variable_name: &str, expected: &str) -> Self {
        // Get variable type from the repl
        let variables = self.repl.get_variables();
        let variable_type = variables
            .iter()
            .find(|(name, _)| name == variable_name)
            .map(|(_, ty)| ty);

        match variable_type {
            Some(ty) => {
                let actual = self.repl.format_type(ty);
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
        match self.repl.resolve_type_alias(alias_name) {
            Ok(ty) => {
                let actual = self.repl.format_type(&ty);
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

    pub fn expect_duration(self, min_ms: u64, max_ms: u64) -> Self {
        assert!(
            self.virtual_time_ms >= min_ms && self.virtual_time_ms <= max_ms,
            "Expected virtual time between {}ms and {}ms, but got {}ms for source: {}",
            min_ms,
            max_ms,
            self.virtual_time_ms,
            self.source
        );
        self
    }
}

#[allow(dead_code)]
pub fn quiver() -> TestBuilder {
    TestBuilder::new()
}
