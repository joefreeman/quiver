use quiver::spawn_worker;
use quiver_compiler::modules::InMemoryModuleLoader;
use quiver_core::program::Program;
use quiver_core::value::Value;
use quiver_environment::{Repl, ReplError, WorkerHandle};
use std::collections::HashMap;

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
        // Create workers
        let num_workers = 1; // Single worker for tests
        let mut workers: Vec<Box<dyn WorkerHandle>> = Vec::new();
        for _ in 0..num_workers {
            workers.push(Box::new(spawn_worker()));
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
                    repl.step().ok();

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
        }
    }
}

#[allow(dead_code)]
pub struct TestResult {
    result: ReplResult,
    source: String,
    repl: Repl,
}

#[allow(dead_code)]
impl TestResult {
    /// Expect a value matching the given Quiver syntax string representation
    pub fn expect(self, expected: &str) {
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
                // No executable code (e.g., only type definitions) - treat as []
                assert_eq!(
                    "[]", expected,
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
}

#[allow(dead_code)]
pub fn quiver() -> TestBuilder {
    TestBuilder::new()
}
