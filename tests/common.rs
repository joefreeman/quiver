use quiver::{Quiver, vm::Value};
use std::collections::HashMap;

#[allow(dead_code)]
pub struct TestBuilder {
    modules: Option<HashMap<String, String>>,
}

#[allow(dead_code)]
impl TestBuilder {
    pub fn new() -> Self {
        Self { modules: None }
    }

    pub fn with_modules(mut self, modules: HashMap<String, String>) -> Self {
        self.modules = Some(modules);
        self
    }

    pub fn evaluate(self, source: &str) -> TestResult {
        let mut quiver = Quiver::new(self.modules);
        let result = quiver.evaluate(source, None, None, None);

        let (result_value, _) = match result {
            Ok((value, vars)) => (Ok(value), vars),
            Err(e) => (Err(e), HashMap::new()),
        };

        TestResult {
            result: result_value,
            source: source.to_string(),
            quiver,
        }
    }
}

#[allow(dead_code)]
pub struct TestResult {
    result: Result<Option<Value>, quiver::Error>,
    source: String,
    quiver: Quiver,
}

#[allow(dead_code)]
impl TestResult {
    /// Expect a value matching the given Quiver syntax string representation
    pub fn expect(self, expected: &str) {
        match self.result {
            Ok(Some(ref value)) => {
                let actual = self.quiver.format_value(value);
                assert_eq!(
                    actual, expected,
                    "Expected '{}', got '{}' for source: {}",
                    expected, actual, self.source
                );
            }
            Ok(None) => {
                // None is treated as nil/empty tuple
                let actual = "[]";
                assert_eq!(
                    actual, expected,
                    "Expected '{}', got None (displayed as '{}') for source: {}",
                    expected, actual, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Expected value '{}', got error: {} for source: {}",
                    expected, e, self.source
                );
            }
        }
    }

    pub fn expect_runtime_error(self, expected: quiver::vm::Error) {
        match self.result {
            Ok(result) => {
                panic!(
                    "Expected runtime error {:?}, but evaluation succeeded with result: {:?} for source: {}",
                    expected, result, self.source
                );
            }
            Err(quiver::Error::RuntimeError(actual)) => {
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

    pub fn expect_compile_error(self, expected: quiver::compiler::Error) {
        match self.result {
            Ok(result) => {
                panic!(
                    "Expected compile error {:?}, but evaluation succeeded with result: {:?} for source: {}",
                    expected, result, self.source
                );
            }
            Err(quiver::Error::CompileError(actual)) => {
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

    pub fn expect_parse_error(self, expected: quiver::parser::Error) {
        match self.result {
            Ok(result) => {
                panic!(
                    "Expected parse error {:?}, but evaluation succeeded with result: {:?} for source: {}",
                    expected, result, self.source
                );
            }
            Err(quiver::Error::ParseError(boxed_actual)) => {
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
