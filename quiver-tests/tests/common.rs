use quiver::Quiver;
use quiver_core::value::Value;
use std::collections::HashMap;

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
    modules: Option<HashMap<String, String>>,
}

#[allow(dead_code)]
impl TestBuilder {
    pub fn new() -> Self {
        Self {
            modules: Some(load_stdlib_modules!("math", "list")),
        }
    }

    pub fn with_modules(mut self, mut modules: HashMap<String, String>) -> Self {
        // Merge additional modules with standard library modules
        if let Some(stdlib_modules) = self.modules {
            for (key, value) in stdlib_modules {
                modules.entry(key).or_insert(value);
            }
        }
        self.modules = Some(modules);
        self
    }

    pub fn evaluate(self, source: &str) -> TestResult {
        let mut quiver = Quiver::new(self.modules);
        let result = quiver
            .evaluate(source, None, None, None)
            .map(|(value, _, _, heap_data)| value.map(|v| (v, heap_data)));

        TestResult {
            result,
            source: source.to_string(),
            quiver,
        }
    }
}

#[allow(dead_code)]
pub struct TestResult {
    result: Result<Option<(Value, Vec<Vec<u8>>)>, quiver::Error>,
    source: String,
    quiver: Quiver,
}

#[allow(dead_code)]
impl TestResult {
    /// Expect a value matching the given Quiver syntax string representation
    pub fn expect(self, expected: &str) {
        match self.result {
            Ok(Some((ref value, ref heap_data))) => {
                let actual = self.quiver.format_value(value, heap_data);
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

    pub fn expect_runtime_error(self, expected: quiver_core::error::Error) {
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

    pub fn expect_compile_error(self, expected: quiver_compiler::compiler::Error) {
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

    pub fn expect_parse_error(self, expected: quiver_compiler::parser::Error) {
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
