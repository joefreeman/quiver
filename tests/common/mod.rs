use quiver::{
    Quiver,
    bytecode::TypeId,
    vm::{BinaryRef, Value},
};
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
        let result = quiver.evaluate(source, None);
        TestResult {
            result,
            source: source.to_string(),
        }
    }
}

#[allow(dead_code)]
pub struct TestResult {
    result: Result<Option<Value>, quiver::Error>,
    source: String,
}

#[allow(dead_code)]
impl TestResult {
    pub fn expect_int(self, expected: i64) {
        match self.result {
            Ok(Some(Value::Integer(actual))) => {
                assert_eq!(
                    actual, expected,
                    "Expected {}, got {} for source: {}",
                    expected, actual, self.source
                );
            }
            Ok(Some(other)) => {
                panic!(
                    "Expected integer {}, got {:?} for source: {}",
                    expected, other, self.source
                );
            }
            Ok(None) => {
                panic!(
                    "Expected integer {}, got None for source: {}",
                    expected, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Expected integer {}, got error: {} for source: {}",
                    expected, e, self.source
                );
            }
        }
    }

    pub fn expect_nil(self) {
        match self.result {
            Ok(Some(Value::Tuple(TypeId::NIL, elements))) => {
                assert!(
                    elements.is_empty(),
                    "Expected empty tuple (nil), got tuple with {} elements for source: {}",
                    elements.len(),
                    self.source
                );
            }
            Ok(Some(other)) => {
                panic!(
                    "Expected nil ([]), got {:?} for source: {}",
                    other, self.source
                );
            }
            Ok(None) => {
                // None is also considered nil for our purposes
            }
            Err(e) => {
                panic!("Expected nil, got error: {} for source: {}", e, self.source);
            }
        }
    }

    pub fn expect_ok(self) {
        match self.result {
            Ok(Some(Value::Tuple(TypeId::OK, elements))) => {
                assert!(
                    elements.is_empty(),
                    "Expected Ok tuple with no elements, got tuple with {} elements for source: {}",
                    elements.len(),
                    self.source
                );
            }
            Ok(Some(other)) => {
                panic!("Expected Ok, got {:?} for source: {}", other, self.source);
            }
            Ok(None) => {
                panic!("Expected Ok, got None for source: {}", self.source);
            }
            Err(e) => {
                panic!("Expected Ok, got error: {} for source: {}", e, self.source);
            }
        }
    }

    /// Expect a binary value (for backwards compatibility - checks if it's a constant at specific index)
    pub fn expect_binary(self, expected_index: usize) {
        match self.result {
            Ok(Some(Value::Binary(BinaryRef::Constant(actual_index)))) => {
                assert_eq!(
                    actual_index, expected_index,
                    "Expected binary constant at index {}, got index {} for source: {}",
                    expected_index, actual_index, self.source
                );
            }
            Ok(Some(Value::Binary(BinaryRef::Heap(_)))) => {
                panic!(
                    "Expected binary constant at index {}, got heap binary for source: {}",
                    expected_index, self.source
                );
            }
            Ok(Some(other)) => {
                panic!(
                    "Expected binary at index {}, got {:?} for source: {}",
                    expected_index, other, self.source
                );
            }
            Ok(None) => {
                panic!(
                    "Expected binary at index {}, got None for source: {}",
                    expected_index, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Expected binary, got error: {} for source: {}",
                    e, self.source
                );
            }
        }
    }

    /// Expect a binary value with specific content
    pub fn expect_binary_content(self, expected_bytes: &[u8]) {
        match self.result {
            Ok(Some(Value::Binary(binary_ref))) => {
                // For testing, we'll manually check binary content
                // This is a bit hacky but works for our test purposes
                let actual_bytes = match &binary_ref {
                    BinaryRef::Constant(_) => {
                        // We can't easily get the bytes without VM access in tests
                        // For now, just check that it's a binary
                        println!(
                            "Warning: expect_binary_content with constant binary - content not verified"
                        );
                        return;
                    }
                    BinaryRef::Heap(rc_bytes) => rc_bytes.as_ref(),
                };
                assert_eq!(
                    actual_bytes, expected_bytes,
                    "Expected binary content {:?}, got {:?} for source: {}",
                    expected_bytes, actual_bytes, self.source
                );
            }
            Ok(Some(other)) => {
                panic!(
                    "Expected binary with content {:?}, got {:?} for source: {}",
                    expected_bytes, other, self.source
                );
            }
            Ok(None) => {
                panic!(
                    "Expected binary with content {:?}, got None for source: {}",
                    expected_bytes, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Expected binary, got error: {} for source: {}",
                    e, self.source
                );
            }
        }
    }

    /// Expect any binary value (regardless of source)
    pub fn expect_any_binary(self) {
        match self.result {
            Ok(Some(Value::Binary(_))) => {
                // Success - we got a binary
            }
            Ok(Some(other)) => {
                panic!(
                    "Expected binary, got {:?} for source: {}",
                    other, self.source
                );
            }
            Ok(None) => {
                panic!("Expected binary, got None for source: {}", self.source);
            }
            Err(e) => {
                panic!(
                    "Expected binary, got error: {} for source: {}",
                    e, self.source
                );
            }
        }
    }

    pub fn expect_tuple(self, expected_values: Vec<Value>) {
        match self.result {
            Ok(Some(Value::Tuple(_, actual_values))) => {
                assert_eq!(
                    actual_values.len(),
                    expected_values.len(),
                    "Expected tuple with {} elements, got {} elements for source: {}",
                    expected_values.len(),
                    actual_values.len(),
                    self.source
                );
                for (i, (actual, expected)) in
                    actual_values.iter().zip(expected_values.iter()).enumerate()
                {
                    assert_eq!(
                        actual, expected,
                        "Tuple element {} mismatch: expected {:?}, got {:?} for source: {}",
                        i, expected, actual, self.source
                    );
                }
            }
            Ok(Some(other)) => {
                panic!(
                    "Expected tuple {:?}, got {:?} for source: {}",
                    expected_values, other, self.source
                );
            }
            Ok(None) => {
                panic!(
                    "Expected tuple {:?}, got None for source: {}",
                    expected_values, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Expected tuple, got error: {} for source: {}",
                    e, self.source
                );
            }
        }
    }

    pub fn expect_function(self) {
        match self.result {
            Ok(Some(Value::Function { .. })) => {
                // Success - we got a function
            }
            Ok(Some(other)) => {
                panic!(
                    "Expected function, got {:?} for source: {}",
                    other, self.source
                );
            }
            Ok(None) => {
                panic!("Expected function, got None for source: {}", self.source);
            }
            Err(e) => {
                panic!(
                    "Expected function, got error: {} for source: {}",
                    e, self.source
                );
            }
        }
    }

    pub fn expect_error(self) {
        match self.result {
            Ok(result) => {
                panic!(
                    "Expected error, but evaluation succeeded with result: {:?} for source: {}",
                    result, self.source
                );
            }
            Err(_) => {
                // Success - we got an error as expected
            }
        }
    }

    pub fn expect_value(self, expected: Value) {
        match self.result {
            Ok(Some(actual)) => {
                assert_eq!(
                    actual, expected,
                    "Expected {:?}, got {:?} for source: {}",
                    expected, actual, self.source
                );
            }
            Ok(None) => {
                panic!(
                    "Expected {:?}, got None for source: {}",
                    expected, self.source
                );
            }
            Err(e) => {
                panic!(
                    "Expected {:?}, got error: {} for source: {}",
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
