use quiver::{Quiver, bytecode::TypeId, vm::Value};
use std::collections::HashMap;

pub struct TestBuilder {
    modules: Option<HashMap<String, String>>,
}

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
        let result = quiver.evaluate(source);
        TestResult {
            result,
            source: source.to_string(),
        }
    }
}

pub struct TestResult {
    result: Result<Option<Value>, quiver::Error>,
    source: String,
}

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

    pub fn expect_binary(self, expected_index: usize) {
        match self.result {
            Ok(Some(Value::Binary(actual_index))) => {
                assert_eq!(
                    actual_index, expected_index,
                    "Expected binary at index {}, got index {} for source: {}",
                    expected_index, actual_index, self.source
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

pub fn quiver() -> TestBuilder {
    TestBuilder::new()
}
