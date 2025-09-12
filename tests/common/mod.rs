use quiver::{Quiver, bytecode::TypeId, vm::Value};
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

    /// Expect a binary value with specific content
    pub fn expect_binary(self, expected_bytes: &[u8]) {
        match self.result {
            Ok(Some(Value::Binary(ref binary_ref))) => {
                let actual_bytes = self
                    .quiver
                    .get_binary_bytes(binary_ref)
                    .expect("Failed to get binary bytes");
                assert_eq!(
                    actual_bytes.as_slice(),
                    expected_bytes,
                    "Expected binary content {:?}, got {:?} for source: {}",
                    expected_bytes,
                    actual_bytes,
                    self.source
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

    pub fn expect_tuple(
        self,
        expected_type_name: Option<&str>,
        expected_field_names: Vec<Option<&str>>,
        expected_values: Vec<Value>,
    ) {
        // First ensure field names and values have the same length
        assert_eq!(
            expected_field_names.len(),
            expected_values.len(),
            "Test error: expected_field_names length ({}) must match expected_values length ({})",
            expected_field_names.len(),
            expected_values.len()
        );

        match self.result {
            Ok(Some(Value::Tuple(type_id, actual_values))) => {
                // Get type info once
                let type_info = self.quiver.get_type_info(&type_id);

                // Check type name matches expectation (including None)
                match (type_info, expected_type_name) {
                    (Some((Some(actual), _)), Some(expected)) => {
                        assert_eq!(
                            actual, expected,
                            "Expected tuple type '{}', got '{}' for source: {}",
                            expected, actual, self.source
                        );
                    }
                    (Some((None, _)), Some(expected)) => {
                        panic!(
                            "Expected tuple type '{}', but tuple is unnamed for source: {}",
                            expected, self.source
                        );
                    }
                    (Some((Some(actual), _)), None) => {
                        panic!(
                            "Expected unnamed tuple, but got type '{}' for source: {}",
                            actual, self.source
                        );
                    }
                    (Some((None, _)), None) => {
                        // Both unnamed, ok
                    }
                    (None, Some(expected)) => {
                        panic!(
                            "Expected tuple type '{}', but type {} not found in registry for source: {}",
                            expected, type_id.0, self.source
                        );
                    }
                    (None, None) => {
                        // No type info and we expected unnamed - could be runtime-created tuple
                        if !expected_field_names.is_empty()
                            && expected_field_names.iter().any(|f| f.is_some())
                        {
                            panic!(
                                "Expected field names but type {} has no field information for source: {}",
                                type_id.0, self.source
                            );
                        }
                    }
                }

                // Check number of values matches
                assert_eq!(
                    actual_values.len(),
                    expected_values.len(),
                    "Expected tuple with {} elements, got {} elements for source: {}",
                    expected_values.len(),
                    actual_values.len(),
                    self.source
                );

                // Check fields if we have type info
                if let Some((_, fields)) = type_info {
                    assert_eq!(
                        fields.len(),
                        expected_field_names.len(),
                        "Expected {} fields, got {} fields for source: {}",
                        expected_field_names.len(),
                        fields.len(),
                        self.source
                    );

                    // Check each field's name and value
                    for (
                        i,
                        (
                            ((actual_field_name, _), expected_field_name),
                            (actual_value, expected_value),
                        ),
                    ) in fields
                        .iter()
                        .zip(expected_field_names.iter())
                        .zip(actual_values.iter().zip(expected_values.iter()))
                        .enumerate()
                    {
                        // Check field name
                        match (actual_field_name, expected_field_name) {
                            (Some(actual), Some(expected)) => {
                                assert_eq!(
                                    actual, expected,
                                    "Field {} name mismatch: expected '{}', got '{}' for source: {}",
                                    i, expected, actual, self.source
                                );
                            }
                            (None, Some(expected)) => {
                                panic!(
                                    "Field {} expected to be named '{}', but was unnamed for source: {}",
                                    i, expected, self.source
                                );
                            }
                            (Some(actual), None) => {
                                panic!(
                                    "Field {} expected to be unnamed, but was named '{}' for source: {}",
                                    i, actual, self.source
                                );
                            }
                            (None, None) => {
                                // Both unnamed, ok
                            }
                        }

                        // Check field value
                        assert_eq!(
                            actual_value, expected_value,
                            "Field {} value mismatch: expected {:?}, got {:?} for source: {}",
                            i, expected_value, actual_value, self.source
                        );
                    }
                } else {
                    // No type info - just check values
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
