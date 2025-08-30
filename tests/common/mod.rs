use quiver::{Quiver, vm::Value, bytecode::TypeId};
use std::collections::HashMap;

pub fn create_quiver() -> Quiver {
    Quiver::new()
}

pub fn create_quiver_with_modules(modules: HashMap<String, String>) -> Quiver {
    // TODO: Need to implement module loading in Quiver constructor
    // For now, return basic Quiver instance
    Quiver::new()
}

pub fn expect_int(source: &str, expected: i64) {
    let mut quiver = create_quiver();
    match quiver.evaluate(source) {
        Ok(Some(Value::Integer(actual))) => {
            assert_eq!(actual, expected, "Expected {}, got {} for source: {}", expected, actual, source);
        }
        Ok(Some(other)) => {
            panic!("Expected integer {}, got {:?} for source: {}", expected, other, source);
        }
        Ok(None) => {
            panic!("Expected integer {}, got None for source: {}", expected, source);
        }
        Err(e) => {
            panic!("Expected integer {}, got error: {} for source: {}", expected, e, source);
        }
    }
}

pub fn expect_nil(source: &str) {
    let mut quiver = create_quiver();
    match quiver.evaluate(source) {
        Ok(Some(Value::Tuple(TypeId::NIL, elements))) => {
            assert!(elements.is_empty(), "Expected empty tuple (nil), got tuple with {} elements for source: {}", elements.len(), source);
        }
        Ok(Some(other)) => {
            panic!("Expected nil ([]), got {:?} for source: {}", other, source);
        }
        Ok(None) => {
            // None is also considered nil for our purposes
        }
        Err(e) => {
            panic!("Expected nil, got error: {} for source: {}", e, source);
        }
    }
}

pub fn expect_ok(source: &str) {
    let mut quiver = create_quiver();
    match quiver.evaluate(source) {
        Ok(Some(Value::Tuple(TypeId::OK, elements))) => {
            assert!(elements.is_empty(), "Expected Ok tuple with no elements, got tuple with {} elements for source: {}", elements.len(), source);
        }
        Ok(Some(other)) => {
            panic!("Expected Ok, got {:?} for source: {}", other, source);
        }
        Ok(None) => {
            panic!("Expected Ok, got None for source: {}", source);
        }
        Err(e) => {
            panic!("Expected Ok, got error: {} for source: {}", e, source);
        }
    }
}

pub fn expect_binary(source: &str, expected_index: usize) {
    let mut quiver = create_quiver();
    match quiver.evaluate(source) {
        Ok(Some(Value::Binary(actual_index))) => {
            assert_eq!(actual_index, expected_index, "Expected binary at index {}, got index {} for source: {}", expected_index, actual_index, source);
        }
        Ok(Some(other)) => {
            panic!("Expected binary at index {}, got {:?} for source: {}", expected_index, other, source);
        }
        Ok(None) => {
            panic!("Expected binary at index {}, got None for source: {}", expected_index, source);
        }
        Err(e) => {
            panic!("Expected binary, got error: {} for source: {}", e, source);
        }
    }
}

pub fn expect_tuple(source: &str, expected_values: Vec<Value>) {
    let mut quiver = create_quiver();
    match quiver.evaluate(source) {
        Ok(Some(Value::Tuple(_, actual_values))) => {
            assert_eq!(actual_values.len(), expected_values.len(), 
                "Expected tuple with {} elements, got {} elements for source: {}", 
                expected_values.len(), actual_values.len(), source);
            for (i, (actual, expected)) in actual_values.iter().zip(expected_values.iter()).enumerate() {
                assert_eq!(actual, expected, 
                    "Tuple element {} mismatch: expected {:?}, got {:?} for source: {}", 
                    i, expected, actual, source);
            }
        }
        Ok(Some(other)) => {
            panic!("Expected tuple {:?}, got {:?} for source: {}", expected_values, other, source);
        }
        Ok(None) => {
            panic!("Expected tuple {:?}, got None for source: {}", expected_values, source);
        }
        Err(e) => {
            panic!("Expected tuple, got error: {} for source: {}", e, source);
        }
    }
}

pub fn expect_function(source: &str) {
    let mut quiver = create_quiver();
    match quiver.evaluate(source) {
        Ok(Some(Value::Function { .. })) => {
            // Success - we got a function
        }
        Ok(Some(other)) => {
            panic!("Expected function, got {:?} for source: {}", other, source);
        }
        Ok(None) => {
            panic!("Expected function, got None for source: {}", source);
        }
        Err(e) => {
            panic!("Expected function, got error: {} for source: {}", e, source);
        }
    }
}

pub fn expect_error(source: &str) {
    let mut quiver = create_quiver();
    match quiver.evaluate(source) {
        Ok(result) => {
            panic!("Expected error, but evaluation succeeded with result: {:?} for source: {}", result, source);
        }
        Err(_) => {
            // Success - we got an error as expected
        }
    }
}

pub fn expect_with_modules(source: &str, modules: HashMap<String, String>, expected: Value) {
    let mut quiver = create_quiver_with_modules(modules);
    match quiver.evaluate(source) {
        Ok(Some(actual)) => {
            assert_eq!(actual, expected, "Expected {:?}, got {:?} for source: {}", expected, actual, source);
        }
        Ok(None) => {
            panic!("Expected {:?}, got None for source: {}", expected, source);
        }
        Err(e) => {
            panic!("Expected {:?}, got error: {} for source: {}", expected, e, source);
        }
    }
}

pub fn with_module(path: &str, content: &str) -> HashMap<String, String> {
    let mut modules = HashMap::new();
    modules.insert(path.to_string(), content.to_string());
    modules
}

pub fn add_module(mut modules: HashMap<String, String>, path: &str, content: &str) -> HashMap<String, String> {
    modules.insert(path.to_string(), content.to_string());
    modules
}