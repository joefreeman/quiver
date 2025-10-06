//! Math builtin function implementations

use crate::scheduler::Scheduler;
use crate::vm::{Error, Value};

/// Builtin function: math:abs
/// Returns the absolute value of an integer
pub fn builtin_math_abs(arg: &Value, _program: &mut Scheduler) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => Ok(Value::Integer(n.abs())),
        _other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: "non-integer".to_string(),
        }),
    }
}

/// Builtin function: math:sqrt
/// Returns the square root of an integer (truncated to integer)
pub fn builtin_math_sqrt(arg: &Value, _program: &mut Scheduler) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => {
            if *n < 0 {
                Err(Error::InvalidArgument(
                    "Cannot take square root of negative number".to_string(),
                ))
            } else {
                let result = (*n as f64).sqrt() as i64;
                Ok(Value::Integer(result))
            }
        }
        _other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: "non-integer".to_string(),
        }),
    }
}

/// Builtin function: math:sin
/// Returns the sine of an integer (treating it as radians, truncated to integer)
pub fn builtin_math_sin(arg: &Value, _program: &mut Scheduler) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => {
            let result = (*n as f64).sin() as i64;
            Ok(Value::Integer(result))
        }
        _other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: "non-integer".to_string(),
        }),
    }
}

/// Builtin function: math:cos
/// Returns the cosine of an integer (treating it as radians, truncated to integer)
pub fn builtin_math_cos(arg: &Value, _program: &mut Scheduler) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => {
            let result = (*n as f64).cos() as i64;
            Ok(Value::Integer(result))
        }
        _other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: "non-integer".to_string(),
        }),
    }
}

/// Helper function to extract exactly two integers from a tuple
fn extract_two_integers(arg: &Value) -> Result<(i64, i64), Error> {
    match arg {
        Value::Tuple(_, fields) => {
            if fields.len() != 2 {
                return Err(Error::InvalidArgument(format!(
                    "Expected tuple with exactly 2 elements, got {}",
                    fields.len()
                )));
            }

            let first = match &fields[0] {
                Value::Integer(n) => *n,
                _other => {
                    return Err(Error::TypeMismatch {
                        expected: "integer".to_string(),
                        found: "non-integer".to_string(),
                    });
                }
            };

            let second = match &fields[1] {
                Value::Integer(n) => *n,
                _other => {
                    return Err(Error::TypeMismatch {
                        expected: "integer".to_string(),
                        found: "non-integer".to_string(),
                    });
                }
            };

            Ok((first, second))
        }
        _other => Err(Error::TypeMismatch {
            expected: "tuple with two integers".to_string(),
            found: "non-tuple".to_string(),
        }),
    }
}

/// Builtin function: <add>
/// Adds two integers from a tuple
pub fn builtin_add(arg: &Value, _program: &mut Scheduler) -> Result<Value, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(Value::Integer(a + b))
}

/// Builtin function: <subtract>
/// Subtracts two integers from a tuple
pub fn builtin_subtract(arg: &Value, _program: &mut Scheduler) -> Result<Value, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(Value::Integer(a - b))
}

/// Builtin function: <multiply>
/// Multiplies two integers from a tuple
pub fn builtin_multiply(arg: &Value, _program: &mut Scheduler) -> Result<Value, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(Value::Integer(a * b))
}

/// Builtin function: <divide>
/// Divides two integers from a tuple
pub fn builtin_divide(arg: &Value, _program: &mut Scheduler) -> Result<Value, Error> {
    let (a, b) = extract_two_integers(arg)?;
    if b == 0 {
        return Err(Error::InvalidArgument("Division by zero".to_string()));
    }
    Ok(Value::Integer(a / b))
}

/// Builtin function: <modulo>
/// Takes modulo of two integers from a tuple
pub fn builtin_modulo(arg: &Value, _program: &mut Scheduler) -> Result<Value, Error> {
    let (a, b) = extract_two_integers(arg)?;
    if b == 0 {
        return Err(Error::InvalidArgument("Modulo by zero".to_string()));
    }
    Ok(Value::Integer(a % b))
}

/// Builtin function: <compare>
/// Compares two integers and returns -1, 0, or 1
pub fn builtin_compare(arg: &Value, _program: &mut Scheduler) -> Result<Value, Error> {
    let (a, b) = extract_two_integers(arg)?;

    let result = if a < b {
        -1
    } else if a > b {
        1
    } else {
        0
    };

    Ok(Value::Integer(result))
}
