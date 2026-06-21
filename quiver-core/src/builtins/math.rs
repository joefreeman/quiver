//! Math builtin function implementations
use crate::builtins::BuiltinResult;
use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::ProcessId;
use crate::value::Value;

/// Builtin function: math:abs
/// Returns the absolute value of an integer
pub fn builtin_math_abs<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Integer(n) => Ok(BuiltinResult::Value(Value::Integer(n.abs()))),
        _other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: "non-integer".to_string(),
        }),
    }
}

/// Builtin function: math:sqrt
/// Returns the square root of an integer (truncated to integer)
pub fn builtin_math_sqrt<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Integer(n) => {
            if *n < 0 {
                Err(Error::InvalidArgument(
                    "Cannot take square root of negative number".to_string(),
                ))
            } else {
                let result = (*n as f64).sqrt() as i64;
                Ok(BuiltinResult::Value(Value::Integer(result)))
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
pub fn builtin_math_sin<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Integer(n) => {
            let result = (*n as f64).sin() as i64;
            Ok(BuiltinResult::Value(Value::Integer(result)))
        }
        _other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: "non-integer".to_string(),
        }),
    }
}

/// Builtin function: math:cos
/// Returns the cosine of an integer (treating it as radians, truncated to integer)
pub fn builtin_math_cos<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Integer(n) => {
            let result = (*n as f64).cos() as i64;
            Ok(BuiltinResult::Value(Value::Integer(result)))
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

/// Builtin function: __add__
/// Adds two integers from a tuple
pub fn builtin_add<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(BuiltinResult::Value(Value::Integer(a + b)))
}

/// Builtin function: __gcd__
/// Returns the greatest common divisor of two integers (always non-negative).
/// Used by the `num` module to reduce rationals to canonical form.
pub fn builtin_gcd<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_integers(arg)?;
    let mut a = a.unsigned_abs();
    let mut b = b.unsigned_abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    Ok(BuiltinResult::Value(Value::Integer(a as i64)))
}

/// Builtin function: __subtract__
/// Subtracts two integers from a tuple
pub fn builtin_subtract<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(BuiltinResult::Value(Value::Integer(a - b)))
}

/// Builtin function: __multiply__
/// Multiplies two integers from a tuple
pub fn builtin_multiply<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(BuiltinResult::Value(Value::Integer(a * b)))
}

/// Builtin function: __divide__
/// Divides two integers from a tuple
pub fn builtin_divide<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_integers(arg)?;
    if b == 0 {
        return Err(Error::InvalidArgument("Division by zero".to_string()));
    }
    Ok(BuiltinResult::Value(Value::Integer(a / b)))
}

/// Builtin function: __modulo__
/// Takes modulo of two integers from a tuple
pub fn builtin_modulo<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_integers(arg)?;
    if b == 0 {
        return Err(Error::InvalidArgument("Modulo by zero".to_string()));
    }
    Ok(BuiltinResult::Value(Value::Integer(a % b)))
}

/// Builtin function: __compare__
/// Compares two integers and returns -1, 0, or 1
pub fn builtin_compare<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_integers(arg)?;

    let result = if a < b {
        -1
    } else if a > b {
        1
    } else {
        0
    };

    Ok(BuiltinResult::Value(Value::Integer(result)))
}
