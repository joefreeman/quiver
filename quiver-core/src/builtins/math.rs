//! Math builtin function implementations
use crate::builtins::BuiltinResult;
use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::ProcessId;
use crate::value::Value;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{Signed, ToPrimitive, Zero};
use std::cmp::Ordering;

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

/// Convert a `BigInt` to `f64` for the lossy trigonometric builtins, falling back to
/// infinity when the magnitude is too large to represent.
fn to_f64_lossy(n: &BigInt) -> f64 {
    n.to_f64().unwrap_or(f64::INFINITY)
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
            if n.is_negative() {
                Err(Error::InvalidArgument(
                    "Cannot take square root of negative number".to_string(),
                ))
            } else {
                Ok(BuiltinResult::Value(Value::Integer(n.sqrt())))
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
            let result = to_f64_lossy(n).sin() as i64;
            Ok(BuiltinResult::Value(Value::Integer(BigInt::from(result))))
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
            let result = to_f64_lossy(n).cos() as i64;
            Ok(BuiltinResult::Value(Value::Integer(BigInt::from(result))))
        }
        _other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: "non-integer".to_string(),
        }),
    }
}

/// Helper function to extract exactly two integers from a tuple
fn extract_two_integers(arg: &Value) -> Result<(BigInt, BigInt), Error> {
    match arg {
        Value::Tuple(_, fields) => {
            if fields.len() != 2 {
                return Err(Error::InvalidArgument(format!(
                    "Expected tuple with exactly 2 elements, got {}",
                    fields.len()
                )));
            }

            let first = match &fields[0] {
                Value::Integer(n) => n.clone(),
                _other => {
                    return Err(Error::TypeMismatch {
                        expected: "integer".to_string(),
                        found: "non-integer".to_string(),
                    });
                }
            };

            let second = match &fields[1] {
                Value::Integer(n) => n.clone(),
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
    Ok(BuiltinResult::Value(Value::Integer(a.gcd(&b))))
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
    if b.is_zero() {
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
    if b.is_zero() {
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

    let result = match a.cmp(&b) {
        Ordering::Less => -1,
        Ordering::Greater => 1,
        Ordering::Equal => 0,
    };

    Ok(BuiltinResult::Value(Value::Integer(BigInt::from(result))))
}
