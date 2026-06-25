//! Integer builtin function implementations
//!
//! Two families of operations over integers live here:
//!
//! - **Arithmetic and math** (`integer_add`, `integer_subtract`, `integer_multiply`,
//!   `integer_divide`, `integer_modulo`, `integer_gcd`, `integer_compare`, `integer_abs`,
//!   `integer_sqrt`, `integer_sin`, `integer_cos`) operate on arbitrary-precision `BigInt`s.
//! - **Bitwise** operations (`integer_and`, `integer_or`, `integer_xor`, `integer_not`,
//!   `integer_shift`, `integer_popcount`) operate on 64-bit machine integers: each `BigInt`
//!   operand is narrowed to `i64` (erroring if out of range) before the logic runs, and the
//!   result is widened back to a `BigInt`.
use crate::builtins::{BuiltinResult, bigint_to_i64};
use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::ProcessId;
use crate::value::Value;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{Signed, ToPrimitive, Zero};
use std::cmp::Ordering;

/// Extract exactly two arbitrary-precision integers from a tuple (used by the arithmetic
/// and comparison builtins, which keep full precision).
fn extract_two_bigints(arg: &Value) -> Result<(BigInt, BigInt), Error> {
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

/// Extract exactly two `i64` integers from a tuple (used by the bitwise builtins, which
/// operate on machine words).
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
                Value::Integer(n) => bigint_to_i64(n)?,
                _other => {
                    return Err(Error::TypeMismatch {
                        expected: "integer".to_string(),
                        found: "non-integer".to_string(),
                    });
                }
            };

            let second = match &fields[1] {
                Value::Integer(n) => bigint_to_i64(n)?,
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

/// Convert a `BigInt` to `f64` for the lossy trigonometric builtins, falling back to
/// infinity when the magnitude is too large to represent.
fn to_f64_lossy(n: &BigInt) -> f64 {
    n.to_f64().unwrap_or(f64::INFINITY)
}

/// Builtin function: `__integer_abs__`
/// Returns the absolute value of an integer.
pub fn builtin_integer_abs<E: Effect>(
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

/// Builtin function: `__integer_sqrt__`
/// Returns the square root of an integer (truncated to integer).
pub fn builtin_integer_sqrt<E: Effect>(
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

/// Builtin function: `__integer_sin__`
/// Returns the sine of an integer (treating it as radians, truncated to integer).
pub fn builtin_integer_sin<E: Effect>(
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

/// Builtin function: `__integer_cos__`
/// Returns the cosine of an integer (treating it as radians, truncated to integer).
pub fn builtin_integer_cos<E: Effect>(
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

/// Builtin function: `__integer_add__`
/// Adds two integers from a tuple.
pub fn builtin_integer_add<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_bigints(arg)?;
    Ok(BuiltinResult::Value(Value::Integer(a + b)))
}

/// Builtin function: `__integer_gcd__`
/// Returns the greatest common divisor of two integers (always non-negative).
/// Used by the `num` module to reduce rationals to canonical form.
pub fn builtin_integer_gcd<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_bigints(arg)?;
    Ok(BuiltinResult::Value(Value::Integer(a.gcd(&b))))
}

/// Builtin function: `__integer_subtract__`
/// Subtracts two integers from a tuple.
pub fn builtin_integer_subtract<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_bigints(arg)?;
    Ok(BuiltinResult::Value(Value::Integer(a - b)))
}

/// Builtin function: `__integer_multiply__`
/// Multiplies two integers from a tuple.
pub fn builtin_integer_multiply<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_bigints(arg)?;
    Ok(BuiltinResult::Value(Value::Integer(a * b)))
}

/// Builtin function: `__integer_divide__`
/// Divides two integers from a tuple.
pub fn builtin_integer_divide<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_bigints(arg)?;
    if b.is_zero() {
        return Err(Error::InvalidArgument("Division by zero".to_string()));
    }
    Ok(BuiltinResult::Value(Value::Integer(a / b)))
}

/// Builtin function: `__integer_modulo__`
/// Takes modulo of two integers from a tuple.
pub fn builtin_integer_modulo<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_bigints(arg)?;
    if b.is_zero() {
        return Err(Error::InvalidArgument("Modulo by zero".to_string()));
    }
    Ok(BuiltinResult::Value(Value::Integer(a % b)))
}

/// Builtin function: `__integer_compare__`
/// Compares two integers and returns -1, 0, or 1.
pub fn builtin_integer_compare<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_bigints(arg)?;

    let result = match a.cmp(&b) {
        Ordering::Less => -1,
        Ordering::Greater => 1,
        Ordering::Equal => 0,
    };

    Ok(BuiltinResult::Value(Value::Integer(BigInt::from(result))))
}

/// Bitwise AND of two integers
/// integer_and([int, int]) -> int
pub fn builtin_integer_and<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(BuiltinResult::Value(Value::Integer(BigInt::from(a & b))))
}

/// Bitwise OR of two integers
/// integer_or([int, int]) -> int
pub fn builtin_integer_or<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(BuiltinResult::Value(Value::Integer(BigInt::from(a | b))))
}

/// Bitwise XOR of two integers
/// integer_xor([int, int]) -> int
pub fn builtin_integer_xor<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(BuiltinResult::Value(Value::Integer(BigInt::from(a ^ b))))
}

/// Bitwise NOT of an integer
/// integer_not(int) -> int
pub fn builtin_integer_not<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Integer(n) => {
            let n = bigint_to_i64(n)?;
            Ok(BuiltinResult::Value(Value::Integer(BigInt::from(!n))))
        }
        other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}

/// Shift integer by n bits (positive = left, negative = right)
/// integer_shift([int, int]) -> int
/// Note: This is an arithmetic right shift (sign-extending)
pub fn builtin_integer_shift<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (value, shift_amount) = extract_two_integers(arg)?;

    if shift_amount == 0 {
        return Ok(BuiltinResult::Value(Value::Integer(BigInt::from(value))));
    }

    // Rust's shift operators panic if shift amount is >= bit width
    // We clamp to reasonable values (0-63 for i64)
    let shift_amount_abs = shift_amount.unsigned_abs();

    if shift_amount_abs >= 64 {
        // Shifting by 64+ bits
        if shift_amount > 0 {
            // Left shift by 64+ always gives 0
            return Ok(BuiltinResult::Value(Value::Integer(BigInt::from(0))));
        } else {
            // Right shift by 64+ gives 0 or -1 depending on sign
            return Ok(BuiltinResult::Value(Value::Integer(BigInt::from(
                if value >= 0 { 0 } else { -1 },
            ))));
        }
    }

    let result = if shift_amount > 0 {
        // Left shift
        value << shift_amount_abs
    } else {
        // Arithmetic right shift (sign-extending)
        value >> shift_amount_abs
    };

    Ok(BuiltinResult::Value(Value::Integer(BigInt::from(result))))
}

/// Count number of set bits (population count) in an integer
/// integer_popcount(int) -> int
pub fn builtin_integer_popcount<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Integer(n) => {
            // Use u64's count_ones for positive values, handle negative via two's complement
            let count = (bigint_to_i64(n)? as u64).count_ones() as i64;
            Ok(BuiltinResult::Value(Value::Integer(BigInt::from(count))))
        }
        other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}
