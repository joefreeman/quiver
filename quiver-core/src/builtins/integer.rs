//! Integer builtin function implementations

use crate::error::Error;
use crate::executor::Executor;
use crate::value::Value;

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

/// Bitwise AND of two integers
/// integer_and([int, int]) -> int
pub fn builtin_integer_and(arg: &Value, _executor: &mut Executor) -> Result<Value, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(Value::Integer(a & b))
}

/// Bitwise OR of two integers
/// integer_or([int, int]) -> int
pub fn builtin_integer_or(arg: &Value, _executor: &mut Executor) -> Result<Value, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(Value::Integer(a | b))
}

/// Bitwise XOR of two integers
/// integer_xor([int, int]) -> int
pub fn builtin_integer_xor(arg: &Value, _executor: &mut Executor) -> Result<Value, Error> {
    let (a, b) = extract_two_integers(arg)?;
    Ok(Value::Integer(a ^ b))
}

/// Bitwise NOT of an integer
/// integer_not(int) -> int
pub fn builtin_integer_not(arg: &Value, _executor: &mut Executor) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => Ok(Value::Integer(!n)),
        other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}

/// Shift integer by n bits (positive = left, negative = right)
/// integer_shift([int, int]) -> int
/// Note: This is an arithmetic right shift (sign-extending)
pub fn builtin_integer_shift(arg: &Value, _executor: &mut Executor) -> Result<Value, Error> {
    let (value, shift_amount) = extract_two_integers(arg)?;

    if shift_amount == 0 {
        return Ok(Value::Integer(value));
    }

    // Rust's shift operators panic if shift amount is >= bit width
    // We clamp to reasonable values (0-63 for i64)
    let shift_amount_abs = shift_amount.unsigned_abs();

    if shift_amount_abs >= 64 {
        // Shifting by 64+ bits
        if shift_amount > 0 {
            // Left shift by 64+ always gives 0
            return Ok(Value::Integer(0));
        } else {
            // Right shift by 64+ gives 0 or -1 depending on sign
            return Ok(Value::Integer(if value >= 0 { 0 } else { -1 }));
        }
    }

    let result = if shift_amount > 0 {
        // Left shift
        value << shift_amount_abs
    } else {
        // Arithmetic right shift (sign-extending)
        value >> shift_amount_abs
    };

    Ok(Value::Integer(result))
}

/// Count number of set bits (population count) in an integer
/// integer_popcount(int) -> int
pub fn builtin_integer_popcount(arg: &Value, _executor: &mut Executor) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => {
            // Use u64's count_ones for positive values, handle negative via two's complement
            let count = (*n as u64).count_ones() as i64;
            Ok(Value::Integer(count))
        }
        other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}
