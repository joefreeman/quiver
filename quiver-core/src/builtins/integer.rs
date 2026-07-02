//! Integer builtin function implementations
//!
//! Two families of operations over integers live here:
//!
//! - **Arithmetic and math** (`integer_add`, `integer_subtract`, `integer_multiply`,
//!   `integer_divide`, `integer_modulo`, `integer_gcd`, `integer_compare`, `integer_abs`,
//!   `integer_sqrt`, `integer_sin`, `integer_cos`) are exact over arbitrary precision.
//!   Each takes the allocation-free machine-word path when both operands are small
//!   (`Value::Int`), promoting to `BigInt` only on overflow or big operands.
//! - **Bitwise** operations (`integer_and`, `integer_or`, `integer_xor`, `integer_not`,
//!   `integer_shift`, `integer_popcount`) operate on 64-bit machine integers: each
//!   operand is narrowed to `i64` (erroring if out of range) before the logic runs.
use crate::builtins::BuiltinResult;
use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::ProcessId;
use crate::value::{IntRef, Value};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{Signed, ToPrimitive};
use std::cmp::Ordering;

fn int_type_mismatch() -> Error {
    Error::TypeMismatch {
        expected: "integer".to_string(),
        found: "non-integer".to_string(),
    }
}

/// View a single integer argument without allocating.
fn extract_int(arg: &Value) -> Result<IntRef<'_>, Error> {
    arg.as_int().ok_or_else(int_type_mismatch)
}

/// View exactly two integers from a tuple without allocating.
fn extract_two_ints(arg: &Value) -> Result<(IntRef<'_>, IntRef<'_>), Error> {
    match arg {
        Value::Tuple(_, fields) => {
            if fields.len() != 2 {
                return Err(Error::InvalidArgument(format!(
                    "Expected tuple with exactly 2 elements, got {}",
                    fields.len()
                )));
            }
            let first = fields[0].as_int().ok_or_else(int_type_mismatch)?;
            let second = fields[1].as_int().ok_or_else(int_type_mismatch)?;
            Ok((first, second))
        }
        _other => Err(Error::TypeMismatch {
            expected: "tuple with two integers".to_string(),
            found: "non-tuple".to_string(),
        }),
    }
}

/// Extract exactly two `i64` integers from a tuple (used by the bitwise builtins, which
/// operate on machine words). Errors on integers outside the i64 range.
fn extract_two_i64(arg: &Value) -> Result<(i64, i64), Error> {
    let (a, b) = extract_two_ints(arg)?;
    Ok((narrow_to_i64(a)?, narrow_to_i64(b)?))
}

fn narrow_to_i64(n: IntRef<'_>) -> Result<i64, Error> {
    n.to_i64().ok_or_else(|| {
        Error::InvalidArgument(format!("Integer {n} does not fit in a 64-bit value"))
    })
}

/// Apply a binary arithmetic operation: the machine-word path when both operands are
/// small (promoting to arbitrary precision when `small` reports overflow with `None`),
/// the arbitrary-precision path otherwise. The result is renormalized by
/// [`Value::integer`], so a big-path result that fits an i64 comes back small.
fn arith(
    arg: &Value,
    small: impl FnOnce(i64, i64) -> Option<i64>,
    big: impl FnOnce(BigInt, BigInt) -> BigInt,
) -> Result<Value, Error> {
    let (a, b) = extract_two_ints(arg)?;
    Ok(match (a, b) {
        (IntRef::Small(x), IntRef::Small(y)) => match small(x, y) {
            Some(z) => Value::int(z),
            None => Value::integer(big(BigInt::from(x), BigInt::from(y))),
        },
        (a, b) => Value::integer(big(a.to_bigint(), b.to_bigint())),
    })
}

/// Convert to `f64` for the lossy trigonometric builtins, falling back to infinity when
/// the magnitude is too large to represent.
fn to_f64_lossy(n: IntRef<'_>) -> f64 {
    match n {
        IntRef::Small(n) => n as f64,
        IntRef::Big(n) => n.to_f64().unwrap_or(f64::INFINITY),
    }
}

/// Builtin function: `__integer_abs__`
/// Returns the absolute value of an integer.
pub fn builtin_integer_abs<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let value = match extract_int(arg)? {
        // i64::MIN has no i64 absolute value; promote.
        IntRef::Small(n) => match n.checked_abs() {
            Some(abs) => Value::int(abs),
            None => Value::integer(-BigInt::from(n)),
        },
        IntRef::Big(n) => Value::integer(n.abs()),
    };
    Ok(BuiltinResult::Value(value))
}

/// Builtin function: `__integer_sqrt__`
/// Returns the square root of an integer (truncated to integer).
pub fn builtin_integer_sqrt<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let n = extract_int(arg)?;
    if match n {
        IntRef::Small(n) => n < 0,
        IntRef::Big(n) => n.is_negative(),
    } {
        return Err(Error::InvalidArgument(
            "Cannot take square root of negative number".to_string(),
        ));
    }
    let value = match n {
        IntRef::Small(n) => Value::int(n.isqrt()),
        IntRef::Big(n) => Value::integer(n.sqrt()),
    };
    Ok(BuiltinResult::Value(value))
}

/// Builtin function: `__integer_sin__`
/// Returns the sine of an integer (treating it as radians, truncated to integer).
pub fn builtin_integer_sin<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let n = extract_int(arg)?;
    Ok(BuiltinResult::Value(Value::int(
        to_f64_lossy(n).sin() as i64
    )))
}

/// Builtin function: `__integer_cos__`
/// Returns the cosine of an integer (treating it as radians, truncated to integer).
pub fn builtin_integer_cos<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let n = extract_int(arg)?;
    Ok(BuiltinResult::Value(Value::int(
        to_f64_lossy(n).cos() as i64
    )))
}

/// Builtin function: `__integer_add__`
/// Adds two integers from a tuple.
pub fn builtin_integer_add<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    Ok(BuiltinResult::Value(arith(
        arg,
        i64::checked_add,
        |a, b| a + b,
    )?))
}

/// Builtin function: `__integer_gcd__`
/// Returns the greatest common divisor of two integers (always non-negative).
/// Used by the `num` module to reduce rationals to canonical form.
pub fn builtin_integer_gcd<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_ints(arg)?;
    let value = match (a, b) {
        // Compute on unsigned magnitudes so gcd(i64::MIN, _) can't overflow; the result
        // exceeds i64 only in the gcd(MIN, MIN) = 2^63 corner, which promotes.
        (IntRef::Small(x), IntRef::Small(y)) => {
            let gcd = x.unsigned_abs().gcd(&y.unsigned_abs());
            match i64::try_from(gcd) {
                Ok(small) => Value::int(small),
                Err(_) => Value::integer(BigInt::from(gcd)),
            }
        }
        (a, b) => Value::integer(a.to_bigint().gcd(&b.to_bigint())),
    };
    Ok(BuiltinResult::Value(value))
}

/// Builtin function: `__integer_subtract__`
/// Subtracts two integers from a tuple.
pub fn builtin_integer_subtract<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    Ok(BuiltinResult::Value(arith(
        arg,
        i64::checked_sub,
        |a, b| a - b,
    )?))
}

/// Builtin function: `__integer_multiply__`
/// Multiplies two integers from a tuple.
pub fn builtin_integer_multiply<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    Ok(BuiltinResult::Value(arith(
        arg,
        i64::checked_mul,
        |a, b| a * b,
    )?))
}

/// Builtin function: `__integer_divide__`
/// Divides two integers from a tuple.
pub fn builtin_integer_divide<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    if is_zero(arg) {
        return Err(Error::InvalidArgument("Division by zero".to_string()));
    }
    // With zero excluded, checked_div only reports None for i64::MIN / -1, which promotes.
    Ok(BuiltinResult::Value(arith(
        arg,
        i64::checked_div,
        |a, b| a / b,
    )?))
}

/// Builtin function: `__integer_modulo__`
/// Takes modulo of two integers from a tuple.
pub fn builtin_integer_modulo<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    if is_zero(arg) {
        return Err(Error::InvalidArgument("Modulo by zero".to_string()));
    }
    // With zero excluded, checked_rem only reports None for i64::MIN % -1, which promotes.
    Ok(BuiltinResult::Value(arith(
        arg,
        i64::checked_rem,
        |a, b| a % b,
    )?))
}

/// True if the second element of a two-integer argument tuple is zero (the divisor
/// check for `divide`/`modulo`). Non-integer shapes report false and are left for the
/// arithmetic path to reject with its usual errors.
fn is_zero(arg: &Value) -> bool {
    match arg {
        Value::Tuple(_, fields) if fields.len() == 2 => {
            // A canonical big integer is never zero.
            matches!(fields[1].as_int(), Some(IntRef::Small(0)))
        }
        _ => false,
    }
}

/// Builtin function: `__integer_compare__`
/// Compares two integers and returns -1, 0, or 1.
pub fn builtin_integer_compare<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _program: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_ints(arg)?;

    // A canonical big integer lies strictly outside the i64 range, so against a small
    // operand its sign alone decides the ordering.
    let ordering = match (a, b) {
        (IntRef::Small(x), IntRef::Small(y)) => x.cmp(&y),
        (IntRef::Big(x), IntRef::Big(y)) => x.cmp(y),
        (IntRef::Small(_), IntRef::Big(y)) => {
            if y.is_negative() {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        }
        (IntRef::Big(x), IntRef::Small(_)) => {
            if x.is_negative() {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        }
    };

    let result = match ordering {
        Ordering::Less => -1,
        Ordering::Greater => 1,
        Ordering::Equal => 0,
    };

    Ok(BuiltinResult::Value(Value::int(result)))
}

/// Bitwise AND of two integers
/// integer_and([int, int]) -> int
pub fn builtin_integer_and<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_i64(arg)?;
    Ok(BuiltinResult::Value(Value::int(a & b)))
}

/// Bitwise OR of two integers
/// integer_or([int, int]) -> int
pub fn builtin_integer_or<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_i64(arg)?;
    Ok(BuiltinResult::Value(Value::int(a | b)))
}

/// Bitwise XOR of two integers
/// integer_xor([int, int]) -> int
pub fn builtin_integer_xor<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b) = extract_two_i64(arg)?;
    Ok(BuiltinResult::Value(Value::int(a ^ b)))
}

/// Bitwise NOT of an integer
/// integer_not(int) -> int
pub fn builtin_integer_not<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let n = narrow_to_i64(extract_int(arg)?)?;
    Ok(BuiltinResult::Value(Value::int(!n)))
}

/// Shift integer by n bits (positive = left, negative = right)
/// integer_shift([int, int]) -> int
/// Note: This is an arithmetic right shift (sign-extending)
pub fn builtin_integer_shift<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (value, shift_amount) = extract_two_i64(arg)?;

    if shift_amount == 0 {
        return Ok(BuiltinResult::Value(Value::int(value)));
    }

    // Rust's shift operators panic if shift amount is >= bit width
    // We clamp to reasonable values (0-63 for i64)
    let shift_amount_abs = shift_amount.unsigned_abs();

    if shift_amount_abs >= 64 {
        // Shifting by 64+ bits
        if shift_amount > 0 {
            // Left shift by 64+ always gives 0
            return Ok(BuiltinResult::Value(Value::int(0)));
        } else {
            // Right shift by 64+ gives 0 or -1 depending on sign
            return Ok(BuiltinResult::Value(Value::int(if value >= 0 {
                0
            } else {
                -1
            })));
        }
    }

    let result = if shift_amount > 0 {
        // Left shift
        value << shift_amount_abs
    } else {
        // Arithmetic right shift (sign-extending)
        value >> shift_amount_abs
    };

    Ok(BuiltinResult::Value(Value::int(result)))
}

/// Count number of set bits (population count) in an integer
/// integer_popcount(int) -> int
pub fn builtin_integer_popcount<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    _executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let n = narrow_to_i64(extract_int(arg)?)?;
    // Use u64's count_ones, handling negative values via two's complement
    Ok(BuiltinResult::Value(Value::int(
        (n as u64).count_ones() as i64
    )))
}
