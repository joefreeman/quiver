//! Integer builtin function implementations
//!
//! Two families of operations over integers live here:
//!
//! - **Arithmetic and math** (`integer_add`, `integer_subtract`, `integer_multiply`,
//!   `integer_divide`, `integer_modulo`, `integer_gcd`, `integer_compare`, `integer_abs`,
//!   `integer_sqrt`, `integer_factor`, `integer_sin`, `integer_cos`) are exact over arbitrary precision.
//!   Each takes the allocation-free machine-word path when both operands are small
//!   (`Value::Int`), promoting to `BigInt` only on overflow or big operands.
//! - **Bitwise** operations (`integer_and`, `integer_or`, `integer_xor`, `integer_not`,
//!   `integer_shift`, `integer_popcount`) operate on 64-bit machine integers: each
//!   operand is narrowed to `i64` (erroring if out of range) before the logic runs.
use crate::builtins::{BuiltinContext, Completion};
use crate::effects::Effect;
use crate::error::Error;
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
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let value = match extract_int(arg)? {
        // i64::MIN has no i64 absolute value; promote.
        IntRef::Small(n) => match n.checked_abs() {
            Some(abs) => Value::int(abs),
            None => Value::integer(-BigInt::from(n)),
        },
        IntRef::Big(n) => Value::integer(n.abs()),
    };
    Ok(Completion::Value(value))
}

/// Builtin function: `__integer_sqrt__`
/// Returns the square root of an integer (truncated to integer).
pub fn builtin_integer_sqrt<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
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
    Ok(Completion::Value(value))
}

/// Builtin function: `__integer_factor__`
/// Returns a prime factor of an integer `n ≥ 2` (`n` itself when it is prime), or nil when
/// splitting `n` would exceed a fixed amount of work. Which factor is unspecified; callers
/// wanting a full factorisation divide it out and repeat.
///
/// Small factors come from trial division; beyond that, primality is Miller–Rabin over the
/// first twelve prime bases (deterministic below 3.3·10²⁴, and a vanishingly unlikely error
/// above) and a composite is split with Pollard–Brent. Rho's cost grows with the square root of
/// the smallest prime factor, so it gets [`RHO_BUDGET`] iterations — enough to find a factor
/// up to about 10¹¹ — before giving up, which keeps a single call bounded.
pub fn builtin_integer_factor<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let n = extract_int(arg)?.to_bigint();
    if n < BigInt::from(2) {
        return Err(Error::InvalidArgument(format!(
            "Cannot factor {n}: expected an integer of at least 2"
        )));
    }
    Ok(Completion::Value(
        prime_factor(n).map_or_else(Value::nil, Value::integer),
    ))
}

const SMALL_PRIMES: [u32; 12] = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37];

/// The polynomial steps Pollard–Brent may take over one `__integer_factor__` call.
const RHO_BUDGET: u64 = 1 << 20;

/// A prime factor of `n ≥ 2`, or `None` once the rho budget runs out.
fn prime_factor(n: BigInt) -> Option<BigInt> {
    for d in 2u32..1000 {
        let d = BigInt::from(d);
        if &d * &d > n {
            return Some(n);
        }
        if n.is_multiple_of(&d) {
            return Some(d);
        }
    }
    let mut budget = RHO_BUDGET;
    let mut n = n;
    while !is_probable_prime(&n) {
        n = pollard_brent(&n, &mut budget)?;
    }
    Some(n)
}

fn is_probable_prime(n: &BigInt) -> bool {
    let one = BigInt::from(1);
    let n_minus_one = n - &one;
    let mut d = n_minus_one.clone();
    let mut s = 0u32;
    while d.is_even() {
        d >>= 1;
        s += 1;
    }
    SMALL_PRIMES.iter().all(|&a| {
        let a = BigInt::from(a);
        if a.is_multiple_of(n) {
            return true;
        }
        let mut x = a.modpow(&d, n);
        if x == one || x == n_minus_one {
            return true;
        }
        (1..s).any(|_| {
            x = x.modpow(&BigInt::from(2), n);
            x == n_minus_one
        })
    })
}

/// A nontrivial factor of the odd composite `n`, by Brent's variant of Pollard's rho, retrying
/// with the next polynomial constant when a cycle closes without one. Each polynomial step
/// spends one unit of `budget`, and `None` means it ran out.
fn pollard_brent(n: &BigInt, budget: &mut u64) -> Option<BigInt> {
    let one = BigInt::from(1);
    let mut c = one.clone();
    loop {
        let mut f = |x: &BigInt| {
            *budget = budget.checked_sub(1)?;
            Some((x * x + &c) % n)
        };
        let (mut y, mut r, mut q) = (BigInt::from(2), 1u64, one.clone());
        let (mut x, mut ys);
        let mut g = one.clone();
        while g == one {
            x = y.clone();
            for _ in 0..r {
                y = f(&y)?;
            }
            let mut k = 0;
            while k < r && g == one {
                ys = y.clone();
                for _ in 0..r.min(128).min(r - k) {
                    y = f(&y)?;
                    q = (q * (&x - &y).abs()) % n;
                }
                g = q.gcd(n);
                k += 128;
                if g == *n {
                    // The batched product overshot: step back one at a time from `ys`.
                    loop {
                        ys = f(&ys)?;
                        g = (&x - &ys).abs().gcd(n);
                        if g != one {
                            break;
                        }
                    }
                }
            }
            r *= 2;
        }
        if g != *n {
            return Some(g);
        }
        c += 1;
    }
}

/// Builtin function: `__integer_sin__`
/// Returns the sine of an integer (treating it as radians, truncated to integer).
pub fn builtin_integer_sin<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let n = extract_int(arg)?;
    Ok(Completion::Value(Value::int(to_f64_lossy(n).sin() as i64)))
}

/// Builtin function: `__integer_cos__`
/// Returns the cosine of an integer (treating it as radians, truncated to integer).
pub fn builtin_integer_cos<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let n = extract_int(arg)?;
    Ok(Completion::Value(Value::int(to_f64_lossy(n).cos() as i64)))
}

/// Builtin function: `__integer_add__`
/// Adds two integers from a tuple.
pub fn builtin_integer_add<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    Ok(Completion::Value(arith(arg, i64::checked_add, |a, b| {
        a + b
    })?))
}

/// Builtin function: `__integer_gcd__`
/// Returns the greatest common divisor of two integers (always non-negative).
/// Used by the `num` module to reduce rationals to canonical form.
pub fn builtin_integer_gcd<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
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
    Ok(Completion::Value(value))
}

/// Builtin function: `__integer_subtract__`
/// Subtracts two integers from a tuple.
pub fn builtin_integer_subtract<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    Ok(Completion::Value(arith(arg, i64::checked_sub, |a, b| {
        a - b
    })?))
}

/// Builtin function: `__integer_multiply__`
/// Multiplies two integers from a tuple.
pub fn builtin_integer_multiply<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    Ok(Completion::Value(arith(arg, i64::checked_mul, |a, b| {
        a * b
    })?))
}

/// Builtin function: `__integer_divide__`
/// Divides two integers from a tuple.
pub fn builtin_integer_divide<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    if is_zero(arg) {
        return Err(Error::InvalidArgument("Division by zero".to_string()));
    }
    // With zero excluded, checked_div only reports None for i64::MIN / -1, which promotes.
    Ok(Completion::Value(arith(arg, i64::checked_div, |a, b| {
        a / b
    })?))
}

/// Builtin function: `__integer_modulo__`
/// Takes modulo of two integers from a tuple.
pub fn builtin_integer_modulo<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    if is_zero(arg) {
        return Err(Error::InvalidArgument("Modulo by zero".to_string()));
    }
    // With zero excluded, checked_rem only reports None for i64::MIN % -1, which promotes.
    Ok(Completion::Value(arith(arg, i64::checked_rem, |a, b| {
        a % b
    })?))
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
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
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

    Ok(Completion::Value(Value::int(result)))
}

/// Bitwise AND of two integers
/// integer_and([int, int]) -> int
pub fn builtin_integer_and<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let (a, b) = extract_two_i64(arg)?;
    Ok(Completion::Value(Value::int(a & b)))
}

/// Bitwise OR of two integers
/// integer_or([int, int]) -> int
pub fn builtin_integer_or<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let (a, b) = extract_two_i64(arg)?;
    Ok(Completion::Value(Value::int(a | b)))
}

/// Bitwise XOR of two integers
/// integer_xor([int, int]) -> int
pub fn builtin_integer_xor<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let (a, b) = extract_two_i64(arg)?;
    Ok(Completion::Value(Value::int(a ^ b)))
}

/// Bitwise NOT of an integer
/// integer_not(int) -> int
pub fn builtin_integer_not<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let n = narrow_to_i64(extract_int(arg)?)?;
    Ok(Completion::Value(Value::int(!n)))
}

/// Shift integer by n bits (positive = left, negative = right)
/// integer_shift([int, int]) -> int
/// Note: This is an arithmetic right shift (sign-extending)
pub fn builtin_integer_shift<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let (value, shift_amount) = extract_two_i64(arg)?;

    if shift_amount == 0 {
        return Ok(Completion::Value(Value::int(value)));
    }

    // Rust's shift operators panic if shift amount is >= bit width
    // We clamp to reasonable values (0-63 for i64)
    let shift_amount_abs = shift_amount.unsigned_abs();

    if shift_amount_abs >= 64 {
        // Shifting by 64+ bits
        if shift_amount > 0 {
            // Left shift by 64+ always gives 0
            return Ok(Completion::Value(Value::int(0)));
        } else {
            // Right shift by 64+ gives 0 or -1 depending on sign
            return Ok(Completion::Value(Value::int(if value >= 0 {
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

    Ok(Completion::Value(Value::int(result)))
}

/// Count number of set bits (population count) in an integer
/// integer_popcount(int) -> int
pub fn builtin_integer_popcount<E: Effect>(
    arg: &Value,
    _ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let n = narrow_to_i64(extract_int(arg)?)?;
    // Use u64's count_ones, handling negative values via two's complement
    Ok(Completion::Value(
        Value::int((n as u64).count_ones() as i64),
    ))
}
