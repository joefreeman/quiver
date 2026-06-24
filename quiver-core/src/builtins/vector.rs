//! Packed numeric vector kernels.
//!
//! Each kernel treats a `'bin` as a flat, **little-endian, two's-complement** array of
//! fixed-width signed integer lanes (`width` bytes per lane: 4 or 8). They are deliberately
//! *schema-agnostic*: they never see the `Vec[...]` tuple, its dtype tag, or its scale —
//! that bookkeeping lives in `std/vec.qv`. Anything that could overflow the lane width is
//! **checked**, returning nil (`[]`) rather than wrapping, matching the language's
//! nil-propagation convention. Reductions accumulate into `BigInt`, so they never overflow.
use crate::builtins::{BuiltinResult, bigint_to_i64};
use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::ProcessId;
use crate::value::Value;
use num_bigint::BigInt;
use num_traits::Zero;

/// Read lane `i` from a contiguous little-endian signed buffer. The caller guarantees
/// `width ∈ {4, 8}` and that the lane is in bounds.
fn lane(bytes: &[u8], width: usize, i: usize) -> i64 {
    let off = i * width;
    match width {
        4 => i32::from_le_bytes(bytes[off..off + 4].try_into().unwrap()) as i64,
        8 => i64::from_le_bytes(bytes[off..off + 8].try_into().unwrap()),
        _ => unreachable!("width validated by caller"),
    }
}

/// Whether `value` fits in a signed lane of `width` bytes.
fn fits(value: i64, width: usize) -> bool {
    match width {
        4 => i32::try_from(value).is_ok(),
        8 => true,
        _ => false,
    }
}

/// Append `value` as a little-endian signed lane of `width` bytes.
fn push_lane(out: &mut Vec<u8>, width: usize, value: i64) {
    match width {
        4 => out.extend_from_slice(&(value as i32).to_le_bytes()),
        8 => out.extend_from_slice(&value.to_le_bytes()),
        _ => unreachable!("width validated by caller"),
    }
}

/// Validate a lane width argument (only 4- and 8-byte lanes are supported for now).
fn checked_width(width: &BigInt) -> Result<usize, Error> {
    let width = bigint_to_i64(width)?;
    if width == 4 || width == 8 {
        Ok(width as usize)
    } else {
        Err(Error::InvalidArgument(format!(
            "Unsupported lane width {width} (expected 4 or 8)"
        )))
    }
}

fn nil<E: Effect>() -> BuiltinResult<E> {
    BuiltinResult::Value(Value::nil())
}

/// Shared core for the elementwise binary kernels (`add`/`sub`/`mul`). Reconciling scales
/// or dtypes is the Quiver layer's job; here both buffers must already share a width and
/// length. Returns nil on length mismatch, ragged buffers, or a lane that overflows `width`.
fn elementwise<E: Effect>(
    arg: &Value,
    executor: &mut Executor<E>,
    op: impl Fn(i64, i64) -> Option<i64>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b, width) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(a), Value::Binary(b), Value::Integer(w)) => (a, b, w),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let a = executor.get_binary_data(a)?.to_vec();
    let b = executor.get_binary_data(b)?.to_vec();

    if a.len() != b.len() || a.len() % width != 0 {
        return Ok(nil());
    }

    let mut out = Vec::with_capacity(a.len());
    for i in 0..a.len() / width {
        match op(lane(&a, width, i), lane(&b, width, i)).filter(|v| fits(*v, width)) {
            Some(v) => push_lane(&mut out, width, v),
            None => return Ok(nil()), // overflow → nil
        }
    }
    Ok(BuiltinResult::Value(Value::Binary(
        executor.allocate_binary(out)?,
    )))
}

/// Elementwise addition: `vec_add([bin, bin, width]) -> bin | []`
pub fn builtin_vec_add<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    elementwise(arg, executor, i64::checked_add)
}

/// Elementwise subtraction: `vec_sub([bin, bin, width]) -> bin | []`
pub fn builtin_vec_sub<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    elementwise(arg, executor, i64::checked_sub)
}

/// Elementwise multiplication: `vec_mul([bin, bin, width]) -> bin | []`
pub fn builtin_vec_mul<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    elementwise(arg, executor, i64::checked_mul)
}

/// Shared core for the comparison kernels (`lt`/`eq`/`gt`). Both buffers must already share a
/// width, length, and scale — aligning scales is the Quiver layer's job, so a comparison is
/// between logical values, not raw lanes. Produces a **mask**: one byte per lane, `1` where the
/// predicate holds and `0` otherwise. Returns nil on length mismatch or a ragged buffer.
fn compare<E: Effect>(
    arg: &Value,
    executor: &mut Executor<E>,
    pred: impl Fn(i64, i64) -> bool,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b, width) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(a), Value::Binary(b), Value::Integer(w)) => (a, b, w),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let a = executor.get_binary_data(a)?.to_vec();
    let b = executor.get_binary_data(b)?.to_vec();

    if a.len() != b.len() || a.len() % width != 0 {
        return Ok(nil());
    }

    let lanes = a.len() / width;
    let mut out = Vec::with_capacity(lanes);
    for i in 0..lanes {
        out.push(u8::from(pred(lane(&a, width, i), lane(&b, width, i))));
    }
    Ok(BuiltinResult::Value(Value::Binary(
        executor.allocate_binary(out)?,
    )))
}

/// Elementwise less-than mask: `vec_lt([bin, bin, width]) -> bin | []`.
pub fn builtin_vec_lt<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    compare(arg, executor, |a, b| a < b)
}

/// Elementwise equality mask: `vec_eq([bin, bin, width]) -> bin | []`.
pub fn builtin_vec_eq<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    compare(arg, executor, |a, b| a == b)
}

/// Elementwise greater-than mask: `vec_gt([bin, bin, width]) -> bin | []`.
pub fn builtin_vec_gt<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    compare(arg, executor, |a, b| a > b)
}

/// Gather the lanes selected by a mask: `vec_take([data, width, mask]) -> bin | []`. `mask` is
/// one byte per lane (non-zero selects); the kept lanes are packed, in order, into a fresh
/// buffer. Nil if the mask length doesn't match the lane count, or the data buffer is ragged.
pub fn builtin_vec_take<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (data, width, mask) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(d), Value::Integer(w), Value::Binary(m)) => (d, w, m),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let data = executor.get_binary_data(data)?.to_vec();
    let mask = executor.get_binary_data(mask)?.to_vec();

    if data.len() % width != 0 || mask.len() != data.len() / width {
        return Ok(nil());
    }

    let mut out = Vec::new();
    for (i, &selected) in mask.iter().enumerate() {
        if selected != 0 {
            out.extend_from_slice(&data[i * width..(i + 1) * width]);
        }
    }
    Ok(BuiltinResult::Value(Value::Binary(
        executor.allocate_binary(out)?,
    )))
}

/// Read one lane as a signed integer: `vec_get([bin, width, index]) -> int | []`.
/// Nil for a negative or out-of-bounds index, or a ragged buffer.
pub fn builtin_vec_get<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (binary, width, index) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(b), Value::Integer(w), Value::Integer(i)) => (b, w, i),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let bytes = executor.get_binary_data(binary)?.to_vec();

    let Some(index) = index.try_into().ok().filter(|i: &usize| {
        bytes.len() % width == 0 && (*i + 1).saturating_mul(width) <= bytes.len()
    }) else {
        return Ok(nil());
    };
    Ok(BuiltinResult::Value(Value::Integer(BigInt::from(lane(
        &bytes, width, index,
    )))))
}

/// Append one signed lane: `vec_push([bin, width, value]) -> bin | []`.
/// Nil if `value` doesn't fit the lane width, or the buffer is ragged.
///
/// TODO: this clones the whole buffer per push, so folding it to build a vector is O(n²).
/// Fine for small vectors; a bulk packer is the eventual replacement.
pub fn builtin_vec_push<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (binary, width, value) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(b), Value::Integer(w), Value::Integer(v)) => (b, w, v),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let mut bytes = executor.get_binary_data(binary)?.to_vec();

    let Some(value) = value.try_into().ok().filter(|v| fits(*v, width)) else {
        return Ok(nil());
    };
    if bytes.len() % width != 0 {
        return Ok(nil());
    }
    push_lane(&mut bytes, width, value);
    Ok(BuiltinResult::Value(Value::Binary(
        executor.allocate_binary(bytes)?,
    )))
}

/// Sum of all lanes: `vec_sum([bin, width]) -> int | []`. Exact (BigInt), so never overflows.
pub fn builtin_vec_sum<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (binary, width) = match arg {
        Value::Tuple(_, e) if e.len() == 2 => match (&e[0], &e[1]) {
            (Value::Binary(b), Value::Integer(w)) => (b, w),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let bytes = executor.get_binary_data(binary)?.to_vec();
    if bytes.len() % width != 0 {
        return Ok(nil());
    }
    let mut acc = BigInt::zero();
    for i in 0..bytes.len() / width {
        acc += lane(&bytes, width, i);
    }
    Ok(BuiltinResult::Value(Value::Integer(acc)))
}

/// Dot product: `vec_dot([bin, bin, width]) -> int | []`. Exact (BigInt); nil on length
/// mismatch or a ragged buffer.
pub fn builtin_vec_dot<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    let (a, b, width) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(a), Value::Binary(b), Value::Integer(w)) => (a, b, w),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let a = executor.get_binary_data(a)?.to_vec();
    let b = executor.get_binary_data(b)?.to_vec();
    if a.len() != b.len() || a.len() % width != 0 {
        return Ok(nil());
    }
    let mut acc = BigInt::zero();
    for i in 0..a.len() / width {
        acc += BigInt::from(lane(&a, width, i)) * lane(&b, width, i);
    }
    Ok(BuiltinResult::Value(Value::Integer(acc)))
}

fn arity_error() -> Error {
    Error::TypeMismatch {
        expected: "vector kernel arguments".to_string(),
        found: "invalid argument shape".to_string(),
    }
}
