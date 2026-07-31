//! Packed numeric vector kernels.
//!
//! Each kernel treats a `'bin` as a flat, **little-endian, two's-complement** array of
//! fixed-width signed integer lanes (`width` bytes per lane: 4 or 8). They are deliberately
//! *schema-agnostic*: they never see the `Vec[...]` tuple, its dtype tag, or its scale —
//! that bookkeeping lives in `std/vec.qv`. Anything that could overflow the lane width is
//! **checked**, returning nil (`[]`) rather than wrapping, matching the language's
//! nil-propagation convention. Reductions accumulate into `BigInt`, so they never overflow.
//!
//! Failure discipline: nil is reserved for **value-dependent** conditions (overflow, a
//! length mismatch between two vectors, an out-of-bounds index). A **ragged** buffer —
//! one whose length isn't a whole number of lanes — is a violated invariant (only
//! reachable by hand-forging a `Vec[...]` value), so it is a runtime error, not nil.
use crate::binary::BinaryData;
use crate::builtins::{BuiltinContext, Completion, value_to_i64};
use crate::effects::Effect;
use crate::error::Error;
use crate::value::Value;
use num_bigint::BigInt;
use num_traits::Zero;
use std::rc::Rc;

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
fn checked_width(width: &Value) -> Result<usize, Error> {
    let width = value_to_i64(width)?;
    if width == 4 || width == 8 {
        Ok(width as usize)
    } else {
        Err(Error::InvalidArgument(format!(
            "Unsupported lane width {width} (expected 4 or 8)"
        )))
    }
}

fn nil<E: Effect>() -> Completion<E> {
    Completion::Value(Value::nil())
}

/// Reject a ragged buffer: a length that isn't a whole number of lanes is a violated
/// invariant (a hand-forged `Vec` value, not a data condition) — an error, not nil.
fn check_lanes(len: usize, width: usize) -> Result<(), Error> {
    if len.is_multiple_of(width) {
        Ok(())
    } else {
        Err(Error::InvalidArgument(format!(
            "Ragged vector buffer: {len} bytes is not a whole number of {width}-byte lanes"
        )))
    }
}

/// Shared core for the elementwise binary kernels (`add`/`sub`/`mul`). Reconciling scales
/// or dtypes is the Quiver layer's job; here both buffers must already share a width and
/// length. Returns nil on length mismatch or a lane that overflows `width`.
fn elementwise<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
    op: impl Fn(i64, i64) -> Option<i64>,
) -> Result<Completion<E>, Error> {
    let (a, b, width) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(a), Value::Binary(b), w) => (a, b, w),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let a = ctx.executor.materialize(a)?;
    let b = ctx.executor.materialize(b)?;
    check_lanes(a.len(), width)?;
    check_lanes(b.len(), width)?;

    if a.len() != b.len() {
        return Ok(nil());
    }

    let mut out = Vec::with_capacity(a.len());
    for i in 0..a.len() / width {
        match op(lane(&a, width, i), lane(&b, width, i)).filter(|v| fits(*v, width)) {
            Some(v) => push_lane(&mut out, width, v),
            None => return Ok(nil()), // overflow → nil
        }
    }
    Ok(Completion::Value(Value::Binary(
        ctx.executor.allocate_binary(out)?,
    )))
}

/// Elementwise addition: `vector_add([bin, bin, width]) -> bin | []`
pub fn builtin_vector_add<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    elementwise(arg, ctx, i64::checked_add)
}

/// Elementwise subtraction: `vector_subtract([bin, bin, width]) -> bin | []`
pub fn builtin_vector_subtract<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    elementwise(arg, ctx, i64::checked_sub)
}

/// Elementwise multiplication: `vector_multiply([bin, bin, width]) -> bin | []`
pub fn builtin_vector_multiply<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    elementwise(arg, ctx, i64::checked_mul)
}

/// Shared core for the comparison kernels (`lt`/`eq`/`gt`). Both buffers must already share a
/// width, length, and scale — aligning scales is the Quiver layer's job, so a comparison is
/// between logical values, not raw lanes. Produces a **mask**: one byte per lane, `1` where the
/// predicate holds and `0` otherwise. Returns nil on length mismatch.
fn compare<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
    pred: impl Fn(i64, i64) -> bool,
) -> Result<Completion<E>, Error> {
    let (a, b, width) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(a), Value::Binary(b), w) => (a, b, w),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let a = ctx.executor.materialize(a)?;
    let b = ctx.executor.materialize(b)?;
    check_lanes(a.len(), width)?;
    check_lanes(b.len(), width)?;

    if a.len() != b.len() {
        return Ok(nil());
    }

    let lanes = a.len() / width;
    let mut out = Vec::with_capacity(lanes);
    for i in 0..lanes {
        out.push(u8::from(pred(lane(&a, width, i), lane(&b, width, i))));
    }
    Ok(Completion::Value(Value::Binary(
        ctx.executor.allocate_binary(out)?,
    )))
}

/// Elementwise less-than mask: `vector_less_than([bin, bin, width]) -> bin | []`.
pub fn builtin_vector_less_than<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    compare(arg, ctx, |a, b| a < b)
}

/// Elementwise equality mask: `vector_equal([bin, bin, width]) -> bin | []`.
pub fn builtin_vector_equal<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    compare(arg, ctx, |a, b| a == b)
}

/// Elementwise greater-than mask: `vector_greater_than([bin, bin, width]) -> bin | []`.
pub fn builtin_vector_greater_than<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    compare(arg, ctx, |a, b| a > b)
}

/// Gather the lanes selected by a mask: `vector_take([data, width, mask]) -> bin | []`. `mask` is
/// one byte per lane (non-zero selects); the kept lanes are packed, in order, into a fresh
/// buffer. Nil if the mask length doesn't match the lane count.
pub fn builtin_vector_take<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let (data, width, mask) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(d), w, Value::Binary(m)) => (d, w, m),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let data = ctx.executor.materialize(data)?;
    let mask = ctx.executor.materialize(mask)?;
    check_lanes(data.len(), width)?;

    if mask.len() != data.len() / width {
        return Ok(nil());
    }

    let mut out = Vec::new();
    for (i, &selected) in mask.iter().enumerate() {
        if selected != 0 {
            out.extend_from_slice(&data[i * width..(i + 1) * width]);
        }
    }
    Ok(Completion::Value(Value::Binary(
        ctx.executor.allocate_binary(out)?,
    )))
}

/// Read one lane as a signed integer: `vector_get([bin, width, index]) -> int | []`.
/// Nil for a negative or out-of-bounds index.
pub fn builtin_vector_get<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let (binary, width, index) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(b), w, i) => (b, w, i),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let bytes = ctx.executor.materialize(binary)?;

    check_lanes(bytes.len(), width)?;
    let index = index.as_int().ok_or_else(arity_error)?;
    let Some(index) = index
        .to_i64()
        .and_then(|i| usize::try_from(i).ok())
        .filter(|i| (*i + 1).saturating_mul(width) <= bytes.len())
    else {
        return Ok(nil());
    };
    Ok(Completion::Value(Value::int(lane(&bytes, width, index))))
}

/// Append one signed lane: `vector_push([bin, width, value]) -> bin | []`.
/// Nil if `value` doesn't fit the lane width.
///
/// Appends via an O(1) `Concat`: cloning the existing buffer is a refcount bump (`Owned` is
/// `Rc`-backed), so folding this to build a vector is O(n), not O(n²). The result is a rope
/// that materialises (and compacts) lazily on the next full read — see `Executor::materialize`.
pub fn builtin_vector_push<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let (binary, width, value) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(b), w, v) => (b, w, v),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;

    let value = value.as_int().ok_or_else(arity_error)?;
    let Some(value) = value.to_i64().filter(|v| fits(*v, width)) else {
        return Ok(nil());
    };

    let old = ctx.executor.get_binary_data(binary)?;
    let old_len = old.len();
    check_lanes(old_len, width)?;

    let mut lane_bytes = Vec::with_capacity(width);
    push_lane(&mut lane_bytes, width, value);
    let lane = BinaryData::new(lane_bytes);

    // Empty buffer: the lane *is* the new buffer (avoids a degenerate `Concat` over nil).
    // Otherwise share `old` — a refcount bump on the existing node — as the left of a fresh
    // `Concat`.
    let appended = if old_len == 0 {
        lane
    } else {
        BinaryData::concat(old.clone(), Rc::new(lane))
    };
    Ok(Completion::Value(Value::Binary(
        ctx.executor.allocate_binary_data(appended)?,
    )))
}

/// Sum of all lanes: `vector_sum([bin, width]) -> int`. Exact (BigInt), so never overflows
/// and never nil.
pub fn builtin_vector_sum<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let (binary, width) = match arg {
        Value::Tuple(_, e) if e.len() == 2 => match (&e[0], &e[1]) {
            (Value::Binary(b), w) => (b, w),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let bytes = ctx.executor.materialize(binary)?;
    check_lanes(bytes.len(), width)?;
    let mut acc = BigInt::zero();
    for i in 0..bytes.len() / width {
        acc += lane(&bytes, width, i);
    }
    Ok(Completion::Value(Value::integer(acc)))
}

/// Dot product: `vector_dot([bin, bin, width]) -> int | []`. Exact (BigInt); nil on length
/// mismatch.
pub fn builtin_vector_dot<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let (a, b, width) = match arg {
        Value::Tuple(_, e) if e.len() == 3 => match (&e[0], &e[1], &e[2]) {
            (Value::Binary(a), Value::Binary(b), w) => (a, b, w),
            _ => return Err(arity_error()),
        },
        _ => return Err(arity_error()),
    };
    let width = checked_width(width)?;
    let a = ctx.executor.materialize(a)?;
    let b = ctx.executor.materialize(b)?;
    check_lanes(a.len(), width)?;
    check_lanes(b.len(), width)?;
    if a.len() != b.len() {
        return Ok(nil());
    }
    let mut acc = BigInt::zero();
    for i in 0..a.len() / width {
        acc += BigInt::from(lane(&a, width, i)) * lane(&b, width, i);
    }
    Ok(Completion::Value(Value::integer(acc)))
}

fn arity_error() -> Error {
    Error::TypeMismatch {
        expected: "vector kernel arguments".to_string(),
        found: "invalid argument shape".to_string(),
    }
}
