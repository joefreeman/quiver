//! Small shared helpers for the IO builtin implementations.

use crate::effects::NativeEffect;
use quiver_core::builtins::BuiltinContext;
use quiver_core::error::Error;
use quiver_core::value::{ResourceId, Value};

/// Extract a resource id from a value that must be a resource handle. The single-resource IO
/// builtins (close/flush/next/accept/…) take the handle directly, not wrapped in a tuple.
pub fn expect_resource(value: &Value) -> Result<ResourceId, Error> {
    match value {
        Value::Resource(id, _) => Ok(*id),
        _ => Err(Error::TypeMismatch {
            expected: "resource".to_string(),
            found: value.type_name().to_string(),
        }),
    }
}

/// The fields of a tuple argument of exactly `arity` fields.
pub fn expect_tuple(value: &Value, arity: usize) -> Result<&[Value], Error> {
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };
    if fields.len() != arity {
        return Err(Error::ArityMismatch {
            expected: arity,
            found: fields.len(),
        });
    }
    Ok(fields)
}

/// The bytes of a binary value. The executor resolves a constant, so a path or a payload
/// that arrived as one (over the wire, say) reads the same as one that owns its bytes.
pub fn binary_bytes(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Vec<u8>, Error> {
    let Value::Binary(binary) = value else {
        return Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: value.type_name().to_string(),
        });
    };
    Ok(ctx.executor.get_binary_data(binary)?.to_vec())
}
