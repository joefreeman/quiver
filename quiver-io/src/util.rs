//! Small shared helpers for the IO builtin implementations.

use crate::effects::NativeEffect;
use quiver_core::builtins::BuiltinContext;
use quiver_core::error::Error;
use quiver_core::value::{Binary, ResourceId, Value};

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

/// The bytes of a binary value, resolving a constant through the executor's table.
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
    match binary {
        Binary::Constant(index) => match ctx
            .executor
            .get_constant(*index)
            .ok_or(Error::ConstantUndefined(*index))?
        {
            quiver_core::bytecode::Constant::Binary(bytes) => Ok(bytes.clone()),
            _ => Err(Error::TypeMismatch {
                expected: "binary".to_string(),
                found: "integer".to_string(),
            }),
        },
        Binary::Data(data) => Ok(data.to_vec()),
    }
}
