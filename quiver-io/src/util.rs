//! Small shared helpers for the IO builtin implementations.

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
