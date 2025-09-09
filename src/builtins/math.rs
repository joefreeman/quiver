//! Math builtin function implementations

use crate::bytecode::Constant;
use crate::vm::{Error, Value};

/// Builtin function: math:abs
/// Returns the absolute value of an integer
pub fn builtin_math_abs(arg: &Value, _constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => Ok(Value::Integer(n.abs())),
        _other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: "non-integer".to_string(),
        }),
    }
}

/// Builtin function: math:sqrt
/// Returns the square root of an integer (truncated to integer)
pub fn builtin_math_sqrt(arg: &Value, _constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => {
            if *n < 0 {
                Err(Error::InvalidArgument(
                    "Cannot take square root of negative number".to_string(),
                ))
            } else {
                let result = (*n as f64).sqrt() as i64;
                Ok(Value::Integer(result))
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
pub fn builtin_math_sin(arg: &Value, _constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => {
            let result = (*n as f64).sin() as i64;
            Ok(Value::Integer(result))
        }
        _other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: "non-integer".to_string(),
        }),
    }
}

/// Builtin function: math:cos
/// Returns the cosine of an integer (treating it as radians, truncated to integer)
pub fn builtin_math_cos(arg: &Value, _constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => {
            let result = (*n as f64).cos() as i64;
            Ok(Value::Integer(result))
        }
        _other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: "non-integer".to_string(),
        }),
    }
}
