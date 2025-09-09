//! IO builtin function implementations

use crate::bytecode::Constant;
use crate::bytecode::TypeId;
use crate::vm::{Error, Value};

/// Builtin function: io:print
/// Prints a value to stdout without a trailing newline
/// Supports both integers and binary values (interpreted as UTF-8 strings)
pub fn builtin_io_print(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => {
            print!("{}", n);
            Ok(Value::Tuple(TypeId::OK, vec![]))
        }
        Value::Binary(const_index) => {
            if let Some(Constant::Binary(data)) = constants.get(*const_index) {
                match std::str::from_utf8(data) {
                    Ok(s) => print!("{}", s),
                    Err(_) => print!("<invalid UTF-8 binary>"),
                }
            } else {
                print!("<invalid binary constant>");
            }
            Ok(Value::Tuple(TypeId::OK, vec![]))
        }
        other => {
            print!("{}", other);
            Ok(Value::Tuple(TypeId::OK, vec![]))
        }
    }
}

/// Builtin function: io:println
/// Prints a value to stdout with a trailing newline
/// Supports both integers and binary values (interpreted as UTF-8 strings)
pub fn builtin_io_println(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => {
            println!("{}", n);
            Ok(Value::Tuple(TypeId::OK, vec![]))
        }
        Value::Binary(const_index) => {
            if let Some(Constant::Binary(data)) = constants.get(*const_index) {
                match std::str::from_utf8(data) {
                    Ok(s) => println!("{}", s),
                    Err(_) => println!("<invalid UTF-8 binary>"),
                }
            } else {
                println!("<invalid binary constant>");
            }
            Ok(Value::Tuple(TypeId::OK, vec![]))
        }
        other => {
            println!("{}", other);
            Ok(Value::Tuple(TypeId::OK, vec![]))
        }
    }
}
