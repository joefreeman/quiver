//! IO builtin function implementations

use crate::bytecode::Constant;
use crate::vm::{BinaryRef, Error, Value};

/// Helper function to get bytes from a BinaryRef using the constants array
fn get_binary_bytes_from_ref<'a>(
    binary_ref: &'a BinaryRef,
    constants: &'a [Constant],
) -> Option<&'a [u8]> {
    match binary_ref {
        BinaryRef::Constant(index) => match constants.get(*index) {
            Some(Constant::Binary(bytes)) => Some(bytes),
            _ => None,
        },
        BinaryRef::Heap(rc_bytes) => Some(rc_bytes),
    }
}

/// Builtin function: io:print
/// Prints a value to stdout without a trailing newline
/// Supports both integers and binary values (interpreted as UTF-8 strings)
pub fn builtin_io_print(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Integer(n) => {
            print!("{}", n);
            Ok(Value::ok())
        }
        Value::Binary(binary_ref) => {
            if let Some(data) = get_binary_bytes_from_ref(binary_ref, constants) {
                match std::str::from_utf8(data) {
                    Ok(s) => print!("{}", s),
                    Err(_) => print!("<invalid UTF-8 binary>"),
                }
            } else {
                print!("<invalid binary reference>");
            }
            Ok(Value::ok())
        }
        other => {
            print!("{}", other);
            Ok(Value::ok())
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
            Ok(Value::ok())
        }
        Value::Binary(binary_ref) => {
            if let Some(data) = get_binary_bytes_from_ref(binary_ref, constants) {
                match std::str::from_utf8(data) {
                    Ok(s) => println!("{}", s),
                    Err(_) => println!("<invalid UTF-8 binary>"),
                }
            } else {
                println!("<invalid binary reference>");
            }
            Ok(Value::ok())
        }
        other => {
            println!("{}", other);
            Ok(Value::ok())
        }
    }
}
