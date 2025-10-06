//! IO builtin function implementations

use crate::scheduler::Scheduler;
use crate::vm::{Error, Value};

/// Builtin function: io:print
/// Prints a Str[bin] tuple to stdout without a trailing newline
pub fn builtin_io_print(arg: &Value, program: &mut Scheduler) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, fields) => {
            if fields.len() != 1 {
                return Err(Error::InvalidArgument(
                    "print requires a Str tuple with exactly one field".to_string(),
                ));
            }
            match &fields[0] {
                Value::Binary(binary) => {
                    if let Ok(data) = program.get_binary_bytes(binary) {
                        match str::from_utf8(&data) {
                            Ok(s) => print!("{}", s),
                            Err(_) => print!("<invalid UTF-8>"),
                        }
                    } else {
                        print!("<invalid binary reference>");
                    }
                    Ok(Value::ok())
                }
                _ => Err(Error::InvalidArgument(
                    "print requires a Str tuple containing a binary".to_string(),
                )),
            }
        }
        _ => Err(Error::InvalidArgument(
            "print requires a Str tuple".to_string(),
        )),
    }
}

/// Builtin function: io:println
/// Prints a Str[bin] tuple to stdout with a trailing newline
pub fn builtin_io_println(arg: &Value, program: &mut Scheduler) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, fields) => {
            if fields.len() != 1 {
                return Err(Error::InvalidArgument(
                    "println requires a Str tuple with exactly one field".to_string(),
                ));
            }
            match &fields[0] {
                Value::Binary(binary) => {
                    if let Ok(data) = program.get_binary_bytes(binary) {
                        match str::from_utf8(&data) {
                            Ok(s) => println!("{}", s),
                            Err(_) => println!("<invalid UTF-8>"),
                        }
                    } else {
                        println!("<invalid binary reference>");
                    }
                    Ok(Value::ok())
                }
                _ => Err(Error::InvalidArgument(
                    "println requires a Str tuple containing a binary".to_string(),
                )),
            }
        }
        _ => Err(Error::InvalidArgument(
            "println requires a Str tuple".to_string(),
        )),
    }
}
