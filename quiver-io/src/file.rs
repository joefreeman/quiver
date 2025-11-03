use crate::effects::NativeEffect;
use quiver_core::builtins::{BuiltinResult, TypeSpec};
use quiver_core::error::Error;
use quiver_core::executor::Executor;
use quiver_core::process::{Action, ProcessId};
use quiver_core::value::{Binary, Value};

/// file_open([path: bin, flags: int, buffer_size: int, mode: int]) -> File
/// Open a file with the specified flags, buffer size, and permissions
pub fn builtin_file_open(
    process_id: ProcessId,
    value: &Value,
    executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [path, flags, buffer_size, mode] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 4 {
        return Err(Error::ArityMismatch {
            expected: 4,
            found: fields.len(),
        });
    }

    // Get path binary
    let path_binary = match &fields[0] {
        Value::Binary(binary) => *binary,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "binary".to_string(),
                found: fields[0].type_name().to_string(),
            });
        }
    };

    // Get flags
    let Value::Integer(flags) = fields[1] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[1].type_name().to_string(),
        });
    };

    // Get buffer size
    let Value::Integer(buffer_size) = fields[2] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[2].type_name().to_string(),
        });
    };

    // Get mode (permissions)
    let Value::Integer(mode) = fields[3] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[3].type_name().to_string(),
        });
    };

    if buffer_size <= 0 {
        return Err(Error::InvalidArgument(format!(
            "Buffer size must be positive, got {}",
            buffer_size
        )));
    }

    // Get path bytes from binary
    let path_bytes = match &path_binary {
        Binary::Constant(idx) => {
            let constant = executor
                .get_constant(*idx)
                .ok_or(Error::ConstantUndefined(*idx))?;
            match constant {
                quiver_core::bytecode::Constant::Binary(bytes) => bytes.clone(),
                _ => {
                    return Err(Error::TypeMismatch {
                        expected: "binary".to_string(),
                        found: "integer".to_string(),
                    });
                }
            }
        }
        Binary::Heap(idx) => executor
            .heap
            .get(*idx)
            .ok_or_else(|| Error::InvalidArgument(format!("Heap binary index {} not found", idx)))?
            .to_vec(),
    };

    // Return Action to request file opening from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::OpenFile {
            path: path_bytes,
            flags: flags as i32,
            mode: mode as u32,
            buffer_size: buffer_size as usize,
        },
    }))
}

/// file_read([file]) -> bin
/// Read from a file (async)
pub fn builtin_file_read(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract file resource from tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 1 {
        return Err(Error::ArityMismatch {
            expected: 1,
            found: fields.len(),
        });
    }

    let resource_id = match &fields[0] {
        Value::Resource(id, _) => *id,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "resource".to_string(),
                found: fields[0].type_name().to_string(),
            });
        }
    };

    // Return Action to request read operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::Read { resource_id },
    }))
}

/// file_write([file, data]) -> int
/// Write to a file (async), returns bytes written
pub fn builtin_file_write(
    process_id: ProcessId,
    value: &Value,
    executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [file, data] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 2 {
        return Err(Error::ArityMismatch {
            expected: 2,
            found: fields.len(),
        });
    }

    let resource_id = match &fields[0] {
        Value::Resource(id, _) => *id,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "resource".to_string(),
                found: fields[0].type_name().to_string(),
            });
        }
    };

    // Get data binary
    let data_binary = match &fields[1] {
        Value::Binary(binary) => *binary,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "binary".to_string(),
                found: fields[1].type_name().to_string(),
            });
        }
    };

    // Get data bytes
    let data = match &data_binary {
        Binary::Constant(idx) => {
            let constant = executor
                .get_constant(*idx)
                .ok_or(Error::ConstantUndefined(*idx))?;
            match constant {
                quiver_core::bytecode::Constant::Binary(bytes) => bytes.clone(),
                _ => {
                    return Err(Error::TypeMismatch {
                        expected: "binary".to_string(),
                        found: "integer".to_string(),
                    });
                }
            }
        }
        Binary::Heap(idx) => executor
            .heap
            .get(*idx)
            .ok_or_else(|| Error::InvalidArgument(format!("Heap binary index {} not found", idx)))?
            .to_vec(),
    };

    // Return Action to request write operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::Write { resource_id, data },
    }))
}

/// file_flush([file]) -> Ok
/// Flush a file (async)
pub fn builtin_file_flush(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract file resource from tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 1 {
        return Err(Error::ArityMismatch {
            expected: 1,
            found: fields.len(),
        });
    }

    let resource_id = match &fields[0] {
        Value::Resource(id, _) => *id,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "resource".to_string(),
                found: fields[0].type_name().to_string(),
            });
        }
    };

    // Return Action to request flush operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::Flush { resource_id },
    }))
}

/// file_close([file]) -> Ok
/// Close a file (async)
pub fn builtin_file_close(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract file resource from tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 1 {
        return Err(Error::ArityMismatch {
            expected: 1,
            found: fields.len(),
        });
    }

    let resource_id = match &fields[0] {
        Value::Resource(id, _) => *id,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "resource".to_string(),
                found: fields[0].type_name().to_string(),
            });
        }
    };

    // Return Action to request close operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::Close { resource_id },
    }))
}

/// Register file builtin functions
pub fn register_file_builtins(registry: &mut quiver_core::builtins::BuiltinRegistry<NativeEffect>) {
    let file_resource = TypeSpec::Resource("File".to_string());
    let bin_type = TypeSpec::Binary;
    let int_type = TypeSpec::Integer;
    let ok_type = TypeSpec::Tuple(Some("Ok"), vec![]);

    // file_open([path, flags, buffer_size, mode]) -> File
    registry.register(
        "file_open".to_string(),
        builtin_file_open,
        TypeSpec::Tuple(
            None,
            vec![
                (None, bin_type.clone()),
                (None, int_type.clone()),
                (None, int_type.clone()),
                (None, int_type.clone()),
            ],
        ),
        file_resource.clone(),
    );

    // file_read([File]) -> bin
    registry.register(
        "file_read".to_string(),
        builtin_file_read,
        TypeSpec::Tuple(None, vec![(None, file_resource.clone())]),
        bin_type.clone(),
    );

    // file_write([File, bin]) -> int
    registry.register(
        "file_write".to_string(),
        builtin_file_write,
        TypeSpec::Tuple(None, vec![(None, file_resource.clone()), (None, bin_type)]),
        int_type,
    );

    // file_flush([File]) -> Ok
    registry.register(
        "file_flush".to_string(),
        builtin_file_flush,
        TypeSpec::Tuple(None, vec![(None, file_resource.clone())]),
        ok_type.clone(),
    );

    // file_close([File]) -> Ok
    registry.register(
        "file_close".to_string(),
        builtin_file_close,
        TypeSpec::Tuple(None, vec![(None, file_resource)]),
        ok_type,
    );
}
