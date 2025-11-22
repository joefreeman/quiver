use crate::effects::NativeEffect;
use quiver_core::builtins::{BuiltinResult, TypeSpec};
use quiver_core::error::Error;
use quiver_core::executor::Executor;
use quiver_core::process::{Action, ProcessId};
use quiver_core::value::{Binary, Value};

/// file_open([path: bin, flags: int, mode: int]) -> File
/// Open a file with the specified flags and permissions
pub fn builtin_file_open(
    process_id: ProcessId,
    value: &Value,
    executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [path, flags, mode] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 3 {
        return Err(Error::ArityMismatch {
            expected: 3,
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

    // Get mode (permissions)
    let Value::Integer(mode) = fields[2] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[2].type_name().to_string(),
        });
    };

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
            .get_heap_binary(*idx)
            .ok_or_else(|| Error::InvalidArgument(format!("Heap binary index {} not found", idx)))?
            .to_vec(),
    };

    // Return Action to request file opening from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::FileOpen {
            path: path_bytes,
            flags: flags as i32,
            mode: mode as u32,
        },
    }))
}

/// file_read([file, offset, length]) -> bin
/// Read from a file at the specified offset (async)
pub fn builtin_file_read(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [file, offset, length] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 3 {
        return Err(Error::ArityMismatch {
            expected: 3,
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

    let Value::Integer(offset) = fields[1] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[1].type_name().to_string(),
        });
    };

    let Value::Integer(length) = fields[2] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[2].type_name().to_string(),
        });
    };

    if offset < 0 {
        return Err(Error::InvalidArgument(format!(
            "Offset must be non-negative, got {}",
            offset
        )));
    }

    if length <= 0 {
        return Err(Error::InvalidArgument(format!(
            "Length must be positive, got {}",
            length
        )));
    }

    // Return Action to request read operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::FileRead {
            resource_id,
            offset: offset as u64,
            length: length as usize,
        },
    }))
}

/// file_write([file, offset, data]) -> int
/// Write to a file at the specified offset (async), returns bytes written
pub fn builtin_file_write(
    process_id: ProcessId,
    value: &Value,
    executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [file, offset, data] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 3 {
        return Err(Error::ArityMismatch {
            expected: 3,
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

    let Value::Integer(offset) = fields[1] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[1].type_name().to_string(),
        });
    };

    // Get data binary
    let data_binary = match &fields[2] {
        Value::Binary(binary) => *binary,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "binary".to_string(),
                found: fields[2].type_name().to_string(),
            });
        }
    };

    if offset < 0 {
        return Err(Error::InvalidArgument(format!(
            "Offset must be non-negative, got {}",
            offset
        )));
    }

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
            .get_heap_binary(*idx)
            .ok_or_else(|| Error::InvalidArgument(format!("Heap binary index {} not found", idx)))?
            .to_vec(),
    };

    // Return Action to request write operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::FileWrite {
            resource_id,
            offset: offset as u64,
            data,
        },
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
        effect: NativeEffect::FileFlush { resource_id },
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
        effect: NativeEffect::FileClose { resource_id },
    }))
}

/// Register file builtin functions
pub fn register_file_builtins(registry: &mut quiver_core::builtins::BuiltinRegistry<NativeEffect>) {
    let file_resource = TypeSpec::Resource("File".to_string());
    let bin_type = TypeSpec::Binary;
    let int_type = TypeSpec::Integer;
    let ok_type = TypeSpec::Tuple(Some("Ok"), vec![]);

    // file_open([path, flags, mode]) -> File
    registry.register(
        "file_open".to_string(),
        builtin_file_open,
        TypeSpec::Tuple(
            None,
            vec![
                (None, bin_type.clone()),
                (None, int_type.clone()),
                (None, int_type.clone()),
            ],
        ),
        file_resource.clone(),
    );

    // file_read([File, offset, length]) -> bin
    registry.register(
        "file_read".to_string(),
        builtin_file_read,
        TypeSpec::Tuple(
            None,
            vec![
                (None, file_resource.clone()),
                (None, int_type.clone()),
                (None, int_type.clone()),
            ],
        ),
        bin_type.clone(),
    );

    // file_write([File, offset, bin]) -> int
    registry.register(
        "file_write".to_string(),
        builtin_file_write,
        TypeSpec::Tuple(
            None,
            vec![
                (None, file_resource.clone()),
                (None, int_type.clone()),
                (None, bin_type),
            ],
        ),
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
