use crate::effects::NativeEffect;
use crate::util::expect_resource;
use quiver_core::builtins::{BuiltinFn, BuiltinRegistry, BuiltinResult, bigint_to_i64};
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
    let Value::Integer(flags) = &fields[1] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[1].type_name().to_string(),
        });
    };
    let flags = bigint_to_i64(flags)? as i32;

    // Get mode (permissions)
    let Value::Integer(mode) = &fields[2] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[2].type_name().to_string(),
        });
    };
    let mode = bigint_to_i64(mode)? as u32;

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
            flags,
            mode,
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

    let Value::Integer(offset) = &fields[1] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[1].type_name().to_string(),
        });
    };
    let offset = bigint_to_i64(offset)?;

    let Value::Integer(length) = &fields[2] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[2].type_name().to_string(),
        });
    };
    let length = bigint_to_i64(length)?;

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

    let Value::Integer(offset) = &fields[1] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[1].type_name().to_string(),
        });
    };
    let offset = bigint_to_i64(offset)?;

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
    let resource_id = expect_resource(value)?;

    // Return Action to request flush operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::FileFlush { resource_id },
    }))
}

/// file_close(file) -> Ok
/// Close a file (async)
pub fn builtin_file_close(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    let resource_id = expect_resource(value)?;

    // Return Action to request close operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::FileClose { resource_id },
    }))
}

/// directory_read(path: bin) -> Dir
/// Open a directory for iteration, returning a resource that yields one entry per `directory_next`.
pub fn builtin_directory_read(
    process_id: ProcessId,
    value: &Value,
    executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    let path_binary = match value {
        Value::Binary(binary) => *binary,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "binary".to_string(),
                found: value.type_name().to_string(),
            });
        }
    };

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

    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::ReadDirOpen { path: path_bytes },
    }))
}

/// filesystem_stat(path: bin) -> [kind, size, modified, mode] | Nil
/// Look up metadata for a path (following symlinks). Yields nil if the path does not exist.
pub fn builtin_filesystem_stat(
    process_id: ProcessId,
    value: &Value,
    executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    let path_binary = match value {
        Value::Binary(binary) => *binary,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "binary".to_string(),
                found: value.type_name().to_string(),
            });
        }
    };

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

    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::Stat { path: path_bytes },
    }))
}

/// directory_next(dir: Dir) -> bin | Nil
/// Get the next entry name from a directory, or Nil once exhausted.
pub fn builtin_directory_next(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    let resource_id = expect_resource(value)?;

    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::ReadDirNext { resource_id },
    }))
}

/// directory_close(dir: Dir) -> Ok
/// Close a directory resource.
pub fn builtin_directory_close(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    let resource_id = expect_resource(value)?;

    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::ReadDirClose { resource_id },
    }))
}

/// Attach the native (io-uring) implementations of the file builtins. Their signatures are part
/// of the universal contract (registered everywhere via `core_modules`); this backs them with a
/// real runtime for an executing host.
pub fn attach_file_builtins(registry: &mut BuiltinRegistry<NativeEffect>) {
    let implementations: [(&str, BuiltinFn<NativeEffect>); 9] = [
        ("file_open", builtin_file_open),
        ("file_read", builtin_file_read),
        ("file_write", builtin_file_write),
        ("file_flush", builtin_file_flush),
        ("file_close", builtin_file_close),
        ("directory_read", builtin_directory_read),
        ("directory_next", builtin_directory_next),
        ("directory_close", builtin_directory_close),
        ("filesystem_stat", builtin_filesystem_stat),
    ];
    for (name, impl_fn) in implementations {
        registry.attach_implementation(name, impl_fn);
    }
}
