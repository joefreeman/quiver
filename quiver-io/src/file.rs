use crate::effects::NativeEffect;
use crate::util::{binary_bytes, expect_resource};
use quiver_core::builtins::{BuiltinContext, BuiltinFn, BuiltinRegistry, Completion, value_to_i64};
use quiver_core::error::Error;
use quiver_core::value::Value;

/// file_open([path: bin, flags: int, mode: int]) -> File
/// Open a file with the specified flags and permissions
pub fn builtin_file_open(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
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

    let path = binary_bytes(&fields[0], ctx)?;

    // Get flags
    let flags = value_to_i64(&fields[1])? as i32;

    // Get mode (permissions)
    let mode = value_to_i64(&fields[2])? as u32;

    // Return Action to request file opening from Environment
    Ok(Completion::Effect(NativeEffect::FileOpen {
        path,
        flags,
        mode,
    }))
}

/// file_read([file, offset, length]) -> bin
/// Read from a file at the specified offset (async)
pub fn builtin_file_read(
    value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
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

    let offset = value_to_i64(&fields[1])?;

    let length = value_to_i64(&fields[2])?;

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
    Ok(Completion::Effect(NativeEffect::FileRead {
        resource_id,
        offset: offset as u64,
        length: length as usize,
    }))
}

/// file_write([file, offset, data]) -> int
/// Write to a file at the specified offset (async), returns bytes written
pub fn builtin_file_write(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
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

    let offset = value_to_i64(&fields[1])?;

    let data = binary_bytes(&fields[2], ctx)?;

    if offset < 0 {
        return Err(Error::InvalidArgument(format!(
            "Offset must be non-negative, got {}",
            offset
        )));
    }

    // Return Action to request write operation from Environment
    Ok(Completion::Effect(NativeEffect::FileWrite {
        resource_id,
        offset: offset as u64,
        data,
    }))
}

/// file_flush([file]) -> Ok
/// Flush a file (async)
pub fn builtin_file_flush(
    value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let resource_id = expect_resource(value)?;

    // Return Action to request flush operation from Environment
    Ok(Completion::Effect(NativeEffect::FileFlush { resource_id }))
}

/// file_close(file) -> Ok
/// Close a file (async)
pub fn builtin_file_close(
    value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let resource_id = expect_resource(value)?;

    // Return Action to request close operation from Environment
    Ok(Completion::Effect(NativeEffect::FileClose { resource_id }))
}

/// directory_read(path: bin) -> Dir
/// Open a directory for iteration, returning a resource that yields one entry per `directory_next`.
pub fn builtin_directory_read(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let path = binary_bytes(value, ctx)?;

    Ok(Completion::Effect(NativeEffect::ReadDirOpen { path }))
}

/// filesystem_stat(path: bin) -> [kind, size, modified, mode] | Nil
/// Look up metadata for a path (following symlinks). Yields nil if the path does not exist.
pub fn builtin_filesystem_stat(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let path = binary_bytes(value, ctx)?;

    Ok(Completion::Effect(NativeEffect::Stat { path }))
}

/// directory_next(dir: Dir) -> bin | Nil
/// Get the next entry name from a directory, or Nil once exhausted.
pub fn builtin_directory_next(
    value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let resource_id = expect_resource(value)?;

    Ok(Completion::Effect(NativeEffect::ReadDirNext {
        resource_id,
    }))
}

/// directory_close(dir: Dir) -> Ok
/// Close a directory resource.
pub fn builtin_directory_close(
    value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let resource_id = expect_resource(value)?;

    Ok(Completion::Effect(NativeEffect::ReadDirClose {
        resource_id,
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
