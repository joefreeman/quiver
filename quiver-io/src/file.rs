use crate::effects::NativeEffect;
use crate::util::{binary_bytes, expect_resource, expect_tuple};
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

/// filesystem_stat([path: bin, follow: Ok | nil]) -> [kind, size, modified, perm] | Nil
/// Look up metadata for a path, following a final symlink only when `follow` is `Ok`. Yields
/// nil if the path does not exist.
pub fn builtin_filesystem_stat(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let fields = expect_tuple(value, 2)?;
    let path = binary_bytes(&fields[0], ctx)?;
    let follow = !fields[1].is_nil();

    Ok(Completion::Effect(NativeEffect::Stat { path, follow }))
}

/// filesystem_create_dir([path: bin, all: Ok | nil]) -> Ok
/// Create a directory; with `all`, its missing parents too, accepting one that already exists.
pub fn builtin_filesystem_create_dir(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let fields = expect_tuple(value, 2)?;
    let path = binary_bytes(&fields[0], ctx)?;
    let all = !fields[1].is_nil();

    Ok(Completion::Effect(NativeEffect::CreateDir { path, all }))
}

/// filesystem_remove([path: bin, recursive: Ok | nil]) -> Ok
/// Remove a file, symlink or empty directory; with `recursive`, a directory and its contents.
pub fn builtin_filesystem_remove(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let fields = expect_tuple(value, 2)?;
    let path = binary_bytes(&fields[0], ctx)?;
    let recursive = !fields[1].is_nil();

    Ok(Completion::Effect(NativeEffect::Remove { path, recursive }))
}

/// filesystem_rename([from: bin, to: bin]) -> Ok
/// Atomically rename a path, replacing an existing destination.
pub fn builtin_filesystem_rename(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let fields = expect_tuple(value, 2)?;
    let from = binary_bytes(&fields[0], ctx)?;
    let to = binary_bytes(&fields[1], ctx)?;

    Ok(Completion::Effect(NativeEffect::Rename { from, to }))
}

/// filesystem_copy([from: bin, to: bin, replace: Ok | nil]) -> Ok
/// Copy a regular file's contents and permissions, overwriting the destination only with
/// `replace`.
pub fn builtin_filesystem_copy(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let fields = expect_tuple(value, 3)?;
    let from = binary_bytes(&fields[0], ctx)?;
    let to = binary_bytes(&fields[1], ctx)?;
    let replace = !fields[2].is_nil();

    Ok(Completion::Effect(NativeEffect::Copy { from, to, replace }))
}

/// filesystem_symlink([link: bin, target: bin]) -> Ok
/// Create a symlink at `link` pointing to `target`.
pub fn builtin_filesystem_symlink(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let fields = expect_tuple(value, 2)?;
    let link = binary_bytes(&fields[0], ctx)?;
    let target = binary_bytes(&fields[1], ctx)?;

    Ok(Completion::Effect(NativeEffect::Symlink { link, target }))
}

/// filesystem_read_link(path: bin) -> bin
/// The target a symlink points to, as written.
pub fn builtin_filesystem_read_link(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let path = binary_bytes(value, ctx)?;

    Ok(Completion::Effect(NativeEffect::ReadLink { path }))
}

/// filesystem_set_perm([path: bin, perm: int]) -> Ok
/// Set a path's permission bits. Anything beyond the mode's low 12 bits is a fault.
pub fn builtin_filesystem_set_perm(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let fields = expect_tuple(value, 2)?;
    let path = binary_bytes(&fields[0], ctx)?;
    let perm = value_to_i64(&fields[1])?;
    if !(0..=0o7777).contains(&perm) {
        return Err(Error::InvalidArgument(format!(
            "permission bits must be within 0o7777, got {perm:#o}"
        )));
    }

    Ok(Completion::Effect(NativeEffect::SetPerm {
        path,
        perm: perm as u32,
    }))
}

/// filesystem_canonical(path: bin) -> bin
/// The absolute path with every symlink resolved (`realpath`).
pub fn builtin_filesystem_canonical(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let path = binary_bytes(value, ctx)?;

    Ok(Completion::Effect(NativeEffect::Canonical { path }))
}

/// filesystem_cwd([]) -> bin
/// The host's current working directory.
pub fn builtin_filesystem_cwd(
    _value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    Ok(Completion::Effect(NativeEffect::Cwd))
}

/// filesystem_temp([]) -> bin
/// Create a fresh, empty directory under the host's temp dir. The caller removes it.
pub fn builtin_filesystem_temp(
    _value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    Ok(Completion::Effect(NativeEffect::Temp))
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
    let implementations: [(&str, BuiltinFn<NativeEffect>); 19] = [
        ("file_open", builtin_file_open),
        ("file_read", builtin_file_read),
        ("file_write", builtin_file_write),
        ("file_flush", builtin_file_flush),
        ("file_close", builtin_file_close),
        ("directory_read", builtin_directory_read),
        ("directory_next", builtin_directory_next),
        ("directory_close", builtin_directory_close),
        ("filesystem_stat", builtin_filesystem_stat),
        ("filesystem_create_dir", builtin_filesystem_create_dir),
        ("filesystem_remove", builtin_filesystem_remove),
        ("filesystem_rename", builtin_filesystem_rename),
        ("filesystem_copy", builtin_filesystem_copy),
        ("filesystem_symlink", builtin_filesystem_symlink),
        ("filesystem_read_link", builtin_filesystem_read_link),
        ("filesystem_set_perm", builtin_filesystem_set_perm),
        ("filesystem_canonical", builtin_filesystem_canonical),
        ("filesystem_cwd", builtin_filesystem_cwd),
        ("filesystem_temp", builtin_filesystem_temp),
    ];
    for (name, impl_fn) in implementations {
        registry.attach_implementation(name, impl_fn);
    }
}
