//! The exec builtins: operating-system programs run as children of the host. Thin, like the
//! network builtins — each unpacks its arguments into an effect and parks; the backend spawns,
//! watches and reaps.

use crate::effects::NativeEffect;
use crate::util::{binary_bytes, expect_resource, expect_tuple};
use quiver_core::builtins::{BuiltinContext, BuiltinFn, BuiltinRegistry, Completion, value_to_i64};
use quiver_core::error::Error;
use quiver_core::value::Value;

/// Split a binary of NUL-terminated entries — how list-shaped arguments cross, being the
/// operating system's own encoding — into the entries. Anything after the last NUL would be
/// an unterminated entry, which no encoder produces.
fn entries(bytes: Vec<u8>, what: &str) -> Result<Vec<Vec<u8>>, Error> {
    if bytes.last().is_some_and(|&byte| byte != 0) {
        return Err(Error::InvalidArgument(format!(
            "{what} must be NUL-terminated entries"
        )));
    }
    let mut entries: Vec<Vec<u8>> = bytes.split(|&byte| byte == 0).map(<[u8]>::to_vec).collect();
    // The final terminator leaves an empty tail behind it.
    entries.pop();
    Ok(entries)
}

/// exec_spawn([cmd, args, cwd, env, clear_env]) -> [child, stdin, stdout, stderr]
pub fn builtin_exec_spawn(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let fields = expect_tuple(value, 5)?;
    let cmd = binary_bytes(&fields[0], ctx)?;
    if cmd.is_empty() || cmd.contains(&0) {
        return Err(Error::InvalidArgument(
            "a program name must be non-empty and contain no NUL".to_string(),
        ));
    }
    Ok(Completion::Effect(NativeEffect::ExecSpawn {
        cmd,
        args: entries(binary_bytes(&fields[1], ctx)?, "args")?,
        cwd: binary_bytes(&fields[2], ctx)?,
        env: entries(binary_bytes(&fields[3], ctx)?, "env")?,
        clear_env: !fields[4].is_nil(),
    }))
}

/// exec_kill([child, signal]) -> Ok
pub fn builtin_exec_kill(
    value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let fields = expect_tuple(value, 2)?;
    let resource_id = expect_resource(&fields[0])?;
    let signal = value_to_i64(&fields[1])?;
    if !(1..=64).contains(&signal) {
        return Err(Error::InvalidArgument(format!(
            "{signal} is not a signal number"
        )));
    }
    Ok(Completion::Effect(NativeEffect::ExecKill {
        resource_id,
        signal: signal as i32,
    }))
}

/// Attach the native implementations of the exec builtins.
pub fn attach_exec_builtins(registry: &mut BuiltinRegistry<NativeEffect>) {
    let implementations: [(&str, BuiltinFn<NativeEffect>); 2] = [
        ("exec_spawn", builtin_exec_spawn),
        ("exec_kill", builtin_exec_kill),
    ];
    for (name, impl_fn) in implementations {
        registry.attach_implementation(name, impl_fn);
    }
}

#[cfg(test)]
mod tests {
    use super::entries;

    #[test]
    fn entries_are_nul_terminated() {
        assert_eq!(entries(Vec::new(), "args").unwrap(), Vec::<Vec<u8>>::new());
        assert_eq!(entries(b"\0".to_vec(), "args").unwrap(), vec![Vec::new()]);
        assert_eq!(
            entries(b"a\0bc\0".to_vec(), "args").unwrap(),
            vec![b"a".to_vec(), b"bc".to_vec()]
        );
        assert!(entries(b"a\0b".to_vec(), "args").is_err());
    }
}
