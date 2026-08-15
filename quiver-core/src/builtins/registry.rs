//! The `%registry` builtins: a per-environment name table binding data-value keys to
//! live processes, so processes with no shared ancestor (other sessions on a shared
//! server, detached services) can find each other.
//!
//! Keys are data values — the `%data` encodable subset — and are canonicalised to
//! their notation text at the call site: encoding rejects identity-bearing values and
//! folds away representation differences (constant vs heap binaries, annotations), so
//! the environment stores and compares plain strings, and every key is by construction
//! serializable across a future node boundary.
//!
//! The table lives in the environment, which alone observes every termination path, so
//! all three operations park the caller ([`Completion::Suspend`]) and resume on the
//! environment's value push. Entries only ever name live processes: registration
//! installs a `Watcher::Registered` on the target (refusing an already-terminated one),
//! and the flush of that watcher at termination is what frees the name.

use super::{BuiltinContext, Completion, data};
use crate::effects::Effect;
use crate::error::Error;
use crate::process::RegistryRequest;
use crate::value::Value;

/// Canonicalise a registry key to data-notation text. Identity-bearing values
/// (processes, functions, refs, resources) anywhere in the key are a runtime error —
/// a key must mean the same thing to every process that writes it.
fn encode_key<E: Effect>(key: &Value, ctx: &mut BuiltinContext<E>) -> Result<String, Error> {
    let mut out = String::new();
    data::encode_value(key, ctx, &mut out).map_err(|error| match error {
        Error::InvalidArgument(message) => {
            Error::InvalidArgument(format!("invalid registry key: {message}"))
        }
        other => other,
    })?;
    Ok(out)
}

/// `__registry_register__ [key, pid]`: bind `key` to the process. Answers `Ok`, or nil
/// when the key is already bound or the process has already terminated. The name is
/// freed when the process terminates, so the registry only ever answers live processes.
pub fn builtin_registry_register<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let Value::Tuple(_, payload) = arg else {
        return Err(Error::TypeMismatch {
            expected: "[key, process]".to_string(),
            found: arg.type_name().to_string(),
        });
    };
    let [key, target] = &payload[..] else {
        return Err(Error::ArityMismatch {
            expected: 2,
            found: payload.len(),
        });
    };
    let Value::Process(pid, function_index) = target else {
        return Err(Error::TypeMismatch {
            expected: "process".to_string(),
            found: target.type_name().to_string(),
        });
    };
    let (pid, function_index) = (*pid, *function_index);
    let key = encode_key(key, ctx)?;
    ctx.registry(RegistryRequest::Register {
        key,
        pid,
        function_index,
    })?;
    Ok(Completion::Suspend)
}

/// `__registry_unregister__ key`: remove the binding. Answers `Ok`, or nil when the
/// key was not bound.
pub fn builtin_registry_unregister<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let key = encode_key(arg, ctx)?;
    ctx.registry(RegistryRequest::Unregister { key })?;
    Ok(Completion::Suspend)
}

/// `__registry_lookup__<'p> key`: answer the pid bound to `key` at the process type
/// `'p`, or nil when the key is unbound or the process fails the type test — checked
/// at runtime against the pid's root-function capabilities, with the declared-type
/// variance rules (send contravariant, result covariant, state covariant and strict).
pub fn builtin_registry_lookup<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let Some(expected_type) = ctx.type_argument() else {
        return Err(Error::InvalidArgument(
            "__registry_lookup__ called without a type argument: a lookup must state \
             the process type it expects, and be instantiated where it is applied \
             (%registry.lookup<'p> key)"
                .to_string(),
        ));
    };
    let key = encode_key(arg, ctx)?;
    ctx.registry(RegistryRequest::Lookup { key, expected_type })?;
    Ok(Completion::Suspend)
}
