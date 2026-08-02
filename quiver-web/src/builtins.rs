//! The web host's builtin capability set, and the browser implementations backing it.
//!
//! The capability is `fetch` plus the system builtins: entropy and clocks. Their signatures are part of the
//! universal contract (`register_system_signatures`); this backs them for an executing web host.
//! Like the native ones these are immediate (synchronous) builtins — `Purity::HostRead`, no effect
//! round-trip — which is why they need no effect backend and run directly in the worker. The
//! file and network groups stay out: a browser has no filesystem or sockets, so a program naming
//! `__tcp_connect__` should fail to compile rather than fail to run. `fetch` takes their place —
//! it is the browser's io floor, and the reason `%http/client` works here at all.
//!
//! Each host object is fetched off the global scope by name rather than through `window` or
//! `DedicatedWorkerGlobalScope`, so the same code serves the worker and the main thread.

use crate::effects::WebEffect;
use quiver_core::binary::BinaryData;
use quiver_core::builtins::{BuiltinContext, BuiltinFn, BuiltinRegistry, Completion};
use quiver_core::error::Error;
use quiver_core::value::{Binary, Value};
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;

/// `Crypto.getRandomValues` rejects a view longer than this, so larger draws are chunked.
const MAX_RANDOM_CHUNK: usize = 65536;

/// A property of the global scope, cast to a web-sys type. Works in both a worker and the main
/// thread, where the concrete global differs but the property does not.
fn global_property<T: JsCast>(name: &str) -> Result<T, Error> {
    js_sys::Reflect::get(&js_sys::global(), &JsValue::from_str(name))
        .ok()
        .and_then(|value| value.dyn_into::<T>().ok())
        .ok_or_else(|| Error::InvalidArgument(format!("`{name}` is unavailable in this context")))
}

/// random_bytes(n) -> bin: n cryptographically secure bytes from the Web Crypto API.
pub fn builtin_random_bytes(
    arg: &Value,
    ctx: &mut BuiltinContext<WebEffect>,
) -> Result<Completion<WebEffect>, Error> {
    let n = match arg {
        Value::Int(n) if *n >= 0 => *n as usize,
        Value::Int(_) => {
            return Err(Error::InvalidArgument(
                "random_bytes requires a non-negative count".to_string(),
            ));
        }
        other => {
            return Err(Error::TypeMismatch {
                expected: "integer".to_string(),
                found: other.type_name().to_string(),
            });
        }
    };
    if n > quiver_core::value::MAX_BINARY_SIZE {
        return Err(Error::InvalidArgument(format!(
            "Size {} exceeds maximum {}",
            n,
            quiver_core::value::MAX_BINARY_SIZE
        )));
    }
    let crypto = global_property::<web_sys::Crypto>("crypto")?;
    let mut bytes = vec![0u8; n];
    for chunk in bytes.chunks_mut(MAX_RANDOM_CHUNK) {
        crypto
            .get_random_values_with_u8_array(chunk)
            .map_err(|e| Error::InvalidArgument(format!("entropy source failed: {e:?}")))?;
    }
    let binary = ctx.executor.allocate_binary_data(BinaryData::new(bytes))?;
    Ok(Completion::Value(Value::Binary(binary)))
}

/// time_now([]) -> int: milliseconds since the Unix epoch (UTC).
pub fn builtin_time_now(
    _arg: &Value,
    _ctx: &mut BuiltinContext<WebEffect>,
) -> Result<Completion<WebEffect>, Error> {
    Ok(Completion::Value(Value::int(js_sys::Date::now() as i64)))
}

/// time_monotonic([]) -> int: milliseconds since an arbitrary origin, steady.
///
/// `performance.now()` alone is measured from the *context's* start, so a process that moved
/// between workers would see the origin jump. Adding `timeOrigin` puts every context on one
/// scale (it is that context's start as epoch milliseconds) while keeping the monotonicity that
/// makes the reading steadier than the wall clock.
pub fn builtin_time_monotonic(
    _arg: &Value,
    _ctx: &mut BuiltinContext<WebEffect>,
) -> Result<Completion<WebEffect>, Error> {
    let performance = global_property::<web_sys::Performance>("performance")?;
    let ms = performance.time_origin() + performance.now();
    Ok(Completion::Value(Value::int(ms as i64)))
}

/// Attach the browser implementations of the system builtins (entropy + clocks).
fn attach_system_builtins(registry: &mut BuiltinRegistry<WebEffect>) {
    let implementations: [(&str, BuiltinFn<WebEffect>); 3] = [
        ("random_bytes", builtin_random_bytes),
        ("time_now", builtin_time_now),
        ("time_monotonic", builtin_time_monotonic),
    ];
    for (name, impl_fn) in implementations {
        registry.attach_implementation(name, impl_fn);
    }
}

/// The web host's registry: the language core plus the system builtins, with implementations
/// attached. Every registry in this crate is built here — the compiler's, the environment's
/// (for its runtime declarations), and each worker's — because a program's capability set is
/// decided by whichever registry compiles it, and any disagreement between them means a program
/// that compiles and then cannot run (or the reverse).
pub fn web_builtins() -> BuiltinRegistry<WebEffect> {
    let mut registry = BuiltinRegistry::with_modules(&quiver_core::builtins::core_modules());
    for module in quiver_core::builtins::system_modules()
        .into_iter()
        .chain(quiver_core::builtins::fetch_modules())
    {
        module(&mut registry);
    }
    attach_system_builtins(&mut registry);
    attach_fetch_builtin(&mut registry);
    registry
}

/// `fetch` parks its process while the backend performs the request; the implementation only
/// unpacks the four binaries into the effect.
fn attach_fetch_builtin(registry: &mut BuiltinRegistry<WebEffect>) {
    registry.attach_implementation("fetch", builtin_fetch);
}

/// The bytes of a binary field, resolving a constant through the executor's table.
fn field_bytes(
    fields: &[Value],
    index: usize,
    ctx: &mut BuiltinContext<WebEffect>,
) -> Result<Vec<u8>, Error> {
    let Some(Value::Binary(binary)) = fields.get(index) else {
        return Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: fields
                .get(index)
                .map(|f| f.type_name().to_string())
                .unwrap_or_else(|| "nothing".to_string()),
        });
    };
    match binary {
        Binary::Constant(index) => {
            match ctx
                .executor
                .get_constant(*index)
                .ok_or(Error::ConstantUndefined(*index))?
            {
                quiver_core::bytecode::Constant::Binary(bytes) => Ok(bytes.clone()),
                _ => Err(Error::TypeMismatch {
                    expected: "binary".to_string(),
                    found: "integer".to_string(),
                }),
            }
        }
        Binary::Data(data) => Ok(data.to_vec()),
    }
}

pub fn builtin_fetch(
    value: &Value,
    ctx: &mut BuiltinContext<WebEffect>,
) -> Result<Completion<WebEffect>, Error> {
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
    let fields = fields.clone();
    Ok(Completion::Effect(WebEffect::Fetch {
        method: field_bytes(&fields, 0, ctx)?,
        url: field_bytes(&fields, 1, ctx)?,
        headers: field_bytes(&fields, 2, ctx)?,
        body: field_bytes(&fields, 3, ctx)?,
    }))
}
