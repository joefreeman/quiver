//! Native implementations of the system builtins: OS entropy and clocks. Their signatures are
//! part of the universal contract (registered everywhere via `core_modules`); this backs them
//! for an executing native host. These are immediate (synchronous) builtins — no effect
//! round-trip.

use crate::NativeEffect;
use quiver_core::binary::BinaryData;
use quiver_core::builtins::{BuiltinContext, BuiltinFn, BuiltinRegistry, Completion};
use quiver_core::error::Error;
use quiver_core::value::Value;
use std::sync::OnceLock;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

/// random_bytes(n) -> bin: n cryptographically secure bytes from the OS entropy source.
pub fn builtin_random_bytes(
    arg: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
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
    let mut bytes = vec![0u8; n];
    getrandom::fill(&mut bytes)
        .map_err(|e| Error::InvalidArgument(format!("entropy source failed: {e}")))?;
    let binary = ctx.executor.allocate_binary_data(BinaryData::new(bytes))?;
    Ok(Completion::Value(Value::Binary(binary)))
}

/// time_now([]) -> int: nanoseconds since the Unix epoch (UTC).
pub fn builtin_time_now(
    _arg: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let ns = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| Error::InvalidArgument(format!("system clock before epoch: {e}")))?
        .as_nanos();
    Ok(Completion::Value(Value::int(
        i64::try_from(ns).expect("system clock beyond 2262"),
    )))
}

/// time_monotonic([]) -> int: nanoseconds since an arbitrary per-run origin. Steady (never
/// steps backwards); only differences are meaningful.
pub fn builtin_time_monotonic(
    _arg: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    static ORIGIN: OnceLock<Instant> = OnceLock::new();
    let origin = *ORIGIN.get_or_init(Instant::now);
    Ok(Completion::Value(Value::int(
        i64::try_from(origin.elapsed().as_nanos()).expect("monotonic clock overflow"),
    )))
}

/// Attach the native implementations of the system builtins (entropy + clocks).
pub fn attach_system_builtins(registry: &mut BuiltinRegistry<NativeEffect>) {
    let implementations: [(&str, BuiltinFn<NativeEffect>); 3] = [
        ("random_bytes", builtin_random_bytes),
        ("time_now", builtin_time_now),
        ("time_monotonic", builtin_time_monotonic),
    ];
    for (name, impl_fn) in implementations {
        registry.attach_implementation(name, impl_fn);
    }
}
