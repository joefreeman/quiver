//! Native implementations of the system builtins: OS entropy, clocks and the time zone
//! database. Their signatures are
//! part of the universal contract (registered everywhere via `core_modules`); this backs them
//! for an executing native host. These are immediate (synchronous) builtins — no effect
//! round-trip.

use crate::NativeEffect;
use crate::util::binary_bytes;
use quiver_core::binary::BinaryData;
use quiver_core::builtins::{BuiltinContext, BuiltinFn, BuiltinRegistry, Completion};
use quiver_core::error::Error;
use quiver_core::value::Value;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

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

/// time_zone(name) -> bin | nil: the named zone's TZif data from the system database
/// (`$TZDIR`, else `/usr/share/zoneinfo`); nil when it has no such zone.
pub fn builtin_time_zone(
    arg: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let data = String::from_utf8(binary_bytes(arg, ctx)?)
        .ok()
        .filter(|name| is_zone_name(name))
        .and_then(|name| std::fs::read(zoneinfo_dir().join(name)).ok())
        .filter(|bytes| bytes.starts_with(b"TZif"));
    Ok(Completion::Value(match data {
        Some(bytes) => Value::Binary(ctx.executor.allocate_binary(bytes)?),
        None => Value::nil(),
    }))
}

/// time_zone_local([]) -> bin | nil: the IANA name of the host's zone — from `TZ`, else the
/// `/etc/localtime` link, else `/etc/timezone`; nil when none of them names one.
pub fn builtin_time_zone_local(
    _arg: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    Ok(Completion::Value(match local_zone_name() {
        Some(name) => Value::Binary(ctx.executor.allocate_binary(name.into_bytes())?),
        None => Value::nil(),
    }))
}

fn zoneinfo_dir() -> PathBuf {
    std::env::var_os("TZDIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("/usr/share/zoneinfo"))
}

/// An IANA zone name: `/`-separated components of letters, digits, `_`, `-` and `+`, none
/// empty or starting with a dot — so a name can never reach outside the database directory.
fn is_zone_name(name: &str) -> bool {
    name.split('/').all(|component| {
        !component.is_empty()
            && !component.starts_with('.')
            && component
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | '+'))
    })
}

/// The zone name in a path into a zoneinfo directory (`/usr/share/zoneinfo/Europe/London`).
fn zone_in_path(path: &str) -> Option<String> {
    let (_, name) = path.rsplit_once("zoneinfo/")?;
    is_zone_name(name).then(|| name.to_string())
}

fn local_zone_name() -> Option<String> {
    // An empty `TZ` is UTC, per POSIX; a leading `:` marks an implementation-defined value,
    // here a zone name or a path to a zone file.
    if let Ok(tz) = std::env::var("TZ") {
        let tz = tz.strip_prefix(':').unwrap_or(&tz);
        return match tz {
            "" => Some("UTC".to_string()),
            _ if tz.starts_with('/') => zone_in_path(tz),
            _ => is_zone_name(tz).then(|| tz.to_string()),
        };
    }
    if let Ok(target) = std::fs::read_link("/etc/localtime") {
        return zone_in_path(&target.to_string_lossy());
    }
    std::fs::read_to_string("/etc/timezone")
        .ok()
        .map(|name| name.trim().to_string())
        .filter(|name| is_zone_name(name))
}

/// Attach the native implementations of the system builtins (entropy, clocks, zones).
pub fn attach_system_builtins(registry: &mut BuiltinRegistry<NativeEffect>) {
    let implementations: [(&str, BuiltinFn<NativeEffect>); 4] = [
        ("random_bytes", builtin_random_bytes),
        ("time_now", builtin_time_now),
        ("time_zone", builtin_time_zone),
        ("time_zone_local", builtin_time_zone_local),
    ];
    for (name, impl_fn) in implementations {
        registry.attach_implementation(name, impl_fn);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zone_names_cannot_leave_the_database() {
        assert!(is_zone_name("Europe/London"));
        assert!(is_zone_name("America/Argentina/Buenos_Aires"));
        assert!(is_zone_name("Etc/GMT+5"));
        assert!(!is_zone_name(""));
        assert!(!is_zone_name("/etc/passwd"));
        assert!(!is_zone_name("../etc/passwd"));
        assert!(!is_zone_name("Europe/../../etc"));
        assert!(!is_zone_name("Europe//London"));
        assert!(!is_zone_name("Europe/.hidden"));
    }

    #[test]
    fn zone_in_path_takes_the_name_after_zoneinfo() {
        assert_eq!(
            zone_in_path("/usr/share/zoneinfo/Europe/London").as_deref(),
            Some("Europe/London")
        );
        assert_eq!(
            zone_in_path("../usr/share/zoneinfo/America/New_York").as_deref(),
            Some("America/New_York")
        );
        assert_eq!(zone_in_path("/etc/localtime"), None);
    }
}
