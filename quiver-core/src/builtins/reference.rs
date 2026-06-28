//! The reference builtin: creates a unique, opaque ref.
//!
//! Exposed to Quiver as the `%ref` standard-library module (a single nilary function), so
//! `ref = %ref, tag = ref` mints a fresh ref. Ref creation needs the executor's per-worker
//! counter, so unlike the pure builtins this one reads and advances executor state.

use crate::builtins::BuiltinResult;
use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::ProcessId;
use crate::value::Value;

/// Mint a fresh, unique ref. The argument (nil) is ignored.
pub fn builtin_reference<E: Effect>(
    _process_id: ProcessId,
    _arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    Ok(BuiltinResult::Value(executor.create_ref()))
}
