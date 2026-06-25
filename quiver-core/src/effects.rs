use crate::error::Error;
use crate::process::ProcessId;
use crate::value::{ResourceId, Value};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Debug;

/// Structured error type for effect operations
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum EffectError {
    /// Resource not found (ENOENT)
    NotFound(String),
    /// Permission denied (EACCES, EPERM)
    PermissionDenied(String),
    /// Resource already exists (EEXIST)
    AlreadyExists(String),
    /// Connection refused (ECONNREFUSED)
    ConnectionRefused(String),
    /// Operation would block (EAGAIN, EWOULDBLOCK)
    WouldBlock,
    /// Operation interrupted (EINTR)
    Interrupted,
    /// Invalid argument provided
    InvalidArgument(String),
    /// I/O error with description
    IO(String),
    /// Other error with description
    Other(String),
}

impl std::fmt::Display for EffectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EffectError::NotFound(msg) => write!(f, "Not found: {}", msg),
            EffectError::PermissionDenied(msg) => write!(f, "Permission denied: {}", msg),
            EffectError::AlreadyExists(msg) => write!(f, "Already exists: {}", msg),
            EffectError::ConnectionRefused(msg) => write!(f, "Connection refused: {}", msg),
            EffectError::WouldBlock => write!(f, "Operation would block"),
            EffectError::Interrupted => write!(f, "Operation interrupted"),
            EffectError::InvalidArgument(msg) => write!(f, "Invalid argument: {}", msg),
            EffectError::IO(msg) => write!(f, "I/O error: {}", msg),
            EffectError::Other(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for EffectError {}

/// Result of an effect execution: either (Value, heap_data) or an error
/// The Vec<Vec<u8>> contains heap-allocated binary data that needs to be transferred to the worker
pub type EffectResult = Result<(Value, Vec<Vec<u8>>), EffectError>;

/// Trait for platform-specific effects that processes can request.
///
/// Effects represent external operations (I/O, time, system calls, etc.)
/// that are executed by platform-specific backends. The core runtime is
/// generic over the effect type, allowing different platforms (native, WASM)
/// to define their own effect sets.
pub trait Effect: Send + Clone + Debug + Serialize + for<'de> Deserialize<'de> {
    /// Returns the resource ID this effect operates on, if any.
    ///
    /// Returns None for effects that create new resources (e.g., opening files,
    /// connecting to sockets). Returns Some(resource_id) for effects that operate
    /// on existing resources (e.g., reading, writing, closing).
    ///
    /// This is used by the environment to validate that the requesting process
    /// owns the resource before executing the effect.
    fn resource_id(&self) -> Option<ResourceId>;
}

/// Trait for backends that execute platform-specific effects.
///
/// Implementations provide the actual execution logic for effects,
/// interfacing with the operating system, browser APIs, or other
/// platform-specific functionality.
///
/// Effects are tracked by process ID - a process can only have one pending
/// effect at a time (enforced by the executor's `effecting` set).
pub trait EffectBackend: Send {
    /// The effect type this backend handles.
    type E: Effect;

    /// Execute an effect for a process, possibly returning an immediate completion.
    ///
    /// The `process_id` parameter identifies which process requested this effect.
    /// The backend uses this to track pending operations and return completions.
    ///
    /// Returns:
    /// - `Ok(Some(result))` - Effect completed immediately
    /// - `Ok(None)` - Effect submitted for async processing (will complete later)
    /// - `Err(error)` - Effect failed to submit
    ///
    /// Async effects will be returned later via `process_completions()`.
    fn execute(
        &mut self,
        process_id: ProcessId,
        effect: Self::E,
    ) -> Result<Option<EffectResult>, Error>;

    /// Process all completed async effects.
    ///
    /// Returns a list of (process_id, result) tuples for effects that
    /// were previously submitted and have now completed.
    fn process_completions(&mut self) -> Vec<(ProcessId, EffectResult)>;

    /// Close a resource without going through the effect system.
    ///
    /// This is used for automatic cleanup when processes terminate.
    /// The backend should close the resource and remove it from its registry.
    /// If the resource doesn't exist, this should be a no-op.
    fn close_resource(&mut self, resource_id: ResourceId);

    /// Supply the type ids the backend needs to stamp real values onto effect results.
    ///
    /// A backend has no access to the type registry, so it can't invent ids for the composite
    /// values it produces. Instead the environment pushes them here, sourced from the program:
    /// `resources` is the resource-type-name list (a name's index is its resource type id), and
    /// `results` maps a builtin name to the type ids of its composite result (see
    /// [`ResultTupleInfo`]). Called on every program load/update; ids are append-only, so the
    /// tables only ever grow.
    ///
    /// Default no-op — for backends that produce no such values (e.g. a type-checking host).
    fn set_type_ids(&mut self, _resources: &[String], _results: &[(String, ResultTupleInfo)]) {}
}

/// The tuple type ids a backend needs to stamp a builtin's composite result value.
///
/// A result may be a single top-level tuple (`tuple_id`) that nests named tag variants — e.g.
/// `directory_next`'s `[bin, (File | Dir | Symlink | Other)]`. The backend stamps `tuple_id` on
/// the outer tuple and picks the inner tag's id from `variants` by the name it's producing.
#[derive(Clone, Debug, Default)]
pub struct ResultTupleInfo {
    /// The outer (top-level, non-nil) result tuple id.
    pub tuple_id: usize,
    /// Nullary named tag variants reachable in the result type, keyed by name — e.g. `File`/`Dir`.
    /// Lets the backend stamp the specific tag it produces from a name it knows statically. Keyed
    /// by name because a nullary named tuple is nominal: its name is its complete identity (see
    /// `collect_named_variants`). Field-bearing tuples are structural and excluded.
    pub variants: HashMap<String, usize>,
}
