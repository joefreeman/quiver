use crate::binary::BinaryData;
use crate::bytecode::{ConcreteType, Constant, Function, Instruction};
use crate::effects::Effect;
use crate::error::{Error, Operation};
use crate::process::{
    Action, Frame, Process, ProcessAdjacency, ProcessCategory, ProcessId, ProcessInfo,
    ProcessStatus, RestrictedContext, SelectState, StreamEvent, Watcher,
};
use crate::types::{BuiltinInfo, NIL, TupleTypeInfo, Type};
use crate::value::{Binary, MAX_BINARY_SIZE, Payload, ResourceId, Value};
use crate::wire::{WirePayload, WireValue};
use num_traits::ToPrimitive;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;
use std::sync::Arc;
use std::time::Instant;

/// Bundled program update data for incremental compilation.
/// Contains full tuple type information for merging with Environment's Program state.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramUpdate {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    /// Full tuple type information (name, fields, is_partial)
    pub tuples: Vec<TupleTypeInfo>,
    /// Types used by IsType instructions for pattern matching
    pub types: Vec<Type>,
    /// Builtin information (name and resolved types)
    pub builtins: Vec<BuiltinInfo>,
    pub resources: Vec<String>,
    /// For each type_id, the set of concrete types compatible with it (for IsType checks).
    /// Shared rather than copied: these tables are replaced wholesale on every update and are
    /// identical in every worker, so on the native transport all workers reference one copy.
    /// The web transport serializes (separate WASM linear memories), which serde's `rc`
    /// feature handles — a web worker deserializes its own handle at refcount 1. The type is
    /// therefore uniform across platforms and needs no `cfg`; only the sharing differs.
    pub type_compatibility: Arc<Vec<HashSet<ConcreteType>>>,
    /// For each function_id, the set of concrete types compatible with its parameter
    pub function_param_compatibility: Arc<Vec<HashSet<ConcreteType>>>,
    /// For each builtin_id, the set of concrete types compatible with its parameter
    pub builtin_param_compatibility: Arc<Vec<HashSet<ConcreteType>>>,
    /// For each field-name id, each tuple_id's offset for that field (for GetNamed)
    pub field_offsets: Arc<Vec<Vec<Option<usize>>>>,
    /// Failure-provenance sites (debug builds): the full table, from which the executor
    /// prebuilds each site's annotated-nil value. `None` leaves any existing table as is.
    pub debug: Option<crate::bytecode::SiteTable>,
    /// The runtime-delivered vocabulary, resolved per merged program (see
    /// `Program::runtime_tables`): crash shapes, the demand-scoped `Changed` wakeup,
    /// and stream events. `None` leaves any existing tables as is (the compile-time
    /// sync driver never delivers any of these).
    pub runtime: Option<crate::bytecode::RuntimeTables>,
    /// For each tuple_id, a canonical *value-shape* id (same name + field labels). Lets `==`
    /// treat structurally-identical tuples built via different paths as equal.
    pub canonical_tuples: Arc<Vec<usize>>,
}

/// Instruction type for profiling statistics (groups parameterized instructions)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum InstructionType {
    Constant,
    Pop,
    Duplicate,
    Pick,
    Rotate,
    Reset,
    Load,
    Store,
    Tuple,
    GetPositional,
    GetNamed,
    IsType,
    Jump,
    JumpIf,
    Call,
    TailCall,
    Function,
    Builtin,
    Equal,
    Not,
    Annotate,
    GetAnnotation,
    Stamp,
    Spawn,
    Send,
    Self_,
    Select,
    Process,
    State,
}

impl InstructionType {
    fn from_instruction(instr: &Instruction) -> Self {
        match instr {
            Instruction::Constant(_) => InstructionType::Constant,
            Instruction::Pop => InstructionType::Pop,
            Instruction::Duplicate => InstructionType::Duplicate,
            Instruction::Pick(_) => InstructionType::Pick,
            Instruction::Rotate(_) => InstructionType::Rotate,
            Instruction::Reset(_) => InstructionType::Reset,
            Instruction::Load(_) => InstructionType::Load,
            Instruction::Store => InstructionType::Store,
            Instruction::Tuple(_) => InstructionType::Tuple,
            Instruction::GetPositional(_) => InstructionType::GetPositional,
            Instruction::GetNamed(_) => InstructionType::GetNamed,
            Instruction::IsType(_) => InstructionType::IsType,
            Instruction::Jump(_) => InstructionType::Jump,
            Instruction::JumpIf(_) => InstructionType::JumpIf,
            Instruction::Call => InstructionType::Call,
            Instruction::TailCall(_) => InstructionType::TailCall,
            Instruction::Function(_) => InstructionType::Function,
            Instruction::Builtin(..) => InstructionType::Builtin,
            Instruction::Equal(_) => InstructionType::Equal,
            Instruction::Not => InstructionType::Not,
            Instruction::Annotate(_) => InstructionType::Annotate,
            Instruction::GetAnnotation(..) => InstructionType::GetAnnotation,
            Instruction::Stamp(_) => InstructionType::Stamp,
            Instruction::Spawn => InstructionType::Spawn,
            Instruction::Send => InstructionType::Send,
            Instruction::Self_ => InstructionType::Self_,
            Instruction::Select => InstructionType::Select,
            Instruction::Process(_, _) => InstructionType::Process,
            Instruction::State => InstructionType::State,
        }
    }
}

/// Execution statistics for profiling
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ExecutionStats {
    /// Statistics by instruction type: (count, total_time_ns)
    pub instruction_stats: HashMap<InstructionType, (u64, u64)>,

    /// Per-builtin statistics by index: (count, total_time_ns)
    pub builtin_stats: HashMap<usize, (u64, u64)>,

    /// Peak stack size across all processes
    pub peak_stack_size: usize,

    /// Peak locals size across all processes
    pub peak_locals_size: usize,

    /// Peak frame count across all processes
    pub peak_frame_count: usize,
}

impl ExecutionStats {
    pub fn new() -> Self {
        Self::default()
    }

    /// Merge another ExecutionStats into this one (for aggregating from multiple executors)
    pub fn merge(&mut self, other: &ExecutionStats) {
        for (key, (count, time)) in &other.instruction_stats {
            let entry = self.instruction_stats.entry(*key).or_insert((0, 0));
            entry.0 += count;
            entry.1 += time;
        }
        for (idx, (count, time)) in &other.builtin_stats {
            let entry = self.builtin_stats.entry(*idx).or_insert((0, 0));
            entry.0 += count;
            entry.1 += time;
        }
        // Take max of peaks
        self.peak_stack_size = self.peak_stack_size.max(other.peak_stack_size);
        self.peak_locals_size = self.peak_locals_size.max(other.peak_locals_size);
        self.peak_frame_count = self.peak_frame_count.max(other.peak_frame_count);
    }

    /// Update peak memory statistics if current values exceed previous peaks
    pub fn update_peaks(&mut self, stack_size: usize, locals_size: usize, frame_count: usize) {
        self.peak_stack_size = self.peak_stack_size.max(stack_size);
        self.peak_locals_size = self.peak_locals_size.max(locals_size);
        self.peak_frame_count = self.peak_frame_count.max(frame_count);
    }

    pub fn total_instructions(&self) -> u64 {
        self.instruction_stats
            .values()
            .map(|(count, _)| count)
            .sum()
    }

    pub fn total_time_ns(&self) -> u64 {
        self.instruction_stats.values().map(|(_, time)| time).sum()
    }
}

/// Result of processing a select source
enum SelectResult {
    /// Select should complete with this value
    Complete(Value),
    /// A receive function was called, return from handle_select to let it execute
    CalledFunction,
    /// Continue to the next source
    Continue,
}

pub struct Executor<E: Effect> {
    processes: FxHashMap<ProcessId, Process>,
    process_function_indices: FxHashMap<ProcessId, usize>, // Maps process ID to its function index (for REPL)
    queue: VecDeque<ProcessId>,
    spawning: HashSet<ProcessId>,
    selecting: HashSet<ProcessId>,
    effecting: HashSet<ProcessId>,
    /// Parked on a remote `?` state read, woken by `notify_state`.
    sampling: HashSet<ProcessId>,
    /// Completion notifications awaiting delivery: `(watcher, completed pid)` pairs
    /// recorded when a watched process terminated (see `flush_watchers`). The runtime
    /// above drains these via `take_watcher_events` — delivery routes through the
    /// environment, which the executor knows nothing about.
    pending_watcher_events: Vec<(Watcher, ProcessId)>,
    /// Reactive subscribers to wake this round: a set of subscriber pids recorded when a
    /// process they watch changed state. A set, so several state
    /// changes to one subscriber's dependencies between drains coalesce into one wakeup.
    /// Drained by the runtime above via `take_state_wakeups`.
    pending_state_wakeups: HashSet<ProcessId>,
    /// Reactive subscriptions to a *remote* target dropped by reconciliation:
    /// `(target, subscriber)` pairs the runtime above routes as `UnsubscribeState` to the
    /// target's worker. Drained via `take_unsubscribes`.
    pending_unsubscribes: Vec<(ProcessId, ProcessId)>,
    // Program data owned by executor
    constants: Vec<Constant>,
    functions: Vec<Function>,
    builtins: Vec<String>, // Builtin names (for effect dispatch)
    // Resolved builtin implementations, indexed by builtin_id (parallel to `builtins`).
    // Resolved once at update_program time to avoid a String clone + HashMap lookup per call.
    builtin_impls: Vec<Option<crate::builtins::BuiltinFn<E>>>,
    // Purity classes, indexed by builtin_id (parallel to `builtins`) — the dispatch
    // site's purity gate reads these before invoking.
    builtin_purities: Vec<crate::builtins::Purity>,
    /// Whether this executor drives compile-time execution (a program's top level and
    /// module bodies), which must be deterministic: `Purity::HostRead` builtins are
    /// rejected. Set only by the sync driver; runtime workers leave it false.
    pub(crate) compile_time: bool,
    tuples: Vec<usize>, // Tuple arities
    /// The full type and tuple tables (what the program serializes), so type-consuming
    /// builtins (`__type_name__<'t>`) can read their type argument's structure at
    /// runtime — exposed to implementations through the executor's `TypeLookup`.
    types: Vec<Type>,
    tuple_infos: Vec<TupleTypeInfo>,
    resources: Vec<String>, // Resource type names
    /// For each tuple_id, a canonical value-shape id (same name + field labels) — used by `==`
    /// so structurally-identical tuples built via different paths compare equal.
    canonical_tuples: Arc<Vec<usize>>,
    /// For each type_id, the set of concrete types compatible with it (for IsType checks)
    type_compatibility: Arc<Vec<HashSet<ConcreteType>>>,
    /// For each function_id, the set of concrete types compatible with its parameter
    function_param_compatibility: Arc<Vec<HashSet<ConcreteType>>>,
    /// For each builtin_id, the set of concrete types compatible with its parameter
    builtin_param_compatibility: Arc<Vec<HashSet<ConcreteType>>>,
    /// For each field-name id, each tuple_id's offset for that field — GetNamed's table.
    field_offsets: Arc<Vec<Vec<Option<usize>>>>,
    // Cumulative count of tombstone process entries reclaimed (see `reclaim_process`).
    reclaimed_processes: usize,
    // Cache of constant binaries already materialised on the heap, keyed by constant index,
    // so a binary literal in a loop is allocated once rather than on every load.
    constant_binaries: Vec<Option<Binary>>,
    // Failure provenance (debug builds): per-site prebuilt values. `site_nils[i]` is a nil
    // already carrying site i's `origin` annotation — `Stamp` clones it (an Arc refcount
    // bump, no allocation) onto fresh bare nils. `site_origins[i]` is the bare `Site[...]`
    // tuple, attached copy-on-write when the nil already carries other annotations.
    site_nils: Vec<Value>,
    site_origins: Vec<Value>,
    origin_key: Option<usize>,
    // Crash delivery (both build modes): the ids for building `:crash`/`:timeout`
    // stamped nils (see `crash_result` / `handle_select_timeout`).
    crash_table: Option<crate::bytecode::CrashTable>,
    /// The `Changed` wakeup tuple id — present iff the program demanded it (`track`).
    changed_tuple: Option<usize>,
    /// Stream event vocabulary by resource type id (see `ProgramUpdate::runtime`).
    stream_table: Option<crate::bytecode::StreamTable>,
    // Builtin registry for executing builtin functions
    builtins_registry: crate::builtins::BuiltinRegistry<E>,
    // Profiling
    pub stats: ExecutionStats,
    profile: bool,
    // Ref generation: worker_id (upper 16 bits) combined with counter (lower 48 bits)
    worker_id: u16,
    next_ref: u64,
}

// Note: TupleLookup is not implemented for Executor anymore since tuples only stores arities
// Type compatibility is now precomputed at compile time
impl<E: Effect> Executor<E> {
    pub fn get_constant(&self, index: usize) -> Option<&Constant> {
        self.constants.get(index)
    }

    /// The `BinaryData` behind a binary value: its own bytes, or a constant's.
    ///
    /// A constant is materialised on first use and cached (see `cached_constant_binary`), so
    /// this only meets `Binary::Constant` on values the runtime never allocated for — the
    /// module-name binaries in failure-provenance sites, which the formatter reads directly.
    pub fn get_binary_data<'a>(&'a self, binary: &'a Binary) -> Result<&'a BinaryData, Error> {
        match binary {
            Binary::Data(data) => Ok(data),
            Binary::Constant(index) => Err(Error::InvalidArgument(format!(
                "constant binary {index} is not materialised"
            ))),
        }
    }

    /// Wrap `data` as a binary value. Owning the bytes is the whole allocation — there is no
    /// slot to claim and no count to initialise; the `Rc` is the accounting.
    pub fn allocate_binary_data(&mut self, data: BinaryData) -> Result<Binary, Error> {
        if data.len() > MAX_BINARY_SIZE {
            return Err(Error::InvalidArgument(format!(
                "Binary size {} exceeds maximum {}",
                data.len(),
                MAX_BINARY_SIZE
            )));
        }
        Ok(Binary::Data(Rc::new(data)))
    }

    // --- Choke points for storage. These used to keep hand-maintained reference counts in
    // step with what is reachable; a binary now owns its bytes, so dropping a value releases
    // them and these are plain stack/locals operations. They stay as choke points because the
    // REPL's compaction entry points below still address storage deliberately.

    /// Push a value onto the process stack.
    fn push_value(&mut self, proc: &mut Process, value: Value) {
        proc.stack.push(value);
    }

    /// Pop a value off the process stack.
    fn pop_value(&mut self, proc: &mut Process) -> Option<Value> {
        proc.stack.pop()
    }

    /// Push a value into the process's locals.
    fn push_local(&mut self, proc: &mut Process, value: Value) {
        proc.locals.push(value);
    }

    /// Truncate the process's locals to `len`, dropping every discarded binding.
    fn truncate_locals(&mut self, proc: &mut Process, len: usize) {
        proc.locals.truncate(len);
    }

    /// Like [`truncate_locals`] but addresses the process by id, for callers (e.g. frame
    /// teardown in the step loop) that hold only `&mut self`, not a separate `&mut Process`.
    fn truncate_locals_pid(&mut self, pid: ProcessId, len: usize) {
        if let Some(process) = self.get_process_mut(pid) {
            process.locals.truncate(len);
        }
    }

    /// Replace a process's locals wholesale. Returns `false` if the process is gone. Used by
    /// the REPL's between-evaluation compaction (the caller selects which bindings to keep).
    pub fn replace_locals(&mut self, process_id: ProcessId, new_locals: Vec<Value>) -> bool {
        match self.get_process_mut(process_id) {
            Some(process) => {
                process.locals = new_locals;
                true
            }
            None => false,
        }
    }

    /// Drop the locals at every index *not* in `keep`, overwriting each with nil. Unlike
    /// [`replace_locals`] the indices stay in place (no re-indexing), so binding indices stay
    /// valid. Used by the REPL to reclaim a finished line's orphaned parameter and temporaries
    /// at the moment its result is delivered, without disturbing the host's binding map.
    /// Returns `false` if the process is gone.
    pub fn release_orphan_locals(&mut self, process_id: ProcessId, keep: &[usize]) -> bool {
        let keep: HashSet<usize> = keep.iter().copied().collect();
        let Some(process) = self.get_process_mut(process_id) else {
            return false;
        };
        for (index, slot) in process.locals.iter_mut().enumerate() {
            if !keep.contains(&index) {
                *slot = Value::nil();
            }
        }
        true
    }

    /// Drop a completed process's execution state — stack, locals, mailbox, awaited results,
    /// and any select state — releasing the heap references they held. Only the stored
    /// `result` (and `state`) survive, so late awaits (`!p`) and samples (`?p`) keep working.
    /// The entry itself (the tombstone) stays in the process map until the environment proves
    /// it unreachable and calls `reclaim_process`.
    /// Persistent (REPL) processes are exempt — they resume across submissions and keep their
    /// locals and mailbox. Idempotent.
    pub fn tombstone(&mut self, pid: ProcessId) {
        // Every kill path sets `result` before tombstoning, so flushing here gives all
        // completions — step-finish, effect failure, error propagation — one choke
        // point for watcher notification. Idempotent (the list is taken).
        self.flush_watchers(pid);
        let taken = self.get_process_mut(pid).and_then(|process| {
            if process.persistent {
                return None;
            }
            // Reactive tracker state is execution state: a terminated process no longer
            // renders, so drop its subscriptions (they hold no heap values — raw pids —
            // so nothing to release) and any in-flight tracking. This keeps a dead
            // subscriber from pinning its dependencies' tombstones;
            // the `Subscriber` entries it left on those targets are cleaned by
            // `prune_watchers` when this process is reclaimed.
            process.subscriptions.clear();
            process.tracking = None;
            // A dead process reads no more stream events: drop the stash and the armed
            // set (an in-flight completion for a tombstone is dropped at delivery). Dropping
            // the storage releases whatever its values held.
            process.armed_resources.clear();
            process.stack = Vec::new();
            process.mailbox = Default::default();
            process.awaiting = Default::default();
            process.select_state = None;
            process.resource_events = Default::default();
            Some(())
        });
        if taken.is_none() {
            return;
        }
        self.truncate_locals_pid(pid, 0);
    }

    /// Reclaim a tombstone the environment has proven unreachable: remove the entry,
    /// releasing the heap references its surviving `result`
    /// and `state` still held. The complement of `tombstone`, which dropped everything *but*
    /// those. A no-op on an unknown pid. Must only be called on a non-persistent, completed
    /// process — the environment's mark-sweep guarantees this.
    pub fn reclaim_process(&mut self, pid: ProcessId) {
        let Some(process) = self.processes.remove(&pid) else {
            return;
        };
        debug_assert!(
            process.result.is_some() && !process.persistent,
            "reclaim of a live or persistent process {pid}",
        );
        self.reclaimed_processes += 1;
    }

    /// Cumulative count of tombstone entries reclaimed by [`Self::reclaim_process`].
    pub fn reclaimed_processes(&self) -> usize {
        self.reclaimed_processes
    }

    /// Drop watcher entries pointing at any reclaimed pid.
    /// A parent's `OwnedChild` (and any `Link`) entry outlives the child it
    /// names — it clears only when the *parent* terminates — so without this a long-lived
    /// parent that spawns per request leaks watcher entries even as tombstones are reclaimed.
    /// Harmless to over-apply: acting on a reclaimed (monotonic, never-reused) pid is a no-op.
    pub fn prune_watchers(&mut self, reclaimed: &HashSet<ProcessId>) {
        for process in self.processes.values_mut() {
            let mut removed_subscribers = 0u32;
            process.watchers.retain(|watcher| {
                let stale = reclaimed.contains(&watcher.pid());
                // A pruned reactive subscription must return the count to the fast path.
                if stale && matches!(watcher, Watcher::Subscriber { .. }) {
                    removed_subscribers += 1;
                }
                !stale
            });
            process.subscriber_count -= removed_subscribers;
        }
    }

    /// Register a watcher on `target`, to be notified when it terminates. Returns
    /// `false` when the target has already terminated (its result is set — including a
    /// sleeping persistent process's) — the caller answers the watcher immediately
    /// instead — or when the target is unknown to this executor.
    pub fn add_watcher(&mut self, target: ProcessId, watcher: Watcher) -> bool {
        match self.get_process_mut(target) {
            Some(process) if process.result.is_none() => {
                process.watchers.push(watcher);
                true
            }
            _ => false,
        }
    }

    /// Add the target-side half of a link (idempotent — one entry per peer): `peer` is
    /// killed when `target` terminates abnormally. No-op on a terminated or unknown
    /// target (the caller decides what an already-dead target means).
    pub fn add_link(&mut self, target: ProcessId, peer: ProcessId) {
        if let Some(process) = self.get_process_mut(target) {
            let entry = Watcher::Link { pid: peer };
            if process.result.is_none() && !process.watchers.contains(&entry) {
                process.watchers.push(entry);
            }
        }
    }

    /// Register `subscriber` as a reactive subscriber of `target` (a tracked `?` sample),
    /// keeping `target.subscriber_count` in step. Idempotent (one
    /// entry per subscriber). No-op on a terminated or unknown target: its state can no
    /// longer change, so a subscription would never fire.
    pub fn add_subscriber(&mut self, target: ProcessId, subscriber: ProcessId) {
        if let Some(process) = self.get_process_mut(target) {
            let entry = Watcher::Subscriber { pid: subscriber };
            if process.result.is_none() && !process.watchers.contains(&entry) {
                process.watchers.push(entry);
                process.subscriber_count += 1;
            }
        }
    }

    /// Remove `subscriber`'s reactive subscription from `target` (reconciliation dropped
    /// the dependency), decrementing `target.subscriber_count`. No-op if absent.
    pub fn remove_subscriber(&mut self, target: ProcessId, subscriber: ProcessId) {
        if let Some(process) = self.get_process_mut(target) {
            let entry = Watcher::Subscriber { pid: subscriber };
            let before = process.watchers.len();
            process.watchers.retain(|watcher| *watcher != entry);
            process.subscriber_count -= (before - process.watchers.len()) as u32;
        }
    }

    /// Move a terminated process's watchers onto the pending-events queue for the
    /// runtime above to deliver. No-op until the process has a result; idempotent
    /// thereafter (the list is taken). A persistent (REPL) process's completion is a
    /// sleep, not a termination: its result-observers (awaiters) fire, but its
    /// termination-observers (owned children) survive across the resume.
    fn flush_watchers(&mut self, pid: ProcessId) {
        let Some(process) = self.get_process_mut(pid) else {
            return;
        };
        if process.result.is_none() || process.watchers.is_empty() {
            return;
        }
        let watchers = if process.persistent {
            let (flushed, kept): (Vec<_>, Vec<_>) = std::mem::take(&mut process.watchers)
                .into_iter()
                .partition(|watcher| matches!(watcher, Watcher::Awaiter { .. }));
            process.watchers = kept;
            flushed
        } else {
            std::mem::take(&mut process.watchers)
        };
        // `Subscriber` entries are not termination notifications — a terminated process
        // will not change state again — so they never become events.
        // Drop them here; a live subscriber sheds the dead dependency on its next
        // reconciliation (or reclamation prunes it).
        self.pending_watcher_events.extend(
            watchers
                .into_iter()
                .filter(|watcher| !matches!(watcher, Watcher::Subscriber { .. }))
                .map(|watcher| (watcher, pid)),
        );
    }

    /// Terminate a process from outside — containment teardown of a terminated
    /// parent's subtree (later also `%proc.kill`): record the error as its result and
    /// tombstone it, which flushes its own watchers — awaiters observe the crash, and
    /// the teardown cascades through its owned children. No-op on terminated
    /// (idempotent), unknown, and persistent processes (a REPL process is the host's
    /// to stop).
    pub fn kill(&mut self, pid: ProcessId, error: Error) {
        match self.get_process_mut(pid) {
            Some(process) if process.result.is_none() && !process.persistent => {
                process.result = Some(Err(error));
                process.frames.clear();
            }
            _ => return,
        }
        self.tombstone(pid);
    }

    /// Drain pending completion notifications: `(watcher, completed pid)` pairs. The
    /// caller (the worker) delivers each — routing through the environment, since a
    /// watcher's pid may live on another worker.
    pub fn take_watcher_events(&mut self) -> Vec<(Watcher, ProcessId)> {
        std::mem::take(&mut self.pending_watcher_events)
    }

    /// The `Changed` wakeup value delivered to a reactive subscriber — an empty named
    /// tuple, no heap. Its tuple id comes from the installed crash/runtime table, so it
    /// matches `std/proc.qv`'s `'changed = Changed`.
    pub fn changed_value(&self) -> Result<Value, Error> {
        // Present iff the program references `track` — and wakeups require
        // subscriptions, which require `track`, so absence here is a wiring bug.
        let tuple = self.changed_tuple.ok_or_else(|| {
            Error::InvalidArgument("Changed vocabulary not installed".to_string())
        })?;
        Ok(Value::tuple(tuple, vec![]))
    }

    /// Build — extracted, ready to ship — the `:crash`-stamped nil a never-lethal await
    /// delivers for a crashed process: nil annotated under the `crash` key with
    /// `Panic[pid, message]` for a `__panic__` abort, and `Error[pid, message]` for every
    /// other runtime error. Must run on the *crashed process's* executor: the pid field
    /// carries its root function index, so it compares equal (`=&p`) to the pid values
    /// other processes hold.
    pub fn crash_result(&mut self, pid: ProcessId, error: &Error) -> Result<WireValue, Error> {
        let table = self
            .crash_table
            .clone()
            .ok_or_else(|| Error::InvalidArgument("crash table not installed".to_string()))?;
        let payload = match error {
            // A teardown/kill answers the bare `Killed` kind.
            Error::Killed => Value::tuple(table.killed_tuple, vec![]),
            _ => {
                let function_index = self
                    .process_function_indices
                    .get(&pid)
                    .copied()
                    .ok_or_else(|| {
                        Error::InvalidArgument(format!(
                            "no function index for crashed process {pid}"
                        ))
                    })?;
                let message_binary = self.allocate_binary(error.crash_message().into_bytes())?;
                let message = Value::tuple(table.str_tuple, vec![Value::Binary(message_binary)]);
                let kind_tuple = match error {
                    Error::Panic(_) => table.panic_tuple,
                    _ => table.error_tuple,
                };
                Value::tuple(
                    kind_tuple,
                    vec![Value::Process(pid, function_index), message],
                )
            }
        };
        let stamped = Value::Tuple(
            NIL,
            Payload::with_annotations(vec![], vec![(table.crash_key, payload)]).shared(),
        );
        self.to_wire(&stamped)
    }

    /// Create a binary from Vec<u8>
    pub fn allocate_binary(&mut self, bytes: Vec<u8>) -> Result<Binary, Error> {
        self.allocate_binary_data(BinaryData::new(bytes))
    }

    /// A binary's bytes as a shared handle. Flat already (the common case — anything a read
    /// produced): O(1), the leaf itself. A rope is realised into a fresh handle.
    ///
    /// Unlike the heap-table version this cannot write the flat form back, because there is no
    /// slot to write it to and the rope node may be shared. A repeatedly-read rope therefore
    /// re-realises. If that shows up, the fix is a `OnceCell<Arc<[u8]>>` memo on the composite
    /// variants of `BinaryData`, not a return to the table.
    pub fn materialize(&mut self, binary: &Binary) -> Result<Arc<[u8]>, Error> {
        Ok(self.get_binary_data(binary)?.shared_bytes())
    }

    /// This worker's contribution to the reclamation graph: one [`ProcessAdjacency`] per
    /// process, tagging it `Root` (live/persistent — never swept, seeds the mark set) or
    /// `Tombstone` (completed, non-persistent — a sweep candidate), with the pids its
    /// Value-bearing storage references. A tombstone's storage is emptied except
    /// `result`/`state`, so the same uniform walk yields its surviving edges (a late
    /// `!p` exposes `result`, `?p` exposes `state`). `Error` results carry no `Value`, so
    /// only `Ok` is walked. Mirrors the storage set of [`Self::reachable_heap_indices`].
    ///
    /// Like [`Self::reachable_heap_indices`], call only at a quiescent point (between
    /// steps): a process removed from the table mid-`step` would be missed.
    pub fn process_adjacency(&self) -> Vec<ProcessAdjacency> {
        self.processes
            .iter()
            .map(|(&pid, process)| {
                let category = if process.result.is_some() && !process.persistent {
                    ProcessCategory::Tombstone
                } else {
                    ProcessCategory::Root
                };
                let mut outgoing = Vec::new();
                for value in &process.stack {
                    collect_process_refs(value, &mut outgoing);
                }
                for value in &process.locals {
                    collect_process_refs(value, &mut outgoing);
                }
                for value in &process.mailbox {
                    collect_process_refs(value, &mut outgoing);
                }
                if let Some(Ok(value)) = &process.result {
                    collect_process_refs(value, &mut outgoing);
                }
                collect_process_refs(&process.state, &mut outgoing);
                // Reactive subscriptions are edges: a live tracker
                // pins its sampled dependencies' tombstones so `?dep` keeps yielding their
                // final state — the twin of an awaiter pinning its target via
                // `select_state.sources`. Raw pids, so added directly. Cleared at
                // tombstone, so a dead tracker pins nothing.
                outgoing.extend(process.subscriptions.iter().copied());
                if let Some(state) = &process.select_state {
                    for value in &state.sources {
                        collect_process_refs(value, &mut outgoing);
                    }
                    if let Some((_, value)) = &state.receiving {
                        collect_process_refs(value, &mut outgoing);
                    }
                }
                // Only the delivered *results* are live edges — not the keys. `awaiting` is
                // not cleared on `complete_select`, so a key lingers as a stale entry after
                // the await finishes; and an *active* await already keeps its target in
                // `select_state.sources` above, making the key redundant when live.
                for value in process.awaiting.values().flatten() {
                    collect_process_refs(value, &mut outgoing);
                }
                ProcessAdjacency {
                    pid,
                    category,
                    outgoing,
                }
            })
            .collect()
    }

    pub fn new(
        builtins_registry: crate::builtins::BuiltinRegistry<E>,
        profile: bool,
        worker_id: u16,
    ) -> Self {
        // Pre-initialize with NIL (index 0) and OK (index 1) tuple arities.
        // This matches Program::new() so incremental updates are consistent.
        Self {
            processes: FxHashMap::default(),
            process_function_indices: FxHashMap::default(),
            queue: VecDeque::new(),
            spawning: HashSet::new(),
            selecting: HashSet::new(),
            effecting: HashSet::new(),
            sampling: HashSet::new(),
            pending_watcher_events: Vec::new(),
            pending_state_wakeups: HashSet::new(),
            pending_unsubscribes: Vec::new(),
            constants: vec![],
            functions: vec![],
            builtins: vec![],
            builtin_impls: vec![],
            builtin_purities: vec![],
            compile_time: false,
            tuples: vec![0, 0], // NIL and OK have 0 fields
            // Full infos for the same two pre-seeded tuples (updates skip them), keeping
            // `tuple_infos` index-aligned with the arity table.
            tuple_infos: vec![
                TupleTypeInfo {
                    name: None,
                    fields: vec![],
                },
                TupleTypeInfo {
                    name: Some("Ok".to_string()),
                    fields: vec![],
                },
            ],
            types: vec![],
            // NIL (id 0) and OK (id 1) are each their own canonical shape; replaced on first update.
            canonical_tuples: Arc::new(vec![0, 1]),
            resources: vec![],
            type_compatibility: Arc::default(),
            function_param_compatibility: Arc::default(),
            builtin_param_compatibility: Arc::default(),
            field_offsets: Arc::default(),
            reclaimed_processes: 0,
            constant_binaries: vec![],
            site_nils: vec![],
            site_origins: vec![],
            origin_key: None,
            crash_table: None,
            changed_tuple: None,
            stream_table: None,
            builtins_registry,
            stats: ExecutionStats::new(),
            profile,
            worker_id,
            next_ref: 0,
        }
    }

    /// Create a new unique ref value
    pub(crate) fn create_ref(&mut self) -> Value {
        let ref_value = ((self.worker_id as u64) << 48) | self.next_ref;
        self.next_ref += 1;
        Value::Reference(ref_value)
    }

    /// Spawn a new process
    /// If function_index is None, creates a sleeping process ready for resume (used by REPL)
    pub fn spawn_process(
        &mut self,
        id: ProcessId,
        function_index: Option<usize>,
        captures: Vec<WireValue>,
        argument: WireValue,
        persistent: bool,
    ) -> Result<(), Error> {
        // Create the process
        let mut process = Process::new(persistent);

        // If no function, create a sleeping process directly
        let Some(function_index) = function_index else {
            process.result = Some(Ok(Value::nil()));
            self.processes.insert(id, process);
            // Not added to queue - it's sleeping, waiting for resume
            return Ok(());
        };

        self.processes.insert(id, process);

        // Rebuild the captures on this worker's heap and populate locals with them.
        let captures_count = captures.len();
        for value in captures {
            let injected = self.from_wire(value)?;
            // Injected into rooted storage (the new frame's locals).
            let process = self
                .get_process_mut(id)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            process.locals.push(injected);
        }

        // Push argument onto stack; it is also the process's initial observable state
        // (one retain per storage location).
        let injected_arg = self.from_wire(argument)?;
        let process = self
            .get_process_mut(id)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
        process.state = injected_arg.clone();
        process.stack.push(injected_arg);

        // Push initial frame with function index
        let process = self
            .get_process_mut(id)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
        process.frames.push(crate::process::Frame::new(
            function_index,
            0,
            captures_count,
        ));

        // Cache the function index for REPL references
        self.set_process_function_index(id, function_index);

        // Add to queue
        self.queue.push_back(id);

        Ok(())
    }

    fn get_current_instruction(&self, process_id: ProcessId) -> Option<Instruction> {
        self.get_process(process_id)
            .and_then(|p| p.frames.last())
            .and_then(|f| {
                self.functions[f.function_index]
                    .instructions
                    .get(f.counter)
                    .copied()
            })
    }

    pub fn get_process(&self, id: ProcessId) -> Option<&Process> {
        self.processes.get(&id)
    }

    pub fn get_process_mut(&mut self, id: ProcessId) -> Option<&mut Process> {
        self.processes.get_mut(&id)
    }

    pub fn get_builtins_registry(&self) -> &crate::builtins::BuiltinRegistry<E> {
        &self.builtins_registry
    }

    pub fn suspend_process(&mut self, id: ProcessId) {
        self.queue.retain(|&pid| pid != id);
    }

    /// Notify a process that spawned a new process with the new PID
    pub fn notify_spawn(&mut self, id: ProcessId, pid: Value) {
        let was_spawning = self.spawning.remove(&id);

        if let Some(process) = self.processes.get_mut(&id) {
            // For spawn notifications, just push the PID onto the stack and increment counter
            process.stack.push(pid);

            if let Some(frame) = process.frames.last_mut() {
                frame.counter += 1;
            }

            // Only re-queue if it was actually spawning (not already queued by something else)
            if was_spawning {
                self.queue.push_back(id);
            }
        }
    }

    /// Deliver a remote `?` state sample to the process that requested it: push the
    /// sample and wake the caller. No gate — the state type is statically known.
    pub fn notify_state(&mut self, id: ProcessId, state: WireValue) -> Result<(), Error> {
        let sample = self.from_wire(state)?;

        if let Some(process) = self.get_process_mut(id) {
            process.stack.push(sample);
            if let Some(frame) = process.frames.last_mut() {
                frame.counter += 1;
            }
            if self.sampling.remove(&id) {
                self.queue.push_back(id);
            }
        }
        Ok(())
    }

    /// Notify a process that was waiting for a result with the result value
    pub fn notify_result(
        &mut self,
        awaiter: ProcessId,
        awaited: ProcessId,
        result: WireValue,
    ) -> Result<(), Error> {
        // Rebuild the result on this worker's heap.
        let injected_result = self.from_wire(result)?;

        // Store the result in the process's awaiting map (retaining as it enters storage,
        // releasing any stale result the insert displaces). A terminated awaiter (e.g.
        // killed while parked on this very select) is skipped, like a dead message
        // target — its watcher entry on the source dangles harmlessly.
        if self
            .get_process(awaiter)
            .is_some_and(|p| p.persistent || p.result.is_none())
        {
            self.get_process_mut(awaiter)
                .unwrap()
                .awaiting
                .insert(awaited, Some(injected_result));
        }

        // Re-queue awaiter to retry its Select instruction
        if self.selecting.remove(&awaiter) {
            self.queue.push_back(awaiter);
        }

        Ok(())
    }

    /// Notify a process that an effect operation completed
    pub fn notify_effect_completion(
        &mut self,
        process_id: ProcessId,
        result: Result<WireValue, String>,
    ) -> Result<(), Error> {
        let was_effecting = self.effecting.remove(&process_id);

        // Convert result to either Ok(Value) or Err(Error)
        let value_result = match result {
            Ok(v) => Ok(self.from_wire(v)?),
            Err(err_msg) => Err(Error::InvalidArgument(format!(
                "Effect operation failed: {}",
                err_msg
            ))),
        };

        // Retain the success value as it enters the stack (below).

        // Get process and update based on result
        let process = self
            .get_process_mut(process_id)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let mut killed = false;
        match value_result {
            Ok(value) => {
                // Success: push value and increment counter
                process.stack.push(value);
                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }
            }
            Err(error) => {
                // Error: set error and terminate the process
                process.result = Some(Err(error));
                process.frames.clear();
                killed = true;
            }
        }
        if killed {
            self.tombstone(process_id);
        }

        // Re-queue if it was effecting
        if was_effecting {
            self.queue.push_back(process_id);
        }

        Ok(())
    }

    pub fn notify_message(&mut self, id: ProcessId, message: WireValue) -> Result<(), Error> {
        // A completed process can never receive again — drop the message (rather than
        // queueing it forever on the tombstone). Persistent (REPL) processes complete
        // between submissions but stay addressable, so they still queue. Never rebuilding
        // the value means its binaries are never allocated here, so there is nothing to
        // account for.
        let deliverable = self
            .get_process(id)
            .is_some_and(|p| p.persistent || p.result.is_none());

        if deliverable {
            let injected_message = self.from_wire(message)?;
            self.get_process_mut(id)
                .unwrap()
                .mailbox
                .push_back(injected_message);
        }

        // Re-queue if the process is selecting (waiting for messages)
        if self.selecting.remove(&id) {
            self.queue.push_back(id);
        }

        Ok(())
    }

    /// Deliver a stream resource's next event (the completion of an armed select
    /// read): build the event tuple, stash it on the owner — one slot per resource,
    /// since at most one read is armed — and wake its select. A tombstoned owner
    /// drops the event.
    pub fn notify_resource_event(
        &mut self,
        id: ProcessId,
        resource_id: ResourceId,
        resource_type: usize,
        event: StreamEvent,
        bytes: Vec<u8>,
    ) -> Result<(), Error> {
        let deliverable = self
            .get_process(id)
            .is_some_and(|p| p.persistent || p.result.is_none());
        if !deliverable {
            if let Some(process) = self.get_process_mut(id) {
                process.armed_resources.remove(&resource_id);
            }
            return Ok(());
        }

        let info = self
            .stream_table
            .as_ref()
            .and_then(|table| table.streams.get(resource_type))
            .and_then(|entry| entry.clone())
            .ok_or_else(|| {
                Error::InvalidArgument(format!(
                    "no stream declaration for resource type {resource_type}"
                ))
            })?;
        let source = Value::Resource(resource_id, resource_type);
        let value = match event {
            StreamEvent::Data => {
                let tuple = info.data_tuple.ok_or_else(|| {
                    Error::InvalidArgument("stream kind yields no bytes".to_string())
                })?;
                let binary = self.allocate_binary(bytes)?;
                Value::tuple(tuple, vec![source, Value::Binary(binary)])
            }
            StreamEvent::Resource {
                resource_id: produced,
            } => {
                let (tuple, produced_type) = info.resource_tuple.ok_or_else(|| {
                    Error::InvalidArgument("stream kind yields no resources".to_string())
                })?;
                Value::tuple(
                    tuple,
                    vec![source, Value::Resource(produced, produced_type)],
                )
            }
            StreamEvent::End => Value::tuple(info.end_tuple, vec![source]),
        };

        let process = self.get_process_mut(id).unwrap();
        process.armed_resources.remove(&resource_id);
        let displaced = process.resource_events.insert(resource_id, value);
        debug_assert!(
            displaced.is_none(),
            "a stream resource may have at most one event in flight"
        );

        if self.selecting.remove(&id) {
            self.queue.push_back(id);
        }
        Ok(())
    }

    pub fn mark_spawning(&mut self, id: ProcessId) {
        self.spawning.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn mark_selecting(&mut self, id: ProcessId) {
        self.selecting.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn mark_effecting(&mut self, id: ProcessId) {
        self.effecting.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn mark_sampling(&mut self, id: ProcessId) {
        self.sampling.insert(id);
        self.queue.retain(|&pid| pid != id);
    }

    pub fn mark_active(&mut self, id: ProcessId) {
        let was_spawning = self.spawning.remove(&id);
        let was_selecting = self.selecting.remove(&id);
        if was_spawning || was_selecting {
            self.queue.push_back(id);
        }
    }

    pub fn add_to_queue(&mut self, process_id: ProcessId) {
        self.queue.push_back(process_id);
    }

    fn get_status(&self, id: ProcessId, process: &Process) -> ProcessStatus {
        if self.queue.contains(&id) {
            ProcessStatus::Active
        } else if self.spawning.contains(&id)
            || self.selecting.contains(&id)
            || self.effecting.contains(&id)
            || self.sampling.contains(&id)
        {
            ProcessStatus::Waiting
        } else if matches!(&process.result, Some(Err(_))) {
            ProcessStatus::Failed
        } else if matches!(&process.result, Some(Ok(_))) {
            // Has a successful result
            if process.persistent {
                ProcessStatus::Sleeping
            } else {
                ProcessStatus::Completed
            }
        } else {
            // No result yet - must still be active
            ProcessStatus::Active
        }
    }

    pub fn get_process_statuses(&self) -> HashMap<ProcessId, ProcessStatus> {
        self.processes
            .iter()
            .map(|(id, process)| (*id, self.get_status(*id, process)))
            .collect()
    }

    /// Get process function indices for REPL process references
    /// Returns a map from process ID to the function index that process is running
    pub fn get_process_function_indices(&self) -> HashMap<ProcessId, usize> {
        self.process_function_indices
            .iter()
            .map(|(k, v)| (*k, *v))
            .collect()
    }

    /// Record the function index for a process (called on spawn and resume)
    pub fn set_process_function_index(&mut self, id: ProcessId, function_index: usize) {
        self.process_function_indices.insert(id, function_index);
    }

    /// The distinct binary buffers reachable from a set of roots.
    fn binary_set<'a>(
        &self,
        values: impl Iterator<Item = &'a Value>,
    ) -> HashMap<*const BinaryData, BinaryStat> {
        let mut set = HashMap::new();
        for value in values {
            collect_binaries(value, &mut set);
        }
        set
    }

    fn usage(
        &self,
        binaries: &HashMap<*const BinaryData, BinaryStat>,
    ) -> crate::process::HeapUsage {
        crate::process::HeapUsage {
            binaries: binaries.len(),
            bytes: binaries.values().map(|stat| stat.bytes).sum(),
        }
    }

    /// A process's per-root binary footprint (see [`crate::process::ProcessHeapUsage`]).
    fn process_heap_usage(&self, process: &Process) -> crate::process::ProcessHeapUsage {
        let stack = self.binary_set(process.stack.iter());
        let locals = self.binary_set(process.locals.iter());
        let mailbox = self.binary_set(process.mailbox.iter());

        // The total is the union across every root, deduplicated by buffer identity.
        let mut total = stack.clone();
        total.extend(locals.iter());
        total.extend(mailbox.iter());
        if let Some(Ok(value)) = &process.result {
            collect_binaries(value, &mut total);
        }
        if let Some(state) = &process.select_state {
            for value in &state.sources {
                collect_binaries(value, &mut total);
            }
            if let Some((_, value)) = &state.receiving {
                collect_binaries(value, &mut total);
            }
        }
        for value in process.awaiting.values().flatten() {
            collect_binaries(value, &mut total);
        }
        for value in process.resource_events.values() {
            collect_binaries(value, &mut total);
        }

        crate::process::ProcessHeapUsage {
            stack: self.usage(&stack),
            locals: self.usage(&locals),
            mailbox: self.usage(&mailbox),
            total: self.usage(&total),
        }
    }

    /// A snapshot of this worker's executor for the `\w` inspector (see
    /// [`crate::process::WorkerInfo`]).
    ///
    /// Reports *bytes held*, not slots: with binaries owned by the values that reference them
    /// there is no slot table to occupy, and no free list or reclamation queue to report. A
    /// buffer shared between processes — or between workers — is counted once here, by
    /// identity.
    pub fn worker_info(&self) -> crate::process::WorkerInfo {
        let mut live = HashMap::new();
        for process in self.processes.values() {
            for value in process.stack.iter().chain(process.locals.iter()) {
                collect_binaries(value, &mut live);
            }
            for value in process.mailbox.iter() {
                collect_binaries(value, &mut live);
            }
            if let Some(Ok(value)) = &process.result {
                collect_binaries(value, &mut live);
            }
        }
        let mut constants = HashMap::new();
        for binary in self.constant_binaries.iter().flatten() {
            collect_binaries(&Value::Binary(binary.clone()), &mut constants);
        }
        crate::process::WorkerInfo {
            worker_id: self.worker_id,
            process_ids: self.processes.keys().copied().collect(),
            live_binaries: live.len(),
            live_bytes: live.values().map(|stat| stat.bytes).sum(),
            rope_binaries: live.values().filter(|stat| stat.depth > 0).count(),
            max_rope_depth: live.values().map(|stat| stat.depth).max().unwrap_or(0),
            shared_bytes: live
                .values()
                .filter(|stat| stat.shared)
                .map(|stat| stat.bytes)
                .sum(),
            constant_binaries: constants.len(),
            constant_bytes: constants.values().map(|stat| stat.bytes).sum(),
        }
    }

    pub fn get_process_info(&self, id: ProcessId) -> Option<ProcessInfo> {
        self.processes.get(&id).map(|process| {
            // The reported result leaves this worker, so it takes the wire form. A value
            // that fails to convert (a bug, not a program state) is
            // reported as nil rather than failing the whole inspection.
            let result = match &process.result {
                Some(Ok(value)) => {
                    Some(Ok(self.to_wire(value).unwrap_or_else(|_| WireValue::nil())))
                }
                Some(Err(e)) => Some(Err(e.clone())),
                None => None,
            };

            // Get function_index from cached entry point (preferred for process type)
            // Falls back to frames for processes that weren't tracked (shouldn't happen normally)
            let function_index = self
                .process_function_indices
                .get(&id)
                .copied()
                .or_else(|| process.frames.first().map(|f| f.function_index));

            ProcessInfo {
                id,
                status: self.get_status(id, process),
                function_index,
                stack_size: process.stack.len(),
                locals_count: process.locals.len(),
                frames_count: process.frames.len(),
                mailbox_size: process.mailbox.len(),
                persistent: process.persistent,
                result,
                heap: self.process_heap_usage(process),
            }
        })
    }

    // Program data accessors
    pub fn get_function(&self, index: usize) -> Option<&Function> {
        self.functions.get(index)
    }

    /// Get builtin name by index
    pub fn get_builtin_name(&self, index: usize) -> Option<&str> {
        self.builtins.get(index).map(|s| s.as_str())
    }

    /// Re-queue a process for execution (e.g., after I/O completion)
    pub fn requeue_process(&mut self, process_id: ProcessId) {
        self.queue.push_back(process_id);
    }

    /// Remove a process from the execution queue
    pub fn remove_from_queue(&mut self, process_id: ProcessId) {
        self.queue.retain(|&p| p != process_id);
    }

    /// Update executor with program data (appends to existing data).
    ///
    /// Appends new constants, functions, tuples, and builtins to the existing program state.
    /// Compatibility tables are replaced (they should be recomputed for the full program).
    ///
    /// On a fresh executor (empty state), this is equivalent to initializing with complete data.
    pub fn update_program(&mut self, update: ProgramUpdate) {
        self.constants.extend(update.constants);
        self.functions.extend(update.functions);
        // Extract arities from TupleTypeInfo (the hot-path table), and keep the full
        // type/tuple info for type-consuming builtins' `TypeLookup`.
        self.tuples
            .extend(update.tuples.iter().map(|t| t.fields.len()));
        self.tuple_infos.extend(update.tuples);
        self.types.extend(update.types);
        for b in &update.builtins {
            self.builtin_impls
                .push(self.builtins_registry.get_implementation(&b.name));
            // An unknown builtin errors at call time; Pure keeps the gate out of its way.
            self.builtin_purities.push(
                self.builtins_registry
                    .get_purity(&b.name)
                    .unwrap_or(crate::builtins::Purity::Pure),
            );
            self.builtins.push(b.name.clone());
        }
        self.resources = update.resources;
        self.canonical_tuples = update.canonical_tuples;
        self.type_compatibility = update.type_compatibility;
        self.function_param_compatibility = update.function_param_compatibility;
        self.builtin_param_compatibility = update.builtin_param_compatibility;
        self.field_offsets = update.field_offsets;
        if let Some(table) = update.debug {
            self.install_sites(&table);
        }
        if let Some(tables) = update.runtime {
            self.crash_table = tables.crash;
            self.changed_tuple = tables.changed;
            self.stream_table = Some(tables.streams);
        }
    }

    /// Prebuild the per-site provenance values a debug build's `Stamp` instructions use.
    /// Everything in them references the constants table (no heap binaries), so cloning a
    /// prebuilt nil on a failure path is pure refcount traffic with no accounting.
    fn install_sites(&mut self, table: &crate::bytecode::SiteTable) {
        self.origin_key = Some(table.origin_key);
        self.site_origins = table
            .sites
            .iter()
            .map(|site| {
                let module = Value::tuple(
                    table.str_tuple,
                    vec![Value::Binary(Binary::Constant(site.module_constant))],
                );
                let kind_tuple = table.kind_tuples[site.kind.index()];
                Value::tuple(
                    table.site_tuple,
                    vec![
                        module,
                        Value::int(site.line as i64),
                        Value::int(site.column as i64),
                        Value::tuple(kind_tuple, vec![]),
                    ],
                )
            })
            .collect();
        self.site_nils = self
            .site_origins
            .iter()
            .map(|origin| {
                Value::Tuple(
                    NIL,
                    Payload::with_annotations(vec![], vec![(table.origin_key, origin.clone())])
                        .shared(),
                )
            })
            .collect();
    }

    /// Execute up to max_units instruction units for a single process.
    /// Returns (did_work, optional_action) where did_work indicates if any instructions were executed.
    pub fn step(&mut self, max_units: usize, current_time_ms: u64) -> (bool, Option<Action<E>>) {
        // Check for expired timeouts before processing
        self.check_expired_timeouts(current_time_ms);
        // Pop process from queue
        let Some(current_pid) = self.queue.pop_front() else {
            return (false, None); // No processes to run
        };

        // Take the running process out of the map for the duration of the time-slice so that
        // instruction handlers can borrow it directly (as a local) alongside `&mut self`,
        // avoiding a hash lookup on every access. It is reinserted before the bookkeeping below.
        let Some(mut proc) = self.processes.remove(&current_pid) else {
            return (false, None);
        };

        let mut units_executed = 0;
        let mut pending_request = None;

        // Execute instructions for current process
        while units_executed < max_units {
            let Some(instruction) = Self::current_instruction(&proc, &self.functions) else {
                // The current frame is exhausted. Returning into the caller ends the
                // time-slice only at the root frame (process completion, handled below);
                // an inner return pops inline so call-heavy code isn't throttled to one
                // return per slice.
                if proc.frames.len() <= 1 {
                    break; // Root frame: process finished
                }
                self.processes.insert(current_pid, proc);
                self.pop_exhausted_frame(current_pid);
                proc = self
                    .processes
                    .remove(&current_pid)
                    .expect("process should remain in map after a frame pop");
                units_executed += 1;
                continue;
            };

            let step_result = if Self::is_cold(instruction) {
                // Rare control/concurrency ops use the existing handlers, which expect the
                // process to be present in the map (they may also touch other processes).
                self.processes.insert(current_pid, proc);
                let r = self.execute_cold(current_pid, instruction, current_time_ms);
                proc = self
                    .processes
                    .remove(&current_pid)
                    .expect("process should remain in map after a cold instruction");
                r
            } else {
                self.execute_hot(&mut proc, current_pid, instruction)
            };

            units_executed += 1;

            // Handle instruction result
            match step_result {
                Ok(request) => {
                    pending_request = request;
                }
                Err(error) => {
                    proc.result = Some(Err(error.clone()));
                    proc.frames.clear();
                }
            }

            // Check if process should yield (moved to spawning/selecting/effecting, or has pending request)
            // Pending request check ensures only ONE routing request per step
            if self.spawning.contains(&current_pid)
                || self.selecting.contains(&current_pid)
                || self.effecting.contains(&current_pid)
                || self.sampling.contains(&current_pid)
                || pending_request.is_some()
            {
                break;
            }
        }

        // Return the process to the map; the bookkeeping below operates via the map as before.
        self.processes.insert(current_pid, proc);

        // Auto-pop any exhausted frames before checking if process is finished
        loop {
            // Check if current instruction exists
            if self.get_current_instruction(current_pid).is_some() {
                break; // Current frame still has instructions to execute
            }

            // Check if there's a frame to pop
            let has_frames = self
                .get_process(current_pid)
                .map(|p| !p.frames.is_empty())
                .unwrap_or(false);

            if !has_frames {
                break; // No frames to pop
            }

            self.pop_exhausted_frame(current_pid);
        }

        let process = self.get_process(current_pid);
        let finished = process.map(|p| p.frames.is_empty()).unwrap_or(false);

        if finished {
            // Store the result (unless an error was already set during execution).
            if let Some(process) = self.get_process_mut(current_pid)
                && !matches!(process.result, Some(Err(_)))
            {
                // The popped stack slot's retained count transfers into `result`.
                process.result = Some(match process.stack.pop() {
                    Some(result) => Ok(result),
                    None => Err(Error::StackUnderflow),
                });
            }

            // The process can never run again: drop its execution state, keeping the
            // result. Tombstoning also hands the completion to this process's watchers
            // (`flush_watchers` — drained by the runtime above via
            // `take_watcher_events`); that runs ahead of the persistent exemption, so
            // sleeping REPL processes notify their watchers too.
            self.tombstone(current_pid);

            // Validate the refcount invariant at this quiescent point (debug only) — the
            // worker/concurrency-path counterpart of the check in `execute_bytecode_sync`. This
            // catches *leaks* (missing releases) that the `release` underflow assert cannot.
        } else {
            let should_requeue = !self.spawning.contains(&current_pid)
                && !self.selecting.contains(&current_pid)
                && !self.sampling.contains(&current_pid);

            if should_requeue {
                // Process not finished - re-queue it so it can continue
                // This handles both: time slice exhaustion AND yielding after routing (e.g., Send)
                self.queue.push_back(current_pid);
            }
        }

        // Return (did_work=true, pending_request) - we always do work if we got here
        (true, pending_request)
    }

    /// Pop one exhausted frame: return into the caller (or a re-entered select),
    /// release the frame's locals, and reconcile a returning tracked-render thunk's
    /// reactive subscriptions. The callee's result is already on the stack.
    fn pop_exhausted_frame(&mut self, pid: ProcessId) {
        // Pop the exhausted frame and decide what to release; the local-release happens after
        // the process borrow ends, since it needs `&mut self`.
        let (clear_base, is_track_boundary) = {
            let process = self.get_process_mut(pid).expect("Process should exist");

            // A returning tracked-render thunk is the frame whose
            // pre-pop depth matches the recorded boundary; its return reconciles the
            // caller's reactive subscriptions.
            let pre_pop_len = process.frames.len();
            let is_track_boundary = process
                .tracking
                .as_ref()
                .is_some_and(|t| t.boundary_len == pre_pop_len);

            // Frame exhausted - pop it without stack manipulation
            // (the result is already on the stack from the last instruction)
            let frame = process.frames.pop().unwrap();
            let is_last_frame = process.frames.is_empty();

            // Clear locals from the popped frame (including captures). For persistent
            // processes, only keep locals if this was the last (top-level) frame.
            let should_clear_locals = !process.persistent || !is_last_frame;

            // Check if we're in an active select and returning to the select instruction
            let should_skip_increment = if let Some(ref select_state) = process.select_state {
                let current_frame = process.frames.len().saturating_sub(1);
                let current_instruction = process.frames.last().map(|f| f.counter).unwrap_or(0);
                select_state.frame == current_frame
                    && select_state.instruction == current_instruction
            } else {
                false
            };

            // Increment counter of calling frame unless we're in an active select
            if !should_skip_increment && let Some(calling_frame) = process.frames.last_mut() {
                calling_frame.counter += 1;
            }

            (
                should_clear_locals.then_some(frame.locals_base),
                is_track_boundary,
            )
        };
        if let Some(base) = clear_base {
            self.truncate_locals_pid(pid, base);
        }
        if is_track_boundary {
            self.reconcile_tracking(pid);
        }
    }

    /// Whether an instruction is a "cold" control/concurrency op handled via the process map
    /// (rather than the hot, process-as-local fast path).
    fn is_cold(instruction: Instruction) -> bool {
        matches!(
            instruction,
            Instruction::Spawn
                | Instruction::Send
                | Instruction::Self_
                | Instruction::Select
                | Instruction::Process(_, _)
                | Instruction::State
        )
    }

    /// Fetch the instruction at the current frame's counter without a process-map lookup.
    fn current_instruction(proc: &Process, functions: &[Function]) -> Option<Instruction> {
        let frame = proc.frames.last()?;
        functions[frame.function_index]
            .instructions
            .get(frame.counter)
            .copied()
    }

    /// Hot path: execute an instruction against the running process held as a local,
    /// avoiding a process-map lookup per access.
    fn execute_hot(
        &mut self,
        proc: &mut Process,
        pid: ProcessId,
        instruction: Instruction,
    ) -> Result<Option<Action<E>>, Error> {
        let start = if self.profile {
            Some(Instant::now())
        } else {
            None
        };

        // Operands widen back to `usize` here: they are `u32` in the instruction stream to
        // keep it dense (see `bytecode::Id`), but every consumer indexes a table or a stack.
        let result = match instruction {
            Instruction::Constant(index) => self.handle_constant(proc, index as usize),
            Instruction::Pop => self.handle_pop(proc),
            Instruction::Duplicate => self.handle_duplicate(proc),
            Instruction::Pick(n) => self.handle_pick(proc, n as usize),
            Instruction::Rotate(n) => self.handle_rotate(proc, n as usize),
            Instruction::Load(index) => self.handle_load(proc, index as usize),
            Instruction::Store => self.handle_store(proc),
            Instruction::Tuple(type_id) => self.handle_tuple(proc, type_id as usize),
            Instruction::GetPositional(index) => self.handle_get_positional(proc, index as usize),
            Instruction::GetNamed(name_id) => self.handle_get_named(proc, name_id as usize),
            Instruction::IsType(type_id) => self.handle_is_type(proc, type_id as usize),
            Instruction::Jump(offset) => self.handle_jump(proc, offset as isize),
            Instruction::JumpIf(offset) => self.handle_jump_if(proc, offset as isize),
            Instruction::Call => self.handle_call(proc, pid),
            Instruction::TailCall(recurse) => self.handle_tail_call(proc, recurse),
            Instruction::Function(function_index) => {
                self.handle_function(proc, function_index as usize)
            }
            Instruction::Reset(index) => self.handle_reset(proc, index as usize),
            Instruction::Builtin(index, type_argument) => {
                self.handle_builtin(proc, index as usize, type_argument.map(|id| id as usize))
            }
            Instruction::Equal(count) => self.handle_equal(proc, count as usize),
            Instruction::Not => self.handle_not(proc),
            Instruction::Annotate(key) => self.handle_annotate(proc, key as usize),
            Instruction::GetAnnotation(key, check) => {
                self.handle_get_annotation(proc, key as usize, check.map(|id| id as usize))
            }
            Instruction::Stamp(site) => self.handle_stamp(proc, site as usize),
            _ => unreachable!("cold instruction routed to execute_hot"),
        };

        if let Some(start) = start {
            let elapsed = start.elapsed().as_nanos() as u64;
            let instr_type = InstructionType::from_instruction(&instruction);
            let entry = self
                .stats
                .instruction_stats
                .entry(instr_type)
                .or_insert((0, 0));
            entry.0 += 1;
            entry.1 += elapsed;

            self.stats
                .update_peaks(proc.stack.len(), proc.locals.len(), proc.frames.len());
        }

        result
    }

    /// Cold path: control/concurrency ops that need the process in the map (and may touch
    /// other processes). The running process has been reinserted before this is called.
    fn execute_cold(
        &mut self,
        pid: ProcessId,
        instruction: Instruction,
        current_time_ms: u64,
    ) -> Result<Option<Action<E>>, Error> {
        let start = if self.profile {
            Some(Instant::now())
        } else {
            None
        };

        let result = match instruction {
            Instruction::Spawn => self.handle_spawn(pid),
            Instruction::Send => self.handle_send(pid),
            Instruction::Self_ => self.handle_self(pid),
            Instruction::Select => self.handle_select(pid, current_time_ms),
            Instruction::Process(process_id, function_index) => {
                self.handle_process_ref(pid, process_id as usize, function_index as usize)
            }
            Instruction::State => self.handle_state(pid),
            _ => unreachable!("hot instruction routed to execute_cold"),
        };

        if let Some(start) = start {
            let elapsed = start.elapsed().as_nanos() as u64;
            let instr_type = InstructionType::from_instruction(&instruction);
            let entry = self
                .stats
                .instruction_stats
                .entry(instr_type)
                .or_insert((0, 0));
            entry.0 += 1;
            entry.1 += elapsed;

            if let Some(process) = self.processes.get(&pid) {
                self.stats.update_peaks(
                    process.stack.len(),
                    process.locals.len(),
                    process.frames.len(),
                );
            }
        }

        result
    }

    /// Resolve a binary constant to owned bytes, materialising and caching them on first use so
    /// a literal in a loop allocates once rather than per iteration. The cache holds a handle
    /// like any other holder; every value carrying the constant shares that one allocation.
    fn cached_constant_binary(&mut self, index: usize) -> Result<Binary, Error> {
        if let Some(Some(binary)) = self.constant_binaries.get(index) {
            return Ok(binary.clone());
        }
        let bytes = match self.get_constant(index) {
            Some(Constant::Binary(bytes)) => bytes.clone(),
            _ => return Err(Error::ConstantUndefined(index)),
        };
        let binary = self.allocate_binary(bytes)?;
        if self.constant_binaries.len() <= index {
            self.constant_binaries.resize(index + 1, None);
        }
        self.constant_binaries[index] = Some(binary.clone());
        Ok(binary)
    }

    fn handle_constant(
        &mut self,
        proc: &mut Process,
        index: usize,
    ) -> Result<Option<Action<E>>, Error> {
        // Resolve integers directly, normalizing to the canonical small/big runtime form
        // (no clone or allocation for i64-sized constants); binaries go through the constant
        // cache. Determine which up front so the constants borrow ends before the (mutable)
        // cache call.
        let integer = match self.get_constant(index) {
            Some(Constant::Integer(integer)) => Some(match integer.to_i64() {
                Some(small) => Value::int(small),
                None => Value::integer(integer.clone()),
            }),
            Some(Constant::Binary(_)) => None,
            None => return Err(Error::ConstantUndefined(index)),
        };
        let value = match integer {
            Some(value) => value,
            None => Value::Binary(self.cached_constant_binary(index)?),
        };

        self.push_value(proc, value);

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_pop(&mut self, proc: &mut Process) -> Result<Option<Action<E>>, Error> {
        self.pop_value(proc).ok_or(Error::StackUnderflow)?;

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_duplicate(&mut self, proc: &mut Process) -> Result<Option<Action<E>>, Error> {
        let value = proc.stack.last().ok_or(Error::StackUnderflow)?.clone();
        self.push_value(proc, value);

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_pick(&mut self, proc: &mut Process, n: usize) -> Result<Option<Action<E>>, Error> {
        if proc.stack.len() <= n {
            return Err(Error::StackUnderflow);
        }
        let index = proc.stack.len() - 1 - n;
        let value = proc.stack[index].clone();
        self.push_value(proc, value);

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_rotate(&mut self, proc: &mut Process, n: usize) -> Result<Option<Action<E>>, Error> {
        let process = &mut *proc;

        let len = process.stack.len();
        if len < n {
            return Err(Error::StackUnderflow);
        }
        // Rotate the top n items: move item at depth (n-1) to the top
        // Example: [a, b, c] with n=3 becomes [b, c, a]
        let item = process.stack.remove(len - n);
        process.stack.push(item);

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_load(
        &mut self,
        proc: &mut Process,
        index: usize,
    ) -> Result<Option<Action<E>>, Error> {
        let frame = proc.frames.last().ok_or(Error::FrameUnderflow)?;
        let actual_index = frame.locals_base + index;

        let value = proc
            .locals
            .get(actual_index)
            .cloned()
            .ok_or_else(|| Error::VariableUndefined(format!("local[{}]", index)))?;

        self.push_value(proc, value);

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_store(&mut self, proc: &mut Process) -> Result<Option<Action<E>>, Error> {
        // Move the top of the stack into locals: release (leaving the stack) then retain
        // (entering locals) nets to zero, keeping the binding's reference.
        let value = self.pop_value(proc).ok_or(Error::StackUnderflow)?;
        self.push_local(proc, value);

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_tuple(
        &mut self,
        proc: &mut Process,
        type_id: usize,
    ) -> Result<Option<Action<E>>, Error> {
        // tuples now stores arities directly
        let size = *self
            .tuples
            .get(type_id)
            .ok_or_else(|| Error::TypeMismatch {
                expected: "known tuple type".to_string(),
                found: format!("unknown type ({:?})", type_id),
            })?;

        // Pop the fields (releasing each as it leaves the stack), then push the tuple, whose
        // deep retain re-counts them in their new home — a net-zero move into the tuple.
        // `with_capacity` is not decoration: `Vec`'s minimum non-zero capacity for a 24-byte
        // `Value` is 4, so growing from empty allocates 96 bytes for every tuple of arity 1-4
        // — 72 wasted on the arity-1 case, which is among the most common.
        let mut values = Vec::with_capacity(size);
        for _ in 0..size {
            let value = self.pop_value(proc).ok_or(Error::StackUnderflow)?;
            values.push(value);
        }
        values.reverse();
        self.push_value(proc, Value::tuple(type_id, values));

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_get_positional(
        &mut self,
        proc: &mut Process,
        index: usize,
    ) -> Result<Option<Action<E>>, Error> {
        // Releasing the tuple drops the counts of all its fields; pushing the extracted field
        // re-counts that one. The other fields are correctly released (no longer referenced).
        let value = self.pop_value(proc).ok_or(Error::StackUnderflow)?;

        match value {
            Value::Tuple(_, elements) => {
                let element = elements
                    .get(index)
                    .ok_or(Error::FieldAccessInvalid(index))?
                    .clone();
                self.push_value(proc, element);

                if let Some(frame) = proc.frames.last_mut() {
                    frame.counter += 1;
                }
                Ok(None)
            }
            _ => Err(Error::TypeMismatch {
                expected: "tuple".to_string(),
                found: value.type_name().to_string(),
            }),
        }
    }

    /// As `handle_get_positional`, but the field is identified by name id: the offset is resolved
    /// against the value's own tuple id via the load-time table. The compiler only emits
    /// `GetNamed` where the static type guarantees the field, so a missing entry is a
    /// compiler invariant violation, not a program error.
    fn handle_get_named(
        &mut self,
        proc: &mut Process,
        name_id: usize,
    ) -> Result<Option<Action<E>>, Error> {
        let value = self.pop_value(proc).ok_or(Error::StackUnderflow)?;

        match value {
            Value::Tuple(tuple_id, elements) => {
                let index = self
                    .field_offsets
                    .get(name_id)
                    .and_then(|offsets| offsets.get(tuple_id).copied().flatten())
                    .ok_or_else(|| {
                        Error::InvalidArgument(format!(
                            "GetNamed({name_id}) on tuple {tuple_id} with no such field (compiler invariant violation)"
                        ))
                    })?;
                let element = elements
                    .get(index)
                    .ok_or(Error::FieldAccessInvalid(index))?
                    .clone();
                self.push_value(proc, element);

                if let Some(frame) = proc.frames.last_mut() {
                    frame.counter += 1;
                }
                Ok(None)
            }
            _ => Err(Error::TypeMismatch {
                expected: "tuple".to_string(),
                found: value.type_name().to_string(),
            }),
        }
    }

    fn handle_annotate(
        &mut self,
        proc: &mut Process,
        key: usize,
    ) -> Result<Option<Action<E>>, Error> {
        // Pop-then-push nets the refcounts: releasing the carrier and annotation drops their
        // counts, and pushing the annotated copy (whose payload references both) re-counts them.
        let annotation = self.pop_value(proc).ok_or(Error::StackUnderflow)?;
        let carrier = self.pop_value(proc).ok_or(Error::StackUnderflow)?;

        // Reachable only where the compiler could not decide: a carrier typed as a bare
        // type variable, which a generic attach site defers to here.
        let annotated = carrier.annotated(key, annotation).ok_or_else(|| {
            Error::InvalidArgument(format!(
                "Annotations require a tuple or function carrier, but the annotated value is {}",
                carrier.type_name()
            ))
        })?;
        self.push_value(proc, annotated);

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_get_annotation(
        &mut self,
        proc: &mut Process,
        key: usize,
        check: Option<usize>,
    ) -> Result<Option<Action<E>>, Error> {
        let carrier = self.pop_value(proc).ok_or(Error::StackUnderflow)?;

        // Total: a value that cannot carry annotations (or doesn't carry this key) yields
        // nil, so retrieval composes with union carriers like `'int | []`. The checked
        // form additionally gates the entry on its expected shape — an incompatible entry
        // answers nil too, exactly as an ascription pattern fails to nil.
        let annotation = carrier
            .get_annotation(key)
            .filter(|value| match check {
                Some(type_id) => self.check_type_compatible(value, type_id),
                None => true,
            })
            .cloned()
            .unwrap_or_else(Value::nil);
        self.push_value(proc, annotation);

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    /// Debug builds: stamp a fresh nil result with its failure site. Fresh-only — a nil
    /// already carrying an `origin` keeps it (the propagating failure's original site),
    /// and non-nil values pass through untouched. A bare nil is *replaced* by the site's
    /// prebuilt annotated nil (refcount bump only); a nil carrying other annotations
    /// (e.g. a user `:error`) gains the origin copy-on-write.
    fn handle_stamp(
        &mut self,
        proc: &mut Process,
        site: usize,
    ) -> Result<Option<Action<E>>, Error> {
        let origin_key = self.origin_key.ok_or_else(|| {
            Error::InvalidArgument("Stamp instruction without a site table".to_string())
        })?;
        let stamped = match proc.stack.last() {
            Some(value) if value.is_nil() && value.get_annotation(origin_key).is_none() => {
                match value {
                    Value::Tuple(_, payload) if payload.annotations().is_empty() => {
                        Some(self.site_nils[site].clone())
                    }
                    other => other.annotated(origin_key, self.site_origins[site].clone()),
                }
            }
            _ => None,
        };
        if let Some(stamped) = stamped {
            // Pop-then-push nets the refcounts, exactly as in `handle_annotate`.
            let old = self.pop_value(proc).ok_or(Error::StackUnderflow)?;
            drop(old);
            self.push_value(proc, stamped);
        }

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_is_type(
        &mut self,
        proc: &mut Process,
        pattern_type_id: usize,
    ) -> Result<Option<Action<E>>, Error> {
        let value = self.pop_value(proc).ok_or(Error::StackUnderflow)?;

        // Use precomputed type compatibility instead of runtime type checking
        let is_match = self.check_type_compatible(&value, pattern_type_id);

        self.push_value(proc, if is_match { Value::ok() } else { Value::nil() });

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    /// Check if a value is compatible with a pattern type using precomputed compatibility table
    fn check_type_compatible(&self, value: &Value, pattern_type_id: usize) -> bool {
        let concrete = self.get_concrete_type(value);
        self.type_compatibility
            .get(pattern_type_id)
            .map(|set| set.contains(&concrete))
            .unwrap_or(false)
    }

    /// Get the concrete type for a runtime value using O(1) lookup
    fn get_concrete_type(&self, value: &Value) -> ConcreteType {
        match value {
            Value::Int(_) | Value::BigInt(_) => ConcreteType::Integer,
            Value::Binary(_) => ConcreteType::Binary,
            Value::Reference(_) => ConcreteType::Reference,
            Value::Tuple(tuple_id, _) => ConcreteType::Tuple(*tuple_id),
            Value::Function(func_id, _) => ConcreteType::Function(*func_id),
            Value::Builtin(builtin_id, _) => ConcreteType::Builtin(*builtin_id),
            Value::Process(_, func_id) => ConcreteType::Process(*func_id),
            Value::Resource(_, resource_type_id) => ConcreteType::Resource(*resource_type_id),
        }
    }

    /// Check if a message is compatible with a function/builtin's parameter type
    fn check_message_compatible(&self, message: &Value, source: &Value) -> bool {
        let concrete = self.get_concrete_type(message);
        match source {
            Value::Function(func_id, _) => self
                .function_param_compatibility
                .get(*func_id)
                .map(|set| set.contains(&concrete))
                .unwrap_or(true),
            Value::Builtin(builtin_id, _) => self
                .builtin_param_compatibility
                .get(*builtin_id)
                .map(|set| set.contains(&concrete))
                .unwrap_or(true),
            _ => true,
        }
    }

    fn handle_jump(
        &mut self,
        proc: &mut Process,
        offset: isize,
    ) -> Result<Option<Action<E>>, Error> {
        if let Some(frame) = proc.frames.last_mut() {
            // Jump modifies counter directly
            // Add 1 to offset because in the old code, Jump got the centralized increment
            frame.counter = frame.counter.wrapping_add_signed(offset + 1);
        }
        Ok(None)
    }

    fn handle_jump_if(
        &mut self,
        proc: &mut Process,
        offset: isize,
    ) -> Result<Option<Action<E>>, Error> {
        let condition = self.pop_value(proc).ok_or(Error::StackUnderflow)?;

        let should_jump = !condition.is_nil();

        if let Some(frame) = proc.frames.last_mut() {
            if should_jump {
                // Jump modifies counter directly
                // Add 1 to offset because in the old code, JumpIf got the centralized increment
                frame.counter = frame.counter.wrapping_add_signed(offset + 1);
            } else {
                // Not jumping, increment normally
                frame.counter += 1;
            }
        }

        Ok(None)
    }

    fn handle_call(
        &mut self,
        proc: &mut Process,
        pid: ProcessId,
    ) -> Result<Option<Action<E>>, Error> {
        let function_value = {
            let process = &mut *proc;
            process.stack.last().ok_or(Error::StackUnderflow)?.clone()
        };

        match function_value {
            Value::Function(function_index, captures) => {
                // Get function instructions before modifying process
                // Verify function exists
                self.get_function(function_index)
                    .ok_or(Error::FunctionUndefined(function_index))?;

                // Pop the function (discarded) and the parameter (re-pushed for the callee).
                // Captures are cloned into the new frame's locals.
                self.pop_value(proc); // function
                let parameter = self.pop_value(proc).ok_or(Error::StackUnderflow)?;

                let locals_base = proc.locals.len();
                let captures_count = captures.len();

                self.push_value(proc, parameter);
                for capture in captures.iter() {
                    self.push_local(proc, capture.clone());
                }

                proc.frames
                    .push(Frame::new(function_index, locals_base, captures_count));

                // Don't increment counter - new frame starts at 0
                Ok(None)
            }
            Value::Builtin(builtin_id, payload) => {
                // Pop function (discarded) and parameter (consumed by the builtin).
                self.pop_value(proc); // function
                let parameter = self.pop_value(proc).ok_or(Error::StackUnderflow)?;
                // A type-consuming builtin's explicit type argument, for the context.
                let type_argument = payload.as_deref().and_then(Payload::type_argument);

                // Resolve the implementation directly by id (no String clone / HashMap lookup).
                let builtin = self
                    .builtin_impls
                    .get(builtin_id)
                    .copied()
                    .flatten()
                    .ok_or_else(|| {
                        let name = self
                            .builtins
                            .get(builtin_id)
                            .map(String::as_str)
                            .unwrap_or("<unknown>");
                        Error::InvalidArgument(format!("Unrecognised builtin: {}", name))
                    })?;

                // Purity gate: reject a stateful or host-reading builtin, before it
                // runs, wherever re-evaluation stability or determinism is assumed.
                // (`Effect` and `Process` builtins are governed by their own gates —
                // the effect-completion check and the context verbs.)
                let purity = self
                    .builtin_purities
                    .get(builtin_id)
                    .copied()
                    .unwrap_or(crate::builtins::Purity::Pure);
                if matches!(
                    purity,
                    crate::builtins::Purity::Stateful | crate::builtins::Purity::HostRead
                ) {
                    let operation = match purity {
                        crate::builtins::Purity::HostRead => Operation::HostRead,
                        _ => Operation::CreateRef,
                    };
                    if let Some(context) = proc.restricted_context() {
                        return Err(Error::OperationNotAllowed { operation, context });
                    }
                    if self.compile_time {
                        return Err(Error::UnsupportedAtCompileTime { operation });
                    }
                }

                let start = if self.profile {
                    Some(Instant::now())
                } else {
                    None
                };

                // The context wraps the step-local `proc` (out of the map for the
                // slice) and the executor; its verbs mutate the caller's record and
                // queue at most one routed action.
                let (result, action) = {
                    let mut ctx =
                        crate::builtins::BuiltinContext::new(pid, proc, self, type_argument);
                    let result = builtin(&parameter, &mut ctx);
                    let action = ctx.take_action();
                    (result, action)
                };

                if let Some(start) = start {
                    let elapsed = start.elapsed().as_nanos() as u64;
                    let entry = self.stats.builtin_stats.entry(builtin_id).or_insert((0, 0));
                    entry.0 += 1;
                    entry.1 += elapsed;
                }

                match result? {
                    crate::builtins::Completion::Value(value) => {
                        // Immediate result: push value (retaining any freshly allocated
                        // binaries). Builtins don't create a frame, so the counter
                        // advances here; a verb-queued action (kill/link) routes on.
                        self.push_value(proc, value);
                        if let Some(frame) = proc.frames.last_mut() {
                            frame.counter += 1;
                        }
                        Ok(action)
                    }
                    crate::builtins::Completion::Effect(effect) => {
                        // A receive filter or a tracked render may be re-evaluated, so
                        // parking for an effect is rejected in either restricted context.
                        if let Some(context) = proc.restricted_context() {
                            return Err(Error::OperationNotAllowed {
                                operation: Operation::Effect,
                                context,
                            });
                        }
                        debug_assert!(
                            action.is_none(),
                            "a builtin cannot both queue an action and park for an effect"
                        );
                        // Park: the result arrives via notify_effect_completion, which
                        // pushes it and advances the counter.
                        self.mark_effecting(pid);
                        Ok(Some(Action::RequestEffect {
                            process_id: pid,
                            effect,
                        }))
                    }
                    crate::builtins::Completion::Call { function, captures } => {
                        // Resolve via a call: push the [.., parameter, function] shape
                        // handle_call expects and leave this call un-advanced — the
                        // callee frame's return delivers the result and bumps the
                        // counter, exactly like an ordinary call. The target is a
                        // genuine function, so handle_call always pushes a frame (and
                        // returns no action). For a tracked render (begin_tracking),
                        // the fresh frame is the reconciliation boundary (see the
                        // frame-pop loop).
                        self.push_value(proc, Value::nil());
                        self.push_value(proc, Value::Function(function, captures));
                        self.handle_call(proc, pid)?;
                        if let Some(tracking) = &mut proc.tracking {
                            tracking.boundary_len = proc.frames.len();
                        }
                        Ok(action)
                    }
                }
            }
            _ => Err(Error::TypeMismatch {
                expected: "function".to_string(),
                found: function_value.type_name().to_string(),
            }),
        }
    }

    /// Record a root-frame (re-)entry argument as the process's observable state (sampled
    /// by `?`). Tail calls in helper frames (depth > 1) are
    /// internal and don't touch it.
    fn record_state(&mut self, proc: &mut Process, argument: &Value) {
        if proc.frames.len() != 1 {
            return;
        }
        // Wake reactive subscribers only on an actual data-plane change. Gated on the
        // cached count so a never-watched process (the common case) pays one integer
        // branch and never runs the structural compare.
        let changed = proc.subscriber_count > 0 && !self.values_equal(&proc.state, argument);
        proc.state = argument.clone();
        if changed {
            self.enqueue_state_wakeups(proc);
        }
    }

    /// Record each of this process's reactive subscribers for a `Changed` wakeup this
    /// round. Coalesced by the set: repeated changes before the
    /// next drain yield one wakeup per subscriber.
    fn enqueue_state_wakeups(&mut self, proc: &Process) {
        for watcher in &proc.watchers {
            if let Watcher::Subscriber { pid } = watcher {
                self.pending_state_wakeups.insert(*pid);
            }
        }
    }

    /// Drain the reactive wakeups accumulated since the last call: the subscriber pids to
    /// deliver a `Changed` message to. The `Subscriber` twin of `take_watcher_events`.
    pub fn take_state_wakeups(&mut self) -> Vec<ProcessId> {
        std::mem::take(&mut self.pending_state_wakeups)
            .into_iter()
            .collect()
    }

    /// Drain the reactive `(target, subscriber)` unsubscriptions to route to remote
    /// targets' workers (see `reconcile_tracking`).
    pub fn take_unsubscribes(&mut self) -> Vec<(ProcessId, ProcessId)> {
        std::mem::take(&mut self.pending_unsubscribes)
    }

    /// Reconcile a tracked render's subscriptions against what it just sampled (called when
    /// the tracked thunk's frame returns). Dependencies still sampled
    /// stay (they were subscribed atomically at the sample); dependencies no longer sampled
    /// are unsubscribed — a local target directly, a remote one via `pending_unsubscribes`.
    fn reconcile_tracking(&mut self, pid: ProcessId) {
        let dropped: Vec<ProcessId> = {
            let Some(process) = self.get_process_mut(pid) else {
                return;
            };
            let Some(tracking) = process.tracking.take() else {
                return;
            };
            let sampled = tracking.sampled;
            let old = std::mem::replace(
                &mut process.subscriptions,
                sampled.iter().copied().collect(),
            );
            old.into_iter().filter(|p| !sampled.contains(p)).collect()
        };
        for target in dropped {
            // A target on this worker (live or tombstone) is removed directly; a remote one
            // routes an unsubscribe to its owning worker.
            if self.get_process(target).is_some() {
                self.remove_subscriber(target, pid);
            } else {
                self.pending_unsubscribes.push((target, pid));
            }
        }
    }

    fn handle_tail_call(
        &mut self,
        proc: &mut Process,
        recurse: bool,
    ) -> Result<Option<Action<E>>, Error> {
        if recurse {
            let argument = self.pop_value(proc).ok_or(Error::StackUnderflow)?;
            self.record_state(proc, &argument);
            let frame = proc.frames.last().ok_or(Error::FrameUnderflow)?;
            let locals_base = frame.locals_base;
            let captures_count = frame.captures_count;
            let function_index = frame.function_index;

            // Clear current frame's locals, but keep captures (releasing what's dropped).
            self.truncate_locals(proc, locals_base + captures_count);

            self.push_value(proc, argument);
            *proc.frames.last_mut().unwrap() =
                Frame::new(function_index, locals_base, captures_count);

            // Don't increment counter - frame was reset to 0
            Ok(None)
        } else {
            let function_value = self.pop_value(proc).ok_or(Error::StackUnderflow)?;
            let argument = self.pop_value(proc).ok_or(Error::StackUnderflow)?;

            match function_value {
                Value::Function(function_index, captures) => {
                    // Verify function exists
                    self.get_function(function_index)
                        .ok_or(Error::FunctionUndefined(function_index))?;

                    self.record_state(proc, &argument);
                    let frame = proc.frames.last().ok_or(Error::FrameUnderflow)?;
                    let locals_base = frame.locals_base;

                    // Clear current frame's locals (releasing the old captures/bindings).
                    self.truncate_locals(proc, locals_base);

                    // Extend with captures for new function
                    let captures_count = captures.len();
                    for capture in captures.iter() {
                        self.push_local(proc, capture.clone());
                    }

                    self.push_value(proc, argument);
                    *proc.frames.last_mut().unwrap() =
                        Frame::new(function_index, locals_base, captures_count);

                    // Don't increment counter - frame was reset to 0
                    Ok(None)
                }
                _ => Err(Error::CallInvalid),
            }
        }
    }

    fn handle_function(
        &mut self,
        proc: &mut Process,
        function_index: usize,
    ) -> Result<Option<Action<E>>, Error> {
        let func = self
            .get_function(function_index)
            .ok_or(Error::FunctionUndefined(function_index))?;
        let capture_count = func.captures;

        // Pop capture values off the stack (releasing each); the function value's deep retain
        // re-counts them in their new home — a net-zero move into the closure.
        let mut captures = Vec::with_capacity(capture_count);
        for _ in 0..capture_count {
            captures.push(self.pop_value(proc).ok_or(Error::StackUnderflow)?);
        }
        captures.reverse();

        let function_value = Value::function(function_index, captures);
        self.push_value(proc, function_value);

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_reset(
        &mut self,
        proc: &mut Process,
        index: usize,
    ) -> Result<Option<Action<E>>, Error> {
        let frame = proc.frames.last().ok_or(Error::FrameUnderflow)?;
        let target = frame.locals_base + index;
        if target > proc.locals.len() {
            return Err(Error::StackUnderflow);
        }
        self.truncate_locals(proc, target);
        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_builtin(
        &mut self,
        proc: &mut Process,
        index: usize,
        type_argument: Option<usize>,
    ) -> Result<Option<Action<E>>, Error> {
        // Verify builtin exists
        if index >= self.builtins.len() {
            return Err(Error::BuiltinUndefined(index));
        }
        // Push builtin by index (no heap references); a type-consuming builtin's
        // explicit type argument rides the value to its eventual call.
        self.push_value(proc, Value::builtin_typed(index, type_argument));

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_equal(
        &mut self,
        proc: &mut Process,
        count: usize,
    ) -> Result<Option<Action<E>>, Error> {
        if count > proc.stack.len() {
            return Err(Error::StackUnderflow);
        }
        let mut values = Vec::with_capacity(count);
        for _ in 0..count {
            values.push(self.pop_value(proc).ok_or(Error::StackUnderflow)?);
        }
        values.reverse();

        let first = &values[0];
        let all_equal = values.iter().all(|value| self.values_equal(first, value));

        // The result is a truth flag (the pattern compiler follows every Equal with
        // Not + a conditional jump), so success must be Ok even when the compared
        // values are themselves nil — pushing the compared value would make "equal
        // nils" indistinguishable from "not equal".
        let result = if all_equal { Value::ok() } else { Value::nil() };

        self.push_value(proc, result);

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_not(&mut self, proc: &mut Process) -> Result<Option<Action<E>>, Error> {
        let value = self.pop_value(proc).ok_or(Error::StackUnderflow)?;

        let result = if value.is_nil() {
            Value::ok()
        } else {
            Value::nil()
        };

        self.push_value(proc, result);

        if let Some(frame) = proc.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    /// Reject `operation` in a restricted context — a receive filter or a `%proc.track`
    /// render. Both may be re-evaluated, so they must stay pure:
    /// spawns, sends, effects, and selects are rejected. (`?` sampling is allowed in a
    /// tracked render — it is a read, and is what tracking is for.)
    fn check_not_restricted(&self, pid: ProcessId, operation: Operation) -> Result<(), Error> {
        match self.get_process(pid).and_then(Process::restricted_context) {
            Some(context) => Err(Error::OperationNotAllowed { operation, context }),
            None => Ok(()),
        }
    }

    fn handle_spawn(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
        self.check_not_restricted(pid, Operation::Spawn)?;

        let (function_value, argument) = {
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            let function_value = process.stack.pop().ok_or(Error::StackUnderflow)?;
            let argument = process.stack.pop().ok_or(Error::StackUnderflow)?;
            (function_value, argument)
        };
        // Both leave this process's stack — carried by the Spawn action and re-injected into the
        // new process by `spawn_process`. Release here so the caller's counts drop.

        let (function_index, captures) = match function_value {
            Value::Function(idx, caps) => (idx, caps),
            _ => {
                return Err(Error::TypeMismatch {
                    expected: "function".to_string(),
                    found: function_value.type_name().to_string(),
                });
            }
        };

        // Mark caller as spawning - will be notified with Value::Pid(new_pid)
        self.mark_spawning(pid);

        // Return routing request for scheduler to handle
        // Don't increment counter - will be incremented in notify_spawn
        Ok(Some(Action::Spawn {
            caller: pid,
            function_index,
            captures: captures.to_vec(),
            argument,
        }))
    }

    fn handle_send(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
        self.check_not_restricted(pid, Operation::Send)?;

        let (target_value, message) = {
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            let target_value = process.stack.pop().ok_or(Error::StackUnderflow)?;
            let message = process.stack.pop().ok_or(Error::StackUnderflow)?;
            (target_value, message)
        };
        // The message leaves this process's stack (carried by the Deliver action, or dropped on a
        // type error); release it. `target_value` is a process/resource handle (no heap
        // references) — it is pushed back or dropped, needing no accounting.

        match target_value {
            Value::Process(target_pid, _) => {
                // Push process back onto stack
                let process = self
                    .get_process_mut(pid)
                    .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                process.stack.push(target_value);

                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }

                // Return routing request for scheduler to handle
                Ok(Some(Action::Deliver {
                    target: target_pid,
                    value: message,
                }))
            }
            Value::Resource(_resource_id, _) => {
                // Resources are opaque handles — writes go through their builtins,
                // not sends.
                Err(Error::TypeMismatch {
                    expected: "process".to_string(),
                    found: "resource".to_string(),
                })
            }
            _ => Err(Error::TypeMismatch {
                expected: "process".to_string(),
                found: target_value.type_name().to_string(),
            }),
        }
    }

    fn handle_self(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let function_index = process
            .frames
            .first()
            .ok_or(Error::FrameUnderflow)?
            .function_index;
        process.stack.push(Value::Process(pid, function_index));

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    fn handle_process_ref(
        &mut self,
        pid: ProcessId,
        process_id: usize,
        function_index: usize,
    ) -> Result<Option<Action<E>>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        process
            .stack
            .push(Value::Process(process_id, function_index));

        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }
        Ok(None)
    }

    /// Sample a process's current state (`?` — a snapshot, never a wait). A local target
    /// answers synchronously; a remote one parks the caller and routes like an await.
    fn handle_state(&mut self, pid: ProcessId) -> Result<Option<Action<E>>, Error> {
        let target_value = {
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            process.stack.pop().ok_or(Error::StackUnderflow)?
        };

        let Value::Process(target, _) = target_value else {
            return Err(Error::TypeMismatch {
                expected: "process".to_string(),
                found: target_value.type_name().to_string(),
            });
        };

        // A sample inside a `%proc.track` render records the dependency and subscribes the
        // caller. Recording the intent now — before the local/remote
        // split — is what lets reconciliation see it whichever path serves the read.
        let tracking = self.get_process(pid).is_some_and(Process::is_tracking);
        if tracking && let Some(t) = self.get_process_mut(pid).and_then(|p| p.tracking.as_mut()) {
            t.sampled.insert(target);
        }

        if let Some(target_process) = self.get_process(target) {
            // Local: snapshot the state cell.
            let sample = target_process.state.clone();
            // Subscribe atomically with the read (idempotent; no-op on a terminated target).
            if tracking {
                self.add_subscriber(target, pid);
            }
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            process.stack.push(sample);
            if let Some(frame) = process.frames.last_mut() {
                frame.counter += 1;
            }
            return Ok(None);
        }

        // Remote: park and route through the environment. Ownership of any resource
        // handles in the state does NOT transfer (a sample is a read, not a message). When
        // tracking, the target's worker registers the subscription as it serves the read.
        self.mark_sampling(pid);
        Ok(Some(Action::ReadState {
            caller: pid,
            target,
            subscribe: tracking,
        }))
    }

    /// Check if we're continuing from a receive function call and pop result if needed
    fn handle_select_continuation(&mut self, pid: ProcessId) -> Result<Option<Value>, Error> {
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let Some(ref select_state) = process.select_state else {
            return Ok(None); // Not a continuation
        };

        // Verify we're handling the same select instruction
        let current_frame = process.frames.len().saturating_sub(1);
        let current_instruction = process.frames.last().map(|f| f.counter).unwrap_or(0);

        if select_state.frame != current_frame || select_state.instruction != current_instruction {
            // A select at a different position while select state exists is a select (or
            // await) inside a receive function — a restricted context (see `is_receiving`).
            return Err(Error::OperationNotAllowed {
                operation: Operation::Select,
                context: RestrictedContext::ReceiveFunction,
            });
        }

        // If we just finished executing a receive function, pop the verdict. It is only inspected
        // for truthiness (then dropped), so release it as it leaves the stack.
        if select_state.receiving.is_some() {
            let verdict = {
                let process = self
                    .get_process_mut(pid)
                    .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                process.stack.pop().ok_or(Error::StackUnderflow)?
            };
            Ok(Some(verdict))
        } else {
            Ok(None)
        }
    }

    /// Initialize select state on first execution
    fn initialize_select(
        &mut self,
        pid: ProcessId,
        current_time_ms: u64,
    ) -> Result<Option<Action<E>>, Error> {
        // A select inside a tracked render is rejected — a render must stay pure and
        // non-blocking. A nested select in a receive filter is
        // caught separately by the continuation guard.
        if self.get_process(pid).is_some_and(Process::is_tracking) {
            return Err(Error::OperationNotAllowed {
                operation: Operation::Select,
                context: RestrictedContext::TrackedRender,
            });
        }
        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Pop a single value from stack (either a tuple of sources or a single source)
        let value = process.stack.pop().ok_or(Error::StackUnderflow)?;

        // Extract sources: if it's a tuple, use its elements; otherwise use the value itself
        let sources: Vec<Value> = match value {
            Value::Tuple(_, elements) => elements.to_vec(),
            single => vec![single],
        };

        // Count receive sources to initialize cursors
        let receive_count = sources
            .iter()
            .filter(|s| matches!(s, Value::Function(_, _) | Value::Builtin(..)))
            .count();

        // Scan for process sources to determine if we need to await
        let pid_targets: Vec<ProcessId> = sources
            .iter()
            .filter_map(|s| {
                if let Value::Process(p, _) = *s {
                    Some(p)
                } else {
                    None
                }
            })
            .collect();

        // Stream-resource sources: arm each one's next-event read, unless an event is
        // already stashed (consumed by the coming scan — the next select re-arms) or a
        // read is already armed (an earlier select's event is still in flight).
        let mut arm: Vec<ResourceId> = Vec::new();
        for source in &sources {
            if let Value::Resource(rid, _) = source
                && !process.resource_events.contains_key(rid)
                && process.armed_resources.insert(*rid)
            {
                arm.push(*rid);
            }
        }

        let current_frame = process.frames.len().saturating_sub(1);
        let current_instruction = process.frames.last().map(|f| f.counter).unwrap_or(0);

        // If we have PIDs, defer start_time until await completes
        let start_time = if pid_targets.is_empty() {
            Some(current_time_ms)
        } else {
            None
        };

        process.select_state = Some(Box::new(SelectState {
            frame: current_frame,
            instruction: current_instruction,
            sources,
            cursors: vec![0; receive_count],
            start_time,
            receiving: None,
        }));

        // If we found PIDs or resources to arm, register/route before processing
        // sources. Re-awaiting a target displaces the previously stored result —
        // release it (it was retained when it entered the map). The scan happens on
        // the guaranteed wake (the await answer, or the worker's wake after routing
        // the arms).
        if !pid_targets.is_empty() || !arm.is_empty() {
            let mut displaced = Vec::new();
            for target in &pid_targets {
                if let Some(Some(old)) = process.awaiting.insert(*target, None) {
                    displaced.push(old);
                }
            }

            self.mark_selecting(pid);
            return Ok(Some(Action::Await {
                targets: pid_targets,
                caller: pid,
                arm,
            }));
        }

        Ok(None)
    }

    /// Ensure select start time is set (lazily after awaits complete)
    fn ensure_select_start_time(
        &mut self,
        pid: ProcessId,
        current_time_ms: u64,
    ) -> Result<u64, Error> {
        let process = self
            .get_process(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        let select_state = process
            .select_state
            .as_ref()
            .ok_or(Error::InvalidArgument("Select state missing".to_string()))?;

        if let Some(t) = select_state.start_time {
            Ok(t)
        } else {
            // Set it now - awaits are complete, time to start evaluating
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            if let Some(ref mut state) = process.select_state {
                state.start_time = Some(current_time_ms);
            }
            Ok(current_time_ms)
        }
    }

    /// Process select sources in order, completing when a source is ready
    fn process_select_sources(
        &mut self,
        pid: ProcessId,
        receive_result: Option<Value>,
        start_time: u64,
        current_time_ms: u64,
    ) -> Result<Option<Action<E>>, Error> {
        let select_state = self
            .get_process(pid)
            .and_then(|p| p.select_state.clone())
            .ok_or(Error::InvalidArgument("Select state missing".to_string()))?;

        for (src_idx, source) in select_state.sources.iter().enumerate() {
            match source {
                Value::Int(_) | Value::BigInt(_) => {
                    // A timeout beyond i64 range (a canonical big) is effectively unbounded.
                    let timeout_ms = match source {
                        Value::Int(ms) => *ms,
                        _ => i64::MAX,
                    };
                    if let Some(value) =
                        self.handle_select_timeout(timeout_ms, start_time, current_time_ms)?
                    {
                        return self.complete_select(pid, value);
                    }
                }
                Value::Process(target_pid, _) => {
                    if let Some(value) = self.handle_select_process(pid, *target_pid)? {
                        return self.complete_select(pid, value);
                    }
                }
                Value::Function(_, _) | Value::Builtin(..) => {
                    // Receive sources may complete, call a function, or continue to next source
                    match self.handle_select_receive(
                        pid,
                        src_idx,
                        source,
                        &select_state,
                        receive_result.as_ref(),
                    )? {
                        SelectResult::Complete(value) => {
                            return self.complete_select(pid, value);
                        }
                        SelectResult::CalledFunction => {
                            // Receive function was called, return Ok(None) to let it execute
                            return Ok(None);
                        }
                        SelectResult::Continue => {
                            // No match, continue to next source
                        }
                    }
                }
                Value::Resource(resource_id, _) => {
                    // A stream resource: complete with its stashed next event if one
                    // has arrived (the armed read was routed at initialization);
                    // otherwise keep waiting — the event's arrival wakes this select.
                    let stashed = self
                        .get_process_mut(pid)
                        .and_then(|p| p.resource_events.remove(resource_id));
                    if let Some(event) = stashed {
                        let completed = self.complete_select(pid, event.clone());
                        // The stash held one retain; complete_select retained again.
                        return completed;
                    }
                }
                _ => {
                    return Err(Error::InvalidArgument(format!(
                        "Invalid select source: {:?}",
                        source
                    )));
                }
            }
        }

        // No sources ready - mark as selecting
        self.mark_selecting(pid);
        Ok(None)
    }

    /// Check if timeout has elapsed, returning a `:timeout`-stamped nil if so — the
    /// stamp carries the ms that fired, so a fallback branch can discriminate a timeout
    /// from a crash or a legit nil result. Bare nil when no
    /// crash table is installed (the compile-time sync driver).
    fn handle_select_timeout(
        &mut self,
        timeout_ms: i64,
        start_time: u64,
        current_time_ms: u64,
    ) -> Result<Option<Value>, Error> {
        let elapsed = current_time_ms.saturating_sub(start_time);
        if elapsed >= timeout_ms.max(0) as u64 {
            let value = match &self.crash_table {
                Some(table) => Value::nil()
                    .annotated(table.timeout_key, Value::int(timeout_ms.max(0)))
                    .expect("nil carries annotations"),
                None => Value::nil(),
            };
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    /// Check if an awaited process has completed, returning its result if so
    fn handle_select_process(
        &mut self,
        pid: ProcessId,
        target_pid: ProcessId,
    ) -> Result<Option<Value>, Error> {
        let process = self
            .get_process(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Check if result is available (we've already awaited upfront)
        if let Some(result_opt) = process.awaiting.get(&target_pid)
            && let Some(result) = result_opt
        {
            return Ok(Some(result.clone()));
        }

        Ok(None)
    }

    /// Handle a receive source, checking for completed receive or scanning mailbox
    fn handle_select_receive(
        &mut self,
        pid: ProcessId,
        src_idx: usize,
        source: &Value,
        select_state: &SelectState,
        receive_result: Option<&Value>,
    ) -> Result<SelectResult, Error> {
        // Calculate receive function index (count of receive sources before this one)
        let receive_idx = select_state.sources[..src_idx]
            .iter()
            .filter(|s| matches!(s, Value::Function(_, _) | Value::Builtin(..)))
            .count();

        // Check if we just finished executing this receive function
        if let Some((idx, message_value)) = &select_state.receiving
            && *idx == receive_idx
            && let Some(value) =
                self.handle_receive_result(pid, receive_idx, message_value, receive_result)?
        {
            return Ok(SelectResult::Complete(value));
        }

        // Nil result - cursor was incremented, continue scanning mailbox
        // Or we haven't checked this receive source yet
        self.scan_mailbox_for_message(pid, receive_idx, source, select_state)
    }

    /// Handle the result from a just-executed receive function
    fn handle_receive_result(
        &mut self,
        pid: ProcessId,
        receive_idx: usize,
        message_value: &Value,
        receive_result: Option<&Value>,
    ) -> Result<Option<Value>, Error> {
        // Use the result we popped earlier (in handle_select_continuation)
        let result = receive_result.ok_or(Error::InvalidArgument(
            "Receive result should be present when receiving is set".to_string(),
        ))?;

        // A filter accepts the message on any non-nil result and skips it on nil — matching
        // Quiver's truthiness convention everywhere else (nil is the only "no"). The filter's
        // result is only a verdict; the received message itself is what the select yields.
        if !result.is_nil() {
            // Accept - remove message from mailbox and complete
            let process = self
                .get_process(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            let select_state = process
                .select_state
                .as_ref()
                .ok_or(Error::InvalidArgument("Select state missing".to_string()))?;
            let msg_idx = select_state.cursors.get(receive_idx).copied().unwrap_or(0);

            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            if msg_idx < process.mailbox.len() {
                process.mailbox.remove(msg_idx); // the accepted message leaves the mailbox
            }
            Ok(Some(message_value.clone()))
        } else {
            // Nil result - increment cursor and reset receiving (releasing the held message).
            let _dropped = {
                let process = self
                    .get_process_mut(pid)
                    .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                match &mut process.select_state {
                    Some(state) => {
                        state.cursors[receive_idx] += 1;
                        state.receiving.take().map(|(_, message)| message)
                    }
                    None => None,
                }
            };
            Ok(None)
        }
    }

    /// Scan mailbox for a type-compatible message, calling receive function if found
    fn scan_mailbox_for_message(
        &mut self,
        pid: ProcessId,
        receive_idx: usize,
        source: &Value,
        select_state: &SelectState,
    ) -> Result<SelectResult, Error> {
        let mut cursor = self
            .get_process(pid)
            .and_then(|p| p.select_state.as_ref())
            .and_then(|s| s.cursors.get(receive_idx).copied())
            .unwrap_or(0);

        let process = self
            .get_process(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Loop through all messages starting from cursor
        for (msg_idx, message) in process.mailbox.iter().enumerate().skip(cursor) {
            // Check if type is compatible using precomputed parameter compatibility
            let type_compatible = self.check_message_compatible(message, source);

            if type_compatible {
                // Found a compatible message
                let message = message.clone();

                // A body-less receiver only specifies the message type; it is not a filter.
                // That covers an identity function (no instructions) and any builtin (which
                // has no Quiver body). Only a function *with* a body runs as a filter, so a
                // builtin behaves exactly like the equivalent body-less function rather than
                // being applied to the message.
                let is_type_only = match source {
                    Value::Function(func_id, _) => self
                        .functions
                        .get(*func_id)
                        .map(|f| f.instructions.is_empty())
                        .unwrap_or(false),
                    Value::Builtin(..) => true,
                    _ => unreachable!(),
                };

                if is_type_only {
                    // Type-only receiver - skip calling, just complete with the message
                    let _removed = {
                        let process = self
                            .get_process_mut(pid)
                            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                        if msg_idx < process.mailbox.len() {
                            process.mailbox.remove(msg_idx)
                        } else {
                            None
                        }
                    };
                    return Ok(SelectResult::Complete(message));
                } else {
                    // Function has a body - set receiving state and call it
                    self.call_receive_function(pid, receive_idx, msg_idx, message, source)?;
                    return Ok(SelectResult::CalledFunction);
                }
            } else {
                // Type not compatible - update cursor to skip this message
                cursor = msg_idx + 1;
            }
        }

        // Update cursor to reflect all skipped messages
        if cursor > select_state.cursors.get(receive_idx).copied().unwrap_or(0) {
            let process = self
                .get_process_mut(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            if let Some(state) = &mut process.select_state
                && receive_idx < state.cursors.len()
            {
                state.cursors[receive_idx] = cursor;
            }
        }

        Ok(SelectResult::Continue)
    }

    /// Call a receive function and prepare for re-entry
    fn call_receive_function(
        &mut self,
        pid: ProcessId,
        receive_idx: usize,
        msg_idx: usize,
        message: Value,
        source: &Value,
    ) -> Result<(), Error> {
        // Take the process out so it can be passed to handle_call as a local (handle_call now
        // borrows the process directly rather than looking it up in the map).
        let mut proc = self
            .processes
            .remove(&pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // The message clone enters the select_state.receiving slot.
        if let Some(state) = &mut proc.select_state {
            state.receiving = Some((receive_idx, message.clone()));
            state.cursors[receive_idx] = msg_idx;
        }

        // The message (parameter) and source (the receive function) enter the call's stack frame.
        self.push_value(&mut proc, message);
        self.push_value(&mut proc, source.clone());

        // Call the function - when it returns, handle_select will be called again
        let result = self.handle_call(&mut proc, pid);
        self.processes.insert(pid, proc);
        result?;
        Ok(())
    }

    fn handle_select(
        &mut self,
        pid: ProcessId,
        current_time_ms: u64,
    ) -> Result<Option<Action<E>>, Error> {
        // Phase 1: Check if we're continuing from a receive function call
        let receive_result = self.handle_select_continuation(pid)?;

        // Phase 2: Initialize if this is the first time
        if receive_result.is_none() {
            let has_select_state = self
                .get_process(pid)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?
                .select_state
                .is_some();

            if !has_select_state {
                return self.initialize_select(pid, current_time_ms);
            }
        }

        // Phase 3: Ensure start time is set (lazily after awaits complete)
        let start_time = self.ensure_select_start_time(pid, current_time_ms)?;

        // Phase 4: Process sources by type
        self.process_select_sources(pid, receive_result, start_time, current_time_ms)
    }

    /// Complete a select by cleaning up state and pushing result
    fn complete_select(
        &mut self,
        pid: ProcessId,
        result: Value,
    ) -> Result<Option<Action<E>>, Error> {
        // Tear down the select state — dropping it releases the source list and any in-flight
        // received message — then push the result.
        self.get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?
            .select_state
            .take();

        let process = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
        process.stack.push(result);

        // Increment frame counter
        if let Some(frame) = process.frames.last_mut() {
            frame.counter += 1;
        }

        Ok(None)
    }

    /// Whether any process is queued to run immediately. Event-driven runtimes use this to
    /// decide whether to keep stepping (true) or go idle and wait for a wake (false).
    pub fn has_runnable(&self) -> bool {
        !self.queue.is_empty()
    }

    /// Earliest absolute clock time (ms, same scale as the `current_time_ms` passed to `step`)
    /// at which a pending select timeout will expire, or `None` if no selecting process has a
    /// timeout. Event-driven runtimes schedule a single timer for this instant instead of
    /// polling the clock. Mirrors the expiry rule in `check_expired_timeouts`.
    pub fn next_timeout_ms(&self) -> Option<u64> {
        self.selecting
            .iter()
            .filter_map(|pid| {
                let process = self.get_process(*pid)?;
                let select_state = process.select_state.as_ref()?;
                let start_time = select_state.start_time?;
                let timeout = select_state
                    .sources
                    .iter()
                    .filter_map(|source| match source {
                        Value::Int(ms) => Some((*ms).max(0) as u64),
                        Value::BigInt(_) => Some(i64::MAX as u64),
                        _ => None,
                    })
                    .min()?;
                Some(start_time.saturating_add(timeout))
            })
            .min()
    }

    fn check_expired_timeouts(&mut self, current_time_ms: u64) {
        // Scan selecting processes for expired select timeouts
        let expired: Vec<ProcessId> = self
            .selecting
            .iter()
            .filter(|pid| {
                if let Some(process) = self.get_process(**pid)
                    && let Some(ref select_state) = process.select_state
                    && let Some(start_time) = select_state.start_time
                {
                    // Check if any timeout sources have expired
                    let elapsed = current_time_ms.saturating_sub(start_time);
                    return select_state.sources.iter().any(|source| match source {
                        Value::Int(ms) => elapsed >= (*ms).max(0) as u64,
                        Value::BigInt(_) => elapsed >= i64::MAX as u64,
                        _ => false,
                    });
                }
                false
            })
            .copied()
            .collect();

        // Re-queue expired processes to retry their Select instruction
        for pid in expired {
            self.queue.push_back(pid);
            self.selecting.remove(&pid);
        }
    }

    /// Canonical value-shape id for a tuple-id (falls back to the id itself if unmapped).
    fn canonical_tuple(&self, tuple_id: usize) -> usize {
        self.canonical_tuples
            .get(tuple_id)
            .copied()
            .unwrap_or(tuple_id)
    }

    /// Structural equality, as the `Equal` instruction sees it.
    ///
    /// **Iterative**: a pair work-list rather than recursion, because a value nests once per
    /// list element and comparing two long lists (`xs ~> =&ys`) would otherwise abort. Order of
    /// comparison is unobservable — the answer is a conjunction — so a LIFO list is fine.
    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        // `Vec::new`, and the first pair handled without it: a `Vec` does not allocate until
        // something is pushed, so comparing two scalars — much the commonest case, and on the
        // `Equal` instruction's hot path — costs nothing beyond the comparison.
        let mut pending: Vec<(&Value, &Value)> = Vec::new();
        let mut next = Some((a, b));
        while let Some((a, b)) = next.take().or_else(|| pending.pop()) {
            let equal = match (a, b) {
                (Value::Int(a), Value::Int(b)) => a == b,
                // Mixed small/big pairs are unequal by the canonical-form invariant.
                (Value::BigInt(a), Value::BigInt(b)) => a == b,
                (Value::Binary(a), Value::Binary(b)) => {
                    // Compare contents, whichever side owns its bytes. `to_vec` realises a
                    // rope, which a byte-wise walk would have to do anyway.
                    let bytes = |binary: &Binary| -> Option<Vec<u8>> {
                        match binary {
                            Binary::Data(data) => Some(data.to_vec()),
                            Binary::Constant(index) => match self.get_constant(*index) {
                                Some(Constant::Binary(bytes)) => Some(bytes.clone()),
                                _ => None,
                            },
                        }
                    };
                    match (bytes(a), bytes(b)) {
                        (Some(a), Some(b)) => a == b,
                        _ => false,
                    }
                }
                (Value::Tuple(type_a, elements_a), Value::Tuple(type_b, elements_b)) => {
                    // Compare by canonical value-shape (name + field labels), not raw tuple-id:
                    // the same shape built via paths that inferred different field types gets
                    // distinct ids but is the same value. Elements are then compared
                    // structurally, by queueing them.
                    if self.canonical_tuple(*type_a) != self.canonical_tuple(*type_b)
                        || elements_a.len() != elements_b.len()
                    {
                        return false;
                    }
                    pending.extend(elements_a.iter().zip(elements_b.iter()));
                    continue;
                }
                (Value::Function(idx_a, caps_a), Value::Function(idx_b, caps_b)) => {
                    if idx_a != idx_b || caps_a.len() != caps_b.len() {
                        return false;
                    }
                    pending.extend(caps_a.iter().zip(caps_b.iter()));
                    continue;
                }
                // The type argument is operational (a differently-instantiated builtin behaves
                // differently), so it participates; annotations stay invisible.
                (Value::Builtin(a, p), Value::Builtin(b, q)) => {
                    a == b
                        && p.as_deref().and_then(Payload::type_argument)
                            == q.as_deref().and_then(Payload::type_argument)
                }
                (Value::Process(a, func_a), Value::Process(b, func_b)) => {
                    a == b && func_a == func_b
                }
                (Value::Reference(a), Value::Reference(b)) => a == b,
                _ => false,
            };
            if !equal {
                return false;
            }
        }
        true
    }
}

/// Recursively collect all pids referenced by a value (the reclamation-graph twin of
/// [`collect_heap_indices`]). Walks tuple/function/builtin payloads via `all_values()`,
/// which covers annotations too — so a pid inside a `:crash` payload is followed. Pushes
/// duplicates; the caller dedups.
fn collect_process_refs(value: &Value, pids: &mut Vec<ProcessId>) {
    let mut pending: Vec<&Value> = Vec::new();
    let mut next = Some(value);
    while let Some(value) = next.take().or_else(|| pending.pop()) {
        match value {
            Value::Process(pid, _) => pids.push(*pid),
            Value::Tuple(_, elements) | Value::Function(_, elements) => {
                pending.extend(elements.all_values())
            }
            Value::Builtin(_, Some(payload)) => pending.extend(payload.all_values()),
            _ => {}
        }
    }
}

/// What the inspector records about one distinct binary buffer.
#[derive(Clone, Copy)]
struct BinaryStat {
    bytes: usize,
    /// Rope depth; 0 for a flat leaf. Non-zero means the bytes are not contiguous, so every
    /// read realises them — see [`Executor::materialize`], which (unlike the heap-table
    /// version it replaced) cannot write the flat form back for other holders to reuse. A
    /// climbing depth here is the signal that a memo on the rope would be worth building.
    depth: usize,
    /// Whether the bytes are shared with another holder — see
    /// [`BinaryData::bytes_shared`](crate::binary::BinaryData::bytes_shared). Only knowable
    /// now that a value owns its bytes; the slot table counted references to a *slot*.
    shared: bool,
}

/// Every distinct binary buffer a value references, keyed by identity so a buffer shared
/// between roots is counted once.
fn collect_binaries(value: &Value, out: &mut HashMap<*const BinaryData, BinaryStat>) {
    let mut pending: Vec<&Value> = Vec::new();
    let mut next = Some(value);
    while let Some(value) = next.take().or_else(|| pending.pop()) {
        match value {
            Value::Binary(Binary::Data(data)) => {
                out.insert(
                    Rc::as_ptr(data),
                    BinaryStat {
                        bytes: data.len(),
                        depth: data.depth(),
                        shared: data.bytes_shared(),
                    },
                );
            }
            Value::Tuple(_, elements) | Value::Function(_, elements) => {
                pending.extend(elements.all_values())
            }
            Value::Builtin(_, Some(payload)) => pending.extend(payload.all_values()),
            _ => {}
        }
    }
}

// The executor carries the program's full type/tuple tables (shipped in every build),
// so a type-consuming builtin can resolve its type argument's structure at runtime —
// `format_type_by_id`, and eventually type-directed decoding, read through this.
impl<E: Effect> crate::types::TypeLookup for Executor<E> {
    fn lookup_type(&self, type_id: usize) -> Option<&Type> {
        self.types.get(type_id)
    }

    fn lookup_tuple(&self, tuple_id: usize) -> Option<&TupleTypeInfo> {
        self.tuple_infos.get(tuple_id)
    }
}

/// Which composite a conversion frame is rebuilding when its children are done.
#[derive(Clone, Copy)]
enum Node {
    Tuple(usize),
    Function(usize),
    Builtin(usize),
}

/// One level of an in-progress [`Executor::to_wire`]: the composite being rebuilt, the payload
/// its children come from, and the children converted so far (elements first, then annotation
/// values, matching [`Payload::all_values`]).
struct ToWireFrame<'a> {
    node: Node,
    payload: &'a Payload,
    done: Vec<WireValue>,
}

impl<'a> ToWireFrame<'a> {
    /// The `index`-th child of a payload in `all_values` order, or `None` past the end.
    fn child(payload: &'a Payload, index: usize) -> Option<&'a Value> {
        payload.get(index).or_else(|| {
            payload
                .annotations()
                .get(index - payload.len())
                .map(|(_, v)| v)
        })
    }

    fn assemble(self) -> WireValue {
        let ToWireFrame {
            node,
            payload,
            mut done,
        } = self;
        let annotation_values = done.split_off(payload.len());
        let annotations = payload
            .annotations()
            .iter()
            .map(|(key, _)| *key)
            .zip(annotation_values)
            .collect();
        let wire = WirePayload::with_annotations(done, annotations)
            .with_type_argument(payload.type_argument());
        match node {
            Node::Tuple(id) => WireValue::Tuple(id, wire),
            Node::Function(id) => WireValue::Function(id, wire),
            Node::Builtin(id) => WireValue::Builtin(id, Some(wire)),
        }
    }
}

/// One level of an in-progress [`Executor::from_wire`]. The wire form is consumed, so unlike
/// [`ToWireFrame`] this owns its remaining children rather than borrowing them.
struct FromWireFrame {
    node: Node,
    elements: usize,
    keys: Vec<usize>,
    type_argument: Option<usize>,
    remaining: std::iter::Chain<std::vec::IntoIter<WireValue>, std::vec::IntoIter<WireValue>>,
    done: Vec<Value>,
}

thread_local! {
    /// Frame stack for [`Executor::from_wire`], reused across calls. A fresh `Vec` per message
    /// is not free: a `FromWireFrame` is fat, and `Vec`'s minimum capacity is 4, so the first
    /// push of a *shallow* message allocated ~576 bytes — measurable on `zzmem`'s `binary_send`.
    /// The same lesson as `Payload`'s drop work-list. `to_wire`'s frames borrow their payloads,
    /// so only this side can be a thread-local.
    static FROM_WIRE_STACK: std::cell::RefCell<Vec<FromWireFrame>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Capacity retained between conversions; a pathologically deep message should not leave its
/// stack reserved for the rest of the thread's life.
const FROM_WIRE_RETAIN: usize = 64;

impl FromWireFrame {
    fn new(node: Node, mut payload: WirePayload) -> Self {
        let (elements, annotations, type_argument) = payload.take_parts();
        let (keys, annotation_values): (Vec<usize>, Vec<WireValue>) =
            annotations.into_iter().unzip();
        let count = elements.len();
        FromWireFrame {
            node,
            elements: count,
            keys,
            type_argument,
            remaining: elements.into_iter().chain(annotation_values),
            done: Vec::with_capacity(count),
        }
    }

    fn assemble(self) -> Value {
        let FromWireFrame {
            node,
            elements,
            keys,
            type_argument,
            mut done,
            ..
        } = self;
        let annotations = keys.into_iter().zip(done.split_off(elements)).collect();
        let payload = Payload::with_annotations(done, annotations)
            .with_type_argument(type_argument)
            .shared();
        match node {
            Node::Tuple(id) => Value::Tuple(id, payload),
            Node::Function(id) => Value::Function(id, payload),
            Node::Builtin(id) => Value::Builtin(id, Some(payload)),
        }
    }
}

impl<E: Effect> Executor<E> {
    /// This value in its self-contained [`WireValue`] form: binaries carried as a shared handle,
    /// so the result depends on no executor state and can cross a process boundary.
    ///
    /// **Iterative.** A message is arbitrarily deep — a cons list is a value nested once per
    /// element — and a recursive conversion aborted the process on a 400k-element list even
    /// against the worker's 256 MiB stack. An abort is uncatchable, so depth has to be bounded
    /// by the heap rather than merely given a lot of stack. Same shape as the receiving side and
    /// as `Payload`'s drop: an explicit frame stack, one level per composite.
    pub fn to_wire(&self, value: &Value) -> Result<WireValue, Error> {
        let mut stack: Vec<ToWireFrame<'_>> = Vec::new();
        let mut value = value;

        'descend: loop {
            // Convert leaves outright; descend into composites, pushing a frame each time.
            let mut converted = loop {
                let (node, payload) = match value {
                    Value::Tuple(id, payload) => (Node::Tuple(*id), payload),
                    Value::Function(id, payload) => (Node::Function(*id), payload),
                    Value::Builtin(id, Some(payload)) => (Node::Builtin(*id), payload),
                    leaf => break self.leaf_to_wire(leaf)?,
                };
                stack.push(ToWireFrame {
                    node,
                    payload,
                    done: Vec::with_capacity(payload.len()),
                });
                match ToWireFrame::child(payload, 0) {
                    Some(child) => value = child,
                    // A field-less composite has nothing to descend into.
                    None => break stack.pop().expect("just pushed").assemble(),
                }
            };

            // Hand the finished child to its parent, then either move on to the parent's next
            // child or assemble the parent and keep unwinding.
            loop {
                let Some(frame) = stack.last_mut() else {
                    return Ok(converted);
                };
                frame.done.push(converted);
                match ToWireFrame::child(frame.payload, frame.done.len()) {
                    Some(child) => {
                        value = child;
                        continue 'descend;
                    }
                    None => converted = stack.pop().expect("just borrowed").assemble(),
                }
            }
        }
    }

    /// The wire form of a value that owns no payload — everything `to_wire`'s frame stack does
    /// not descend into.
    fn leaf_to_wire(&self, value: &Value) -> Result<WireValue, Error> {
        Ok(match value {
            Value::Int(n) => WireValue::Int(*n),
            Value::BigInt(n) => WireValue::BigInt((**n).clone()),
            Value::Binary(Binary::Constant(index)) => WireValue::Constant(*index),
            Value::Binary(binary) => {
                WireValue::Binary(self.get_binary_data(binary)?.shared_bytes())
            }
            Value::Reference(id) => WireValue::Reference(*id),
            Value::Builtin(id, None) => WireValue::Builtin(*id, None),
            Value::Process(pid, function_index) => WireValue::Process(*pid, *function_index),
            Value::Resource(id, type_id) => WireValue::Resource(*id, *type_id),
            Value::Tuple(..) | Value::Function(..) | Value::Builtin(_, Some(_)) => {
                unreachable!("composites are handled by the frame stack")
            }
        })
    }

    /// The inverse of [`to_wire`](Self::to_wire): rebuild a value in *this* executor.
    ///
    /// Takes the wire value **by value**: the receiver owns it and drops it immediately after,
    /// so a binary's handle is adopted rather than its bytes copied. Iterative for the same
    /// reason as `to_wire`.
    pub fn from_wire(&mut self, wire: WireValue) -> Result<Value, Error> {
        // Take the shared stack, use it, put it back. Taking rather than holding a borrow
        // across the conversion keeps `wire` moveable (cloning it to satisfy two branches would
        // undo the whole point of taking it by value), and leaves an empty `Vec` behind so any
        // unexpected re-entry gets its own. `try_with` because a value can be rebuilt during
        // thread teardown, when the thread-local may already be destroyed.
        let mut stack = FROM_WIRE_STACK
            .try_with(|cell| {
                cell.try_borrow_mut()
                    .map(|mut stack| std::mem::take(&mut *stack))
                    .unwrap_or_default()
            })
            .unwrap_or_default();

        let result = self.rebuild_from_wire(&mut stack, wire);

        stack.clear();
        if stack.capacity() > FROM_WIRE_RETAIN {
            stack.shrink_to(FROM_WIRE_RETAIN);
        }
        let _ = FROM_WIRE_STACK.try_with(|cell| {
            if let Ok(mut slot) = cell.try_borrow_mut() {
                *slot = stack;
            }
        });
        result
    }

    fn rebuild_from_wire(
        &mut self,
        stack: &mut Vec<FromWireFrame>,
        wire: WireValue,
    ) -> Result<Value, Error> {
        let mut wire = wire;

        'descend: loop {
            let mut converted = loop {
                let (node, payload) = match wire {
                    WireValue::Tuple(id, payload) => (Node::Tuple(id), payload),
                    WireValue::Function(id, payload) => (Node::Function(id), payload),
                    WireValue::Builtin(id, Some(payload)) => (Node::Builtin(id), payload),
                    leaf => break self.leaf_from_wire(leaf)?,
                };
                let mut frame = FromWireFrame::new(node, payload);
                match frame.remaining.next() {
                    Some(child) => {
                        stack.push(frame);
                        wire = child;
                    }
                    None => break frame.assemble(),
                }
            };

            loop {
                let Some(frame) = stack.last_mut() else {
                    return Ok(converted);
                };
                frame.done.push(converted);
                match frame.remaining.next() {
                    Some(child) => {
                        wire = child;
                        continue 'descend;
                    }
                    None => converted = stack.pop().expect("just borrowed").assemble(),
                }
            }
        }
    }

    /// The value form of a wire value that owns no payload.
    fn leaf_from_wire(&mut self, wire: WireValue) -> Result<Value, Error> {
        Ok(match wire {
            WireValue::Int(n) => Value::Int(n),
            WireValue::BigInt(n) => Value::integer(n),
            // The handle is adopted, not copied: the receiver points at the sender's allocation.
            WireValue::Binary(bytes) => {
                Value::Binary(self.allocate_binary_data(BinaryData::Owned(bytes))?)
            }
            WireValue::Constant(index) => Value::Binary(Binary::Constant(index)),
            WireValue::Reference(id) => Value::Reference(id),
            WireValue::Builtin(id, None) => Value::Builtin(id, None),
            WireValue::Process(pid, function_index) => Value::Process(pid, function_index),
            WireValue::Resource(id, type_id) => Value::Resource(id, type_id),
            WireValue::Tuple(..) | WireValue::Function(..) | WireValue::Builtin(_, Some(_)) => {
                unreachable!("composites are handled by the frame stack")
            }
        })
    }
}

#[cfg(test)]
mod process_adjacency_tests {
    use super::*;
    use crate::builtins::BuiltinRegistry;
    use crate::process::{ProcessCategory, SelectState};
    use crate::value::ResourceId;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    struct TestEffect;
    impl Effect for TestEffect {
        fn resource_id(&self) -> Option<ResourceId> {
            None
        }
    }

    fn executor() -> Executor<TestEffect> {
        Executor::new(BuiltinRegistry::new(), false, 0)
    }

    fn pid(id: ProcessId) -> Value {
        Value::Process(id, 0)
    }

    fn adjacency_of(ex: &Executor<TestEffect>, id: ProcessId) -> ProcessAdjacency {
        ex.process_adjacency()
            .into_iter()
            .find(|a| a.pid == id)
            .expect("process present in adjacency")
    }

    fn outgoing_set(a: &ProcessAdjacency) -> HashSet<ProcessId> {
        a.outgoing.iter().copied().collect()
    }

    #[test]
    fn collect_process_refs_finds_nested_pids() {
        // A pid buried two tuples deep, alongside a non-pid, must still be found.
        let inner = Value::tuple(0, vec![pid(7), Value::nil()]);
        let value = Value::tuple(0, vec![Value::nil(), inner, pid(9)]);
        let mut pids = Vec::new();
        collect_process_refs(&value, &mut pids);
        assert_eq!(
            pids.iter().copied().collect::<HashSet<_>>(),
            HashSet::from([7, 9])
        );
    }

    #[test]
    fn live_process_is_root_and_sweeps_every_edge_kind() {
        let mut ex = executor();
        let mut p = Process::new(false);
        p.stack.push(pid(10));
        p.locals.push(pid(11));
        p.mailbox.push_back(pid(12));
        p.state = pid(13);
        p.select_state = Some(Box::new(SelectState {
            frame: 0,
            instruction: 0,
            sources: vec![pid(14)],
            cursors: vec![],
            start_time: None,
            receiving: Some((0, pid(15))),
        }));
        // A delivered await *result* is an edge; the key (16, the awaited pid) is not — it
        // can linger stale, and a live await is covered by select_state.sources above.
        p.awaiting.insert(16, Some(pid(17)));
        ex.processes.insert(0, p);

        let a = adjacency_of(&ex, 0);
        assert_eq!(a.category, ProcessCategory::Root);
        assert_eq!(
            outgoing_set(&a),
            HashSet::from([10, 11, 12, 13, 14, 15, 17]),
        );
    }

    #[test]
    fn completed_process_is_tombstone_with_result_and_state_edges() {
        let mut ex = executor();
        let mut p = Process::new(false);
        // A tombstone keeps only result + state; those pids remain reachable via !p / ?p.
        p.result = Some(Ok(pid(20)));
        p.state = pid(21);
        ex.processes.insert(0, p);

        let a = adjacency_of(&ex, 0);
        assert_eq!(a.category, ProcessCategory::Tombstone);
        assert_eq!(outgoing_set(&a), HashSet::from([20, 21]));
    }

    #[test]
    fn persistent_completed_process_stays_root() {
        let mut ex = executor();
        let mut p = Process::new(true); // persistent (REPL): never a sweep candidate
        p.result = Some(Ok(Value::nil()));
        ex.processes.insert(0, p);

        assert_eq!(adjacency_of(&ex, 0).category, ProcessCategory::Root);
    }

    #[test]
    fn error_result_contributes_no_edges() {
        let mut ex = executor();
        let mut p = Process::new(false);
        p.result = Some(Err(Error::Killed)); // Error carries no Value
        ex.processes.insert(0, p);

        let a = adjacency_of(&ex, 0);
        assert_eq!(a.category, ProcessCategory::Tombstone);
        assert!(a.outgoing.is_empty());
    }
}

#[cfg(test)]
mod annotation_tests {
    use super::*;
    use crate::builtins::BuiltinRegistry;
    use crate::value::ResourceId;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    struct TestEffect;
    impl Effect for TestEffect {
        fn resource_id(&self) -> Option<ResourceId> {
            None
        }
    }

    fn executor() -> Executor<TestEffect> {
        Executor::new(BuiltinRegistry::new(), false, 0)
    }

    #[test]
    fn annotate_and_retrieve_round_trip() {
        let tuple = Value::tuple(3, vec![Value::int(1)]);
        let annotated = tuple.annotated(0, Value::int(42)).unwrap();
        assert_eq!(annotated.get_annotation(0), Some(&Value::int(42)));
        assert_eq!(annotated.get_annotation(1), None);
        // Replacing an existing key keeps one entry.
        let replaced = annotated.annotated(0, Value::int(7)).unwrap();
        assert_eq!(replaced.get_annotation(0), Some(&Value::int(7)));
        // The original is untouched (copy-on-annotate).
        assert_eq!(tuple.get_annotation(0), None);
        // Non-composites cannot carry annotations.
        assert_eq!(Value::int(5).annotated(0, Value::int(1)), None);
    }

    #[test]
    fn annotations_invisible_to_equality_nil_and_matching() {
        let ex = executor();
        let plain = Value::nil();
        let annotated = plain.annotated(0, Value::int(1)).unwrap();
        assert!(annotated.is_nil(), "annotated nil must still be nil");
        assert_eq!(plain, annotated, "derived equality ignores annotations");
        assert!(
            ex.values_equal(&plain, &annotated),
            "VM equality ignores annotations"
        );
        assert_eq!(
            ex.get_concrete_type(&plain),
            ex.get_concrete_type(&annotated),
            "type checks see the same concrete type"
        );
    }

    #[test]
    fn annotated_builtin_walks_and_stays_equal() {
        let mut ex = executor();
        let b = ex.allocate_binary(vec![4, 5]).unwrap();

        let bare = Value::builtin(7);
        let annotated = bare.annotated(0, Value::Binary(b)).unwrap();
        assert_eq!(bare, annotated, "derived equality ignores annotations");
        assert!(
            ex.values_equal(&bare, &annotated),
            "VM equality ignores annotations"
        );
        assert_eq!(
            ex.get_concrete_type(&bare),
            ex.get_concrete_type(&annotated),
            "type checks see the same concrete type"
        );
    }

    /// Every shape a message can carry survives `to_wire` -> `from_wire` between two
    /// executors: nesting, annotations, a builtin's type argument, constants (which cross as
    /// references, not bytes), and the identity-bearing variants.
    #[test]
    fn wire_round_trip_preserves_every_shape() {
        let mut source = executor();
        let heap = source.allocate_binary(vec![1, 2, 3]).unwrap();
        let inner = Value::tuple(3, vec![Value::Binary(heap), Value::int(-9)])
            .annotated(
                2,
                Value::Binary(source.allocate_binary(vec![4, 5]).unwrap()),
            )
            .unwrap();
        let value = Value::tuple(
            3,
            vec![
                inner,
                Value::Binary(Binary::Constant(0)),
                Value::builtin_typed(7, Some(11)),
                Value::function(1, vec![Value::int(42)]),
                Value::Process(5, 6),
                Value::Resource(8, 9),
                Value::Reference(0xdead_beef),
                Value::nil(),
            ],
        );

        let wire = source.to_wire(&value).unwrap();
        // The bytes a send moves are exactly the heap binaries; the constant is a reference.
        assert_eq!(wire.byte_size(), 5, "3 + 2 bytes of heap binaries");
        assert!(matches!(
            wire,
            WireValue::Tuple(3, ref p) if matches!(p.elements[1], WireValue::Constant(0))
        ));

        let mut target = executor();
        let received = target.from_wire(wire).unwrap();

        // Structural equality ignores annotations and heap placement, so compare the parts
        // that must survive explicitly as well.
        assert_eq!(received, value, "structure and identities preserved");
        let Value::Tuple(_, fields) = &received else {
            panic!("expected a tuple")
        };
        let Value::Tuple(_, inner_fields) = &fields[0] else {
            panic!("expected a nested tuple")
        };
        let Value::Binary(binary) = &inner_fields[0] else {
            panic!("expected a heap binary")
        };
        assert_eq!(
            target.get_binary_data(binary).unwrap().to_vec(),
            vec![1, 2, 3]
        );
        let Some(Value::Binary(annotation)) = fields[0].get_annotation(2) else {
            panic!("annotation lost in transit")
        };
        assert_eq!(
            target.get_binary_data(annotation).unwrap().to_vec(),
            vec![4, 5]
        );
        assert_eq!(
            fields[2].type_argument(),
            Some(11),
            "type argument survives"
        );
        assert!(matches!(fields[1], Value::Binary(Binary::Constant(0))));
    }
}

#[cfg(test)]
mod reactive_notification_tests {
    // Slice 1 of the reactive design: the `Subscriber` watcher + `record_state` change
    // detection + coalesced wakeup drain, plus the count maintenance the fast-path gate
    // depends on. Deterministic (no timing), unlike the end-to-end reactive tests.
    use super::*;
    use crate::builtins::BuiltinRegistry;
    use crate::value::ResourceId;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    struct TestEffect;
    impl Effect for TestEffect {
        fn resource_id(&self) -> Option<ResourceId> {
            None
        }
    }

    fn executor() -> Executor<TestEffect> {
        Executor::new(BuiltinRegistry::new(), false, 0)
    }

    /// A target process at a root frame (so `record_state` fires) with the given state.
    fn target_with_state(state: Value) -> Process {
        let mut p = Process::new(false);
        p.frames.push(Frame::new(0, 0, 0));
        p.state = state;
        p
    }

    #[test]
    fn subscribe_maintains_count_and_is_idempotent() {
        let mut ex = executor();
        ex.processes.insert(0, target_with_state(Value::int(1)));
        ex.add_subscriber(0, 1);
        ex.add_subscriber(0, 1); // idempotent
        ex.add_subscriber(0, 2);
        assert_eq!(ex.get_process(0).unwrap().subscriber_count, 2);
        ex.remove_subscriber(0, 1);
        assert_eq!(ex.get_process(0).unwrap().subscriber_count, 1);
        assert_eq!(ex.get_process(0).unwrap().watchers.len(), 1);
    }

    #[test]
    fn wakes_only_on_actual_state_change() {
        let mut ex = executor();
        ex.processes.insert(0, target_with_state(Value::int(5)));
        ex.add_subscriber(0, 1);

        // Re-entering the same state wakes no one.
        let mut target = ex.processes.remove(&0).unwrap();
        ex.record_state(&mut target, &Value::int(5));
        ex.processes.insert(0, target);
        assert!(ex.take_state_wakeups().is_empty());

        // A real change wakes the subscriber exactly once.
        let mut target = ex.processes.remove(&0).unwrap();
        ex.record_state(&mut target, &Value::int(10));
        ex.processes.insert(0, target);
        assert_eq!(ex.take_state_wakeups(), vec![1]);
    }

    #[test]
    fn no_wakeup_without_subscribers() {
        // The fast-path gate: a change on an unwatched process enqueues nothing.
        let mut ex = executor();
        ex.processes.insert(0, target_with_state(Value::int(5)));
        let mut target = ex.processes.remove(&0).unwrap();
        ex.record_state(&mut target, &Value::int(10));
        ex.processes.insert(0, target);
        assert!(ex.take_state_wakeups().is_empty());
    }

    #[test]
    fn changes_coalesce_per_drain() {
        let mut ex = executor();
        ex.processes.insert(0, target_with_state(Value::int(0)));
        ex.add_subscriber(0, 1);
        for v in [1, 2, 3] {
            let mut target = ex.processes.remove(&0).unwrap();
            ex.record_state(&mut target, &Value::int(v));
            ex.processes.insert(0, target);
        }
        // Three changes between drains coalesce into one wakeup.
        assert_eq!(ex.take_state_wakeups(), vec![1]);
    }

    #[test]
    fn tombstone_drops_subscriber_without_emitting_an_event() {
        // A `Subscriber` is not a termination watcher: it is dropped by `flush_watchers`,
        // never turned into a completion event.
        let mut ex = executor();
        let mut target = target_with_state(Value::int(5));
        target.result = Some(Ok(Value::int(5)));
        ex.processes.insert(0, target);
        ex.add_subscriber(0, 1);
        ex.tombstone(0);
        assert!(ex.take_watcher_events().is_empty());
    }

    #[test]
    fn reconcile_drops_unsampled_local_dependency() {
        use crate::process::TrackingState;
        let mut ex = executor();
        // Dependency A (pid 1) the tracker (pid 0) is currently subscribed to.
        ex.processes.insert(1, target_with_state(Value::int(0)));
        ex.add_subscriber(1, 0);
        // Tracker mid-render, having sampled nothing this time.
        let mut tracker = Process::new(false);
        tracker.frames.push(Frame::new(0, 0, 0));
        tracker.subscriptions = vec![1];
        tracker.tracking = Some(Box::new(TrackingState {
            sampled: HashSet::new(),
            boundary_len: 1,
        }));
        ex.processes.insert(0, tracker);

        ex.reconcile_tracking(0);

        // The dropped dependency is unsubscribed (count back to the fast path) and the
        // tracking state is cleared.
        assert!(ex.get_process(0).unwrap().subscriptions.is_empty());
        assert_eq!(ex.get_process(1).unwrap().subscriber_count, 0);
        assert!(ex.get_process(0).unwrap().tracking.is_none());
    }

    #[test]
    fn prune_watchers_restores_count_for_reclaimed_subscriber() {
        let mut ex = executor();
        ex.processes.insert(0, target_with_state(Value::int(5)));
        ex.add_subscriber(0, 1);
        ex.add_subscriber(0, 2);
        assert_eq!(ex.get_process(0).unwrap().subscriber_count, 2);
        // Subscriber 1 was reclaimed: its entry is pruned and the count returns to the
        // fast path for the survivor.
        ex.prune_watchers(&HashSet::from([1]));
        assert_eq!(ex.get_process(0).unwrap().subscriber_count, 1);
        assert_eq!(ex.get_process(0).unwrap().watchers.len(), 1);
    }
}
