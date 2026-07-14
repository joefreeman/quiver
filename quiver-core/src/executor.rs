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
    /// For each type_id, the set of concrete types compatible with it (for IsType checks)
    pub type_compatibility: Vec<HashSet<ConcreteType>>,
    /// For each function_id, the set of concrete types compatible with its parameter
    pub function_param_compatibility: Vec<HashSet<ConcreteType>>,
    /// For each builtin_id, the set of concrete types compatible with its parameter
    pub builtin_param_compatibility: Vec<HashSet<ConcreteType>>,
    /// For each field-name id, each tuple_id's offset for that field (for GetNamed)
    pub field_offsets: Vec<Vec<Option<usize>>>,
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
    pub canonical_tuples: Vec<usize>,
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
            Instruction::Builtin(_) => InstructionType::Builtin,
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

/// A snapshot of binary-heap occupancy, for leak detection and REPL/test introspection.
///
/// `slots` counts every heap slot allocated so far (the heap is currently append-only, so this
/// includes dead slots); `reachable` counts those still referenced from a live root — any
/// process's stack/locals/mailbox/result/select state, plus the constant-binary cache. The
/// gap, [`HeapStats::dead`], is garbage: the binaries a future reclamation pass will collect,
/// and the quantity a manual refcount must drive to zero.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HeapStats {
    /// Total heap slots allocated (live + dead).
    pub slots: usize,
    /// Slots reachable from a live root.
    pub reachable: usize,
    /// Total bytes across all slots.
    pub total_bytes: usize,
    /// Bytes across reachable slots only.
    pub reachable_bytes: usize,
}

impl HeapStats {
    /// Slots allocated but no longer reachable — the leak a reclamation pass would collect.
    pub fn dead(&self) -> usize {
        self.slots - self.reachable
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
    tuples: Vec<usize>,     // Tuple arities
    resources: Vec<String>, // Resource type names
    /// For each tuple_id, a canonical value-shape id (same name + field labels) — used by `==`
    /// so structurally-identical tuples built via different paths compare equal.
    canonical_tuples: Vec<usize>,
    /// For each type_id, the set of concrete types compatible with it (for IsType checks)
    type_compatibility: Vec<HashSet<ConcreteType>>,
    /// For each function_id, the set of concrete types compatible with its parameter
    function_param_compatibility: Vec<HashSet<ConcreteType>>,
    /// For each builtin_id, the set of concrete types compatible with its parameter
    builtin_param_compatibility: Vec<HashSet<ConcreteType>>,
    /// For each field-name id, each tuple_id's offset for that field — GetNamed's table.
    field_offsets: Vec<Vec<Option<usize>>>,
    // Heap for runtime-allocated binaries (using BinaryData for O(1) operations)
    heap: Vec<BinaryData>,
    // Reference count per heap slot, parallel to `heap`. A freshly allocated slot starts at 0
    // ("floating" — held only in a transient Rust local); it gains a count as it enters rooted
    // storage (stack/locals/tuple/etc.) via `retain` and loses it via `release`. The invariant
    // (validated by `check_refcounts`) is `refcounts[i] > 0  <=>  slot i is reachable`.
    refcounts: Vec<u32>,
    // Reclamation (a slot reaching count 0 is reusable). `pending_free` queues slots whose count
    // hit 0; `process_pending_free` (at the start of `step`, a quiescent point) actually frees
    // those still at 0 — deferred so that moves (release-then-retain) and in-flight Actions
    // carrying a value out of `step` don't reclaim a slot still in use. `free` is the reuse pool;
    // `freed[i]` marks a slot currently free (guards double-free and powers debug use-after-free
    // assertions in `retain`/`release`/`get_binary_data`).
    free: Vec<usize>,
    pending_free: Vec<usize>,
    freed: Vec<bool>,
    // Cumulative count of slots reclaimed (for the worker inspector's "reclaimed this session").
    reclaimed: usize,
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

    /// Get BinaryData from heap by index
    pub fn get_heap_binary(&self, index: usize) -> Option<&BinaryData> {
        self.heap.get(index)
    }

    /// Get BinaryData from either constants or heap
    pub fn get_binary_data(&self, binary: &Binary) -> Result<&BinaryData, Error> {
        match binary {
            Binary::Constant(index) => {
                let constant = self
                    .get_constant(*index)
                    .ok_or(Error::ConstantUndefined(*index))?;
                match constant {
                    Constant::Binary(_bytes) => {
                        // For constants, we need to return a reference, but we only have Vec<u8>
                        // This is a limitation - we'll need to handle this differently
                        // For now, return an error - we'll fix this properly
                        Err(Error::InvalidArgument(
                            "Getting BinaryData from constant not yet implemented".to_string(),
                        ))
                    }
                    _ => Err(Error::TypeMismatch {
                        expected: "binary".to_string(),
                        found: "integer".to_string(),
                    }),
                }
            }
            Binary::Heap(index) => {
                debug_assert!(
                    !self.freed.get(*index).copied().unwrap_or(false),
                    "access of freed heap slot {index} (use-after-free)"
                );
                self.heap.get(*index).ok_or_else(|| {
                    Error::InvalidArgument(format!("Heap binary index {} not found", index))
                })
            }
        }
    }

    /// Allocate BinaryData on the heap and return a Binary reference
    pub fn allocate_binary_data(&mut self, data: BinaryData) -> Result<Binary, Error> {
        if data.len() > MAX_BINARY_SIZE {
            return Err(Error::InvalidArgument(format!(
                "Binary size {} exceeds maximum {}",
                data.len(),
                MAX_BINARY_SIZE
            )));
        }
        // Reuse a reclaimed slot if one is available, else grow the heap. Either way the slot
        // starts floating at count 0 until it enters rooted storage (see `retain`).
        if let Some(index) = self.free.pop() {
            self.heap[index] = data;
            self.refcounts[index] = 0;
            self.freed[index] = false;
            Ok(Binary::Heap(index))
        } else {
            let index = self.heap.len();
            self.heap.push(data);
            self.refcounts.push(0);
            self.freed.push(false);
            Ok(Binary::Heap(index))
        }
    }

    /// Reclaim slots whose count has settled at 0. Called at a safe point (the start of `step`),
    /// where no transient Rust-local Value handle or in-flight Action references a slot — so a
    /// slot still at 0 here is genuinely unreferenced. A slot re-retained since being queued
    /// (count > 0) is skipped; `freed` guards against double-freeing a duplicate queue entry.
    fn process_pending_free(&mut self) {
        while let Some(index) = self.pending_free.pop() {
            if self.refcounts[index] == 0 && !self.freed[index] {
                self.heap[index] = BinaryData::new(Vec::new()); // drop the data, reclaim memory
                self.freed[index] = true;
                self.free.push(index);
                self.reclaimed += 1;
            }
        }
    }

    /// Account for a value entering rooted storage: increment the count of every heap slot it
    /// references, recursing through tuples/functions. Deep and symmetric with [`release`];
    /// each `retain` must be matched by exactly one `release` when the reference leaves storage.
    ///
    pub(crate) fn retain(&mut self, value: &Value) {
        match value {
            Value::Binary(Binary::Heap(idx)) => {
                debug_assert!(
                    !self.freed[*idx],
                    "retain of freed heap slot {idx} (use-after-free)"
                );
                self.refcounts[*idx] += 1;
            }
            // The cached flag prunes the walk: a composite with no (transitive) heap
            // references has nothing to account for, so data movement stays O(1).
            Value::Tuple(_, elements) | Value::Function(_, elements)
                if elements.has_heap_refs() =>
            {
                for element in elements.all_values() {
                    self.retain(element);
                }
            }
            Value::Builtin(_, Some(payload)) if payload.has_heap_refs() => {
                for element in payload.all_values() {
                    self.retain(element);
                }
            }
            _ => {}
        }
    }

    /// Account for a value leaving rooted storage: the inverse of [`retain`]. A debug build
    /// panics on underflow — a `release` without a matching `retain`, i.e. an unwired insertion
    /// site. A count reaching 0 queues the slot for `process_pending_free`, which frees it at
    /// the next safe point.
    pub(crate) fn release(&mut self, value: &Value) {
        match value {
            Value::Binary(Binary::Heap(idx)) => {
                debug_assert!(
                    !self.freed[*idx],
                    "release of freed heap slot {idx} (use-after-free)"
                );
                debug_assert!(
                    self.refcounts[*idx] > 0,
                    "release underflow on heap slot {idx} (release without a matching retain)"
                );
                self.refcounts[*idx] = self.refcounts[*idx].saturating_sub(1);
                if self.refcounts[*idx] == 0 {
                    // Defer the actual free to the next safe point (see `process_pending_free`).
                    self.pending_free.push(*idx);
                }
            }
            Value::Tuple(_, elements) | Value::Function(_, elements)
                if elements.has_heap_refs() =>
            {
                for element in elements.all_values() {
                    self.release(element);
                }
            }
            Value::Builtin(_, Some(payload)) if payload.has_heap_refs() => {
                for element in payload.all_values() {
                    self.release(element);
                }
            }
            _ => {}
        }
    }

    // --- Choke points for rooted storage. The interpreter routes stack/locals mutations through
    // these so the reference counts stay in step with what is reachable. `proc` is the running
    // process, which during a `step` is owned separately from `self`, so retaining/releasing
    // against `self` alongside a `proc` borrow is conflict-free. Pure reordering (rotate/swap)
    // does not change the stored multiset and is left as a raw `proc.stack` call.

    /// Push a value onto the process stack, retaining its heap references.
    fn push_value(&mut self, proc: &mut Process, value: Value) {
        self.retain(&value);
        proc.stack.push(value);
    }

    /// Pop a value off the process stack, releasing its heap references. The returned handle is
    /// still valid because reclamation is deferred (a slot at 0 is freed only at the next safe
    /// point, in `process_pending_free`); re-inserting it via another `push_*` re-retains it
    /// before then, so a pop-then-push "move" nets to zero and the slot is never reclaimed.
    fn pop_value(&mut self, proc: &mut Process) -> Option<Value> {
        let value = proc.stack.pop();
        if let Some(value) = &value {
            self.release(value);
        }
        value
    }

    /// Push a value into the process's locals, retaining its heap references.
    fn push_local(&mut self, proc: &mut Process, value: Value) {
        self.retain(&value);
        proc.locals.push(value);
    }

    /// Truncate the process's locals to `len`, releasing every dropped binding.
    fn truncate_locals(&mut self, proc: &mut Process, len: usize) {
        if proc.locals.len() > len {
            let dropped = proc.locals.split_off(len);
            for value in &dropped {
                self.release(value);
            }
        }
    }

    /// Like [`truncate_locals`] but addresses the process by id, for callers (e.g. frame teardown
    /// in the step loop) that hold only `&mut self`, not a separate `&mut Process`.
    fn truncate_locals_pid(&mut self, pid: ProcessId, len: usize) {
        let dropped = match self.get_process_mut(pid) {
            Some(process) if process.locals.len() > len => process.locals.split_off(len),
            _ => return,
        };
        for value in &dropped {
            self.release(value);
        }
    }

    /// Replace a process's locals wholesale, retaining the incoming bindings and releasing the
    /// outgoing ones so the reference counts stay correct. Returns `false` if the process is gone.
    /// Used by the REPL's between-evaluation compaction (the caller selects which bindings to keep).
    pub fn replace_locals(&mut self, process_id: ProcessId, new_locals: Vec<Value>) -> bool {
        if self.get_process(process_id).is_none() {
            return false;
        }
        for value in &new_locals {
            self.retain(value);
        }
        let old = std::mem::replace(
            &mut self.get_process_mut(process_id).unwrap().locals,
            new_locals,
        );
        for value in &old {
            self.release(value);
        }
        true
    }

    /// Release the locals at every index *not* in `keep`, overwriting each with nil so its heap
    /// references are dropped. Unlike [`replace_locals`], indices are left in place (no re-indexing),
    /// so binding indices stay valid. Used by the REPL to reclaim a finished line's orphaned
    /// parameter and temporaries at the moment its result is delivered, without disturbing the
    /// host's binding map. Returns `false` if the process is gone.
    pub fn release_orphan_locals(&mut self, process_id: ProcessId, keep: &[usize]) -> bool {
        let keep: HashSet<usize> = keep.iter().copied().collect();
        let Some(process) = self.get_process_mut(process_id) else {
            return false;
        };
        let mut orphans = Vec::new();
        for (index, slot) in process.locals.iter_mut().enumerate() {
            if !keep.contains(&index) {
                orphans.push(std::mem::replace(slot, Value::nil()));
            }
        }
        for value in &orphans {
            self.release(value);
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
            // A dead process reads no more stream events: drop the stash (releasing
            // its values below) and the armed set (an in-flight completion for a
            // tombstone is dropped at delivery).
            process.armed_resources.clear();
            Some((
                std::mem::take(&mut process.stack),
                std::mem::take(&mut process.mailbox),
                std::mem::take(&mut process.awaiting),
                process.select_state.take(),
                std::mem::take(&mut process.resource_events),
            ))
        });
        let Some((stack, mailbox, awaiting, select_state, resource_events)) = taken else {
            return;
        };
        for value in stack.iter().chain(mailbox.iter()) {
            self.release(value);
        }
        for value in awaiting.values().flatten() {
            self.release(value);
        }
        for value in resource_events.values() {
            self.release(value);
        }
        if let Some(state) = select_state {
            for source in &state.sources {
                self.release(source);
            }
            if let Some((_, message)) = &state.receiving {
                self.release(message);
            }
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
        if let Some(Ok(value)) = &process.result {
            self.release(value);
        }
        self.release(&process.state);
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
    pub fn crash_result(
        &mut self,
        pid: ProcessId,
        error: &Error,
    ) -> Result<(Value, Vec<Vec<u8>>), Error> {
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
            Arc::new(Payload::with_annotations(
                vec![],
                vec![(table.crash_key, payload)],
            )),
        );
        // Root → extract → release, so the freshly allocated message binary goes
        // through the refcount lifecycle and its slot is reclaimable afterwards.
        self.retain(&stamped);
        let extracted = self.extract_heap_data(&stamped);
        self.release(&stamped);
        extracted
    }

    /// Validate the reference-count invariant against the tracing oracle: every heap slot must
    /// have a positive count exactly when it is reachable from a root ([`reachable_heap_indices`]).
    /// Returns the first violating slot, so it doubles as a debug assertion (the wiring is
    /// correct iff this stays `Ok` at every quiescent point) and a test oracle.
    pub fn check_refcounts(&self) -> Result<(), String> {
        let reachable = self.reachable_heap_indices();
        for index in 0..self.heap.len() {
            let counted = self.refcounts[index] > 0;
            let live = reachable.contains(&index);
            if counted != live {
                return Err(format!(
                    "heap slot {index}: refcount={} but reachable={live}",
                    self.refcounts[index]
                ));
            }
        }
        Ok(())
    }

    /// Create a binary from Vec<u8>
    pub fn allocate_binary(&mut self, bytes: Vec<u8>) -> Result<Binary, Error> {
        self.allocate_binary_data(BinaryData::new(bytes))
    }

    /// Read a binary's bytes, compacting a heap-stored rope to a flat `Owned` in place as a
    /// side effect. Flattening is content-preserving, so it is sound no matter how many
    /// references share the slot — a rope's children are internal `Rc`s, not heap slots, so no
    /// heap refcount changes and compaction is invisible to [`check_refcounts`]. After the first
    /// materialize, a vector built up by repeated push is contiguous, so subsequent reads and
    /// random access are cheap. Returns the bytes as a shared `Rc<Vec<u8>>`: the same allocation
    /// is stored in the slot and returned, so there is no extra copy.
    pub fn materialize(&mut self, binary: &Binary) -> Result<Rc<Vec<u8>>, Error> {
        if let Binary::Heap(index) = binary {
            debug_assert!(
                !self.freed.get(*index).copied().unwrap_or(false),
                "materialize of freed heap slot {index} (use-after-free)"
            );
            if let BinaryData::Owned(rc) = &self.heap[*index] {
                return Ok(rc.clone()); // already flat — O(1) share
            }
            // Flatten once; storing and returning the same `Rc` keeps the peak at ~2× (the
            // rope is dropped as the slot is overwritten), with no further copy.
            let rc = Rc::new(self.heap[*index].to_vec());
            self.heap[*index] = BinaryData::Owned(rc.clone());
            return Ok(rc);
        }
        // Non-heap (constant): flatten without write-back.
        Ok(Rc::new(self.get_binary_data(binary)?.to_vec()))
    }

    /// The set of heap-slot indices reachable from any live root: every process's
    /// Value-bearing state (stack, locals, mailbox, result, select sources/receiving, awaited
    /// results) plus the constant-binary cache (which pins its materialised slots for the
    /// program's lifetime). This is the tracing *oracle* against which incremental refcounts
    /// will be validated — a slot is correctly live iff it appears here.
    ///
    /// Meaningful at a quiescent point (between steps); during a `step` the running process is
    /// temporarily removed from the table and would be missed.
    pub fn reachable_heap_indices(&self) -> HashSet<usize> {
        let mut indices = HashSet::new();
        for process in self.processes.values() {
            for value in &process.stack {
                collect_heap_indices(value, &mut indices);
            }
            for value in &process.locals {
                collect_heap_indices(value, &mut indices);
            }
            for value in &process.mailbox {
                collect_heap_indices(value, &mut indices);
            }
            if let Some(Ok(value)) = &process.result {
                collect_heap_indices(value, &mut indices);
            }
            collect_heap_indices(&process.state, &mut indices);
            if let Some(state) = &process.select_state {
                for value in &state.sources {
                    collect_heap_indices(value, &mut indices);
                }
                if let Some((_, value)) = &state.receiving {
                    collect_heap_indices(value, &mut indices);
                }
            }
            for value in process.awaiting.values().flatten() {
                collect_heap_indices(value, &mut indices);
            }
            for value in process.resource_events.values() {
                collect_heap_indices(value, &mut indices);
            }
        }
        for binary in self.constant_binaries.iter().flatten() {
            if let Binary::Heap(idx) = binary {
                indices.insert(*idx);
            }
        }
        indices
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

    /// Snapshot heap occupancy (see [`HeapStats`]). Read-only; call at a quiescent point.
    pub fn heap_stats(&self) -> HeapStats {
        let reachable = self.reachable_heap_indices();
        let total_bytes = self.heap.iter().map(BinaryData::len).sum();
        let reachable_bytes = reachable
            .iter()
            .filter_map(|&idx| self.heap.get(idx))
            .map(BinaryData::len)
            .sum();
        HeapStats {
            slots: self.heap.len(),
            reachable: reachable.len(),
            total_bytes,
            reachable_bytes,
        }
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
            // NIL (id 0) and OK (id 1) are each their own canonical shape; replaced on first update.
            canonical_tuples: vec![0, 1],
            resources: vec![],
            type_compatibility: vec![],
            function_param_compatibility: vec![],
            builtin_param_compatibility: vec![],
            field_offsets: vec![],
            heap: vec![],
            refcounts: vec![],
            free: vec![],
            pending_free: vec![],
            freed: vec![],
            reclaimed: 0,
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
        captures: Vec<Value>,
        argument: Value,
        heap_data: Vec<Vec<u8>>,
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

        // Inject heap data and populate locals with captures
        let captures_count = captures.len();
        for value in captures {
            let injected = self.inject_heap_data(value, &heap_data)?;
            // Injected into rooted storage (the new frame's locals).
            self.retain(&injected);
            let process = self
                .get_process_mut(id)
                .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
            process.locals.push(injected);
        }

        // Push argument onto stack; it is also the process's initial observable state
        // (one retain per storage location).
        let injected_arg = self.inject_heap_data(argument, &heap_data)?;
        self.retain(&injected_arg);
        self.retain(&injected_arg);
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
    pub fn notify_state(
        &mut self,
        id: ProcessId,
        state: Value,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), Error> {
        let sample = self.inject_heap_data(state, &heap)?;
        self.retain(&sample);

        match self.get_process_mut(id) {
            Some(process) => {
                // Already retained above; push raw.
                process.stack.push(sample);
                if let Some(frame) = process.frames.last_mut() {
                    frame.counter += 1;
                }
                if self.sampling.remove(&id) {
                    self.queue.push_back(id);
                }
            }
            None => self.release(&sample),
        }
        Ok(())
    }

    /// Notify a process that was waiting for a result with the result value
    pub fn notify_result(
        &mut self,
        awaiter: ProcessId,
        awaited: ProcessId,
        result: Value,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), Error> {
        // Inject heap data into the result value
        let injected_result = self.inject_heap_data(result, &heap)?;

        // Store the result in the process's awaiting map (retaining as it enters storage,
        // releasing any stale result the insert displaces). A terminated awaiter (e.g.
        // killed while parked on this very select) is skipped, like a dead message
        // target — its watcher entry on the source dangles harmlessly.
        if self
            .get_process(awaiter)
            .is_some_and(|p| p.persistent || p.result.is_none())
        {
            self.retain(&injected_result);
            let displaced = self
                .get_process_mut(awaiter)
                .unwrap()
                .awaiting
                .insert(awaited, Some(injected_result));
            if let Some(Some(old)) = displaced {
                self.release(&old);
            }
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
        result: Result<Value, String>,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), Error> {
        let was_effecting = self.effecting.remove(&process_id);

        // Convert result to either Ok(Value) or Err(Error)
        let value_result = match result {
            Ok(v) => Ok(self.inject_heap_data(v, &heap)?),
            Err(err_msg) => Err(Error::InvalidArgument(format!(
                "Effect operation failed: {}",
                err_msg
            ))),
        };

        // Retain the success value as it enters the stack (below).
        if let Ok(value) = &value_result {
            self.retain(value);
        }

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

    pub fn notify_message(
        &mut self,
        id: ProcessId,
        message: Value,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), Error> {
        // A completed process can never receive again — drop the message (rather than
        // queueing it forever on the tombstone). Persistent (REPL) processes complete
        // between submissions but stay addressable, so they still queue. Skipping the
        // heap injection entirely means there is nothing to account for.
        let deliverable = self
            .get_process(id)
            .is_some_and(|p| p.persistent || p.result.is_none());

        if deliverable {
            let injected_message = self.inject_heap_data(message, &heap)?;
            self.retain(&injected_message);
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
        heap: Vec<Vec<u8>>,
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
                let bytes = heap.into_iter().next().unwrap_or_default();
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
        self.retain(&value);

        let process = self.get_process_mut(id).unwrap();
        process.armed_resources.remove(&resource_id);
        let displaced = process.resource_events.insert(resource_id, value);
        debug_assert!(
            displaced.is_none(),
            "a stream resource may have at most one event in flight"
        );
        if let Some(old) = displaced {
            self.release(&old);
        }

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

    /// Distinct heap-slot indices reachable from a set of values.
    fn heap_set<'a>(&self, values: impl Iterator<Item = &'a Value>) -> HashSet<usize> {
        let mut set = HashSet::new();
        for value in values {
            collect_heap_indices(value, &mut set);
        }
        set
    }

    /// Total bytes occupied by a set of heap slots.
    fn heap_bytes(&self, indices: &HashSet<usize>) -> usize {
        indices
            .iter()
            .filter_map(|&i| self.heap.get(i))
            .map(BinaryData::len)
            .sum()
    }

    fn usage(&self, indices: &HashSet<usize>) -> crate::process::HeapUsage {
        crate::process::HeapUsage {
            slots: indices.len(),
            bytes: self.heap_bytes(indices),
        }
    }

    /// A process's per-root binary-heap footprint (see [`crate::process::ProcessHeapUsage`]).
    fn process_heap_usage(&self, process: &Process) -> crate::process::ProcessHeapUsage {
        let stack = self.heap_set(process.stack.iter());
        let locals = self.heap_set(process.locals.iter());
        let mailbox = self.heap_set(process.mailbox.iter());

        // The total is the union across every root, deduplicated.
        let mut total = &stack | &locals;
        total.extend(&mailbox);
        if let Some(Ok(value)) = &process.result {
            collect_heap_indices(value, &mut total);
        }
        if let Some(state) = &process.select_state {
            for value in &state.sources {
                collect_heap_indices(value, &mut total);
            }
            if let Some((_, value)) = &state.receiving {
                collect_heap_indices(value, &mut total);
            }
        }
        for value in process.awaiting.values().flatten() {
            collect_heap_indices(value, &mut total);
        }
        for value in process.resource_events.values() {
            collect_heap_indices(value, &mut total);
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
    pub fn worker_info(&self) -> crate::process::WorkerInfo {
        let reachable = self.reachable_heap_indices();
        let constant_indices: HashSet<usize> = self
            .constant_binaries
            .iter()
            .flatten()
            .filter_map(|b| match b {
                Binary::Heap(i) => Some(*i),
                _ => None,
            })
            .collect();
        crate::process::WorkerInfo {
            worker_id: self.worker_id,
            process_ids: self.processes.keys().copied().collect(),
            heap_slots: self.heap.len(),
            live_slots: reachable.len(),
            free_slots: self.free.len(),
            pending_free: self.pending_free.len(),
            reclaimed: self.reclaimed,
            live_bytes: self.heap_bytes(&reachable),
            total_bytes: self.heap.iter().map(BinaryData::len).sum(),
            constant_slots: constant_indices.len(),
            constant_bytes: self.heap_bytes(&constant_indices),
        }
    }

    pub fn get_process_info(&self, id: ProcessId) -> Option<ProcessInfo> {
        self.processes.get(&id).map(|process| {
            // Extract heap data from result if present
            let result = match &process.result {
                Some(Ok(value)) => match self.extract_heap_data(value) {
                    Ok((extracted_value, heap)) => Some(Ok((extracted_value, heap))),
                    Err(_) => process.result.clone().map(|r| r.map(|v| (v, vec![]))),
                },
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
        // Extract arities from TupleTypeInfo - executor only needs arities at runtime
        self.tuples
            .extend(update.tuples.iter().map(|t| t.fields.len()));
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
                    Arc::new(Payload::with_annotations(
                        vec![],
                        vec![(table.origin_key, origin.clone())],
                    )),
                )
            })
            .collect();
    }

    /// Execute up to max_units instruction units for a single process.
    /// Returns (did_work, optional_action) where did_work indicates if any instructions were executed.
    pub fn step(&mut self, max_units: usize, current_time_ms: u64) -> (bool, Option<Action<E>>) {
        // Reclaim slots that settled at count 0 since the last step. Doing it here (a quiescent
        // point — any Action returned by the previous step has been handled by the Environment,
        // and no Rust-local Value handles are live) is what makes deferred reclamation safe.
        self.process_pending_free();
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
                break; // Process finished or no more instructions in current frame
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

            // Pop the exhausted frame and decide what to release; the local-release happens after
            // the process borrow ends, since it needs `&mut self`.
            let (clear_base, is_track_boundary) = {
                let process = self
                    .get_process_mut(current_pid)
                    .expect("Process should exist");

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
                self.truncate_locals_pid(current_pid, base);
            }
            if is_track_boundary {
                self.reconcile_tracking(current_pid);
            }
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
            #[cfg(debug_assertions)]
            if let Err(e) = self.check_refcounts() {
                panic!("refcount invariant violated at process {current_pid} completion: {e}");
            }
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

        let result = match instruction {
            Instruction::Constant(index) => self.handle_constant(proc, index),
            Instruction::Pop => self.handle_pop(proc),
            Instruction::Duplicate => self.handle_duplicate(proc),
            Instruction::Pick(n) => self.handle_pick(proc, n),
            Instruction::Rotate(n) => self.handle_rotate(proc, n),
            Instruction::Load(index) => self.handle_load(proc, index),
            Instruction::Store => self.handle_store(proc),
            Instruction::Tuple(type_id) => self.handle_tuple(proc, type_id),
            Instruction::GetPositional(index) => self.handle_get_positional(proc, index),
            Instruction::GetNamed(name_id) => self.handle_get_named(proc, name_id),
            Instruction::IsType(type_id) => self.handle_is_type(proc, type_id),
            Instruction::Jump(offset) => self.handle_jump(proc, offset),
            Instruction::JumpIf(offset) => self.handle_jump_if(proc, offset),
            Instruction::Call => self.handle_call(proc, pid),
            Instruction::TailCall(recurse) => self.handle_tail_call(proc, recurse),
            Instruction::Function(function_index) => self.handle_function(proc, function_index),
            Instruction::Reset(index) => self.handle_reset(proc, index),
            Instruction::Builtin(index) => self.handle_builtin(proc, index),
            Instruction::Equal(count) => self.handle_equal(proc, count),
            Instruction::Not => self.handle_not(proc),
            Instruction::Annotate(key) => self.handle_annotate(proc, key),
            Instruction::GetAnnotation(key, check) => self.handle_get_annotation(proc, key, check),
            Instruction::Stamp(site) => self.handle_stamp(proc, site),
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
                self.handle_process_ref(pid, process_id, function_index)
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

    /// Resolve a binary constant to a heap reference, materialising and caching it on first use.
    fn cached_constant_binary(&mut self, index: usize) -> Result<Binary, Error> {
        if let Some(Some(binary)) = self.constant_binaries.get(index) {
            return Ok(*binary);
        }
        let bytes = match self.get_constant(index) {
            Some(Constant::Binary(bytes)) => bytes.clone(),
            _ => return Err(Error::ConstantUndefined(index)),
        };
        let binary = self.allocate_binary(bytes)?;
        if self.constant_binaries.len() <= index {
            self.constant_binaries.resize(index + 1, None);
        }
        self.constant_binaries[index] = Some(binary);
        // The constant cache is itself a root, so it holds a reference to the materialised slot.
        self.retain(&Value::Binary(binary));
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
        let mut values = Vec::new();
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

        let annotated = carrier
            .annotated(key, annotation)
            .ok_or_else(|| Error::TypeMismatch {
                expected: "tuple, function or builtin".to_string(),
                found: carrier.type_name().to_string(),
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
            Value::Builtin(builtin_id, _) => {
                // Pop function (discarded) and parameter (consumed by the builtin).
                self.pop_value(proc); // function
                let parameter = self.pop_value(proc).ok_or(Error::StackUnderflow)?;

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
                    if purity == crate::builtins::Purity::HostRead && self.compile_time {
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
                    let mut ctx = crate::builtins::BuiltinContext::new(pid, proc, self);
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
        self.retain(argument);
        let old = std::mem::replace(&mut proc.state, argument.clone());
        self.release(&old);
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
    ) -> Result<Option<Action<E>>, Error> {
        // Verify builtin exists
        if index >= self.builtins.len() {
            return Err(Error::BuiltinUndefined(index));
        }
        // Push builtin by index (no heap references).
        self.push_value(proc, Value::builtin(index));

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

        let result = if all_equal {
            first.clone()
        } else {
            Value::nil()
        };

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
        self.release(&function_value);
        self.release(&argument);

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
        self.release(&message);

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
        self.release(&target_value);

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
            self.retain(&sample);
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
            self.release(&verdict);
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

        process.select_state = Some(SelectState {
            frame: current_frame,
            instruction: current_instruction,
            sources,
            cursors: vec![0; receive_count],
            start_time,
            receiving: None,
        });

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
            for old in &displaced {
                self.release(old);
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
                        self.release(&event);
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

            let removed = {
                let process = self
                    .get_process_mut(pid)
                    .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                if msg_idx < process.mailbox.len() {
                    process.mailbox.remove(msg_idx)
                } else {
                    None
                }
            };
            if let Some(removed) = &removed {
                self.release(removed); // accepted message leaves the mailbox
            }
            Ok(Some(message_value.clone()))
        } else {
            // Nil result - increment cursor and reset receiving (releasing the held message).
            let dropped = {
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
            if let Some(message) = &dropped {
                self.release(message);
            }
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
                    let removed = {
                        let process = self
                            .get_process_mut(pid)
                            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;
                        if msg_idx < process.mailbox.len() {
                            process.mailbox.remove(msg_idx)
                        } else {
                            None
                        }
                    };
                    if let Some(removed) = &removed {
                        self.release(removed); // message leaves the mailbox
                    }
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
        self.retain(&message);
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
        // Tear down the select state, releasing the references it held (the source list and any
        // in-flight received message), then push the result (retaining it on the stack).
        let state = self
            .get_process_mut(pid)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?
            .select_state
            .take();
        if let Some(state) = state {
            for source in &state.sources {
                self.release(source);
            }
            if let Some((_, message)) = &state.receiving {
                self.release(message);
            }
        }

        self.retain(&result);
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

    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => a == b,
            // Mixed small/big pairs are unequal by the canonical-form invariant.
            (Value::BigInt(a), Value::BigInt(b)) => a == b,
            (Value::Binary(a), Value::Binary(b)) => {
                // Compare binary data content
                match (a, b) {
                    (Binary::Constant(idx_a), Binary::Constant(idx_b)) => {
                        // Both are constants - compare the constant data
                        if let (Some(Constant::Binary(bytes_a)), Some(Constant::Binary(bytes_b))) =
                            (self.get_constant(*idx_a), self.get_constant(*idx_b))
                        {
                            bytes_a == bytes_b
                        } else {
                            false
                        }
                    }
                    (Binary::Heap(idx_a), Binary::Heap(idx_b)) => {
                        // Both are heap - compare the heap data
                        if let (Some(data_a), Some(data_b)) =
                            (self.heap.get(*idx_a), self.heap.get(*idx_b))
                        {
                            // Compare lengths first (fast path)
                            if data_a.len() != data_b.len() {
                                return false;
                            }
                            // Compare bytes
                            data_a.to_vec() == data_b.to_vec()
                        } else {
                            false
                        }
                    }
                    (Binary::Constant(idx_c), Binary::Heap(idx_h))
                    | (Binary::Heap(idx_h), Binary::Constant(idx_c)) => {
                        // One is constant, one is heap - compare bytes
                        if let (Some(Constant::Binary(bytes_c)), Some(data_h)) =
                            (self.get_constant(*idx_c), self.heap.get(*idx_h))
                        {
                            bytes_c.as_slice() == data_h.to_vec().as_slice()
                        } else {
                            false
                        }
                    }
                }
            }
            (Value::Tuple(type_a, elements_a), Value::Tuple(type_b, elements_b)) => {
                // Compare by canonical value-shape (name + field labels), not raw tuple-id: the
                // same tuple shape built via paths that inferred different field types gets distinct
                // ids, but is the same value. Elements are then compared structurally.
                self.canonical_tuple(*type_a) == self.canonical_tuple(*type_b)
                    && elements_a.len() == elements_b.len()
                    && elements_a
                        .iter()
                        .zip(elements_b.iter())
                        .all(|(a, b)| self.values_equal(a, b))
            }
            (Value::Function(idx_a, caps_a), Value::Function(idx_b, caps_b)) => {
                idx_a == idx_b
                    && caps_a.len() == caps_b.len()
                    && caps_a
                        .iter()
                        .zip(caps_b.iter())
                        .all(|(a, b)| self.values_equal(a, b))
            }
            (Value::Builtin(a, _), Value::Builtin(b, _)) => a == b,
            (Value::Process(a, func_a), Value::Process(b, func_b)) => a == b && func_a == func_b,
            (Value::Reference(a), Value::Reference(b)) => a == b,
            _ => false,
        }
    }

    /// Extract heap data from a value for serialization across thread boundaries
    /// Returns (value, heap_data) where heap_data is a Vec of flattened binary data
    /// Extract heap data across several values at once, remapped against ONE shared
    /// index space — the values ship with a single side-channel vec (a spawn's
    /// captures plus argument). Extracting values separately and concatenating their
    /// vecs would leave every value after the first pointing at the wrong entries.
    pub fn extract_heap_data_many(
        &self,
        values: &[Value],
    ) -> Result<(Vec<Value>, Vec<Vec<u8>>), Error> {
        let mut heap_indices = HashSet::new();
        for value in values {
            collect_heap_indices(value, &mut heap_indices);
        }
        let mut indices_vec: Vec<usize> = heap_indices.into_iter().collect();
        indices_vec.sort_unstable();
        let mut index_map = HashMap::new();
        for (new_idx, &old_idx) in indices_vec.iter().enumerate() {
            index_map.insert(old_idx, new_idx);
        }
        let mut heap_data = Vec::new();
        for &old_idx in &indices_vec {
            if let Some(binary_data) = self.heap.get(old_idx) {
                heap_data.push(binary_data.to_vec());
            } else {
                return Err(Error::InvalidArgument(format!(
                    "Heap index {} not found",
                    old_idx
                )));
            }
        }
        let remapped = values
            .iter()
            .map(|value| remap_heap_indices(value, &index_map))
            .collect::<Result<Vec<_>, _>>()?;
        Ok((remapped, heap_data))
    }

    pub fn extract_heap_data(&self, value: &Value) -> Result<(Value, Vec<Vec<u8>>), Error> {
        // Collect all unique heap indices referenced by this value
        let mut heap_indices = HashSet::new();
        collect_heap_indices(value, &mut heap_indices);

        // Sort indices for deterministic ordering
        let mut indices_vec: Vec<usize> = heap_indices.into_iter().collect();
        indices_vec.sort_unstable();

        // Create index mapping from old heap index to new compact index
        let mut index_map = HashMap::new();
        for (new_idx, &old_idx) in indices_vec.iter().enumerate() {
            index_map.insert(old_idx, new_idx);
        }

        // Extract and flatten binary data
        let mut heap_data = Vec::new();
        for &old_idx in &indices_vec {
            if let Some(binary_data) = self.heap.get(old_idx) {
                heap_data.push(binary_data.to_vec());
            } else {
                return Err(Error::InvalidArgument(format!(
                    "Heap index {} not found",
                    old_idx
                )));
            }
        }

        // Remap value indices
        let remapped_value = remap_heap_indices(value, &index_map)?;

        Ok((remapped_value, heap_data))
    }
}

/// Recursively collect all pids referenced by a value (the reclamation-graph twin of
/// [`collect_heap_indices`]). Walks tuple/function/builtin payloads via `all_values()`,
/// which covers annotations too — so a pid inside a `:crash` payload is followed. Pushes
/// duplicates; the caller dedups.
fn collect_process_refs(value: &Value, pids: &mut Vec<ProcessId>) {
    match value {
        Value::Process(pid, _) => pids.push(*pid),
        Value::Tuple(_, elements) | Value::Function(_, elements) => {
            for elem in elements.all_values() {
                collect_process_refs(elem, pids);
            }
        }
        Value::Builtin(_, Some(payload)) => {
            for elem in payload.all_values() {
                collect_process_refs(elem, pids);
            }
        }
        _ => {}
    }
}

/// Recursively collect all heap indices referenced by a value
fn collect_heap_indices(value: &Value, indices: &mut HashSet<usize>) {
    match value {
        Value::Binary(Binary::Heap(idx)) => {
            indices.insert(*idx);
        }
        Value::Tuple(_, elements) | Value::Function(_, elements) => {
            for elem in elements.all_values() {
                collect_heap_indices(elem, indices);
            }
        }
        Value::Builtin(_, Some(payload)) => {
            for elem in payload.all_values() {
                collect_heap_indices(elem, indices);
            }
        }
        _ => {}
    }
}

/// Remap the heap indices in a payload's annotations (helper for `remap_heap_indices`).
fn remap_annotations(
    payload: &Payload,
    index_map: &HashMap<usize, usize>,
) -> Result<Vec<(usize, Value)>, Error> {
    payload
        .annotations()
        .iter()
        .map(|(key, value)| Ok((*key, remap_heap_indices(value, index_map)?)))
        .collect()
}

/// Remap heap indices in a value according to the provided mapping
/// Remap the heap-binary indices in a value that has crossed a registry-less boundary (an effect
/// result or an incoming message) into this executor's heap via `index_map`. Tuple/function ids
/// are left untouched — the producer (a registry-ful process, or a backend handed its type ids)
/// already stamped real ids.
fn remap_heap_indices(value: &Value, index_map: &HashMap<usize, usize>) -> Result<Value, Error> {
    match value {
        Value::Binary(Binary::Heap(old_idx)) => {
            if let Some(&new_idx) = index_map.get(old_idx) {
                Ok(Value::Binary(Binary::Heap(new_idx)))
            } else {
                Err(Error::InvalidArgument(format!(
                    "Heap index {} not in mapping",
                    old_idx
                )))
            }
        }
        Value::Binary(binary @ Binary::Constant(_)) => Ok(Value::Binary(*binary)),
        Value::Tuple(type_id, elements) => {
            let remapped_elements: Result<Vec<_>, _> = elements
                .iter()
                .map(|elem| remap_heap_indices(elem, index_map))
                .collect();
            Ok(Value::Tuple(
                *type_id,
                Arc::new(Payload::with_annotations(
                    remapped_elements?,
                    remap_annotations(elements, index_map)?,
                )),
            ))
        }
        Value::Function(func_idx, captures) => {
            let remapped_captures: Result<Vec<_>, _> = captures
                .iter()
                .map(|capture| remap_heap_indices(capture, index_map))
                .collect();
            Ok(Value::Function(
                *func_idx,
                Arc::new(Payload::with_annotations(
                    remapped_captures?,
                    remap_annotations(captures, index_map)?,
                )),
            ))
        }
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::BigInt(n) => Ok(Value::BigInt(n.clone())),
        Value::Builtin(builtin_id, payload) => {
            let payload = payload
                .as_ref()
                .map(|payload| {
                    Ok::<_, Error>(Arc::new(Payload::with_annotations(
                        vec![],
                        remap_annotations(payload, index_map)?,
                    )))
                })
                .transpose()?;
            Ok(Value::Builtin(*builtin_id, payload))
        }
        Value::Process(pid, func_idx) => Ok(Value::Process(*pid, *func_idx)),
        Value::Resource(id, type_name) => Ok(Value::Resource(*id, *type_name)),
        Value::Reference(r) => Ok(Value::Reference(*r)),
    }
}

impl<E: Effect> Executor<E> {
    /// Inject heap data into this executor and remap heap indices in the value.
    /// Binary heap data is allocated to the executor's heap and indices are remapped.
    pub fn inject_heap_data(
        &mut self,
        value: Value,
        heap_data: &[Vec<u8>],
    ) -> Result<Value, Error> {
        // Allocate all heap data to executor and build index mapping. Goes through
        // `allocate_binary_data` so `refcounts` stays parallel to `heap` (the injected slots
        // start floating at 0; the receiving process's placement sites retain them).
        let mut index_map = HashMap::new();
        for (new_idx, bytes) in heap_data.iter().enumerate() {
            let Binary::Heap(heap_idx) = self.allocate_binary(bytes.clone())? else {
                unreachable!("allocate_binary always returns a heap binary")
            };
            index_map.insert(new_idx, heap_idx);
        }

        // Remap value indices
        remap_heap_indices(&value, &index_map)
    }
}

#[cfg(test)]
mod heap_stats_tests {
    use super::*;
    use crate::builtins::BuiltinRegistry;
    use crate::process::SelectState;
    use crate::value::ResourceId;
    use serde::{Deserialize, Serialize};

    // A do-nothing effect so we can build a bare Executor without a host backend.
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

    fn bin(value: &Binary) -> Value {
        Value::Binary(*value)
    }

    #[test]
    fn counts_slots_bytes_and_reachable() {
        let mut ex = executor();
        let b0 = ex.allocate_binary(vec![1, 2, 3]).unwrap(); // Heap(0), reachable
        let b1 = ex.allocate_binary(vec![4, 5]).unwrap(); // Heap(1), reachable
        let _b2 = ex.allocate_binary(vec![6]).unwrap(); // Heap(2), dead

        let mut p = Process::new(false);
        p.stack.push(bin(&b0));
        p.locals.push(bin(&b1));
        ex.processes.insert(0, p);

        let stats = ex.heap_stats();
        assert_eq!(stats.slots, 3);
        assert_eq!(stats.reachable, 2);
        assert_eq!(stats.dead(), 1); // the unreferenced b2 is garbage
        assert_eq!(stats.total_bytes, 3 + 2 + 1);
        assert_eq!(stats.reachable_bytes, 3 + 2);
    }

    #[test]
    fn finds_binaries_nested_in_tuples() {
        let mut ex = executor();
        let b = ex.allocate_binary(vec![7, 7]).unwrap();

        let mut p = Process::new(false);
        // A binary buried two tuples deep must still be reached.
        let inner = Value::tuple(0, vec![bin(&b)]);
        p.locals.push(Value::tuple(0, vec![Value::nil(), inner]));
        ex.processes.insert(0, p);

        assert_eq!(ex.reachable_heap_indices(), HashSet::from([0]));
    }

    #[test]
    fn sweeps_every_root_kind() {
        let mut ex = executor();
        let in_stack = ex.allocate_binary(vec![0]).unwrap();
        let in_locals = ex.allocate_binary(vec![1]).unwrap();
        let in_mailbox = ex.allocate_binary(vec![2]).unwrap();
        let in_result = ex.allocate_binary(vec![3]).unwrap();
        let in_select = ex.allocate_binary(vec![4]).unwrap();
        let in_receiving = ex.allocate_binary(vec![5]).unwrap();
        let in_awaiting = ex.allocate_binary(vec![6]).unwrap();
        let dead = ex.allocate_binary(vec![9]).unwrap();

        let mut p = Process::new(false);
        p.stack.push(bin(&in_stack));
        p.locals.push(bin(&in_locals));
        p.mailbox.push_back(bin(&in_mailbox));
        p.result = Some(Ok(bin(&in_result)));
        p.select_state = Some(SelectState {
            frame: 0,
            instruction: 0,
            sources: vec![bin(&in_select)],
            cursors: vec![],
            start_time: None,
            receiving: Some((0, bin(&in_receiving))),
        });
        p.awaiting.insert(1, Some(bin(&in_awaiting)));
        ex.processes.insert(0, p);

        let reachable = ex.reachable_heap_indices();
        // Every root kind contributes; only `dead` is missing.
        assert_eq!(reachable.len(), 7);
        for b in [
            in_stack,
            in_locals,
            in_mailbox,
            in_result,
            in_select,
            in_receiving,
            in_awaiting,
        ] {
            let Binary::Heap(i) = b else { unreachable!() };
            assert!(reachable.contains(&i));
        }
        let Binary::Heap(d) = dead else {
            unreachable!()
        };
        assert!(!reachable.contains(&d));
    }

    #[test]
    fn constant_cache_pins_slots() {
        let mut ex = executor();
        let pinned = ex.allocate_binary(vec![1, 2, 3, 4]).unwrap();
        // No process references it, but the constant cache does, so it stays reachable.
        ex.constant_binaries.push(Some(pinned));

        let stats = ex.heap_stats();
        assert_eq!(stats.slots, 1);
        assert_eq!(stats.reachable, 1);
        assert_eq!(stats.dead(), 0);
    }

    // --- refcount engine (the value-movement bookkeeping the interpreter will drive) ---

    #[test]
    fn retain_release_round_trip_keeps_invariant() {
        let mut ex = executor();
        let b = ex.allocate_binary(vec![1, 2, 3]).unwrap();
        // Floating: count 0, not reachable — consistent.
        assert_eq!(ex.refcounts[0], 0);
        assert!(ex.check_refcounts().is_ok());

        // Enter rooted storage (a process's stack) -> retain.
        let value = bin(&b);
        ex.retain(&value);
        let mut p = Process::new(false);
        p.stack.push(value);
        ex.processes.insert(0, p);
        assert_eq!(ex.refcounts[0], 1);
        assert!(ex.check_refcounts().is_ok());

        // Leave storage -> release. Back to floating/consistent.
        let value = ex.get_process_mut(0).unwrap().stack.pop().unwrap();
        ex.release(&value);
        assert_eq!(ex.refcounts[0], 0);
        assert!(ex.check_refcounts().is_ok());
    }

    #[test]
    fn retain_recurses_into_nested_tuples() {
        let mut ex = executor();
        let b = ex.allocate_binary(vec![9]).unwrap();
        let tuple = Value::tuple(0, vec![Value::nil(), bin(&b)]);

        ex.retain(&tuple); // deep: bumps the nested binary
        let mut p = Process::new(false);
        p.locals.push(tuple);
        ex.processes.insert(0, p);

        assert_eq!(ex.refcounts[0], 1);
        assert!(ex.check_refcounts().is_ok());
    }

    #[test]
    fn shared_references_count_each_path() {
        let mut ex = executor();
        let b = ex.allocate_binary(vec![1]).unwrap();
        let tuple = Value::tuple(0, vec![bin(&b)]);

        // Two stack slots each hold the tuple -> two reference paths to the binary.
        let mut p = Process::new(false);
        ex.retain(&tuple);
        p.stack.push(tuple.clone());
        ex.retain(&tuple);
        p.stack.push(tuple);
        ex.processes.insert(0, p);
        assert_eq!(ex.refcounts[0], 2);
        assert!(ex.check_refcounts().is_ok());

        // Drop one path: still reachable via the other slot (presence-consistent).
        let value = ex.get_process_mut(0).unwrap().stack.pop().unwrap();
        ex.release(&value);
        assert_eq!(ex.refcounts[0], 1);
        assert!(ex.check_refcounts().is_ok());
    }

    #[test]
    fn check_catches_missing_retain() {
        // A binary rooted on the stack but never retained: reachable yet count 0.
        let mut ex = executor();
        let b = ex.allocate_binary(vec![1]).unwrap();
        let mut p = Process::new(false);
        p.stack.push(bin(&b));
        ex.processes.insert(0, p);
        assert!(ex.check_refcounts().is_err());
    }

    #[test]
    fn check_catches_missing_release() {
        // A binary retained but never rooted (a leak): count 1 yet unreachable.
        let mut ex = executor();
        let b = ex.allocate_binary(vec![1]).unwrap();
        ex.retain(&bin(&b));
        assert!(ex.check_refcounts().is_err());
    }

    // --- reclamation (deferred free + slot reuse) ---

    #[test]
    fn reclaims_and_reuses_slots() {
        let mut ex = executor();
        let b0 = ex.allocate_binary(vec![1, 2, 3]).unwrap(); // Heap(0)
        let Binary::Heap(i0) = b0 else { unreachable!() };
        ex.retain(&bin(&b0)); // rooted (count 1)
        ex.release(&bin(&b0)); // count 0 -> queued, but not yet freed (deferred)
        assert!(!ex.freed[i0]);
        assert_eq!(ex.heap.len(), 1);

        ex.process_pending_free(); // what `step` does at a safe point
        assert!(ex.freed[i0]);
        assert_eq!(ex.free, vec![i0]);

        // The next allocation reuses the slot instead of growing the heap.
        let b1 = ex.allocate_binary(vec![9]).unwrap();
        let Binary::Heap(i1) = b1 else { unreachable!() };
        assert_eq!(i1, i0, "freed slot should be reused");
        assert_eq!(
            ex.heap.len(),
            1,
            "heap must not grow while a free slot exists"
        );
        assert!(!ex.freed[i1]);
    }

    #[test]
    fn deferral_protects_a_move() {
        // A release-then-retain "move" must NOT reclaim the slot: by the time the deferred free
        // runs, the count is back above zero, so the slot is kept.
        let mut ex = executor();
        let b = ex.allocate_binary(vec![1]).unwrap();
        let Binary::Heap(i) = b else { unreachable!() };
        ex.retain(&bin(&b)); // on the stack (count 1)
        ex.release(&bin(&b)); // pop: count 0 -> queued
        ex.retain(&bin(&b)); // re-push (the move): count 1

        ex.process_pending_free();
        assert!(!ex.freed[i], "a re-retained slot must not be reclaimed");
        assert!(ex.free.is_empty());
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
        p.select_state = Some(SelectState {
            frame: 0,
            instruction: 0,
            sources: vec![pid(14)],
            cursors: vec![],
            start_time: None,
            receiving: Some((0, pid(15))),
        });
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
    fn retain_release_walk_annotation_values() {
        let mut ex = executor();
        let b = ex.allocate_binary(vec![1, 2, 3]).unwrap();
        let Binary::Heap(idx) = b else { unreachable!() };

        let carrier = Value::tuple(3, vec![Value::int(1)]);
        let annotated = carrier.annotated(0, Value::Binary(b)).unwrap();
        assert!(
            annotated.has_heap_refs(),
            "heap ref inside an annotation must set the cached flag"
        );

        ex.retain(&annotated);
        assert_eq!(ex.refcounts[idx], 1);
        ex.release(&annotated);
        assert_eq!(ex.refcounts[idx], 0);
    }

    #[test]
    fn annotated_builtin_walks_and_stays_equal() {
        let mut ex = executor();
        let b = ex.allocate_binary(vec![4, 5]).unwrap();
        let Binary::Heap(idx) = b else { unreachable!() };

        let bare = Value::builtin(7);
        let annotated = bare.annotated(0, Value::Binary(b)).unwrap();
        assert!(
            annotated.has_heap_refs(),
            "heap ref inside a builtin annotation must be visible to accounting"
        );
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

        ex.retain(&annotated);
        assert_eq!(ex.refcounts[idx], 1);
        ex.release(&annotated);
        assert_eq!(ex.refcounts[idx], 0);
    }

    #[test]
    fn transfer_round_trip_preserves_builtin_annotations() {
        let mut source = executor();
        let b = source.allocate_binary(vec![6, 6, 6]).unwrap();
        let annotated = Value::builtin(7).annotated(2, Value::Binary(b)).unwrap();

        let (wire_value, heap_data) = source.extract_heap_data(&annotated).unwrap();
        assert_eq!(heap_data, vec![vec![6, 6, 6]]);

        let mut target = executor();
        let received = target.inject_heap_data(wire_value, &heap_data).unwrap();
        let Some(Value::Binary(Binary::Heap(new_idx))) = received.get_annotation(2).cloned() else {
            panic!("annotation lost or not a heap binary after transfer");
        };
        assert_eq!(
            target.get_heap_binary(new_idx).unwrap().to_vec(),
            vec![6, 6, 6]
        );
    }

    #[test]
    fn transfer_round_trip_preserves_annotations() {
        let mut source = executor();
        let b = source.allocate_binary(vec![9, 8, 7]).unwrap();
        let carrier = Value::tuple(3, vec![Value::int(1)]);
        let annotated = carrier.annotated(2, Value::Binary(b)).unwrap();

        let (wire_value, heap_data) = source.extract_heap_data(&annotated).unwrap();
        assert_eq!(heap_data, vec![vec![9, 8, 7]]);

        let mut target = executor();
        let received = target.inject_heap_data(wire_value, &heap_data).unwrap();
        let Some(Value::Binary(Binary::Heap(new_idx))) = received.get_annotation(2).cloned() else {
            panic!("annotation lost or not a heap binary after transfer");
        };
        assert_eq!(
            target.get_heap_binary(new_idx).unwrap().to_vec(),
            vec![9, 8, 7]
        );
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
        tracker.tracking = Some(TrackingState {
            sampled: HashSet::new(),
            boundary_len: 1,
        });
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
