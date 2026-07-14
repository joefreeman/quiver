use crate::effects::Effect;
use crate::value::{ResourceId, Value};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;

/// An evaluation context in which routing and side-effecting operations are rejected,
/// because the enclosing evaluation may run more than once or must stay pure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RestrictedContext {
    /// A receive filter: evaluated against each candidate message, possibly repeatedly.
    ReceiveFunction,
    /// A tracked render (`%proc.track`): must stay pure and non-blocking.
    TrackedRender,
}

impl fmt::Display for RestrictedContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            RestrictedContext::ReceiveFunction => "receive function",
            RestrictedContext::TrackedRender => "tracked render",
        })
    }
}

pub type ProcessId = usize;

/// Result type containing a value with its heap data
pub type ProcessResult = Result<(Value, Vec<Vec<u8>>), crate::error::Error>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ProcessStatus {
    Active,
    Waiting,
    Sleeping,
    Failed,
    Completed,
}

/// Distinct heap slots (and their total bytes) reachable from some set of values.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct HeapUsage {
    pub slots: usize,
    pub bytes: usize,
}

/// A process's binary-heap footprint, broken down by root. The per-root figures count distinct
/// slots reachable from that root; `total` is distinct across *all* roots (including result/select/
/// awaiting), so the per-root figures may overlap each other and need not sum to it. Binaries
/// shared with other processes are included here too — this is "reachable from", not "owned by".
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct ProcessHeapUsage {
    pub stack: HeapUsage,
    pub locals: HeapUsage,
    pub mailbox: HeapUsage,
    pub total: HeapUsage,
}

/// A worker's executor snapshot, for the `\w` inspector. Heap slots are `live + free`; `live` is
/// reachable from a root, `free` are reclaimed-and-reusable, `pending` await the next reclamation.
/// `constant_*` is the share of the live heap pinned by the constant-binary cache.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct WorkerInfo {
    pub worker_id: u16,
    pub process_ids: Vec<ProcessId>,
    pub heap_slots: usize,
    pub live_slots: usize,
    pub free_slots: usize,
    pub pending_free: usize,
    pub reclaimed: usize,
    pub live_bytes: usize,
    pub total_bytes: usize,
    pub constant_slots: usize,
    pub constant_bytes: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessInfo {
    pub id: ProcessId,
    pub status: ProcessStatus,
    pub function_index: Option<usize>,
    pub stack_size: usize,
    pub locals_count: usize,
    pub frames_count: usize,
    pub mailbox_size: usize,
    pub persistent: bool,
    pub result: Option<ProcessResult>,
    pub heap: ProcessHeapUsage,
}

/// How a process participates in the reclamation graph.
/// A `Root` process is live or persistent: it is never swept, and the
/// pids it references seed the mark set. A `Tombstone` is a completed, non-persistent
/// process: a sweep candidate, kept only if reached from a root.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProcessCategory {
    Root,
    Tombstone,
}

/// One process's outgoing edges in the reclamation graph: the pids reachable from its
/// Value-bearing storage (a `Root`) or from its surviving `result`/`state` (a
/// `Tombstone`). Reported per worker and unioned centrally to trace live tombstones.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessAdjacency {
    pub pid: ProcessId,
    pub category: ProcessCategory,
    /// Pids this process references. May contain duplicates and self-references; the
    /// consumer dedups. Watchers are deliberately excluded (control-plane, no-op on a
    /// missing pid).
    pub outgoing: Vec<ProcessId>,
}

#[derive(Debug, Clone)]
pub enum Action<E: Effect> {
    /// Spawn a new process with the given function, captures, and argument
    Spawn {
        caller: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        argument: Value,
    },
    /// Deliver a message to a target process
    Deliver { target: ProcessId, value: Value },
    /// Request the results of target processes and/or arm stream-resource reads —
    /// everything a parking select needs routed. `arm` names owned stream resources
    /// whose next event should be read; each completion arrives back as a
    /// resource event (stashed on the caller, waking its select).
    Await {
        targets: Vec<ProcessId>,
        caller: ProcessId,
        arm: Vec<ResourceId>,
    },
    /// Request a platform-specific effect
    RequestEffect { process_id: ProcessId, effect: E },
    /// Read the current state of a process on another worker (`?` — a snapshot, not a wait).
    /// `subscribe` (a *tracked* sample) asks the target's worker to
    /// also register the caller as a reactive `Subscriber` as it serves the read — so
    /// read-and-subscribe is atomic and no state change is missed.
    ReadState {
        caller: ProcessId,
        target: ProcessId,
        subscribe: bool,
    },
    /// Kill a process (`%proc.kill` — fire-and-forget; the caller is not parked)
    Kill { target: ProcessId },
    /// Establish the target-side half of a link (`%proc.link` — the caller-side half
    /// was recorded at the call site; fire-and-forget)
    Link {
        caller: ProcessId,
        target: ProcessId,
    },
}

/// A stream resource's next event, as routed from the io backend to the owning
/// process's worker (the completion of an [`Action::Await`] `arm`). Deliberately
/// generic — which tuples these become is the stream kind's registry declaration.
/// `Data` bytes travel in the carrying command's heap side-channel.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StreamEvent {
    /// Bytes arrived (the payload is the command's heap entry).
    Data,
    /// The stream produced a fresh resource (e.g. an accepted connection). The
    /// environment has already recorded the receiving process as its owner.
    Resource { resource_id: ResourceId },
    /// The stream ended: EOF, or any error — a select answers events, not errno.
    End,
}

#[derive(Debug, Clone)]
pub struct Frame {
    pub function_index: usize,
    pub(crate) locals_base: usize,
    pub(crate) captures_count: usize,
    pub counter: usize,
}

impl Frame {
    pub fn new(function_index: usize, locals_base: usize, captures_count: usize) -> Self {
        Self {
            function_index,
            locals_base,
            captures_count,
            counter: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SelectState {
    /// The frame index where the select instruction is
    pub frame: usize,
    /// The instruction counter within that frame
    pub instruction: usize,
    /// The sources for this select (popped from stack)
    pub sources: Vec<Value>,
    /// Cursors for each receive source (indexed by receive function index)
    pub cursors: Vec<usize>,
    /// The clock time when select started evaluating sources (None until awaits complete)
    pub start_time: Option<u64>,
    /// The receive function being executed (index, message value), if any
    pub receiving: Option<(usize, Value)>,
}

/// A party to notify when the carrying process terminates. Registered on the *target*
/// (via `Executor::add_watcher`), so completion walks the target's own list instead of
/// scanning for interested parties. `Awaiter` is the only
/// kind today; ownership (owned children) and links ride the same list in later steps.
/// Entries may dangle — a notification aimed at a terminated watcher is a no-op — and
/// carry no `Value`s, so they are invisible to heap accounting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Watcher {
    /// `pid` awaits this process's result via a select: deliver the result (or the
    /// crash, as a `:crash`-stamped nil) when it terminates.
    Awaiter { pid: ProcessId },
    /// `pid` is an owned child of this process (containment-by-default spawning): kill
    /// it when this process terminates — any reason, like resource auto-close —
    /// cascading through the subtree. Removed by the parent-only `%proc.detach`. Not
    /// flushed when a *persistent* (REPL) process completes a submission — sleeping is
    /// not termination.
    OwnedChild { pid: ProcessId },
    /// `pid` is a link peer (`%proc.link` — symmetric fate-sharing: the entry sits in
    /// both processes' lists): kill it when this process terminates *abnormally*
    /// (crash or kill; a normal completion never propagates). Like `OwnedChild`, never
    /// flushed by a persistent process's per-line completion.
    Link { pid: ProcessId },
    /// `pid` is a reactive subscriber: deliver a `Changed`
    /// wakeup message whenever *this* process's data-plane state changes at a
    /// root-frame (tail-)entry. Unlike the other kinds, it fires **repeatedly** and is
    /// **not** consumed at termination — it is dropped (never turned into an event) by
    /// `flush_watchers`, and removed by reconciliation or reclamation. Registered by a
    /// tracked `?` sample (`%proc.track`); the target's `subscriber_count` mirrors how
    /// many of these it carries.
    Subscriber { pid: ProcessId },
}

impl Watcher {
    /// The process this watcher references. Used to drop stale entries when their target is
    /// reclaimed.
    pub fn pid(&self) -> ProcessId {
        match self {
            Watcher::Awaiter { pid }
            | Watcher::OwnedChild { pid }
            | Watcher::Link { pid }
            | Watcher::Subscriber { pid } => *pid,
        }
    }
}

/// A `%proc.track` render in progress. Set while the tracked thunk
/// runs; every `?` sample lands its target in `sampled`, and when the thunk's frame returns
/// the executor reconciles the process's subscriptions against it. `boundary_len` is the
/// `frames.len()` just after the thunk frame was pushed, so its return is recognised when a
/// popped frame had exactly that depth.
#[derive(Debug)]
pub struct TrackingState {
    pub sampled: HashSet<ProcessId>,
    pub boundary_len: usize,
}

#[derive(Debug)]
pub struct Process {
    pub stack: Vec<Value>,
    pub locals: Vec<Value>,
    pub frames: Vec<Frame>,
    pub mailbox: VecDeque<Value>,
    pub persistent: bool,
    pub result: Option<Result<Value, crate::error::Error>>,
    pub select_state: Option<SelectState>,
    pub awaiting: HashMap<ProcessId, Option<Value>>,
    /// Who to notify when this process terminates (see [`Watcher`]). Taken (emptied)
    /// exactly once, when the process completes.
    pub watchers: Vec<Watcher>,
    /// The observable state: the argument the root function was most recently
    /// (tail-)entered with — the spawn init, then each root-frame tail call. Sampled
    /// by `?`; persists after termination, like `result`.
    pub state: Value,
    /// How many `Watcher::Subscriber` entries this process carries.
    /// A cached count so `record_state` can decide whether to change-detect with one
    /// integer branch, never scanning `watchers` on the hot tail-call path. Maintained at
    /// subscribe/unsubscribe (and on reclamation pruning).
    pub subscriber_count: u32,
    /// The processes this one is currently subscribed to as a reactive tracker (the
    /// tracker-side record — the twin of `select_state.sources` for awaits). A reclamation
    /// edge: it pins a sampled process's tombstone so `?dep` keeps yielding its final
    /// state. Empty ⇒ no allocation. Reconciled by each `%proc.track` render.
    pub subscriptions: Vec<ProcessId>,
    /// Set while a `%proc.track` render runs (see [`TrackingState`]); `None` otherwise.
    pub tracking: Option<TrackingState>,
    /// Stream events that arrived while no select was waiting on their resource —
    /// one slot per resource, since at most one read is armed per stream. Consumed
    /// (in preference to arming) by the next select naming the resource, or by a
    /// plain read builtin.
    pub resource_events: HashMap<ResourceId, Value>,
    /// Stream resources with a next-event read armed at the io backend. Prevents
    /// double-arming across select re-entries; cleared as each event arrives.
    pub armed_resources: HashSet<ResourceId>,
}

impl Process {
    /// Whether this process is currently executing a receive function (select filter).
    /// A filter may be evaluated repeatedly, so it must stay pure: spawns, sends,
    /// selects, and effects are rejected while this is true.
    pub fn is_receiving(&self) -> bool {
        self.select_state
            .as_ref()
            .is_some_and(|s| s.receiving.is_some())
    }

    pub fn new(persistent: bool) -> Self {
        Self {
            stack: Vec::new(),
            locals: Vec::new(),
            frames: Vec::new(),
            mailbox: VecDeque::new(),
            persistent,
            result: None,
            select_state: None,
            awaiting: HashMap::new(),
            watchers: Vec::new(),
            state: Value::nil(),
            subscriber_count: 0,
            subscriptions: Vec::new(),
            tracking: None,
            resource_events: HashMap::new(),
            armed_resources: HashSet::new(),
        }
    }

    /// Whether this process is currently inside a `%proc.track` render — a restricted,
    /// re-evaluable context, like a receive filter.
    pub fn is_tracking(&self) -> bool {
        self.tracking.is_some()
    }

    /// The name of the restricted context this process is in, if any — a receive filter or
    /// a tracked render. Both must stay pure (they may be re-evaluated), so spawns, sends,
    /// effects, and selects are rejected inside them, reported against this name.
    pub fn restricted_context(&self) -> Option<RestrictedContext> {
        if self.is_receiving() {
            Some(RestrictedContext::ReceiveFunction)
        } else if self.is_tracking() {
            Some(RestrictedContext::TrackedRender)
        } else {
            None
        }
    }
}
