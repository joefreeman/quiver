use crate::effects::Effect;
use crate::value::{ResourceId, Value};
use serde::{Deserialize, Serialize};
use std::collections::{HashSet, VecDeque};
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
/// A completed process's result, in the form it leaves the worker in (see
/// [`crate::wire`]). The process's own `result` field holds a live `Value`; this is what a
/// requester receives.
pub type ProcessResult = Result<crate::wire::WireValue, crate::error::Error>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ProcessStatus {
    Active,
    Waiting,
    Sleeping,
    Failed,
    Completed,
}

/// Distinct binary buffers (and their total bytes) reachable from some set of values.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct HeapUsage {
    pub binaries: usize,
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

/// A worker's executor snapshot, for the `\w` inspector. Everything here is measured by walking
/// this worker's processes: a binary is owned by the values that reference it, so there is no
/// table to report occupancy of, and figures are deduplicated by buffer identity.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct WorkerInfo {
    pub worker_id: u16,
    pub process_ids: Vec<ProcessId>,
    /// Distinct binary buffers reachable from this worker's processes, and their total bytes.
    /// Counted by identity, so a buffer shared between processes appears once.
    pub live_binaries: usize,
    pub live_bytes: usize,
    /// How many of those are unrealised ropes, and the deepest. Every read of a rope realises
    /// it, and nothing caches the result, so a rising depth is the cue that a binary is being
    /// built by repeated append and read repeatedly.
    pub rope_binaries: usize,
    pub max_rope_depth: usize,
    /// Bytes in buffers whose allocation has more than one holder — another value here, or one
    /// on another worker, since a send passes the handle rather than the bytes.
    pub shared_bytes: usize,
    pub constant_binaries: usize,
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
    /// Kill a process (`%proc.kill` — fire-and-forget; the caller is not parked), with the
    /// data value its awaiters see as the `Killed` reason
    Kill {
        target: ProcessId,
        reason: Option<Value>,
    },
    /// Establish the target-side half of a link (`%proc.link` — the caller-side half
    /// was recorded at the call site; fire-and-forget)
    Link {
        caller: ProcessId,
        target: ProcessId,
    },
    /// A name-registry operation (`%registry`). The caller parks until the
    /// environment — which owns the name table — answers with a value push.
    Registry {
        caller: ProcessId,
        request: RegistryRequest,
    },
}

/// One `%registry` operation, as carried from the calling worker to the environment.
/// Keys travel pre-canonicalised as data-notation text: encoding at the call site
/// rejects identity-bearing values and folds away representation differences
/// (constant vs heap binaries, annotations), so the environment compares plain strings.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RegistryRequest {
    /// Bind `key` to the process, provided the key is free and the process still
    /// lives. `function_index` is the pid's root function, kept for lookup type tests.
    Register {
        key: String,
        pid: ProcessId,
        function_index: usize,
    },
    /// Remove `key`. Answers `Ok`, or nil when the key was not bound.
    Unregister { key: String },
    /// Answer the pid bound to `key` at the process type `expected_type` names,
    /// or nil when the key is unbound or the process fails the type test.
    Lookup { key: String, expected_type: usize },
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
    /// The stream ended cleanly: EOF, a FIN, a close_notify. Nothing more will
    /// arrive, and everything that did arrive is the whole of it.
    End,
    /// The stream failed: a reset, a truncation, an aborted transfer. The select
    /// answers the same `:error IoError[…]`-stamped nil every failed I/O operation
    /// does, so a consumer that would recover must first look — completeness is
    /// exactly what a failure no longer promises.
    Failed { error: crate::effects::EffectError },
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
    /// The select instruction's index within that frame — where the frame's counter is
    /// put back while the select is unfinished, so that it runs again.
    pub instruction: usize,
    /// The sources for this select (popped from stack)
    pub sources: Vec<Value>,
    /// Cursors for each receive source (indexed by receive function index)
    pub cursors: Vec<usize>,
    /// The clock time when select started evaluating sources (None until awaits complete)
    pub start_time: Option<u64>,
    /// The receive function being executed (index, message value), if any
    pub receiving: Option<(usize, Value)>,
    /// Results delivered for this select's process sources: `None` while an await is in
    /// flight, `Some(result)` once it answers.
    ///
    /// It lives *here*, not on the process, because that is exactly its lifetime — a result
    /// is carried from the environment to the select that asked for it, and nothing may read
    /// one afterwards (`initialize_select` overwrites each target's slot before every await,
    /// and a repeat `!p` re-fetches from the target's tombstone). Hanging it off `Process`
    /// instead meant entries outlived their select, which both leaked every completed await's
    /// result and left the map unbounded; here `complete_select`'s `take()` disposes of them
    /// and the invariant needs no upkeep.
    pub awaited: AwaitedResults,
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
    /// The environment's name registry holds an entry for this process: notify the
    /// environment at termination so the name is freed. Installed by `%registry.register`
    /// (and, like the ownership kinds, not flushed by a persistent process's per-line
    /// completion). References no process, so it never dangles and is never pruned.
    Registered,
}

impl Watcher {
    /// The process this watcher references, if any. Used to drop stale entries when
    /// their target is reclaimed.
    pub fn pid(&self) -> Option<ProcessId> {
        match self {
            Watcher::Awaiter { pid }
            | Watcher::OwnedChild { pid }
            | Watcher::Link { pid }
            | Watcher::Subscriber { pid } => Some(*pid),
            Watcher::Registered => None,
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

/// Delivered await results, keyed by the process they came from — an association list.
///
/// Bounded by the *active select's* process sources, which is one for a plain `!p` and a
/// handful for a race: entries are created only for a select that is waiting on that target,
/// and cleared when the select completes. (They were once left behind, which both leaked the
/// result values and made this unbounded — a linear scan would have been a quadratic then.)
#[derive(Debug, Default, Clone)]
pub struct AwaitedResults(Vec<(ProcessId, Option<Value>)>);

impl AwaitedResults {
    /// Register or deliver a result for `process`, returning what it displaces.
    pub fn insert(&mut self, process: ProcessId, result: Option<Value>) -> Option<Option<Value>> {
        match self.0.iter_mut().find(|(id, _)| *id == process) {
            Some((_, slot)) => Some(std::mem::replace(slot, result)),
            None => {
                self.0.push((process, result));
                None
            }
        }
    }

    pub fn get(&self, process: &ProcessId) -> Option<&Option<Value>> {
        self.0.iter().find(|(id, _)| id == process).map(|(_, r)| r)
    }

    pub fn contains_key(&self, process: &ProcessId) -> bool {
        self.0.iter().any(|(id, _)| id == process)
    }

    pub fn values(&self) -> impl Iterator<Item = &Option<Value>> {
        self.0.iter().map(|(_, result)| result)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}

/// Stashed stream events, keyed by resource — an association list, not a map.
///
/// A process holds at most one entry per resource it owns, and owning more than a handful is
/// already unusual, so a linear scan beats hashing outright at this size. The reason it is
/// worth the swap is `Process`'s footprint rather than speed: a `HashMap` is 48 bytes inline
/// and a `Vec` 24, on a struct every live process pays for.
#[derive(Debug, Default)]
pub struct ResourceEvents(Vec<(ResourceId, Value)>);

impl ResourceEvents {
    /// Stash `value` for `resource`, returning any event it displaces.
    pub fn insert(&mut self, resource: ResourceId, value: Value) -> Option<Value> {
        match self.0.iter_mut().find(|(id, _)| *id == resource) {
            Some((_, slot)) => Some(std::mem::replace(slot, value)),
            None => {
                self.0.push((resource, value));
                None
            }
        }
    }

    pub fn remove(&mut self, resource: &ResourceId) -> Option<Value> {
        let index = self.0.iter().position(|(id, _)| id == resource)?;
        Some(self.0.swap_remove(index).1)
    }

    pub fn contains_key(&self, resource: &ResourceId) -> bool {
        self.0.iter().any(|(id, _)| id == resource)
    }

    pub fn values(&self) -> impl Iterator<Item = &Value> {
        self.0.iter().map(|(_, value)| value)
    }
}

/// The resources with a next-event read armed, as an association list. Same size argument as
/// [`ResourceEvents`], and bounded by the same thing — the resources one process owns.
#[derive(Debug, Default)]
pub struct ArmedResources(Vec<ResourceId>);

impl ArmedResources {
    /// Arm `resource`, answering whether it was newly armed (as `HashSet::insert` does) — the
    /// caller uses that to avoid double-arming across select re-entries.
    pub fn insert(&mut self, resource: ResourceId) -> bool {
        if self.0.contains(&resource) {
            return false;
        }
        self.0.push(resource);
        true
    }

    pub fn remove(&mut self, resource: &ResourceId) -> bool {
        match self.0.iter().position(|id| id == resource) {
            Some(index) => {
                self.0.swap_remove(index);
                true
            }
            None => false,
        }
    }

    pub fn contains(&self, resource: &ResourceId) -> bool {
        self.0.contains(resource)
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}

/// What a parked process waits for. Each is answered by one notification, which resumes
/// the process past the instruction that parked it — except a select, which runs again.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Wait {
    /// A value the environment answers with: a spawned process's pid, a remote `?` sample,
    /// or the reply to a routed request.
    Reply,
    /// Anything its select may complete on: a message, an awaited result, a stream event,
    /// or its timeout.
    Select,
    /// The result of an effect the host performs.
    Effect,
}

#[derive(Debug)]
pub struct Process {
    pub stack: Vec<Value>,
    pub locals: Vec<Value>,
    pub frames: Vec<Frame>,
    pub mailbox: VecDeque<Value>,
    pub persistent: bool,
    /// What the process is parked on, if anything; the matching notification clears it and
    /// queues the process to run.
    pub(crate) wait: Option<Wait>,
    /// Whether the process is in its executor's run queue. The queue holds a process at
    /// most once, and taking one out only clears this — the entry left behind is skipped
    /// when it reaches the front.
    pub(crate) queued: bool,
    /// The completed process's outcome. The error arm is **boxed**: `Error` is 48 bytes and a
    /// crash is the rare case, so inline it made `result` tie for the largest field in a struct
    /// every live process pays for.
    pub result: Option<Result<Value, Box<crate::error::Error>>>,
    /// Boxed: `SelectState` is 112 bytes and almost every process is `None` here, so inline
    /// it would be the single largest field in `Process` and paid by every process that never
    /// selects.
    pub select_state: Option<Box<SelectState>>,
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
    /// Boxed, for the same reason as `select_state`: 56 bytes inline, `None` in every
    /// process that is not mid-`%proc.track` render.
    pub tracking: Option<Box<TrackingState>>,
    /// Stream events that arrived while no select was waiting on their resource —
    /// one slot per resource, since at most one read is armed per stream. Consumed
    /// (in preference to arming) by the next select naming the resource, or by a
    /// plain read builtin.
    pub resource_events: ResourceEvents,
    /// Stream resources with a next-event read armed at the io backend. Prevents
    /// double-arming across select re-entries; cleared as each event arrives.
    pub armed_resources: ArmedResources,
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
            wait: None,
            queued: false,
            result: None,
            select_state: None,
            watchers: Vec::new(),
            state: Value::nil(),
            subscriber_count: 0,
            subscriptions: Vec::new(),
            tracking: None,
            resource_events: ResourceEvents::default(),
            armed_resources: ArmedResources::default(),
        }
    }

    /// Register `subscriber` as a reactive subscriber of this process (a tracked `?`
    /// sample), keeping `subscriber_count` in step. Idempotent — one entry per subscriber —
    /// and a no-op once the process has terminated: its state can no longer change, so a
    /// subscription would never fire.
    pub(crate) fn add_subscriber(&mut self, subscriber: ProcessId) {
        let entry = Watcher::Subscriber { pid: subscriber };
        if self.result.is_none() && !self.watchers.contains(&entry) {
            self.watchers.push(entry);
            self.subscriber_count += 1;
        }
    }

    /// Remove `subscriber`'s reactive subscription to this process, keeping
    /// `subscriber_count` in step. No-op if absent.
    pub(crate) fn remove_subscriber(&mut self, subscriber: ProcessId) {
        let entry = Watcher::Subscriber { pid: subscriber };
        let before = self.watchers.len();
        self.watchers.retain(|watcher| *watcher != entry);
        self.subscriber_count -= (before - self.watchers.len()) as u32;
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
