use crate::WorkerId;
use crate::environment::{LocalsResult, ProcessResultsMap};
use quiver_core::effects::Effect;
use quiver_core::executor::ProgramUpdate;
use quiver_core::process::{
    ProcessAdjacency, ProcessId, ProcessInfo, ProcessStatus, RegistryRequest, StreamEvent,
    WorkerInfo,
};
use quiver_core::value::ResourceId;
use quiver_core::wire::WireValue;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// What a standing subscription observes. Unlike the one-shot `Get*` requests, a worker keeps a
/// subscription registered and re-pushes a [`SubscriptionPayload`] whenever the observed state
/// changes (see `Worker::flush_subscriptions`). New observable kinds are added here.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SubscriptionKind {
    /// All process statuses owned by the worker.
    ProcessStatuses,
    /// Detailed info for a single process (stats, mailbox/heap sizes, result).
    ProcessInfo { process_id: ProcessId },
    /// The worker's executor snapshot (heap/memory stats and owned process ids).
    WorkerInfo,
}

/// The data a worker pushes for a subscription. Variants correspond to [`SubscriptionKind`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SubscriptionPayload {
    ProcessStatuses(HashMap<ProcessId, ProcessStatus>),
    ProcessInfo(Option<ProcessInfo>),
    WorkerInfo(WorkerInfo),
}

/// Commands sent from Environment to Workers
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Command<E: Effect> {
    /// Update program data (additive only)
    UpdateProgram(Box<ProgramUpdate>),

    /// Start a new persistent process (e.g., from REPL)
    /// If function_index is None, the process starts in a sleeping state ready for resume
    StartProcess {
        id: ProcessId,
        function_index: Option<usize>,
    },

    /// Spawn a new process (from another process)
    SpawnProcess {
        id: ProcessId,
        function_index: usize,
        captures: Vec<WireValue>,
        argument: WireValue,
    },

    /// Resume a sleeping persistent process
    ResumeProcess {
        id: ProcessId,
        function_index: usize,
    },

    /// Query process states and register as awaiter
    /// Worker should check each target, respond with current states,
    /// and register awaiter for incomplete processes
    QueryAndAwait {
        awaiter: ProcessId,
        targets: Vec<ProcessId>,
    },

    /// Update awaiter with process results (initial snapshot or later completions)
    /// None means process not yet completed, Some means completed with result
    UpdateAwaitResults {
        awaiter: ProcessId,
        results: ProcessResultsMap,
    },

    /// Deliver a message to a process
    DeliverMessage {
        target: ProcessId,
        message: WireValue,
    },

    /// Notify a process with the PID of a spawned process
    NotifySpawn {
        process_id: ProcessId,
        spawned_pid: ProcessId,
        function_index: usize,
    },

    /// Request process result.
    /// `keep_locals`, when present, is the REPL's keep-set: on successful completion the worker
    /// releases every local *not* listed (the finished line's parameter and temporaries), reclaiming
    /// their heap in place without re-indexing. `None` for non-REPL one-shot requests, which keep
    /// all locals.
    GetResult {
        request_id: u64,
        process_id: ProcessId,
        keep_locals: Option<Vec<usize>>,
    },

    /// Request all process statuses
    GetStatuses { request_id: u64 },

    /// Request worker info (memory/heap snapshot)
    GetWorkerInfo { request_id: u64 },

    /// Request all process types (for REPL process references)
    GetProcessTypes { request_id: u64 },

    /// Request process info
    GetProcessInfo {
        request_id: u64,
        process_id: ProcessId,
    },

    /// Request process locals
    GetLocals {
        request_id: u64,
        process_id: ProcessId,
        indices: Vec<usize>,
    },

    /// Compact process locals
    CompactLocals {
        process_id: ProcessId,
        keep_indices: Vec<usize>,
    },

    /// Install a standing subscription. The worker pushes a `SubscriptionUpdate` immediately (the
    /// initial snapshot) and again on every subsequent change until unsubscribed. The same
    /// `subscription_id` is used across all workers a subscription fans out to.
    Subscribe {
        subscription_id: u64,
        kind: SubscriptionKind,
    },

    /// Remove a standing subscription. Broadcast to all workers; a worker without that
    /// subscription simply ignores it.
    Unsubscribe { subscription_id: u64 },

    /// Kill a process — containment teardown of a terminated parent's subtree, an
    /// explicit `%proc.kill`, or link propagation. The worker records `Killed` as its
    /// result and tombstones it, cascading to its own watchers.
    KillProcess { id: ProcessId },

    /// Stop a host-started (persistent) process — the host's session-teardown verb
    /// (REPL interrupt and reset), which `KillProcess` deliberately refuses. The worker
    /// clears the persistence flag and terminates the process whether running or
    /// sleeping; the tombstone's watcher flush tears down its owned subtree.
    StopProcess { id: ProcessId },

    /// Establish the target-side half of a link (`%proc.link`): kill `peer` when
    /// `target` terminates abnormally. An already-crashed target kills `peer`
    /// immediately; a normally-completed one makes this a no-op.
    LinkProcess { target: ProcessId, peer: ProcessId },

    /// Effect operation completed. A failure is classified rather than stringly typed: an
    /// `Expected` outcome resumes the process with a `:error`-stamped nil, a `Fault`
    /// terminates it.
    EffectCompletion {
        process_id: ProcessId,
        result: Result<WireValue, quiver_core::effects::EffectFailure>,
    },

    /// A stream resource's next event (the completion of a select-armed read):
    /// stash it on the owning process and wake its select.
    ResourceEvent {
        process_id: ProcessId,
        resource_id: ResourceId,
        resource_type: usize,
        event: StreamEvent,
        /// A `Data` event's bytes; empty for the other kinds. A stream event is not a
        /// value, so it carries its payload directly rather than through a wire value.
        bytes: Vec<u8>,
    },

    /// Read a process's current state on behalf of a remote `?` sample (a snapshot —
    /// the target is not disturbed and resource ownership does not transfer). When
    /// `subscribe` (a *tracked* sample), also register `caller` as a
    /// reactive `Subscriber` of `target` — atomically with the read, on the target's worker.
    ReadState {
        caller: ProcessId,
        target: ProcessId,
        subscribe: bool,
    },
    /// Remove `subscriber`'s reactive subscription from `target` (reconciliation dropped the
    /// dependency). Fire-and-forget; a no-op on an unknown target.
    UnsubscribeState {
        target: ProcessId,
        subscriber: ProcessId,
    },

    /// Deliver a remote `?` sample to the caller that requested it. Also the reply
    /// path for `%registry` operations, whose parked caller resumes on the same
    /// value push.
    NotifyState {
        process_id: ProcessId,
        state: WireValue,
    },

    /// Install a `Watcher::Registered` on `target`, on behalf of a pending
    /// `%registry.register` — the flush of that watcher at termination is what frees
    /// the name. Installation doubles as the liveness check: an already-terminated
    /// target refuses, and the registration answers nil. The pending entry rides
    /// along and is echoed back in `RegistryWatched`, keeping the environment's
    /// handling stateless.
    WatchProcess {
        target: ProcessId,
        function_index: usize,
        key: String,
        caller: ProcessId,
    },

    /// Phase 1 of a reclamation round: pause
    /// stepping so the worker produces no new cross-worker traffic, then ack with
    /// `CollectionReady`. The worker keeps draining commands while paused. FIFO channels
    /// make the ack a barrier — once every worker has acked, every pre-pause send has
    /// already been routed by the environment.
    BeginCollection { request_id: u64 },

    /// Code phase (after `Reclaim`, still paused): report this worker's `code_roots()`
    /// — the function/constant indices its surviving processes keep alive — as a
    /// `CodeRootsResponse`. FIFO puts this after `Reclaim`, so swept processes are
    /// already gone from the walk.
    CollectCodeRoots { request_id: u64 },

    /// Phase 2: report this worker's `process_adjacency()` as an `AdjacencyResponse`.
    /// Sent only after all workers are paused, so any in-flight message has landed in a
    /// mailbox (FIFO: the routed `DeliverMessage` precedes this command).
    CollectAdjacency { request_id: u64 },

    /// Phase 3: reclaim these tombstones (remove the entries, release their result/state
    /// heap refs). Fire-and-forget; the environment has already proven them unreachable.
    Reclaim { pids: Vec<ProcessId> },

    /// Phase 4: resume stepping. Fire-and-forget.
    EndCollection,

    // Phantom data to maintain generic parameter
    #[serde(skip)]
    _Phantom(std::marker::PhantomData<E>),
}

/// Events sent from Workers to Environment
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Event<E: Effect> {
    /// Action: Spawn new process
    SpawnAction {
        caller: ProcessId,
        function_index: usize,
        captures: Vec<WireValue>,
        argument: WireValue,
    },

    /// Action: Deliver message
    DeliverAction {
        target: ProcessId,
        message: WireValue,
    },

    /// Action: Await multiple processes
    AwaitAction {
        awaiter: ProcessId,
        targets: Vec<ProcessId>,
    },

    /// Action: arm a stream resource's next-event read for a parked select. The
    /// completion returns as a `Command::ResourceEvent` to the resource's owner.
    ArmStreamAction {
        caller: ProcessId,
        resource_id: ResourceId,
    },

    /// Action: kill a process (containment teardown, `%proc.kill`, or link
    /// propagation; routed like every cross-process effect, and the point where its
    /// resources are freed)
    KillAction { target: ProcessId },

    /// Action: establish the target-side half of a link (`%proc.link` — the
    /// caller-side half was recorded at the call site)
    LinkAction {
        caller: ProcessId,
        target: ProcessId,
    },

    /// Process results (initial snapshot or later completions)
    /// None means process not yet completed, Some means completed with result
    ProcessResults {
        awaiter: ProcessId,
        results: ProcessResultsMap,
    },

    /// Response to GetResult (only sent when process completes)
    ResultResponse {
        request_id: u64,
        result: Result<WireValue, quiver_core::error::Error>,
    },

    /// Response to GetStatuses
    StatusesResponse {
        request_id: u64,
        result: Result<HashMap<ProcessId, ProcessStatus>, crate::environment::EnvironmentError>,
    },

    /// Response to GetWorkerInfo
    WorkerInfoResponse {
        request_id: u64,
        result: Result<quiver_core::process::WorkerInfo, crate::environment::EnvironmentError>,
    },

    /// Response to GetProcessTypes (returns function indices, Environment reconstructs types)
    ProcessTypesResponse {
        request_id: u64,
        result: Result<HashMap<ProcessId, usize>, crate::environment::EnvironmentError>,
    },

    /// Response to GetProcessInfo
    InfoResponse {
        request_id: u64,
        result: Result<Option<ProcessInfo>, crate::environment::EnvironmentError>,
    },

    /// Response to GetLocals
    LocalsResponse {
        request_id: u64,
        result: LocalsResult,
    },

    /// Push for a standing subscription: the worker's current view of the observed state. Carries
    /// `worker_id` so the environment can merge updates from the several workers a `ProcessStatuses`
    /// subscription fans out to.
    SubscriptionUpdate {
        subscription_id: u64,
        worker_id: WorkerId,
        payload: SubscriptionPayload,
    },

    /// Worker encountered an unrecoverable error
    WorkerError {
        error: crate::environment::EnvironmentError,
    },

    /// Request effect operation from Environment
    EffectRequest { process_id: ProcessId, effect: E },

    /// Action: read a remote process's state (`?` sample). `subscribe` carries the tracked
    /// sample's request to also subscribe the caller.
    ReadStateAction {
        caller: ProcessId,
        target: ProcessId,
        subscribe: bool,
    },
    /// Action: remove a reactive subscription from a remote target (reconciliation dropped
    /// the dependency). Routed to the target's worker.
    UnsubscribeAction {
        target: ProcessId,
        subscriber: ProcessId,
    },

    /// A `?` sample read on the target's worker, headed back to the caller
    StateRead { caller: ProcessId, state: WireValue },

    /// Action: a `%registry` operation, headed to the environment's name table. The
    /// caller is parked; the environment answers with a `NotifyState` value push.
    RegistryAction {
        caller: ProcessId,
        request: RegistryRequest,
    },

    /// Reply to `WatchProcess`: whether the watcher was installed (`alive`), echoing
    /// the pending registration for the environment to commit or refuse.
    RegistryWatched {
        target: ProcessId,
        function_index: usize,
        key: String,
        caller: ProcessId,
        alive: bool,
    },

    /// A registered process terminated (its `Watcher::Registered` flushed): free
    /// every name bound to it.
    RegistryExpired { pid: ProcessId },

    /// Ack for `BeginCollection`: this worker is paused.
    CollectionReady {
        request_id: u64,
        worker_id: WorkerId,
    },

    /// Response to `CollectCodeRoots`: the code this worker's processes keep alive.
    CodeRootsResponse {
        request_id: u64,
        worker_id: usize,
        functions: Vec<usize>,
        constants: Vec<usize>,
    },

    /// Response to `CollectAdjacency`: this worker's slice of the reclamation graph.
    AdjacencyResponse {
        request_id: u64,
        worker_id: WorkerId,
        adjacency: Vec<ProcessAdjacency>,
    },

    // Phantom data to maintain generic parameter
    #[serde(skip)]
    _Phantom(std::marker::PhantomData<E>),
}
