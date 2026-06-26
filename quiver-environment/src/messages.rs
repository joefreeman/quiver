use crate::WorkerId;
use crate::environment::{LocalsResult, ProcessResultsMap, ValueWithHeap};
use quiver_core::effects::Effect;
use quiver_core::executor::ProgramUpdate;
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::value::Value;
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
}

/// The data a worker pushes for a subscription. Variants correspond to [`SubscriptionKind`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SubscriptionPayload {
    ProcessStatuses(HashMap<ProcessId, ProcessStatus>),
    ProcessInfo(Option<ProcessInfo>),
}

/// Commands sent from Environment to Workers
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Command<E: Effect> {
    /// Update program data (additive only)
    UpdateProgram(ProgramUpdate),

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
        captures: Vec<Value>,
        argument: Value,
        heap_data: Vec<Vec<u8>>,
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
        message: Value,
        heap: Vec<Vec<u8>>,
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

    /// Request execution statistics
    GetExecutionStats { request_id: u64 },

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

    /// Effect operation completed
    EffectCompletion {
        process_id: ProcessId,
        result: Result<Value, String>,
        heap: Vec<Vec<u8>>,
    },

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
        captures: Vec<Value>,
        argument: Value,
        heap: Vec<Vec<u8>>,
    },

    /// Action: Deliver message
    DeliverAction {
        target: ProcessId,
        message: Value,
        heap: Vec<Vec<u8>>,
    },

    /// Action: Await multiple processes
    AwaitAction {
        awaiter: ProcessId,
        targets: Vec<ProcessId>,
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
        result: Result<ValueWithHeap, quiver_core::error::Error>,
        stats: Option<quiver_core::executor::ExecutionStats>,
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

    /// Response to GetExecutionStats
    StatsResponse {
        request_id: u64,
        result: Result<quiver_core::executor::ExecutionStats, crate::environment::EnvironmentError>,
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

    // Phantom data to maintain generic parameter
    #[serde(skip)]
    _Phantom(std::marker::PhantomData<E>),
}
