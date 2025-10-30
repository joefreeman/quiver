use crate::environment::{LocalsResult, ProcessResultsMap, ValueWithHeap};
use quiver_core::bytecode::{BuiltinInfo, Constant, Function};
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::types::{TupleTypeInfo, Type};
use quiver_core::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Commands sent from Environment to Workers
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Command {
    /// Update program data (additive only)
    UpdateProgram {
        constants: Vec<Constant>,
        functions: Vec<Function>,
        tuples: Vec<TupleTypeInfo>,
        types: Vec<Type>,
        builtins: Vec<BuiltinInfo>,
    },

    /// Start a new persistent process (e.g., from REPL)
    StartProcess {
        id: ProcessId,
        function_index: usize,
    },

    /// Spawn a new process (from another process)
    SpawnProcess {
        id: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
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

    /// Request process result
    GetResult {
        request_id: u64,
        process_id: ProcessId,
    },

    /// Request all process statuses
    GetStatuses { request_id: u64 },

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
}

/// Events sent from Workers to Environment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Event {
    /// Action: Spawn new process
    SpawnAction {
        caller: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
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
    },

    /// Response to GetStatuses
    StatusesResponse {
        request_id: u64,
        result: Result<HashMap<ProcessId, ProcessStatus>, crate::environment::EnvironmentError>,
    },

    /// Response to GetProcessTypes
    ProcessTypesResponse {
        request_id: u64,
        result: Result<HashMap<ProcessId, (Type, usize)>, crate::environment::EnvironmentError>,
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

    /// Worker encountered an unrecoverable error
    WorkerError {
        error: crate::environment::EnvironmentError,
    },
}
