use quiver_core::bytecode::{Constant, Function};
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::types::TupleTypeInfo;
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
        types: Vec<TupleTypeInfo>,
        builtins: Vec<String>,
    },

    /// Start a new process
    StartProcess {
        id: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap_data: Vec<Vec<u8>>,
        persistent: bool,
    },

    /// Resume a sleeping persistent process
    ResumeProcess {
        id: ProcessId,
        function_index: usize,
    },

    /// Register that a process is awaiting another process
    RegisterAwaiter {
        target: ProcessId,
        awaiter: ProcessId,
    },

    /// Notify a process with a result it was awaiting
    NotifyResult {
        process_id: ProcessId,
        result: Result<Value, quiver_core::error::Error>,
        heap: Vec<Vec<u8>>,
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
    },

    /// Request process result
    GetResult {
        request_id: u64,
        process_id: ProcessId,
    },

    /// Request all process statuses
    GetStatuses { request_id: u64 },

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
    /// Process completed with result
    Completed {
        process_id: ProcessId,
        result: Result<Value, quiver_core::error::Error>,
        heap: Vec<Vec<u8>>,
    },

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

    /// Action: Await result
    AwaitAction {
        caller: ProcessId,
        target: ProcessId,
    },

    /// Response to GetResult (only sent when process completes)
    ResultResponse {
        request_id: u64,
        result: Result<(Value, Vec<Vec<u8>>), quiver_core::error::Error>,
    },

    /// Response to GetStatuses
    StatusesResponse {
        request_id: u64,
        result: Result<HashMap<ProcessId, ProcessStatus>, crate::environment::EnvironmentError>,
    },

    /// Response to GetProcessInfo
    InfoResponse {
        request_id: u64,
        result: Result<Option<ProcessInfo>, crate::environment::EnvironmentError>,
    },

    /// Response to GetLocals
    LocalsResponse {
        request_id: u64,
        result: Result<Vec<(Value, Vec<Vec<u8>>)>, crate::environment::EnvironmentError>,
    },

    /// Worker encountered an unrecoverable error
    WorkerError {
        error: crate::environment::EnvironmentError,
    },
}
