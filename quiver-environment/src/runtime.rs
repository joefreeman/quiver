use quiver_core::bytecode::{Constant, Function, Instruction, TypeId};
use quiver_core::error::Error;
use quiver_core::process::{ProcessId, ProcessStatus};
use quiver_core::types::TupleTypeInfo;
use quiver_core::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Commands sent to executor threads/workers
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SchedulerCommand {
    Execute {
        request_id: u64,
        process_id: ProcessId,
        instructions: Vec<Instruction>,
        persistent: bool,
    },
    Wake {
        request_id: u64,
        process_id: ProcessId,
        instructions: Vec<Instruction>,
    },
    GetProcesses {
        request_id: u64,
    },
    GetProcess {
        request_id: u64,
        id: ProcessId,
    },
    GetLocals {
        request_id: u64,
        process_id: ProcessId,
        indices: Vec<usize>,
    },
    GetResult {
        request_id: u64,
        process_id: ProcessId,
    },
    CompactLocals {
        request_id: u64,
        process_id: ProcessId,
        referenced_indices: Vec<usize>,
    },
    UpdateProgram {
        constants: Vec<Constant>,
        functions: Vec<Function>,
        builtins: Vec<String>,
        types: Vec<(TypeId, TupleTypeInfo)>,
    },
    NotifySpawn {
        process_id: ProcessId,
        pid_value: Value,
        heap_data: Vec<Vec<u8>>,
    },
    NotifyMessage {
        process_id: ProcessId,
        message: Value,
        heap_data: Vec<Vec<u8>>,
    },
    NotifyResult {
        process_id: ProcessId,
        result: Value,
        heap_data: Vec<Vec<u8>>,
    },
    Spawn {
        process_id: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap_data: Vec<Vec<u8>>,
    },
    Shutdown,
}

/// Events returned from executor threads/workers
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Event {
    ExecuteResponse {
        request_id: u64,
        result: Result<ProcessId, Error>,
    },
    WakeResponse {
        request_id: u64,
        result: Result<(), Error>,
    },
    GetProcessesResponse {
        request_id: u64,
        result: Result<HashMap<ProcessId, ProcessStatus>, Error>,
    },
    GetProcessResponse {
        request_id: u64,
        result: Result<Option<quiver_core::process::ProcessInfo>, Error>,
    },
    GetLocalsResponse {
        request_id: u64,
        result: Result<Vec<Value>, Error>,
    },
    GetResultResponse {
        request_id: u64,
        result: Result<Value, Error>,
        heap_data: Vec<Vec<u8>>,
    },
    CompactLocalsResponse {
        request_id: u64,
        result: Result<(), Error>,
    },
    SpawnRequested {
        caller: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap_data: Vec<Vec<u8>>,
    },
    DeliverMessage {
        target: ProcessId,
        value: Value,
        heap_data: Vec<Vec<u8>>,
    },
    AwaitResult {
        target: ProcessId,
        caller: ProcessId,
    },
    ProcessCompleted {
        process_id: ProcessId,
        result: Value,
        heap_data: Vec<Vec<u8>>,
    },
}

/// Trait for sending commands to executors.
/// Implementations should be cheap to clone for easy sharing.
pub trait CommandSender {
    /// Send a command to a specific executor
    fn send_command(&mut self, executor_id: usize, command: SchedulerCommand) -> Result<(), Error>;
}
