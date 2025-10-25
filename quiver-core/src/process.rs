use crate::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};

pub type ProcessId = usize;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ProcessStatus {
    Active,
    Waiting,
    Sleeping,
    Failed,
    Completed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessInfo {
    pub id: ProcessId,
    pub status: ProcessStatus,
    pub function_index: usize,
    pub stack_size: usize,
    pub locals_size: usize,
    pub frames_count: usize,
    pub mailbox_size: usize,
    pub persistent: bool,
    pub result: Option<Result<Value, crate::error::Error>>,
}

#[derive(Debug, Clone)]
pub enum Action {
    /// Spawn a new process with the given function and captures
    Spawn {
        caller: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
    },
    /// Deliver a message to a target process
    Deliver { target: ProcessId, value: Value },
    /// Request the result of one or more target processes
    Await {
        targets: Vec<ProcessId>,
        caller: ProcessId,
    },
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

#[derive(Debug)]
pub struct Process {
    pub function_index: usize,
    pub stack: Vec<Value>,
    pub locals: Vec<Value>,
    pub frames: Vec<Frame>,
    pub mailbox: VecDeque<Value>,
    pub persistent: bool,
    pub result: Option<Result<Value, crate::error::Error>>,
    pub select_state: Option<SelectState>,
    pub awaiting: HashMap<ProcessId, Option<Value>>,
}

impl Process {
    pub fn new(function_index: usize, persistent: bool) -> Self {
        Self {
            function_index,
            stack: Vec::new(),
            locals: Vec::new(),
            frames: Vec::new(),
            mailbox: VecDeque::new(),
            persistent,
            result: None,
            select_state: None,
            awaiting: HashMap::new(),
        }
    }
}
