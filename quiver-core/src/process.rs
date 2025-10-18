use crate::bytecode::Instruction;
use crate::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;

pub type ProcessId = usize;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ProcessStatus {
    Active,
    Waiting,
    Sleeping,
    Terminated,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessInfo {
    pub id: ProcessId,
    pub status: ProcessStatus,
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
    /// Request the result of a target process
    AwaitResult {
        target: ProcessId,
        caller: ProcessId,
    },
}

#[derive(Debug, Clone)]
pub struct Frame {
    pub instructions: Vec<Instruction>,
    pub(crate) locals_base: usize,
    pub(crate) captures_count: usize,
    pub counter: usize,
}

impl Frame {
    pub fn new(instructions: Vec<Instruction>, locals_base: usize, captures_count: usize) -> Self {
        Self {
            instructions,
            locals_base,
            captures_count,
            counter: 0,
        }
    }
}

#[derive(Debug)]
pub struct Process {
    pub stack: Vec<Value>,
    pub locals: Vec<Value>,
    pub frames: Vec<Frame>,
    pub mailbox: VecDeque<Value>,
    pub persistent: bool,
    pub cursor: usize,
    pub result: Option<Result<Value, crate::error::Error>>,
}

impl Process {
    pub fn new(persistent: bool) -> Self {
        Self {
            stack: Vec::new(),
            locals: Vec::new(),
            frames: Vec::new(),
            mailbox: VecDeque::new(),
            persistent,
            cursor: 0,
            result: None,
        }
    }
}
