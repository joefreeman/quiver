use crate::bytecode::Instruction;
use crate::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProcessId(pub usize);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ProcessStatus {
    Running,
    Queued,
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
    pub result: Option<Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StepResult {
    /// More work remains - processes are ready to execute
    Running,
    /// All processes are waiting for messages/events
    Idle,
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
    pub id: ProcessId,
    pub stack: Vec<Value>,
    pub locals: Vec<Value>,
    pub frames: Vec<Frame>,
    pub mailbox: VecDeque<Value>,
    pub persistent: bool,
    pub cursor: usize,
    pub result: Option<Value>,
    pub awaiters: Vec<ProcessId>,
}

impl Process {
    pub fn new(id: ProcessId, persistent: bool) -> Self {
        Self {
            id,
            stack: Vec::new(),
            locals: Vec::new(),
            frames: Vec::new(),
            mailbox: VecDeque::new(),
            persistent,
            cursor: 0,
            result: None,
            awaiters: Vec::new(),
        }
    }
}
