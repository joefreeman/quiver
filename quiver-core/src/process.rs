use crate::effects::Effect;
use crate::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};

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
    /// Request the result of one or more target processes
    Await {
        targets: Vec<ProcessId>,
        caller: ProcessId,
    },
    /// Request a platform-specific effect
    RequestEffect { process_id: ProcessId, effect: E },
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
        }
    }
}
