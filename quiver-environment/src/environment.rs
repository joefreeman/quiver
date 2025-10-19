use crate::WorkerId;
use crate::messages::{Command, Event};
use crate::transport::WorkerHandle;
use quiver_core::bytecode::{Constant, Function};
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::types::TupleTypeInfo;
use quiver_core::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EnvironmentError {
    // From worker/executor
    Executor(quiver_core::error::Error),

    // Process management
    ProcessNotFound(ProcessId),
    FunctionNotFound(usize),

    // Data operations
    LocalNotFound { process_id: ProcessId, index: usize },
    HeapData(String),

    // Communication
    WorkerCommunication(String),
    ChannelDisconnected,

    // Request handling
    UnexpectedResultType,

    // Timeouts
    Timeout(std::time::Duration),

    // REPL state
    NoReplProcess,
    VariableNotFound(String),
    InvalidVariableIndex(usize),
}

impl std::fmt::Display for EnvironmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnvironmentError::Executor(e) => write!(f, "{:?}", e),
            EnvironmentError::ProcessNotFound(pid) => write!(f, "process {} not found", pid),
            EnvironmentError::FunctionNotFound(idx) => write!(f, "function {} not found", idx),
            EnvironmentError::LocalNotFound { process_id, index } => {
                write!(
                    f,
                    "local variable {} not found in process {}",
                    index, process_id
                )
            }
            EnvironmentError::HeapData(msg) => write!(f, "heap data: {}", msg),
            EnvironmentError::WorkerCommunication(msg) => {
                write!(f, "worker communication: {}", msg)
            }
            EnvironmentError::ChannelDisconnected => write!(f, "channel disconnected"),
            EnvironmentError::UnexpectedResultType => write!(f, "unexpected result type"),
            EnvironmentError::Timeout(duration) => {
                write!(f, "operation timed out after {:?}", duration)
            }
            EnvironmentError::NoReplProcess => write!(f, "no REPL process started"),
            EnvironmentError::VariableNotFound(name) => write!(f, "variable '{}' not found", name),
            EnvironmentError::InvalidVariableIndex(idx) => {
                write!(f, "invalid variable index: {}", idx)
            }
        }
    }
}

impl std::error::Error for EnvironmentError {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RequestResult {
    Result(Option<(Value, Vec<Vec<u8>>)>),
    RuntimeError(quiver_core::error::Error),
    Statuses(HashMap<ProcessId, ProcessStatus>),
    Info(Option<ProcessInfo>),
    Locals(Vec<(Value, Vec<Vec<u8>>)>),
}

pub struct Environment {
    workers: Vec<Box<dyn WorkerHandle>>,
    process_router: HashMap<ProcessId, WorkerId>,
    awaiters_for: HashMap<ProcessId, Vec<ProcessId>>,
    pending_requests: HashMap<u64, Option<RequestResult>>,
    next_request_id: u64,
    next_process_id: ProcessId,
    num_workers: usize,
}

impl Environment {
    pub fn new(
        workers: Vec<Box<dyn WorkerHandle>>,
        program: &Program,
    ) -> Result<Self, EnvironmentError> {
        let num_workers = workers.len();

        let mut env = Self {
            workers,
            process_router: HashMap::new(),
            awaiters_for: HashMap::new(),
            pending_requests: HashMap::new(),
            next_request_id: 0,
            next_process_id: 0,
            num_workers,
        };

        // Send initial program to all workers
        let update_cmd = Command::UpdateProgram {
            constants: program.get_constants().clone(),
            functions: program.get_functions().clone(),
            types: program.get_types().clone(),
            builtins: program.get_builtins().clone(),
        };

        for worker in &mut env.workers {
            worker
                .send(update_cmd.clone())
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }

        Ok(env)
    }

    /// Process events from workers, route actions
    /// Returns true if work was done, false if idle
    pub fn step(&mut self) -> Result<bool, EnvironmentError> {
        let mut did_work = false;

        // Collect all events from all workers first
        let mut events = Vec::new();
        for worker in &mut self.workers {
            while let Some(event) = worker
                .try_recv()
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?
            {
                events.push(event);
                did_work = true;
            }
        }

        // Handle all collected events
        for event in events {
            self.handle_event(event)?;
        }

        Ok(did_work)
    }

    /// Update program (additive only)
    pub fn update_program(
        &mut self,
        constants: Vec<Constant>,
        functions: Vec<Function>,
        types: Vec<TupleTypeInfo>,
        builtins: Vec<String>,
    ) -> Result<(), EnvironmentError> {
        let cmd = Command::UpdateProgram {
            constants,
            functions,
            types,
            builtins,
        };

        for worker in &mut self.workers {
            worker
                .send(cmd.clone())
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }

        Ok(())
    }

    /// Start a new process, returns assigned ProcessId
    pub fn start_process(
        &mut self,
        function_index: usize,
        persistent: bool,
    ) -> Result<ProcessId, EnvironmentError> {
        let pid = self.allocate_process_id();
        let worker_id = pid % self.num_workers; // Round-robin

        self.process_router.insert(pid, worker_id);
        self.workers[worker_id]
            .send(Command::StartProcess {
                id: pid,
                function_index,
                captures: vec![],
                heap_data: vec![],
                persistent,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(pid)
    }

    /// Resume a sleeping persistent process
    pub fn resume_process(
        &mut self,
        pid: ProcessId,
        function_index: usize,
    ) -> Result<(), EnvironmentError> {
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(pid))?;

        self.workers[*worker_id]
            .send(Command::ResumeProcess {
                id: pid,
                function_index,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(())
    }

    /// Request a process result (async operation)
    pub fn request_result(&mut self, pid: ProcessId) -> Result<u64, EnvironmentError> {
        let request_id = self.allocate_request_id();
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(pid))?;

        self.workers[*worker_id]
            .send(Command::GetResult {
                request_id,
                process_id: pid,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        self.pending_requests.insert(request_id, None);
        Ok(request_id)
    }

    /// Request all process statuses
    pub fn request_statuses(&mut self) -> Result<Vec<u64>, EnvironmentError> {
        let mut request_ids = Vec::new();
        let num_workers = self.workers.len();

        // Allocate all request IDs first
        for _ in 0..num_workers {
            request_ids.push(self.allocate_request_id());
        }

        // Send requests to workers
        for (i, worker) in self.workers.iter_mut().enumerate() {
            let request_id = request_ids[i];
            worker
                .send(Command::GetStatuses { request_id })
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
            self.pending_requests.insert(request_id, None);
        }

        Ok(request_ids)
    }

    /// Request process info
    pub fn request_process_info(&mut self, pid: ProcessId) -> Result<u64, EnvironmentError> {
        let request_id = self.allocate_request_id();
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(pid))?;

        self.workers[*worker_id]
            .send(Command::GetProcessInfo {
                request_id,
                process_id: pid,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        self.pending_requests.insert(request_id, None);
        Ok(request_id)
    }

    /// Request process locals
    pub fn request_locals(
        &mut self,
        pid: ProcessId,
        indices: Vec<usize>,
    ) -> Result<u64, EnvironmentError> {
        let request_id = self.allocate_request_id();
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(pid))?;

        self.workers[*worker_id]
            .send(Command::GetLocals {
                request_id,
                process_id: pid,
                indices,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        self.pending_requests.insert(request_id, None);
        Ok(request_id)
    }

    /// Compact process locals
    pub fn compact_locals(
        &mut self,
        pid: ProcessId,
        keep_indices: Vec<usize>,
    ) -> Result<(), EnvironmentError> {
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(pid))?;

        self.workers[*worker_id]
            .send(Command::CompactLocals {
                process_id: pid,
                keep_indices,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(())
    }

    /// Check if a request has completed (non-blocking)
    pub fn poll_request(&mut self, request_id: u64) -> Option<RequestResult> {
        self.pending_requests
            .get(&request_id)
            .and_then(|opt| opt.as_ref())
            .map(|result| match result {
                RequestResult::Result(r) => RequestResult::Result(r.clone()),
                RequestResult::RuntimeError(e) => RequestResult::RuntimeError(e.clone()),
                RequestResult::Statuses(s) => RequestResult::Statuses(s.clone()),
                RequestResult::Info(i) => RequestResult::Info(i.clone()),
                RequestResult::Locals(l) => RequestResult::Locals(l.clone()),
            })
    }

    /// Blocking wait for request
    pub fn wait_for_request(&mut self, request_id: u64) -> Result<RequestResult, EnvironmentError> {
        loop {
            if let Some(result) = self.poll_request(request_id) {
                self.pending_requests.remove(&request_id);
                return Ok(result);
            }
            self.step()?;
        }
    }

    fn allocate_process_id(&mut self) -> ProcessId {
        let pid = self.next_process_id;
        self.next_process_id += 1;
        pid
    }

    fn allocate_request_id(&mut self) -> u64 {
        let id = self.next_request_id;
        self.next_request_id += 1;
        id
    }

    fn handle_event(&mut self, event: Event) -> Result<(), EnvironmentError> {
        match event {
            Event::Completed {
                process_id,
                result,
                heap,
            } => self.handle_process_completed(process_id, result, heap),
            Event::SpawnAction {
                caller,
                function_index,
                captures,
                heap,
            } => self.handle_spawn(caller, function_index, captures, heap),
            Event::DeliverAction {
                target,
                message,
                heap,
            } => self.handle_deliver(target, message, heap),
            Event::AwaitAction { caller, target } => self.handle_await_result(caller, target),
            Event::ResultResponse { request_id, result } => {
                self.handle_result_response(request_id, result)
            }
            Event::StatusesResponse { request_id, result } => {
                self.handle_statuses_response(request_id, result)
            }
            Event::InfoResponse { request_id, result } => {
                self.handle_info_response(request_id, result)
            }
            Event::LocalsResponse { request_id, result } => {
                self.handle_locals_response(request_id, result)
            }
            Event::WorkerError { error } => self.handle_worker_error(error),
        }
    }

    fn handle_process_completed(
        &mut self,
        process_id: ProcessId,
        result: Result<Value, quiver_core::error::Error>,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), EnvironmentError> {
        // Look up awaiters and remove the entry
        let awaiters = self.awaiters_for.remove(&process_id).unwrap_or_default();

        // Notify all awaiters
        for awaiter in awaiters {
            let worker_id = self
                .process_router
                .get(&awaiter)
                .ok_or_else(|| EnvironmentError::ProcessNotFound(awaiter))?;

            self.workers[*worker_id]
                .send(Command::NotifyResult {
                    process_id: awaiter,
                    result: result.clone(),
                    heap: heap.clone(),
                })
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }

        Ok(())
    }

    fn handle_spawn(
        &mut self,
        caller: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), EnvironmentError> {
        // Allocate new ProcessId
        let new_pid = self.allocate_process_id();

        // Choose worker (round-robin)
        let worker_id = new_pid % self.num_workers;
        self.process_router.insert(new_pid, worker_id);

        // Start process on chosen worker with function and captures
        self.workers[worker_id]
            .send(Command::StartProcess {
                id: new_pid,
                function_index,
                captures,
                heap_data: heap.clone(),
                persistent: false,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        // Notify caller
        let caller_worker = self
            .process_router
            .get(&caller)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(caller))?;

        self.workers[*caller_worker]
            .send(Command::NotifySpawn {
                process_id: caller,
                spawned_pid: new_pid,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(())
    }

    fn handle_deliver(
        &mut self,
        target: ProcessId,
        message: Value,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), EnvironmentError> {
        let worker_id = self
            .process_router
            .get(&target)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(target))?;

        self.workers[*worker_id]
            .send(Command::DeliverMessage {
                target,
                message,
                heap,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(())
    }

    fn handle_await_result(
        &mut self,
        caller: ProcessId,
        target: ProcessId,
    ) -> Result<(), EnvironmentError> {
        // Record the awaiter relationship
        self.awaiters_for.entry(target).or_default().push(caller);

        let target_worker = self
            .process_router
            .get(&target)
            .ok_or_else(|| EnvironmentError::ProcessNotFound(target))?;

        self.workers[*target_worker]
            .send(Command::RegisterAwaiter {
                target,
                awaiter: caller,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(())
    }

    fn handle_result_response(
        &mut self,
        request_id: u64,
        result: Result<Option<(Value, Vec<Vec<u8>>)>, quiver_core::error::Error>,
    ) -> Result<(), EnvironmentError> {
        let request_result = match result {
            Ok(value) => RequestResult::Result(value),
            Err(error) => RequestResult::RuntimeError(error),
        };
        self.pending_requests
            .insert(request_id, Some(request_result));
        Ok(())
    }

    fn handle_statuses_response(
        &mut self,
        request_id: u64,
        result: Result<HashMap<ProcessId, ProcessStatus>, EnvironmentError>,
    ) -> Result<(), EnvironmentError> {
        let statuses = result?;
        self.pending_requests
            .insert(request_id, Some(RequestResult::Statuses(statuses)));
        Ok(())
    }

    fn handle_info_response(
        &mut self,
        request_id: u64,
        result: Result<Option<ProcessInfo>, EnvironmentError>,
    ) -> Result<(), EnvironmentError> {
        let info = result?;
        self.pending_requests
            .insert(request_id, Some(RequestResult::Info(info)));
        Ok(())
    }

    fn handle_locals_response(
        &mut self,
        request_id: u64,
        result: Result<Vec<(Value, Vec<Vec<u8>>)>, EnvironmentError>,
    ) -> Result<(), EnvironmentError> {
        let locals = result?;
        self.pending_requests
            .insert(request_id, Some(RequestResult::Locals(locals)));
        Ok(())
    }

    fn handle_worker_error(&mut self, error: EnvironmentError) -> Result<(), EnvironmentError> {
        // Log the worker error to stderr
        // In the future, we could track which worker failed and handle it more gracefully
        eprintln!("Worker error: {}", error);
        Ok(())
    }
}
