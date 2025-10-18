use crate::WorkerId;
use crate::messages::{Command, Event};
use crate::transport::WorkerHandle;
use quiver_core::bytecode::{Constant, Function, TypeId};
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::types::TupleTypeInfo;
use quiver_core::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
    pending_requests: HashMap<u64, Option<RequestResult>>,
    next_request_id: u64,
    next_process_id: ProcessId,
    num_workers: usize,
}

impl Environment {
    pub fn new(workers: Vec<Box<dyn WorkerHandle>>, program: &Program) -> Result<Self, String> {
        let num_workers = workers.len();

        let mut env = Self {
            workers,
            process_router: HashMap::new(),
            pending_requests: HashMap::new(),
            next_request_id: 0,
            next_process_id: 0,
            num_workers,
        };

        // Send initial program to all workers
        let update_cmd = Command::UpdateProgram {
            constants: program.get_constants().clone(),
            functions: program.get_functions().clone(),
            types: program.get_types(),
            builtins: program.get_builtins().clone(),
        };

        for worker in &mut env.workers {
            worker.send(update_cmd.clone())?;
        }

        Ok(env)
    }

    /// Process events from workers, route actions
    /// Returns true if work was done, false if idle
    pub fn step(&mut self) -> Result<bool, String> {
        let mut did_work = false;

        // Collect all events from all workers first
        let mut events = Vec::new();
        for worker in &mut self.workers {
            while let Some(event) = worker.try_recv()? {
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
        types: HashMap<TypeId, TupleTypeInfo>,
        builtins: Vec<String>,
    ) -> Result<(), String> {
        let cmd = Command::UpdateProgram {
            constants,
            functions,
            types,
            builtins,
        };

        for worker in &mut self.workers {
            worker.send(cmd.clone())?;
        }

        Ok(())
    }

    /// Start a new process, returns assigned ProcessId
    pub fn start_process(
        &mut self,
        function_index: usize,
        persistent: bool,
    ) -> Result<ProcessId, String> {
        let pid = self.allocate_process_id();
        let worker_id = pid % self.num_workers; // Round-robin

        self.process_router.insert(pid, worker_id);
        self.workers[worker_id].send(Command::StartProcess {
            id: pid,
            function_index,
            captures: vec![],
            heap_data: vec![],
            persistent,
        })?;

        Ok(pid)
    }

    /// Wake a sleeping persistent process
    pub fn wake_process(&mut self, pid: ProcessId, function_index: usize) -> Result<(), String> {
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or_else(|| format!("Process {:?} not found", pid))?;

        self.workers[*worker_id].send(Command::WakeProcess {
            id: pid,
            function_index,
        })?;

        Ok(())
    }

    /// Request a process result (async operation)
    pub fn request_result(&mut self, pid: ProcessId) -> Result<u64, String> {
        let request_id = self.allocate_request_id();
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or_else(|| format!("Process {:?} not found", pid))?;

        self.workers[*worker_id].send(Command::GetResult {
            request_id,
            process_id: pid,
        })?;

        self.pending_requests.insert(request_id, None);
        Ok(request_id)
    }

    /// Request all process statuses
    pub fn request_statuses(&mut self) -> Result<Vec<u64>, String> {
        let mut request_ids = Vec::new();
        let num_workers = self.workers.len();

        // Allocate all request IDs first
        for _ in 0..num_workers {
            request_ids.push(self.allocate_request_id());
        }

        // Send requests to workers
        for (i, worker) in self.workers.iter_mut().enumerate() {
            let request_id = request_ids[i];
            worker.send(Command::GetStatuses { request_id })?;
            self.pending_requests.insert(request_id, None);
        }

        Ok(request_ids)
    }

    /// Request process info
    pub fn request_process_info(&mut self, pid: ProcessId) -> Result<u64, String> {
        let request_id = self.allocate_request_id();
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or_else(|| format!("Process {:?} not found", pid))?;

        self.workers[*worker_id].send(Command::GetProcessInfo {
            request_id,
            process_id: pid,
        })?;

        self.pending_requests.insert(request_id, None);
        Ok(request_id)
    }

    /// Request process locals
    pub fn request_locals(&mut self, pid: ProcessId, indices: Vec<usize>) -> Result<u64, String> {
        let request_id = self.allocate_request_id();
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or_else(|| format!("Process {:?} not found", pid))?;

        self.workers[*worker_id].send(Command::GetLocals {
            request_id,
            process_id: pid,
            indices,
        })?;

        self.pending_requests.insert(request_id, None);
        Ok(request_id)
    }

    /// Compact process locals
    pub fn compact_locals(
        &mut self,
        pid: ProcessId,
        keep_indices: Vec<usize>,
    ) -> Result<(), String> {
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or_else(|| format!("Process {:?} not found", pid))?;

        self.workers[*worker_id].send(Command::CompactLocals {
            process_id: pid,
            keep_indices,
        })?;

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
    pub fn wait_for_request(&mut self, request_id: u64) -> Result<RequestResult, String> {
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

    fn handle_event(&mut self, event: Event) -> Result<(), String> {
        match event {
            Event::ProcessCompleted {
                process_id,
                result,
                heap,
                awaiters,
            } => self.handle_process_completed(process_id, result, heap, awaiters),
            Event::ActionSpawn {
                caller,
                function_index,
                captures,
                heap,
            } => self.handle_spawn(caller, function_index, captures, heap),
            Event::ActionDeliver {
                target,
                message,
                heap,
            } => self.handle_deliver(target, message, heap),
            Event::ActionAwaitResult { caller, target } => self.handle_await_result(caller, target),
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
        }
    }

    fn handle_process_completed(
        &mut self,
        _process_id: ProcessId,
        result: Result<Value, quiver_core::error::Error>,
        heap: Vec<Vec<u8>>,
        awaiters: Vec<ProcessId>,
    ) -> Result<(), String> {
        // Notify all awaiters
        for awaiter in awaiters {
            let worker_id = self
                .process_router
                .get(&awaiter)
                .ok_or_else(|| format!("Awaiter process {:?} not found", awaiter))?;

            self.workers[*worker_id].send(Command::NotifyResult {
                process_id: awaiter,
                result: result.clone(),
                heap: heap.clone(),
            })?;
        }

        Ok(())
    }

    fn handle_spawn(
        &mut self,
        caller: ProcessId,
        function_index: usize,
        captures: Vec<Value>,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), String> {
        // Allocate new ProcessId
        let new_pid = self.allocate_process_id();

        // Choose worker (round-robin)
        let worker_id = new_pid % self.num_workers;
        self.process_router.insert(new_pid, worker_id);

        // Start process on chosen worker with function and captures
        self.workers[worker_id].send(Command::StartProcess {
            id: new_pid,
            function_index,
            captures,
            heap_data: heap.clone(),
            persistent: false,
        })?;

        // Notify caller
        let caller_worker = self
            .process_router
            .get(&caller)
            .ok_or_else(|| format!("Caller process {:?} not found", caller))?;

        self.workers[*caller_worker].send(Command::NotifySpawn {
            process_id: caller,
            spawned_pid: new_pid,
        })?;

        Ok(())
    }

    fn handle_deliver(
        &mut self,
        target: ProcessId,
        message: Value,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), String> {
        let worker_id = self
            .process_router
            .get(&target)
            .ok_or_else(|| format!("Target process {:?} not found", target))?;

        self.workers[*worker_id].send(Command::DeliverMessage {
            target,
            message,
            heap,
        })?;

        Ok(())
    }

    fn handle_await_result(&mut self, caller: ProcessId, target: ProcessId) -> Result<(), String> {
        let target_worker = self
            .process_router
            .get(&target)
            .ok_or_else(|| format!("Target process {:?} not found", target))?;

        self.workers[*target_worker].send(Command::RegisterAwaiter {
            target,
            awaiter: caller,
        })?;

        Ok(())
    }

    fn handle_result_response(
        &mut self,
        request_id: u64,
        result: Result<Option<(Value, Vec<Vec<u8>>)>, quiver_core::error::Error>,
    ) -> Result<(), String> {
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
        result: Result<HashMap<ProcessId, ProcessStatus>, String>,
    ) -> Result<(), String> {
        let statuses = result.map_err(|e| format!("Statuses request failed: {}", e))?;
        self.pending_requests
            .insert(request_id, Some(RequestResult::Statuses(statuses)));
        Ok(())
    }

    fn handle_info_response(
        &mut self,
        request_id: u64,
        result: Result<Option<ProcessInfo>, String>,
    ) -> Result<(), String> {
        let info = result.map_err(|e| format!("Info request failed: {}", e))?;
        self.pending_requests
            .insert(request_id, Some(RequestResult::Info(info)));
        Ok(())
    }

    fn handle_locals_response(
        &mut self,
        request_id: u64,
        result: Result<Vec<(Value, Vec<Vec<u8>>)>, String>,
    ) -> Result<(), String> {
        let locals = result.map_err(|e| format!("Locals request failed: {}", e))?;
        self.pending_requests
            .insert(request_id, Some(RequestResult::Locals(locals)));
        Ok(())
    }
}
