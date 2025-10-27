use crate::WorkerId;
use crate::messages::{Command, Event};
use crate::transport::WorkerHandle;
use quiver_core::bytecode::{Bytecode, Function, Instruction, TypeId};
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::types::{CallableType, ProcessType, Type};
use quiver_core::value::Value;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Remap a Type's TypeIds according to the remap table
fn remap_type(ty: Type, type_remap: &HashMap<TypeId, TypeId>) -> Type {
    match ty {
        Type::Tuple(old_id) => Type::Tuple(*type_remap.get(&old_id).unwrap_or(&old_id)),
        Type::Partial(old_id) => Type::Partial(*type_remap.get(&old_id).unwrap_or(&old_id)),
        Type::Union(types) => Type::Union(
            types
                .into_iter()
                .map(|t| remap_type(t, type_remap))
                .collect(),
        ),
        Type::Callable(callable) => Type::Callable(Box::new(CallableType {
            parameter: remap_type(callable.parameter, type_remap),
            result: remap_type(callable.result, type_remap),
            receive: remap_type(callable.receive, type_remap),
        })),
        Type::Process(process) => Type::Process(Box::new(ProcessType {
            receive: process
                .receive
                .map(|t| Box::new(remap_type(*t, type_remap))),
            returns: process
                .returns
                .map(|t| Box::new(remap_type(*t, type_remap))),
        })),
        other => other,
    }
}

/// Remap a Function's indices according to the remap tables
fn remap_function(
    function: Function,
    constant_remap: &HashMap<usize, usize>,
    function_remap: &HashMap<usize, usize>,
    type_remap: &HashMap<TypeId, TypeId>,
    builtin_remap: &HashMap<usize, usize>,
) -> Function {
    let remapped_instructions: Vec<Instruction> = function
        .instructions
        .into_iter()
        .map(|inst| match inst {
            Instruction::Constant(idx) => {
                Instruction::Constant(*constant_remap.get(&idx).unwrap_or(&idx))
            }
            Instruction::Function(idx) => {
                Instruction::Function(*function_remap.get(&idx).unwrap_or(&idx))
            }
            Instruction::Builtin(idx) => {
                Instruction::Builtin(*builtin_remap.get(&idx).unwrap_or(&idx))
            }
            Instruction::Tuple(type_id) => {
                Instruction::Tuple(*type_remap.get(&type_id).unwrap_or(&type_id))
            }
            other => other,
        })
        .collect();

    Function {
        instructions: remapped_instructions,
        function_type: CallableType {
            parameter: remap_type(function.function_type.parameter, type_remap),
            result: remap_type(function.function_type.result, type_remap),
            receive: remap_type(function.function_type.receive, type_remap),
        },
        captures: function.captures,
    }
}

// Type aliases for complex types
pub type ValueWithHeap = (Value, Vec<Vec<u8>>);
pub type RuntimeResult = Result<Value, quiver_core::error::Error>;
pub type CompletedResult = (RuntimeResult, Vec<Vec<u8>>);
pub type ProcessResultsMap = HashMap<ProcessId, Option<CompletedResult>>;
pub type WorkerResponsesMap = HashMap<WorkerId, ProcessResultsMap>;
pub type LocalsResult = Result<Vec<ValueWithHeap>, EnvironmentError>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EnvironmentError {
    // From worker/executor
    Executor(quiver_core::error::Error),

    // Process management
    ProcessNotFound(ProcessId),
    ProcessNotSleeping(ProcessId),
    ProcessFailed(ProcessId),
    FunctionNotFound(usize),

    // Data operations
    LocalNotFound { process_id: ProcessId, index: usize },
    HeapData(String),

    // Communication
    WorkerCommunication(String),
    ChannelDisconnected,

    // Request handling
    UnexpectedResultType,
    RequestNotFound(u64),

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
            EnvironmentError::ProcessNotSleeping(pid) => {
                write!(f, "process {} is not sleeping", pid)
            }
            EnvironmentError::ProcessFailed(pid) => {
                write!(f, "process {} failed with an error", pid)
            }
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
            EnvironmentError::RequestNotFound(id) => write!(f, "request {} not found", id),
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
    Result(Result<ValueWithHeap, quiver_core::error::Error>),
    // RuntimeError(quiver_core::error::Error),
    Statuses(HashMap<ProcessId, ProcessStatus>),
    ProcessInfo(Option<ProcessInfo>),
    Locals(Vec<ValueWithHeap>),
}

struct PendingAwait {
    expected_workers: HashSet<WorkerId>,
    responses: WorkerResponsesMap,
}

pub struct Environment {
    workers: Vec<Box<dyn WorkerHandle>>,
    program: Program,
    process_router: HashMap<ProcessId, WorkerId>,
    pending_awaits: HashMap<ProcessId, PendingAwait>, // awaiter -> pending await state
    pending_requests: HashMap<u64, Option<RequestResult>>,
    // Maps aggregation_id -> worker_request_id -> Option<statuses>
    status_aggregations: HashMap<u64, HashMap<u64, Option<HashMap<ProcessId, ProcessStatus>>>>,
    next_request_id: u64,
    next_process_id: ProcessId,
}

impl Environment {
    pub fn new(workers: Vec<Box<dyn WorkerHandle>>) -> Self {
        let program = Program::new();

        Self {
            workers,
            program: program.clone(),
            process_router: HashMap::new(),
            pending_awaits: HashMap::new(),
            pending_requests: HashMap::new(),
            status_aggregations: HashMap::new(),
            next_request_id: 0,
            next_process_id: 0,
        }
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

    /// Start a new persistent process, returns assigned ProcessId
    pub fn start_process(&mut self, bytecode: Bytecode) -> Result<ProcessId, EnvironmentError> {
        // Merge bytecode into program and get remapped function index
        let function_index = self.merge_bytecode(bytecode)?;

        let pid = self.allocate_process_id();
        let worker_id = pid % self.workers.len(); // Round-robin

        self.process_router.insert(pid, worker_id);
        self.workers[worker_id]
            .send(Command::StartProcess {
                id: pid,
                function_index,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(pid)
    }

    /// Resume a sleeping persistent process
    pub fn resume_process(
        &mut self,
        pid: ProcessId,
        bytecode: Bytecode,
    ) -> Result<(), EnvironmentError> {
        // Merge bytecode into program and get remapped function index
        let function_index = self.merge_bytecode(bytecode)?;

        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or(EnvironmentError::ProcessNotFound(pid))?;

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
            .ok_or(EnvironmentError::ProcessNotFound(pid))?;

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
    /// Returns a single aggregation ID that will collect results from all workers
    pub fn request_statuses(&mut self) -> Result<u64, EnvironmentError> {
        let num_workers = self.workers.len();

        // Create aggregation ID
        let aggregation_id = self.allocate_request_id();

        // Allocate all request IDs into a Vec (to preserve ordering)
        let mut request_ids = Vec::new();
        for _ in 0..num_workers {
            request_ids.push(self.allocate_request_id());
        }

        // Create aggregation map with all worker requests marked as pending (None)
        let mut worker_requests = HashMap::new();
        for &request_id in &request_ids {
            worker_requests.insert(request_id, None);
            self.pending_requests.insert(request_id, None);
        }

        // Store aggregation state
        self.status_aggregations
            .insert(aggregation_id, worker_requests);

        // Mark aggregation as pending
        self.pending_requests.insert(aggregation_id, None);

        // Send requests to workers (using ordered Vec)
        for (i, worker) in self.workers.iter_mut().enumerate() {
            let request_id = request_ids[i];
            worker
                .send(Command::GetStatuses { request_id })
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }

        Ok(aggregation_id)
    }

    /// Request process info
    pub fn request_process_info(&mut self, pid: ProcessId) -> Result<u64, EnvironmentError> {
        let request_id = self.allocate_request_id();
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or(EnvironmentError::ProcessNotFound(pid))?;

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
            .ok_or(EnvironmentError::ProcessNotFound(pid))?;

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
            .ok_or(EnvironmentError::ProcessNotFound(pid))?;

        self.workers[*worker_id]
            .send(Command::CompactLocals {
                process_id: pid,
                keep_indices,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(())
    }

    /// Check if a request has completed (non-blocking)
    /// Automatically removes the request from pending_requests when returning a final result
    pub fn poll_request(
        &mut self,
        request_id: u64,
    ) -> Result<Option<RequestResult>, EnvironmentError> {
        match self.pending_requests.get(&request_id).cloned() {
            None => {
                self.pending_requests.remove(&request_id);
                Err(EnvironmentError::RequestNotFound(request_id))
            }
            Some(None) => {
                // No response from worker yet
                Ok(None)
            }
            Some(Some(result)) => {
                self.pending_requests.remove(&request_id);
                Ok(Some(result))
            }
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

    /// Merge bytecode into the environment's program with deduplication
    /// Returns the remapped entry function index
    fn merge_bytecode(&mut self, bytecode: Bytecode) -> Result<usize, EnvironmentError> {
        // Track old program sizes
        let old_constants_len = self.program.get_constants().len();
        let old_functions_len = self.program.get_functions().len();
        let old_types_len = self.program.get_types().len();
        let old_builtins_len = self.program.get_builtins().len();

        // Build remapping tables
        let mut constant_remap: HashMap<usize, usize> = HashMap::new();
        let mut function_remap: HashMap<usize, usize> = HashMap::new();
        let mut type_remap: HashMap<TypeId, TypeId> = HashMap::new();
        let mut builtin_remap: HashMap<usize, usize> = HashMap::new();

        // Merge constants (register_constant handles deduplication)
        for (old_idx, constant) in bytecode.constants.iter().enumerate() {
            let new_idx = self.program.register_constant(constant.clone());
            constant_remap.insert(old_idx, new_idx);
        }

        // Merge types (register_type handles deduplication after we remap TypeIds in fields)
        for (old_idx, type_info) in bytecode.types.iter().enumerate() {
            let old_type_id = TypeId(old_idx);

            // Remap TypeIds in the type's fields before registering
            let remapped_fields: Vec<_> = type_info
                .fields
                .iter()
                .map(|(name, ty)| (name.clone(), remap_type(ty.clone(), &type_remap)))
                .collect();

            // register_type_with_partial will deduplicate based on name, fields, and is_partial
            let new_type_id = self.program.register_type_with_partial(
                type_info.name.clone(),
                remapped_fields,
                type_info.is_partial,
            );
            type_remap.insert(old_type_id, new_type_id);
        }

        // Merge builtins (register_builtin handles deduplication by name)
        for (old_idx, builtin_info) in bytecode.builtins.iter().enumerate() {
            let new_idx = self.program.register_builtin(builtin_info.name.clone());
            builtin_remap.insert(old_idx, new_idx);
        }

        // Merge functions (register_function handles deduplication after we remap indices)
        for (old_idx, function) in bytecode.functions.iter().enumerate() {
            // Remap all indices in the function before registering
            let remapped_function = remap_function(
                function.clone(),
                &constant_remap,
                &function_remap,
                &type_remap,
                &builtin_remap,
            );

            // register_function will deduplicate if the function already exists
            let new_idx = self.program.register_function(remapped_function);
            function_remap.insert(old_idx, new_idx);
        }

        // Get new program data to send to workers
        let new_constants: Vec<_> = self.program.get_constants()[old_constants_len..].to_vec();
        let new_functions: Vec<_> = self.program.get_functions()[old_functions_len..].to_vec();
        let new_types: Vec<_> = self.program.get_types()[old_types_len..].to_vec();
        let new_builtins: Vec<_> = self.program.get_builtins()[old_builtins_len..].to_vec();

        // Send updates to workers
        if !new_constants.is_empty()
            || !new_functions.is_empty()
            || !new_types.is_empty()
            || !new_builtins.is_empty()
        {
            let update_cmd = Command::UpdateProgram {
                constants: new_constants,
                functions: new_functions,
                types: new_types,
                builtins: new_builtins,
            };

            for worker in &mut self.workers {
                worker
                    .send(update_cmd.clone())
                    .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
            }
        }

        // Return remapped entry function index
        let entry_idx = bytecode
            .entry
            .expect("Bytecode must have an entry function");
        Ok(*function_remap
            .get(&entry_idx)
            .expect("Entry function should be in remap table"))
    }

    fn handle_event(&mut self, event: Event) -> Result<(), EnvironmentError> {
        match event {
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
            Event::AwaitAction { awaiter, targets } => {
                self.handle_await_processes(awaiter, targets)
            }
            Event::ProcessResults { awaiter, results } => {
                self.handle_process_results(awaiter, results)
            }
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

    fn handle_await_processes(
        &mut self,
        awaiter: ProcessId,
        targets: Vec<ProcessId>,
    ) -> Result<(), EnvironmentError> {
        // Group targets by worker
        let mut targets_by_worker: HashMap<WorkerId, Vec<ProcessId>> = HashMap::new();
        for target in &targets {
            let worker_id = self
                .process_router
                .get(target)
                .ok_or(EnvironmentError::ProcessNotFound(*target))?;
            targets_by_worker
                .entry(*worker_id)
                .or_default()
                .push(*target);
        }

        // Track expected workers for this awaiter
        let expected_workers: HashSet<WorkerId> = targets_by_worker.keys().copied().collect();
        self.pending_awaits.insert(
            awaiter,
            PendingAwait {
                expected_workers,
                responses: HashMap::new(),
            },
        );

        // Send QueryAndAwait command to each worker
        for (worker_id, worker_targets) in targets_by_worker {
            self.workers[worker_id]
                .send(Command::QueryAndAwait {
                    awaiter,
                    targets: worker_targets,
                })
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }

        Ok(())
    }

    fn handle_process_results(
        &mut self,
        awaiter: ProcessId,
        results: ProcessResultsMap,
    ) -> Result<(), EnvironmentError> {
        // Get the worker ID that sent this response by looking up any process ID in results
        // (works even if all results are None, since we still have the process IDs)
        let sender_worker_id = results
            .keys()
            .next()
            .and_then(|pid| self.process_router.get(pid))
            .copied();

        if let Some(pending) = self.pending_awaits.get_mut(&awaiter) {
            // This is part of an initial await - collect the response
            if let Some(worker_id) = sender_worker_id {
                pending.responses.insert(worker_id, results.clone());
                pending.expected_workers.remove(&worker_id);

                // Check if all workers have responded
                if pending.expected_workers.is_empty() {
                    // Merge all results
                    let all_results: ProcessResultsMap = pending
                        .responses
                        .values()
                        .flat_map(|r| r.iter())
                        .map(|(k, v)| (*k, v.clone()))
                        .collect();

                    // Clean up pending state
                    self.pending_awaits.remove(&awaiter);

                    // Send merged results to awaiter's worker
                    let awaiter_worker = self
                        .process_router
                        .get(&awaiter)
                        .ok_or(EnvironmentError::ProcessNotFound(awaiter))?;

                    self.workers[*awaiter_worker]
                        .send(Command::UpdateAwaitResults {
                            awaiter,
                            results: all_results,
                        })
                        .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
                }
            }
        } else {
            // This is a later completion - forward directly to awaiter's worker
            let awaiter_worker = self
                .process_router
                .get(&awaiter)
                .ok_or(EnvironmentError::ProcessNotFound(awaiter))?;

            self.workers[*awaiter_worker]
                .send(Command::UpdateAwaitResults { awaiter, results })
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
        let worker_id = new_pid % self.workers.len();
        self.process_router.insert(new_pid, worker_id);

        // Spawn process on chosen worker with function and captures
        self.workers[worker_id]
            .send(Command::SpawnProcess {
                id: new_pid,
                function_index,
                captures,
                heap_data: heap.clone(),
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        // Notify caller
        let caller_worker = self
            .process_router
            .get(&caller)
            .ok_or(EnvironmentError::ProcessNotFound(caller))?;

        self.workers[*caller_worker]
            .send(Command::NotifySpawn {
                process_id: caller,
                spawned_pid: new_pid,
                function_index,
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
            .ok_or(EnvironmentError::ProcessNotFound(target))?;

        self.workers[*worker_id]
            .send(Command::DeliverMessage {
                target,
                message,
                heap,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(())
    }

    fn handle_result_response(
        &mut self,
        request_id: u64,
        result: Result<ValueWithHeap, quiver_core::error::Error>,
    ) -> Result<(), EnvironmentError> {
        self.pending_requests
            .insert(request_id, Some(RequestResult::Result(result)));
        Ok(())
    }

    fn handle_statuses_response(
        &mut self,
        request_id: u64,
        result: Result<HashMap<ProcessId, ProcessStatus>, EnvironmentError>,
    ) -> Result<(), EnvironmentError> {
        let statuses = result?;

        // Check if this request is part of an aggregation
        let mut aggregation_id = None;
        for (agg_id, worker_requests) in &self.status_aggregations {
            if worker_requests.contains_key(&request_id) {
                aggregation_id = Some(*agg_id);
                break;
            }
        }

        if let Some(agg_id) = aggregation_id {
            // This is part of an aggregation - store the result
            if let Some(worker_requests) = self.status_aggregations.get_mut(&agg_id) {
                worker_requests.insert(request_id, Some(statuses));

                // Check if all results are collected
                if worker_requests.values().all(|v| v.is_some()) {
                    // Merge all results into a single HashMap
                    let mut merged = HashMap::new();
                    for stats in worker_requests.values().flatten() {
                        merged.extend(stats.clone());
                    }

                    // Store the aggregated result
                    self.pending_requests
                        .insert(agg_id, Some(RequestResult::Statuses(merged)));

                    // Clean up aggregation and individual worker requests
                    let worker_req_ids: Vec<u64> = worker_requests.keys().copied().collect();
                    self.status_aggregations.remove(&agg_id);
                    for worker_req_id in worker_req_ids {
                        self.pending_requests.remove(&worker_req_id);
                    }
                } else {
                    // Still waiting for more results - mark this individual request as complete
                    self.pending_requests.remove(&request_id);
                }
            }
        } else {
            // Not part of an aggregation - store directly
            self.pending_requests
                .insert(request_id, Some(RequestResult::Statuses(statuses)));
        }

        Ok(())
    }

    fn handle_info_response(
        &mut self,
        request_id: u64,
        result: Result<Option<ProcessInfo>, EnvironmentError>,
    ) -> Result<(), EnvironmentError> {
        let info = result?;
        self.pending_requests
            .insert(request_id, Some(RequestResult::ProcessInfo(info)));
        Ok(())
    }

    fn handle_locals_response(
        &mut self,
        request_id: u64,
        result: LocalsResult,
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

    /// Format a value for display
    pub fn format_value(&self, value: &Value, heap: &[Vec<u8>]) -> String {
        quiver_core::format::format_value(value, heap, &self.program)
    }

    /// Format a type for display
    pub fn format_type(&self, ty: &Type) -> String {
        quiver_core::format::format_type(&self.program, ty)
    }

    /// Get the formatted type for a process given its function index
    pub fn format_process_type(&self, function_index: usize) -> Option<String> {
        let function = self.program.get_function(function_index)?;
        let process_type = Type::Process(Box::new(ProcessType {
            receive: Some(Box::new(function.function_type.receive.clone())),
            returns: Some(Box::new(function.function_type.result.clone())),
        }));
        Some(self.format_type(&process_type))
    }

    /// Convert a runtime Value to its Type representation
    pub fn value_to_type(&self, value: &Value) -> Type {
        match value {
            Value::Integer(_) => Type::Integer,
            Value::Binary(_) => Type::Binary,
            Value::Tuple(type_id, _) => Type::Tuple(*type_id),
            Value::Function(func_idx, _) => {
                let func_type = &self
                    .program
                    .get_function(*func_idx)
                    .expect("Function should exist")
                    .function_type;
                Type::Callable(Box::new(func_type.clone()))
            }
            Value::Builtin(name) => {
                let builtin_info = self
                    .program
                    .get_builtins()
                    .iter()
                    .find(|b| &b.name == name)
                    .expect("Builtin should be registered");
                Type::Callable(Box::new(CallableType {
                    parameter: builtin_info.parameter_type.clone(),
                    result: builtin_info.result_type.clone(),
                    receive: Type::Union(vec![]),
                }))
            }
            Value::Process(_, function_idx) => {
                let func_type = &self
                    .program
                    .get_function(*function_idx)
                    .expect("Function should exist")
                    .function_type;
                Type::Process(Box::new(ProcessType {
                    receive: Some(Box::new(func_type.receive.clone())),
                    returns: Some(Box::new(func_type.result.clone())),
                }))
            }
        }
    }

    /// Resolve a type alias and return the resolved Type.
    /// Type parameters are resolved to Type::Variable placeholders.
    /// This is useful for testing and displaying type aliases.
    pub fn resolve_type_alias(
        &mut self,
        type_aliases: &std::collections::HashMap<String, quiver_compiler::compiler::TypeAliasDef>,
        alias_name: &str,
    ) -> Result<Type, String> {
        // Convert type_aliases HashMap to a single scope for resolution
        let mut bindings = std::collections::HashMap::new();
        for (name, type_alias) in type_aliases {
            bindings.insert(
                name.clone(),
                quiver_compiler::compiler::Binding::TypeAlias(type_alias.clone()),
            );
        }
        let scope = quiver_compiler::compiler::Scope::new(bindings, None);
        let scopes = vec![scope];

        quiver_compiler::compiler::resolve_type_alias_for_display(
            &scopes,
            alias_name,
            &mut self.program,
        )
        .map_err(|e| format!("{:?}", e))
    }
}
