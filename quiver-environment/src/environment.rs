use crate::WorkerId;
use crate::messages::{Command, Event};
use crate::transport::WorkerHandle;
use quiver_compiler::compiler::{
    Binding, Scope, ScopeKind, TypeAliasDef, resolve_type_alias_for_display,
};
use quiver_core::bytecode::{Bytecode, Constant, Function, Instruction};
use quiver_core::compatibility::{
    CompatibilityInput, compute_param_compatibility, compute_type_compatibility,
};
use quiver_core::effects::{Effect, EffectBackend};
use quiver_core::executor::ProgramUpdate;
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::types::{Type, TypeLookup};
use quiver_core::value::{ResourceId, Value};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

type WorkerRequestMap<T> = HashMap<u64, Option<HashMap<ProcessId, T>>>;

enum Aggregation {
    Statuses(WorkerRequestMap<ProcessStatus>),
    ProcessTypes(WorkerRequestMap<usize>), // Maps request_id -> Option<HashMap<ProcessId, function_index>>
}

fn remap_type_id(id: usize, type_remap: &HashMap<usize, usize>) -> usize {
    *type_remap.get(&id).unwrap_or(&id)
}

fn remap_type(ty: Type, type_remap: &HashMap<usize, usize>) -> Type {
    match ty {
        Type::Tuple(old_id) => Type::Tuple(remap_type_id(old_id, type_remap)),
        Type::Partial { name, fields } => Type::Partial {
            name,
            fields: fields
                .into_iter()
                .map(|(fname, ftype)| (fname, remap_type_id(ftype, type_remap)))
                .collect(),
        },
        Type::Union(type_ids) => Type::Union(
            type_ids
                .into_iter()
                .map(|t| remap_type_id(t, type_remap))
                .collect(),
        ),
        Type::Callable {
            parameter,
            result,
            receive,
        } => Type::Callable {
            parameter: remap_type_id(parameter, type_remap),
            result: remap_type_id(result, type_remap),
            receive: remap_type_id(receive, type_remap),
        },
        Type::Process { send, receive } => Type::Process {
            send: send.map(|t| remap_type_id(t, type_remap)),
            receive: receive.map(|t| remap_type_id(t, type_remap)),
        },
        other => other,
    }
}

/// Remap a Function's indices according to the remap tables
fn remap_function(
    function: Function,
    constant_remap: &HashMap<usize, usize>,
    function_remap: &HashMap<usize, usize>,
    tuple_remap: &HashMap<usize, usize>,
    type_remap: &HashMap<usize, usize>,
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
                Instruction::Tuple(*tuple_remap.get(&type_id).unwrap_or(&type_id))
            }
            Instruction::IsType(type_id) => {
                Instruction::IsType(*type_remap.get(&type_id).unwrap_or(&type_id))
            }
            other => other,
        })
        .collect();

    Function {
        instructions: remapped_instructions,
        captures: function.captures,
        type_id: *type_remap
            .get(&function.type_id)
            .unwrap_or(&function.type_id),
    }
}

// Type aliases for complex types
pub type ValueWithHeap = (Value, Vec<Vec<u8>>);
pub type RuntimeResult = Result<ValueWithHeap, quiver_core::error::Error>;
pub type ProcessResultsMap = HashMap<ProcessId, Option<RuntimeResult>>;
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
    Result(
        Result<ValueWithHeap, quiver_core::error::Error>,
        Option<quiver_core::executor::ExecutionStats>,
    ),
    Statuses(HashMap<ProcessId, ProcessStatus>),
    ProcessTypes(HashMap<ProcessId, (Type, usize)>),
    ProcessInfo(Option<ProcessInfo>),
    Locals(Vec<ValueWithHeap>),
}

struct PendingAwait {
    expected_workers: HashSet<WorkerId>,
    responses: WorkerResponsesMap,
}

pub struct Environment<E: Effect> {
    workers: Vec<Box<dyn WorkerHandle<E>>>,
    // Accumulated program state with full type information
    program: Program,
    process_router: HashMap<ProcessId, WorkerId>,
    pending_awaits: HashMap<ProcessId, PendingAwait>, // awaiter -> pending await state
    pending_requests: HashMap<u64, Option<RequestResult>>,
    // Maps aggregation_id -> Aggregation (either Statuses or ProcessTypes)
    aggregations: HashMap<u64, Aggregation>,
    next_request_id: u64,
    next_process_id: ProcessId,

    // Effect backend and resource management
    effect_backend: Option<Box<dyn EffectBackend<E = E>>>,
    resource_ownership: HashMap<ResourceId, ProcessId>,
}

impl<E: Effect> Environment<E> {
    pub fn new(workers: Vec<Box<dyn WorkerHandle<E>>>) -> Self {
        Self {
            workers,
            program: Program::new(),
            process_router: HashMap::new(),
            pending_awaits: HashMap::new(),
            pending_requests: HashMap::new(),
            aggregations: HashMap::new(),
            next_request_id: 0,
            next_process_id: 0,
            effect_backend: None,
            resource_ownership: HashMap::new(),
        }
    }

    /// Set the effect backend for executing platform-specific effects
    pub fn set_effect_backend(&mut self, backend: Box<dyn EffectBackend<E = E>>) {
        self.effect_backend = Some(backend);
    }

    /// Process events from workers, route actions
    /// Returns true if work was done, false if idle
    pub fn step(&mut self) -> Result<bool, EnvironmentError> {
        let mut did_work = false;

        // Process effect completions
        if let Some(effect_backend) = self.effect_backend.as_mut() {
            let completions = effect_backend.process_completions();
            if !completions.is_empty() {
                did_work = true;
                for (process_id, completion) in completions {
                    self.handle_effect_completion(process_id, completion)?;
                }
            }
        }

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
    /// If bytecode is None, creates a sleeping process ready for resume (used by REPL)
    pub fn start_process(
        &mut self,
        bytecode: Option<Bytecode>,
    ) -> Result<ProcessId, EnvironmentError> {
        let function_index = match bytecode {
            Some(bc) => Some(self.merge_bytecode(bc)?),
            None => None,
        };

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
        // Merge bytecode and get remapped function index
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
    /// Stats are included in the response if the executor has profiling enabled
    pub fn request_result(&mut self, pid: ProcessId) -> Result<u64, EnvironmentError> {
        let request_id = self.allocate_request_id();
        let worker_id = *self
            .process_router
            .get(&pid)
            .ok_or(EnvironmentError::ProcessNotFound(pid))?;

        self.workers[worker_id]
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
        self.aggregations
            .insert(aggregation_id, Aggregation::Statuses(worker_requests));

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

    /// Request all process types (for REPL process references)
    /// Returns a single aggregation ID that will collect results from all workers
    pub fn request_process_types(&mut self) -> Result<u64, EnvironmentError> {
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
        self.aggregations
            .insert(aggregation_id, Aggregation::ProcessTypes(worker_requests));

        // Mark aggregation as pending
        self.pending_requests.insert(aggregation_id, None);

        // Send requests to workers (using ordered Vec)
        for (i, worker) in self.workers.iter_mut().enumerate() {
            let request_id = request_ids[i];
            worker
                .send(Command::GetProcessTypes { request_id })
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

    /// Merge bytecode into the environment's accumulated state with deduplication
    /// Returns the remapped entry function index
    fn merge_bytecode(&mut self, bytecode: Bytecode) -> Result<usize, EnvironmentError> {
        let entry_fn = bytecode.entry.expect("Bytecode must have an entry point");

        // Track old sizes for computing deltas
        let old_constants_len = self.program.get_constants().len();
        let old_functions_len = self.program.get_functions().len();
        let old_tuples_len = self.program.get_tuples().len();
        let old_builtins_len = self.program.get_builtins().len();
        let old_types_len = self.program.get_types().len();

        // Build remapping tables using Program::register_* methods
        let mut constant_remap: HashMap<usize, usize> = HashMap::new();
        let mut tuple_remap: HashMap<usize, usize> = HashMap::new();
        let mut type_remap: HashMap<usize, usize> = HashMap::new();
        let mut builtin_remap: HashMap<usize, usize> = HashMap::new();
        let mut function_remap: HashMap<usize, usize> = HashMap::new();

        // Merge constants using Program::register_constant
        for (old_idx, constant) in bytecode.constants.iter().enumerate() {
            let new_idx = self.program.register_constant(constant.clone());
            constant_remap.insert(old_idx, new_idx);
        }

        // Merge tuples using Program::register_tuple
        // Tuples are processed in order, so type references to earlier tuples are already remapped
        for (old_idx, tuple_info) in bytecode.tuples.iter().enumerate() {
            // Remap type ID references in fields
            let remapped_fields: Vec<_> = tuple_info
                .fields
                .iter()
                .map(|(name, type_id)| (name.clone(), remap_type_id(*type_id, &type_remap)))
                .collect();

            let new_idx = self
                .program
                .register_tuple(tuple_info.name.clone(), remapped_fields);
            tuple_remap.insert(old_idx, new_idx);
        }

        // Merge types (used by IsType instructions) using Program::register_type
        for (old_idx, typ) in bytecode.types.iter().enumerate() {
            // Remap tuple type references within the type
            let remapped_type = remap_type(typ.clone(), &type_remap);
            let new_idx = self.program.register_type(remapped_type);
            type_remap.insert(old_idx, new_idx);
        }

        // Merge builtins using Program::register_builtin_info
        for (old_idx, builtin_info) in bytecode.builtins.iter().enumerate() {
            // Remap type ID references within the builtin info
            let remapped_info = quiver_core::types::BuiltinInfo {
                name: builtin_info.name.clone(),
                param_type: remap_type_id(builtin_info.param_type, &type_remap),
                result_type: remap_type_id(builtin_info.result_type, &type_remap),
            };
            let new_idx = self.program.register_builtin_info(remapped_info);
            builtin_remap.insert(old_idx, new_idx);
        }

        // Merge functions (type_id is remapped by remap_function)
        for (old_idx, function) in bytecode.functions.iter().enumerate() {
            let remapped_function = remap_function(
                function.clone(),
                &constant_remap,
                &function_remap,
                &tuple_remap,
                &type_remap,
                &builtin_remap,
            );

            let new_idx = self.program.register_function(remapped_function);
            function_remap.insert(old_idx, new_idx);
        }

        // Compute deltas - only new items since before the merge
        let new_constants: Vec<Constant> =
            self.program.get_constants()[old_constants_len..].to_vec();
        let new_functions: Vec<Function> =
            self.program.get_functions()[old_functions_len..].to_vec();
        let new_tuples: Vec<quiver_core::types::TupleTypeInfo> =
            self.program.get_tuples()[old_tuples_len..].to_vec();
        let new_types: Vec<Type> = self.program.get_types()[old_types_len..].to_vec();
        let new_builtins: Vec<quiver_core::types::BuiltinInfo> =
            self.program.get_builtins()[old_builtins_len..].to_vec();

        // Only send update if there's new data
        if !new_constants.is_empty()
            || !new_functions.is_empty()
            || !new_tuples.is_empty()
            || !new_types.is_empty()
            || !new_builtins.is_empty()
        {
            // Recompute compatibility tables for the FULL merged program state
            let resource_names = self.program.collect_resource_names();

            let input = CompatibilityInput {
                types: self.program.get_types(),
                tuples: self.program.get_tuples(),
                functions: self.program.get_functions(),
                builtins: self.program.get_builtins(),
                resource_names: &resource_names,
            };

            let type_compatibility = compute_type_compatibility(&input);
            let (function_param_compatibility, builtin_param_compatibility) =
                compute_param_compatibility(&input);

            let update = ProgramUpdate {
                constants: new_constants,
                functions: new_functions,
                tuples: new_tuples,
                types: new_types,
                builtins: new_builtins,
                resources: resource_names,
                type_compatibility,
                function_param_compatibility,
                builtin_param_compatibility,
            };

            let update_cmd = Command::UpdateProgram(update);

            for worker in &mut self.workers {
                worker
                    .send(update_cmd.clone())
                    .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
            }
        }

        // Return remapped entry function index
        Ok(*function_remap
            .get(&entry_fn)
            .expect("Entry function should be in remap table"))
    }

    fn handle_event(&mut self, event: Event<E>) -> Result<(), EnvironmentError> {
        match event {
            Event::SpawnAction {
                caller,
                function_index,
                captures,
                argument,
                heap,
            } => self.handle_spawn(caller, function_index, captures, argument, heap),
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
            Event::ResultResponse {
                request_id,
                result,
                stats,
            } => self.handle_result_response(request_id, result, stats),
            Event::StatusesResponse { request_id, result } => {
                self.handle_statuses_response(request_id, result)
            }
            Event::ProcessTypesResponse { request_id, result } => {
                self.handle_process_types_response(request_id, result)
            }
            Event::StatsResponse { request_id, result } => {
                self.handle_stats_response(request_id, result)
            }
            Event::InfoResponse { request_id, result } => {
                self.handle_info_response(request_id, result)
            }
            Event::LocalsResponse { request_id, result } => {
                self.handle_locals_response(request_id, result)
            }
            Event::WorkerError { error } => self.handle_worker_error(error),
            Event::EffectRequest { process_id, effect } => {
                self.handle_effect_request(process_id, effect)
            }
            Event::_Phantom(_) => {
                // This variant is never actually used, only for maintaining generics
                unreachable!("_Phantom variant should never be constructed")
            }
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
        // Clean up resources for any completed processes
        for (process_id, result) in &results {
            if result.is_some() {
                // Process has completed (success or failure) - clean up its resources
                self.cleanup_process_resources(*process_id);
            }
        }
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
        argument: Value,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), EnvironmentError> {
        // Allocate new ProcessId
        let new_pid = self.allocate_process_id();

        // Choose worker: if any captures or argument are resources, spawn on the same worker
        // that owns the first resource (via the resource's owner process_id).
        // Otherwise use round-robin.
        let worker_id = captures
            .iter()
            .chain(std::iter::once(&argument))
            .find_map(|value| {
                if let Value::Resource(resource_id, _) = value {
                    // Look up the owner of this resource and find their worker
                    self.resource_ownership
                        .get(resource_id)
                        .and_then(|owner_pid| self.process_router.get(owner_pid).copied())
                } else {
                    None
                }
            })
            .unwrap_or_else(|| new_pid % self.workers.len());

        self.process_router.insert(new_pid, worker_id);

        // Transfer ownership of any resources in captures or argument to the new process
        for capture in &captures {
            self.transfer_resource_ownership(capture, new_pid);
        }
        self.transfer_resource_ownership(&argument, new_pid);

        // Spawn process on chosen worker with function, captures, and argument
        self.workers[worker_id]
            .send(Command::SpawnProcess {
                id: new_pid,
                function_index,
                captures,
                argument,
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

    /// Recursively transfer ownership of all resources in a value to a target process
    fn transfer_resource_ownership(&mut self, value: &Value, new_owner: ProcessId) {
        match value {
            Value::Resource(resource_id, _) => {
                self.resource_ownership.insert(*resource_id, new_owner);
            }
            Value::Tuple(_, fields) => {
                for field in fields.iter() {
                    self.transfer_resource_ownership(field, new_owner);
                }
            }
            Value::Function(_, captures) => {
                for capture in captures.iter() {
                    self.transfer_resource_ownership(capture, new_owner);
                }
            }
            _ => {} // Other value types don't contain resources
        }
    }

    fn handle_deliver(
        &mut self,
        target: ProcessId,
        message: Value,
        heap: Vec<Vec<u8>>,
    ) -> Result<(), EnvironmentError> {
        // Transfer ownership of any resources in the message to the target process
        self.transfer_resource_ownership(&message, target);

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
        stats: Option<quiver_core::executor::ExecutionStats>,
    ) -> Result<(), EnvironmentError> {
        // Stats come directly from the worker that executed the process
        self.pending_requests
            .insert(request_id, Some(RequestResult::Result(result, stats)));
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
        for (agg_id, aggregation) in &self.aggregations {
            if let Aggregation::Statuses(worker_requests) = aggregation
                && worker_requests.contains_key(&request_id)
            {
                aggregation_id = Some(*agg_id);
                break;
            }
        }

        if let Some(agg_id) = aggregation_id {
            // This is part of an aggregation - store the result
            if let Some(Aggregation::Statuses(worker_requests)) = self.aggregations.get_mut(&agg_id)
            {
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
                    self.aggregations.remove(&agg_id);
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

    fn handle_process_types_response(
        &mut self,
        request_id: u64,
        result: Result<HashMap<ProcessId, usize>, EnvironmentError>,
    ) -> Result<(), EnvironmentError> {
        let function_indices = result?;

        // Check if this request is part of an aggregation
        let mut aggregation_id = None;
        for (agg_id, aggregation) in &self.aggregations {
            if let Aggregation::ProcessTypes(worker_requests) = aggregation
                && worker_requests.contains_key(&request_id)
            {
                aggregation_id = Some(*agg_id);
                break;
            }
        }

        if let Some(agg_id) = aggregation_id {
            // This is part of an aggregation - store the raw function indices
            // First, insert and check if all results are collected
            let (all_collected, merged_indices, worker_req_ids) = {
                if let Some(Aggregation::ProcessTypes(worker_requests)) =
                    self.aggregations.get_mut(&agg_id)
                {
                    worker_requests.insert(request_id, Some(function_indices));

                    if worker_requests.values().all(|v| v.is_some()) {
                        // Merge all function indices from workers
                        let mut merged = HashMap::new();
                        for indices in worker_requests.values().flatten() {
                            merged.extend(indices.clone());
                        }
                        let req_ids: Vec<u64> = worker_requests.keys().copied().collect();
                        (true, Some(merged), req_ids)
                    } else {
                        // Still waiting for more results
                        self.pending_requests.insert(request_id, None);
                        (false, None, vec![])
                    }
                } else {
                    (false, None, vec![])
                }
            };

            if all_collected && let Some(merged) = merged_indices {
                // Enrich with actual types (now outside the mutable borrow)
                let enriched = self.enrich_process_types(merged);

                // Store the aggregated result
                self.pending_requests
                    .insert(agg_id, Some(RequestResult::ProcessTypes(enriched)));

                // Clean up aggregation and individual worker requests
                self.aggregations.remove(&agg_id);
                for worker_req_id in worker_req_ids {
                    self.pending_requests.remove(&worker_req_id);
                }
            }
        } else {
            // Single (non-aggregated) request - enrich and store
            let enriched = self.enrich_process_types(function_indices);
            self.pending_requests
                .insert(request_id, Some(RequestResult::ProcessTypes(enriched)));
        }

        Ok(())
    }

    /// Convert function indices to full process types by looking up callable types
    fn enrich_process_types(
        &self,
        function_indices: HashMap<ProcessId, usize>,
    ) -> HashMap<ProcessId, (Type, usize)> {
        function_indices
            .into_iter()
            .map(|(pid, function_index)| {
                // Try to get the actual process type from the function's callable type
                let process_type = self
                    .program
                    .get_function(function_index)
                    .and_then(|func| self.program.lookup_type(func.type_id))
                    .map(|callable| match callable {
                        Type::Callable {
                            result, receive, ..
                        } => Type::Process {
                            send: Some(*receive),
                            receive: Some(*result),
                        },
                        _ => Type::Process {
                            send: None,
                            receive: None,
                        },
                    })
                    .unwrap_or(Type::Process {
                        send: None,
                        receive: None,
                    });
                (pid, (process_type, function_index))
            })
            .collect()
    }

    fn handle_stats_response(
        &mut self,
        _request_id: u64,
        _result: Result<quiver_core::executor::ExecutionStats, EnvironmentError>,
    ) -> Result<(), EnvironmentError> {
        // Stats are now bundled with results from the worker directly
        // This handler is kept for completeness but shouldn't be called
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
        let binary_lookup = quiver_core::format::HeapAndProgramLookup {
            heap,
            program: &self.program,
        };
        quiver_core::format::format_value(value, &self.program, &binary_lookup)
    }

    /// Format a type for display
    pub fn format_type(&self, ty: &Type) -> String {
        quiver_core::format::format_type(&self.program, ty)
    }

    /// Format a type by its ID for display
    pub fn format_type_by_id(&self, type_id: usize) -> String {
        quiver_core::format::format_type_by_id(&self.program, type_id)
    }

    /// Get a reference to the program
    pub fn get_program(&self) -> &Program {
        &self.program
    }

    /// Get the formatted type for a process given its function index
    pub fn format_process_type(&self, function_index: usize) -> Option<String> {
        let process_type = self.get_process_type(function_index)?;
        Some(self.format_type(&process_type))
    }

    /// Get the type for a process given its function index
    pub fn get_process_type(&self, function_index: usize) -> Option<Type> {
        let func = self.program.get_function(function_index)?;
        let callable = self.program.lookup_type(func.type_id)?;
        match callable {
            Type::Callable {
                result, receive, ..
            } => Some(Type::Process {
                send: Some(*receive),
                receive: Some(*result),
            }),
            _ => None,
        }
    }

    /// Convert a runtime Value to its Type representation
    pub fn value_to_type(&mut self, value: &Value) -> Type {
        match value {
            Value::Integer(_) => Type::Integer,
            Value::Binary(_) => Type::Binary,
            Value::Reference(_) => Type::Reference,
            Value::Tuple(type_id, _) => Type::Tuple(*type_id),
            Value::Function(func_idx, _) => {
                // Get the callable type directly from function's type_id
                self.program
                    .get_function(*func_idx)
                    .and_then(|func| self.program.lookup_type(func.type_id).cloned())
                    .unwrap_or_else(|| Type::Union(vec![])) // Fallback for unknown functions
            }
            Value::Builtin(builtin_id) => {
                // Get the builtin info by index
                let builtin_info = self
                    .program
                    .get_builtins()
                    .get(*builtin_id)
                    .expect("Builtin should be registered");
                let param_type = builtin_info.param_type;
                let result_type = builtin_info.result_type;
                Type::Callable {
                    parameter: param_type,
                    result: result_type,
                    receive: self.program.never(), // Builtins don't receive values
                }
            }
            Value::Process(_, function_idx) => {
                // Get the process type from function's callable type
                self.get_process_type(*function_idx)
                    .unwrap_or_else(|| Type::Union(vec![])) // Fallback for unknown functions
            }
            Value::Resource(_, resource_type_id) => {
                // Look up resource name from program's resource names
                let resource_names = self.program.collect_resource_names();
                resource_names
                    .get(*resource_type_id)
                    .map(|name| Type::Resource(name.clone()))
                    .unwrap_or_else(|| Type::Resource(format!("Resource#{}", resource_type_id)))
            }
        }
    }

    /// Resolve a type alias and return the resolved type ID.
    /// Type parameters are resolved to type variable placeholders.
    /// This is useful for testing and displaying type aliases.
    pub fn resolve_type_alias(
        &mut self,
        type_aliases: &std::collections::HashMap<String, TypeAliasDef>,
        alias_name: &str,
    ) -> Result<usize, String> {
        // Convert type_aliases HashMap to a single scope for resolution
        let mut bindings = std::collections::HashMap::new();
        for (name, type_alias) in type_aliases {
            bindings.insert(name.clone(), Binding::TypeAlias(type_alias.clone()));
        }
        let scope = Scope::new(bindings, None, ScopeKind::Root);
        let scopes = vec![scope];

        resolve_type_alias_for_display(&scopes, alias_name).map_err(|e| format!("{:?}", e))
    }

    /// Handle effect request from a worker
    fn handle_effect_request(
        &mut self,
        process_id: ProcessId,
        effect: E,
    ) -> Result<(), EnvironmentError> {
        // Validate ownership for operations on existing resources
        if let Some(resource_id) = effect.resource_id() {
            // Check that the process owns this resource
            if let Some(owner) = self.resource_ownership.get(&resource_id)
                && *owner != process_id
            {
                return Err(EnvironmentError::Executor(
                    quiver_core::error::Error::InvalidArgument(format!(
                        "Process {} does not own resource {}",
                        process_id, resource_id
                    )),
                ));
            }
        }
        // Resource-creating operations (those that return None from resource_id()) don't need ownership checks

        // Execute the effect via the effect backend
        let effect_backend = self.effect_backend.as_mut().ok_or_else(|| {
            EnvironmentError::Executor(quiver_core::error::Error::InvalidArgument(
                "No effect backend available".to_string(),
            ))
        })?;

        // Execute effect (may return immediate result or submit for async processing)
        // The backend tracks pending operations by process_id
        match effect_backend.execute(process_id, effect) {
            Ok(Some(completion)) => {
                // Immediate completion - handle it now
                self.handle_effect_completion(process_id, completion)?;
            }
            Ok(None) => {
                // Async operation submitted - backend will track it and return via process_completions()
            }
            Err(e) => {
                // Operation failed - send error to worker
                let worker_id = self
                    .process_router
                    .get(&process_id)
                    .ok_or(EnvironmentError::ProcessNotFound(process_id))?;
                self.workers[*worker_id]
                    .send(Command::EffectCompletion {
                        process_id,
                        result: Err(format!("{:?}", e)),
                        heap: vec![],
                    })
                    .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
            }
        }

        Ok(())
    }

    /// Handle completed effect operation (generic version)
    fn handle_effect_completion(
        &mut self,
        process_id: ProcessId,
        result: Result<(Value, Vec<Vec<u8>>), quiver_core::effects::EffectError>,
    ) -> Result<(), EnvironmentError> {
        // If this was a resource-creating operation, register ownership
        if result.is_ok()
            && let Ok((Value::Resource(rid, _), _)) = &result
        {
            self.resource_ownership.insert(*rid, process_id);
        }

        // Unwrap the result and heap data
        let (result, heap) = match result {
            Ok((value, heap_data)) => (Ok(value), heap_data),
            Err(err) => (Err(format!("{}", err)), vec![]),
        };

        // Send completion to the worker
        let worker_id = self
            .process_router
            .get(&process_id)
            .ok_or(EnvironmentError::ProcessNotFound(process_id))?;

        self.workers[*worker_id]
            .send(Command::EffectCompletion {
                process_id,
                result,
                heap,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(())
    }

    /// Clean up all resources owned by a process
    /// This is called when a process completes (either successfully or with an error)
    fn cleanup_process_resources(&mut self, process_id: ProcessId) {
        if let Some(backend) = &mut self.effect_backend {
            // Find all resources owned by this process
            let resources: Vec<_> = self
                .resource_ownership
                .iter()
                .filter(|(_, owner)| **owner == process_id)
                .map(|(rid, _)| *rid)
                .collect();

            // Close each resource via the backend
            for resource_id in resources {
                backend.close_resource(resource_id);
                self.resource_ownership.remove(&resource_id);
            }
        }
    }
}
