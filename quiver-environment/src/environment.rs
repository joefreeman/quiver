use crate::WorkerId;
use crate::messages::{Command, Event, SubscriptionKind, SubscriptionPayload};
use crate::transport::WorkerHandle;
use quiver_compiler::compiler::{
    Binding, Scope, ScopeKind, TypeAliasDef, resolve_type_alias_for_display,
};
use quiver_core::bytecode::{Bytecode, Constant, Function, Instruction};
use quiver_core::compatibility::{
    CompatibilityInput, compute_canonical_tuples, compute_param_compatibility,
    compute_type_compatibility,
};
use quiver_core::effects::{Effect, EffectBackend, ResultTupleInfo};
use quiver_core::executor::ProgramUpdate;
use quiver_core::process::{ProcessId, ProcessInfo, ProcessStatus};
use quiver_core::program::Program;
use quiver_core::types::{NIL, OK, Type, TypeLookup};
use quiver_core::value::{ResourceId, Value};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

type WorkerRequestMap<T> = HashMap<u64, Option<HashMap<ProcessId, T>>>;

enum Aggregation {
    Statuses(WorkerRequestMap<ProcessStatus>),
    ProcessTypes(WorkerRequestMap<usize>), // Maps request_id -> Option<HashMap<ProcessId, function_index>>
    WorkerInfo(HashMap<u64, Option<quiver_core::process::WorkerInfo>>), // Maps request_id -> Option<WorkerInfo>
}

fn remap_type_id(id: usize, type_remap: &HashMap<usize, usize>) -> usize {
    *type_remap.get(&id).unwrap_or(&id)
}

/// Deep-copy a type from a source id space (`src_types` / `src_tuples`) into `program`,
/// returning an equivalent type whose every child id has been remapped into `program`'s id
/// space.
///
/// Types and tuples are mutually recursive: a type may reference tuples (`Type::Tuple`,
/// `Type::Partial`) while a tuple's fields reference types. We therefore import a node's
/// dependencies *before* registering the node itself, so every id it carries is already
/// remapped into `program`. This is essential because `register_type` / `register_tuple`
/// deduplicate by structural equality — registering a node with stale (source-space) child
/// ids would both store wrong references and defeat deduplication.
///
/// Recursion terminates because Quiver expresses recursive types with `Type::Cycle` (a depth,
/// not an id), so there are no id cycles between types and tuples. Per-id results are memoised
/// in `type_remap` / `tuple_remap`.
fn import_type_value(
    program: &mut Program,
    src_types: &[Type],
    src_tuples: &[quiver_core::types::TupleTypeInfo],
    type_remap: &mut HashMap<usize, usize>,
    tuple_remap: &mut HashMap<usize, usize>,
    ty: Type,
) -> Type {
    match ty {
        Type::Tuple(old_tuple_id) => Type::Tuple(import_tuple(
            program,
            src_types,
            src_tuples,
            type_remap,
            tuple_remap,
            old_tuple_id,
        )),
        Type::Partial { name, fields } => Type::Partial {
            name,
            fields: fields
                .into_iter()
                .map(|(fname, ftype)| {
                    (
                        fname,
                        import_type(
                            program,
                            src_types,
                            src_tuples,
                            type_remap,
                            tuple_remap,
                            ftype,
                        ),
                    )
                })
                .collect(),
        },
        Type::Union(type_ids) => Type::Union(
            type_ids
                .into_iter()
                .map(|t| import_type(program, src_types, src_tuples, type_remap, tuple_remap, t))
                .collect(),
        ),
        Type::Callable {
            parameter,
            result,
            receive,
        } => Type::Callable {
            parameter: import_type(
                program,
                src_types,
                src_tuples,
                type_remap,
                tuple_remap,
                parameter,
            ),
            result: import_type(
                program,
                src_types,
                src_tuples,
                type_remap,
                tuple_remap,
                result,
            ),
            receive: import_type(
                program,
                src_types,
                src_tuples,
                type_remap,
                tuple_remap,
                receive,
            ),
        },
        Type::Process { send, receive } => Type::Process {
            send: send
                .map(|t| import_type(program, src_types, src_tuples, type_remap, tuple_remap, t)),
            receive: receive
                .map(|t| import_type(program, src_types, src_tuples, type_remap, tuple_remap, t)),
        },
        other => other,
    }
}

/// Import the type at `old_id` in the source id space into `program`, returning its `program`
/// id. See [`import_type_value`] for the deep-copy contract.
fn import_type(
    program: &mut Program,
    src_types: &[Type],
    src_tuples: &[quiver_core::types::TupleTypeInfo],
    type_remap: &mut HashMap<usize, usize>,
    tuple_remap: &mut HashMap<usize, usize>,
    old_id: usize,
) -> usize {
    if let Some(&new_id) = type_remap.get(&old_id) {
        return new_id;
    }

    let remapped = import_type_value(
        program,
        src_types,
        src_tuples,
        type_remap,
        tuple_remap,
        src_types[old_id].clone(),
    );

    let new_id = program.register_type(remapped);
    type_remap.insert(old_id, new_id);
    new_id
}

/// Import the tuple type at `old_id` in the source id space into `program`, returning its
/// `program` id. See [`import_type_value`] for why dependencies are imported first.
fn import_tuple(
    program: &mut Program,
    src_types: &[Type],
    src_tuples: &[quiver_core::types::TupleTypeInfo],
    type_remap: &mut HashMap<usize, usize>,
    tuple_remap: &mut HashMap<usize, usize>,
    old_id: usize,
) -> usize {
    if let Some(&new_id) = tuple_remap.get(&old_id) {
        return new_id;
    }

    let info = src_tuples[old_id].clone();
    let fields: Vec<_> = info
        .fields
        .into_iter()
        .map(|(name, ftype)| {
            (
                name,
                import_type(
                    program,
                    src_types,
                    src_tuples,
                    type_remap,
                    tuple_remap,
                    ftype,
                ),
            )
        })
        .collect();

    let new_id = program.register_tuple(info.name, fields);
    tuple_remap.insert(old_id, new_id);
    new_id
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
    WorkerInfo(Vec<quiver_core::process::WorkerInfo>),
    ProcessTypes(HashMap<ProcessId, (Type, usize)>),
    ProcessInfo(Option<ProcessInfo>),
    Locals(Vec<ValueWithHeap>),
}

struct PendingAwait {
    expected_workers: HashSet<WorkerId>,
    responses: WorkerResponsesMap,
}

/// Environment-side state for a standing subscription. `per_worker` holds the latest payload pushed
/// by each worker; for a `ProcessStatuses` subscription (fanned out to all workers) these are merged
/// into the combined view, while a `ProcessInfo` subscription only ever has its single owning
/// worker's entry. Each push replaces that worker's entry (last-wins) and re-merges.
struct SubscriptionState {
    kind: SubscriptionKind,
    per_worker: HashMap<WorkerId, SubscriptionPayload>,
}

impl SubscriptionState {
    /// Collapse the per-worker payloads into the single user-facing result delivered to the
    /// subscriber.
    fn merge(&self) -> RequestResult {
        match self.kind {
            SubscriptionKind::ProcessStatuses => {
                let mut merged = HashMap::new();
                for payload in self.per_worker.values() {
                    if let SubscriptionPayload::ProcessStatuses(statuses) = payload {
                        merged.extend(statuses.clone());
                    }
                }
                RequestResult::Statuses(merged)
            }
            SubscriptionKind::ProcessInfo { .. } => {
                let info = self.per_worker.values().find_map(|payload| match payload {
                    SubscriptionPayload::ProcessInfo(info) => Some(info.clone()),
                    _ => None,
                });
                RequestResult::ProcessInfo(info.flatten())
            }
            SubscriptionKind::WorkerInfo => {
                let mut workers: Vec<quiver_core::process::WorkerInfo> = self
                    .per_worker
                    .values()
                    .filter_map(|payload| match payload {
                        SubscriptionPayload::WorkerInfo(info) => Some(info.clone()),
                        _ => None,
                    })
                    .collect();
                workers.sort_by_key(|w| w.worker_id);
                RequestResult::WorkerInfo(workers)
            }
        }
    }
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
    // Standing subscriptions, keyed by subscription id. Distinct from `pending_requests`/
    // `aggregations`: these are never torn down on response — they live until `unsubscribe`.
    subscriptions: HashMap<u64, SubscriptionState>,
    // Latest merged result per subscription, awaiting delivery to the subscriber. Last-wins: a
    // slow consumer only ever sees the most recent snapshot, never a backlog. Drained by
    // `take_subscription_updates`.
    subscription_updates: HashMap<u64, RequestResult>,
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
            subscriptions: HashMap::new(),
            subscription_updates: HashMap::new(),
            next_request_id: 0,
            next_process_id: 0,
            effect_backend: None,
            resource_ownership: HashMap::new(),
        }
    }

    /// Set the effect backend for executing platform-specific effects
    pub fn set_effect_backend(&mut self, backend: Box<dyn EffectBackend<E = E>>) {
        self.effect_backend = Some(backend);
        // Hand the backend the type ids it needs for any program already loaded (the backend may
        // be attached after the program). Re-pushed on each subsequent merge; see merge_bytecode.
        self.push_type_ids_to_backend();
    }

    /// Push the type ids the backend needs to stamp real values onto effect results: the resource
    /// type names, and each builtin's composite-result tuple id. Called whenever the program or
    /// the backend changes; tables are append-only so this only ever grows them.
    fn push_type_ids_to_backend(&mut self) {
        let resources = self.program.collect_resource_names();
        let results = composite_result_infos(&self.program);
        if let Some(backend) = self.effect_backend.as_mut() {
            backend.set_type_ids(&resources, &results);
        }
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
    /// Stats are included in the response if the executor has profiling enabled.
    /// `keep_locals` is the REPL's keep-set (see [`Command::GetResult`]); pass `None` for
    /// non-REPL callers that have no orphaned locals to reclaim.
    pub fn request_result(
        &mut self,
        pid: ProcessId,
        keep_locals: Option<Vec<usize>>,
    ) -> Result<u64, EnvironmentError> {
        let request_id = self.allocate_request_id();
        let worker_id = *self
            .process_router
            .get(&pid)
            .ok_or(EnvironmentError::ProcessNotFound(pid))?;

        self.workers[worker_id]
            .send(Command::GetResult {
                request_id,
                process_id: pid,
                keep_locals,
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

    /// Request worker info (memory/heap snapshot) from all workers
    /// Returns a single aggregation ID that will collect results from all workers
    pub fn request_worker_info(&mut self) -> Result<u64, EnvironmentError> {
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
            .insert(aggregation_id, Aggregation::WorkerInfo(worker_requests));

        // Mark aggregation as pending
        self.pending_requests.insert(aggregation_id, None);

        // Send requests to workers (using ordered Vec)
        for (i, worker) in self.workers.iter_mut().enumerate() {
            let request_id = request_ids[i];
            worker
                .send(Command::GetWorkerInfo { request_id })
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

    /// Subscribe to all process statuses across all workers. Returns the subscription id; the
    /// subscriber receives the merged statuses via [`Self::take_subscription_updates`], first as an
    /// initial snapshot and then on every change, until [`Self::unsubscribe`].
    pub fn subscribe_process_statuses(&mut self) -> Result<u64, EnvironmentError> {
        let subscription_id = self.allocate_request_id();
        self.subscriptions.insert(
            subscription_id,
            SubscriptionState {
                kind: SubscriptionKind::ProcessStatuses,
                per_worker: HashMap::new(),
            },
        );
        for worker in &mut self.workers {
            worker
                .send(Command::Subscribe {
                    subscription_id,
                    kind: SubscriptionKind::ProcessStatuses,
                })
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }
        Ok(subscription_id)
    }

    /// Subscribe to live worker info (executor heap/memory snapshots) across all workers. The
    /// subscriber receives a `Vec<WorkerInfo>` sorted by worker id, refreshed on every change.
    pub fn subscribe_worker_info(&mut self) -> Result<u64, EnvironmentError> {
        let subscription_id = self.allocate_request_id();
        self.subscriptions.insert(
            subscription_id,
            SubscriptionState {
                kind: SubscriptionKind::WorkerInfo,
                per_worker: HashMap::new(),
            },
        );
        for worker in &mut self.workers {
            worker
                .send(Command::Subscribe {
                    subscription_id,
                    kind: SubscriptionKind::WorkerInfo,
                })
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }
        Ok(subscription_id)
    }

    /// Subscribe to detailed info for a single process. Routed to the owning worker only.
    pub fn subscribe_process_info(&mut self, pid: ProcessId) -> Result<u64, EnvironmentError> {
        let worker_id = *self
            .process_router
            .get(&pid)
            .ok_or(EnvironmentError::ProcessNotFound(pid))?;
        let subscription_id = self.allocate_request_id();
        self.subscriptions.insert(
            subscription_id,
            SubscriptionState {
                kind: SubscriptionKind::ProcessInfo { process_id: pid },
                per_worker: HashMap::new(),
            },
        );
        self.workers[worker_id]
            .send(Command::Subscribe {
                subscription_id,
                kind: SubscriptionKind::ProcessInfo { process_id: pid },
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        Ok(subscription_id)
    }

    /// Cancel a subscription. Broadcast to all workers (a worker without it ignores the command),
    /// which keeps this correct regardless of which workers the subscription fanned out to. Drops
    /// any update not yet taken.
    pub fn unsubscribe(&mut self, subscription_id: u64) -> Result<(), EnvironmentError> {
        if self.subscriptions.remove(&subscription_id).is_none() {
            return Ok(());
        }
        self.subscription_updates.remove(&subscription_id);
        for worker in &mut self.workers {
            worker
                .send(Command::Unsubscribe { subscription_id })
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }
        Ok(())
    }

    /// Take and clear all pending subscription updates: `(subscription_id, merged_result)` pairs.
    /// The caller looks each id up against its registered (persistent) callback. Order is
    /// unspecified; at most one entry per subscription (last-wins).
    pub fn take_subscription_updates(&mut self) -> Vec<(u64, RequestResult)> {
        std::mem::take(&mut self.subscription_updates)
            .into_iter()
            .collect()
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

        // Merge types and tuples. They are mutually recursive (a type may reference tuples and
        // vice versa), so we import every node via `import_type` / `import_tuple`, which import
        // each node's dependencies before registering it. Iterating over all indices guarantees
        // every source index ends up in the remap tables (including those reachable only through
        // function instructions), and memoisation keeps repeated visits cheap.
        for old_idx in 0..bytecode.types.len() {
            import_type(
                &mut self.program,
                &bytecode.types,
                &bytecode.tuples,
                &mut type_remap,
                &mut tuple_remap,
                old_idx,
            );
        }
        for old_idx in 0..bytecode.tuples.len() {
            import_tuple(
                &mut self.program,
                &bytecode.types,
                &bytecode.tuples,
                &mut type_remap,
                &mut tuple_remap,
                old_idx,
            );
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
            let canonical_tuples = compute_canonical_tuples(self.program.get_tuples());
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
                canonical_tuples,
            };

            let update_cmd = Command::UpdateProgram(update);

            for worker in &mut self.workers {
                worker
                    .send(update_cmd.clone())
                    .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
            }

            // Hand the backend the (now possibly larger) set of type ids it needs to stamp real
            // values onto effect results.
            self.push_type_ids_to_backend();
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
            Event::WorkerInfoResponse { request_id, result } => {
                self.handle_worker_info_response(request_id, result)
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
            Event::SubscriptionUpdate {
                subscription_id,
                worker_id,
                payload,
            } => self.handle_subscription_update(subscription_id, worker_id, payload),
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

    fn handle_worker_info_response(
        &mut self,
        request_id: u64,
        result: Result<quiver_core::process::WorkerInfo, EnvironmentError>,
    ) -> Result<(), EnvironmentError> {
        let info = result?;

        // Check if this request is part of an aggregation
        let mut aggregation_id = None;
        for (agg_id, aggregation) in &self.aggregations {
            if let Aggregation::WorkerInfo(worker_requests) = aggregation
                && worker_requests.contains_key(&request_id)
            {
                aggregation_id = Some(*agg_id);
                break;
            }
        }

        if let Some(agg_id) = aggregation_id {
            // This is part of an aggregation - store the result
            if let Some(Aggregation::WorkerInfo(worker_requests)) =
                self.aggregations.get_mut(&agg_id)
            {
                worker_requests.insert(request_id, Some(info));

                // Check if all results are collected
                if worker_requests.values().all(|v| v.is_some()) {
                    // Collect all results into a single Vec, sorted by worker_id for stability
                    let mut merged: Vec<quiver_core::process::WorkerInfo> =
                        worker_requests.values().flatten().cloned().collect();
                    merged.sort_by_key(|w| w.worker_id);

                    // Store the aggregated result
                    self.pending_requests
                        .insert(agg_id, Some(RequestResult::WorkerInfo(merged)));

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
            // Not part of an aggregation - store directly (single worker)
            self.pending_requests
                .insert(request_id, Some(RequestResult::WorkerInfo(vec![info])));
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

    fn handle_subscription_update(
        &mut self,
        subscription_id: u64,
        worker_id: WorkerId,
        payload: SubscriptionPayload,
    ) -> Result<(), EnvironmentError> {
        // A late update for an already-cancelled subscription: just drop it.
        let Some(state) = self.subscriptions.get_mut(&subscription_id) else {
            return Ok(());
        };
        state.per_worker.insert(worker_id, payload);
        let merged = state.merge();
        self.subscription_updates.insert(subscription_id, merged);
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

    /// Deep-copy a type whose child ids reference this environment's program into `target`,
    /// returning an equivalent type with every id remapped into `target`'s id space.
    ///
    /// Process types (built in the environment's id space by [`Self::get_process_type`]) are
    /// folded into a REPL's own program this way, so the REPL program stays internally
    /// consistent — and its emitted bytecode self-contained — rather than carrying dangling
    /// references into the environment's id space.
    pub fn import_type_into(&self, target: &mut Program, ty: Type) -> Type {
        let mut type_remap = HashMap::new();
        let mut tuple_remap = HashMap::new();
        import_type_value(
            target,
            self.program.get_types(),
            self.program.get_tuples(),
            &mut type_remap,
            &mut tuple_remap,
            ty,
        )
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
        // Validate ownership for operations on existing resources. A violation is reported
        // back to the requesting process as a runtime error (rather than propagated up the
        // environment loop), so the process fails cleanly instead of hanging on a completion
        // that never arrives.
        if let Some(resource_id) = effect.resource_id()
            && let Some(owner) = self.resource_ownership.get(&resource_id)
            && *owner != process_id
        {
            return self.report_effect_error(
                process_id,
                format!(
                    "Process {} does not own resource {}",
                    process_id, resource_id
                ),
            );
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
                // Operation failed - report the error back to the requesting process
                self.report_effect_error(process_id, format!("{:?}", e))?;
            }
        }

        Ok(())
    }

    /// Report an effect failure back to the requesting process as a runtime error.
    ///
    /// The process is suspended waiting for its effect to complete; delivering an error
    /// completion lets it resume and fail, rather than hanging indefinitely.
    fn report_effect_error(
        &mut self,
        process_id: ProcessId,
        message: String,
    ) -> Result<(), EnvironmentError> {
        let worker_id = self
            .process_router
            .get(&process_id)
            .ok_or(EnvironmentError::ProcessNotFound(process_id))?;
        self.workers[*worker_id]
            .send(Command::EffectCompletion {
                process_id,
                result: Err(message),
                heap: vec![],
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
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

#[cfg(test)]
impl<E: Effect> Environment<E> {
    /// Test helper: merge a bytecode's types/tuples and return the merged index of its first
    /// tuple. Mirrors the type/tuple half of `merge_bytecode` without requiring workers.
    fn merge_tuples_for_test(&mut self, bytecode: Bytecode) -> usize {
        let mut type_remap: HashMap<usize, usize> = HashMap::new();
        let mut tuple_remap: HashMap<usize, usize> = HashMap::new();
        for old_idx in 0..bytecode.types.len() {
            import_type(
                &mut self.program,
                &bytecode.types,
                &bytecode.tuples,
                &mut type_remap,
                &mut tuple_remap,
                old_idx,
            );
        }
        for old_idx in 0..bytecode.tuples.len() {
            import_tuple(
                &mut self.program,
                &bytecode.types,
                &bytecode.tuples,
                &mut type_remap,
                &mut tuple_remap,
                old_idx,
            );
        }
        tuple_remap[&0]
    }
}

/// For each builtin whose result has exactly one top-level non-trivial tuple variant, the builtin
/// name paired with the type ids a backend needs to stamp that result (see [`ResultTupleInfo`]):
/// the outer tuple id, plus every named tuple variant reachable in the result type (e.g. the
/// `File`/`Dir`/… kind tags nested inside). `NIL`/`OK` are excluded — the backend builds those
/// directly. A result with zero or several top-level tuple variants is skipped (the latter would
/// need per-variant outer selection, which no current builtin requires).
fn composite_result_infos(program: &Program) -> Vec<(String, ResultTupleInfo)> {
    let types = program.get_types();
    let mut out = Vec::new();
    for builtin in program.get_builtins() {
        let mut tuple_ids = Vec::new();
        collect_result_tuple_ids(types, builtin.result_type, &mut tuple_ids);
        if let [tuple_id] = tuple_ids[..] {
            let mut variants = HashMap::new();
            collect_named_variants(
                program,
                builtin.result_type,
                &mut HashSet::new(),
                &mut variants,
            );
            out.push((builtin.name.clone(), ResultTupleInfo { tuple_id, variants }));
        }
    }
    out
}

/// Collect the top-level non-trivial (non-`NIL`/`OK`) tuple ids of `type_id`, descending through
/// unions but *not* into tuple fields (those are the nested variants — see below).
fn collect_result_tuple_ids(types: &[Type], type_id: usize, out: &mut Vec<usize>) {
    match types.get(type_id) {
        Some(Type::Tuple(tuple_id)) if *tuple_id != NIL && *tuple_id != OK => out.push(*tuple_id),
        Some(Type::Union(members)) => {
            for &member in members {
                collect_result_tuple_ids(types, member, out);
            }
        }
        _ => {}
    }
}

/// Collect every *nullary* named tuple reachable in `type_id` (descending through unions and into
/// tuple fields), keyed by name — e.g. the `File`/`Dir`/`Symlink`/`Other` kind tags nested in a
/// directory entry's result.
///
/// Restricted to nullary (field-less) tags on purpose: a nullary named tuple is *nominal* — its
/// name is its complete identity, since `register_tuple` dedups `(name, [])` to a single id (just
/// as the type system identifies resources by name alone). A field-bearing tuple is *structural*,
/// identified by its full `(name, fields)` shape, so two such variants could share a name; those
/// are deliberately excluded rather than collide here. A backend that ever needs to produce one
/// would error in `result_info`/`kind_tag` rather than be mis-stamped (selecting among structural
/// variants needs value-directed resolution, which nothing requires yet).
fn collect_named_variants(
    program: &Program,
    type_id: usize,
    visited: &mut HashSet<usize>,
    out: &mut HashMap<String, usize>,
) {
    if !visited.insert(type_id) {
        return;
    }
    match program.get_types().get(type_id) {
        Some(Type::Tuple(tuple_id)) => {
            let tuple = &program.get_tuples()[*tuple_id];
            if let Some(name) = &tuple.name
                && tuple.fields.is_empty()
            {
                out.insert(name.clone(), *tuple_id);
            }
            let field_types: Vec<usize> = tuple.fields.iter().map(|(_, t)| *t).collect();
            for field_type in field_types {
                collect_named_variants(program, field_type, visited, out);
            }
        }
        Some(Type::Union(members)) => {
            for member in members.clone() {
                collect_named_variants(program, member, visited, out);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quiver_core::bytecode::Bytecode;
    use quiver_core::types::TupleTypeInfo;

    // Minimal effect for constructing an Environment in tests; no effects are exercised.
    #[derive(Clone, Debug, Serialize, Deserialize)]
    enum TestEffect {}

    impl Effect for TestEffect {
        fn resource_id(&self) -> Option<ResourceId> {
            None
        }
    }

    /// A bytecode whose single tuple `[a, b]` has both fields typed as the type at
    /// `int_index`. Leading `Type::Reference` filler entries shift that index so the tuple's
    /// field references only resolve correctly if they are remapped during the merge.
    fn pair_of_ints_bytecode(int_index: usize) -> Bytecode {
        let mut types = vec![Type::Reference; int_index];
        types.push(Type::Integer);
        Bytecode {
            constants: vec![],
            functions: vec![],
            builtins: vec![],
            entry: None,
            tuples: vec![TupleTypeInfo {
                name: None,
                fields: vec![(None, int_index), (None, int_index)],
            }],
            types,
            resources: vec![],
        }
    }

    /// Merging a second, independently-compiled program must not corrupt the field type
    /// references of its tuples. Before the fix, tuple fields were remapped against an
    /// empty type table (tuples were merged before types), so the second program's `[a, b]`
    /// tuple resolved its int fields to whatever happened to sit at the stale index.
    #[test]
    fn merged_tuple_fields_keep_their_int_type() {
        let mut env: Environment<TestEffect> = Environment::new(vec![]);

        // First program: `'int` at index 0, tuple fields point at 0.
        let first = env.merge_tuples_for_test(pair_of_ints_bytecode(0));
        // Second program: `'int` at index 1 (with filler at 0), tuple fields point at 1.
        let second = env.merge_tuples_for_test(pair_of_ints_bytecode(1));

        for tuple_id in [first, second] {
            let info = env.program.get_tuples()[tuple_id].clone();
            for (_, field_type_id) in &info.fields {
                assert_eq!(
                    env.program.get_types()[*field_type_id],
                    Type::Integer,
                    "merged tuple {tuple_id} field should resolve to 'int"
                );
            }
        }
    }

    /// Process types (for `@N` references) are built in the environment's id space, but are
    /// injected into a REPL's own, independent program. `import_type_into` must deep-copy them
    /// so their child ids land in the target program — otherwise the REPL program would carry
    /// references dangling into the environment's table (the cause of an out-of-bounds panic
    /// when its bytecode was later merged back).
    #[test]
    fn process_type_children_are_deep_imported() {
        let mut env: Environment<TestEffect> = Environment::new(vec![]);

        // Give the environment program an `'int` at a non-trivial index, then describe a
        // process that sends and receives it — exactly the shape `enrich_process_types` builds.
        let int_id = env.program.register_type(Type::Integer);
        let process_type = Type::Process {
            send: Some(int_id),
            receive: Some(int_id),
        };

        // A REPL's fresh program where that environment id is not yet meaningful.
        let mut repl_program = Program::new();
        let imported = env.import_type_into(&mut repl_program, process_type);

        let Type::Process {
            send: Some(send),
            receive: Some(receive),
        } = imported
        else {
            panic!("expected a process type with send and receive");
        };
        // The child ids now index the REPL program and resolve back to `'int`.
        assert_eq!(repl_program.get_types()[send], Type::Integer);
        assert_eq!(repl_program.get_types()[receive], Type::Integer);
    }
}
