use crate::WorkerId;
use crate::messages::{Command, Event, SubscriptionKind, SubscriptionPayload};
use crate::transport::WorkerHandle;
use quiver_compiler::compiler::{
    Bindings, Scope, ScopeKind, TypeAliasDef, resolve_type_alias_for_display,
};
use quiver_core::bytecode::{Bytecode, Constant, Function};
use quiver_core::compatibility::{
    CompatibilityInput, CompatibilityTables, compute_canonical_tuples, compute_field_offsets,
};
use quiver_core::effects::{Effect, EffectBackend, ResultTupleInfo};
use quiver_core::executor::{ProgramUpdate, TableUpdate};
use quiver_core::process::{
    ProcessAdjacency, ProcessCategory, ProcessId, ProcessInfo, ProcessStatus,
};
use quiver_core::program::Program;
use quiver_core::types::{NIL, OK, Type, TypeLookup};
use quiver_core::value::{ResourceId, Value};
use quiver_core::wire::WireValue;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

type WorkerRequestMap<T> = HashMap<u64, Option<HashMap<ProcessId, T>>>;

enum Aggregation {
    Statuses(WorkerRequestMap<ProcessStatus>),
    ProcessTypes(WorkerRequestMap<usize>), // Maps request_id -> Option<HashMap<ProcessId, function_index>>
    WorkerInfo(HashMap<u64, Option<quiver_core::process::WorkerInfo>>), // Maps request_id -> Option<WorkerInfo>
}

/// Phase of an in-flight reclamation round. The
/// round is a two-phase handshake that *creates* quiescence — workers run autonomously, so
/// there is no natural idle point to detect: first pause every worker, then snapshot the
/// now-frozen process graph.
enum CollectionPhase {
    /// Awaiting each worker's `CollectionReady` ack (i.e. every worker paused). FIFO makes
    /// this a barrier: once all acks are in, every pre-pause send has been routed.
    Pausing,
    /// Awaiting each worker's `AdjacencyResponse` (its slice of the graph).
    Collecting,
    /// Code phase (rounds that include one, after the process sweep): awaiting each
    /// worker's `CodeRootsResponse`.
    CollectingCode,
}

struct CollectionState {
    phase: CollectionPhase,
    /// Correlates this round's responses; a stale response from a prior round is ignored.
    request_id: u64,
    /// Workers not yet heard from in the current phase.
    pending: HashSet<WorkerId>,
    /// Graph slices accumulated during `Collecting`.
    adjacency: Vec<ProcessAdjacency>,
    /// Whether this round runs the code phase after the process sweep.
    include_code: bool,
    /// Table lengths when the round began: entries registered mid-round are outside
    /// this sweep (the workers' root walk predates them).
    snapshot_functions: usize,
    snapshot_constants: usize,
    /// Code roots accumulated during `CollectingCode`.
    code_functions: HashSet<usize>,
    code_constants: HashSet<usize>,
    /// Ids a mid-round `merge_bytecode` touched (its full remap image, revivals
    /// included): the merged code may reference them statically, and the workers'
    /// walk cannot have seen it — excluded from this round's dead set.
    code_exclusion_functions: HashSet<usize>,
    code_exclusion_constants: HashSet<usize>,
}

/// Default spawns since the last reclamation round after which one is auto-triggered. The
/// pause barrier creates its own quiescence, so the trigger need not wait for an idle
/// moment; this just bounds how many tombstones may accumulate between rounds. Overridable
/// via [`Environment::set_collection_threshold`].
const DEFAULT_COLLECTION_THRESHOLD: usize = 256;

/// Default function+constant registrations since the last code sweep after which the next
/// reclamation round includes a code phase. Growth-based: an environment that stops
/// registering code stops paying for sweeps. Overridable via
/// [`Environment::set_code_collection_threshold`].
const DEFAULT_CODE_COLLECTION_THRESHOLD: usize = 4096;

/// Trace the process graph and return the tombstones that are unreachable from any root and
/// may be reclaimed. Roots are the pids that
/// `Root`-category (live/persistent) processes reference; reachability follows outgoing edges
/// — including through a tombstone's surviving `result`/`state`, since a late `!p`/`?p`
/// exposes those — to a fixpoint. A tombstone not reached is unobservable and collectible;
/// this naturally sweeps cycles of mutually-referencing dead processes that refcounting can't.
fn compute_sweep(adjacency: &[ProcessAdjacency]) -> Vec<ProcessId> {
    let mut edges: HashMap<ProcessId, &[ProcessId]> = HashMap::new();
    let mut tombstones: HashSet<ProcessId> = HashSet::new();
    let mut stack: Vec<ProcessId> = Vec::new();
    for entry in adjacency {
        edges.insert(entry.pid, &entry.outgoing);
        match entry.category {
            // A root's referenced pids seed the mark set (the root itself is never swept).
            ProcessCategory::Root => stack.extend(entry.outgoing.iter().copied()),
            ProcessCategory::Tombstone => {
                tombstones.insert(entry.pid);
            }
        }
    }
    let mut marked: HashSet<ProcessId> = HashSet::new();
    while let Some(pid) = stack.pop() {
        if !marked.insert(pid) {
            continue;
        }
        if let Some(outgoing) = edges.get(&pid) {
            stack.extend(outgoing.iter().copied());
        }
    }
    tombstones
        .into_iter()
        .filter(|pid| !marked.contains(pid))
        .collect()
}

/// Collect the code a host-held request result keeps alive — the environment's slice of
/// the code-reclamation root set (worker heaps are walked worker-side).
fn collect_request_result_refs(
    result: &RequestResult,
    functions: &mut HashSet<usize>,
    constants: &mut HashSet<usize>,
) {
    match result {
        RequestResult::Result(Ok(value), _) => value.collect_code_refs(functions, constants),
        RequestResult::Locals(values) => {
            for value in values {
                value.collect_code_refs(functions, constants);
            }
        }
        RequestResult::ProcessInfo(Some(info)) => {
            if let Some(Ok(value)) = &info.result {
                value.collect_code_refs(functions, constants);
            }
        }
        _ => {}
    }
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
    src: &TypeSource,
    type_remap: &mut HashMap<usize, usize>,
    tuple_remap: &mut HashMap<usize, usize>,
    ty: Type,
) -> Type {
    match ty {
        Type::Tuple(old_tuple_id) => Type::Tuple(import_tuple(
            program,
            src,
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
                        import_type(program, src, type_remap, tuple_remap, ftype),
                    )
                })
                .collect(),
        },
        Type::Union(type_ids) => Type::Union(
            type_ids
                .into_iter()
                .map(|t| import_type(program, src, type_remap, tuple_remap, t))
                .collect(),
        ),
        Type::Callable {
            parameter,
            result,
            receive,
            states,
        } => Type::Callable {
            parameter: import_type(program, src, type_remap, tuple_remap, parameter),
            result: import_type(program, src, type_remap, tuple_remap, result),
            receive: import_type(program, src, type_remap, tuple_remap, receive),
            states: states.map(|t| import_type(program, src, type_remap, tuple_remap, t)),
        },
        Type::Process {
            send,
            receive,
            state,
        } => Type::Process {
            send: send.map(|t| import_type(program, src, type_remap, tuple_remap, t)),
            receive: receive.map(|t| import_type(program, src, type_remap, tuple_remap, t)),
            state: state.map(|t| import_type(program, src, type_remap, tuple_remap, t)),
        },
        Type::Annotated {
            base,
            exact,
            entries,
        } => {
            // Annotation rows carry a base type, per-entry value types, and key ids;
            // keys remap by name (like the merge's instruction remap). Entries stay
            // sorted by key id — the visibility walks binary-search them.
            let base = import_type(program, src, type_remap, tuple_remap, base);
            let mut entries: Vec<(usize, usize)> = entries
                .into_iter()
                .map(|(key, value_type)| {
                    let name = src
                        .annotation_keys
                        .get(key)
                        .expect("annotation key id without a name in the source id space");
                    let new_key = program.register_annotation_key(name);
                    let new_value = import_type(program, src, type_remap, tuple_remap, value_type);
                    (new_key, new_value)
                })
                .collect();
            entries.sort_by_key(|(key, _)| *key);
            Type::Annotated {
                base,
                exact,
                entries,
            }
        }
        other => other,
    }
}

/// Import the type at `old_id` in the source id space into `program`, returning its `program`
/// id. See [`import_type_value`] for the deep-copy contract.
/// A source id space to import types from: the tables `import_type_value` needs to
/// deep-remap a type tree, including annotation-row keys (which remap by name).
struct TypeSource<'a> {
    types: &'a [Type],
    tuples: &'a [quiver_core::types::TupleTypeInfo],
    annotation_keys: &'a [String],
}

fn import_type(
    program: &mut Program,
    src: &TypeSource,
    type_remap: &mut HashMap<usize, usize>,
    tuple_remap: &mut HashMap<usize, usize>,
    old_id: usize,
) -> usize {
    if let Some(&new_id) = type_remap.get(&old_id) {
        return new_id;
    }

    let remapped = import_type_value(
        program,
        src,
        type_remap,
        tuple_remap,
        src.types[old_id].clone(),
    );

    let new_id = program.register_type(remapped);
    type_remap.insert(old_id, new_id);
    new_id
}

/// Import the tuple type at `old_id` in the source id space into `program`, returning its
/// `program` id. See [`import_type_value`] for why dependencies are imported first.
fn import_tuple(
    program: &mut Program,
    src: &TypeSource,
    type_remap: &mut HashMap<usize, usize>,
    tuple_remap: &mut HashMap<usize, usize>,
    old_id: usize,
) -> usize {
    if let Some(&new_id) = tuple_remap.get(&old_id) {
        return new_id;
    }

    let info = src.tuples[old_id].clone();
    let fields: Vec<_> = info
        .fields
        .into_iter()
        .map(|(name, ftype)| {
            (
                name,
                import_type(program, src, type_remap, tuple_remap, ftype),
            )
        })
        .collect();

    let new_id = program.register_tuple(info.name, fields);
    tuple_remap.insert(old_id, new_id);
    new_id
}

// Type aliases for complex types
pub type RuntimeResult = Result<WireValue, quiver_core::error::Error>;
pub type ProcessResultsMap = HashMap<ProcessId, Option<RuntimeResult>>;
pub type WorkerResponsesMap = HashMap<WorkerId, ProcessResultsMap>;
pub type LocalsResult = Result<Vec<WireValue>, EnvironmentError>;

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
        Result<WireValue, quiver_core::error::Error>,
        Option<quiver_core::executor::ExecutionStats>,
    ),
    Statuses(HashMap<ProcessId, ProcessStatus>),
    WorkerInfo(Vec<quiver_core::process::WorkerInfo>),
    ProcessTypes(HashMap<ProcessId, (Type, usize)>),
    ProcessInfo(Option<ProcessInfo>),
    Locals(Vec<WireValue>),
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
    /// The runtime-delivered vocabulary the host's registry declares (crash shapes,
    /// the reactive wakeup, stream events) — resolved against the merged program at
    /// each merge (`Program::runtime_tables`).
    runtime_declarations: quiver_core::builtins::RuntimeDeclarations,
    resource_ownership: HashMap<ResourceId, ProcessId>,

    /// Compatibility tables for the merged program, extended incrementally at each merge
    /// (the program grows append-only) and shipped whole to the workers.
    compatibility: CompatibilityTables,

    // Process reclamation. At most one round runs
    // at a time; `spawns_since_collection` drives the auto-trigger, `reclaimed_total` is a
    // cumulative metric / test hook.
    collection: Option<CollectionState>,
    spawns_since_collection: usize,
    collection_threshold: usize,
    reclaimed_total: usize,
    // Code-reclamation state: growth since the last code sweep drives the trigger, the
    // request flag forces one on the next round, and the totals are metric/test hooks.
    code_registered_since_sweep: usize,
    code_collection_threshold: usize,
    code_collection_requested: bool,
    code_reclaimed_functions_total: usize,
    code_reclaimed_constants_total: usize,
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
            runtime_declarations: quiver_core::builtins::RuntimeDeclarations::default(),
            resource_ownership: HashMap::new(),
            compatibility: CompatibilityTables::default(),
            collection: None,
            spawns_since_collection: 0,
            collection_threshold: DEFAULT_COLLECTION_THRESHOLD,
            reclaimed_total: 0,
            code_registered_since_sweep: 0,
            code_collection_threshold: DEFAULT_CODE_COLLECTION_THRESHOLD,
            code_collection_requested: false,
            code_reclaimed_functions_total: 0,
            code_reclaimed_constants_total: 0,
        }
    }

    /// Set how many spawns since the last reclamation round trigger the next one. Lower
    /// values reclaim more eagerly.
    pub fn set_collection_threshold(&mut self, threshold: usize) {
        self.collection_threshold = threshold;
    }

    /// Set how many function/constant registrations since the last code sweep make the
    /// next reclamation round include a code phase. Lower values sweep more eagerly.
    pub fn set_code_collection_threshold(&mut self, threshold: usize) {
        self.code_collection_threshold = threshold;
    }

    /// Request a code phase on the next reclamation round (and start one if none is in
    /// flight). Answers whether a round was started — `false` means one was already
    /// running, and the flag applies to the next.
    pub fn start_code_collection(&mut self) -> Result<bool, EnvironmentError> {
        self.code_collection_requested = true;
        if self.collection.is_some() {
            return Ok(false);
        }
        self.start_collection()
    }

    /// Functions and constants reclaimed by code sweeps so far (a test/metric hook).
    pub fn code_reclaimed_totals(&self) -> (usize, usize) {
        (
            self.code_reclaimed_functions_total,
            self.code_reclaimed_constants_total,
        )
    }

    /// Whether a function slot in the merged program is currently a reclaimed stub
    /// (a test/metric hook).
    pub fn function_stubbed(&self, index: usize) -> bool {
        self.program.function_stubbed(index)
    }

    /// Set the effect backend for executing platform-specific effects
    /// Install the host's runtime-delivered vocabulary (from its builtin registry:
    /// `registry.runtime_declarations()`), resolved against the merged program at
    /// each merge. Without it, crash payloads degrade to bare nils and stream
    /// selects cannot be served — call it right after construction.
    pub fn set_runtime_declarations(
        &mut self,
        declarations: quiver_core::builtins::RuntimeDeclarations,
    ) {
        self.runtime_declarations = declarations;
    }

    /// Whether the effect backend has an operation outstanding, so a blocking driver must keep
    /// coming back to drain completions rather than sleeping until a worker wakes it. `false`
    /// with no backend attached — there is nothing to complete.
    pub fn io_in_flight(&self) -> bool {
        self.effect_backend
            .as_ref()
            .is_some_and(|backend| backend.has_operations_in_flight())
    }

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

        // Route completed armed stream reads to their resources' owners (a select-armed
        // read completes here even if the select has since been won by another source —
        // the owner stashes it for its next select or read).
        if let Some(effect_backend) = self.effect_backend.as_mut() {
            let events = effect_backend.take_stream_events();
            if !events.is_empty() {
                did_work = true;
                for (resource_id, resource_type, event, heap) in events {
                    self.route_stream_event(resource_id, resource_type, event, heap)?;
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

        // Auto-trigger a reclamation round once enough processes — or enough freshly
        // registered code — have accumulated. The round's pause barrier supplies its
        // own quiescence, so this can fire at any time.
        if self.collection.is_none()
            && (self.spawns_since_collection >= self.collection_threshold
                || self.code_registered_since_sweep >= self.code_collection_threshold)
        {
            self.start_collection()?;
            did_work = true;
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

    /// Stop a host-started (persistent) process: the host-side sibling of `%proc.kill`,
    /// authorised to cross the persistent exemption that protects such processes from
    /// in-language kills. Frees the resources the process owns, then routes the stop to
    /// its worker; ownership teardown cascades from there to everything it spawned. A
    /// pending result request for the process resolves with the `Killed` error.
    pub fn stop_process(&mut self, pid: ProcessId) -> Result<(), EnvironmentError> {
        self.cleanup_process_resources(pid);
        let worker_id = self
            .process_router
            .get(&pid)
            .ok_or(EnvironmentError::ProcessNotFound(pid))?;
        self.workers[*worker_id]
            .send(Command::StopProcess { id: pid })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))
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

    /// Check if a request has completed (non-blocking). Removes the request from
    /// `pending_requests` when returning a final result.
    ///
    /// The ready result is **taken, not cloned**. It used to be `.get(..).cloned()`, which deep
    /// copied the whole result — including an arbitrarily large value — on the poll that found
    /// it, and did so *recursively*: a 200,000-element list overflowed `main`'s stack and
    /// aborted. Taking it is both bounded and free, and the entry is removed either way.
    pub fn poll_request(
        &mut self,
        request_id: u64,
    ) -> Result<Option<RequestResult>, EnvironmentError> {
        match self.pending_requests.get_mut(&request_id) {
            None => {
                self.pending_requests.remove(&request_id);
                Err(EnvironmentError::RequestNotFound(request_id))
            }
            // No response from the worker yet; leave the entry in place.
            Some(None) => Ok(None),
            Some(slot) => {
                let result = slot.take();
                self.pending_requests.remove(&request_id);
                Ok(result)
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
        let mut remaps = quiver_core::bytecode::IdRemaps::default();

        // Merge constants using Program::register_constant
        for (old_idx, constant) in bytecode.constants.iter().enumerate() {
            let new_idx = self.program.register_constant(constant.clone());
            remaps.constants.insert(old_idx, new_idx);
        }

        // Merge annotation keys by name, so Annotate/GetAnnotation key ids stay aligned
        // when several independently-compiled programs share the environment.
        for (old_idx, name) in bytecode.annotation_keys.iter().enumerate() {
            let new_idx = self.program.register_annotation_key(name);
            remaps.annotation_keys.insert(old_idx, new_idx);
        }

        // Merge field names by name, so GetNamed ids stay aligned across merged programs.
        for (old_idx, name) in bytecode.field_names.iter().enumerate() {
            let new_idx = self.program.register_field_name(name);
            remaps.field_names.insert(old_idx, new_idx);
        }

        // Merge types and tuples. They are mutually recursive (a type may reference tuples and
        // vice versa), so we import every node via `import_type` / `import_tuple`, which import
        // each node's dependencies before registering it. Iterating over all indices guarantees
        // every source index ends up in the remap tables (including those reachable only through
        // function instructions), and memoisation keeps repeated visits cheap.
        let src = TypeSource {
            types: &bytecode.types,
            tuples: &bytecode.tuples,
            annotation_keys: &bytecode.annotation_keys,
        };
        for old_idx in 0..bytecode.types.len() {
            import_type(
                &mut self.program,
                &src,
                &mut remaps.types,
                &mut remaps.tuples,
                old_idx,
            );
        }
        for old_idx in 0..bytecode.tuples.len() {
            import_tuple(
                &mut self.program,
                &src,
                &mut remaps.types,
                &mut remaps.tuples,
                old_idx,
            );
        }

        // Merge builtins using Program::register_builtin_info
        for (old_idx, builtin_info) in bytecode.builtins.iter().enumerate() {
            // Remap type ID references within the builtin info
            let remapped_info = quiver_core::types::BuiltinInfo {
                name: builtin_info.name.clone(),
                param_type: remap_type_id(builtin_info.param_type, &remaps.types),
                result_type: remap_type_id(builtin_info.result_type, &remaps.types),
            };
            let new_idx = self.program.register_builtin_info(remapped_info);
            remaps.builtins.insert(old_idx, new_idx);
        }

        // Merge the failure-provenance sites (debug builds): each site is re-registered
        // with its module-name constant remapped, so `Stamp` ids can be remapped below.
        // `register_debug_site` re-derives the table's key/tuple ids in this program,
        // where the same names/shapes deduplicate to the ids imported above.
        if let Some(table) = &bytecode.debug {
            for (old_idx, site) in table.sites.iter().enumerate() {
                let new_idx = self
                    .program
                    .register_debug_site(quiver_core::bytecode::Site {
                        module_constant: *remaps
                            .constants
                            .get(&site.module_constant)
                            .unwrap_or(&site.module_constant),
                        ..site.clone()
                    });
                remaps.sites.insert(old_idx, new_idx);
            }
        }

        // Merge functions (type_id is remapped by remap_function)
        for (old_idx, function) in bytecode.functions.iter().enumerate() {
            let remapped_function = function.clone().remap_ids(&remaps);

            let new_idx = self.program.register_function(remapped_function);
            remaps.functions.insert(old_idx, new_idx);
        }

        // Revived stubs (identical content re-registered after reclamation refilled its
        // original slot) must reach append-only workers explicitly; a shared table
        // already carries the new content. Mid-collection, everything this merge
        // touched — its whole remap image — is excluded from the round's dead set: the
        // merged instructions may reference it, and the workers' root walk predates
        // this code running.
        let (revived_functions, revived_constants) = self.program.take_revived();
        if let Some(state) = self.collection.as_mut() {
            state
                .code_exclusion_functions
                .extend(remaps.functions.values().copied());
            state
                .code_exclusion_constants
                .extend(remaps.constants.values().copied());
        }
        self.code_registered_since_sweep += (self.program.get_functions().len()
            - old_functions_len)
            + (self.program.get_constants().len() - old_constants_len);

        self.send_program_update(
            old_constants_len,
            old_functions_len,
            old_tuples_len,
            old_types_len,
            old_builtins_len,
            revived_functions,
            revived_constants,
        )?;

        // Return remapped entry function index
        Ok(*remaps
            .functions
            .get(&entry_fn)
            .expect("Entry function should be in remap table"))
    }

    /// Refresh the derived tables over the merged program and ship every registry item
    /// beyond the given watermarks to the workers (pass zeros to ship everything, as a
    /// seed does). `patched_functions`/`patched_constants` name slots *below* the
    /// watermarks whose content changed in place — revived stubs after a merge, or
    /// freshly reclaimed stubs after a code sweep; shared-table workers pick the new
    /// content up wholesale, append-only workers receive explicit patches. No-op when
    /// nothing is new and nothing was patched.
    #[allow(clippy::too_many_arguments)]
    fn send_program_update(
        &mut self,
        old_constants_len: usize,
        old_functions_len: usize,
        old_tuples_len: usize,
        old_types_len: usize,
        old_builtins_len: usize,
        patched_functions: Vec<usize>,
        patched_constants: Vec<usize>,
    ) -> Result<(), EnvironmentError> {
        // Ensure the crash-delivery shapes exist in the merged program *before* the
        // deltas below are computed, so the workers receive their tuple infos and the
        // compatibility tables cover them (checked `:crash` retrievals test against
        // these shapes structurally).
        let runtime_tables = self
            .program
            .runtime_tables(&self.runtime_declarations)
            .map_err(EnvironmentError::Executor)?;

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

        // Only send update if there's new data or an in-place patch
        if !new_constants.is_empty()
            || !new_functions.is_empty()
            || !new_tuples.is_empty()
            || !new_types.is_empty()
            || !new_builtins.is_empty()
            || !patched_functions.is_empty()
            || !patched_constants.is_empty()
        {
            let resource_names = self.program.collect_resource_names();

            let input = CompatibilityInput {
                types: self.program.get_types(),
                tuples: self.program.get_tuples(),
                functions: self.program.get_functions(),
                builtins: self.program.get_builtins(),
                resource_names: &resource_names,
            };

            // Extend the incrementally-maintained compatibility tables to the merged
            // program; workers receive them whole and replace their copies.
            self.compatibility.update(&input);
            if std::env::var("QUIVER_VERIFY_COMPAT").is_ok() {
                self.compatibility.assert_matches_full(&input);
            }
            // Built once and wrapped once. `update_cmd.clone()` below runs per worker, so
            // without the `Arc` each of these tables was deep-copied N times into N identical
            // private copies; now the clone is a refcount bump and the natives share one copy.
            // (The web transport serializes each command anyway, so it is unaffected either
            // way — see `ProgramUpdate::type_compatibility`.)
            let type_compatibility = Arc::new(self.compatibility.type_compatibility.clone());
            let function_param_compatibility = Arc::new(self.compatibility.function_params.clone());
            let builtin_param_compatibility = Arc::new(self.compatibility.builtin_params.clone());
            let canonical_tuples = Arc::new(compute_canonical_tuples(self.program.get_tuples()));
            let field_offsets = Arc::new(compute_field_offsets(
                self.program.get_field_names(),
                self.program.get_tuples(),
            ));

            // Shape the growing tables to the transport. Where the workers are threads, each
            // takes the whole merged table by pointer and they all reference one allocation —
            // the duplication this removes was the largest single cost in the runtime's
            // footprint. Where a command has to be serialized, a delta is the only affordable
            // form: the whole program would otherwise be re-encoded, per worker, per update.
            let shared = self.workers.iter().all(|worker| worker.shares_memory());

            // In-place patches only matter where tables arrive as appends; a shared
            // table already carries the patched slots.
            let (patched_functions, patched_constants) = if shared {
                (Vec::new(), Vec::new())
            } else {
                (
                    patched_functions
                        .into_iter()
                        .map(|index| (index, self.program.get_functions()[index].clone()))
                        .collect(),
                    patched_constants
                        .into_iter()
                        .map(|index| (index, self.program.get_constants()[index].clone()))
                        .collect(),
                )
            };

            let update = ProgramUpdate {
                constants: table(shared, self.program.get_constants(), new_constants),
                functions: table(shared, self.program.get_functions(), new_functions),
                tuples: table(shared, self.program.get_tuples(), new_tuples),
                types: table(shared, self.program.get_types(), new_types),
                builtins: table(shared, self.program.get_builtins(), new_builtins),
                resources: resource_names,
                type_compatibility,
                function_param_compatibility,
                builtin_param_compatibility,
                field_offsets,
                canonical_tuples,
                // Full snapshot: the executor rebuilds its prebuilt site values from it.
                debug: self.program.debug_sites().cloned(),
                runtime: Some(runtime_tables),
                patched_functions,
                patched_constants,
            };

            let update_cmd = Command::UpdateProgram(Box::new(update));

            for worker in &mut self.workers {
                worker
                    .send(update_cmd.clone())
                    .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
            }

            // Hand the backend the (now possibly larger) set of type ids it needs to stamp real
            // values onto effect results.
            self.push_type_ids_to_backend();
        }

        Ok(())
    }

    /// Assert the incrementally-maintained compatibility tables equal a full
    /// recomputation over the merged program. A validation hook for tests; the
    /// `QUIVER_VERIFY_COMPAT` environment variable applies the same check at every
    /// merge.
    pub fn verify_compatibility_tables(&self) {
        let resource_names = self.program.collect_resource_names();
        self.compatibility.assert_matches_full(&CompatibilityInput {
            types: self.program.get_types(),
            tuples: self.program.get_tuples(),
            functions: self.program.get_functions(),
            builtins: self.program.get_builtins(),
            resource_names: &resource_names,
        });
    }

    fn handle_event(&mut self, event: Event<E>) -> Result<(), EnvironmentError> {
        match event {
            Event::SpawnAction {
                caller,
                function_index,
                captures,
                argument,
            } => self.handle_spawn(caller, function_index, captures, argument),
            Event::DeliverAction { target, message } => self.handle_deliver(target, message),
            Event::AwaitAction { awaiter, targets } => {
                self.handle_await_processes(awaiter, targets)
            }
            Event::KillAction { target } => self.handle_kill(target),
            Event::LinkAction { caller, target } => self.handle_link(caller, target),
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
            Event::ArmStreamAction {
                caller,
                resource_id,
            } => self.handle_arm_stream(caller, resource_id),
            Event::ReadStateAction {
                caller,
                target,
                subscribe,
            } => {
                // Route the read to the target's worker. Unlike delivery, no resource
                // ownership transfer: a sample is a read, not a message.
                let worker_id = self
                    .process_router
                    .get(&target)
                    .ok_or(EnvironmentError::ProcessNotFound(target))?;
                self.workers[*worker_id]
                    .send(Command::ReadState {
                        caller,
                        target,
                        subscribe,
                    })
                    .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
                Ok(())
            }
            Event::UnsubscribeAction { target, subscriber } => {
                // Route to the target's worker to drop the reactive subscription. A target
                // that has since been reclaimed is simply gone — a no-op.
                if let Some(worker_id) = self.process_router.get(&target) {
                    self.workers[*worker_id]
                        .send(Command::UnsubscribeState { target, subscriber })
                        .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
                }
                Ok(())
            }
            Event::StateRead { caller, state } => {
                let worker_id = self
                    .process_router
                    .get(&caller)
                    .ok_or(EnvironmentError::ProcessNotFound(caller))?;
                self.workers[*worker_id]
                    .send(Command::NotifyState {
                        process_id: caller,
                        state,
                    })
                    .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
                Ok(())
            }
            Event::CollectionReady {
                request_id,
                worker_id,
            } => self.handle_collection_ready(request_id, worker_id),
            Event::AdjacencyResponse {
                request_id,
                worker_id,
                adjacency,
            } => self.handle_adjacency_response(request_id, worker_id, adjacency),
            Event::CodeRootsResponse {
                request_id,
                worker_id,
                functions,
                constants,
            } => self.handle_code_roots_response(request_id, worker_id, functions, constants),
            Event::_Phantom(_) => {
                // This variant is never actually used, only for maintaining generics
                unreachable!("_Phantom variant should never be constructed")
            }
        }
    }

    /// Begin a reclamation round if none is in flight. Returns whether a round was
    /// started. The round advances across subsequent `step()`s — pause all workers,
    /// snapshot the frozen graph, sweep unreachable tombstones, resume — so poll
    /// [`Self::is_collecting`] for completion.
    pub fn start_collection(&mut self) -> Result<bool, EnvironmentError> {
        if self.collection.is_some() {
            return Ok(false);
        }
        let request_id = self.allocate_request_id();
        for worker in self.workers.iter_mut() {
            worker
                .send(Command::BeginCollection { request_id })
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }
        // The code phase rides a round rather than running alone: it needs the same
        // pause barrier, and process reachability settled first (tombstone results
        // keep code alive).
        let include_code = self.code_collection_requested
            || self.code_registered_since_sweep >= self.code_collection_threshold;
        self.code_collection_requested = false;
        self.collection = Some(CollectionState {
            phase: CollectionPhase::Pausing,
            request_id,
            pending: (0..self.workers.len()).collect(),
            adjacency: Vec::new(),
            include_code,
            snapshot_functions: self.program.get_functions().len(),
            snapshot_constants: self.program.get_constants().len(),
            code_functions: HashSet::new(),
            code_constants: HashSet::new(),
            code_exclusion_functions: HashSet::new(),
            code_exclusion_constants: HashSet::new(),
        });
        self.spawns_since_collection = 0;
        Ok(true)
    }

    /// Whether a reclamation round is currently in flight.
    pub fn is_collecting(&self) -> bool {
        self.collection.is_some()
    }

    /// Cumulative tombstone entries reclaimed across all rounds.
    pub fn reclaimed_total(&self) -> usize {
        self.reclaimed_total
    }

    /// Number of processes the environment still tracks: live processes plus tombstones not
    /// yet reclaimed. Shrinks as a reclamation round prunes the router. A test/metric hook.
    pub fn process_count(&self) -> usize {
        self.process_router.len()
    }

    /// A worker paused for the current round. Once every worker has acked, FIFO channels
    /// guarantee all pre-pause sends are routed, so advance to snapshotting.
    fn handle_collection_ready(
        &mut self,
        request_id: u64,
        worker_id: WorkerId,
    ) -> Result<(), EnvironmentError> {
        let advance = match self.collection.as_mut() {
            Some(state)
                if state.request_id == request_id
                    && matches!(state.phase, CollectionPhase::Pausing) =>
            {
                state.pending.remove(&worker_id);
                state.pending.is_empty()
            }
            _ => return Ok(()),
        };
        if advance {
            let request_id = self.collection.as_ref().unwrap().request_id;
            let state = self.collection.as_mut().unwrap();
            state.phase = CollectionPhase::Collecting;
            state.pending = (0..self.workers.len()).collect();
            for worker in self.workers.iter_mut() {
                worker
                    .send(Command::CollectAdjacency { request_id })
                    .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
            }
        }
        Ok(())
    }

    /// A worker's graph slice arrived. Once every worker has reported, trace and sweep.
    fn handle_adjacency_response(
        &mut self,
        request_id: u64,
        worker_id: WorkerId,
        adjacency: Vec<ProcessAdjacency>,
    ) -> Result<(), EnvironmentError> {
        let complete = match self.collection.as_mut() {
            Some(state)
                if state.request_id == request_id
                    && matches!(state.phase, CollectionPhase::Collecting) =>
            {
                if state.pending.remove(&worker_id) {
                    state.adjacency.extend(adjacency);
                }
                state.pending.is_empty()
            }
            _ => return Ok(()),
        };
        if complete {
            self.finalize_collection()?;
        }
        Ok(())
    }

    /// Trace the assembled graph, reclaim the unreachable tombstones, prune the router,
    /// then either resume every worker or hand off to the code phase.
    fn finalize_collection(&mut self) -> Result<(), EnvironmentError> {
        let Some(mut state) = self.collection.take() else {
            return Ok(());
        };
        let sweep = compute_sweep(&state.adjacency);

        for pid in &sweep {
            self.process_router.remove(pid);
        }
        self.reclaimed_total += sweep.len();

        // Broadcast the sweep set to every worker: the owner removes each tombstone; all
        // workers prune stale watcher references to it (an OwnedChild/Link entry may live on a
        // different worker than its target). Then resume. FIFO keeps Reclaim before
        // EndCollection, so pruning happens while still paused.
        if !sweep.is_empty() {
            for worker in self.workers.iter_mut() {
                worker
                    .send(Command::Reclaim {
                        pids: sweep.clone(),
                    })
                    .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
            }
        }

        if state.include_code {
            // Still paused; FIFO puts CollectCodeRoots after Reclaim, so the walk sees
            // only surviving processes.
            let request_id = state.request_id;
            state.phase = CollectionPhase::CollectingCode;
            state.pending = (0..self.workers.len()).collect();
            state.adjacency = Vec::new();
            self.collection = Some(state);
            for worker in self.workers.iter_mut() {
                worker
                    .send(Command::CollectCodeRoots { request_id })
                    .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
            }
            return Ok(());
        }

        for worker in self.workers.iter_mut() {
            worker
                .send(Command::EndCollection)
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }
        Ok(())
    }

    /// A worker's code-roots slice arrived. Once every worker has reported, sweep the
    /// merged program's dead code and resume.
    fn handle_code_roots_response(
        &mut self,
        request_id: u64,
        worker_id: WorkerId,
        functions: Vec<usize>,
        constants: Vec<usize>,
    ) -> Result<(), EnvironmentError> {
        let complete = match self.collection.as_mut() {
            Some(state)
                if state.request_id == request_id
                    && matches!(state.phase, CollectionPhase::CollectingCode) =>
            {
                if state.pending.remove(&worker_id) {
                    state.code_functions.extend(functions);
                    state.code_constants.extend(constants);
                }
                state.pending.is_empty()
            }
            _ => return Ok(()),
        };
        if complete {
            self.finalize_code_collection()?;
        }
        Ok(())
    }

    /// Compute the live code set — worker roots, host-held values, mid-round merge
    /// exclusions, debug-site constants — close it over static instruction references,
    /// stub what's left, ship the patched tables, and resume every worker.
    fn finalize_code_collection(&mut self) -> Result<(), EnvironmentError> {
        let Some(state) = self.collection.take() else {
            return Ok(());
        };
        let mut live_functions = state.code_functions;
        let mut live_constants = state.code_constants;
        live_functions.extend(state.code_exclusion_functions);
        live_constants.extend(state.code_exclusion_constants);

        // Host-held values: resolved-but-unpolled request results, subscription
        // payloads, await results collected but not yet delivered. (Aggregation
        // slices carry statuses/types, never values.)
        for result in self
            .pending_requests
            .values()
            .flatten()
            .chain(self.subscription_updates.values())
        {
            collect_request_result_refs(result, &mut live_functions, &mut live_constants);
        }
        for pending in self.pending_awaits.values() {
            for results in pending.responses.values() {
                for value in results.values().flatten().filter_map(|r| r.as_ref().ok()) {
                    value.collect_code_refs(&mut live_functions, &mut live_constants);
                }
            }
        }
        for subscription in self.subscriptions.values() {
            for payload in subscription.per_worker.values() {
                if let SubscriptionPayload::ProcessInfo(Some(info)) = payload
                    && let Some(Ok(value)) = &info.result
                {
                    value.collect_code_refs(&mut live_functions, &mut live_constants);
                }
            }
        }

        // Debug sites stay (they are shared, positionally indexed weight-free entries),
        // and workers prebuild their provenance values over these constants.
        if let Some(table) = self.program.debug_sites() {
            for site in &table.sites {
                live_constants.insert(site.module_constant);
            }
        }

        // Close over static instruction references: a live function's operands keep
        // the functions and constants they name alive.
        let mut worklist: Vec<usize> = live_functions.iter().copied().collect();
        while let Some(index) = worklist.pop() {
            let Some(function) = self.program.get_function(index) else {
                continue;
            };
            let mut referenced = HashSet::new();
            function.collect_code_refs(&mut referenced, &mut live_constants);
            for function_index in referenced {
                if live_functions.insert(function_index) {
                    worklist.push(function_index);
                }
            }
        }

        // Dead: everything the round could see that nothing live reaches. Entries
        // registered mid-round sit beyond the snapshot and are untouchable.
        let dead_functions: Vec<usize> = (0..state.snapshot_functions)
            .filter(|index| {
                !live_functions.contains(index) && !self.program.function_stubbed(*index)
            })
            .collect();
        let dead_constants: Vec<usize> = (0..state.snapshot_constants)
            .filter(|index| !live_constants.contains(index))
            .collect();

        let before = self.program.stubbed_counts();
        self.program.reclaim_code(&dead_functions, &dead_constants);
        let after = self.program.stubbed_counts();
        self.code_reclaimed_functions_total += after.0 - before.0;
        self.code_reclaimed_constants_total += after.1 - before.1;
        self.code_registered_since_sweep = 0;

        // Ship the stubs (whole shared tables for native workers; index patches for
        // append-only ones), then resume.
        if after != before {
            let functions_len = self.program.get_functions().len();
            let constants_len = self.program.get_constants().len();
            self.send_program_update(
                constants_len,
                functions_len,
                self.program.get_tuples().len(),
                self.program.get_types().len(),
                self.program.get_builtins().len(),
                dead_functions,
                dead_constants,
            )?;
        }
        for worker in self.workers.iter_mut() {
            worker
                .send(Command::EndCollection)
                .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        }
        Ok(())
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

    /// Route a containment-teardown kill to its process's worker, freeing the
    /// resources it owns on the way. The kill is fire-and-forget — there may be no
    /// awaiter to ever report the death — so this routing point is where the
    /// environment reliably learns of it. (The process may still run a final slice
    /// before the command lands; an operation on a just-freed resource then fails,
    /// which only hastens the death already in progress.)
    fn handle_kill(&mut self, target: ProcessId) -> Result<(), EnvironmentError> {
        self.cleanup_process_resources(target);
        let worker_id = self
            .process_router
            .get(&target)
            .ok_or(EnvironmentError::ProcessNotFound(target))?;
        self.workers[*worker_id]
            .send(Command::KillProcess { id: target })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))
    }

    /// Route the target-side half of a link to the target's worker.
    fn handle_link(
        &mut self,
        caller: ProcessId,
        target: ProcessId,
    ) -> Result<(), EnvironmentError> {
        let worker_id = self
            .process_router
            .get(&target)
            .ok_or(EnvironmentError::ProcessNotFound(target))?;
        self.workers[*worker_id]
            .send(Command::LinkProcess {
                target,
                peer: caller,
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))
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
        captures: Vec<WireValue>,
        argument: WireValue,
    ) -> Result<(), EnvironmentError> {
        // Each spawn is a future tombstone; count it to drive the reclamation auto-trigger.
        self.spawns_since_collection += 1;

        // Allocate new ProcessId
        let new_pid = self.allocate_process_id();

        // Choose worker: if any captures or argument are resources, spawn on the same worker
        // that owns the first resource (via the resource's owner process_id).
        // Otherwise use round-robin.
        let worker_id = captures
            .iter()
            .chain(std::iter::once(&argument))
            .find_map(|value| {
                if let WireValue::Resource(resource_id, _) = value {
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
            self.transfer_wire_resource_ownership(capture, new_pid);
        }
        self.transfer_wire_resource_ownership(&argument, new_pid);

        // Spawn process on chosen worker with function, captures, and argument
        self.workers[worker_id]
            .send(Command::SpawnProcess {
                id: new_pid,
                function_index,
                captures,
                argument,
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

    /// Transfer ownership of every resource in a transferred value to its new owner. Walks the
    /// wire form: the environment owns no heap, so an executor's `Value` is not its to inspect.
    ///
    /// **Iterative**, and that is not decoration. This runs on the environment thread, which is
    /// `main` with the default 8 MiB stack — not a worker, so `WORKER_STACK_SIZE` does not
    /// cover it. A recursive version aborted the whole process on a message carrying a
    /// 200k-element list, which is ordinary code: build a list, send it to a process. An abort
    /// is uncatchable, so this has to be bounded rather than merely deep.
    fn transfer_wire_resource_ownership(&mut self, value: &WireValue, new_owner: ProcessId) {
        // Lazily allocated: a message with no nesting never pushes, so never allocates.
        let mut pending: Vec<&WireValue> = Vec::new();
        let mut next = Some(value);
        while let Some(value) = next.take().or_else(|| pending.pop()) {
            match value {
                WireValue::Resource(resource_id, _) => {
                    self.resource_ownership.insert(*resource_id, new_owner);
                }
                // `all_values`, not `elements`: an annotation carries a value like any other
                // field, so a handle attached as one crosses with the message and must move
                // with it. A builtin's elements are always empty — its payload exists only to
                // carry annotations — so that arm is about annotations alone.
                WireValue::Tuple(_, payload)
                | WireValue::Function(_, payload)
                | WireValue::Builtin(_, Some(payload)) => pending.extend(payload.all_values()),
                _ => {} // Other value types don't contain resources
            }
        }
    }

    fn handle_deliver(
        &mut self,
        target: ProcessId,
        message: WireValue,
    ) -> Result<(), EnvironmentError> {
        // Transfer ownership of any resources in the message to the target process
        self.transfer_wire_resource_ownership(&message, target);

        let worker_id = self
            .process_router
            .get(&target)
            .ok_or(EnvironmentError::ProcessNotFound(target))?;

        self.workers[*worker_id]
            .send(Command::DeliverMessage { target, message })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;

        Ok(())
    }

    fn handle_result_response(
        &mut self,
        request_id: u64,
        result: Result<WireValue, quiver_core::error::Error>,
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
                            result,
                            receive,
                            states,
                            ..
                        } => Type::Process {
                            send: Some(*receive),
                            receive: Some(*result),
                            state: *states,
                        },
                        _ => Type::Process {
                            send: None,
                            receive: None,
                            state: None,
                        },
                    })
                    .unwrap_or(Type::Process {
                        send: None,
                        receive: None,
                        state: None,
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
    /// Format a value that never crossed a worker boundary — the web bridge builds one
    /// directly from JS. Transferred values use [`format_value`](Self::format_value).
    pub fn format_core_value(&self, value: &Value, heap: &[Vec<u8>]) -> String {
        let binary_lookup = quiver_core::format::HeapAndProgramLookup {
            heap,
            program: &self.program,
        };
        quiver_core::format::format_value(value, &self.program, &binary_lookup)
    }

    pub fn format_value(&self, value: &WireValue) -> String {
        let (value, heap) = value.for_display();
        let binary_lookup = quiver_core::format::HeapAndProgramLookup {
            heap: &heap,
            program: &self.program,
        };
        quiver_core::format::format_value(&value, &self.program, &binary_lookup)
    }

    /// Describe a nil result's failure provenance (debug builds): the `origin`
    /// annotation's site, rendered as e.g. `no branch matched at shapes:12:9`.
    pub fn describe_origin(&self, value: &WireValue) -> Option<String> {
        let (value, heap) = value.for_display();
        let binary_lookup = quiver_core::format::HeapAndProgramLookup {
            heap: &heap,
            program: &self.program,
        };
        quiver_core::format::describe_origin(
            &value,
            self.program.get_annotation_keys(),
            &self.program,
            &binary_lookup,
        )
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
        let src = TypeSource {
            types: self.program.get_types(),
            tuples: self.program.get_tuples(),
            annotation_keys: self.program.get_annotation_keys(),
        };
        import_type_value(target, &src, &mut type_remap, &mut tuple_remap, ty)
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
                result,
                receive,
                states,
                ..
            } => Some(Type::Process {
                send: Some(*receive),
                receive: Some(*result),
                state: *states,
            }),
            _ => None,
        }
    }

    /// Convert a runtime Value to its Type representation
    pub fn value_to_type(&mut self, value: &Value) -> Type {
        match value {
            Value::Int(_) | Value::BigInt(_) => Type::Integer,
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
            Value::Builtin(builtin_id, _) => {
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
                    states: Some(param_type),      // ... and never tail-call
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
        let bindings = Bindings {
            variables: std::collections::HashMap::new(),
            type_aliases: type_aliases.clone(),
        };
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

    /// Arm a stream resource's next-event read for a parked select. Ownership is
    /// enforced like any effect on an existing resource; a violation (or a
    /// stream-less backend) fails the caller as a runtime error rather than leaving
    /// its select waiting on an event that never arrives.
    fn handle_arm_stream(
        &mut self,
        caller: ProcessId,
        resource_id: ResourceId,
    ) -> Result<(), EnvironmentError> {
        if let Some(owner) = self.resource_ownership.get(&resource_id)
            && *owner != caller
        {
            return self.report_effect_error(
                caller,
                format!("Process {} does not own resource {}", caller, resource_id),
            );
        }
        let Some(effect_backend) = self.effect_backend.as_mut() else {
            return self.report_effect_error(caller, "No effect backend available".to_string());
        };
        if let Err(e) = effect_backend.arm_stream(resource_id) {
            return self.report_effect_error(caller, format!("{:?}", e));
        }
        Ok(())
    }

    /// Route a completed armed read to its resource's owner as a `ResourceEvent`
    /// command. An `Accepted` event's fresh socket is recorded as owned by the
    /// acceptor before it ships. An unowned resource (its owner died — cleanup
    /// already closed what it could) drops the event.
    fn route_stream_event(
        &mut self,
        resource_id: ResourceId,
        resource_type: usize,
        event: quiver_core::process::StreamEvent,
        bytes: Vec<u8>,
    ) -> Result<(), EnvironmentError> {
        let Some(owner) = self.resource_ownership.get(&resource_id).copied() else {
            // Owner gone: release anything the event carries. A fresh produced
            // resource (an accepted socket) would leak its fd otherwise.
            if let quiver_core::process::StreamEvent::Resource {
                resource_id: produced,
            } = event
                && let Some(backend) = self.effect_backend.as_mut()
            {
                backend.close_resource(produced);
            }
            return Ok(());
        };
        if let quiver_core::process::StreamEvent::Resource {
            resource_id: produced,
        } = &event
        {
            self.resource_ownership.insert(*produced, owner);
        }
        let worker_id = self
            .process_router
            .get(&owner)
            .copied()
            .ok_or(EnvironmentError::ProcessNotFound(owner))?;
        self.workers[worker_id].send(Command::ResourceEvent {
            process_id: owner,
            resource_id,
            resource_type,
            event,
            bytes,
        })?;
        Ok(())
    }

    /// Report an environment-side effect failure back to the requesting process as a fault.
    ///
    /// The process is suspended waiting for its effect to complete; delivering a failure
    /// completion lets it resume and fail, rather than hanging indefinitely. These are always
    /// faults, never outcomes — an ownership violation or a missing backend is a bug, not
    /// something a caller can branch on.
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
                result: Err(quiver_core::effects::EffectFailure::Fault(message)),
            })
            .map_err(|e| EnvironmentError::WorkerCommunication(e.to_string()))?;
        Ok(())
    }

    /// Handle completed effect operation (generic version)
    fn handle_effect_completion(
        &mut self,
        process_id: ProcessId,
        result: Result<WireValue, quiver_core::effects::EffectError>,
    ) -> Result<(), EnvironmentError> {
        // Register ownership of every resource the completion carries — not just a bare one.
        // An effect may answer a *composite* (fetch's `[status, headers, body]` carries the
        // body stream), and an unregistered resource is worse than it looks: the ownership
        // check treats "no owner recorded" as "no violation", so it would pass every check
        // and never be auto-closed when its process ends.
        if let Ok(value) = &result {
            let mut found = Vec::new();
            collect_resources(value, &mut found);
            for rid in found {
                self.resource_ownership.insert(rid, process_id);
            }
        }

        // A backend error is classified: world outcomes become values on the process,
        // argument-domain ones stay faults.
        let result = result.map_err(quiver_core::effects::EffectFailure::from_effect_error);

        // Send completion to the worker
        let worker_id = self
            .process_router
            .get(&process_id)
            .ok_or(EnvironmentError::ProcessNotFound(process_id))?;

        self.workers[*worker_id]
            .send(Command::EffectCompletion { process_id, result })
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
        let src = TypeSource {
            types: &bytecode.types,
            tuples: &bytecode.tuples,
            annotation_keys: &bytecode.annotation_keys,
        };
        for old_idx in 0..bytecode.types.len() {
            import_type(
                &mut self.program,
                &src,
                &mut type_remap,
                &mut tuple_remap,
                old_idx,
            );
        }
        for old_idx in 0..bytecode.tuples.len() {
            import_tuple(
                &mut self.program,
                &src,
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
    let mut out = Vec::new();
    for builtin in program.get_builtins() {
        let mut tuple_ids = Vec::new();
        collect_result_tuple_ids(program, builtin.result_type, &mut tuple_ids);
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
/// The *composite* tuples of a result type — the ones a backend has to build field by field.
/// Nullary tuples are skipped: they are tags, and `collect_named_variants` gathers them by name
/// so the backend can pick one (`Missing`, a `kind`) without knowing the union's shape.
/// Every resource handle inside a wire value, however deeply nested.
fn collect_resources(value: &WireValue, out: &mut Vec<quiver_core::value::ResourceId>) {
    match value {
        WireValue::Resource(rid, _) => out.push(*rid),
        WireValue::Tuple(_, payload) | WireValue::Function(_, payload) => {
            for element in &payload.elements {
                collect_resources(element, out);
            }
        }
        WireValue::Builtin(_, Some(payload)) => {
            for element in &payload.elements {
                collect_resources(element, out);
            }
        }
        _ => {}
    }
}

fn collect_result_tuple_ids(program: &Program, type_id: usize, out: &mut Vec<usize>) {
    match program.get_types().get(type_id) {
        Some(Type::Tuple(tuple_id))
            if *tuple_id != NIL
                && *tuple_id != OK
                && !program.get_tuples()[*tuple_id].fields.is_empty() =>
        {
            out.push(*tuple_id)
        }
        Some(Type::Union(members)) => {
            for member in members.clone() {
                collect_result_tuple_ids(program, member, out);
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
            annotation_keys: vec![],
            field_names: vec![],
            debug: None,
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
            state: None,
        };

        // A REPL's fresh program where that environment id is not yet meaningful.
        let mut repl_program = Program::new();
        let imported = env.import_type_into(&mut repl_program, process_type);

        let Type::Process {
            send: Some(send),
            receive: Some(receive),
            state: None,
        } = imported
        else {
            panic!("expected a process type with send and receive");
        };
        // The child ids now index the REPL program and resolve back to `'int`.
        assert_eq!(repl_program.get_types()[send], Type::Integer);
        assert_eq!(repl_program.get_types()[receive], Type::Integer);
    }

    /// An annotated type crossing id spaces must remap its base id, entry value-type ids
    /// AND entry key ids (by name). Before the `Type::Annotated` arm existed in
    /// `import_type_value`, all three were copied verbatim — harmless only while the
    /// merge happened to be an identity mapping.
    #[test]
    fn annotated_types_are_deep_imported() {
        let mut env: Environment<TestEffect> = Environment::new(vec![]);

        // Shift every id in the source space: types (filler at 0), and keys ("pad" at 0,
        // "doc" at 1). The target program registers keys in a different order, so the
        // key id must remap by name, not by index.
        let src_types = vec![Type::Reference, Type::Integer, Type::Binary];
        let annotated = Type::Annotated {
            base: 1, // 'int in the source space
            exact: true,
            entries: vec![(1, 2)], // :doc (source key 1) at 'bin (source type 2)
        };
        let src = TypeSource {
            types: &src_types,
            tuples: &[],
            annotation_keys: &["pad".to_string(), "doc".to_string()],
        };

        env.program.register_annotation_key("doc"); // target: "doc" is key 0 here
        let mut type_remap = HashMap::new();
        let mut tuple_remap = HashMap::new();
        let imported = import_type_value(
            &mut env.program,
            &src,
            &mut type_remap,
            &mut tuple_remap,
            annotated,
        );

        let Type::Annotated {
            base,
            exact: true,
            entries,
        } = imported
        else {
            panic!("expected an exact annotated type");
        };
        assert_eq!(env.program.get_types()[base], Type::Integer);
        let [(key, value_type)] = entries.as_slice() else {
            panic!("expected a single entry");
        };
        assert_eq!(env.program.lookup_annotation_key_name(*key), Some("doc"));
        assert_eq!(env.program.get_types()[*value_type], Type::Binary);
    }
}

#[cfg(test)]
mod reclamation_tests {
    use super::*;

    fn root(pid: ProcessId, outgoing: &[ProcessId]) -> ProcessAdjacency {
        ProcessAdjacency {
            pid,
            category: ProcessCategory::Root,
            outgoing: outgoing.to_vec(),
        }
    }

    fn tombstone(pid: ProcessId, outgoing: &[ProcessId]) -> ProcessAdjacency {
        ProcessAdjacency {
            pid,
            category: ProcessCategory::Tombstone,
            outgoing: outgoing.to_vec(),
        }
    }

    fn sweep_set(adjacency: &[ProcessAdjacency]) -> HashSet<ProcessId> {
        compute_sweep(adjacency).into_iter().collect()
    }

    #[test]
    fn unreferenced_tombstone_is_swept() {
        // A live root holding nothing; a dead process nobody references.
        let adj = [root(0, &[]), tombstone(1, &[])];
        assert_eq!(sweep_set(&adj), HashSet::from([1]));
    }

    #[test]
    fn tombstone_held_by_a_live_root_is_kept() {
        // Root 0 still holds pid 1 (e.g. a bound pid): 1 stays observable via !p / ?p.
        let adj = [root(0, &[1]), tombstone(1, &[])];
        assert!(sweep_set(&adj).is_empty());
    }

    #[test]
    fn reachability_flows_through_a_kept_tombstone() {
        // Root holds tombstone 1; 1's result/state holds tombstone 2. Awaiting 1 exposes 2,
        // so 2 must be kept too. Tombstone 3 is unreferenced and swept.
        let adj = [
            root(0, &[1]),
            tombstone(1, &[2]),
            tombstone(2, &[]),
            tombstone(3, &[]),
        ];
        assert_eq!(sweep_set(&adj), HashSet::from([3]));
    }

    #[test]
    fn dead_cycle_is_collected() {
        // Two tombstones referencing each other, reachable from no root: refcounting could
        // never free these, but tracing sweeps both.
        let adj = [root(0, &[]), tombstone(1, &[2]), tombstone(2, &[1])];
        assert_eq!(sweep_set(&adj), HashSet::from([1, 2]));
    }

    #[test]
    fn live_cycle_holding_a_tombstone_keeps_it() {
        // Root -> tombstone 1 <-> tombstone 2, and 2 -> tombstone 3. All kept via the root.
        let adj = [
            root(0, &[1]),
            tombstone(1, &[2]),
            tombstone(2, &[1, 3]),
            tombstone(3, &[]),
        ];
        assert!(sweep_set(&adj).is_empty());
    }

    #[test]
    fn reachability_through_a_live_process_keeps_a_tombstone() {
        // Root holds live process 1; 1 holds tombstone 2 (awaiting/sampling 1 would expose it).
        let adj = [root(0, &[1]), root(1, &[2]), tombstone(2, &[])];
        assert!(sweep_set(&adj).is_empty());
    }
}

/// Shape one growing table for the transport: the whole merged table (shared by pointer) when
/// the workers are threads, or just the new entries when a command has to be serialized.
///
/// Copying `whole` once here is the point — it replaces one copy *per worker*, and on the
/// native transport that copy is then shared rather than duplicated.
fn table<T: Clone>(shared: bool, whole: &[T], delta: Vec<T>) -> TableUpdate<T> {
    if shared {
        TableUpdate::Shared(Arc::new(whole.to_vec()))
    } else {
        TableUpdate::Appended(delta)
    }
}
