use crate::pump::{Pump, Tick};
use crate::types::*;
use crate::web_transport::WebWorkerHandle;
use quiver_compiler::PackageResolver;
use quiver_environment::{RequestResult, WorkerHandle};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::Worker;

// Define TypeScript types for callbacks and classes
#[wasm_bindgen(typescript_custom_section)]
const TS_DEFINITIONS: &'static str = r#"
export type WorkerFactory = () => Worker;
export type EvaluateCallback = (result: Result<EvaluationResult | null>) => void;
export type VariablesCallback = (result: Result<Variable[]>) => void;
export type ProcessStatusesCallback = (result: Result<Process[]>) => void;
export type ProcessInfoCallback = (result: Result<ProcessInfo | null>) => void;
export type WorkerInfoCallback = (result: Result<WorkerInfo[]>) => void;

export class Environment {
  free(): void;
  [Symbol.dispose](): void;

  /**
   * Create a new environment with the specified number of workers
   * @param num_workers - Number of worker threads to create
   * @param worker_factory - Factory function that returns a new Worker
   */
  constructor(num_workers: number, worker_factory: WorkerFactory);

  /**
   * Start the event loop
   */
  start(): void;

  /**
   * Stop the event loop
   */
  stop(): void;

  /**
   * Get all process statuses
   * @param callback - Callback invoked with process statuses
   */
  getProcessStatuses(callback: ProcessStatusesCallback): void;

  /**
   * Get info for a specific process
   * @param pid - Process ID
   * @param callback - Callback invoked with process info (or null if not found)
   */
  getProcessInfo(pid: number, callback: ProcessInfoCallback): void;

  /**
   * Subscribe to live process-status updates. The callback fires with an initial snapshot and
   * again on every change, until `unsubscribe` is called with the returned id.
   * @param callback - Callback invoked with process statuses on every change
   * @returns A subscription id to pass to `unsubscribe`
   */
  subscribeProcessStatuses(callback: ProcessStatusesCallback): number;

  /**
   * Subscribe to live info for a single process (status, stats, mailbox/heap sizes, result).
   * The callback fires with an initial snapshot and again on every change, until `unsubscribe`.
   * @param pid - Process ID
   * @param callback - Callback invoked with process info on every change
   * @returns A subscription id to pass to `unsubscribe`
   */
  subscribeProcessInfo(pid: number, callback: ProcessInfoCallback): number;

  /**
   * Subscribe to live worker info (executor heap/memory snapshots across all workers). The
   * callback fires with an initial snapshot and again on every change, until `unsubscribe`.
   * @param callback - Callback invoked with the worker list on every change
   * @returns A subscription id to pass to `unsubscribe`
   */
  subscribeWorkerInfo(callback: WorkerInfoCallback): number;

  /**
   * Cancel a subscription created by `subscribeProcessStatuses` / `subscribeProcessInfo` /
   * `subscribeWorkerInfo`.
   * @param subscriptionId - The id returned by the subscribe call
   */
  unsubscribe(subscriptionId: number): void;

  /**
   * Format a value for display
   * @param value - Value to format
   * @param heap - Heap data
   * @returns Formatted string representation
   */
  formatValue(value: Value, heap: number[][]): string;

  /**
   * Format a type for display (derives type from value)
   * @param value - Value to derive type from
   * @returns Formatted string representation
   */
  formatType(value: Value): string;
}

export class Repl {
  free(): void;
  [Symbol.dispose](): void;

  /**
   * Create a new REPL using the given environment
   * @param environment - Environment instance to use
   * @param files - Optional virtual filesystem (path -> source) imports resolve against. A
   *   `quiver.toml` entry, if present, defines the module routing table.
   */
  constructor(environment: Environment, files?: Record<string, string>);

  /**
   * Replace the REPL's virtual filesystem. Subsequent evaluations resolve imports against the
   * new files; accumulated variables are preserved. Throws if a `quiver.toml` in `files` is
   * invalid, leaving the current files in place.
   * @param files - Virtual filesystem (path -> source)
   */
  setFiles(files: Record<string, string>): void;

  /**
   * Refresh the cache of process types for process references (@N)
   * Call this before evaluate() to ensure process references work correctly
   * @param callback - Callback invoked when types are cached (or on error)
   */
  refreshProcessTypes(callback: (result: Result<void>) => void): void;

  /**
   * Evaluate source code and invoke callback when result is ready
   * Note: Call refreshProcessTypes() first if you need process references (@N) to work
   * @param source - Quiver source code to evaluate
   * @param callback - Callback invoked with the evaluation result
   */
  evaluate(source: string, callback: EvaluateCallback): void;

  /**
   * Get all variables defined in the REPL
   * @param callback - Callback invoked with the list of variables
   */
  getVariables(callback: VariablesCallback): void;

  /**
   * The id of this REPL's persistent process.
   */
  readonly processId: number;
}
"#;

/// Parse the JS `files` argument — a `Record<string, string>` of virtual path → contents, or
/// `undefined`/`null` for an empty filesystem — into a map.
fn parse_files(files: JsValue) -> std::result::Result<HashMap<String, String>, JsValue> {
    if files.is_undefined() || files.is_null() {
        Ok(HashMap::new())
    } else {
        serde_wasm_bindgen::from_value(files)
            .map_err(|e| JsValue::from_str(&format!("Invalid files map: {}", e)))
    }
}

/// Build a resolver over an in-memory file map. A `quiver.toml` in the map defines the module
/// routing table; without one, files resolve by path and may shadow the standard library.
fn create_resolver(
    files: HashMap<String, String>,
) -> std::result::Result<Box<PackageResolver>, JsValue> {
    PackageResolver::memory_files(files)
        .map(Box::new)
        .map_err(|e| JsValue::from_str(&e.to_string()))
}

/// Callback wrapper to store JS callbacks
struct CallbackHandle {
    callback: js_sys::Function,
}

impl CallbackHandle {
    fn new(callback: js_sys::Function) -> Self {
        Self { callback }
    }

    fn invoke<T: serde::Serialize>(&self, result: crate::types::Result<T>) {
        let js_value = serde_wasm_bindgen::to_value(&result).unwrap();
        let _ = self.callback.call1(&JsValue::NULL, &js_value);
    }
}

use crate::effects::WebEffect;

/// Shared handle to the main-thread event-driven loop. Held by `Environment`, the per-worker
/// `onmessage` handlers, and each `Repl`, so any of them can `wake()` the loop when they queue
/// work. Wrapped in `Option` because the worker handlers are created before the pump exists.
type PumpHandle = Rc<RefCell<Option<Pump>>>;
type SharedEnvironment = Rc<RefCell<quiver_environment::Environment<WebEffect>>>;
type SharedCallbacks = Rc<RefCell<HashMap<u64, CallbackHandle>>>;
type SharedProcessTypes = Rc<RefCell<HashMap<usize, (quiver_core::types::Type, usize)>>>;
type SharedPendingEvaluations = Rc<
    RefCell<
        Vec<(
            String,
            CallbackHandle,
            Rc<RefCell<quiver_environment::Repl<WebEffect>>>,
        )>,
    >,
>;

/// Wake the main-thread loop if it exists yet. A no-op before `Environment::new` installs the
/// pump, which is fine: nothing queues work against the environment before then.
fn wake(handle: &PumpHandle) {
    if let Some(pump) = handle.borrow().as_ref() {
        pump.wake();
    }
}

#[wasm_bindgen(skip_typescript)]
pub struct Environment {
    environment: SharedEnvironment,
    running: Rc<RefCell<bool>>,
    pump: PumpHandle,
    pending_callbacks: SharedCallbacks,
    // Persistent callbacks for standing subscriptions. Unlike `pending_callbacks`, an entry here is
    // re-invoked on every update and only removed by `unsubscribe`.
    subscription_callbacks: SharedCallbacks,
    cached_process_types: SharedProcessTypes,
    pending_evaluations: SharedPendingEvaluations,
}

#[wasm_bindgen]
impl Environment {
    /// Create a new environment with the specified number of workers
    /// worker_factory: A JavaScript function that returns a new Worker
    #[wasm_bindgen(constructor)]
    pub fn new(
        num_workers: usize,
        worker_factory: JsValue,
    ) -> std::result::Result<Environment, JsValue> {
        let worker_factory: js_sys::Function = worker_factory.into();
        // Set up panic hook for better error messages
        console_error_panic_hook::set_once();

        // The main-thread event loop, installed at the end of this function. The per-worker
        // `onmessage` handlers below capture this handle so an incoming event can wake the loop.
        let pump: PumpHandle = Rc::new(RefCell::new(None));

        // Create workers by calling the factory function
        let mut workers: Vec<Box<dyn WorkerHandle<WebEffect>>> = Vec::new();
        for i in 0..num_workers {
            let worker = worker_factory
                .call0(&JsValue::NULL)
                .map_err(|e| JsValue::from_str(&format!("Failed to create worker: {:?}", e)))?;

            let worker: Worker = worker
                .dyn_into()
                .map_err(|_| JsValue::from_str("Worker factory must return a Worker"))?;

            // Create event queue for this worker
            let event_queue = Rc::new(RefCell::new(std::collections::VecDeque::new()));

            // Create the handle first so we can use its shared state
            let handle = WebWorkerHandle::new(worker.clone(), event_queue.clone());

            // Get shared references from the handle
            let ready_flag = handle.ready();
            let pending_commands = handle.pending_commands();
            let event_queue_for_closure = event_queue.clone();
            let worker_for_closure = worker.clone();
            let pump_for_closure = pump.clone();
            let worker_id = i;

            // Set up message handler for worker events
            let onmessage = Closure::wrap(Box::new(move |event: web_sys::MessageEvent| {
                if let Some(text) = event.data().as_string() {
                    if text == "ready" {
                        // Worker is ready - send init message with worker_id
                        let init_msg = format!("init:{}", worker_id);
                        let _ = worker_for_closure.post_message(&JsValue::from_str(&init_msg));

                        // Mark as ready and flush pending commands
                        *ready_flag.borrow_mut() = true;

                        // Send all pending commands
                        while let Some(cmd) = pending_commands.borrow_mut().pop_front() {
                            if let Ok(json) = serde_json::to_string(&cmd) {
                                let _ = worker_for_closure.post_message(&JsValue::from_str(&json));
                            }
                        }
                    } else {
                        // Parse as Event
                        match serde_json::from_str::<quiver_environment::Event<WebEffect>>(&text) {
                            Ok(event) => {
                                event_queue_for_closure.borrow_mut().push_back(event);
                                // Wake the loop so it drains the event; it may be sleeping.
                                wake(&pump_for_closure);
                            }
                            Err(e) => {
                                web_sys::console::error_1(
                                    &format!("Failed to parse event: {}", e).into(),
                                );
                            }
                        }
                    }
                }
            }) as Box<dyn FnMut(web_sys::MessageEvent)>);

            worker.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
            onmessage.forget(); // Keep closure alive

            workers.push(Box::new(handle));
        }

        // Create environment
        // WASM doesn't use old io_backend (no io_uring available)
        let environment = quiver_environment::Environment::new(workers);
        let environment_rc = Rc::new(RefCell::new(environment));
        let pending_callbacks = Rc::new(RefCell::new(HashMap::new()));
        let subscription_callbacks = Rc::new(RefCell::new(HashMap::new()));
        let cached_process_types = Rc::new(RefCell::new(HashMap::new()));
        let pending_evaluations = Rc::new(RefCell::new(Vec::new()));
        let running = Rc::new(RefCell::new(false));

        // Install the event-driven main-thread loop. It starts idle (running == false); `start()`
        // arms it. The worker `onmessage` handlers above and the JS-facing methods below wake it
        // whenever they queue work, so it only ticks when there is something to do.
        *pump.borrow_mut() = Some(Self::build_pump(
            environment_rc.clone(),
            running.clone(),
            pending_callbacks.clone(),
            subscription_callbacks.clone(),
            cached_process_types.clone(),
            pending_evaluations.clone(),
        ));

        Ok(Self {
            environment: environment_rc,
            running,
            pump,
            pending_callbacks,
            subscription_callbacks,
            cached_process_types,
            pending_evaluations,
        })
    }

    /// Get shared state for Repl instances (internal use)
    fn get_shared_state(
        &self,
    ) -> (
        SharedEnvironment,
        SharedCallbacks,
        SharedProcessTypes,
        SharedPendingEvaluations,
        PumpHandle,
    ) {
        (
            self.environment.clone(),
            self.pending_callbacks.clone(),
            self.cached_process_types.clone(),
            self.pending_evaluations.clone(),
            self.pump.clone(),
        )
    }

    /// Build the main-thread loop. Each tick advances the environment and drains ready work; the
    /// returned `Tick` keeps it running while work flows and lets it sleep once everything is
    /// quiet (waiting for a `wake` from a worker event or a JS-side call).
    fn build_pump(
        environment: SharedEnvironment,
        running: Rc<RefCell<bool>>,
        pending_callbacks: SharedCallbacks,
        subscription_callbacks: SharedCallbacks,
        cached_process_types: SharedProcessTypes,
        pending_evaluations: SharedPendingEvaluations,
    ) -> Pump {
        Pump::new(move || {
            if !*running.borrow() {
                return Tick::Idle;
            }
            let did_work = Self::run_tick(
                &environment,
                &pending_callbacks,
                &subscription_callbacks,
                &cached_process_types,
                &pending_evaluations,
            );
            // If this tick did something it may have produced follow-up work; run once more. When a
            // tick finds nothing to do we go idle — the next change arrives via a wake.
            if did_work { Tick::Busy } else { Tick::Idle }
        })
    }

    /// Run one iteration of the main-thread loop: advance the environment (draining worker events
    /// and effect completions), resolve any ready request callbacks, and dispatch queued
    /// evaluations. Returns whether any work was done this tick.
    fn run_tick(
        environment: &SharedEnvironment,
        pending_callbacks: &SharedCallbacks,
        subscription_callbacks: &SharedCallbacks,
        cached_process_types: &SharedProcessTypes,
        pending_evaluations: &SharedPendingEvaluations,
    ) -> bool {
        // Step the environment
        let mut did_work = environment.borrow_mut().step().unwrap_or(false);

        // Deliver any standing-subscription updates produced by this step. Unlike one-shot
        // requests, the callback stays registered (re-invoked on each future update); it is only
        // removed by `unsubscribe`. Last-wins per subscription, so each fires at most once per tick.
        let subscription_updates = environment.borrow_mut().take_subscription_updates();
        if !subscription_updates.is_empty() {
            did_work = true;
            for (subscription_id, result) in subscription_updates {
                let callbacks = subscription_callbacks.borrow();
                if let Some(callback) = callbacks.get(&subscription_id) {
                    Self::handle_result(&environment.borrow(), callback, result);
                }
            }
        }

        // Process pending callbacks
        let mut callbacks_to_invoke = Vec::new();
        {
            let mut callbacks = pending_callbacks.borrow_mut();
            let request_ids: Vec<u64> = callbacks.keys().copied().collect();

            for request_id in request_ids {
                match environment.borrow_mut().poll_request(request_id) {
                    Ok(Some(result)) => {
                        // Cache process types if this is a ProcessTypes result
                        if let RequestResult::ProcessTypes(ref types) = result {
                            *cached_process_types.borrow_mut() = types.clone();
                        }

                        if let Some(callback) = callbacks.remove(&request_id) {
                            callbacks_to_invoke.push((request_id, callback, result));
                        }
                    }
                    Ok(None) => {
                        // Not ready yet
                    }
                    Err(e) => {
                        // Error polling request
                        if let Some(callback) = callbacks.remove(&request_id) {
                            callback.invoke::<()>(crate::types::Result::err(e.to_string()));
                        }
                        did_work = true;
                    }
                }
            }
        }

        if !callbacks_to_invoke.is_empty() {
            did_work = true;
        }

        // Invoke callbacks outside of the borrow. A successful line's orphaned locals were already
        // released by the worker when it delivered the result (the keep-set rode along with the
        // result request), so inspectors reflect the post-evaluation heap with nothing to do here.
        for (_request_id, callback, result) in callbacks_to_invoke {
            Self::handle_result(&environment.borrow(), &callback, result);
        }

        // Process pending evaluations
        let evaluations_to_process = {
            let mut evals = pending_evaluations.borrow_mut();
            std::mem::take(&mut *evals)
        };

        if !evaluations_to_process.is_empty() {
            did_work = true;
        }

        for (source, callback, repl) in evaluations_to_process {
            let process_types = cached_process_types.borrow().clone();

            let outcome =
                repl.borrow_mut()
                    .evaluate(&mut environment.borrow_mut(), &source, process_types);
            match outcome {
                Ok(Some(request_id)) => {
                    pending_callbacks.borrow_mut().insert(request_id, callback);
                }
                Ok(None) => {
                    callback.invoke(crate::types::Result::ok(None::<EvaluationResult>));
                }
                Err(e) => {
                    callback
                        .invoke::<EvaluationResult>(crate::types::Result::err(format!("{}", e)));
                }
            }
        }

        did_work
    }

    /// Start the event loop
    pub fn start(&mut self) {
        if *self.running.borrow() {
            return; // Already running
        }

        *self.running.borrow_mut() = true;
        wake(&self.pump);
    }

    /// Stop the event loop
    pub fn stop(&mut self) {
        *self.running.borrow_mut() = false;
        if let Some(pump) = self.pump.borrow().as_ref() {
            pump.cancel();
        }
    }

    /// Get all process statuses
    #[wasm_bindgen(js_name = "getProcessStatuses")]
    pub fn get_process_statuses(&mut self, callback: JsValue) {
        let callback: js_sys::Function = callback.into();
        match self.environment.borrow_mut().request_statuses() {
            Ok(request_id) => {
                self.pending_callbacks
                    .borrow_mut()
                    .insert(request_id, CallbackHandle::new(callback));
                wake(&self.pump);
            }
            Err(e) => {
                let cb = CallbackHandle::new(callback);
                cb.invoke::<Vec<Process>>(crate::types::Result::err(e.to_string()));
            }
        }
    }

    /// Get info for a specific process
    #[wasm_bindgen(js_name = "getProcessInfo")]
    pub fn get_process_info(&mut self, pid: usize, callback: JsValue) {
        let callback: js_sys::Function = callback.into();
        match self.environment.borrow_mut().request_process_info(pid) {
            Ok(request_id) => {
                self.pending_callbacks
                    .borrow_mut()
                    .insert(request_id, CallbackHandle::new(callback));
                wake(&self.pump);
            }
            Err(e) => {
                let cb = CallbackHandle::new(callback);
                cb.invoke::<Option<ProcessInfo>>(crate::types::Result::err(e.to_string()));
            }
        }
    }

    /// Subscribe to live process-status updates. The callback fires with an initial snapshot and
    /// again on every change, until `unsubscribe` is called with the returned id.
    #[wasm_bindgen(js_name = "subscribeProcessStatuses")]
    pub fn subscribe_process_statuses(
        &mut self,
        callback: JsValue,
    ) -> std::result::Result<f64, JsValue> {
        let callback: js_sys::Function = callback.into();
        match self.environment.borrow_mut().subscribe_process_statuses() {
            Ok(subscription_id) => {
                self.subscription_callbacks
                    .borrow_mut()
                    .insert(subscription_id, CallbackHandle::new(callback));
                wake(&self.pump);
                Ok(subscription_id as f64)
            }
            Err(e) => Err(JsValue::from_str(&e.to_string())),
        }
    }

    /// Subscribe to live info for a single process (status, stats, mailbox/heap sizes, result).
    /// The callback fires with an initial snapshot and again on every change, until `unsubscribe`.
    #[wasm_bindgen(js_name = "subscribeProcessInfo")]
    pub fn subscribe_process_info(
        &mut self,
        pid: usize,
        callback: JsValue,
    ) -> std::result::Result<f64, JsValue> {
        let callback: js_sys::Function = callback.into();
        match self.environment.borrow_mut().subscribe_process_info(pid) {
            Ok(subscription_id) => {
                self.subscription_callbacks
                    .borrow_mut()
                    .insert(subscription_id, CallbackHandle::new(callback));
                wake(&self.pump);
                Ok(subscription_id as f64)
            }
            Err(e) => Err(JsValue::from_str(&e.to_string())),
        }
    }

    /// Subscribe to live worker info (executor heap/memory snapshots across all workers). The
    /// callback fires with an initial snapshot and again on every change, until `unsubscribe`.
    #[wasm_bindgen(js_name = "subscribeWorkerInfo")]
    pub fn subscribe_worker_info(
        &mut self,
        callback: JsValue,
    ) -> std::result::Result<f64, JsValue> {
        let callback: js_sys::Function = callback.into();
        match self.environment.borrow_mut().subscribe_worker_info() {
            Ok(subscription_id) => {
                self.subscription_callbacks
                    .borrow_mut()
                    .insert(subscription_id, CallbackHandle::new(callback));
                wake(&self.pump);
                Ok(subscription_id as f64)
            }
            Err(e) => Err(JsValue::from_str(&e.to_string())),
        }
    }

    /// Cancel a subscription created by `subscribeProcessStatuses` / `subscribeProcessInfo` /
    /// `subscribeWorkerInfo`.
    #[wasm_bindgen(js_name = "unsubscribe")]
    pub fn unsubscribe(&mut self, subscription_id: f64) {
        let subscription_id = subscription_id as u64;
        self.subscription_callbacks
            .borrow_mut()
            .remove(&subscription_id);
        let _ = self.environment.borrow_mut().unsubscribe(subscription_id);
        wake(&self.pump);
    }

    /// Format a value for display
    #[wasm_bindgen(js_name = "formatValue")]
    pub fn format_value(
        &self,
        value: wasm_bindgen::JsValue,
        heap: wasm_bindgen::JsValue,
    ) -> std::result::Result<String, wasm_bindgen::JsValue> {
        let web_value: crate::types::Value = serde_wasm_bindgen::from_value(value)?;
        let heap: Vec<Vec<u8>> = serde_wasm_bindgen::from_value(heap)?;
        // Convert web value to core value, extending heap with any hex-encoded binaries
        let (core_value, extended_heap) = web_value.to_core_for_formatting(&heap);
        Ok(self
            .environment
            .borrow()
            .format_value(&core_value, &extended_heap))
    }

    /// Format a type for display (derives type from value)
    #[wasm_bindgen(js_name = "formatType")]
    pub fn format_type(
        &self,
        value: wasm_bindgen::JsValue,
    ) -> std::result::Result<String, wasm_bindgen::JsValue> {
        let web_value: crate::types::Value = serde_wasm_bindgen::from_value(value)?;
        // Convert to core value with empty heap (formatting doesn't need actual binary data)
        let (core_value, _heap) = web_value.to_core_for_formatting(&[]);
        let ty = self.environment.borrow_mut().value_to_type(&core_value);
        Ok(self.environment.borrow().format_type(&ty))
    }

    // Helper to convert RequestResult to appropriate callback invocation
    fn handle_result(
        env: &quiver_environment::Environment<WebEffect>,
        callback: &CallbackHandle,
        result: RequestResult,
    ) {
        match result {
            RequestResult::Result(Ok((value, heap)), _) => {
                callback.invoke(crate::types::Result::ok(Some(EvaluationResult {
                    value: crate::types::Value::from_core_value(&value, &heap, env.get_program()),
                    heap,
                })));
            }
            RequestResult::Result(Err(e), _) => {
                callback.invoke::<EvaluationResult>(crate::types::Result::err(format!(
                    "Runtime error: {:?}",
                    e
                )));
            }
            RequestResult::Statuses(statuses) => {
                let mut processes: Vec<Process> = statuses
                    .into_iter()
                    .map(|(id, status)| Process {
                        id,
                        status: status.into(),
                    })
                    .collect();
                processes.sort_by_key(|p| p.id);

                callback.invoke(crate::types::Result::ok(processes));
            }
            RequestResult::ProcessInfo(info_opt) => {
                let js_info = info_opt.map(|info| {
                    // Get the formatted process type
                    let process_type = info
                        .function_index
                        .and_then(|idx| env.format_process_type(idx));

                    let result = info.result.map(|r| match r {
                        Ok((value, heap)) => crate::types::Result::Ok {
                            value: EvaluationResult {
                                value: crate::types::Value::from_core_value(
                                    &value,
                                    &heap,
                                    env.get_program(),
                                ),
                                heap,
                            },
                        },
                        Err(e) => crate::types::Result::Err {
                            error: format!("{:?}", e),
                        },
                    });

                    ProcessInfo {
                        id: info.id,
                        status: info.status.into(),
                        process_type,
                        stack_size: info.stack_size,
                        locals_count: info.locals_count,
                        frames_count: info.frames_count,
                        mailbox_size: info.mailbox_size,
                        persistent: info.persistent,
                        result,
                        heap: info.heap.into(),
                    }
                });

                callback.invoke(crate::types::Result::ok(js_info));
            }
            RequestResult::Locals(_) => {
                // Not used in the web API
                callback.invoke::<()>(crate::types::Result::err("Unexpected result type: Locals"));
            }
            RequestResult::WorkerInfo(workers) => {
                let js_workers: Vec<WorkerInfo> =
                    workers.into_iter().map(WorkerInfo::from).collect();
                callback.invoke(crate::types::Result::ok(js_workers));
            }
            RequestResult::ProcessTypes(_) => {
                // Process types are cached automatically by the polling loop
                // Just return success to the callback
                callback.invoke::<()>(crate::types::Result::ok(()));
            }
        }
    }
}

#[wasm_bindgen(skip_typescript)]
pub struct Repl {
    environment: SharedEnvironment,
    pending_callbacks: SharedCallbacks,
    pending_evaluations: SharedPendingEvaluations,
    pump: PumpHandle,
    repl: Rc<RefCell<quiver_environment::Repl<WebEffect>>>,
}

#[wasm_bindgen]
impl Repl {
    /// Create a new REPL using the given environment.
    ///
    /// `files` is an optional `Record<string, string>` of virtual path → source (e.g. the
    /// browser's file explorer), which becomes the in-memory package that imports resolve
    /// against. A `quiver.toml` entry, if present, defines the module routing table.
    #[wasm_bindgen(constructor)]
    pub fn new(environment: &Environment, files: JsValue) -> std::result::Result<Repl, JsValue> {
        // Set up panic hook for better error messages
        console_error_panic_hook::set_once();

        // Get shared state from environment
        let (env_rc, callbacks_rc, _process_types_rc, evaluations_rc, pump) =
            environment.get_shared_state();

        // Create REPL
        let resolver = create_resolver(parse_files(files)?)?;
        // For WASM, use core modules only (no network builtins)
        let builtins = quiver_core::builtins::BuiltinRegistry::with_modules(
            &quiver_core::builtins::core_modules(),
        );
        let repl = quiver_environment::Repl::new(&mut *env_rc.borrow_mut(), resolver, builtins)
            .map_err(|e| JsValue::from_str(&format!("Failed to create REPL: {}", e)))?;

        Ok(Self {
            environment: env_rc,
            pending_callbacks: callbacks_rc,
            pending_evaluations: evaluations_rc,
            pump,
            repl: Rc::new(RefCell::new(repl)),
        })
    }

    /// Replace the REPL's virtual filesystem, so subsequent evaluations resolve imports against
    /// the new files (and `quiver.toml`). Accumulated variables are preserved; bindings that
    /// already captured values from a previous version of a module keep those values. Returns an
    /// error (without disturbing the current resolver) if a `quiver.toml` in `files` is invalid.
    #[wasm_bindgen(js_name = "setFiles")]
    pub fn set_files(&mut self, files: JsValue) -> std::result::Result<(), JsValue> {
        let resolver = create_resolver(parse_files(files)?)?;
        self.repl.borrow_mut().reload_modules(resolver);
        Ok(())
    }

    /// Refresh the cache of process types
    ///
    /// Call this before evaluate() to ensure process references (@N) work correctly.
    /// The callback will be invoked when types are ready (or on error).
    #[wasm_bindgen(js_name = "refreshProcessTypes")]
    pub fn refresh_process_types(&mut self, callback: JsValue) {
        let callback: js_sys::Function = callback.into();

        match self.environment.borrow_mut().request_process_types() {
            Ok(request_id) => {
                // The loop will cache the types and invoke the callback; wake it to do so.
                self.pending_callbacks
                    .borrow_mut()
                    .insert(request_id, CallbackHandle::new(callback));
                wake(&self.pump);
            }
            Err(e) => {
                let cb = CallbackHandle::new(callback);
                cb.invoke::<()>(crate::types::Result::err(e.to_string()));
            }
        }
    }

    /// Evaluate source code and invoke callback when result is ready
    ///
    /// Note: Call refreshProcessTypes() first if you need process references (@N) to work.
    pub fn evaluate(&mut self, source: String, callback: JsValue) {
        let callback: js_sys::Function = callback.into();

        // Queue the evaluation to be processed by the loop (avoids RefCell borrow conflicts), then
        // wake the loop so it dispatches it.
        self.pending_evaluations.borrow_mut().push((
            source,
            CallbackHandle::new(callback),
            self.repl.clone(),
        ));
        wake(&self.pump);
    }

    /// Get all variables (synchronous)
    #[wasm_bindgen(js_name = "getVariables")]
    pub fn get_variables(&self, callback: JsValue) {
        let callback: js_sys::Function = callback.into();
        let repl = self.repl.borrow();
        let vars = repl.get_variables();
        let js_vars: Vec<Variable> = vars
            .into_iter()
            .map(|(name, var_type)| Variable { name, var_type })
            .collect();

        let cb = CallbackHandle::new(callback);
        cb.invoke(crate::types::Result::ok(js_vars));
    }

    /// The id of this REPL's persistent process.
    #[wasm_bindgen(getter, js_name = "processId")]
    pub fn process_id(&self) -> usize {
        self.repl.borrow().process_id()
    }
}
