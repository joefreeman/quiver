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
   * Allocate a fresh persistent (session) process, sleeping and ready for `resumeProcess`.
   * The environment half of the split-driver API: a compiler worker (`compiler_worker_main`)
   * produces payloads, this class runs them.
   */
  createProcess(): number;

  /**
   * Re-align a session process's locals to a keep-set (the `compactKeep` of a compiler
   * worker's `evaluated` response). Apply before the resume it belongs to; command order
   * is preserved, so no ack is needed.
   */
  compactProcess(pid: number, keep: number[]): void;

  /**
   * Resume a session process on a compiler worker's payload (its JSON, verbatim): link the
   * attached module units — validated against their content keys — and run the entry.
   * Returns `{ pending: true }` when the resume is in flight (the callback will fire with
   * the result), or `{ missing }` naming module keys this environment no longer holds —
   * resend those and retry; the callback does not fire. Throws on an invalid payload.
   */
  resumeProcess(
    pid: number,
    payload: string,
    keep: number[] | undefined,
    callback: EvaluateCallback,
  ): { pending?: boolean; missing?: string[] };

  /**
   * Stop a session process — the host-side interrupt. A pending resume callback settles
   * with "Interrupted", everything the process spawned is torn down, and the process is
   * gone: create a new one to continue.
   */
  stopProcess(pid: number): void;

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

/**
 * A request to the compiler worker (`compiler_worker_main`), sent as a JSON string over
 * `postMessage`. Send `init` first; every request carries an `id` echoed on its response.
 */
export type CompilerRequest =
  | // `debug` defaults to true: provenance and checked assertions, like the native REPL.
    { type: "init"; id: number; files?: Record<string, string>; debug?: boolean }
  | { type: "evaluate"; id: number; source: string }
  | { type: "setFiles"; id: number; files: Record<string, string> }
  | { type: "variables"; id: number }
  | // Start the session over (fresh bindings, current files, warm caches); the caller
    // allocates a fresh process via `createProcess` and continues.
    { type: "reset"; id: number };

/**
 * A response from the compiler worker, received as a JSON string. `ready` arrives once at
 * boot. An `evaluated` response carries the session bookkeeping and the payload to hand to
 * `Environment.resumeProcess` (or any host speaking the same payload): apply `compactKeep`
 * via `compactProcess`, then resume with the payload JSON and `keepIndices`. A null
 * payload means the line had nothing to run (type definitions alone).
 */
export type CompilerResponse =
  | { type: "ready" }
  | { type: "ok"; id: number }
  | { type: "error"; id: number | null; message: string }
  | {
      type: "evaluated";
      id: number;
      compactKeep: number[];
      keepIndices: number[];
      /** WirePayload JSON — pass verbatim to `Environment.resumeProcess`. */
      payload: string | null;
      resultType: string;
    }
  | { type: "variables"; id: number; variables: Variable[] };
"#;

/// Build a resolver over an in-memory file map. A `quiver.toml` in the map defines the module
/// routing table; without one, files resolve by path and may shadow the standard library.
pub(crate) fn create_session_resolver(
    files: HashMap<String, String>,
) -> std::result::Result<Box<PackageResolver>, String> {
    PackageResolver::memory_files(files)
        // The browser's tag: `%http/transport` resolves to `transport.web.qv` (over `fetch`)
        // rather than `transport.native.qv` (over sockets, which do not exist here).
        .map(|resolver| Box::new(resolver.with_host_tags(vec!["web".to_string()])))
        .map_err(|e| e.to_string())
}

/// `resumeProcess`'s synchronous answer: the resume is in flight (the callback will
/// fire), or the module keys the environment no longer holds. A struct rather than a
/// `serde_json` map, deliberately: `serde_wasm_bindgen` turns maps into ES `Map`s,
/// while the plain object the TS signature promises comes from a struct.
#[derive(serde::Serialize)]
struct ResumeStart {
    #[serde(skip_serializing_if = "Option::is_none")]
    pending: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    missing: Option<Vec<String>>,
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
pub type PumpHandle = Rc<RefCell<Option<Pump>>>;
type SharedEnvironment = Rc<RefCell<quiver_environment::Environment<WebEffect>>>;
type SharedCallbacks = Rc<RefCell<HashMap<u64, CallbackHandle>>>;
/// Wake the main-thread loop if it exists yet. A no-op before `Environment::new` installs the
/// pump, which is fine: nothing queues work against the environment before then.
pub fn wake(handle: &PumpHandle) {
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

        // Create environment, with the browser's effect backend: `fetch` parks its process
        // while a promise runs, and the backend wakes this loop when the completion lands.
        let mut environment = quiver_environment::Environment::new(workers);
        environment.set_effect_backend(Box::new(crate::backend::WebEffectBackend::new(
            pump.clone(),
        )));
        // The scoped (always-set) registry still declares the crash and Changed
        // vocabulary; it declares no streams, matching the absent io capability.
        let web_builtins = crate::builtins::web_builtins();
        environment.set_runtime_declarations(web_builtins.runtime_declarations().clone());
        let environment_rc = Rc::new(RefCell::new(environment));
        let pending_callbacks = Rc::new(RefCell::new(HashMap::new()));
        let subscription_callbacks = Rc::new(RefCell::new(HashMap::new()));
        let running = Rc::new(RefCell::new(false));

        // Install the event-driven main-thread loop. It starts idle (running == false); `start()`
        // arms it. The worker `onmessage` handlers above and the JS-facing methods below wake it
        // whenever they queue work, so it only ticks when there is something to do.
        *pump.borrow_mut() = Some(Self::build_pump(
            environment_rc.clone(),
            running.clone(),
            pending_callbacks.clone(),
            subscription_callbacks.clone(),
        ));

        Ok(Self {
            environment: environment_rc,
            running,
            pump,
            pending_callbacks,
            subscription_callbacks,
        })
    }

    /// Build the main-thread loop. Each tick advances the environment and drains ready work; the
    /// returned `Tick` keeps it running while work flows and lets it sleep once everything is
    /// quiet (waiting for a `wake` from a worker event or a JS-side call).
    fn build_pump(
        environment: SharedEnvironment,
        running: Rc<RefCell<bool>>,
        pending_callbacks: SharedCallbacks,
        subscription_callbacks: SharedCallbacks,
    ) -> Pump {
        Pump::new(move || {
            if !*running.borrow() {
                return Tick::Idle;
            }
            let did_work =
                Self::run_tick(&environment, &pending_callbacks, &subscription_callbacks);
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
    ) -> bool {
        // Step the environment
        let mut did_work = match environment.borrow_mut().step() {
            Ok(did_work) => did_work,
            // A step failure used to vanish here, leaving a process parked on an effect that
            // would never complete and no sign of why.
            Err(error) => {
                web_sys::console::error_1(&format!("Quiver environment error: {error}").into());
                false
            }
        };

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

    /// Allocate a fresh persistent (session) process, sleeping and ready for
    /// `resumeProcess`. The environment half of the split-driver API: a compiler
    /// worker produces payloads, this class runs them.
    #[wasm_bindgen(js_name = "createProcess")]
    pub fn create_process(&mut self) -> std::result::Result<f64, JsValue> {
        let pid = self
            .environment
            .borrow_mut()
            .start_process()
            .map_err(|e| JsValue::from_str(&e.to_string()))?;
        wake(&self.pump);
        Ok(pid as f64)
    }

    /// Re-align a session process's locals to a keep-set (the `compactKeep` of a
    /// compiler worker's `evaluated` response) — apply before the resume it belongs to.
    /// Command order to the process's worker is preserved, so this needs no ack.
    #[wasm_bindgen(js_name = "compactProcess")]
    pub fn compact_process(&mut self, pid: f64, keep: JsValue) -> std::result::Result<(), JsValue> {
        let keep: Vec<usize> = serde_wasm_bindgen::from_value(keep)
            .map_err(|e| JsValue::from_str(&format!("Invalid keep-set: {e}")))?;
        self.environment
            .borrow_mut()
            .compact_locals(pid as usize, keep)
            .map_err(|e| JsValue::from_str(&e.to_string()))?;
        wake(&self.pump);
        Ok(())
    }

    /// Resume a session process on a compiler worker's payload (its JSON, verbatim):
    /// link the attached module units — validated against their content keys — and run
    /// the entry, delivering the result to `callback`.
    ///
    /// Answers `{ pending: true }` when the resume is in flight (the callback will
    /// fire), or `{ missing: [keys] }` when the payload names modules this environment
    /// no longer holds — resend those and retry; the callback does not fire. An
    /// invalid payload throws.
    #[wasm_bindgen(js_name = "resumeProcess")]
    pub fn resume_process(
        &mut self,
        pid: f64,
        payload: String,
        keep: JsValue,
        callback: JsValue,
    ) -> std::result::Result<JsValue, JsValue> {
        let payload: quiver_environment::WirePayload = serde_json::from_str(&payload)
            .map_err(|e| JsValue::from_str(&format!("Invalid payload: {e}")))?;
        let keep: Option<Vec<usize>> = if keep.is_undefined() || keep.is_null() {
            None
        } else {
            Some(
                serde_wasm_bindgen::from_value(keep)
                    .map_err(|e| JsValue::from_str(&format!("Invalid keep-set: {e}")))?,
            )
        };
        let callback: js_sys::Function = callback.into();
        let builtins = crate::builtins::web_builtins();

        let request = {
            let mut env = self.environment.borrow_mut();
            let missing = env
                .link_payload_modules(&payload.modules, &payload.unit, &builtins)
                .map_err(|e| JsValue::from_str(&e.to_string()))?;
            if !missing.is_empty() {
                let keys: Vec<String> = missing.iter().map(|key| key.to_string()).collect();
                return serde_wasm_bindgen::to_value(&ResumeStart {
                    pending: None,
                    missing: Some(keys),
                })
                .map_err(|e| JsValue::from_str(&e.to_string()));
            }
            env.resume_process_unit(pid as usize, &payload.unit, &builtins)
                .map_err(|e| JsValue::from_str(&e.to_string()))?;
            env.request_result(pid as usize, keep)
                .map_err(|e| JsValue::from_str(&e.to_string()))?
        };
        self.pending_callbacks
            .borrow_mut()
            .insert(request, CallbackHandle::new(callback));
        wake(&self.pump);
        serde_wasm_bindgen::to_value(&ResumeStart {
            pending: Some(true),
            missing: None,
        })
        .map_err(|e| JsValue::from_str(&e.to_string()))
    }

    /// Stop a session process — the host-side interrupt. A pending resume callback
    /// settles with "Interrupted", everything the process spawned is torn down, and
    /// the process is gone: create a new one to continue.
    #[wasm_bindgen(js_name = "stopProcess")]
    pub fn stop_process(&mut self, pid: f64) -> std::result::Result<(), JsValue> {
        self.environment
            .borrow_mut()
            .stop_process(pid as usize)
            .map_err(|e| JsValue::from_str(&e.to_string()))?;
        wake(&self.pump);
        Ok(())
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
            .format_core_value(&core_value, &extended_heap))
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
            RequestResult::Result(Ok(value)) => {
                // A stamped nil's failure provenance, surfaced alongside the value the
                // way the native REPL prints it.
                let origin = if value.is_nil() {
                    env.describe_origin(&value)
                } else {
                    None
                };
                // The JS bridge speaks `(Value, heap)`; a wire value renders to that pair.
                let (core, heap) = value.for_display();
                callback.invoke(crate::types::Result::ok(Some(EvaluationResult {
                    value: crate::types::Value::from_core_value(&core, env.get_program()),
                    heap,
                    // The compiler worker reports the line's static type in its
                    // `evaluated` response; the caller joins the two.
                    result_type: None,
                    origin,
                })));
            }
            RequestResult::Result(Err(e)) => {
                // A `Killed` result can only come from a host stop (`Repl.interrupt`) —
                // no in-language kill reaches a persistent session process — so report
                // it as the interruption it is rather than a runtime error.
                let message = if matches!(e, quiver_core::error::Error::Killed) {
                    "Interrupted".to_string()
                } else {
                    format!("Runtime error: {:?}", e)
                };
                callback.invoke::<EvaluationResult>(crate::types::Result::err(message));
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
                        Ok(value) => crate::types::Result::Ok {
                            value: {
                                let origin = if value.is_nil() {
                                    env.describe_origin(&value)
                                } else {
                                    None
                                };
                                let (core, heap) = value.for_display();
                                EvaluationResult {
                                    value: crate::types::Value::from_core_value(
                                        &core,
                                        env.get_program(),
                                    ),
                                    heap,
                                    result_type: None,
                                    origin,
                                }
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
                // Nothing on the web requests process types (`@N` is unsupported here).
                callback.invoke::<()>(crate::types::Result::err(
                    "Unexpected result type: ProcessTypes",
                ));
            }
        }
    }
}
