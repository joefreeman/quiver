use crate::types::*;
use crate::web_transport::WebWorkerHandle;
use quiver_compiler::modules::InMemoryModuleLoader;
use quiver_core::program::Program;
use quiver_environment::{RequestResult, WorkerHandle};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::Worker;

// Define TypeScript types for callbacks and the Repl class
#[wasm_bindgen(typescript_custom_section)]
const TS_DEFINITIONS: &'static str = r#"
export type WorkerFactory = () => Worker;
export type EvaluateCallback = (result: JsResult<JsEvaluationResult | null>) => void;
export type VariablesCallback = (result: JsResult<JsVariable[]>) => void;
export type ProcessStatusesCallback = (result: JsResult<JsProcess[]>) => void;
export type ProcessInfoCallback = (result: JsResult<JsProcessInfo | null>) => void;

export class Repl {
  free(): void;
  [Symbol.dispose](): void;

  /**
   * Create a new REPL with the specified number of workers
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
   * Evaluate source code and invoke callback when result is ready
   * @param source - Quiver source code to evaluate
   * @param callback - Callback invoked with the evaluation result
   */
  evaluate(source: string, callback: EvaluateCallback): void;

  /**
   * Get all variables defined in the REPL
   * @param callback - Callback invoked with the list of variables
   */
  get_variables(callback: VariablesCallback): void;

  /**
   * Get all process statuses
   * @param callback - Callback invoked with process statuses
   */
  get_process_statuses(callback: ProcessStatusesCallback): void;

  /**
   * Get info for a specific process
   * @param pid - Process ID
   * @param callback - Callback invoked with process info (or null if not found)
   */
  get_process_info(pid: number, callback: ProcessInfoCallback): void;

  /**
   * Format a value for display
   * @param value - Value to format
   * @param heap - Heap data
   * @returns Formatted string representation
   */
  format_value(value: QuiverValue, heap: number[][]): string;

  /**
   * Format a type for display (derives type from value)
   * @param value - Value to derive type from
   * @returns Formatted string representation
   */
  format_type(value: QuiverValue): string;
}
"#;

// Embed standard library files at compile time
const STD_MATH: &str = include_str!("../../std/math.qv");
const STD_LIST: &str = include_str!("../../std/list.qv");

/// Create a module loader with the standard library pre-loaded
fn create_std_module_loader() -> Box<InMemoryModuleLoader> {
    let mut modules = HashMap::new();
    modules.insert("math".to_string(), STD_MATH.to_string());
    modules.insert("list".to_string(), STD_LIST.to_string());
    Box::new(InMemoryModuleLoader::new(modules))
}

/// Callback wrapper to store JS callbacks
struct CallbackHandle {
    callback: js_sys::Function,
}

impl CallbackHandle {
    fn new(callback: js_sys::Function) -> Self {
        Self { callback }
    }

    fn invoke<T: serde::Serialize>(&self, result: JsResult<T>) {
        let js_value = serde_wasm_bindgen::to_value(&result).unwrap();
        let _ = self.callback.call1(&JsValue::NULL, &js_value);
    }
}

type EventLoopClosure = Rc<RefCell<Option<Closure<dyn FnMut()>>>>;

#[wasm_bindgen(skip_typescript)]
pub struct Repl {
    environment: Rc<RefCell<quiver_environment::Environment>>,
    repl: Rc<RefCell<quiver_environment::Repl>>,
    running: Rc<RefCell<bool>>,
    event_loop_closure: EventLoopClosure,
    pending_callbacks: Rc<RefCell<HashMap<u64, CallbackHandle>>>,
}

#[wasm_bindgen]
impl Repl {
    /// Create a new REPL with the specified number of workers
    /// worker_factory: A JavaScript function that returns a new Worker
    #[wasm_bindgen(constructor)]
    pub fn new(num_workers: usize, worker_factory: JsValue) -> Result<Repl, JsValue> {
        let worker_factory: js_sys::Function = worker_factory.into();
        // Set up panic hook for better error messages
        console_error_panic_hook::set_once();

        // Create workers by calling the factory function
        let mut workers: Vec<Box<dyn WorkerHandle>> = Vec::new();
        for _ in 0..num_workers {
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

            // Set up message handler for worker events
            let onmessage = Closure::wrap(Box::new(move |event: web_sys::MessageEvent| {
                if let Some(text) = event.data().as_string() {
                    if text == "ready" {
                        // Worker is ready - mark it and flush pending commands
                        *ready_flag.borrow_mut() = true;

                        // Send all pending commands
                        while let Some(cmd) = pending_commands.borrow_mut().pop_front() {
                            if let Ok(json) = serde_json::to_string(&cmd) {
                                let _ = worker_for_closure.post_message(&JsValue::from_str(&json));
                            }
                        }
                    } else {
                        // Parse as Event
                        match serde_json::from_str::<quiver_environment::Event>(&text) {
                            Ok(event) => {
                                event_queue_for_closure.borrow_mut().push_back(event);
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

        // Create environment and REPL
        let environment = quiver_environment::Environment::new(workers);
        let program = Program::new();
        let module_loader = create_std_module_loader();
        let repl = quiver_environment::Repl::new(program, module_loader);

        Ok(Self {
            environment: Rc::new(RefCell::new(environment)),
            repl: Rc::new(RefCell::new(repl)),
            running: Rc::new(RefCell::new(false)),
            event_loop_closure: Rc::new(RefCell::new(None)),
            pending_callbacks: Rc::new(RefCell::new(HashMap::new())),
        })
    }

    /// Start the event loop
    pub fn start(&mut self) {
        if *self.running.borrow() {
            return; // Already running
        }

        *self.running.borrow_mut() = true;

        let environment = self.environment.clone();
        let repl = self.repl.clone();
        let running = self.running.clone();
        let pending_callbacks = self.pending_callbacks.clone();

        // Use self.event_loop_closure as the holder directly
        let closure_holder = self.event_loop_closure.clone();

        // Create the closure that captures the closure holder
        let tick_fn = Closure::wrap(Box::new(move || {
            if !*running.borrow() {
                return; // Stop the loop
            }

            // Step the environment
            let _ = environment.borrow_mut().step();

            // Process pending callbacks
            let mut callbacks_to_invoke = Vec::new();
            {
                let mut callbacks = pending_callbacks.borrow_mut();
                let request_ids: Vec<u64> = callbacks.keys().copied().collect();

                for request_id in request_ids {
                    match repl
                        .borrow_mut()
                        .poll_request(&mut environment.borrow_mut(), request_id)
                    {
                        Ok(Some(result)) => {
                            if let Some(callback) = callbacks.remove(&request_id) {
                                callbacks_to_invoke.push((callback, result));
                            }
                        }
                        Ok(None) => {
                            // Not ready yet
                        }
                        Err(e) => {
                            // Error polling request
                            if let Some(callback) = callbacks.remove(&request_id) {
                                callback.invoke::<()>(JsResult::err(e.to_string()));
                            }
                        }
                    }
                }
            }

            // Invoke callbacks outside of the borrow
            for (callback, result) in callbacks_to_invoke {
                Self::handle_result(&environment.borrow(), callback, result);
            }

            // Schedule next tick if still running
            if *running.borrow() {
                let window = js_sys::global();
                let set_timeout = js_sys::Reflect::get(&window, &JsValue::from_str("setTimeout"))
                    .expect("setTimeout not found");
                let set_timeout: js_sys::Function = set_timeout.dyn_into().unwrap();

                // Get reference to self from the holder
                let closure_ref = closure_holder.borrow();
                if let Some(ref callback) = *closure_ref {
                    let _ = set_timeout.call2(
                        &window,
                        callback.as_ref().unchecked_ref(),
                        &JsValue::from_f64(0.0),
                    );
                }
            }
        }) as Box<dyn FnMut()>);

        // Store the closure in the holder
        *self.event_loop_closure.borrow_mut() = Some(tick_fn);

        // Start the loop with the first tick
        let window = js_sys::global();
        let set_timeout = js_sys::Reflect::get(&window, &JsValue::from_str("setTimeout"))
            .expect("setTimeout not found");
        let set_timeout: js_sys::Function = set_timeout.dyn_into().unwrap();

        let closure_ref = self.event_loop_closure.borrow();
        if let Some(ref cb) = *closure_ref {
            let _ = set_timeout.call2(
                &window,
                cb.as_ref().unchecked_ref(),
                &JsValue::from_f64(0.0),
            );
        }
    }

    /// Stop the event loop
    pub fn stop(&mut self) {
        *self.running.borrow_mut() = false;
    }

    /// Evaluate source code and invoke callback when result is ready
    pub fn evaluate(&mut self, source: String, callback: JsValue) {
        let callback: js_sys::Function = callback.into();
        let result = self
            .repl
            .borrow_mut()
            .evaluate(&mut self.environment.borrow_mut(), &source);

        match result {
            Ok(Some(request_id)) => {
                // Store callback for later
                self.pending_callbacks
                    .borrow_mut()
                    .insert(request_id, CallbackHandle::new(callback));
            }
            Ok(None) => {
                // No executable code (e.g., only type definitions)
                let cb = CallbackHandle::new(callback);
                cb.invoke(JsResult::ok(None::<JsEvaluationResult>));
            }
            Err(e) => {
                let cb = CallbackHandle::new(callback);
                cb.invoke::<JsEvaluationResult>(JsResult::err(format!("{}", e)));
            }
        }
    }

    /// Get all variables (synchronous)
    pub fn get_variables(&self, callback: JsValue) {
        let callback: js_sys::Function = callback.into();
        let environment = self.environment.borrow();
        let repl = self.repl.borrow();
        let vars = repl.get_variables();
        let js_vars: Vec<JsVariable> = vars
            .into_iter()
            .map(|(name, ty)| JsVariable {
                name,
                var_type: environment.format_type(&ty),
            })
            .collect();

        let cb = CallbackHandle::new(callback);
        cb.invoke(JsResult::ok(js_vars));
    }

    /// Get all process statuses
    pub fn get_process_statuses(&mut self, callback: JsValue) {
        let callback: js_sys::Function = callback.into();
        match self.environment.borrow_mut().request_statuses() {
            Ok(request_id) => {
                self.pending_callbacks
                    .borrow_mut()
                    .insert(request_id, CallbackHandle::new(callback));
            }
            Err(e) => {
                let cb = CallbackHandle::new(callback);
                cb.invoke::<Vec<JsProcess>>(JsResult::err(e.to_string()));
            }
        }
    }

    /// Get info for a specific process
    pub fn get_process_info(&mut self, pid: usize, callback: JsValue) {
        let callback: js_sys::Function = callback.into();
        match self.environment.borrow_mut().request_process_info(pid) {
            Ok(request_id) => {
                self.pending_callbacks
                    .borrow_mut()
                    .insert(request_id, CallbackHandle::new(callback));
            }
            Err(e) => {
                let cb = CallbackHandle::new(callback);
                cb.invoke::<Option<JsProcessInfo>>(JsResult::err(e.to_string()));
            }
        }
    }

    /// Format a value for display
    pub fn format_value(
        &self,
        value: wasm_bindgen::JsValue,
        heap: wasm_bindgen::JsValue,
    ) -> Result<String, wasm_bindgen::JsValue> {
        let value: crate::types::QuiverValue = serde_wasm_bindgen::from_value(value)?;
        let heap: Vec<Vec<u8>> = serde_wasm_bindgen::from_value(heap)?;
        let value: quiver_core::value::Value = value.into();
        Ok(self.environment.borrow().format_value(&value, &heap))
    }

    /// Format a type for display (derives type from value)
    pub fn format_type(
        &self,
        value: wasm_bindgen::JsValue,
    ) -> Result<String, wasm_bindgen::JsValue> {
        let value: crate::types::QuiverValue = serde_wasm_bindgen::from_value(value)?;
        let value: quiver_core::value::Value = value.into();
        let ty = self.environment.borrow().value_to_type(&value);
        Ok(self.environment.borrow().format_type(&ty))
    }

    // Helper to convert RequestResult to appropriate callback invocation
    fn handle_result(
        env: &quiver_environment::Environment,
        callback: CallbackHandle,
        result: RequestResult,
    ) {
        match result {
            RequestResult::Result(Ok((value, heap))) => {
                callback.invoke(JsResult::ok(Some(JsEvaluationResult {
                    value: value.into(),
                    heap,
                })));
            }
            RequestResult::Result(Err(e)) => {
                callback
                    .invoke::<JsEvaluationResult>(JsResult::err(format!("Runtime error: {:?}", e)));
            }
            RequestResult::Statuses(statuses) => {
                let mut processes: Vec<JsProcess> = statuses
                    .into_iter()
                    .map(|(id, status)| JsProcess {
                        id,
                        status: status.into(),
                    })
                    .collect();
                processes.sort_by_key(|p| p.id);

                callback.invoke(JsResult::ok(processes));
            }
            RequestResult::ProcessInfo(info_opt) => {
                let js_info = info_opt.map(|info| {
                    // Get the formatted process type
                    let process_type = env.format_process_type(info.function_index);

                    let result = info.result.map(|r| match r {
                        Ok(value) => JsResult::Ok {
                            value: JsEvaluationResult {
                                value: value.into(),
                                heap: vec![],
                            },
                        },
                        Err(e) => JsResult::Err {
                            error: format!("{:?}", e),
                        },
                    });

                    JsProcessInfo {
                        id: info.id,
                        status: info.status.into(),
                        process_type,
                        stack_size: info.stack_size,
                        locals_size: info.locals_size,
                        frames_count: info.frames_count,
                        mailbox_size: info.mailbox_size,
                        persistent: info.persistent,
                        result,
                    }
                });

                callback.invoke(JsResult::ok(js_info));
            }
            RequestResult::Locals(_) => {
                // Not used in the web API
                callback.invoke::<()>(JsResult::err("Unexpected result type: Locals"));
            }
        }
    }
}
