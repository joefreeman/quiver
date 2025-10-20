use quiver_compiler::InMemoryModuleLoader;
use quiver_core::program::Program;
use quiver_environment::WorkerHandle;
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use web_sys::{MessageEvent, Worker, WorkerOptions, WorkerType};

use crate::web_transport;

/// Load standard library modules at compile time
fn create_stdlib_loader() -> InMemoryModuleLoader {
    let mut modules = HashMap::new();

    // Include standard library modules
    modules.insert(
        "list".to_string(),
        include_str!("../../std/list.qv").to_string(),
    );
    modules.insert(
        "math".to_string(),
        include_str!("../../std/math.qv").to_string(),
    );

    InMemoryModuleLoader::new(modules)
}

/// WASM-bindgen wrapper for Repl
#[wasm_bindgen]
pub struct Repl {
    inner: RefCell<quiver_environment::Repl>,
    callbacks: Rc<RefCell<HashMap<u64, js_sys::Function>>>,
}

// TypeScript callback type definitions
#[wasm_bindgen(typescript_custom_section)]
const TS_APPEND_CONTENT: &'static str = r#"
/**
 * Callback for evaluation results
 * @param result - The evaluation result with value and heap, or null if error
 * @param error - Error message if evaluation failed, or null if successful
 */
export type EvaluateCallback = (
    result: { value: any; heap: number[][] } | null,
    error: string | null
) => void;

/**
 * Callback for variable value requests
 * @param result - Array of variable values with their heaps, or null if error
 * @param error - Error message if request failed, or null if successful
 */
export type VariableCallback = (
    result: Array<{ value: any; heap: number[][] }> | null,
    error: string | null
) => void;

/**
 * Callback for process status requests
 * @param result - Process status information, or null if error
 * @param error - Error message if request failed, or null if successful
 */
export type StatusesCallback = (
    result: any | null,
    error: string | null
) => void;

/**
 * Callback for process info requests
 * @param result - Process information, or null if error
 * @param error - Error message if request failed, or null if successful
 */
export type ProcessInfoCallback = (
    result: any | null,
    error: string | null
) => void;
"#;

#[wasm_bindgen]
impl Repl {
    /// Create a new REPL with the specified number of workers
    /// Accepts either:
    /// - A string URL (e.g., "./worker.js")
    /// - A factory function that creates Workers (e.g., () => new Worker(...))
    #[wasm_bindgen(constructor)]
    pub fn new(num_workers: usize, worker_url_or_factory: JsValue) -> Result<Repl, String> {
        // Create workers
        let mut workers: Vec<Box<dyn WorkerHandle>> = Vec::new();

        for _ in 0..num_workers {
            // Check if it's a factory function or a string URL
            let worker = if worker_url_or_factory.is_function() {
                // It's a factory function - call it to get a Worker
                let factory = js_sys::Function::from(worker_url_or_factory.clone());
                let worker_value = factory
                    .call0(&JsValue::NULL)
                    .map_err(|e| format!("Worker factory function failed: {:?}", e))?;
                Worker::from(worker_value)
            } else if let Some(worker_url_str) = worker_url_or_factory.as_string() {
                // It's a string URL - create worker ourselves
                let worker_options = WorkerOptions::new();
                worker_options.set_type(WorkerType::Module);

                Worker::new_with_options(&worker_url_str, &worker_options)
                    .map_err(|e| format!("Failed to create worker: {:?}", e))?
            } else {
                return Err(
                    "worker_url_or_factory must be a string or factory function".to_string()
                );
            };

            // Create event queue for this worker
            let event_queue: Rc<RefCell<VecDeque<quiver_environment::Event>>> =
                Rc::new(RefCell::new(VecDeque::new()));
            let event_queue_clone = event_queue.clone();

            let handle = web_transport::WebWorkerHandle::new(worker.clone(), event_queue);
            let ready = handle.ready();
            let pending_commands = handle.pending_commands();
            let worker_for_ready = worker.clone();

            // Set up message handler
            let onmessage = Closure::wrap(Box::new(move |event: MessageEvent| {
                if let Some(text) = event.data().as_string() {
                    // Check for ready signal
                    if text == "ready" {
                        *ready.borrow_mut() = true;

                        let mut pending = pending_commands.borrow_mut();
                        while let Some(command) = pending.pop_front() {
                            if let Ok(json) = serde_json::to_string(&command) {
                                if let Err(e) = worker_for_ready
                                    .post_message(&wasm_bindgen::JsValue::from_str(&json))
                                {
                                    web_sys::console::error_1(
                                        &format!("Failed to flush command: {:?}", e).into(),
                                    );
                                }
                            }
                        }
                        return;
                    }

                    // Parse as Event
                    match serde_json::from_str::<quiver_environment::Event>(&text) {
                        Ok(evt) => {
                            event_queue_clone.borrow_mut().push_back(evt);
                        }
                        Err(e) => {
                            web_sys::console::error_1(
                                &format!("Failed to parse event: {}", e).into(),
                            );
                        }
                    }
                }
            }) as Box<dyn FnMut(MessageEvent)>);

            worker.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
            onmessage.forget(); // Keep closure alive

            workers.push(Box::new(handle));
        }

        // Create module loader with embedded standard library
        let module_loader = Box::new(create_stdlib_loader());

        let program = Program::new();
        let inner = quiver_environment::Repl::new(workers, program, module_loader)
            .map_err(|e| e.to_string())?;

        Ok(Repl {
            inner: RefCell::new(inner),
            callbacks: Rc::new(RefCell::new(HashMap::new())),
        })
    }

    /// Evaluate an expression with a callback
    #[wasm_bindgen(skip_typescript)]
    pub fn evaluate(&self, source: &str, callback: js_sys::Function) -> Result<(), String> {
        let request_id = self
            .inner
            .borrow_mut()
            .evaluate(source)
            .map_err(|e| format!("{:?}", e))?;
        self.callbacks.borrow_mut().insert(request_id, callback);
        Ok(())
    }

    /// Request a variable value by name with a callback
    #[wasm_bindgen(skip_typescript)]
    pub fn get_variable(&self, name: &str, callback: js_sys::Function) -> Result<(), String> {
        let request_id = self
            .inner
            .borrow_mut()
            .request_variable(name)
            .map_err(|e| e.to_string())?;
        self.callbacks.borrow_mut().insert(request_id, callback);
        Ok(())
    }

    /// Get all variable names and their types
    /// Throws exception on serialization failure
    #[wasm_bindgen]
    pub fn get_variables(&self) -> Result<JsValue, String> {
        let variables = self.inner.borrow().get_variables();
        serde_wasm_bindgen::to_value(&variables)
            .map_err(|e| format!("Failed to serialize variables: {}", e))
    }

    /// Request all process statuses across all workers with a callback
    #[wasm_bindgen(skip_typescript)]
    pub fn get_process_statuses(&self, callback: js_sys::Function) -> Result<(), String> {
        let request_id = self
            .inner
            .borrow_mut()
            .request_process_statuses()
            .map_err(|e| e.to_string())?;
        self.callbacks.borrow_mut().insert(request_id, callback);
        Ok(())
    }

    /// Request process info for a specific process with a callback
    #[wasm_bindgen(skip_typescript)]
    pub fn get_process_info(&self, pid: usize, callback: js_sys::Function) -> Result<(), String> {
        let request_id = self
            .inner
            .borrow_mut()
            .request_process_info(pid)
            .map_err(|e| e.to_string())?;
        self.callbacks.borrow_mut().insert(request_id, callback);
        Ok(())
    }

    /// Step the environment (process events) and drive callbacks
    #[wasm_bindgen]
    pub fn step(&self) -> Result<bool, String> {
        let did_work = self.inner.borrow_mut().step().map_err(|e| e.to_string())?;

        // Poll all pending requests and invoke callbacks when ready
        let request_ids: Vec<u64> = self.callbacks.borrow().keys().cloned().collect();
        for request_id in request_ids {
            // Explicitly separate the borrow from the match to ensure it's dropped
            let poll_result = self.inner.borrow_mut().poll_request(request_id);
            match poll_result {
                Ok(None) => {
                    // Still pending - keep waiting
                }
                Ok(Some(result)) => {
                    // Got a result - invoke callback and remove it
                    if let Some(callback) = self.callbacks.borrow_mut().remove(&request_id) {
                        self.invoke_callback(&callback, result)?;
                    }
                }
                Err(e) => {
                    // Error - invoke callback with error and remove it
                    if let Some(callback) = self.callbacks.borrow_mut().remove(&request_id) {
                        let this = JsValue::NULL;
                        let error = JsValue::from_str(&e.to_string());
                        callback
                            .call2(&this, &JsValue::NULL, &error)
                            .map_err(|e| format!("Failed to invoke callback: {:?}", e))?;
                    }
                }
            }
        }

        Ok(did_work)
    }

    /// Helper to invoke callbacks with the appropriate result type
    fn invoke_callback(
        &self,
        callback: &js_sys::Function,
        result: quiver_environment::RequestResult,
    ) -> Result<(), String> {
        use quiver_environment::RequestResult;

        let this = JsValue::NULL;

        match result {
            RequestResult::Result(Ok((value, heap))) => {
                // Create result object with value and heap
                let result_obj = js_sys::Object::new();
                js_sys::Reflect::set(
                    &result_obj,
                    &"value".into(),
                    &serde_wasm_bindgen::to_value(&value)
                        .map_err(|e| format!("Failed to serialize value: {}", e))?,
                )
                .map_err(|e| format!("Failed to set value: {:?}", e))?;
                js_sys::Reflect::set(
                    &result_obj,
                    &"heap".into(),
                    &serde_wasm_bindgen::to_value(&heap)
                        .map_err(|e| format!("Failed to serialize heap: {}", e))?,
                )
                .map_err(|e| format!("Failed to set heap: {:?}", e))?;

                callback
                    .call2(&this, &result_obj, &JsValue::NULL)
                    .map_err(|e| format!("Failed to invoke callback: {:?}", e))?;
            }
            RequestResult::Result(Err(error)) => {
                let error_str = JsValue::from_str(&format!("{:?}", error));
                callback
                    .call2(&this, &JsValue::NULL, &error_str)
                    .map_err(|e| format!("Failed to invoke callback: {:?}", e))?;
            }
            RequestResult::Statuses(statuses) => {
                let statuses_value = serde_wasm_bindgen::to_value(&statuses)
                    .map_err(|e| format!("Failed to serialize statuses: {}", e))?;
                callback
                    .call2(&this, &statuses_value, &JsValue::NULL)
                    .map_err(|e| format!("Failed to invoke callback: {:?}", e))?;
            }
            RequestResult::ProcessInfo(info) => {
                let info_value = serde_wasm_bindgen::to_value(&info)
                    .map_err(|e| format!("Failed to serialize process info: {}", e))?;
                callback
                    .call2(&this, &info_value, &JsValue::NULL)
                    .map_err(|e| format!("Failed to invoke callback: {:?}", e))?;
            }
            RequestResult::Locals(locals) => {
                // Convert locals to array of {value, heap} objects
                let array = js_sys::Array::new();
                for (value, heap) in locals {
                    let obj = js_sys::Object::new();
                    js_sys::Reflect::set(
                        &obj,
                        &"value".into(),
                        &serde_wasm_bindgen::to_value(&value)
                            .map_err(|e| format!("Failed to serialize value: {}", e))?,
                    )
                    .map_err(|e| format!("Failed to set value: {:?}", e))?;
                    js_sys::Reflect::set(
                        &obj,
                        &"heap".into(),
                        &serde_wasm_bindgen::to_value(&heap)
                            .map_err(|e| format!("Failed to serialize heap: {}", e))?,
                    )
                    .map_err(|e| format!("Failed to set heap: {:?}", e))?;
                    array.push(&obj);
                }
                callback
                    .call2(&this, &array, &JsValue::NULL)
                    .map_err(|e| format!("Failed to invoke callback: {:?}", e))?;
            }
        }

        Ok(())
    }

    /// Format a value for display
    #[wasm_bindgen]
    pub fn format_value(&self, value: JsValue, heap: JsValue) -> Result<String, String> {
        let value: quiver_core::value::Value = serde_wasm_bindgen::from_value(value)
            .map_err(|e| format!("Failed to deserialize value: {}", e))?;
        let heap: Vec<Vec<u8>> = serde_wasm_bindgen::from_value(heap)
            .map_err(|e| format!("Failed to deserialize heap: {}", e))?;
        Ok(self.inner.borrow().format_value(&value, &heap))
    }

    /// Format a type for display
    #[wasm_bindgen]
    pub fn format_type(&self, ty: JsValue) -> Result<String, String> {
        let ty: quiver_core::types::Type = serde_wasm_bindgen::from_value(ty)
            .map_err(|e| format!("Failed to deserialize type: {}", e))?;
        Ok(self.inner.borrow().format_type(&ty))
    }
}

// Merge typed callback signatures with auto-generated class using interface merging
#[wasm_bindgen(typescript_custom_section)]
const TS_METHOD_SIGNATURES: &'static str = r#"
export interface Repl {
    /**
     * Evaluate an expression with a callback
     * @param source - The Quiver source code to evaluate
     * @param callback - Callback invoked when evaluation completes
     */
    evaluate(source: string, callback: EvaluateCallback): void;

    /**
     * Request a variable value by name with a callback
     * @param name - The variable name to retrieve
     * @param callback - Callback invoked when the variable value is available
     */
    get_variable(name: string, callback: VariableCallback): void;

    /**
     * Request all process statuses across all workers with a callback
     * @param callback - Callback invoked once with aggregated status from all workers
     */
    get_process_statuses(callback: StatusesCallback): void;

    /**
     * Request process info for a specific process with a callback
     * @param pid - The process ID to get information for
     * @param callback - Callback invoked when process info is available
     */
    get_process_info(pid: number, callback: ProcessInfoCallback): void;
}
"#;
