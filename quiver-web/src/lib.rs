mod web_transport;
mod worker_entry;

use quiver_compiler::InMemoryModuleLoader;
use quiver_core::program::Program;
use quiver_environment::{Environment as CoreEnvironment, WorkerHandle};
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use web_sys::{MessageEvent, Worker, WorkerOptions, WorkerType};

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

/// WASM-bindgen wrapper for Environment
#[wasm_bindgen]
pub struct Environment {
    inner: CoreEnvironment,
}

#[wasm_bindgen]
impl Environment {
    /// Create a new environment with the specified number of workers
    #[wasm_bindgen(constructor)]
    pub fn new(num_workers: usize, worker_url: &str) -> Result<Environment, String> {
        // Create workers
        let mut workers: Vec<Box<dyn WorkerHandle>> = Vec::new();

        for worker_id in 0..num_workers {
            // Create worker options to specify module type
            let worker_options = WorkerOptions::new();
            worker_options.set_type(WorkerType::Module);

            let worker = Worker::new_with_options(worker_url, &worker_options)
                .map_err(|e| format!("Failed to create worker: {:?}", e))?;

            // Create event queue for this worker
            let event_queue: Rc<RefCell<VecDeque<quiver_environment::Event>>> =
                Rc::new(RefCell::new(VecDeque::new()));
            let event_queue_clone = event_queue.clone();

            // Prepare initialization message to send when worker is ready
            let init_msg = serde_json::json!({
                "type": "init",
                "worker_id": worker_id,
            });
            let init_msg_str = init_msg.to_string();

            let worker_clone = worker.clone();

            // Set up message handler
            let onmessage = Closure::wrap(Box::new(move |event: MessageEvent| {
                if let Some(text) = event.data().as_string() {
                    // Check for ready signal
                    if text == "ready" {
                        let _ = worker_clone.post_message(&JsValue::from_str(&init_msg_str));
                        return;
                    }

                    // Otherwise parse as Event
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

            let handle = web_transport::WebWorkerHandle::new(worker, event_queue);
            workers.push(Box::new(handle));
        }

        let program = Program::new();
        let inner = CoreEnvironment::new(workers, &program).map_err(|e| e.to_string())?;

        Ok(Environment { inner })
    }

    /// Process events and step the environment
    /// Returns true if work was done
    #[wasm_bindgen]
    pub fn step(&mut self) -> Result<bool, String> {
        self.inner.step().map_err(|e| e.to_string())
    }

    /// Start a new process
    #[wasm_bindgen]
    pub fn start_process(
        &mut self,
        function_index: usize,
        persistent: bool,
    ) -> Result<usize, String> {
        let pid = self
            .inner
            .start_process(function_index, persistent)
            .map_err(|e| e.to_string())?;
        Ok(pid)
    }

    /// Request a process result
    #[wasm_bindgen]
    pub fn request_result(&mut self, pid: usize) -> Result<u64, String> {
        self.inner.request_result(pid).map_err(|e| e.to_string())
    }

    /// Poll for a request result
    /// Returns result value if ready, or null if not ready yet
    #[wasm_bindgen]
    pub fn poll_request(&mut self, request_id: u64) -> JsValue {
        match self.inner.poll_request(request_id) {
            Some(result) => match serde_wasm_bindgen::to_value(&result) {
                Ok(js_value) => js_value,
                Err(e) => {
                    web_sys::console::error_1(&format!("Failed to serialize result: {}", e).into());
                    JsValue::NULL
                }
            },
            None => JsValue::NULL,
        }
    }

    /// Wait for a request to complete (blocking poll loop)
    #[wasm_bindgen]
    pub fn wait_for_request(&mut self, request_id: u64) -> Result<JsValue, String> {
        let result = self
            .inner
            .wait_for_request(request_id)
            .map_err(|e| e.to_string())?;
        serde_wasm_bindgen::to_value(&result)
            .map_err(|e| format!("Failed to serialize result: {}", e))
    }
}

/// WASM-bindgen wrapper for Repl
#[wasm_bindgen]
pub struct Repl {
    inner: quiver_environment::Repl,
    pending_requests:
        Rc<RefCell<std::collections::HashMap<u64, quiver_environment::EvaluateRequest>>>,
    next_request_id: Rc<RefCell<u64>>,
}

#[wasm_bindgen]
impl Repl {
    /// Create a new REPL with the specified number of workers
    #[wasm_bindgen(constructor)]
    pub fn new(num_workers: usize, worker_url: &str) -> Result<Repl, String> {
        // Create workers
        let mut workers: Vec<Box<dyn WorkerHandle>> = Vec::new();

        for worker_id in 0..num_workers {
            // Create worker options to specify module type
            let worker_options = WorkerOptions::new();
            worker_options.set_type(WorkerType::Module);

            let worker = Worker::new_with_options(worker_url, &worker_options)
                .map_err(|e| format!("Failed to create worker: {:?}", e))?;

            // Create event queue for this worker
            let event_queue: Rc<RefCell<VecDeque<quiver_environment::Event>>> =
                Rc::new(RefCell::new(VecDeque::new()));
            let event_queue_clone = event_queue.clone();

            // Prepare initialization message to send when worker is ready
            let init_msg = serde_json::json!({
                "type": "init",
                "worker_id": worker_id,
            });
            let init_msg_str = init_msg.to_string();

            let worker_clone = worker.clone();

            // Set up message handler
            let onmessage = Closure::wrap(Box::new(move |event: MessageEvent| {
                if let Some(text) = event.data().as_string() {
                    // Check for ready signal
                    if text == "ready" {
                        let _ = worker_clone.post_message(&JsValue::from_str(&init_msg_str));
                        return;
                    }

                    // Otherwise parse as Event
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

            let handle = web_transport::WebWorkerHandle::new(worker, event_queue);
            workers.push(Box::new(handle));
        }

        // Create module loader with embedded standard library
        let module_loader = Box::new(create_stdlib_loader());

        let program = Program::new();
        let inner = quiver_environment::Repl::new(workers, program, module_loader)
            .map_err(|e| e.to_string())?;

        Ok(Repl {
            inner,
            pending_requests: Rc::new(RefCell::new(std::collections::HashMap::new())),
            next_request_id: Rc::new(RefCell::new(0)),
        })
    }

    /// Evaluate an expression and return a request ID
    #[wasm_bindgen]
    pub fn evaluate(&mut self, source: &str) -> Result<u64, String> {
        let request = self
            .inner
            .evaluate(source)
            .map_err(|e| format!("{:?}", e))?;

        // Generate a new request ID
        let request_id = {
            let mut next_id = self.next_request_id.borrow_mut();
            let id = *next_id;
            *next_id += 1;
            id
        };

        // Store the request
        self.pending_requests
            .borrow_mut()
            .insert(request_id, request);

        Ok(request_id)
    }

    /// Poll for evaluation result (non-blocking)
    /// Returns value if ready, or null if not ready yet
    #[wasm_bindgen]
    pub fn poll_evaluate(&mut self, request_id: u64) -> JsValue {
        // We need mutable access to update the request's result_request_id
        let mut requests = self.pending_requests.borrow_mut();
        let request = match requests.get_mut(&request_id) {
            Some(r) => r,
            None => return JsValue::NULL,
        };

        match self.inner.poll_evaluate(request) {
            Some(Ok(value)) => {
                // Remove the completed request
                requests.remove(&request_id);
                drop(requests); // Drop the borrow before serializing

                // Serialize the value
                match serde_wasm_bindgen::to_value(&value) {
                    Ok(js_value) => js_value,
                    Err(e) => {
                        web_sys::console::error_1(
                            &format!("Failed to serialize value: {}", e).into(),
                        );
                        JsValue::NULL
                    }
                }
            }
            Some(Err(e)) => {
                // Remove the failed request
                requests.remove(&request_id);
                drop(requests);

                // Return error object
                let error = serde_json::json!({"error": e.to_string()});
                serde_wasm_bindgen::to_value(&error).unwrap_or(JsValue::NULL)
            }
            None => JsValue::NULL,
        }
    }

    /// Wait for evaluation result (blocking)
    #[wasm_bindgen]
    pub fn wait_evaluate(&mut self, request_id: u64) -> Result<JsValue, String> {
        let request = self
            .pending_requests
            .borrow_mut()
            .remove(&request_id)
            .ok_or_else(|| format!("Request {} not found", request_id))?;

        let value = self
            .inner
            .wait_evaluate(request)
            .map_err(|e| e.to_string())?;

        serde_wasm_bindgen::to_value(&value)
            .map_err(|e| format!("Failed to serialize value: {}", e))
    }

    /// Get variable value by name
    #[wasm_bindgen]
    pub fn get_variable(&mut self, name: &str) -> Result<JsValue, String> {
        let value = self.inner.get_variable(name).map_err(|e| e.to_string())?;
        serde_wasm_bindgen::to_value(&value)
            .map_err(|e| format!("Failed to serialize value: {}", e))
    }

    /// Get all variable names and their types
    #[wasm_bindgen]
    pub fn get_variables(&self) -> JsValue {
        let variables = self.inner.get_variables();
        match serde_wasm_bindgen::to_value(&variables) {
            Ok(js_value) => js_value,
            Err(e) => {
                web_sys::console::error_1(&format!("Failed to serialize variables: {}", e).into());
                JsValue::from(js_sys::Array::new())
            }
        }
    }

    /// Step the environment (process events)
    #[wasm_bindgen]
    pub fn step(&mut self) -> Result<bool, String> {
        self.inner.step().map_err(|e| e.to_string())
    }

    /// Format a value for display
    #[wasm_bindgen]
    pub fn format_value(&self, value: JsValue, heap: JsValue) -> Result<String, String> {
        let value: quiver_core::value::Value = serde_wasm_bindgen::from_value(value)
            .map_err(|e| format!("Failed to deserialize value: {}", e))?;
        let heap: Vec<Vec<u8>> = serde_wasm_bindgen::from_value(heap)
            .map_err(|e| format!("Failed to deserialize heap: {}", e))?;
        Ok(self.inner.format_value(&value, &heap))
    }

    /// Format a type for display
    #[wasm_bindgen]
    pub fn format_type(&self, ty: JsValue) -> Result<String, String> {
        let ty: quiver_core::types::Type = serde_wasm_bindgen::from_value(ty)
            .map_err(|e| format!("Failed to deserialize type: {}", e))?;
        Ok(self.inner.format_type(&ty))
    }
}

// Re-export worker entry point
pub use worker_entry::worker_main;
