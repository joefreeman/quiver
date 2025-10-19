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
    inner: quiver_environment::Repl,
    pending_requests:
        Rc<RefCell<std::collections::HashMap<u64, quiver_environment::EvaluateRequest>>>,
    next_request_id: Rc<RefCell<u64>>,
}

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

            // Set up message handler
            let onmessage = Closure::wrap(Box::new(move |event: MessageEvent| {
                if let Some(text) = event.data().as_string() {
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
    /// Returns [value, heap] if ready, null if not ready yet
    /// Throws error if evaluation failed
    #[wasm_bindgen]
    pub fn poll_evaluate(&mut self, request_id: u64) -> Result<JsValue, String> {
        // We need mutable access to update the request's result_request_id
        let mut requests = self.pending_requests.borrow_mut();
        let request = match requests.get_mut(&request_id) {
            Some(r) => r,
            None => return Ok(JsValue::NULL),
        };

        match self.inner.poll_evaluate(request) {
            Some(Ok(value)) => {
                // Remove the completed request
                requests.remove(&request_id);
                drop(requests); // Drop the borrow before serializing

                // Serialize and return the value as [value, heap]
                let js_value = serde_wasm_bindgen::to_value(&value)
                    .map_err(|e| format!("Failed to serialize value: {}", e))?;

                Ok(js_value)
            }
            Some(Err(e)) => {
                // Remove the failed request and throw error
                requests.remove(&request_id);
                drop(requests);

                Err(e.to_string())
            }
            None => Ok(JsValue::NULL),
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
    /// Throws exception on serialization failure
    #[wasm_bindgen]
    pub fn get_variables(&self) -> Result<JsValue, String> {
        let variables = self.inner.get_variables();
        serde_wasm_bindgen::to_value(&variables)
            .map_err(|e| format!("Failed to serialize variables: {}", e))
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
