use quiver_core::error::Error;
use quiver_core::program::Program;
use quiver_environment::runtime::{Event, Runtime, SchedulerCommand};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::{MessageEvent, Worker};

/// Bundles all the pieces needed for a single worker executor
struct WorkerHandle {
    worker: Option<Worker>,
    /// Message handler closure - must be kept alive for JavaScript
    _message_closure: Closure<dyn FnMut(MessageEvent)>,
    /// Error handler closure - must be kept alive for JavaScript
    _error_closure: Closure<dyn FnMut(web_sys::ErrorEvent)>,
}

/// Multi-worker web runtime
pub struct WebRuntime {
    workers: Vec<WorkerHandle>,
    pending_events: Rc<RefCell<VecDeque<Event>>>,
}

impl WebRuntime {
    pub fn new() -> Self {
        Self {
            workers: Vec::new(),
            pending_events: Rc::new(RefCell::new(VecDeque::new())),
        }
    }
}

impl Runtime for WebRuntime {
    fn send_command(&mut self, executor_id: usize, command: SchedulerCommand) -> Result<(), Error> {
        let handle = self.workers.get(executor_id).ok_or_else(|| {
            Error::InvalidArgument(format!("Invalid executor_id: {}", executor_id))
        })?;

        let worker = handle.worker.as_ref().ok_or_else(|| {
            Error::InvalidArgument(format!("Executor {} has been stopped", executor_id))
        })?;

        let command_data = serde_wasm_bindgen::to_value(&command)
            .map_err(|_| Error::InvalidArgument("Failed to serialize command".into()))?;

        let message = js_sys::Object::new();
        js_sys::Reflect::set(&message, &"type".into(), &"Command".into())
            .map_err(|_| Error::InvalidArgument("Failed to create message".into()))?;
        js_sys::Reflect::set(&message, &"command".into(), &command_data)
            .map_err(|_| Error::InvalidArgument("Failed to set command".into()))?;

        worker
            .post_message(&message)
            .map_err(|_| Error::InvalidArgument("postMessage failed".into()))?;

        Ok(())
    }

    fn start_executor(
        &mut self,
        program: &Program,
    ) -> Result<usize, Error> {
        let executor_id = self.workers.len();

        // Create worker
        let worker = Worker::new("./worker.js")
            .map_err(|_| Error::InvalidArgument("Failed to create worker".into()))?;

        // Set up message handler
        let pending = self.pending_events.clone();
        let message_closure = Closure::wrap(Box::new(move |e: MessageEvent| {
            // The worker sends back an array of events
            if let Ok(events) = serde_wasm_bindgen::from_value::<Vec<Event>>(e.data()) {
                pending.borrow_mut().extend(events);
            }
        }) as Box<dyn FnMut(_)>);

        worker.set_onmessage(Some(message_closure.as_ref().unchecked_ref()));

        // Set up error handler
        let error_closure = Closure::wrap(Box::new(move |e: web_sys::ErrorEvent| {
            web_sys::console::error_1(
                &format!("Worker {} error: {:?}", executor_id, e.message()).into(),
            );
        }) as Box<dyn FnMut(_)>);

        worker.set_onerror(Some(error_closure.as_ref().unchecked_ref()));

        // Send initial program to worker
        let bytecode = program.to_bytecode(None);
        let program_data = serde_wasm_bindgen::to_value(&bytecode)
            .map_err(|_| Error::InvalidArgument("Failed to serialize program".into()))?;

        let init_message = js_sys::Object::new();
        js_sys::Reflect::set(&init_message, &"type".into(), &"InitProgram".into())
            .map_err(|_| Error::InvalidArgument("Failed to create init message".into()))?;
        js_sys::Reflect::set(&init_message, &"program".into(), &program_data)
            .map_err(|_| Error::InvalidArgument("Failed to set program".into()))?;
        js_sys::Reflect::set(
            &init_message,
            &"workerId".into(),
            &JsValue::from(executor_id),
        )
        .map_err(|_| Error::InvalidArgument("Failed to set worker ID".into()))?;

        worker
            .post_message(&init_message)
            .map_err(|_| Error::InvalidArgument("Failed to send initial program".into()))?;

        self.workers.push(WorkerHandle {
            worker: Some(worker),
            _message_closure: message_closure,
            _error_closure: error_closure,
        });

        Ok(executor_id)
    }

    fn poll(&mut self) -> Vec<Event> {
        self.pending_events.borrow_mut().drain(..).collect()
    }

    fn stop_executor(&mut self, executor_id: usize) -> Result<(), Error> {
        let handle = self.workers.get_mut(executor_id).ok_or_else(|| {
            Error::InvalidArgument(format!("Invalid executor_id: {}", executor_id))
        })?;

        if let Some(worker) = handle.worker.take() {
            worker.terminate();
        }

        Ok(())
    }
}
