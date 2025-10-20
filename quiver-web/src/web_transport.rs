use quiver_environment::{
    Command, CommandReceiver, EnvironmentError, Event, EventSender, WorkerHandle,
};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use web_sys::Worker;

/// Command receiver for Web Worker (worker side)
pub struct WebCommandReceiver {
    queue: Rc<RefCell<VecDeque<Command>>>,
}

impl WebCommandReceiver {
    pub fn new(queue: Rc<RefCell<VecDeque<Command>>>) -> Self {
        Self { queue }
    }
}

impl CommandReceiver for WebCommandReceiver {
    fn try_recv(&mut self) -> Result<Option<Command>, EnvironmentError> {
        Ok(self.queue.borrow_mut().pop_front())
    }
}

/// Event sender for Web Worker (worker side)
pub struct WebEventSender {
    post_message: Rc<dyn Fn(Event) -> Result<(), EnvironmentError>>,
}

impl WebEventSender {
    pub fn new(post_message: Rc<dyn Fn(Event) -> Result<(), EnvironmentError>>) -> Self {
        Self { post_message }
    }
}

impl EventSender for WebEventSender {
    fn send(&mut self, event: Event) -> Result<(), EnvironmentError> {
        (self.post_message)(event)
    }
}

/// Worker handle for web (main thread side)
pub struct WebWorkerHandle {
    worker: Worker,
    event_queue: Rc<RefCell<VecDeque<Event>>>,
    ready: Rc<RefCell<bool>>,
    pending_commands: Rc<RefCell<VecDeque<Command>>>,
}

impl WebWorkerHandle {
    pub fn new(worker: Worker, event_queue: Rc<RefCell<VecDeque<Event>>>) -> Self {
        Self {
            worker,
            event_queue,
            ready: Rc::new(RefCell::new(false)),
            pending_commands: Rc::new(RefCell::new(VecDeque::new())),
        }
    }

    /// Get a handle to the ready flag
    pub fn ready(&self) -> Rc<RefCell<bool>> {
        self.ready.clone()
    }

    /// Get a handle to the pending commands queue
    pub fn pending_commands(&self) -> Rc<RefCell<VecDeque<Command>>> {
        self.pending_commands.clone()
    }
}

impl WorkerHandle for WebWorkerHandle {
    fn send(&mut self, command: Command) -> Result<(), EnvironmentError> {
        if *self.ready.borrow() {
            // Worker is ready - send immediately
            let json = serde_json::to_string(&command).map_err(|e| {
                EnvironmentError::WorkerCommunication(format!("Failed to serialize command: {}", e))
            })?;

            self.worker
                .post_message(&JsValue::from_str(&json))
                .map_err(|e| {
                    EnvironmentError::WorkerCommunication(format!(
                        "Failed to post message: {:?}",
                        e
                    ))
                })?;
        } else {
            // Worker not ready yet - queue the command
            self.pending_commands.borrow_mut().push_back(command);
        }

        Ok(())
    }

    fn try_recv(&mut self) -> Result<Option<Event>, EnvironmentError> {
        Ok(self.event_queue.borrow_mut().pop_front())
    }
}
