use quiver_environment::{Command, CommandReceiver, Event, EventSender, WorkerHandle};
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
    fn try_recv(&mut self) -> Result<Option<Command>, String> {
        Ok(self.queue.borrow_mut().pop_front())
    }
}

/// Event sender for Web Worker (worker side)
pub struct WebEventSender {
    post_message: Rc<dyn Fn(Event) -> Result<(), String>>,
}

impl WebEventSender {
    pub fn new(post_message: Rc<dyn Fn(Event) -> Result<(), String>>) -> Self {
        Self { post_message }
    }
}

impl EventSender for WebEventSender {
    fn send(&mut self, event: Event) -> Result<(), String> {
        (self.post_message)(event)
    }
}

/// Worker handle for web (main thread side)
pub struct WebWorkerHandle {
    worker: Worker,
    event_queue: Rc<RefCell<VecDeque<Event>>>,
}

impl WebWorkerHandle {
    pub fn new(worker: Worker, event_queue: Rc<RefCell<VecDeque<Event>>>) -> Self {
        Self {
            worker,
            event_queue,
        }
    }
}

impl WorkerHandle for WebWorkerHandle {
    fn send(&mut self, command: Command) -> Result<(), String> {
        // Serialize command to JSON
        let json = serde_json::to_string(&command)
            .map_err(|e| format!("Failed to serialize command: {}", e))?;

        // Send via postMessage
        self.worker
            .post_message(&JsValue::from_str(&json))
            .map_err(|e| format!("Failed to post message: {:?}", e))?;

        Ok(())
    }

    fn try_recv(&mut self) -> Result<Option<Event>, String> {
        Ok(self.event_queue.borrow_mut().pop_front())
    }
}
