use crate::web_transport::{WebCommandReceiver, WebEventSender};
use quiver_environment::{Command, Event, Worker};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::{DedicatedWorkerGlobalScope, MessageEvent};

thread_local! {
    static WORKER_STATE: RefCell<Option<WorkerState>> = RefCell::new(None);
}

struct WorkerState {
    worker: Worker<WebCommandReceiver, WebEventSender>,
}

/// Main entry point for the worker
/// Call this from the worker's JS context
#[wasm_bindgen]
pub fn worker_main() {
    let global = js_sys::global()
        .dyn_into::<DedicatedWorkerGlobalScope>()
        .expect("Not in a worker context");

    // Create command queue
    let command_queue: Rc<RefCell<VecDeque<Command>>> = Rc::new(RefCell::new(VecDeque::new()));
    let command_queue_clone = command_queue.clone();

    // Create event sender that uses postMessage
    let global_clone = global.clone();
    let post_message_fn = Rc::new(move |event: Event| -> Result<(), String> {
        let json = serde_json::to_string(&event)
            .map_err(|e| format!("Failed to serialize event: {}", e))?;

        global_clone
            .post_message(&JsValue::from_str(&json))
            .map_err(|e| format!("Failed to post message: {:?}", e))?;

        Ok(())
    });

    let _event_sender = WebEventSender::new(post_message_fn);

    // Set up message handler
    let onmessage = Closure::wrap(Box::new(move |event: MessageEvent| {
        if let Some(text) = event.data().as_string() {
            // Try to parse as JSON first to check message type
            if let Ok(json) = serde_json::from_str::<serde_json::Value>(&text) {
                if let Some(msg_type) = json.get("type").and_then(|v| v.as_str()) {
                    if msg_type == "init" {
                        // Handle initialization
                        handle_init(&json, &command_queue_clone);
                        return;
                    }
                }
            }

            // Otherwise, try to parse as Command
            match serde_json::from_str::<Command>(&text) {
                Ok(cmd) => {
                    command_queue_clone.borrow_mut().push_back(cmd);
                }
                Err(e) => {
                    web_sys::console::error_1(&format!("Failed to parse command: {}", e).into());
                }
            }
        }
    }) as Box<dyn FnMut(MessageEvent)>);

    global.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
    onmessage.forget(); // Keep closure alive

    // Start the worker loop
    start_worker_loop();
}

fn handle_init(json: &serde_json::Value, command_queue: &Rc<RefCell<VecDeque<Command>>>) {
    let worker_id = json
        .get("worker_id")
        .and_then(|v| v.as_u64())
        .expect("Missing worker_id") as usize;

    web_sys::console::log_1(&format!("[Worker {}] Initializing", worker_id).into());

    // Create worker
    let cmd_receiver = WebCommandReceiver::new(command_queue.clone());

    let global = js_sys::global()
        .dyn_into::<DedicatedWorkerGlobalScope>()
        .unwrap();

    let post_message_fn = Rc::new(move |event: Event| -> Result<(), String> {
        let json = serde_json::to_string(&event)
            .map_err(|e| format!("Failed to serialize event: {}", e))?;

        global
            .post_message(&JsValue::from_str(&json))
            .map_err(|e| format!("Failed to post message: {:?}", e))?;

        Ok(())
    });

    let evt_sender = WebEventSender::new(post_message_fn);

    let worker = Worker::new(cmd_receiver, evt_sender);

    WORKER_STATE.with(|state| {
        *state.borrow_mut() = Some(WorkerState { worker });
    });

    web_sys::console::log_1(&format!("[Worker {}] Initialized successfully", worker_id).into());
}

fn start_worker_loop() {
    let closure = Rc::new(RefCell::new(None::<Closure<dyn FnMut()>>));
    let closure_clone = closure.clone();

    *closure.borrow_mut() = Some(Closure::wrap(Box::new(move || {
        // Step the worker if initialized
        let should_continue = WORKER_STATE.with(|state| {
            if let Some(worker_state) = state.borrow_mut().as_mut() {
                match worker_state.worker.step() {
                    Ok(_work_done) => true,
                    Err(e) => {
                        web_sys::console::error_1(&format!("Worker error: {}", e).into());
                        false
                    }
                }
            } else {
                // Not initialized yet, keep looping
                true
            }
        });

        if should_continue {
            // Schedule next step
            let window = js_sys::global();
            let set_timeout = js_sys::Reflect::get(&window, &JsValue::from_str("setTimeout"))
                .expect("setTimeout not found");
            let set_timeout: js_sys::Function = set_timeout.dyn_into().unwrap();

            let closure_ref = closure_clone.borrow();
            let callback = closure_ref.as_ref().unwrap();

            let _ = set_timeout.call2(
                &window,
                callback.as_ref().unchecked_ref(),
                &JsValue::from_f64(0.0),
            );
        }
    }) as Box<dyn FnMut()>));

    // Start the loop
    let closure_ref = closure.borrow();
    let callback = closure_ref.as_ref().unwrap();
    callback
        .as_ref()
        .unchecked_ref::<js_sys::Function>()
        .call0(&JsValue::NULL)
        .unwrap();
}
