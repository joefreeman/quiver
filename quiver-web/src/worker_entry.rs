use crate::effects::WebEffect;
use crate::web_transport::{WebCommandReceiver, WebEventSender};
use quiver_environment::{Command, EnvironmentError, Event, Worker};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::{DedicatedWorkerGlobalScope, MessageEvent};

/// Main entry point for the worker
/// Call this from the worker's JS context
#[wasm_bindgen]
pub fn worker_main() {
    let global = js_sys::global()
        .dyn_into::<DedicatedWorkerGlobalScope>()
        .expect("Not in a worker context");

    // Create command queue
    let command_queue: Rc<RefCell<VecDeque<Command<WebEffect>>>> =
        Rc::new(RefCell::new(VecDeque::new()));
    let command_queue_clone = command_queue.clone();

    // Create event sender that uses postMessage
    let global_clone = global.clone();
    let post_message_fn = Rc::new(
        move |event: Event<WebEffect>| -> Result<(), EnvironmentError> {
            let json = serde_json::to_string(&event).map_err(|e| {
                EnvironmentError::WorkerCommunication(format!("Failed to serialize event: {}", e))
            })?;

            global_clone
                .post_message(&JsValue::from_str(&json))
                .map_err(|e| {
                    EnvironmentError::WorkerCommunication(format!(
                        "Failed to post message: {:?}",
                        e
                    ))
                })?;

            Ok(())
        },
    );

    let evt_sender = WebEventSender::new(post_message_fn.clone());

    // Signal that worker is ready to receive init message
    global
        .post_message(&JsValue::from_str("ready"))
        .expect("Failed to send ready signal");

    // State for initialization
    let initialized = Rc::new(RefCell::new(false));
    let initialized_clone = initialized.clone();
    let evt_sender_clone = Rc::new(RefCell::new(Some(evt_sender)));
    let evt_sender_for_closure = evt_sender_clone.clone();
    let command_queue_for_init = command_queue.clone();

    // Set up message handler - handles both init and commands
    let onmessage = Closure::wrap(Box::new(move |event: MessageEvent| {
        if let Some(text) = event.data().as_string() {
            if !*initialized_clone.borrow() {
                // Waiting for init message: "init:N" where N is worker_id
                if let Some(id_str) = text.strip_prefix("init:") {
                    let worker_id: u16 = id_str.parse().unwrap_or(0);

                    // Create and initialize worker now that we have the ID
                    let cmd_receiver = WebCommandReceiver::new(command_queue_for_init.clone());
                    let builtins = quiver_core::builtins::BuiltinRegistry::with_modules(
                        &quiver_core::builtins::core_modules(),
                    );
                    let evt_sender = evt_sender_for_closure.borrow_mut().take().unwrap();
                    let worker = Worker::new(cmd_receiver, evt_sender, builtins, false, worker_id);

                    *initialized_clone.borrow_mut() = true;

                    // Start the worker loop
                    start_worker_loop(worker);
                }
            } else {
                // Already initialized - parse as Command
                match serde_json::from_str::<Command<WebEffect>>(&text) {
                    Ok(cmd) => {
                        command_queue_clone.borrow_mut().push_back(cmd);
                    }
                    Err(e) => {
                        web_sys::console::error_1(
                            &format!("Failed to parse command: {}", e).into(),
                        );
                    }
                }
            }
        }
    }) as Box<dyn FnMut(MessageEvent)>);

    global.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
    onmessage.forget(); // Keep closure alive
}

fn start_worker_loop(worker: Worker<WebEffect, WebCommandReceiver, WebEventSender>) {
    let worker = Rc::new(RefCell::new(worker));
    let worker_clone = worker.clone();

    let closure = Rc::new(RefCell::new(None::<Closure<dyn FnMut()>>));
    let closure_clone = closure.clone();

    *closure.borrow_mut() = Some(Closure::wrap(Box::new(move || {
        // Get current time from JavaScript
        let current_time_ms = js_sys::Date::now() as u64;

        // Step the worker
        let should_continue = match worker_clone.borrow_mut().step(current_time_ms) {
            Ok(_work_done) => true,
            Err(_e) => {
                // Worker will send WorkerError event to environment
                false
            }
        };

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
