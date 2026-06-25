use crate::effects::WebEffect;
use crate::pump::{Pump, Tick};
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

    // The worker's event-driven loop, created once we receive the init message. Incoming commands
    // wake it; between commands (and with no runnable processes or pending timeout) it sleeps.
    let pump: Rc<RefCell<Option<Pump>>> = Rc::new(RefCell::new(None));
    let pump_for_closure = pump.clone();

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
                    *pump_for_closure.borrow_mut() = Some(start_worker_loop(worker));
                }
            } else {
                // Already initialized - parse as Command
                match serde_json::from_str::<Command<WebEffect>>(&text) {
                    Ok(cmd) => {
                        command_queue_clone.borrow_mut().push_back(cmd);
                        // Wake the loop so it processes the command; it may be sleeping.
                        if let Some(pump) = pump_for_closure.borrow().as_ref() {
                            pump.wake();
                        }
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

/// Drive the worker with an event-driven [`Pump`]. Each tick steps the worker once; the pump then
/// keeps stepping while there are runnable processes, schedules a single timer for the next pending
/// `select` timeout when idle-but-timing, and otherwise sleeps until a command wakes it. The
/// returned pump is stored by the caller so incoming commands can `wake()` it.
fn start_worker_loop(worker: Worker<WebEffect, WebCommandReceiver, WebEventSender>) -> Pump {
    let worker = Rc::new(RefCell::new(worker));

    let pump = Pump::new(move || {
        let current_time_ms = js_sys::Date::now() as u64;

        if worker.borrow_mut().step(current_time_ms).is_err() {
            // Worker has sent a WorkerError event to the environment; stop the loop.
            return Tick::Idle;
        }

        let worker = worker.borrow();
        if worker.has_runnable() {
            // More processes are queued; yield to the event loop, then step again immediately.
            Tick::Busy
        } else if let Some(deadline_ms) = worker.next_timeout_ms() {
            // Nothing runnable, but a select timeout is pending — wake exactly when it expires.
            Tick::WakeAt(deadline_ms as f64)
        } else {
            // Fully idle: wait for a command to arrive (which wakes the pump).
            Tick::Idle
        }
    });
    pump.start();
    pump
}
