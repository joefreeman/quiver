use quiver_environment::{
    Command, CommandReceiver, EnvironmentError, Event, EventSender, Worker, WorkerHandle,
};
use quiver_io::NativeEffect;
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::thread::{self, JoinHandle};

/// Stack size for worker threads (256 MiB). Lazily committed by the OS, so this large
/// reservation is virtually free unless deep recursion actually uses it.
const WORKER_STACK_SIZE: usize = 256 * 1024 * 1024;

/// Command receiver using mpsc::Receiver
pub struct NativeCommandReceiver {
    receiver: Receiver<Command<NativeEffect>>,
}

impl CommandReceiver<NativeEffect> for NativeCommandReceiver {
    fn try_recv(&mut self) -> Result<Option<Command<NativeEffect>>, EnvironmentError> {
        match self.receiver.try_recv() {
            Ok(cmd) => Ok(Some(cmd)),
            Err(TryRecvError::Empty) => Ok(None),
            Err(TryRecvError::Disconnected) => Err(EnvironmentError::ChannelDisconnected),
        }
    }
}

/// Event sender using mpsc::Sender
pub struct NativeEventSender {
    sender: Sender<Event<NativeEffect>>,
}

impl EventSender<NativeEffect> for NativeEventSender {
    fn send(&mut self, event: Event<NativeEffect>) -> Result<(), EnvironmentError> {
        self.sender.send(event).map_err(|e| {
            EnvironmentError::WorkerCommunication(format!("Failed to send event: {}", e))
        })
    }
}

/// Worker handle for native implementation
pub struct NativeWorkerHandle {
    cmd_sender: Sender<Command<NativeEffect>>,
    evt_receiver: Receiver<Event<NativeEffect>>,
    _thread_handle: JoinHandle<()>,
}

impl WorkerHandle<NativeEffect> for NativeWorkerHandle {
    fn send(&mut self, command: Command<NativeEffect>) -> Result<(), EnvironmentError> {
        self.cmd_sender.send(command).map_err(|e| {
            EnvironmentError::WorkerCommunication(format!("Failed to send command: {}", e))
        })
    }

    fn try_recv(&mut self) -> Result<Option<Event<NativeEffect>>, EnvironmentError> {
        match self.evt_receiver.try_recv() {
            Ok(event) => Ok(Some(event)),
            Err(TryRecvError::Empty) => Ok(None),
            Err(TryRecvError::Disconnected) => Err(EnvironmentError::ChannelDisconnected),
        }
    }
}

/// Spawn a native worker thread with a custom time function
pub fn spawn_worker<F>(
    time_fn: F,
    builtins: quiver_core::builtins::BuiltinRegistry<NativeEffect>,
    profile: bool,
    worker_id: u16,
) -> NativeWorkerHandle
where
    F: Fn() -> u64 + Send + 'static,
{
    let (cmd_tx, cmd_rx) = mpsc::channel();
    let (evt_tx, evt_rx) = mpsc::channel();

    // Worker threads get a large stack. Many value traversals (drop, formatting, equality,
    // module codegen) recurse on the depth of nested tuple values, so a deep data structure
    // (e.g. a long Cons list) needs proportional stack. The stack is lazily committed, so a
    // generous reservation costs nothing unless actually used.
    let thread_handle = thread::Builder::new()
        .name(format!("quiver-worker-{worker_id}"))
        .stack_size(WORKER_STACK_SIZE)
        .spawn(move || {
            let cmd_receiver = NativeCommandReceiver { receiver: cmd_rx };

            // Clone the sender so we can use it for error reporting
            let error_sender = evt_tx.clone();
            let evt_sender = NativeEventSender { sender: evt_tx };

            let mut worker = Worker::<NativeEffect, _, _>::new(
                cmd_receiver,
                evt_sender,
                builtins,
                profile,
                worker_id,
            );

            // Run the worker loop
            loop {
                // Get current time from the provided function
                let current_time_ms = time_fn();

                match worker.step(current_time_ms) {
                    Ok(true) => {
                        // Work was done, continue immediately
                    }
                    Ok(false) => {
                        // No work - sleep to save CPU
                        std::thread::sleep(std::time::Duration::from_millis(5));
                    }
                    Err(e) => {
                        // Send error event to environment
                        let _ = error_sender.send(Event::WorkerError { error: e });
                        break;
                    }
                }
            }
        })
        .expect("failed to spawn worker thread");

    NativeWorkerHandle {
        cmd_sender: cmd_tx,
        evt_receiver: evt_rx,
        _thread_handle: thread_handle,
    }
}
