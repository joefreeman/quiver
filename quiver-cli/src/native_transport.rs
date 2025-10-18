use quiver_environment::{Command, CommandReceiver, Event, EventSender, Worker, WorkerHandle};
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::thread::{self, JoinHandle};

/// Command receiver using mpsc::Receiver
pub struct NativeCommandReceiver {
    receiver: Receiver<Command>,
}

impl CommandReceiver for NativeCommandReceiver {
    fn try_recv(&mut self) -> Result<Option<Command>, String> {
        match self.receiver.try_recv() {
            Ok(cmd) => Ok(Some(cmd)),
            Err(TryRecvError::Empty) => Ok(None),
            Err(TryRecvError::Disconnected) => Err("Command channel disconnected".to_string()),
        }
    }
}

/// Event sender using mpsc::Sender
pub struct NativeEventSender {
    sender: Sender<Event>,
}

impl EventSender for NativeEventSender {
    fn send(&mut self, event: Event) -> Result<(), String> {
        self.sender
            .send(event)
            .map_err(|e| format!("Failed to send event: {}", e))
    }
}

/// Worker handle for native implementation
pub struct NativeWorkerHandle {
    cmd_sender: Sender<Command>,
    evt_receiver: Receiver<Event>,
    _thread_handle: JoinHandle<()>,
}

impl WorkerHandle for NativeWorkerHandle {
    fn send(&mut self, command: Command) -> Result<(), String> {
        self.cmd_sender
            .send(command)
            .map_err(|e| format!("Failed to send command: {}", e))
    }

    fn try_recv(&mut self) -> Result<Option<Event>, String> {
        match self.evt_receiver.try_recv() {
            Ok(event) => Ok(Some(event)),
            Err(TryRecvError::Empty) => Ok(None),
            Err(TryRecvError::Disconnected) => Err("Event channel disconnected".to_string()),
        }
    }
}

/// Spawn a native worker thread
pub fn spawn_worker() -> NativeWorkerHandle {
    let (cmd_tx, cmd_rx) = mpsc::channel();
    let (evt_tx, evt_rx) = mpsc::channel();

    let thread_handle = thread::spawn(move || {
        let cmd_receiver = NativeCommandReceiver { receiver: cmd_rx };
        let evt_sender = NativeEventSender { sender: evt_tx };

        let mut worker = Worker::new(cmd_receiver, evt_sender);

        // Run the worker loop
        loop {
            match worker.step() {
                Ok(true) => {
                    // Work was done, continue immediately
                }
                Ok(false) => {
                    // No work, sleep briefly to avoid spinning
                    std::thread::sleep(std::time::Duration::from_micros(100));
                }
                Err(e) => {
                    eprintln!("Worker error: {}", e);
                    break;
                }
            }
        }
    });

    NativeWorkerHandle {
        cmd_sender: cmd_tx,
        evt_receiver: evt_rx,
        _thread_handle: thread_handle,
    }
}
