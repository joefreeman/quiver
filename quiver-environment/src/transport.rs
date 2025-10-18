use crate::messages::{Command, Event};

/// Receive commands (Worker ← Environment)
pub trait CommandReceiver {
    fn try_recv(&mut self) -> Result<Option<Command>, String>;
}

/// Send events (Worker → Environment)
pub trait EventSender {
    fn send(&mut self, event: Event) -> Result<(), String>;
}

/// Handle for communicating with a worker (Environment side)
pub trait WorkerHandle {
    fn send(&mut self, command: Command) -> Result<(), String>;
    fn try_recv(&mut self) -> Result<Option<Event>, String>;
}
