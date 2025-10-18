use crate::environment::EnvironmentError;
use crate::messages::{Command, Event};

/// Receive commands (Worker ← Environment)
pub trait CommandReceiver {
    fn try_recv(&mut self) -> Result<Option<Command>, EnvironmentError>;
}

/// Send events (Worker → Environment)
pub trait EventSender {
    fn send(&mut self, event: Event) -> Result<(), EnvironmentError>;
}

/// Handle for communicating with a worker (Environment side)
pub trait WorkerHandle {
    fn send(&mut self, command: Command) -> Result<(), EnvironmentError>;
    fn try_recv(&mut self) -> Result<Option<Event>, EnvironmentError>;
}
