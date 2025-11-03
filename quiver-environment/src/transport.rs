use crate::environment::EnvironmentError;
use crate::messages::{Command, Event};
use quiver_core::effects::Effect;

/// Receive commands (Worker ← Environment)
pub trait CommandReceiver<E: Effect> {
    fn try_recv(&mut self) -> Result<Option<Command<E>>, EnvironmentError>;
}

/// Send events (Worker → Environment)
pub trait EventSender<E: Effect> {
    fn send(&mut self, event: Event<E>) -> Result<(), EnvironmentError>;
}

/// Handle for communicating with a worker (Environment side)
pub trait WorkerHandle<E: Effect> {
    fn send(&mut self, command: Command<E>) -> Result<(), EnvironmentError>;
    fn try_recv(&mut self) -> Result<Option<Event<E>>, EnvironmentError>;
}
