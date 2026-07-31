use crate::environment::EnvironmentError;
use crate::messages::{Command, Event};
use quiver_core::effects::Effect;

/// Receive commands (Worker ← Environment)
pub trait CommandReceiver<E: Effect> {
    fn try_recv(&mut self) -> Result<Option<Command<E>>, EnvironmentError>;

    /// Block until a command may be available, or until `timeout` elapses (`None` = no
    /// deadline). Called only when the worker has nothing runnable, so it must not busy-wait.
    ///
    /// The alternative — sleeping a fixed interval and polling — puts that interval on the
    /// latency of *every* routed message, because a worker with nothing to do is exactly a
    /// worker about to be handed work. A `timeout` is still needed because not every wake is
    /// a command: a pending select deadline must fire on time.
    ///
    /// Default is a no-op, for hosts that cannot block (a WASM worker is driven by its event
    /// loop and never runs this loop at all). A no-op is safe but *spins* if a host does run
    /// an idle loop, so implement it wherever blocking is possible.
    fn wait(&mut self, _timeout: Option<std::time::Duration>) {}
}

/// Send events (Worker → Environment)
pub trait EventSender<E: Effect> {
    fn send(&mut self, event: Event<E>) -> Result<(), EnvironmentError>;
}

/// Handle for communicating with a worker (Environment side)
pub trait WorkerHandle<E: Effect>: Send {
    fn send(&mut self, command: Command<E>) -> Result<(), EnvironmentError>;
    fn try_recv(&mut self) -> Result<Option<Event<E>>, EnvironmentError>;
}
