mod environment;
mod messages;
mod repl;
mod transport;
mod worker;

pub use environment::{Environment, EnvironmentError, RequestResult};
pub use messages::{Command, Event};
pub use repl::{Repl, ReplError};
pub use transport::{CommandReceiver, EventSender, WorkerHandle};
pub use worker::Worker;

pub type WorkerId = usize;

// Re-export Effect trait for convenience
pub use quiver_core::effects::Effect;
