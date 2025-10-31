pub mod binary;
pub mod builtins;
pub mod bytecode;
pub mod error;
pub mod execute;
pub mod executor;
pub mod format;
pub mod process;
pub mod program;
pub mod types;
pub mod value;

pub use binary::{BinaryData, MAX_BINARY_SIZE};
pub use error::Error;
pub use execute::execute_instructions_sync;
pub use executor::Executor;
pub use process::{Action, ProcessId, ProcessInfo, ProcessStatus};
pub use value::{Binary, Value};
