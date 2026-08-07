use crate::process::RestrictedContext;
use serde::{Deserialize, Serialize};
use std::fmt;

/// An operation that can be rejected — in a [`RestrictedContext`] at runtime, or
/// during compile-time execution (which routes no actions).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Operation {
    Spawn,
    Send,
    Select,
    Await,
    Effect,
    ReadState,
    Kill,
    Link,
    Detach,
    Track,
    /// A host-state read (clock, entropy) — a `Purity::HostRead` builtin.
    HostRead,
    /// Minting a ref (`%ref`) — currently the only `Purity::Stateful` builtin; revisit
    /// the label if another appears.
    CreateRef,
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Operation::Spawn => "spawn",
            Operation::Send => "send",
            Operation::Select => "select",
            Operation::Await => "await",
            Operation::Effect => "effect",
            Operation::ReadState => "state read",
            Operation::Kill => "kill",
            Operation::Link => "link",
            Operation::Detach => "detach",
            Operation::Track => "track",
            Operation::HostRead => "host read",
            Operation::CreateRef => "ref creation",
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Error {
    // Stack operation errors
    StackUnderflow,

    // Function and call errors
    CallInvalid,
    FunctionUndefined(usize),
    BuiltinUndefined(usize),
    FrameUnderflow,

    // Variable and constant access errors
    VariableUndefined(String),
    ConstantUndefined(usize),

    // Data access errors
    FieldAccessInvalid(usize),

    // Type system errors
    TypeMismatch {
        expected: String,
        found: String,
    },
    ArityMismatch {
        expected: usize,
        found: usize,
    },
    InvalidArgument(String),

    // Tuple and structure errors
    TupleEmpty,

    // Operation restrictions
    OperationNotAllowed {
        operation: Operation,
        context: RestrictedContext,
    },

    // `%proc.detach` of a process the caller doesn't own — ownership is the parent's
    // to relinquish.
    NotAnOwnedChild,

    // Compile-time execution (a program's top level and module bodies) routes no
    // actions, so process and effect work there is rejected at its source.
    UnsupportedAtCompileTime {
        operation: Operation,
    },

    // The compile-time backstop: a receive/select waiting on a message that can never
    // arrive (no sender can exist, and time is frozen so timeouts never expire).
    StalledAtCompileTime,

    // Compile-time execution ran past its step budget — the bound that turns an
    // infinite loop at a module's top level into an error instead of a hang.
    ExhaustedAtCompileTime,

    // The host abandoned a compile-time execution in flight (a cancelled evaluation).
    CancelledAtCompileTime,

    // An explicit, unrecoverable abort (`__panic__`) — e.g. a debug-mode `:pre`/`:post`
    // contract whose verdict was nil, or an `assert`/`unreachable` helper.
    Panic(String),

    // Terminated from outside: containment teardown of a terminated parent's subtree
    // (and, later, an explicit `%proc.kill` or link propagation). Reified for awaiters
    // as the `Killed` crash kind.
    Killed,

    // Scope management errors
    ScopeCountInvalid {
        expected: usize,
        found: usize,
    },
    ScopeUnderflow,
}

impl Error {
    /// A human-readable message for crash delivery (the `message` field of a `'crash`
    /// value). Internal-invariant errors (stack/scope/table
    /// misuse — compiler bugs, not user-reachable) all read as internal errors.
    pub fn crash_message(&self) -> String {
        match self {
            Error::Panic(message) => message.clone(),
            Error::Killed => "killed".to_string(),
            Error::TypeMismatch { expected, found } => {
                format!("type mismatch: expected {expected}, found {found}")
            }
            Error::ArityMismatch { expected, found } => {
                format!("arity mismatch: expected {expected}, found {found}")
            }
            Error::InvalidArgument(message) => message.clone(),
            Error::OperationNotAllowed { operation, context } => {
                format!("{operation} is not allowed in {context}")
            }
            Error::NotAnOwnedChild => "detach requires an owned child of the caller".to_string(),
            Error::UnsupportedAtCompileTime { operation } => {
                let doing = match operation {
                    Operation::Spawn => "spawning a process",
                    Operation::Send => "sending a message",
                    Operation::Select => "selecting",
                    Operation::Await => "awaiting a process",
                    Operation::Effect => "performing an effect",
                    Operation::ReadState => "reading a process's state",
                    Operation::Kill => "killing a process",
                    Operation::Link => "linking processes",
                    Operation::Detach => "detaching a process",
                    Operation::Track => "running a tracked render",
                    Operation::HostRead => "reading host state",
                    Operation::CreateRef => "creating a ref",
                };
                format!(
                    "{doing} is not supported in compile-time execution (module bodies \
                     are evaluated at compile time — move process and effect work into a \
                     function the module exports, or into the program that imports it)"
                )
            }
            Error::StalledAtCompileTime => "waiting to receive a message that can never \
                 arrive in compile-time execution (module bodies are evaluated at compile \
                 time — receive inside a function instead)"
                .to_string(),
            Error::ExhaustedAtCompileTime => "compile-time execution exceeded its step \
                 budget (module bodies are evaluated at compile time — move long-running \
                 work into a function the module exports)"
                .to_string(),
            Error::CancelledAtCompileTime => "compile-time execution was cancelled".to_string(),
            Error::VariableUndefined(name) => format!("undefined variable: {name}"),
            other => format!("internal error: {other:?}"),
        }
    }
}
