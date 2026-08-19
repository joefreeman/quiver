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
    /// A `%registry` operation — reads or writes the environment's name table.
    Registry,
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
            Operation::Registry => "registry operation",
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

impl fmt::Display for Error {
    /// The error's human-readable message. It reaches a reader two ways, and reads the same
    /// in both: printed by a host (`quiv run`, the REPL, the test runner), and carried as the
    /// `message` field of a `'crash` value to whoever awaits the failed process.
    ///
    /// Errors that can only mean a broken internal invariant — stack, frame, scope and table
    /// misuse, which no Quiver program can provoke — say "internal error" first, so a reader
    /// can tell "your program did this" from "this is a bug, report it". They are still
    /// spelled out rather than dumped as `Debug`: a bug report is worth more with a
    /// legible symptom in it.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Panic(message) => f.write_str(message),
            Error::Killed => f.write_str("killed"),
            Error::TypeMismatch { expected, found } => {
                write!(f, "type mismatch: expected {expected}, found {found}")
            }
            Error::ArityMismatch { expected, found } => {
                write!(f, "arity mismatch: expected {expected}, found {found}")
            }
            Error::InvalidArgument(message) => f.write_str(message),
            Error::VariableUndefined(name) => write!(f, "undefined variable: {name}"),
            Error::OperationNotAllowed { operation, context } => {
                write!(f, "{operation} is not allowed in {context}")
            }
            Error::NotAnOwnedChild => f.write_str("detach requires an owned child of the caller"),
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
                    Operation::Registry => "a registry operation",
                };
                write!(
                    f,
                    "{doing} is not supported in compile-time execution (module bodies \
                     are evaluated at compile time — move process and effect work into a \
                     function the module exports, or into the program that imports it)"
                )
            }
            Error::StalledAtCompileTime => f.write_str(
                "waiting to receive a message that can never arrive in compile-time \
                 execution (module bodies are evaluated at compile time — receive inside \
                 a function instead)",
            ),
            Error::ExhaustedAtCompileTime => f.write_str(
                "compile-time execution exceeded its step budget (module bodies are \
                 evaluated at compile time — move long-running work into a function the \
                 module exports)",
            ),
            Error::CancelledAtCompileTime => f.write_str("compile-time execution was cancelled"),

            // Broken internal invariants from here down.
            Error::StackUnderflow => f.write_str("internal error: stack underflow"),
            Error::FrameUnderflow => f.write_str("internal error: call frame underflow"),
            Error::ScopeUnderflow => f.write_str("internal error: scope underflow"),
            Error::ScopeCountInvalid { expected, found } => write!(
                f,
                "internal error: expected {expected} scopes, found {found}"
            ),
            Error::CallInvalid => {
                f.write_str("internal error: call of a value that is not callable")
            }
            Error::FunctionUndefined(index) => {
                write!(f, "internal error: no function at index {index}")
            }
            Error::BuiltinUndefined(index) => {
                write!(f, "internal error: no builtin at index {index}")
            }
            Error::ConstantUndefined(index) => {
                write!(f, "internal error: no constant at index {index}")
            }
            Error::FieldAccessInvalid(index) => {
                write!(f, "internal error: tuple has no field at index {index}")
            }
            Error::TupleEmpty => f.write_str("internal error: expected a non-empty tuple"),
        }
    }
}
