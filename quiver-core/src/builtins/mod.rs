use crate::effects::Effect;
use crate::error::{Error, Operation};
use crate::executor::Executor;
use crate::process::{Action, Process, ProcessId, TrackingState, Watcher};
use crate::program::Program;
use crate::types::Type;
use crate::value::ResourceId;
use crate::value::{Payload, Value};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// View a value as an integer, erroring with a type mismatch if it isn't one.
pub fn value_as_int(value: &Value) -> Result<crate::value::IntRef<'_>, Error> {
    value.as_int().ok_or_else(|| Error::TypeMismatch {
        expected: "integer".to_string(),
        found: value.type_name().to_string(),
    })
}

/// Extract an `i64` from an integer value, erroring on non-integers and on values
/// outside the i64 range. Used by the bounded (bitwise/binary) builtins, whose
/// semantics operate on machine integers.
pub fn value_to_i64(value: &Value) -> Result<i64, Error> {
    let n = value_as_int(value)?;
    n.to_i64().ok_or_else(|| {
        Error::InvalidArgument(format!("Integer {n} does not fit in a 64-bit value"))
    })
}

/// Extract a `usize` from an integer value, erroring if it's negative or too large.
pub fn value_to_usize(value: &Value) -> Result<usize, Error> {
    let n = value_as_int(value)?;
    let index = match n {
        crate::value::IntRef::Small(small) => small.to_usize(),
        crate::value::IntRef::Big(_) => None, // out of i64 range, never a valid index
    };
    index.ok_or_else(|| {
        Error::InvalidArgument(format!("Integer {n} does not fit in an unsigned index"))
    })
}

/// Extract a `u8` from an integer value, erroring if it's out of the `0..=255` byte range.
pub fn value_to_u8(value: &Value) -> Result<u8, Error> {
    let n = value_as_int(value)?;
    n.to_i64()
        .and_then(|small| small.to_u8())
        .ok_or_else(|| Error::InvalidArgument(format!("Integer {n} is not a byte (0..=255)")))
}

/// Build a `BigInt` from an `i64`. Lets downstream crates construct integer values
/// without depending on `num-bigint` directly.
pub fn bigint_from_i64(n: i64) -> BigInt {
    BigInt::from(n)
}

/// Parse a decimal string into a `BigInt`. Used to carry arbitrary-precision integers
/// across a boundary (e.g. the wasm/JS value bridge) as strings, so downstream crates
/// need not depend on `num-bigint` directly. The pairing `i.to_string()` round-trips.
pub fn bigint_from_str(s: &str) -> Result<BigInt, Error> {
    s.parse::<BigInt>()
        .map_err(|_| Error::InvalidArgument(format!("Invalid integer string: {s}")))
}

pub mod binary;
pub mod data;
pub mod integer;
pub mod io;
pub mod reference;
pub mod vector;

/// How a builtin call resolves. The bytecode contract of a builtin call is that exactly
/// one result lands on the caller's stack and execution advances past the call, exactly
/// once — a completion names which of the three ways that happens.
#[derive(Debug)]
pub enum Completion<E: Effect> {
    /// The result, immediately: the dispatch site pushes it and advances.
    Value(Value),
    /// The result comes from the host: the caller parks while the environment performs
    /// the effect, and the completion notification delivers the result and advances.
    Effect(E),
    /// The result comes from Quiver code: the dispatch site invokes the (nilary)
    /// function on a fresh frame and leaves the call un-advanced, so the frame's return
    /// delivers the callee's result as the builtin's own — exactly like an ordinary
    /// call (this is how `%proc.track` runs its thunk). The target is specifically a
    /// function — never a builtin, whose frameless resolution would break the tracking
    /// boundary and could route an action the dispatch site doesn't collect.
    Call {
        function: usize,
        captures: Rc<Payload>,
    },
}

/// What a builtin executes against: the executor (heap, constants, refs) plus verbs on
/// the calling process's record — which is out of the process map during the step, so
/// only reachable through here. Verbs take effect immediately (or queue the routed
/// action the step returns); the builtin's return value is its [`Completion`].
pub struct BuiltinContext<'a, E: Effect> {
    /// The executor, for heap and program access (binaries, constants, refs).
    pub executor: &'a mut Executor<E>,
    pid: ProcessId,
    process: &'a mut Process,
    action: Option<Action<E>>,
    /// A type-consuming builtin's explicit type argument (`__type_name__<'t>`), read
    /// off the called builtin value; `None` for ordinary builtins. Resolve it against
    /// the executor's `TypeLookup`.
    type_argument: Option<usize>,
}

impl<'a, E: Effect> BuiltinContext<'a, E> {
    pub(crate) fn new(
        pid: ProcessId,
        process: &'a mut Process,
        executor: &'a mut Executor<E>,
        type_argument: Option<usize>,
    ) -> Self {
        Self {
            executor,
            pid,
            process,
            action: None,
            type_argument,
        }
    }

    /// The calling process's id.
    pub fn pid(&self) -> ProcessId {
        self.pid
    }

    /// The explicit type argument the builtin was instantiated with (a type-consuming
    /// builtin's `<'t>`), or `None` when called without one.
    pub fn type_argument(&self) -> Option<usize> {
        self.type_argument
    }

    /// Take the routed action a verb queued, for the dispatch site to return from the step.
    pub(crate) fn take_action(&mut self) -> Option<Action<E>> {
        self.action.take()
    }

    fn queue(&mut self, action: Action<E>) {
        assert!(
            self.action.is_none(),
            "a builtin may queue at most one routed action"
        );
        self.action = Some(action);
    }

    /// Refuse `operation` in a restricted context (a receive filter or tracked render,
    /// either of which may be re-evaluated).
    fn allow(&self, operation: Operation) -> Result<(), Error> {
        match self.process.restricted_context() {
            Some(context) => Err(Error::OperationNotAllowed { operation, context }),
            None => Ok(()),
        }
    }

    /// Kill `target` (`%proc.kill`): fire-and-forget — the kill routes through the
    /// environment while the caller carries on. A self-kill lands at the next
    /// command-processing point, once the caller is back in the map.
    pub fn kill(&mut self, target: ProcessId) -> Result<(), Error> {
        self.allow(Operation::Kill)?;
        self.queue(Action::Kill { target });
        Ok(())
    }

    /// Link the caller and `target` (`%proc.link`): records the caller-side half on the
    /// caller's record and routes an `Action::Link` for the target-side half.
    /// Idempotent (one entry per peer); self-link is a no-op — a process cannot
    /// fate-share with itself.
    pub fn link(&mut self, target: ProcessId) -> Result<(), Error> {
        self.allow(Operation::Link)?;
        if target == self.pid {
            return Ok(());
        }
        let entry = Watcher::Link { pid: target };
        if !self.process.watchers.contains(&entry) {
            self.process.watchers.push(entry);
        }
        self.queue(Action::Link {
            caller: self.pid,
            target,
        });
        Ok(())
    }

    /// Detach `child` from the caller's owned children (the parent-only `%proc.detach`),
    /// so it survives the caller's termination. Fail-fast: no owned-child entry means
    /// the caller doesn't own the child — ownership is the parent's to relinquish.
    pub fn detach(&mut self, child: ProcessId) -> Result<(), Error> {
        self.allow(Operation::Detach)?;
        let before = self.process.watchers.len();
        self.process
            .watchers
            .retain(|watcher| !matches!(watcher, Watcher::OwnedChild { pid } if *pid == child));
        if self.process.watchers.len() == before {
            return Err(Error::NotAnOwnedChild);
        }
        Ok(())
    }

    /// Consume a stashed stream event for `resource_id`, yielding its byte payload —
    /// the pull-read half of read/select interop on one socket. A stashed `Data`
    /// answers its chunk; a stashed `Closed` answers the empty binary (the read-EOF
    /// convention). `None` when nothing is stashed.
    pub fn take_stream_bytes(&mut self, resource_id: ResourceId) -> Result<Option<Value>, Error> {
        let Some(event) = self.process.resource_events.remove(&resource_id) else {
            return Ok(None);
        };
        let bytes = match &event {
            Value::Tuple(_, payload) => payload.get(1).cloned(),
            _ => None,
        };
        let bytes = match bytes {
            Some(b) => b,
            None => Value::Binary(self.executor.allocate_binary(Vec::new())?),
        };
        // Deferred reclamation makes the ordering safe: releasing the event may zero
        // the chunk's refcount, but the dispatch site re-retains it (push) within the
        // same step, before any reclamation point.
        Ok(Some(bytes))
    }

    /// Whether `resource_id` has a select-armed read in flight (its event has not
    /// arrived yet). A plain read would race it out of order — reject instead.
    pub fn stream_armed(&self, resource_id: ResourceId) -> bool {
        self.process.armed_resources.contains(&resource_id)
    }

    /// Enter a tracked render (`%proc.track`): until the accompanying
    /// [`Completion::Call`] frame returns and reconciles subscriptions, each `?` the
    /// caller samples registers a reactive subscription. Renders must be pure and
    /// non-nested, so this refuses both restricted contexts.
    pub fn begin_tracking(&mut self) -> Result<(), Error> {
        self.allow(Operation::Track)?;
        self.process.tracking = Some(Box::new(TrackingState {
            sampled: HashSet::new(),
            boundary_len: 0,
        }));
        Ok(())
    }
}

/// Type specification for lazy type resolution
#[derive(Clone, Debug)]
pub enum TypeSpec {
    Integer,
    Binary,
    Reference,
    Tuple(Option<&'static str>, Vec<(Option<&'static str>, TypeSpec)>),
    Union(Vec<TypeSpec>),
    Process(Option<Box<TypeSpec>>, Option<Box<TypeSpec>>), // Process type: (send, receive)
    Resource(String), // Opaque resource type identifier (e.g., "File", "TcpSocket")
    /// A type variable, for a polymorphic builtin (e.g. `track`'s `'v`). The name should
    /// carry no `#<number>` suffix so it can't collide with a compiler-uniquified variable;
    /// the call site unifies and substitutes it fresh per call.
    Var(&'static str),
    /// A function type `#parameter -> result` (receives nothing; no state clause), for a
    /// builtin that takes or returns a function — e.g. `track`'s thunk parameter.
    Callable {
        parameter: Box<TypeSpec>,
        result: Box<TypeSpec>,
    },
}

impl TypeSpec {
    /// Resolve this type specification to a type ID in the Program's type registry
    pub fn resolve_to_id(&self, program: &mut Program) -> usize {
        let typ = self.resolve(program);
        program.register_type(typ)
    }

    /// Resolve this type specification to a concrete Type using the Program's type registry
    /// Note: This returns the Type itself, not a type ID
    pub fn resolve(&self, program: &mut Program) -> Type {
        match self {
            TypeSpec::Integer => Type::Integer,
            TypeSpec::Binary => Type::Binary,
            TypeSpec::Reference => Type::Reference,
            TypeSpec::Tuple(name, field_specs) => {
                let fields: Vec<(Option<String>, usize)> = field_specs
                    .iter()
                    .map(|(field_name, spec)| {
                        (
                            field_name.map(|s| s.to_string()),
                            spec.resolve_to_id(program),
                        )
                    })
                    .collect();
                let tuple_id = program.register_tuple(name.map(|s| s.to_string()), fields);
                Type::Tuple(tuple_id)
            }
            TypeSpec::Union(specs) => {
                let type_ids: Vec<usize> = specs
                    .iter()
                    .map(|spec| spec.resolve_to_id(program))
                    .collect();
                Type::Union(type_ids)
            }
            TypeSpec::Process(send, receive) => {
                let send_id = send.as_ref().map(|s| s.resolve_to_id(program));
                let receive_id = receive.as_ref().map(|r| r.resolve_to_id(program));
                Type::Process {
                    send: send_id,
                    receive: receive_id,
                    state: None,
                }
            }
            TypeSpec::Resource(name) => Type::Resource(name.clone()),
            TypeSpec::Var(name) => Type::Variable(name.to_string()),
            TypeSpec::Callable { parameter, result } => {
                let parameter = parameter.resolve_to_id(program);
                let result = result.resolve_to_id(program);
                let receive = program.never();
                Type::Callable {
                    parameter,
                    result,
                    receive,
                    states: None,
                }
            }
        }
    }
}

/// Function signature for builtin implementations
pub type BuiltinFn<E> = fn(&Value, &mut BuiltinContext<'_, E>) -> Result<Completion<E>, Error>;

/// How a builtin relates to state outside its argument — part of its signature contract,
/// independent of any host. The dispatch site consults it before invoking: contexts that
/// assume re-evaluation stability (receive filters, tracked renders) reject `Stateful`
/// and `HostRead`, and compile-time execution (which must be deterministic and produce
/// identity-free values) rejects both as well. `Effect` and `Process` builtins are
/// governed by their own gates — the effect-completion check and the context verbs — so
/// the purity gate passes them through.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Purity {
    /// A pure function of its argument (and the heap data it references) — allowed
    /// everywhere.
    Pure,
    /// Advances executor-local state (`%ref`'s counter): unstable across
    /// re-evaluations, and rejected at compile time — a ref is an identity, and a
    /// compiled module's value must be identity-free so it can be shared (and one day
    /// serialized) across the sessions that import it. Mint refs at runtime instead.
    Stateful,
    /// Reads host state (clock, entropy): nondeterministic across runs.
    HostRead,
    /// Parks for a host effect ([`Completion::Effect`]): rejected where effects are,
    /// at the completion site.
    Effect,
    /// Operates on the calling process through [`BuiltinContext`] verbs, each of which
    /// carries its own restricted-context check.
    Process,
}

/// Function signature for builtin module registration
pub type BuiltinModule<E> = fn(&mut BuiltinRegistry<E>);

/// Helper to coerce function item to function pointer (avoids rust-analyzer warnings)
const fn coerce_builtin<E: Effect>(f: BuiltinFn<E>) -> BuiltinFn<E> {
    f
}

/// Macro for registering a single builtin function. The purity class may be omitted
/// for a [`Purity::Pure`] builtin.
macro_rules! register_builtin {
    ($registry:expr, $fn_name:literal, $impl:path, $param:expr => $result:expr) => {
        register_builtin!($registry, $fn_name, $impl, Purity::Pure, $param => $result);
    };
    ($registry:expr, $fn_name:literal, $impl:path, $purity:expr, $param:expr => $result:expr) => {
        $registry.register($fn_name.to_string(), coerce_builtin($impl), $purity, $param, $result);
    };
}

/// A stream resource kind's select-event vocabulary, declared alongside the builtin
/// signatures that produce the resource (see `register_stream`). Each event is a
/// tuple TypeSpec with a fixed field convention: the first field is the source
/// resource; `data`'s second field is the byte chunk; `resource`'s second field is
/// the produced resource (whose kind is named alongside).
#[derive(Clone, Debug)]
pub struct StreamSpec {
    /// The bytes event (e.g. `Data[sock: \TcpSocket, data: 'bin]`), or None for
    /// streams that never yield bytes.
    pub data: Option<TypeSpec>,
    /// The fresh-resource event (e.g. `Accepted[listener, sock]`) and the produced
    /// resource's type name, or None for byte-only streams.
    pub resource: Option<(TypeSpec, String)>,
    /// The end-of-stream event (e.g. `Closed[sock]`).
    pub end: TypeSpec,
}

/// The crash-delivery vocabulary: the shapes the runtime synthesizes when a process
/// crashes, is killed, or a select times out. Declared by the always-set (every
/// executing host registers it), resolved per merged program by
/// `Program::runtime_tables`.
#[derive(Clone, Debug)]
pub struct CrashDecl {
    /// `Error[pid: (@), message: Str['bin]]` — a runtime error's crash payload.
    pub error: TypeSpec,
    /// `Panic[pid: (@), message: Str['bin]]` — a `__panic__` abort's payload.
    pub panic: TypeSpec,
    /// `Killed` — kill/link/containment teardown.
    pub killed: TypeSpec,
    /// `Str['bin]` — the message wrapper inside the payloads.
    pub str: TypeSpec,
    /// The annotation key a crash's nil is stamped under.
    pub crash_key: String,
    /// The annotation key a timeout's nil is stamped under.
    pub timeout_key: String,
}

/// The io-failure vocabulary: the payload a failed effect's nil is stamped with. Declared
/// by the io capability groups (a host without them can never produce one), resolved per
/// merged program by `Program::runtime_tables`.
#[derive(Clone, Debug)]
pub struct ErrorDecl {
    /// `IoError[kind: (NotFound | …), message: Str['bin]]`.
    pub io_error: TypeSpec,
    /// The nullary kind tags, in [`EffectError::KINDS`](crate::effects::EffectError::KINDS)
    /// order — the executor indexes this by `EffectError::kind_index`.
    pub kinds: Vec<TypeSpec>,
    /// `Str['bin]` — the message wrapper.
    pub str: TypeSpec,
    /// The annotation key a failure's nil is stamped under. This is the *program-owned*
    /// `error` key: one key keeps "did this fail?" a single test, and the payload type
    /// discriminates an io failure from a program's own.
    pub error_key: String,
}

/// The reactive-wakeup vocabulary: the `Changed` message a subscriber receives.
/// Demand-scoped — resolved only when the gating builtin (`track`) is referenced by
/// the program, since subscriptions cannot exist without it.
#[derive(Clone, Debug)]
pub struct ChangedDecl {
    /// The wakeup tuple (`Changed`, empty).
    pub tuple: TypeSpec,
    /// The builtin whose presence in a program demands this vocabulary.
    pub demand_builtin: String,
}

/// Everything the runtime may deliver to programs on this host, as declared by the
/// registered capability groups: crash shapes (always-set), the reactive wakeup
/// (gated on `track`), and stream events (per resource kind, from the io groups).
/// Handed to the environment once (`set_runtime_declarations`) and resolved against
/// each merged program.
#[derive(Clone, Debug, Default)]
pub struct RuntimeDeclarations {
    pub crash: Option<CrashDecl>,
    pub changed: Option<ChangedDecl>,
    pub error: Option<ErrorDecl>,
    pub streams: HashMap<String, StreamSpec>,
}

/// A registered builtin: its implementation, purity class, and type signature.
#[derive(Clone)]
pub struct BuiltinEntry<E: Effect> {
    pub implementation: BuiltinFn<E>,
    pub purity: Purity,
    pub parameter: TypeSpec,
    pub result: TypeSpec,
    /// Declared type parameters, for a **type-consuming** builtin: names in declaration
    /// order (matching any `TypeSpec::Var` occurrences in the signature). Non-empty
    /// means every call must instantiate explicitly (`__type_name__<'t>`) — the compiler
    /// resolves the arguments and embeds the (single, for now) type id in the emitted
    /// instruction for the implementation to read via `BuiltinContext::type_argument`.
    pub type_parameters: Vec<String>,
}

/// Registry of all available builtin functions
#[derive(Clone)]
pub struct BuiltinRegistry<E: Effect> {
    functions: HashMap<String, BuiltinEntry<E>>,
    /// The runtime-delivered vocabulary the registered capability groups declare:
    /// crash shapes, the reactive wakeup, and stream events. Part of the contract,
    /// like signatures and purity.
    runtime: RuntimeDeclarations,
}

impl<E: Effect> Default for BuiltinRegistry<E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<E: Effect> BuiltinRegistry<E> {
    /// Create a new empty builtin registry
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            runtime: RuntimeDeclarations::default(),
        }
    }

    /// Register a single builtin function
    pub fn register(
        &mut self,
        name: String,
        impl_fn: BuiltinFn<E>,
        purity: Purity,
        param: TypeSpec,
        result: TypeSpec,
    ) {
        self.functions.insert(
            name,
            BuiltinEntry {
                implementation: impl_fn,
                purity,
                parameter: param,
                result,
                type_parameters: vec![],
            },
        );
    }

    /// Register a **type-consuming** builtin: one whose behavior depends on an explicit
    /// type argument (`__type_name__<'t>`), declared here in order. Currently limited to
    /// exactly one parameter — the emitted instruction carries a single type id.
    pub fn register_generic(
        &mut self,
        name: String,
        impl_fn: BuiltinFn<E>,
        purity: Purity,
        param: TypeSpec,
        result: TypeSpec,
        type_parameters: Vec<String>,
    ) {
        assert_eq!(
            type_parameters.len(),
            1,
            "type-consuming builtins take exactly one type parameter (for now)"
        );
        self.functions.insert(
            name,
            BuiltinEntry {
                implementation: impl_fn,
                purity,
                parameter: param,
                result,
                type_parameters,
            },
        );
    }

    /// A type-consuming builtin's declared type parameters (empty for ordinary builtins).
    pub fn get_type_parameters(&self, function: &str) -> Option<&[String]> {
        self.functions
            .get(function)
            .map(|entry| entry.type_parameters.as_slice())
    }

    /// Attach (replace) the implementation of an already-registered builtin, keeping its
    /// signature. This is how an executing host backs a builtin whose *signature* is part of the
    /// universal contract but whose *implementation* it provides — e.g. the IO builtins, whose
    /// signatures are registered everywhere (via [`core_modules`]) but whose runtime differs per
    /// host (native io-uring, a web backend, or none in a type-checker).
    pub fn attach_implementation(&mut self, name: &str, impl_fn: BuiltinFn<E>) {
        match self.functions.get_mut(name) {
            Some(entry) => entry.implementation = impl_fn,
            None => debug_assert!(
                false,
                "attaching an implementation for unregistered builtin `{name}`; its signature \
                 must be registered first (see core_modules)"
            ),
        }
    }

    /// Merge another registry into this one
    pub fn merge(&mut self, other: Self) {
        self.functions.extend(other.functions);
        if other.runtime.crash.is_some() {
            self.runtime.crash = other.runtime.crash;
        }
        if other.runtime.changed.is_some() {
            self.runtime.changed = other.runtime.changed;
        }
        self.runtime.streams.extend(other.runtime.streams);
    }

    /// Declare a resource kind as a stream: selectable, yielding the spec's events.
    pub fn register_stream(&mut self, resource_name: &str, spec: StreamSpec) {
        self.runtime.streams.insert(resource_name.to_string(), spec);
    }

    /// Declare the crash-delivery vocabulary (the always-set does this once).
    pub fn declare_crash(&mut self, decl: CrashDecl) {
        self.runtime.crash = Some(decl);
    }

    /// Declare the reactive-wakeup vocabulary, gated on its demanding builtin.
    pub fn declare_changed(&mut self, decl: ChangedDecl) {
        self.runtime.changed = Some(decl);
    }

    /// Declare the io-failure vocabulary (the io capability groups do this).
    pub fn declare_error(&mut self, decl: ErrorDecl) {
        self.runtime.error = Some(decl);
    }

    /// The stream declaration for a resource kind, if it is one.
    pub fn stream_spec(&self, resource_name: &str) -> Option<&StreamSpec> {
        self.runtime.streams.get(resource_name)
    }

    /// Everything the runtime may deliver on this host — for the environment to
    /// resolve per merged program (`Program::runtime_tables`).
    pub fn runtime_declarations(&self) -> &RuntimeDeclarations {
        &self.runtime
    }

    /// Create a registry from a list of builtin module functions
    pub fn with_modules(modules: &[BuiltinModule<E>]) -> Self {
        let mut registry = Self::new();
        for module in modules {
            module(&mut registry);
        }
        registry
    }

    /// Get the implementation function for a builtin by function name
    pub fn get_implementation(&self, function: &str) -> Option<BuiltinFn<E>> {
        self.functions
            .get(function)
            .map(|entry| entry.implementation)
    }

    /// Get the purity class for a builtin by function name
    pub fn get_purity(&self, function: &str) -> Option<Purity> {
        self.functions.get(function).map(|entry| entry.purity)
    }

    /// Resolve and get the type signature for a builtin by function name
    pub fn resolve_signature(&self, function: &str, program: &mut Program) -> Option<(Type, Type)> {
        self.functions.get(function).map(|entry| {
            let param_type = entry.parameter.resolve(program);
            let result_type = entry.result.resolve(program);
            (param_type, result_type)
        })
    }

    /// Get the type specs for a builtin by function name (without resolving)
    pub fn get_specs(&self, function: &str) -> Option<(&TypeSpec, &TypeSpec)> {
        self.functions
            .get(function)
            .map(|entry| (&entry.parameter, &entry.result))
    }

    /// Get all available function names
    pub fn get_function_names(&self) -> Vec<String> {
        let mut functions: Vec<String> = self.functions.keys().cloned().collect();
        functions.sort_unstable();
        functions
    }
}

/// Register all binary builtins (binary data manipulation)
pub fn register_binary_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    // Common type specifications
    let bin_int = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Binary), (None, TypeSpec::Integer)],
    );
    let bin_bin = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Binary), (None, TypeSpec::Binary)],
    );
    let bin_int_int = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
        ],
    );
    let bin_int_int_int = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
        ],
    );
    let bin_int_int_int_int = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
        ],
    );

    // Binary functions
    register_builtin!(registry, "binary_new", binary::builtin_binary_new, TypeSpec::Integer => TypeSpec::Binary);
    register_builtin!(registry, "binary_length", binary::builtin_binary_length, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(registry, "binary_concat", binary::builtin_binary_concat, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(registry, "binary_repeat", binary::builtin_binary_repeat, bin_int.clone() => TypeSpec::Binary);

    // Bitwise operations
    register_builtin!(registry, "binary_and", binary::builtin_binary_and, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(registry, "binary_or", binary::builtin_binary_or, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(registry, "binary_xor", binary::builtin_binary_xor, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(registry, "binary_not", binary::builtin_binary_not, TypeSpec::Binary => TypeSpec::Binary);

    // Shift operations
    register_builtin!(registry, "binary_shift", binary::builtin_binary_shift, bin_int => TypeSpec::Binary);

    // Bit-level operations
    register_builtin!(registry, "binary_popcount", binary::builtin_binary_popcount, TypeSpec::Binary => TypeSpec::Integer);

    // Multi-byte operations (unified bit/byte access)
    register_builtin!(registry, "binary_get", binary::builtin_binary_get, bin_int_int_int => TypeSpec::Integer);
    register_builtin!(registry, "binary_set", binary::builtin_binary_set, bin_int_int_int_int => TypeSpec::Binary);

    // Slicing operations
    register_builtin!(registry, "binary_slice", binary::builtin_binary_slice, bin_int_int.clone() => TypeSpec::Binary);

    // Byte search: index of a byte at or after an offset, or nil
    register_builtin!(registry, "binary_index", binary::builtin_binary_index, bin_int_int.clone() => TypeSpec::Union(vec![TypeSpec::Integer, TypeSpec::Tuple(None, vec![])]));

    // Hashing operations
    register_builtin!(registry, "binary_hash32", binary::builtin_binary_hash32, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(registry, "binary_hash64", binary::builtin_binary_hash64, TypeSpec::Binary => TypeSpec::Integer);

    // Append operation
    register_builtin!(registry, "binary_append", binary::builtin_binary_append, bin_int_int => TypeSpec::Binary);
}

/// Register all integer builtins: arithmetic/math (arbitrary-precision) and bitwise (64-bit).
pub fn register_integer_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    // Common type specification
    let int_int = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Integer), (None, TypeSpec::Integer)],
    );

    // Unary math functions
    register_builtin!(registry, "integer_abs", integer::builtin_integer_abs, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(registry, "integer_sqrt", integer::builtin_integer_sqrt, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(registry, "integer_sin", integer::builtin_integer_sin, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(registry, "integer_cos", integer::builtin_integer_cos, TypeSpec::Integer => TypeSpec::Integer);

    // Arithmetic operations - operate on [int, int] tuples
    register_builtin!(registry, "integer_add", integer::builtin_integer_add, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_subtract", integer::builtin_integer_subtract, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_multiply", integer::builtin_integer_multiply, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_divide", integer::builtin_integer_divide, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_modulo", integer::builtin_integer_modulo, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_gcd", integer::builtin_integer_gcd, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_compare", integer::builtin_integer_compare, int_int.clone() => TypeSpec::Integer);

    // Integer bitwise operations
    register_builtin!(registry, "integer_and", integer::builtin_integer_and, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_or", integer::builtin_integer_or, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_xor", integer::builtin_integer_xor, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_not", integer::builtin_integer_not, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(registry, "integer_shift", integer::builtin_integer_shift, int_int => TypeSpec::Integer);
    register_builtin!(registry, "integer_popcount", integer::builtin_integer_popcount, TypeSpec::Integer => TypeSpec::Integer);
}

/// Register all packed-vector kernels (schema-agnostic byte-buffer numerics; see
/// [`vector`]). Lanes are little-endian two's-complement; widths are 4 or 8 bytes.
pub fn register_vector_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let nil = || TypeSpec::Tuple(None, vec![]);
    let bin_or_nil = TypeSpec::Union(vec![TypeSpec::Binary, nil()]);
    let int_or_nil = TypeSpec::Union(vec![TypeSpec::Integer, nil()]);
    // [bin, bin, width]
    let bin_bin_int = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
        ],
    );
    // [bin, width, int] — used for get (index) and push (value)
    let bin_int_int = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
        ],
    );
    let bin_int = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Binary), (None, TypeSpec::Integer)],
    );
    // [data, width, mask] — used for take (gather)
    let bin_int_bin = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Binary),
        ],
    );

    register_builtin!(registry, "vector_add", vector::builtin_vector_add, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_subtract", vector::builtin_vector_subtract, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_multiply", vector::builtin_vector_multiply, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_less_than", vector::builtin_vector_less_than, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_equal", vector::builtin_vector_equal, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_greater_than", vector::builtin_vector_greater_than, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_dot", vector::builtin_vector_dot, bin_bin_int => int_or_nil.clone());
    register_builtin!(registry, "vector_take", vector::builtin_vector_take, bin_int_bin => bin_or_nil.clone());
    register_builtin!(registry, "vector_get", vector::builtin_vector_get, bin_int_int.clone() => int_or_nil);
    register_builtin!(registry, "vector_push", vector::builtin_vector_push, bin_int_int => bin_or_nil);
    register_builtin!(registry, "vector_sum", vector::builtin_vector_sum, bin_int => TypeSpec::Integer);
}

/// Register the reference builtin (`%ref`): a nilary function minting a unique, opaque ref.
pub fn register_reference_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let nil = TypeSpec::Tuple(None, vec![]);
    // Stateful: each call advances the executor's ref counter, so a fresh identity per
    // evaluation — unstable under re-evaluation, and banned at compile time (a module
    // value must be identity-free).
    register_builtin!(registry, "reference", reference::builtin_reference, Purity::Stateful, nil => TypeSpec::Reference);
}

/// Abort the current process with a runtime panic carrying the given `Str` message. It
/// never returns a value (its result type is the empty union), so a chain step after it is
/// unreachable. This is the language's assertion/trap primitive — debug-mode contract
/// checks compile to it, and it backs any `assert`/`unreachable`-style helper. It is
/// deliberately *not* a nil: a panic is an unrecoverable bug, not a short-circuiting
/// failure, so it propagates as a runtime error rather than flowing on as data.
pub fn builtin_panic<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let message = match arg {
        Value::Tuple(_, fields) if fields.len() == 1 => match &fields[0] {
            Value::Binary(binary) => {
                String::from_utf8_lossy(&ctx.executor.get_binary_data(binary)?.to_vec())
                    .into_owned()
            }
            other => {
                return Err(Error::TypeMismatch {
                    expected: "Str[binary]".to_string(),
                    found: other.type_name().to_string(),
                });
            }
        },
        other => {
            return Err(Error::TypeMismatch {
                expected: "Str[binary]".to_string(),
                found: other.type_name().to_string(),
            });
        }
    };
    Err(Error::Panic(message))
}

pub fn register_control_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let str = TypeSpec::Tuple(Some("Str"), vec![(None, TypeSpec::Binary)]);
    // `__panic__` never returns: its result is the empty union (`never`).
    register_builtin!(registry, "panic", builtin_panic, str => TypeSpec::Union(vec![]));
}

/// The formatted form of the builtin's explicit type argument (`__type_name__<'t>`), as
/// UTF-8 bytes — wrap in `Str[...]` for display. The first **type-consuming** builtin:
/// its behavior depends on its instantiation (read from the context), not on its (nil)
/// argument. Deterministic per program, so pure.
pub fn builtin_type_name<E: Effect>(
    _arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let type_id = ctx.type_argument().ok_or_else(|| {
        Error::InvalidArgument(
            "__type_name__ called without a type argument — a bare reference carries no \
             instantiation; name it with one (`__type_name__<'t>`) where the type is \
             concrete"
                .to_string(),
        )
    })?;
    let formatted = crate::format::format_type_by_id(&*ctx.executor, type_id);
    let binary = ctx.executor.allocate_binary(formatted.into_bytes())?;
    Ok(Completion::Value(Value::Binary(binary)))
}

pub fn register_type_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let nil = TypeSpec::Tuple(None, vec![]);
    registry.register_generic(
        "type_name".to_string(),
        coerce_builtin(builtin_type_name),
        Purity::Pure,
        nil,
        TypeSpec::Binary,
        vec!["t".to_string()],
    );
}

/// The Quiver data notation codec (`%data`): `data_encode` walks any data value into
/// its textual form; `data_decode` is type-consuming — the expected type drives the
/// parse and appears as the result (`#Str['bin] -> ('t | [])`), so the declared
/// parameter list names the same `t` its signature's result carries.
pub fn register_data_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let str_spec = TypeSpec::Tuple(Some("Str"), vec![(None, TypeSpec::Binary)]);
    let nil = TypeSpec::Tuple(None, vec![]);
    register_builtin!(registry, "data_encode", data::builtin_data_encode, TypeSpec::Var("t") => TypeSpec::Binary);
    registry.register_generic(
        "data_decode".to_string(),
        coerce_builtin(data::builtin_data_decode),
        Purity::Pure,
        str_spec,
        TypeSpec::Union(vec![TypeSpec::Var("t"), nil]),
        vec!["t".to_string()],
    );
}

/// Detach an owned child from the calling process (the parent-only `%proc.detach`):
/// the child survives the caller's termination. Errors when the argument is not an
/// owned child of the caller — ownership is the parent's to relinquish, like operating
/// on another process's resource.
pub fn builtin_process_detach<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let Value::Process(child, _) = arg else {
        return Err(Error::TypeMismatch {
            expected: "process".to_string(),
            found: arg.type_name().to_string(),
        });
    };
    ctx.detach(*child)?;
    Ok(Completion::Value(Value::ok()))
}

/// Kill a process (`%proc.kill`): always effective — there
/// is no trap flag — and idempotent on an already-terminated target. Awaiters observe
/// the `Killed` crash kind; the target's owned subtree is torn down with it.
pub fn builtin_process_kill<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let Value::Process(target, _) = arg else {
        return Err(Error::TypeMismatch {
            expected: "process".to_string(),
            found: arg.type_name().to_string(),
        });
    };
    ctx.kill(*target)?;
    Ok(Completion::Value(Value::ok()))
}

/// Link the calling process and the target (`%proc.link`): symmetric fate-sharing —
/// if either terminates *abnormally* (crash or kill), the other is killed; normal
/// completion never propagates. Linking an already-crashed process kills the caller
/// immediately (tombstones keep the error, so there is no establishment race).
pub fn builtin_process_link<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let Value::Process(target, _) = arg else {
        return Err(Error::TypeMismatch {
            expected: "process".to_string(),
            found: arg.type_name().to_string(),
        });
    };
    ctx.link(*target)?;
    Ok(Completion::Value(Value::ok()))
}

/// Run a reactive tracked render (`%proc.track thunk`): evaluate the
/// nilary `thunk` with dependency tracking on, subscribing the caller to every process the
/// thunk samples (`?`) and reconciling those subscriptions when it returns. Tracking is
/// entered here; the thunk itself runs as the builtin's [`Completion::Call`], so its
/// return both reconciles the subscriptions and delivers `track`'s result.
pub fn builtin_track<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let Value::Function(function, captures) = arg else {
        return Err(Error::TypeMismatch {
            expected: "function".to_string(),
            found: arg.type_name().to_string(),
        });
    };
    ctx.begin_tracking()?;
    Ok(Completion::Call {
        function: *function,
        captures: captures.clone(),
    })
}

/// Register the process-management builtins (`%proc`).
pub fn register_process_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let pid = TypeSpec::Process(None, None);
    let ok = TypeSpec::Tuple(Some("Ok"), vec![]);
    register_builtin!(registry, "process_detach", builtin_process_detach, Purity::Process, pid.clone() => ok.clone());
    register_builtin!(registry, "process_kill", builtin_process_kill, Purity::Process, pid.clone() => ok.clone());
    register_builtin!(registry, "process_link", builtin_process_link, Purity::Process, pid => ok);
    // `track`'s polymorphic type `#(#[] -> 'v) -> 'v`: it takes a nilary thunk and returns
    // whatever the thunk returns. The `'v` variable is unified/substituted fresh per call
    // by the ordinary generic-call path.
    let thunk = TypeSpec::Callable {
        parameter: Box::new(TypeSpec::Tuple(None, vec![])),
        result: Box::new(TypeSpec::Var("v")),
    };
    register_builtin!(registry, "track", builtin_track, Purity::Process, thunk => TypeSpec::Var("v"));

    // The runtime-delivered vocabulary this group brings: crash payloads (any
    // process can crash — always resolved) and the reactive `Changed` wakeup
    // (resolved only for programs that reference `track`, since subscriptions
    // cannot exist without it). Content-addressed twins of `std/proc.qv`'s aliases.
    let str_spec = TypeSpec::Tuple(Some("Str"), vec![(None, TypeSpec::Binary)]);
    // The capability-less process type `(@)` — a crash payload's pid grants identity only.
    let crash_fields = |s: &TypeSpec| {
        vec![
            (Some("pid"), TypeSpec::Process(None, None)),
            (Some("message"), s.clone()),
        ]
    };
    registry.declare_crash(CrashDecl {
        error: TypeSpec::Tuple(Some("Error"), crash_fields(&str_spec)),
        panic: TypeSpec::Tuple(Some("Panic"), crash_fields(&str_spec)),
        killed: TypeSpec::Tuple(Some("Killed"), vec![]),
        str: str_spec,
        crash_key: "crash".to_string(),
        timeout_key: "timeout".to_string(),
    });
    registry.declare_changed(ChangedDecl {
        tuple: TypeSpec::Tuple(Some("Changed"), vec![]),
        demand_builtin: "track".to_string(),
    });
}

/// The always-available builtin modules: the pure builtins (integer/binary/vector)
/// with their universal implementations, plus refs, control, and process management —
/// the language runtime, independent of any host capability.
///
/// The IO builtins are deliberately NOT here: a registry is a host's *capability
/// set*, and hosts compose in the [`io_modules`] groups they can actually execute
/// (attaching implementations via [`BuiltinRegistry::attach_implementation`]).
/// Referencing a builtin absent from the registry is a compile error, so "the
/// program compiles" means "this host can run it".
pub fn core_modules<E: Effect>() -> Vec<BuiltinModule<E>> {
    vec![
        register_binary_builtins,
        register_integer_builtins,
        register_vector_builtins,
        register_reference_builtins,
        register_control_builtins,
        register_type_builtins,
        register_data_builtins,
        register_process_builtins,
    ]
}

/// The IO builtin signature groups (file, network + stream declarations, system
/// clocks/entropy) — the capability vocabulary an executing host opts into, or a
/// type-checking host (the LSP) registers in full as the permissive union.
pub fn io_modules<E: Effect>() -> Vec<BuiltinModule<E>> {
    vec![io::register_io_signatures, io::register_system_signatures]
}

/// Just the system builtins (entropy, clocks) — the subset of [`io_modules`] that needs no
/// effect backend, only implementations. A host with no filesystem or sockets (a browser) can
/// serve these alone, and `%time`/`%random` then compile and run there unchanged.
pub fn system_modules<E: Effect>() -> Vec<BuiltinModule<E>> {
    vec![io::register_system_signatures]
}

/// The TLS builtins, over a socket the network group provides. Separate from `io_modules`
/// so a host with sockets but no TLS is representable.
pub fn tls_modules<E: Effect>() -> Vec<BuiltinModule<E>> {
    vec![io::register_tls_signatures]
}

/// The `fetch` builtin — a browser's whole io capability, since it cannot open a socket.
/// Deliberately *not* part of [`io_modules`]: a host with sockets builds HTTP over them in
/// Quiver, and registering both would let a program compile against `fetch` on a host where
/// it means something else.
pub fn fetch_modules<E: Effect>() -> Vec<BuiltinModule<E>> {
    vec![io::register_fetch_signatures]
}
