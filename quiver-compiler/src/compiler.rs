use std::collections::{HashMap, HashSet};

mod annotations;
mod codegen;
mod dialect;
mod helpers;
mod modules;
mod narrowing;
use narrowing::{
    Narrowing, analyze_tuple_pattern_for_complement, apply_narrowing, compute_complement,
    get_field_narrowing, get_field_type, get_type_for_provenance,
};
mod pattern;
mod provenance;
mod scopes;
mod spread;
mod type_queries;
mod typing;
mod variables;

pub use codegen::InstructionBuilder;
pub use modules::{
    CachedModule, ModuleCache, ModuleTypeNamespace, collect_module_references,
    collect_value_imports, module_type_namespace,
};
pub use provenance::{Narrowings, Provenance};
pub use scopes::{Bindings, Parameter, Scope, ScopeKind, Variable};
pub use typing::{TupleAccessor, TypeAliasDef, resolve_type_alias_for_display, union_type_ids};

use crate::{
    ast,
    parser::SourceSpan,
    recorder::{Recorder, SymbolKind},
    resolver::{ModuleError, ModuleOrigin, ModuleResolver, PackageId},
};

use quiver_core::{
    bytecode::{Constant, Function, Instruction},
    program::Program,
    types::{NIL, OK, Type, TypeLookup},
    value::{Binary, Payload, Value},
};

#[derive(Debug, PartialEq)]
pub enum Error {
    // Undefined errors
    VariableUndefined(String),
    /// An outer-parameter reference (`$$`, `$$$x`) names more enclosing functions than
    /// surround it — the sigil run reaches above the outermost function.
    ParameterDepthExceeded {
        written: String,
    },
    BuiltinUndefined(String),
    FunctionUndefined(usize),

    // Type system errors
    TypeUnresolved(String),
    TypeAliasMissing(String),
    TypeMismatch {
        expected: String,
        found: String,
    },
    /// A function body is a non-exhaustive enumeration (some inputs match no branch, so it can
    /// fall through to `[]`), but its declared return type does not allow `[]`.
    NonExhaustiveReturn {
        unhandled: String,
        declared: String,
    },
    /// Explicit type arguments (`f<'t>`) on something that can't take them: a non-callable
    /// value, a callable with no declared type parameters, or a callable whose parameter
    /// list isn't statically known (a declared boundary — a function parameter's written
    /// type — sheds it, as it does other capabilities).
    TypeArgumentsNotApplicable {
        target: String,
    },
    /// More explicit type arguments than the callable declares.
    TypeArgumentsTooMany {
        declared: usize,
        given: usize,
    },
    /// A type-consuming builtin (`__data_decode__`) called without its explicit type
    /// argument(s): the implementation reads the type at runtime, so the call site must
    /// instantiate (`__data_decode__<'t>`).
    TypeArgumentsRequired {
        builtin: String,
        declared: usize,
    },
    /// A type-consuming builtin's type argument resolved to a type still containing
    /// type variables — there is no concrete type id to embed at the call site.
    TypeArgumentNotConcrete {
        builtin: String,
    },
    TupleNotInRegistry {
        tuple_id: usize,
    },

    // Structure errors
    FieldDuplicated(String),
    TupleFieldTypeUnresolved {
        field_index: usize,
    },

    // Access errors
    FieldNotFound {
        field_name: String,
        tuple_id: usize,
    },
    FieldAccessOnNonTuple {
        field_name: String,
    },
    PositionalAccessOnNonTuple {
        index: usize,
    },

    // Member access
    MemberFieldNotFound {
        field_name: String,
        target: String,
    },
    MemberAccessOnNonTuple {
        target: String,
    },
    /// An alternation pattern (`(p | q)`) whose alternatives bind different variables. Every
    /// alternative must bind the same set, so the body sees them whichever one matched.
    OrPatternBindingMismatch {
        expected: Vec<String>,
        found: Vec<String>,
    },

    // Positional access
    PositionalIndexOutOfBounds {
        index: usize,
    },
    /// Positional access (`.0`) through a partial type: a partial constrains fields by
    /// name only, so the value's layout — and hence any position — is unknown.
    PositionalAccessOnPartial {
        index: usize,
    },

    // Operator errors
    OperatorTypeNotInRegistry {
        tuple_id: String,
    },
    OperatorOnNonTuple {
        operator: String,
    },

    // Module errors
    ModuleLoad(ModuleError),
    // The embedded errors are boxed to keep `Error` small (it's the `Err` of nearly every
    // compiler function, and these two variants would otherwise dominate its size).
    ModuleParse {
        module: String,
        error: Box<crate::parser::Error>,
    },
    ModuleExecution {
        module: String,
        error: Box<quiver_core::error::Error>,
    },
    ModuleTypeMissing {
        type_name: String,
        module: String,
    },
    ModuleTypeCycle(String),

    // Dialect errors
    /// A dialect invocation (`%mod{…}`) on a module whose value carries no `:dialect`
    /// annotation.
    DialectMissing {
        module: String,
    },
    /// A dialect function failed (nil, with any `:error` payload folded into the message)
    /// or returned a value that isn't a well-formed `'%meta.expr`.
    DialectFailed {
        module: String,
        message: String,
    },

    // Language feature errors
    FeatureUnsupported(String),

    // Destructuring errors
    DestructuringOnNonTuple(String),
    DestructuringFieldMissing {
        type_name: String,
        field_name: String,
    },

    // Pattern matching errors
    PatternNoMatchingTypes {
        pattern: String,
    },
    /// A fallible match is followed by further terms in its chain. Nothing
    /// short-circuits within a chain, so the continuation would run whether or not the
    /// match succeeded — which would make the pattern's narrowing (its bindings and the
    /// scrutinee's refinement) unsound. A fallible match must end its chain, so that
    /// its verdict directly gates a step boundary (`=P; …`) or a branch (`=P => …`).
    FallibleMatchNotChainFinal,
    /// A fallible match that binds variables appears in a value chain — a tuple field,
    /// an argument, an annotation value — where its verdict is data and gates nothing:
    /// the surrounding code runs whether or not it matched, so the bindings cannot be
    /// relied on.
    FallibleMatchBindingsInValueChain {
        bindings: Vec<String>,
    },
    /// A binding pattern appears in a `//=>` assertion. Nothing gates on the assertion's
    /// verdict — and a release build skips the check entirely — so the binding could never
    /// be relied on. Assertions observe: literals, types and pins only.
    AssertionBindings {
        bindings: Vec<String>,
    },

    /// A value flows into a union whose members include functions or processes, and the
    /// union is not sendable (a union of only process types is — the message is checked
    /// against every member's send type). Rejected rather than silently compiled as a
    /// replace, which would discard the flowing value.
    UnionApplication {
        union: String,
        all_functions: bool,
    },

    // Internal consistency errors
    InternalError {
        message: String,
    },

    /// An error augmented with a usage hint — e.g. the Apply-site-inference note attached
    /// when a `#{…}` literal that reads `$` fell back to a nil parameter and its body failed.
    Noted {
        error: Box<Error>,
        note: String,
    },
}

/// Whether an expression's chains read the enclosing function's parameter (`$`, `$x`, `$0`) —
/// without descending into nested function literals, whose `$` is their own. Used to decide
/// whether a failed fallback-nil `#{…}` literal deserves the Apply-site-inference note.
fn block_references_parameter(block: &ast::Block) -> bool {
    let chain = |chain: &ast::Chain| chain.terms.iter().any(term_references_parameter);
    block.annotations.iter().any(|a| chain(&a.value))
        || block.branches.iter().any(|branch| {
            branch.condition.chains().any(chain)
                || branch
                    .consequence
                    .as_ref()
                    .is_some_and(|c| c.chains().any(chain))
        })
}

fn term_references_parameter(term: &ast::Term) -> bool {
    let chain = |chain: &ast::Chain| chain.terms.iter().any(term_references_parameter);
    match term {
        ast::Term::Access(access) | ast::Term::Reference(access) => {
            matches!(
                access.source,
                Some(ast::AccessSource::Parameter { depth: 0 })
            )
        }
        ast::Term::State(access, _) => {
            matches!(
                access.source,
                Some(ast::AccessSource::Parameter { depth: 0 })
            )
        }
        ast::Term::Tuple(tuple) => tuple.fields.iter().any(|field| match &field.value {
            ast::FieldValue::Chain(c) => chain(c),
            // A sourced spread reads its access: `...$k` is an own-parameter read.
            ast::FieldValue::Spread(source) => source.as_ref().is_some_and(|access| {
                matches!(
                    access.source,
                    Some(ast::AccessSource::Parameter { depth: 0 })
                )
            }),
        }),
        ast::Term::String(_, segments) => segments.iter().any(|segment| match segment {
            ast::StrSegment::Hole(block) => block_references_parameter(block),
            ast::StrSegment::Text(_) => false,
        }),
        ast::Term::Block(block) => block_references_parameter(block),
        // A nested function literal's `$` is its own parameter.
        ast::Term::Function(_) => false,
        ast::Term::Apply(access, argument) => {
            matches!(
                access.source,
                Some(ast::AccessSource::Parameter { depth: 0 })
            ) || term_references_parameter(argument)
        }
        ast::Term::Spawn(inner, argument, _) => {
            term_references_parameter(inner)
                || argument.as_deref().is_some_and(term_references_parameter)
        }
        ast::Term::Select(Some(sources), _) => sources.iter().any(chain),
        ast::Term::Literal(_)
        | ast::Term::Match(_)
        | ast::Term::Self_
        | ast::Term::Select(None, _)
        | ast::Term::Process(_)
        | ast::Term::Dialect(_) => false,
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::VariableUndefined(name) => write!(f, "Undefined variable: {name}"),
            Error::ParameterDepthExceeded { written } => write!(
                f,
                "'{written}' reaches above the outermost function: each '$' names one enclosing function"
            ),
            Error::Noted { error, note } => write!(f, "{error} ({note})"),
            Error::BuiltinUndefined(name) => {
                write!(f, "unknown builtin `__{name}__`")
            }
            Error::FunctionUndefined(index) => write!(f, "Undefined function: {index}"),
            Error::TypeUnresolved(name) => write!(f, "Unresolved type: {name}"),
            Error::TypeAliasMissing(name) => write!(f, "Unknown type alias: {name}"),
            Error::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {expected}, found {found}")
            }
            Error::NonExhaustiveReturn {
                unhandled,
                declared,
            } => {
                write!(
                    f,
                    "Match is not exhaustive: {unhandled} is unhandled, but the return type \
                     {declared} does not allow []. Handle it, or declare -> ({declared} | [])."
                )
            }
            Error::TypeArgumentsNotApplicable { target } => {
                write!(
                    f,
                    "Explicit type arguments need a function with declared type parameters \
                     (`#<'t, …>`) whose definition is statically known — a declared boundary \
                     (e.g. a parameter's written type) sheds them; found {target}"
                )
            }
            Error::TypeArgumentsTooMany { declared, given } => {
                write!(
                    f,
                    "Too many type arguments: the function declares {declared} type \
                     parameter{}, but {given} were given",
                    if *declared == 1 { "" } else { "s" }
                )
            }
            Error::TypeArgumentsRequired { builtin, declared } => {
                write!(
                    f,
                    "`__{builtin}__` consumes its type argument{} at runtime, so the call \
                     must instantiate it explicitly: `__{builtin}__<'t>`  ({declared} \
                     declared)",
                    if *declared == 1 { "" } else { "s" }
                )
            }
            Error::TypeArgumentNotConcrete { builtin } => {
                write!(
                    f,
                    "`__{builtin}__` needs a concrete type argument — one still containing \
                     type variables has no runtime representation to embed"
                )
            }
            Error::TupleNotInRegistry { tuple_id } => {
                write!(f, "Tuple type {tuple_id} not in registry")
            }
            Error::FieldDuplicated(name) => write!(f, "Duplicate field: {name}"),
            Error::TupleFieldTypeUnresolved { field_index } => {
                write!(f, "Unresolved type for field {field_index}")
            }
            Error::FieldNotFound { field_name, .. } => write!(f, "No field named '{field_name}'"),
            Error::FieldAccessOnNonTuple { field_name } => {
                write!(f, "Cannot access field '{field_name}' on a non-tuple value")
            }
            Error::PositionalAccessOnNonTuple { index } => {
                write!(f, "Cannot access position {index} on a non-tuple value")
            }
            Error::MemberFieldNotFound { field_name, target } => {
                write!(f, "No field '{field_name}' on {target}")
            }
            Error::MemberAccessOnNonTuple { target } => {
                write!(f, "Cannot access a field on {target} (not a tuple)")
            }
            Error::OrPatternBindingMismatch { expected, found } => {
                write!(
                    f,
                    "Alternatives of an or-pattern must bind the same variables (expected {expected:?}, found {found:?})"
                )
            }
            Error::PositionalIndexOutOfBounds { index } => {
                write!(f, "Positional index {index} out of bounds")
            }
            Error::PositionalAccessOnPartial { index } => {
                write!(
                    f,
                    "Cannot access position {index} through a partial type (its fields are only known by name)"
                )
            }
            Error::OperatorTypeNotInRegistry { tuple_id } => {
                write!(f, "Operator type {tuple_id} not in registry")
            }
            Error::OperatorOnNonTuple { operator } => {
                write!(f, "Operator '{operator}' requires a tuple operand")
            }
            Error::ModuleLoad(error) => write!(f, "Module load error: {error:?}"),
            Error::ModuleParse { module, error } => {
                write!(f, "Parse error in module '{module}': {error}")
            }
            Error::ModuleExecution { module, error } => {
                write!(
                    f,
                    "Execution error in module '{module}': {}",
                    error.crash_message()
                )
            }
            Error::ModuleTypeMissing { type_name, module } => {
                write!(f, "Type '{type_name}' not found in module '{module}'")
            }
            Error::ModuleTypeCycle(module) => {
                write!(
                    f,
                    "Cyclic module type reference involving module '{module}'"
                )
            }
            Error::DialectMissing { module } => {
                write!(
                    f,
                    "Module '{module}' does not define a dialect (no :dialect annotation)"
                )
            }
            Error::DialectFailed { module, message } => {
                write!(f, "Dialect {module} {message}")
            }
            Error::FeatureUnsupported(what) => write!(f, "Unsupported: {what}"),
            Error::DestructuringOnNonTuple(ty) => {
                write!(f, "Cannot destructure non-tuple value of type {ty}")
            }
            Error::DestructuringFieldMissing {
                type_name,
                field_name,
            } => write!(
                f,
                "Field '{field_name}' missing when destructuring {type_name}"
            ),
            Error::PatternNoMatchingTypes { pattern } => {
                write!(f, "Pattern '{pattern}' matches no possible type")
            }
            Error::FallibleMatchNotChainFinal => {
                write!(
                    f,
                    "A fallible match must be the last term of its chain: nothing \
                     short-circuits within a chain, so a following term would run \
                     whether or not the match succeeded. Separate the steps (`=P; ...`) \
                     so the match gates what follows, or test for a failed match with a \
                     block (`{{ =P => [] | Ok }}`)"
                )
            }
            Error::FallibleMatchBindingsInValueChain { bindings } => {
                write!(
                    f,
                    "A fallible match binding {} cannot appear in a value position (a \
                     tuple field, an argument, an annotation value): nothing gates on \
                     its verdict there, so the bindings cannot be relied on. Bind in a \
                     preceding step instead",
                    bindings
                        .iter()
                        .map(|b| format!("'{b}'"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Error::AssertionBindings { bindings } => {
                write!(
                    f,
                    "An assertion pattern cannot bind {}: an assertion only observes the \
                     step's value (and release builds skip it entirely). Test with \
                     literals, types and pins, or bind in the step itself",
                    bindings
                        .iter()
                        .map(|b| format!("'{b}'"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Error::UnionApplication {
                union,
                all_functions,
            } => {
                if *all_functions {
                    write!(
                        f,
                        "Cannot call {union}: a union of function types cannot be \
                         applied (the members are separate functions). Narrow the union \
                         first, or reference the value with '&'"
                    )
                } else {
                    write!(
                        f,
                        "Cannot pipe a value into {union}: the union mixes process or \
                         function members with other values, so the pipe would be a \
                         send for some members and a replace for others. Narrow the \
                         union first, or reference the value with '&' (a union of only \
                         process types is sendable)"
                    )
                }
            }
            Error::InternalError { message } => write!(f, "Internal compiler error: {message}"),
        }
    }
}

/// A compiler [`Error`] annotated with the source span where it occurred (when known), so
/// the language server can place type-error diagnostics precisely. `compile` callers that
/// don't care about the span just read `.error`.
///
/// A partial semantic index (for hover/go-to-definition on the parts of a file that compiled
/// before the error) does not live here: the recorder is caller-owned (passed to
/// [`Compiler::compile`]), so the caller still has it — and the program it indexes — after a
/// failed compile.
#[derive(Debug)]
pub struct LocatedError {
    pub error: Error,
    pub span: Option<SourceSpan>,
}

impl From<Error> for LocatedError {
    fn from(error: Error) -> Self {
        Self { error, span: None }
    }
}

impl From<LocatedError> for Error {
    fn from(located: LocatedError) -> Self {
        located.error
    }
}

impl std::fmt::Display for LocatedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.error)
    }
}

/// Compiler knowledge that outlives one compilation: what has been learned about values
/// already compiled, keyed by ids in the `Program`'s id space.
///
/// A file is one compilation, so these never need to leave it. A REPL entry is *its own*
/// compilation over a persistent program, and a function defined in an earlier entry is
/// reachable by name in a later one — so without carrying these, that function is compiled as
/// though nothing were known about it: an explicit instantiation (`f<'int>`) fails, and a
/// dispatch function's call-site result widens to its whole union. Imported modules carry the
/// same tables through the module cache, which is why only *entry-level* definitions are
/// affected.
///
/// Their keys stay unique across entries because the uniquifying suffix is minted from
/// `program.type_count()` (see `mint_type_param_suffix`), and the program persists.
#[derive(Debug, Clone, Default)]
pub struct SessionTables {
    /// Per-definition dispatch tables, keyed by function index.
    pub fn_case_tables: HashMap<usize, Vec<(usize, usize)>>,
    /// Callable type id → the function index whose dispatch table to use.
    pub case_tables: HashMap<usize, usize>,
    /// Callable type id → its declared type parameters, in declaration order.
    pub callable_type_params: HashMap<usize, Vec<String>>,
}

/// The products of a successful compilation. The caller-owned `program`, `module_cache`, and
/// (optional) semantic recorder are borrowed by [`Compiler::compile`] and mutated in place,
/// so they are not returned here — only the genuine outputs are.
pub struct Compiled {
    pub instructions: Vec<Instruction>,
    pub result_type: usize,
    pub receive_type: usize,
    pub bindings: Bindings,
    /// What this compilation learned, for a caller that will compile again against the same
    /// program (the REPL). A one-shot caller drops it.
    pub tables: SessionTables,
}

/// Compilation mode options.
#[derive(Debug, Clone)]
pub struct CompileOptions {
    /// Debug build: emit failure-provenance stamps, so nil results carry an `origin`
    /// annotation naming the source position that produced them. Zero cost when off.
    pub debug: bool,
    /// The display name provenance sites use for top-level code (the source file name,
    /// `"repl"`, ...); imported modules use their own module ids.
    pub source_name: String,
    /// Step budget for compile-time evaluation (module bodies, dialect expansion): the
    /// bound that turns an infinite loop at a module's top level into a compile error
    /// instead of a hang.
    pub fuel: u64,
    /// Cooperative cancellation for compile-time evaluation, polled between execution
    /// slices; a host sets it to abandon a compilation in flight.
    pub cancel: Option<std::sync::Arc<std::sync::atomic::AtomicBool>>,
}

/// The default compile-time evaluation budget: far above what any reasonable module
/// top level uses (std's heaviest evaluate in a few million units), while bounding a
/// runaway one to seconds rather than forever.
pub const DEFAULT_COMPILE_FUEL: u64 = 1_000_000_000;

impl Default for CompileOptions {
    fn default() -> Self {
        CompileOptions {
            debug: false,
            source_name: "main".to_string(),
            fuel: DEFAULT_COMPILE_FUEL,
            cancel: None,
        }
    }
}

/// Context for ripple operator (~) usage
/// Tracks the value being rippled and where it is on the stack
struct RippleContext {
    /// Type ID of the value being rippled
    value_type_id: usize,
    /// Stack offset where the ripple value is located
    stack_offset: usize,
    /// Whether this context owns the value and must clean it up
    owns_value: bool,
    /// Provenance of the rippled value for type narrowing
    provenance: Provenance,
}

/// The value flowing into a term: its type (absent when no value is on the stack) paired with the
/// provenance used for type narrowing. Bundled so `compile_term` takes the incoming value as one
/// argument.
struct FlowingValue {
    ty: Option<usize>,
    provenance: Provenance,
}

/// Per-branch dispatch data collected while compiling a function body that is a pure pattern
/// dispatch on its parameter. `branches` pairs each branch's parameter *guard type* (the
/// parameter values that reach it) with that branch's inferred result type. `valid` is cleared
/// if any branch is not a pure parameter dispatch, in which case no case table is recorded.
struct DispatchCollection {
    branches: Vec<(usize, usize)>,
    valid: bool,
}

/// Whether a branch's condition is a pure pattern match against the flowing parameter — i.e.
/// the set of parameter values that take it is captured exactly by type narrowing (no
/// computation that could fail for non-type reasons, and no extra chains). Such branches can
/// contribute to a function's call-site dispatch table. A condition declaring a type alias is
/// not one: the alias has no runtime effect, but `single_chain` is deliberately conservative —
/// dispatch is an optimisation, so a rarer shape is better skipped than reasoned about.
fn branch_is_parameter_dispatch(branch: &ast::Branch) -> bool {
    branch
        .condition
        .single_chain()
        .is_some_and(|chain| chain.terms.iter().all(|t| matches!(t, ast::Term::Match(_))))
}

/// Whether falling through this branch PROVES its pattern didn't match — the soundness
/// condition for recording the pattern's complement (and its coverage) for subsequent
/// branches. That holds only when the condition is a single chain whose final failable
/// element is the pattern itself: a guard step after the match (`=P; G => …`) can fail
/// with the pattern matched, and a step before it (`G; =P => …`) can short-circuit
/// without the pattern ever being tested — either way the fall-through says nothing
/// about the pattern, so no complement may be recorded. A `~>`-joined term after a
/// match (`=P ~> G`) likewise makes the condition's verdict G's, not the pattern's.
fn condition_complement_faithful(condition: &ast::Sequence) -> bool {
    let Some(chain) = condition.single_chain() else {
        return false;
    };
    let match_terms = chain
        .terms
        .iter()
        .filter(|t| matches!(t, ast::Term::Match(_)))
        .count();
    if chain.binding.is_some() {
        match_terms == 0
    } else {
        match_terms == 0
            || (match_terms == 1 && matches!(chain.terms.last(), Some(ast::Term::Match(_))))
    }
}

/// The function index of a resolved value, when it is (directly) a function. Used at a call
/// site to select the callee's dispatch table exactly, by identity rather than by its (possibly
/// shared) callable type.
fn value_fn_index(value: &Value) -> Option<usize> {
    match value {
        Value::Function(index, _) => Some(*index),
        _ => None,
    }
}

/// The reconstruction-CSE key for a value, or `None` for values too cheap to share
/// (their build is a single instruction — a `Store`/`Load` pair would only add weight).
/// `Value` equality would be the wrong identity here — annotations are invisible to it —
/// so the key is the payload's pointer (see [`scopes::CseKey`]).
fn cse_key(value: &Value) -> Option<scopes::CseKey> {
    let (kind, id, payload) = match value {
        Value::Tuple(tuple_id, payload)
            if !payload.is_empty() || !payload.annotations().is_empty() =>
        {
            (scopes::CseKind::Tuple, *tuple_id, payload)
        }
        Value::Function(function, payload)
            if !payload.is_empty() || !payload.annotations().is_empty() =>
        {
            (scopes::CseKind::Function, *function, payload)
        }
        Value::Builtin(builtin_id, Some(payload)) if !payload.annotations().is_empty() => {
            (scopes::CseKind::Builtin, *builtin_id, payload)
        }
        _ => return None,
    };
    Some(scopes::CseKey {
        kind,
        id,
        payload: std::rc::Rc::clone(payload),
    })
}

/// Whether hoisting a compile-time-known value to a capture slot pays its way. A primitive,
/// a bare or merely-instantiated builtin, and an empty-payload tuple or function all
/// reconstruct as a single allocation-free instruction — a slot would only add capture
/// weight. Everything else (captures, fields, annotations) allocates on every rebuild.
fn worth_hoisting(value: &Value) -> bool {
    match value {
        Value::Int(_) | Value::BigInt(_) | Value::Binary(_) | Value::Reference(_) => false,
        Value::Builtin(_, payload) => payload
            .as_ref()
            .is_some_and(|payload| !payload.annotations().is_empty()),
        Value::Tuple(_, payload) | Value::Function(_, payload) => {
            !payload.is_empty() || !payload.annotations().is_empty()
        }
        Value::Process(..) | Value::Resource(..) => false,
    }
}

/// The leading pattern of a branch condition (its binding `binding`, or a first
/// `=pattern` term), through which the branch dispatches on the parameter.
fn leading_match(branch: &ast::Branch) -> Option<&ast::Match> {
    let chain = branch.condition.chains().next()?;
    if let Some(m) = &chain.binding {
        return Some(m);
    }
    match chain.terms.first() {
        Some(ast::Term::Match(m)) => Some(m),
        _ => None,
    }
}

pub struct Compiler<'a, E: quiver_core::effects::Effect> {
    // Core components
    codegen: InstructionBuilder,
    // Caller-owned, borrowed for the duration of the compile (see `Compiler::compile`).
    module_cache: &'a mut ModuleCache,

    // State management
    scopes: Vec<Scope>,
    local_count: usize,
    // Caller-owned; the caller keeps it after the compile (success or failure) to read the
    // type registry the semantic recorder's type-ids point into.
    program: &'a mut Program,
    resolver: &'a dyn ModuleResolver,
    /// The package whose `modules` rules resolve the imports currently being compiled. Swapped
    /// when descending into an imported module, so each module resolves hermetically against
    /// its own package (see `import_and_cache_module`).
    current_package: PackageId,
    builtins: &'a quiver_core::builtins::BuiltinRegistry<E>,

    process_types: &'a HashMap<usize, (usize, usize)>,

    // Track the receive type ID of the function currently being compiled
    current_receive_type_id: usize,

    /// The states a spawn of the function being compiled moves through: seeded with its
    /// parameter type, widened at each tail call by the target's states. `None` = unknown
    /// (poisoned by a `^~` on an unknown-states callee); baked into the registered
    /// `Callable` like `current_receive_type_id`.
    current_states: Option<usize>,

    // While compiling a function body, collects per-branch (guard, result) types for the
    // call-site return-type dispatch. `None` outside a function body.
    collected_dispatch: Option<DispatchCollection>,

    // Set by the most recent function-body block: the unhandled parameter type when the body is
    // a non-exhaustive enumeration (every branch a variant pattern, but some variant uncovered).
    // Consulted by the return-type check to name unhandled cases. `None` if exhaustive or not an
    // enumeration.
    last_uncovered: Option<usize>,

    // Case tables for functions that dispatch on their parameter: a list of (parameter guard
    // type, branch result type), consulted at call sites to compute a result type from the
    // concrete argument type. Keyed by *function index* (unique per definition), so two
    // structurally-identical dispatch functions (e.g. `num.add` and `num.div`, which share a
    // callable type but have different tables) never clobber each other.
    fn_case_tables: HashMap<usize, Vec<(usize, usize)>>,
    // Maps a callable *type id* to the function whose table to use when the callee isn't
    // statically known at the call site (the common case: a local dispatch function). A type
    // shared by dispatch functions with *differing* tables is ambiguous and absent here; calls
    // through it then rely on the statically-known callee (imports/direct calls carry it).
    // Both survive across module compilation (same Compiler instance).
    case_tables: HashMap<usize, usize>,

    // The declared type parameters of each generic callable type, as its uniquified variable
    // names in declaration order — what an explicit instantiation (`f<'int>`) binds
    // positionally. Keyed by callable type id: distinct definitions mint distinct variable
    // names (per-definition suffix), so an id collision implies an identical entry. Populated
    // at function-literal compilation (declaration order) and builtin resolution
    // (first-occurrence order); cached/restored across modules like the dispatch tables.
    callable_type_params: HashMap<usize, Vec<String>>,

    // Span of the term currently being compiled, so an error can be located in source.
    // Only set when a recorder is interested (LSP); harmless otherwise.
    current_span: Option<SourceSpan>,
    /// The type-parameter uniquifying suffix of the top-level function definition being
    /// compiled, inherited by nested literals (see `compile_function`).
    type_param_suffix: Option<usize>,
    /// One entry per in-progress module compile, innermost last: the module's display
    /// name and its definition counter, from which fresh suffixes mint
    /// deterministically (see `mint_type_param_suffix`). Empty at the entry level.
    suffix_scopes: Vec<(String, usize)>,
    /// Type parameters of *builtin* callables (see `record_builtin_type_params`),
    /// kept apart from `callable_type_params`: they are registry vocabulary,
    /// re-derived by every session on reference, so they must not ride in module
    /// dispatch deltas (membership there would depend on which module referenced the
    /// builtin first — session history an artifact must not contain).
    builtin_type_params: HashMap<usize, Vec<String>>,
    /// How many function-literal bodies the compiler is currently inside. The dialect
    /// pre-expansion walk runs only at depth 0 — the outermost `compile_function` expands
    /// every nested body in one pass, so re-walking per nested literal would be
    /// O(depth × AST) on every compile (reset per module in `import_and_cache_module`).
    function_depth: usize,

    // Debug builds: emit failure-provenance stamps (`Stamp` instructions + the site
    // table) so nil results carry their origin. Zero cost when false.
    debug: bool,
    // The display name provenance sites carry for the current compilation unit: the
    // caller-supplied source name at top level, the module id inside imported modules
    // (swapped in `import_and_cache_module`, like `current_package`).
    current_module: String,

    // Bounds on compile-time evaluation (module bodies, dialect expansion): the step
    // budget, and the host's cooperative cancellation flag. See `CompileOptions`.
    fuel: u64,
    cancel: Option<std::sync::Arc<std::sync::atomic::AtomicBool>>,

    // Opt-in symbol recorder for the language server (hover/definition). `None` for
    // ordinary compilation, so there is no cost. Caller-owned, so the recorded data
    // survives a failed compile.
    recorder: Option<&'a mut Recorder>,

    _phantom: std::marker::PhantomData<E>,
}

/// Collect the name and source span of every binding identifier in a pattern, recursing
/// through tuple and partial sub-patterns. Used by the language server to index pattern
/// bindings (destructuring, mid-chain `=x`, block branches, and bare partial fields like
/// `(double)`) for hover/go-to-definition.
/// Build a hover label from a base symbol and an accessor chain, e.g. `foo` + `[.bar]` →
/// `foo.bar`, `$` + `[.0]` → `$.0`, `%num` + `[.add]` → `%num.add`.
fn accessors_label(base: &str, accessors: &[ast::AccessPath]) -> String {
    let mut label = base.to_string();
    for accessor in accessors {
        match accessor {
            ast::AccessPath::Field(name) => {
                label.push('.');
                label.push_str(name);
            }
            ast::AccessPath::Index(index) => {
                label.push('.');
                label.push_str(&index.to_string());
            }
            ast::AccessPath::Annotation(name, _) => {
                label.push(':');
                label.push_str(name);
            }
        }
    }
    label
}

fn collect_binding_spans(pattern: &ast::Match, out: &mut Vec<(String, SourceSpan)>) {
    match pattern {
        ast::Match::Identifier(name, span) => {
            if let Some(span) = span.get() {
                out.push((name.clone(), span));
            }
        }
        ast::Match::Tuple(tuple) => {
            for field in &tuple.fields {
                collect_binding_spans(&field.pattern, out);
            }
        }
        ast::Match::Partial(partial) => {
            for field in &partial.fields {
                match &field.pattern {
                    // `(x: pattern)` — the binding lives in the nested pattern; `x` only selects.
                    Some(nested) => collect_binding_spans(nested, out),
                    // `(x)` — binds the field by name; index it for go-to-definition.
                    None => {
                        if let Some(span) = field.name_span.get() {
                            out.push((field.name.clone(), span));
                        }
                    }
                }
            }
        }
        ast::Match::Or(alternatives) => {
            // Each alternative binds the same variables; index every occurrence so go-to-definition
            // resolves a binding to the arm it was written in.
            for alternative in alternatives {
                collect_binding_spans(alternative, out);
            }
        }
        ast::Match::As(_, name, span) => {
            // The type-ascribed binder `(T)x` binds `x`; the type part carries no bindings.
            if let Some(span) = span.get() {
                out.push((name.clone(), span));
            }
        }
        _ => {}
    }
}

/// Collect every pin target in a pattern (`&name`, `&$x.y`) for language-server recording —
/// the pattern's read references, the counterpart of `collect_binding_spans`' write sites.
fn collect_pin_targets<'m>(pattern: &'m ast::Match, out: &mut Vec<&'m ast::PinTarget>) {
    match pattern {
        ast::Match::Reference(target) => out.push(target),
        ast::Match::Tuple(tuple) => {
            for field in &tuple.fields {
                collect_pin_targets(&field.pattern, out);
            }
        }
        ast::Match::Partial(partial) => {
            for field in &partial.fields {
                if let Some(nested) = &field.pattern {
                    collect_pin_targets(nested, out);
                }
            }
        }
        ast::Match::Or(alternatives) => {
            for alternative in alternatives {
                collect_pin_targets(alternative, out);
            }
        }
        _ => {}
    }
}

impl<'a, E: quiver_core::effects::Effect> Compiler<'a, E> {
    /// Compile a program. `program` and `module_cache` are caller-owned and mutated in
    /// place — the caller keeps them after the call returns, whether it succeeds or fails.
    /// Pass a `recorder` to build the span→semantics index for the language server (hover,
    /// go-to-definition); `None` runs ordinary compilation, which pays nothing. On error the
    /// returned [`LocatedError`] carries the source span; the partial index lives in the
    /// caller's `recorder`, which still points into the caller's `program`.
    #[allow(clippy::too_many_arguments)]
    #[allow(clippy::too_many_arguments)]
    pub fn compile(
        ast_program: ast::Sequence,
        existing_bindings: &Bindings,
        // What earlier compilations against this program learned; `Default::default()` for a
        // one-shot compile.
        tables: SessionTables,
        module_cache: &'a mut ModuleCache,
        resolver: &'a dyn ModuleResolver,
        program: &'a mut Program,
        parameter_type_id: usize,
        process_types: &'a HashMap<usize, (usize, usize)>,
        builtins: &'a quiver_core::builtins::BuiltinRegistry<E>,
        recorder: Option<&'a mut Recorder>,
        options: CompileOptions,
    ) -> Result<Compiled, LocatedError> {
        let never_id = program.never();
        let current_package = resolver.entry_package();

        let mut compiler = Self {
            codegen: InstructionBuilder::new(),
            module_cache,
            scopes: vec![],
            local_count: 0,
            program,
            resolver,
            current_package,
            builtins,
            process_types,
            current_receive_type_id: never_id,
            current_states: None,
            collected_dispatch: None,
            last_uncovered: None,
            fn_case_tables: tables.fn_case_tables,
            case_tables: tables.case_tables,
            callable_type_params: tables.callable_type_params,
            current_span: None,
            type_param_suffix: None,
            suffix_scopes: Vec::new(),
            builtin_type_params: HashMap::new(),
            function_depth: 0,
            debug: options.debug,
            current_module: options.source_name,
            fuel: options.fuel,
            cancel: options.cancel,
            recorder,
            _phantom: std::marker::PhantomData,
        };

        // Prepare scope bindings from existing bindings
        let scope_bindings = existing_bindings.clone();

        // Calculate local_count from variables
        compiler.local_count = existing_bindings
            .variables
            .values()
            .map(|variable| variable.index + 1)
            .max()
            .unwrap_or(0);

        // Drop blocks that carry no runtime meaning before codegen: a single branchless branch with
        // no bindings needs no frame (whatever its step count) and acts only as a narrowing barrier,
        // so removing it elides wasted Store/Load/Reset. `lift` also unwraps multi-step such blocks.
        // The formatter strips/keeps the same blocks (it keeps multi-step ones as grouping braces,
        // which compile identically here), so formatting never changes compiled output.
        let ast_program = crate::simplify::normalize_blocks(
            ast_program,
            &crate::simplify::Options {
                keep: &|_| false,
                lift: true,
                group_consequences: false,
            },
        );

        // `parameter_type_id` indexes the *type* table. A caller passing a tuple id instead
        // (`types::NIL` is the nil tuple, not the nil type) would silently type every
        // top-level step's input as whatever type happened to land at that index, so reject
        // an id no type occupies rather than miscompile.
        if compiler.program.lookup_type(parameter_type_id).is_none() {
            return Err(LocatedError {
                error: Error::InternalError {
                    message: format!("parameter_type_id {parameter_type_id} is not a type id"),
                },
                span: None,
            });
        }

        // Only allocate a parameter slot if there are chain steps; a program of nothing but
        // type aliases produces no value and needs no parameter.
        let has_chains = ast_program.chains().next().is_some();

        let scope_parameter = if has_chains {
            let param_local = compiler.local_count;
            compiler.local_count += 1;

            compiler.codegen.add_instruction(Instruction::store());

            Some(scopes::Parameter {
                ty: parameter_type_id,
                index: param_local,
                provenance: Provenance::Parameter,
            })
        } else {
            None
        };

        // Initialize scope with bindings and optional parameter
        compiler.scopes = vec![Scope::new(scope_bindings, scope_parameter, ScopeKind::Root)];

        // Link-before-compile: every module the program references compiles to
        // completion before the program's own body (see `precompile_imports`). This
        // must precede receive-type extraction: extraction resolves module types, and
        // resolving one before its module is linked would build the namespace from
        // source only for the link to overwrite it.
        let entry_package = compiler.current_package.clone();
        if let Err(error) = compiler.precompile_imports(&ast_program, &entry_package) {
            return Err(LocatedError {
                error,
                span: compiler.current_span,
            });
        }

        // Extract receive type from the program's steps (like we do for function bodies)
        // This seeds the receive type from explicit selects in the code.
        // Additional receive types may be adopted during compilation when calling
        // functions that have receive types.
        compiler.current_receive_type_id =
            compiler.extract_receive_type_from_steps(&ast_program.steps)?;

        // The recorder is caller-owned, so whatever it gathered before an error (and the program it
        // indexes) is still available to the caller for hover/go-to-definition on the parts that
        // compiled.
        let result_type_id = match compiler.compile_top_level(ast_program.steps) {
            Ok(ty) => ty,
            Err(error) => {
                return Err(LocatedError {
                    error,
                    span: compiler.current_span,
                });
            }
        };

        // Extract bindings from the global scope
        let bindings = Bindings {
            variables: compiler.scopes[0]
                .bindings
                .variables
                .iter()
                .filter(|(name, _)| !name.starts_with('~')) // Exclude internal variables
                .map(|(name, variable)| (name.clone(), variable.clone()))
                .collect(),
            type_aliases: compiler.scopes[0].bindings.type_aliases.clone(),
        };

        Ok(Compiled {
            instructions: compiler.codegen.instructions,
            result_type: result_type_id,
            // Use the final receive type, which may have been widened during compilation
            // when calling functions that have receive types
            receive_type: compiler.current_receive_type_id,
            bindings,
            tables: SessionTables {
                fn_case_tables: compiler.fn_case_tables,
                case_tables: compiler.case_tables,
                callable_type_params: compiler.callable_type_params,
            },
        })
    }

    /// Record a reference to a named symbol (variable) at `span` for the LSP, when recording.
    /// Definition resolution uses `name`; `label` is the hover text (the full access path,
    /// e.g. `foo.bar`).
    fn record_reference(
        &mut self,
        span: Option<SourceSpan>,
        name: &str,
        label: String,
        type_id: usize,
    ) {
        if let (Some(recorder), Some(span)) = (self.recorder.as_deref_mut(), span) {
            recorder.record_reference(span, name, type_id, SymbolKind::Variable, Some(label));
        }
    }

    /// Record a reference that has no named definition (`$`, builtins) at `span`, with an
    /// optional hover label.
    fn record_typed(
        &mut self,
        span: Option<SourceSpan>,
        type_id: usize,
        kind: SymbolKind,
        label: Option<String>,
    ) {
        if let (Some(recorder), Some(span)) = (self.recorder.as_deref_mut(), span) {
            recorder.record_typed(span, type_id, kind, label);
        }
    }

    /// Record an import reference at `span`, carrying the module's origin file (when openable)
    /// so the language server can offer cross-file go-to-definition, and the accessed `member`
    /// (`%util.double` → `"double"`) so it can find references to an imported symbol.
    fn record_import(
        &mut self,
        span: Option<SourceSpan>,
        type_id: usize,
        label: Option<String>,
        origin: ModuleOrigin,
        module_name: &[String],
        accessors: &[ast::AccessPath],
    ) {
        if let (Some(recorder), Some(span)) = (self.recorder.as_deref_mut(), span) {
            let definition_module = match origin {
                ModuleOrigin::Path(path) => Some(path),
                ModuleOrigin::Virtual => None,
            };
            let member = match accessors.first() {
                Some(ast::AccessPath::Field(name)) => Some(name.clone()),
                _ => None,
            };
            recorder.record_import(
                span,
                type_id,
                label,
                definition_module,
                module_name.to_vec(),
                member,
            );
        }
    }

    /// Record that `name` is defined at `span` (a binding site).
    fn record_definition(&mut self, name: &str, span: Option<SourceSpan>) {
        if let (Some(recorder), Some(span)) = (self.recorder.as_deref_mut(), span) {
            recorder.record_definition(name, span);
        }
    }

    /// Compile a program/module body. The top level *is* a sequence — chain steps threading and
    /// short-circuiting on nil, with type-alias declarations interspersed and transparent to that
    /// flow — so this is [`Self::compile_sequence`] over the whole body. Bindings persist across
    /// it. Leaves the final value on the stack and returns its type (nil if there are no chains).
    fn compile_top_level(&mut self, steps: Vec<ast::Step>) -> Result<usize, Error> {
        if !steps.iter().any(|s| matches!(s, ast::Step::Chain(_))) {
            // A body of only type aliases produces no value: register the aliases and answer nil.
            for step in steps {
                let ast::Step::TypeAlias {
                    name,
                    type_parameters,
                    type_definition,
                    ..
                } = step
                else {
                    unreachable!("checked for chain steps above")
                };
                self.compile_type_alias(name.as_deref(), type_parameters, type_definition)?;
            }
            return Ok(self.program.register_type(Type::nil()));
        }
        let (ty, _prov) = self.compile_sequence(ast::Sequence { steps }, None, None)?;
        Ok(ty)
    }

    /// Compile a type alias. `name` is `None` for the module's nameless default-type
    /// marker (`' = ...`), which is resolved for validation but not bound locally — it is
    /// only reachable from other modules as `'%mod`.
    fn compile_type_alias(
        &mut self,
        name: Option<&str>,
        type_parameters: Vec<String>,
        type_definition: ast::Type,
    ) -> Result<(), Error> {
        // Prevent shadowing primitive types
        if let Some(name) = name
            && helpers::is_reserved_name(name)
        {
            return Err(Error::TypeUnresolved(format!(
                "Cannot redefine primitive type '{}'",
                name
            )));
        }

        // Validate the AST structure
        Self::validate_type_ast(&type_definition)?;

        // Create Type::Variable bindings for type parameters
        let mut bindings = HashMap::new();
        for param in &type_parameters {
            let var_type_id = self.program.register_type(Type::Variable(param.clone()));
            bindings.insert(param.clone(), var_type_id);
        }

        // Resolve the type definition immediately
        let mut env = typing::TypeEnv {
            resolver: self.resolver,
            module_cache: &mut *self.module_cache,
            package: &self.current_package,
        };
        let type_id = typing::resolve_ast_type_with_bindings(
            &mut env,
            &self.scopes,
            type_definition,
            self.program,
            &bindings,
        )?;

        // Store the resolved alias. The nameless default marker is bound under the reserved
        // self-default key, so a bare `'` elsewhere in the module resolves to it.
        let key = match name {
            Some(name) => name.to_string(),
            None => typing::SELF_DEFAULT_KEY.to_string(),
        };
        scopes::define_type_alias(
            &mut self.scopes,
            key,
            TypeAliasDef {
                parameters: type_parameters,
                type_id,
            },
        );
        Ok(())
    }

    /// Compile an annotation prefix onto the carrier value currently on top of the stack.
    /// Each annotation value is an ordinary chain with nil input, evaluated in the current
    /// (enclosing) scope; `Annotate` then copies the carrier with the annotation attached.
    ///
    /// Returns the carrier's type with the row recorded: entry types are the *inferred*
    /// value types. `exact_carrier` is set when the carrier is known freshly built with
    /// exactly this prefix (a function literal); a block's result keeps its own members'
    /// exactness (plain members stay open — their annotation state is unknown).
    fn compile_annotation_attach(
        &mut self,
        annotation_list: Vec<ast::Annotation>,
        carrier_type: usize,
        exact_carrier: bool,
    ) -> Result<usize, Error> {
        let mut seen = std::collections::HashSet::new();
        for annotation in &annotation_list {
            if !seen.insert(annotation.name.clone()) {
                return Err(Error::TypeUnresolved(format!(
                    "Duplicate annotation :{}",
                    annotation.name
                )));
            }
        }
        // A bare type *variable* carrier is permitted, deferring the carrier check to the
        // runtime (`Annotate` fails fast on a value that cannot hold one). Generic code
        // that stamps metadata onto a value it did not construct — the live layer joining
        // a browser event to a decoded payload, a helper marking a state it was handed —
        // has no other attach site, and a static check on an unpinned variable can only
        // ever reject. The row is *not* extended in that case: nothing is provably visible
        // through the variable, so the entry reads back only through a checked retrieval,
        // exactly as a runtime-attached annotation does.
        let opaque_carrier = matches!(
            self.program.lookup_type(carrier_type),
            Some(quiver_core::types::Type::Variable(_))
        );
        if !opaque_carrier && !annotations::is_annotatable(self.program, carrier_type) {
            return Err(Error::TypeUnresolved(format!(
                "Annotations require a tuple or function carrier, but the annotated value has type {}",
                quiver_core::format::format_type_by_id(&*self.program, carrier_type)
            )));
        }
        let mut entries: Vec<(usize, usize)> = Vec::new();
        for annotation in annotation_list {
            let key_id = annotations::intern_key(self.program, &annotation.name);
            let is_defaults = annotation.name == annotations::DEFAULTS;
            // The builtin keys keep an expected type — `doc` for sanity, `pre`/`post` so
            // a bare `#{ ... }` contract can infer its parameter from the carrier.
            let expected = match annotation.name.as_str() {
                "doc" => Some(annotations::str_type(self.program)),
                annotations::PRE | annotations::POST => {
                    let (parameter, result) =
                        annotations::single_callable(self.program, carrier_type).ok_or_else(
                            || {
                                Error::TypeUnresolved(format!(
                                    "Annotation :{} can only be attached to a function",
                                    annotation.name
                                ))
                            },
                        )?;
                    Some(annotations::contract_type(
                        self.program,
                        &annotation.name,
                        parameter,
                        result,
                    ))
                }
                _ => None,
            };
            // The annotation value is a chain with nil input (it may draw on lexical scope,
            // but there is no meaningful flowing value at attach time).
            self.codegen.add_instruction(Instruction::tuple(NIL));
            let closed_nil = annotations::closed_nil(self.program);
            let (value_type, _) = self.compile_chain_with_input(
                annotation.value,
                None,
                None,
                Some((closed_nil, Provenance::Unknown)),
                None,
                false,
                expected,
                false, // an annotation value is data; nothing gates on it
            )?;
            if let Some(expected) = expected
                && !quiver_core::types::is_compatible(value_type, expected, &*self.program)
            {
                return Err(Error::TypeMismatch {
                    expected: quiver_core::format::format_type_by_id(&*self.program, expected),
                    found: quiver_core::format::format_type_by_id(&*self.program, value_type),
                });
            }
            // `:defaults` has no single expected type — it names a *subset* of the
            // parameter's fields — so it is checked against the carrier after the fact.
            if is_defaults {
                annotations::check_defaults(&*self.program, carrier_type, value_type)?;
            }
            self.codegen.add_instruction(Instruction::annotate(key_id));
            entries.push((key_id, value_type));
        }
        if opaque_carrier {
            return Ok(carrier_type);
        }
        Ok(self
            .program
            .annotate_type(carrier_type, exact_carrier, entries))
    }

    fn validate_type_ast(ast_type: &ast::Type) -> Result<(), Error> {
        match ast_type {
            ast::Type::Tuple(tuple) => {
                // Validate partial types have all named fields
                if tuple.is_partial {
                    let has_unnamed = tuple
                        .fields
                        .iter()
                        .any(|f| matches!(f, ast::FieldType::Field { name: None, .. }));
                    let has_named = tuple
                        .fields
                        .iter()
                        .any(|f| matches!(f, ast::FieldType::Field { name: Some(_), .. }));

                    if has_unnamed && has_named {
                        return Err(Error::TypeUnresolved(
                            "All fields in a partial type must be named".to_string(),
                        ));
                    }
                }
                // Recursively validate field types. A default surviving to here is one
                // written outside a function literal's parameter spelling — a type alias, a
                // function type, a nested tuple — where it belongs to no function and so
                // could never fire. `compile_function` strips the ones it consumes.
                for field in &tuple.fields {
                    if let ast::FieldType::Field {
                        type_def, default, ..
                    } = field
                    {
                        if default.is_some() {
                            return Err(Error::TypeUnresolved(
                                "A field default is only allowed in a function literal's \
                                 parameter type, where it attaches to that function"
                                    .to_string(),
                            ));
                        }
                        Self::validate_type_ast(type_def)?;
                    }
                }
                Ok(())
            }
            ast::Type::Function(func) => {
                Self::validate_type_ast(&func.input)?;
                Self::validate_type_ast(&func.output)
            }
            ast::Type::Process(proc) => {
                if let Some(receive) = &proc.receive_type {
                    Self::validate_type_ast(receive)?;
                }
                if let Some(returns) = &proc.return_type {
                    Self::validate_type_ast(returns)?;
                }
                Ok(())
            }
            ast::Type::Union(union) => {
                for variant in &union.types {
                    Self::validate_type_ast(variant)?;
                }
                Ok(())
            }
            ast::Type::Intersection(members) => {
                for member in members {
                    Self::validate_type_ast(member)?;
                }
                Ok(())
            }
            ast::Type::Identifier { arguments, .. }
            | ast::Type::ModuleType { arguments, .. }
            | ast::Type::SelfDefault { arguments } => {
                for arg in arguments {
                    Self::validate_type_ast(arg)?;
                }
                Ok(())
            }
            ast::Type::Primitive(_) | ast::Type::Cycle(_) | ast::Type::Resource(_) => Ok(()),
        }
    }

    // Helper methods for type operations on type IDs

    /// Check if a type ID represents the never type (empty union)
    fn is_never(&self, type_id: usize) -> bool {
        self.program
            .lookup_type(type_id)
            .map(|t| t.is_never())
            .unwrap_or(false)
    }

    /// Check if a type ID represents the nil type (seeing through annotation rows —
    /// annotated nil is nil for control flow).
    fn is_nil(&self, type_id: usize) -> bool {
        self.program
            .lookup_type(type_id)
            .map(|t| t.is_nil_deep(&*self.program))
            .unwrap_or(false)
    }

    /// Check if a type ID contains nil (either is nil or is a union containing nil)
    fn contains_nil(&self, type_id: usize) -> bool {
        self.program
            .lookup_type(type_id)
            .map(|t| t.contains_nil(&*self.program))
            .unwrap_or(false)
    }

    /// Get type without nil variants
    fn without_nil(&mut self, type_id: usize) -> usize {
        if let Some(ty) = self.program.lookup_type(type_id) {
            let without_nil = ty.without_nil(&*self.program);
            self.program.register_type(without_nil)
        } else {
            self.program.never()
        }
    }

    fn compile_literal(&mut self, literal: ast::Literal) -> Result<usize, Error> {
        match literal {
            ast::Literal::Integer(integer) => {
                let index = self.program.register_constant(Constant::Integer(integer));
                self.codegen.add_instruction(Instruction::constant(index));
                Ok(self.program.register_type(Type::Integer))
            }
            ast::Literal::Binary(bytes) => {
                let index = self
                    .program
                    .register_constant(Constant::Binary(bytes.clone()));
                self.codegen.add_instruction(Instruction::constant(index));
                Ok(self.program.register_type(Type::Binary))
            }
        }
    }

    /// The static type of a spread source access, resolved without emitting code: through a
    /// capture where the whole path is one, else the base plus an accessor walk. Ripple
    /// sources read the flowing value's type.
    fn peek_spread_source_type(
        &mut self,
        access: &ast::Access,
        ripple_context: Option<&RippleContext>,
    ) -> Option<usize> {
        match &access.source {
            Some(ast::AccessSource::Identifier(name)) => {
                scopes::lookup_variable(&self.scopes, name, &access.accessors)
                    .map(|(ty, _)| ty)
                    .or_else(|| {
                        let (base, _) = scopes::lookup_variable(&self.scopes, name, &[])?;
                        self.peek_accessor_type(base, &access.accessors, name).ok()
                    })
            }
            Some(ast::AccessSource::Parameter { depth: 0 }) => {
                let (base, _) = scopes::get_function_parameter(&self.scopes).ok()?;
                self.peek_accessor_type(base, &access.accessors, "$").ok()
            }
            Some(ast::AccessSource::Parameter { depth }) => {
                let name = variables::CaptureSource::OuterParameter(*depth).scope_name();
                scopes::lookup_variable(&self.scopes, &name, &access.accessors)
                    .map(|(ty, _)| ty)
                    .or_else(|| {
                        let (base, _) = scopes::lookup_variable(&self.scopes, &name, &[])?;
                        self.peek_accessor_type(base, &access.accessors, &name).ok()
                    })
            }
            Some(ast::AccessSource::Ripple) => {
                let base = ripple_context?.value_type_id;
                self.peek_accessor_type(base, &access.accessors, "~").ok()
            }
            _ => None,
        }
    }

    /// Resolve the name a name-inheriting spread (`~[...]`, `a[...]`, `$conn[...]`) takes from
    /// its first spread's source: the source access's tuple type, or the flowing value's for
    /// a bare `...`.
    fn inherited_spread_name(
        &mut self,
        fields: &[ast::TupleField],
        ripple_context: Option<&RippleContext>,
    ) -> Option<String> {
        let source = fields.iter().find_map(|f| match &f.value {
            ast::FieldValue::Spread(s) => Some(s),
            _ => None,
        })?;
        let source_type = match source {
            Some(access) => {
                let access = access.clone();
                self.peek_spread_source_type(&access, ripple_context)?
            }
            None => ripple_context?.value_type_id,
        };
        // The source may be a union — e.g. an ascribed response alongside a fallback,
        // whose constructions intern as distinct tuple ids: every member sharing one
        // name inherits it; mixed (or missing) names inherit none.
        let source_type = Type::strip_annotations(source_type, &*self.program);
        let members = match self.program.lookup_type(source_type) {
            Some(Type::Union(members)) => members.clone(),
            _ => vec![source_type],
        };
        let mut name: Option<String> = None;
        for member in members {
            let stripped = Type::strip_annotations(member, &*self.program);
            let Some(Type::Tuple(tuple_id)) = self.program.lookup_type(stripped) else {
                return None;
            };
            let member_name = self.program.lookup_tuple(*tuple_id)?.name.clone()?;
            match &name {
                None => name = Some(member_name),
                Some(existing) if *existing == member_name => {}
                _ => return None,
            }
        }
        name
    }

    fn compile_tuple(
        &mut self,
        name: ast::TupleName,
        fields: Vec<ast::TupleField>,
        ripple_context: Option<&RippleContext>,
        // The tuple type this tuple is expected to produce (from a call argument's callee). Its
        // fields drive parameter inference for un-annotated function-literal fields.
        expected: Option<usize>,
    ) -> Result<(usize, Provenance), Error> {
        helpers::check_field_name_duplicates(&fields, |f| f.name.as_ref())?;

        // `~[..., y]` / `a[..., y]` inherit the result name from their first spread's source.
        let tuple_name = match name {
            ast::TupleName::Anonymous => None,
            ast::TupleName::Named(name) => Some(name),
            ast::TupleName::Inherit => self.inherited_spread_name(&fields, ripple_context),
        };

        // Check if this tuple contains spreads
        let contains_spread = helpers::tuple_contains_spread(&fields);

        if contains_spread {
            // Use specialized compilation for tuples with spreads
            return spread::compile_tuple_with_spread(self, tuple_name, fields, ripple_context);
        }

        // Per-field expected types from a positionally-matching expected tuple type, used to infer
        // un-annotated function-literal fields (e.g. `map [xs, #{ $0 }, Nil]`). `bindings` solves
        // the expected type's variables left-to-right, so an earlier field (`xs`) can pin a
        // variable (`'t`) that a later function field's parameter (`#'t -> 'u`) depends on.
        let expected_tuple = expected.and_then(|e| self.expected_tuple_fields(e, fields.len()));
        let expected_fields = expected_tuple.as_ref().map(|(_, fields)| fields.clone());
        // Which written field fills each slot. Labeled entries may name their slots in any
        // order, so the literal is built — and therefore evaluated — in the expected type's
        // canonical order rather than the written one.
        let order: Vec<usize> = expected_tuple
            .as_ref()
            .and_then(|(tuple_id, _)| self.resolve_slots(*tuple_id, &fields))
            .unwrap_or_else(|| (0..fields.len()).collect());
        let mut bindings: HashMap<String, usize> = HashMap::new();

        // Compile field values and collect their types and provenances
        let mut field_types = Vec::new();
        let mut field_provenances = Vec::new();
        for (fields_compiled, field) in order.iter().map(|&i| &fields[i]).enumerate() {
            // This field's expected type, with the variables solved so far substituted in.
            let field_expected = expected_fields
                .as_ref()
                .map(|efs| typing::substitute(efs[fields_compiled], &bindings, self.program));
            let (field_type, field_prov) = match &field.value {
                ast::FieldValue::Chain(chain) => {
                    // Each field chain receives a copy of the enclosing (piped) value as its
                    // input, so a leading callable field is called with it (and `&` is needed
                    // to pass a callable by value). The original value remains lower on the
                    // stack for nested `~` references and is cleaned up below if owned.
                    let input = ripple_context.map(|ctx| {
                        // Duplicate the piped value to the top of the stack as the input.
                        self.codegen
                            .add_instruction(Instruction::pick(ctx.stack_offset + fields_compiled));
                        (ctx.value_type_id, ctx.provenance.clone())
                    });
                    // The input value carries the piped value (and its provenance); nested
                    // tuples re-derive their own ripple context from it, so we pass no parent
                    // ripple_context here (which would otherwise have a stale stack offset).
                    self.compile_chain_with_input(
                        chain.clone(),
                        None,
                        None,
                        input,
                        None,
                        false,
                        field_expected,
                        false, // a field's value is data; nothing gates on it
                    )?
                }
                ast::FieldValue::Spread(_) => {
                    unreachable!("Spread should be handled by compile_tuple_with_spread")
                }
            };
            // Grow the bindings by unifying the expected field type against the compiled type, so a
            // later field's expected type sees the variables this field pinned. Best-effort: a
            // mismatch here is reported properly later, when the whole tuple is applied to the
            // callee, so only commit bindings on success.
            if let Some(efs) = &expected_fields {
                let mut trial = bindings.clone();
                if typing::unify(&mut trial, efs[fields_compiled], field_type, self.program).is_ok()
                {
                    bindings = trial;
                }
            }
            // Record the field label's type for hover (named source fields only) — e.g. a
            // module's `[ double: #..., triple: #... ]` exposes each member's signature.
            if let (Some(name), Some(span)) = (&field.name, field.name_span.get()) {
                self.record_typed(
                    Some(span),
                    field_type,
                    SymbolKind::Field,
                    Some(name.clone()),
                );
            }

            // An unnamed field adopts the expected field's label when the written type
            // marked it omittable (`[(foo): 'int]`): the value is built fully labeled, so
            // omission is purely a spelling convenience — matching, equality, and partial
            // access see one shape however the literal was written.
            let field_name = field.name.clone().or_else(|| {
                let (expected_tuple_id, _) = expected_tuple.as_ref()?;
                if !self
                    .program
                    .label_omittable(*expected_tuple_id, fields_compiled)
                {
                    return None;
                }
                self.program.lookup_tuple(*expected_tuple_id)?.fields[fields_compiled]
                    .0
                    .clone()
            });
            field_types.push((field_name, field_type));
            field_provenances.push(field_prov);
        }

        // Register the tuple type and emit instruction
        let tuple_id = self.program.register_tuple(tuple_name, field_types);
        self.codegen.add_instruction(Instruction::tuple(tuple_id));

        // Clean up ripple value if we own it
        if let Some(ctx) = ripple_context
            && ctx.owns_value
        {
            self.codegen.add_instruction(Instruction::rotate(2));
            self.codegen.add_instruction(Instruction::pop());
        }

        // A tuple literal is freshly built, provably annotation-free: an exact-empty
        // row, so retrieval on it (or on unions containing it) types absent keys as
        // provably absent rather than rejecting them as possibly-erased.
        let result_type = self.program.register_type(Type::Tuple(tuple_id));
        let result_type = annotations::exact_empty(self.program, result_type);

        Ok((result_type, Provenance::Tuple(field_provenances)))
    }

    /// The tuple id and positional field types of `expected` if it is a tuple type with exactly
    /// `arity` fields, for driving per-field inference and omittable-label adoption. A non-tuple
    /// or mismatched arity yields `None` (no inference), so the existing all-or-nothing call
    /// check still produces any real error.
    /// Which written field fills each slot of `expected_tuple_id`, or `None` when the literal
    /// doesn't resolve against it — in which case the caller keeps the written order and the
    /// ordinary type check reports the mismatch.
    ///
    /// Positional entries fill slots as a prefix, in declared order, and must precede any
    /// labeled entry: adoption is positional, so a trailing positional entry has no
    /// well-defined slot once labels have claimed some out of order. A labeled entry names
    /// its slot, in any order.
    fn resolve_slots(
        &self,
        expected_tuple_id: usize,
        fields: &[ast::TupleField],
    ) -> Option<Vec<usize>> {
        let expected = &self.program.lookup_tuple(expected_tuple_id)?.fields;
        if expected.len() != fields.len() {
            return None;
        }
        let mut slots: Vec<Option<usize>> = vec![None; fields.len()];
        let mut positional = 0;
        for (index, field) in fields.iter().enumerate() {
            let slot = match &field.name {
                None => {
                    // A positional entry after a labeled one: unresolvable.
                    if positional != index {
                        return None;
                    }
                    positional += 1;
                    index
                }
                Some(name) => expected
                    .iter()
                    .position(|(label, _)| label.as_deref() == Some(name.as_str()))?,
            };
            if slots[slot].is_some() {
                return None;
            }
            slots[slot] = Some(index);
        }
        slots.into_iter().collect()
    }

    fn expected_tuple_fields(&self, expected: usize, arity: usize) -> Option<(usize, Vec<usize>)> {
        let Some(Type::Tuple(tuple_id)) = self.program.lookup_type(expected) else {
            return None;
        };
        let info = self.program.lookup_tuple(*tuple_id)?;
        if info.fields.len() != arity {
            return None;
        }
        Some((*tuple_id, info.fields.iter().map(|(_, ty)| *ty).collect()))
    }

    /// The parameter type of an applied head (`f` in `f [args]`), resolved without emitting code,
    /// so a function-literal argument can infer its parameter from it. Returns `None` when the
    /// head isn't a statically-resolvable callable (e.g. `~`, `^`, or a non-callable), leaving
    /// the argument to compile without an expected type.
    /// The type of the value an applied head denotes — a bound variable, an import member,
    /// or a (possibly outer) parameter field — with its **annotation row intact**, so
    /// definition-carried metadata such as `:defaults` stays visible. `None` for heads that
    /// denote no resolvable value (`~`, bare `^`, builtins), which carry none.
    fn callee_value_type(&mut self, access: &ast::Access) -> Option<usize> {
        match &access.source {
            Some(ast::AccessSource::Identifier(name) | ast::AccessSource::TailCall(Some(name))) => {
                // A captured member (`iter.fold` inside a closure) is bound under its full path, so
                // try that first; otherwise resolve the base binding (`iter`, a local record) and
                // follow the accessors (`.fold`) to the member.
                if let Some((ty, _)) =
                    scopes::lookup_variable(&self.scopes, name, &access.accessors)
                {
                    Some(ty)
                } else {
                    let base =
                        scopes::lookup_variable(&self.scopes, name, &[]).map(|(ty, _)| ty)?;
                    self.follow_accessors(base, &access.accessors)
                }
            }
            Some(ast::AccessSource::Import(module)) => {
                // `resolve_import` applies the accessors, yielding the member type directly.
                self.resolve_import(module, &access.accessors)
                    .ok()
                    .map(|(_, _, ty, _)| ty)
            }
            Some(ast::AccessSource::Parameter { depth: 0 }) => {
                let base = scopes::get_function_parameter(&self.scopes)
                    .ok()
                    .map(|(ty, _)| ty)?;
                self.follow_accessors(base, &access.accessors)
            }
            Some(ast::AccessSource::Parameter { depth }) => {
                // An outer parameter (`$$f`) peeks through its capture: the exact path when
                // that's what was captured, else the bare run plus the accessor walk.
                let name = variables::CaptureSource::OuterParameter(*depth).scope_name();
                if let Some((ty, _)) =
                    scopes::lookup_variable(&self.scopes, &name, &access.accessors)
                {
                    Some(ty)
                } else {
                    let (base, _) = scopes::lookup_variable(&self.scopes, &name, &[])?;
                    self.follow_accessors(base, &access.accessors)
                }
            }
            _ => None,
        }
    }

    fn callee_parameter_type(&mut self, access: &ast::Access) -> Option<usize> {
        let callable = match &access.source {
            Some(ast::AccessSource::TailCall(None)) => {
                // Bare `^` recurses into the current function, whose declared parameter is
                // already in scope — so a positional `^ [args]` literal can adopt a labeled
                // parameter's field labels like any other call argument.
                return scopes::get_function_parameter(&self.scopes)
                    .ok()
                    .map(|(ty, _)| ty);
            }
            Some(ast::AccessSource::Builtin(name)) => {
                // A builtin's signature gives its parameter directly — no need to assemble a
                // Callable just to take it apart again below — unless explicit type
                // arguments must instantiate the callable first.
                let (param, result) = self.builtins.resolve_signature(name, self.program)?;
                if access.type_arguments.is_empty() {
                    return Some(self.program.register_type(param));
                }
                let parameter = self.program.register_type(param);
                let result = self.program.register_type(result);
                let receive = self.program.never();
                let callable = self.program.register_type(Type::Callable {
                    parameter,
                    result,
                    receive,
                    states: Some(parameter),
                });
                self.record_builtin_type_params(name, callable, parameter, result);
                callable
            }
            _ => self.callee_value_type(access)?,
        };
        // Explicit type arguments pin the callable's parameters before the argument
        // compiles, so the inference literal sees the instantiated types. Errors are
        // ignored here — this is a speculative peek, and the real compile reports them.
        let callable = self
            .instantiate_type_arguments(callable, &access.type_arguments)
            .ok()?;
        let callable = Type::strip_annotations(callable, &*self.program);
        match self.program.lookup_type(callable)? {
            Type::Callable { parameter, .. } => Some(*parameter),
            _ => None,
        }
    }

    /// The type of annotation `key` on `type_id`, when the row makes it definitely visible.
    /// A declared boundary erases the row, so this answers `None` there — which is exactly
    /// how defaults come to be shed at one.
    fn row_entry(&self, type_id: usize, key: usize) -> Option<usize> {
        match self.program.lookup_type(type_id)? {
            Type::Annotated { entries, .. } => entries
                .binary_search_by_key(&key, |(k, _)| *k)
                .ok()
                .map(|index| entries[index].1),
            Type::Union(members) if members.len() == 1 => self.row_entry(members[0], key),
            _ => None,
        }
    }

    /// The labels an applied head declares defaults for, from its visible `:defaults` row
    /// entry. `None` when it carries none — a plain call, or one whose row was erased at a
    /// declared boundary, where every field is mandatory.
    fn callee_defaults(&mut self, access: &ast::Access) -> Option<Vec<String>> {
        let value_type = self.callee_value_type(access)?;
        let key = annotations::intern_key(self.program, annotations::DEFAULTS);
        let entry = self.row_entry(value_type, key)?;
        let tuple_id = annotations::single_tuple(&*self.program, entry)?;
        Some(
            self.program
                .lookup_tuple(tuple_id)?
                .fields
                .iter()
                .filter_map(|(label, _)| label.clone())
                .collect(),
        )
    }

    /// A synthetic call-argument field reading `<callee>:defaults.<label>`. The default
    /// rides the *closure*, not the type, so it is fetched from the callee value — which is
    /// what lets two functions with the same signature declare different defaults.
    fn default_field(access: &ast::Access, label: &str) -> ast::TupleField {
        let mut source = access.clone();
        // Type arguments instantiate the callable; an annotation read doesn't want them.
        source.type_arguments.clear();
        source.accessors.push(ast::AccessPath::Annotation(
            annotations::DEFAULTS.to_string(),
            None,
        ));
        source
            .accessors
            .push(ast::AccessPath::Field(label.to_string()));
        source.accessor_spans.push(ast::Spanned::default());
        source.accessor_spans.push(ast::Spanned::default());
        ast::TupleField {
            name: Some(label.to_string()),
            name_span: ast::Spanned::default(),
            span: ast::Spanned::default(),
            value: ast::FieldValue::Chain(ast::Chain {
                binding: None,
                binding_span: ast::Spanned::default(),
                span: ast::Spanned::default(),
                terms: vec![ast::Term::Access(source)],
                assertions: Vec::new(),
            }),
        }
    }

    /// Fill a call argument literal's omitted fields from the callee's `:defaults`, so the
    /// literal reaching `compile_tuple` is already complete and the ordinary field loop —
    /// including its ripple stack arithmetic — is untouched.
    ///
    /// `Ok(None)` leaves the argument alone: it isn't a plain literal, it is already
    /// complete, or the callee declares no defaults (where a short literal is an ordinary
    /// arity error). Only a callee that *does* declare defaults, yet leaves a slot
    /// unfillable, reports a field-named error.
    fn fill_defaults(
        &mut self,
        access: &ast::Access,
        argument: &ast::Term,
    ) -> Result<Option<ast::Tuple>, Error> {
        let ast::Term::Tuple(tuple) = argument else {
            return Ok(None);
        };
        if helpers::tuple_contains_spread(&tuple.fields) {
            return Ok(None);
        }
        let Some(parameter) = self.callee_parameter_type(access) else {
            return Ok(None);
        };
        let Some(expected) = annotations::single_tuple(&*self.program, parameter)
            .and_then(|id| self.program.lookup_tuple(id))
            .map(|info| {
                info.fields
                    .iter()
                    .map(|(label, _)| label.clone())
                    .collect::<Vec<_>>()
            })
        else {
            return Ok(None);
        };
        if tuple.fields.len() >= expected.len() {
            return Ok(None);
        }
        let Some(defaults) = self.callee_defaults(access) else {
            return Ok(None);
        };

        // Which slot each written entry fills — positional as a prefix, labeled by name in
        // any order. An unresolvable literal is left alone for the type check to report.
        let mut filled: Vec<Option<ast::TupleField>> = vec![None; expected.len()];
        let mut positional = 0;
        for (index, field) in tuple.fields.iter().enumerate() {
            let slot = match &field.name {
                None if positional == index => {
                    positional += 1;
                    index
                }
                None => return Ok(None),
                Some(name) => {
                    match expected
                        .iter()
                        .position(|label| label.as_deref() == Some(name.as_str()))
                    {
                        Some(slot) => slot,
                        None => return Ok(None),
                    }
                }
            };
            if filled[slot].is_some() {
                return Ok(None);
            }
            filled[slot] = Some(field.clone());
        }

        let mut fields = Vec::with_capacity(expected.len());
        for (slot, entry) in filled.into_iter().enumerate() {
            match entry {
                Some(field) => fields.push(field),
                None => {
                    let label = expected[slot]
                        .as_deref()
                        .filter(|label| defaults.iter().any(|d| d == label));
                    let Some(label) = label else {
                        let field = expected[slot]
                            .as_deref()
                            .map(|label| format!("'{label}'"))
                            .unwrap_or_else(|| format!("at index {slot}"));
                        return Err(Error::TypeUnresolved(format!(
                            "Missing field {field} of {}, which declares no default for it",
                            quiver_core::format::format_type_by_id(&*self.program, parameter)
                        )));
                    };
                    fields.push(Self::default_field(access, label));
                }
            }
        }
        Ok(Some(ast::Tuple {
            name: tuple.name.clone(),
            fields,
            span: tuple.span,
            // Elaboration reorders fields into the expected type's canonical order and fills
            // defaults, so the result no longer matches any written spelling. Only the parsed
            // AST is ever formatted, so dropping the flag here costs nothing.
            punned: false,
        }))
    }

    /// Resolve the type after an accessor path, for type-only inspection (look-ahead
    /// peeks, receive-type collection). Builds the type environment a checked
    /// annotation accessor (`:('t)key`) needs to resolve its expected shape.
    fn peek_accessor_type(
        &mut self,
        base: usize,
        accessors: &[ast::AccessPath],
        target_name: &str,
    ) -> Result<usize, Error> {
        let mut env = typing::TypeEnv {
            resolver: self.resolver,
            module_cache: &mut *self.module_cache,
            package: &self.current_package,
        };
        type_queries::resolve_accessor_type(
            &mut env,
            &self.scopes,
            self.program,
            base,
            accessors,
            target_name,
        )
    }

    /// Resolve a chain of field/index accessors against a type, for type-only inspection.
    /// Returns the base type unchanged when there are no accessors.
    fn follow_accessors(&mut self, base: usize, accessors: &[ast::AccessPath]) -> Option<usize> {
        if accessors.is_empty() {
            return Some(base);
        }
        self.peek_accessor_type(base, accessors, "callee").ok()
    }

    /// Unify multiple receive types into a single type.
    /// If there's one type, return it. If multiple, create a union.
    /// Deduplicates types to avoid redundant unions.
    fn unify_receive_types(&mut self, receive_types: Vec<usize>) -> usize {
        if receive_types.is_empty() {
            return self.program.never();
        }

        if receive_types.len() == 1 {
            return receive_types[0];
        }

        // Deduplicate types
        let mut unique_types: Vec<usize> = Vec::new();
        for ty in receive_types {
            if !unique_types.contains(&ty) {
                unique_types.push(ty);
            }
        }

        if unique_types.len() == 1 {
            return unique_types[0];
        }

        // Create union of all receive types
        self.program.register_type(Type::Union(unique_types))
    }

    fn extract_receive_type(&mut self, body: Option<&ast::Block>) -> Result<usize, Error> {
        let mut receive_types = Vec::new();
        if let Some(body) = body {
            self.collect_receive_types(body, &mut receive_types)?;
        }

        Ok(self.unify_receive_types(receive_types))
    }

    /// Mint the uniquifying suffix for a fresh generic definition. Inside a module
    /// compile it derives from (module, per-module definition counter): deterministic
    /// — artifact content must not vary with session history — and unique across
    /// modules by hashing, so rigid variables from different modules never alias. At
    /// the entry level the historical seed (the program's type count) remains: entry
    /// code is never extracted, and the count strictly grows, so fresh entry
    /// definitions can't collide with cached module types.
    fn mint_type_param_suffix(&mut self) -> usize {
        match self.suffix_scopes.last_mut() {
            Some((module, counter)) => {
                *counter += 1;
                let mut hasher = std::collections::hash_map::DefaultHasher::new();
                use std::hash::{Hash, Hasher};
                module.hash(&mut hasher);
                counter.hash(&mut hasher);
                hasher.finish() as usize
            }
            None => self.program.type_count(),
        }
    }

    fn extract_receive_type_from_steps(&mut self, steps: &[ast::Step]) -> Result<usize, Error> {
        let mut receive_types = Vec::new();
        for chain in steps.iter().filter_map(ast::Step::as_chain) {
            self.collect_receive_types_from_chain(chain, &mut receive_types)?;
        }

        Ok(self.unify_receive_types(receive_types))
    }

    fn collect_receive_types(
        &mut self,
        block: &ast::Block,
        receive_types: &mut Vec<usize>,
    ) -> Result<(), Error> {
        for branch in &block.branches {
            self.collect_receive_types_from_sequence(&branch.condition, receive_types)?;
            if let Some(consequence) = &branch.consequence {
                self.collect_receive_types_from_sequence(consequence, receive_types)?;
            }
        }
        Ok(())
    }

    fn collect_receive_types_from_sequence(
        &mut self,
        sequence: &ast::Sequence,
        receive_types: &mut Vec<usize>,
    ) -> Result<(), Error> {
        for chain in sequence.chains() {
            self.collect_receive_types_from_chain(chain, receive_types)?;
        }
        Ok(())
    }

    fn collect_receive_types_from_chain(
        &mut self,
        chain: &ast::Chain,
        receive_types: &mut Vec<usize>,
    ) -> Result<(), Error> {
        // Track the chained type as we traverse the chain
        let mut chained_type: Option<usize> = None;

        for term in &chain.terms {
            chained_type =
                self.collect_receive_types_from_term(term, chained_type, receive_types)?;
        }
        Ok(())
    }

    fn collect_receive_types_from_term(
        &mut self,
        term: &ast::Term,
        chained_type: Option<usize>,
        receive_types: &mut Vec<usize>,
    ) -> Result<Option<usize>, Error> {
        match term {
            ast::Term::Select(sources, _) => {
                // Extract receive types from all sources
                let mut select_sources = Vec::new();

                match sources {
                    // Handle postfix form: `receiver ~> !` (bare !)
                    // In this case, extract receive type from the chained value
                    None => {
                        if let Some(chained) = chained_type
                            && let Some(Type::Callable { parameter, .. }) =
                                self.program.lookup_base(chained)
                        {
                            select_sources.push(*parameter);
                        }
                    }
                    // Explicit empty `![]` - no receive types
                    Some(sources) if sources.is_empty() => {}
                    // Explicit sources
                    Some(sources) => {
                        for source in sources {
                            self.extract_receive_type_from_source(
                                source,
                                chained_type.as_ref(),
                                &mut select_sources,
                            )?;
                        }
                    }
                }

                // If this select has receive sources, add the unified type
                if !select_sources.is_empty() {
                    if select_sources.len() == 1 {
                        receive_types.push(select_sources[0]);
                    } else {
                        // Multiple sources in same select - create union
                        receive_types.push(self.program.register_type(Type::Union(select_sources)));
                    }
                }
                // Select doesn't produce a chainable type for receive extraction
                Ok(None)
            }
            ast::Term::Access(access) => {
                // Try to resolve the access to get its type
                if let Some(ast::AccessSource::Identifier(identifier)) = &access.source {
                    let var_type =
                        scopes::lookup_variable(&self.scopes, identifier, &access.accessors)
                            .map(|(t, _)| t)
                            .or_else(|| {
                                let (base_type, _) =
                                    scopes::lookup_variable(&self.scopes, identifier, &[])?;
                                self.peek_accessor_type(base_type, &access.accessors, identifier)
                                    .ok()
                            });
                    Ok(var_type)
                } else {
                    Ok(None)
                }
            }
            ast::Term::Apply(_access, argument) => {
                // The argument may contain a select defining a receive type (`f [!#'int]`).
                self.collect_receive_types_from_term(argument, chained_type, receive_types)?;
                Ok(None)
            }
            ast::Term::Block(block) => {
                self.collect_receive_types(block, receive_types)?;
                Ok(None)
            }
            ast::Term::String(_, segments) => {
                // A hole is a block-like expression that may contain a select.
                for segment in segments {
                    if let ast::StrSegment::Hole(block) = segment {
                        self.collect_receive_types(block, receive_types)?;
                    }
                }
                Ok(None)
            }
            ast::Term::Function(_) => {
                // Don't recurse into nested function definitions - they have their own receive types
                Ok(None)
            }
            ast::Term::Tuple(tuple) => {
                // Check tuple fields
                for field in &tuple.fields {
                    if let ast::FieldValue::Chain(chain) = &field.value {
                        self.collect_receive_types_from_chain(chain, receive_types)?;
                    }
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    fn extract_receive_type_from_source(
        &mut self,
        source: &ast::Chain,
        chained_type: Option<&usize>,
        receive_types: &mut Vec<usize>,
    ) -> Result<(), Error> {
        // A source can be:
        // 1. A function literal: #'int { ... } -> extract int
        // 2. A variable reference: r1 -> look up and extract parameter type
        // 3. A ripple: ~ -> use the chained type
        // 4. Something else (process, timeout) -> ignore

        for term in &source.terms {
            match term {
                term if term.is_bare_ripple() => {
                    // Ripple refers to the chained value - extract its receive type
                    if let Some(chained_type_id) = chained_type
                        && let Some(Type::Callable { parameter, .. }) =
                            self.program.lookup_base(*chained_type_id)
                    {
                        receive_types.push(*parameter);
                    }
                }
                ast::Term::Function(func) => {
                    // Inline function definition - extract parameter type
                    if let Some(param_type) = &func.parameter_type {
                        let mut env = typing::TypeEnv {
                            resolver: self.resolver,
                            module_cache: &mut *self.module_cache,
                            package: &self.current_package,
                        };
                        let resolved_type = typing::resolve_ast_type(
                            &mut env,
                            &self.scopes,
                            param_type.clone(),
                            self.program,
                        )?;
                        receive_types.push(resolved_type);
                    }
                }
                ast::Term::Reference(access) => {
                    // A referenced receiver (`&f`, `&%int.and`) — the form the tight `!f`/`!var`
                    // sugar produces. Resolve its type from either a lexical variable or a module
                    // member, then take its parameter type as the message type.
                    let receiver_type = match &access.source {
                        Some(ast::AccessSource::Identifier(identifier)) => {
                            // Variable reference: try full path first, then base + accessor
                            // resolution through the type system.
                            scopes::lookup_variable(&self.scopes, identifier, &access.accessors)
                                .map(|(t, _)| t)
                                .or_else(|| {
                                    let (base_type, _) =
                                        scopes::lookup_variable(&self.scopes, identifier, &[])?;
                                    self.peek_accessor_type(
                                        base_type,
                                        &access.accessors,
                                        identifier,
                                    )
                                    .ok()
                                })
                        }
                        Some(ast::AccessSource::Import(module)) => {
                            // Module member, e.g. `%int.and` — resolve it like the main compiler
                            // does, so an inline module receiver needs no intermediate binding.
                            self.resolve_import(module, &access.accessors)
                                .ok()
                                .map(|(_, _, resolved_type, _)| resolved_type)
                        }
                        _ => continue,
                    };

                    if let Some(type_id) = receiver_type
                        && let Some(Type::Callable { parameter, .. }) =
                            self.program.lookup_base(type_id)
                    {
                        receive_types.push(*parameter);
                    }
                }
                _ => {
                    // Recursively check nested structures
                    self.collect_receive_types_from_term(term, None, receive_types)?;
                }
            }
        }
        Ok(())
    }

    /// Compile a function literal. `expected_parameter` is the parameter type the use site
    /// expects (from a call argument's callee), used to infer the parameter of an un-annotated
    /// literal (`#{ $0 }`); it is ignored when the literal declares its own parameter type.
    /// Lower `name: type = value` defaults in a function literal's parameter spelling into
    /// a synthetic `:defaults` annotation, stripping them from the type AST. Only top-level
    /// parameter fields are consumed; one written anywhere else stays put and is rejected by
    /// `validate_type_ast`.
    ///
    /// A body that already states `:defaults` explicitly keeps both, and the ordinary
    /// duplicate-annotation check rejects the pair — mixing the two spellings is confusing
    /// even when they name different fields.
    fn lower_field_defaults(function: &mut ast::Function) -> Result<(), Error> {
        let Some(ast::Type::Tuple(tuple)) = &mut function.parameter_type else {
            return Ok(());
        };
        let mut fields = Vec::new();
        for (index, field) in tuple.fields.iter_mut().enumerate() {
            let ast::FieldType::Field { name, default, .. } = field else {
                continue;
            };
            let Some(chain) = default.take() else {
                continue;
            };
            let Some(name) = name.clone() else {
                return Err(Error::TypeUnresolved(format!(
                    "The parameter field at index {index} has a default but no label, so a \
                     call could never omit an earlier field and still state it"
                )));
            };
            fields.push(ast::TupleField {
                name: Some(name),
                name_span: ast::Spanned::default(),
                span: ast::Spanned::default(),
                value: ast::FieldValue::Chain(*chain),
            });
        }
        if fields.is_empty() {
            return Ok(());
        }
        let Some(body) = &mut function.body else {
            return Err(Error::TypeUnresolved(
                "A field default needs a function body to attach to".to_string(),
            ));
        };
        body.annotations.push(ast::Annotation {
            name: annotations::DEFAULTS.to_string(),
            name_span: ast::Spanned::default(),
            span: ast::Spanned::default(),
            value: ast::Chain {
                binding: None,
                binding_span: ast::Spanned::default(),
                span: ast::Spanned::default(),
                assertions: Vec::new(),
                terms: vec![ast::Term::Tuple(ast::Tuple {
                    name: ast::TupleName::Anonymous,
                    fields,
                    span: ast::Spanned::default(),
                    punned: false,
                })],
            },
        });
        Ok(())
    }

    fn compile_function(
        &mut self,
        mut function: ast::Function,
        expected_parameter: Option<usize>,
    ) -> Result<usize, Error> {
        // Dialect invocations expand to ordinary AST that may reference enclosing
        // variables (`EVar`), so expand them before captures are collected — the capture
        // collector cannot see through an unexpanded `%mod{…}`. Only the outermost
        // literal walks: it expands every nested body in the same pass (the lazy path in
        // `compile_term` covers non-function contexts).
        if self.function_depth == 0
            && let Some(body) = &mut function.body
        {
            self.expand_dialects_in_block(body)?;
        }

        // `name: type = value` in the parameter spelling is sugar for a `:defaults` entry
        // on the closure. Lower it before the annotation prefix is taken, so the two
        // spellings share one mechanism — and strip it from the type AST, since the
        // default belongs to the function, never to its parameter type.
        Self::lower_field_defaults(&mut function)?;

        // A function body's annotation prefix attaches to the *closure*, not the body's
        // result: extract it before capture collection (the annotation chains evaluate in
        // the enclosing scope at literal-evaluation time, so they contribute no captures)
        // and compile it after the Function instruction below.
        let function_annotations = match &mut function.body {
            Some(body) => std::mem::take(&mut body.annotations),
            None => vec![],
        };

        let mut function_params: HashSet<String> = HashSet::new();

        if let Some(ast::Type::Tuple(tuple_type)) = &function.parameter_type {
            for field in &tuple_type.fields {
                if let ast::FieldType::Field {
                    name: Some(field_name),
                    ..
                } = field
                {
                    function_params.insert(field_name.clone());
                }
            }
        }

        let unique_captures = variables::collect_free_variables(
            function.body.as_ref(),
            &function_params,
            &|name, accessors| {
                // Check if the full path (base + accessors) is defined, or just the base
                scopes::lookup_variable(&self.scopes, name, accessors).is_some()
                    || scopes::lookup_variable(&self.scopes, name, &[]).is_some()
            },
        );

        // A hoisted import earns its slot only when its reconstruction is non-trivial. The
        // capture list must stay exact — registration, the build-site pushes, and the
        // runtime's capture count all walk it — so drop rejects here, before anything
        // counts them. A resolution failure is also dropped: the body's own reference
        // reports it with full context (and compiles inline, exactly as without hoisting).
        // The resolved type is kept (parallel to the list) so registration below doesn't
        // resolve a second time; `import_types[i]` is `Some` exactly for `Import` sources.
        let mut import_types: Vec<Option<usize>> = Vec::with_capacity(unique_captures.len());
        let unique_captures: Vec<variables::Capture> = unique_captures
            .into_iter()
            .filter_map(|capture| {
                let import_type = match &capture.source {
                    variables::CaptureSource::Import(module) => {
                        match self.resolve_import(module, &capture.accessors) {
                            Ok((_, value, resolved_type, _)) if worth_hoisting(&value) => {
                                Some(resolved_type)
                            }
                            _ => return None,
                        }
                    }
                    _ => None,
                };
                import_types.push(import_type);
                Some(capture)
            })
            .collect();

        // Resolve parameter type with declared type parameters, uniquifying their names.
        // Distinct top-level definitions get distinct suffixes — `Type::Variable` is
        // name-keyed, so `map`'s `'t` called from `left`'s body would otherwise unify "a
        // variable with itself" and silently fail to pin — while literals nested in one
        // definition inherit its suffix, preserving the convention that a nested
        // literal's re-declared `<'t>` names the enclosing function's `'t` (a nested
        // signature cannot otherwise reach the enclosing parameters; see e.g. the list
        // module's `iter`). The seed is the program's type count: persistent across
        // compiler instances, so a fresh REPL evaluation can't mint suffixes colliding
        // with cached module types (registering the renamed variable advances it).
        let inherited_suffix = self.type_param_suffix;
        let type_param_suffix = inherited_suffix.unwrap_or_else(|| self.mint_type_param_suffix());
        self.type_param_suffix = Some(type_param_suffix);
        let parameter_type = match &function.parameter_type {
            Some(t) => {
                let mut env = typing::TypeEnv {
                    resolver: self.resolver,
                    module_cache: &mut *self.module_cache,
                    package: &self.current_package,
                };
                typing::resolve_function_parameter_type(
                    &mut env,
                    &self.scopes,
                    t.clone(),
                    &function.type_parameters,
                    type_param_suffix,
                    self.program,
                )?
            }
            None => {
                // No annotation: infer the parameter from the expected callable type at the use
                // site when one is available and usable. A type variable the callee has yet to
                // solve is not usable — there's nothing to pin it, so the literal couldn't act on
                // its parameter — so fall back to nil, preserving the `#{ ... }` nilary-function
                // shorthand. A *rigid* variable is usable: an earlier sibling argument pinned the
                // callee's variable to one of the enclosing generic's own parameters (`%list.map
                // [$list, #{ $ }]` inside `#<'t>['%list<'t>, …]`), which is an opaque but real
                // type the literal can pass through. Falling back there would infer `#[] -> []`
                // and silently widen the caller's `'t` to `'t | []`.
                let usable = expected_parameter.filter(|&ep| match self.program.lookup_type(ep) {
                    Some(Type::Variable(name)) => typing::is_rigid_variable(name, inherited_suffix),
                    _ => true,
                });
                match usable {
                    Some(ep) => ep,
                    None => self.program.register_type(Type::nil()),
                }
            }
        };

        // Extract receive type from function body
        let receive_type = self.extract_receive_type(function.body.as_ref())?;

        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_local_count = self.local_count;
        let saved_receive_type = self.current_receive_type_id;
        let saved_states = self.current_states;

        // Extract type aliases from parent scopes to preserve in function scope
        // (inner scopes' definitions win)
        let mut function_scope_bindings = Bindings::default();
        for scope in &saved_scopes {
            for (name, alias) in &scope.bindings.type_aliases {
                function_scope_bindings
                    .type_aliases
                    .insert(name.clone(), alias.clone());
            }
        }

        // The declared type parameters are visible in the body as ordinary aliases for
        // their uniquified variables, so body-position type references — patterns,
        // ascriptions, checked retrievals, explicit type applications, nested signatures
        // — can name them.
        for param in &function.type_parameters {
            let variable = self
                .program
                .register_type(Type::Variable(format!("{param}#{type_param_suffix}")));
            function_scope_bindings.type_aliases.insert(
                param.clone(),
                TypeAliasDef {
                    parameters: Vec::new(),
                    type_id: variable,
                },
            );
        }

        self.scopes = vec![Scope::new(function_scope_bindings, None, ScopeKind::Root)];
        self.codegen.instructions = Vec::new();
        self.local_count = 0;
        self.current_receive_type_id = receive_type;
        // Seed the states union with the parameter (the spawn init / bare-`^` argument);
        // tail calls widen it during body compilation.
        self.current_states = Some(parameter_type);

        // Define captures as first locals in function body scope
        for (capture, import_type) in unique_captures.iter().zip(&import_types) {
            // Determine the type of the captured value
            let capture_type = match &capture.source {
                variables::CaptureSource::OuterParameter(levels) => {
                    // The value lives in the creation scope, one level shallower there —
                    // level 1 is that scope's own parameter; a deeper level is that scope's
                    // own outer-parameter capture, already registered because functions
                    // compile outside-in. Failure is a real user error (the reference
                    // reaches above the outermost function), never a skippable capture —
                    // and it must point at the reference, not wherever compilation last
                    // recorded a span: materialisation runs far from the referencing site.
                    self.current_span = capture.span.get().or(self.current_span);
                    let exceeded = || Error::ParameterDepthExceeded {
                        written: ast::parameter_sigils(*levels),
                    };
                    if *levels == 1 {
                        let (param_type, _) = scopes::get_function_parameter(&saved_scopes)
                            .map_err(|_| exceeded())?;
                        self.peek_accessor_type(
                            param_type,
                            &capture.accessors,
                            &ast::parameter_sigils(*levels),
                        )?
                    } else {
                        let relay = variables::CaptureSource::OuterParameter(levels - 1);
                        let Some((outer_type, _)) = scopes::lookup_variable(
                            &saved_scopes,
                            &relay.scope_name(),
                            &capture.accessors,
                        ) else {
                            return Err(exceeded());
                        };
                        outer_type
                    }
                }
                variables::CaptureSource::Import(_) => {
                    // A hoisted import slot, typed exactly as the use site would type the
                    // member — the annotation row included, so contract detection and
                    // retrieval through the slot see what inline emission would. The type
                    // was resolved (once) by the filter above; a `None` here would mean
                    // the two walks disagreed, silently skewing every later capture's
                    // local index — fail loudly instead.
                    import_type.ok_or_else(|| Error::InternalError {
                        message: "import capture kept by the filter without a resolved type"
                            .to_string(),
                    })?
                }
                variables::CaptureSource::Variable(base) => {
                    // First check if the full path is already available (for nested captures)
                    if let Some((full_type, _)) =
                        scopes::lookup_variable(&saved_scopes, base, &capture.accessors)
                    {
                        // The full path is already available from parent, use its type
                        full_type
                    } else if capture.accessors.is_empty() {
                        // Simple capture - use the base variable's type
                        if let Some((var_type, _)) =
                            scopes::lookup_variable(&saved_scopes, base, &[])
                        {
                            var_type
                        } else {
                            continue;
                        }
                    } else {
                        // Capture with accessors - need to compute the accessed type
                        if let Some((var_type, _)) =
                            scopes::lookup_variable(&saved_scopes, base, &[])
                        {
                            // Use compile_accessor logic to determine type
                            // We need to compute this without generating bytecode
                            let mut last_type = var_type;

                            for accessor in &capture.accessors {
                                let field_types = match accessor {
                                    ast::AccessPath::Field(field_name) => {
                                        match type_queries::get_field_by_name(
                                            self.program,
                                            last_type,
                                            field_name,
                                            base,
                                        ) {
                                            Ok((_, types)) => types,
                                            _ => continue,
                                        }
                                    }
                                    ast::AccessPath::Index(index) => {
                                        match type_queries::get_field_at_index(
                                            &*self.program,
                                            last_type,
                                            *index,
                                            base,
                                        ) {
                                            Ok(types) => types,
                                            _ => continue,
                                        }
                                    }
                                    ast::AccessPath::Annotation(name, expected) => match expected {
                                        None => {
                                            match annotations::retrieval_type(
                                                self.program,
                                                last_type,
                                                name,
                                            ) {
                                                Ok((_, result_type)) => vec![result_type],
                                                _ => continue,
                                            }
                                        }
                                        Some(ast_type) => {
                                            let mut env = typing::TypeEnv {
                                                resolver: self.resolver,
                                                module_cache: &mut *self.module_cache,
                                                package: &self.current_package,
                                            };
                                            match typing::resolve_ast_type(
                                                &mut env,
                                                &self.scopes,
                                                ast_type.clone(),
                                                self.program,
                                            ) {
                                                Ok(asked) => {
                                                    let (_, result_type, _) =
                                                        annotations::checked_retrieval_type(
                                                            self.program,
                                                            last_type,
                                                            name,
                                                            asked,
                                                        );
                                                    vec![result_type]
                                                }
                                                _ => continue,
                                            }
                                        }
                                    },
                                };
                                last_type = typing::union_type_ids(self.program, field_types);
                            }
                            last_type
                        } else {
                            continue;
                        }
                    }
                }
            };

            scopes::define_variable(
                &mut self.scopes,
                &mut self.local_count,
                &capture.source.scope_name(),
                &capture.accessors,
                capture_type,
                Provenance::Unknown, // Captures don't track provenance
            )?;
        }

        let mut parameter_fields = HashMap::new();
        if let Some(ast::Type::Tuple(tuple_type)) = &function.parameter_type {
            for (field_index, field) in tuple_type.fields.iter().enumerate() {
                if let ast::FieldType::Field {
                    name: Some(field_name),
                    type_def,
                    ..
                } = field
                {
                    let mut env = typing::TypeEnv {
                        resolver: self.resolver,
                        module_cache: &mut *self.module_cache,
                        package: &self.current_package,
                    };
                    let field_type = typing::resolve_ast_type(
                        &mut env,
                        &self.scopes,
                        type_def.clone(),
                        self.program,
                    )?;
                    parameter_fields.insert(field_name.clone(), (field_index, field_type));
                }
            }
        }
        // Start a fresh dispatch collection for this body (saving any outer one, so a function
        // nested inside another function's body collects independently).
        let saved_dispatch = self.collected_dispatch.replace(DispatchCollection {
            branches: Vec::new(),
            valid: true,
        });
        let body_type = match function.body {
            Some(body) => {
                // Function parameters have Provenance::Parameter since they come from callers
                self.function_depth += 1;
                let body_type = self.compile_scoped_block(
                    body,
                    parameter_type,
                    Provenance::Parameter,
                    None,
                    ScopeKind::Function,
                    true,
                );
                self.function_depth -= 1;
                body_type?
            }
            None => {
                // Identity function: just return the parameter
                // The calling convention puts the parameter on the stack,
                // so we don't need any instructions - just leave it there
                parameter_type
            }
        };
        let dispatch = std::mem::replace(&mut self.collected_dispatch, saved_dispatch);

        // Validate return type if specified
        if let Some(return_type_ast) = &function.return_type {
            let mut env = typing::TypeEnv {
                resolver: self.resolver,
                module_cache: &mut *self.module_cache,
                package: &self.current_package,
            };
            let expected_return_type = typing::resolve_function_parameter_type(
                &mut env,
                &self.scopes,
                return_type_ast.clone(),
                &function.type_parameters,
                type_param_suffix,
                self.program,
            )?;

            // For generic functions, we need strict type equality (not just compatibility)
            // because type variables should match exactly, not be compatible with concrete types
            let types_match = if !function.type_parameters.is_empty() {
                // For generic functions: require exact type equality
                body_type == expected_return_type
            } else {
                // For non-generic functions: use compatibility check
                quiver_core::types::is_compatible(body_type, expected_return_type, &*self.program)
            };

            if !types_match {
                // If the *only* reason for the mismatch is the body falling through to nil over a
                // non-exhaustive enumeration, report the unhandled cases instead of the opaque
                // `found T | []`. (`last_uncovered` is set only for enumeration bodies.)
                if let Some(uncovered) = self.last_uncovered
                    && self.contains_nil(body_type)
                    && !self.contains_nil(expected_return_type)
                {
                    let body_without_nil = self.without_nil(body_type);
                    if quiver_core::types::is_compatible(
                        body_without_nil,
                        expected_return_type,
                        &*self.program,
                    ) {
                        return Err(Error::NonExhaustiveReturn {
                            unhandled: quiver_core::format::format_type_by_id(
                                &*self.program,
                                uncovered,
                            ),
                            declared: quiver_core::format::format_type_by_id(
                                &*self.program,
                                expected_return_type,
                            ),
                        });
                    }
                }
                // Diagnose the mismatch through unification: its near-miss/breadcrumb
                // detail names the offending leaf, where a flat expected/found dump of
                // two large types explains nothing. If unification can't reproduce the
                // failure (its semantics differ at the margins), fall back to the dump.
                let mut diagnostic_bindings = HashMap::new();
                if let Err(Error::TypeUnresolved(message)) = typing::unify(
                    &mut diagnostic_bindings,
                    expected_return_type,
                    body_type,
                    self.program,
                ) {
                    return Err(Error::TypeUnresolved(format!("declared result: {message}")));
                }
                // Get types for error message
                let expected_type = self.program.lookup_type(expected_return_type).unwrap();
                let found_type = self.program.lookup_type(body_type).unwrap();
                return Err(Error::TypeMismatch {
                    expected: quiver_core::format::format_type(&*self.program, expected_type),
                    found: quiver_core::format::format_type(&*self.program, found_type),
                });
            }
        }

        let function_instructions = std::mem::take(&mut self.codegen.instructions);

        // If every branch of the body was a pure parameter dispatch, record its case table so
        // calls can specialize the result type to the concrete argument (return-type dispatch).
        let dispatch_table = dispatch
            .filter(|d| d.valid && !d.branches.is_empty())
            .map(|d| d.branches);

        // Create type information for the function. The receive type is the final
        // `current_receive_type_id`: the body pre-pass seed (line above the save), widened
        // during body compilation by every call/tail-call to a receiving function — a
        // callee's receives execute in whichever process runs this function.
        let callable_type_id = self.program.register_type(Type::Callable {
            parameter: parameter_type,
            result: body_type,
            receive: self.current_receive_type_id,
            states: self.current_states,
        });

        // Record the declared type parameters (as their uniquified variable names, in
        // declaration order) so an explicit instantiation (`f<'int>`) can bind them
        // positionally.
        if !function.type_parameters.is_empty() {
            self.callable_type_params.insert(
                callable_type_id,
                function
                    .type_parameters
                    .iter()
                    .map(|p| format!("{p}#{type_param_suffix}"))
                    .collect(),
            );
        }

        let function_index = self.program.register_function(Function {
            instructions: function_instructions,
            captures: unique_captures.len(),
            type_id: callable_type_id,
        });

        // Record the dispatch case table, keyed by this function's (unique) index. Also map the
        // callable *type* to this function so a call whose callee isn't statically identified can
        // still specialize — unless another dispatch function already claimed the type with a
        // *different* table, which makes the type ambiguous (dropped from `case_tables`). Two
        // functions with an identical table (e.g. `num.add`/`num.sub`) keep the type unambiguous.
        if let Some(branches) = dispatch_table {
            let canonical = match self.case_tables.get(&callable_type_id).copied() {
                None => Some(function_index),
                Some(existing_fn) => (self.fn_case_tables.get(&existing_fn) == Some(&branches))
                    .then_some(existing_fn),
            };
            match canonical {
                Some(fi) => {
                    self.case_tables.insert(callable_type_id, fi);
                }
                None => {
                    self.case_tables.remove(&callable_type_id);
                }
            }
            self.fn_case_tables.insert(function_index, branches);
        }

        self.codegen.instructions = saved_instructions;
        self.scopes = saved_scopes;
        self.local_count = saved_local_count;
        self.current_receive_type_id = saved_receive_type;
        self.current_states = saved_states;

        // Emit instructions to push capture values onto the stack
        // These will be popped by the Function instruction
        for capture in &unique_captures {
            match &capture.source {
                variables::CaptureSource::OuterParameter(levels) => {
                    // Load from the creation scope — its own parameter for level 1 (walking
                    // any accessors, so only the named field is captured), its own capture
                    // one level shallower for deeper levels. Errors point at the reference,
                    // as in the registration loop above.
                    self.current_span = capture.span.get().or(self.current_span);
                    let exceeded = || Error::ParameterDepthExceeded {
                        written: ast::parameter_sigils(*levels),
                    };
                    if *levels == 1 {
                        let (param_type, param_index) =
                            scopes::get_function_parameter(&self.scopes).map_err(|_| exceeded())?;
                        self.codegen.add_instruction(Instruction::load(param_index));
                        if !capture.accessors.is_empty() {
                            self.compile_accessor(
                                param_type,
                                capture.accessors.clone(),
                                &ast::parameter_sigils(*levels),
                                Provenance::Unknown,
                            )?;
                        }
                    } else {
                        let relay = variables::CaptureSource::OuterParameter(levels - 1);
                        let Some((_, index)) = scopes::lookup_variable(
                            &self.scopes,
                            &relay.scope_name(),
                            &capture.accessors,
                        ) else {
                            return Err(exceeded());
                        };
                        self.codegen.add_instruction(Instruction::load(index));
                    }
                }
                variables::CaptureSource::Import(module) => {
                    // Emit the hoisted value at the closure's build site. `compile_import`
                    // consults the *enclosing* function's own slots, so nested builds
                    // bubble outward — a Load where the parent also hoisted the value,
                    // inline reconstruction once the chain reaches top level.
                    self.current_span = capture.span.get().or(self.current_span);
                    self.compile_import(module, &capture.accessors, &[])?;
                }
                variables::CaptureSource::Variable(base) => {
                    // First check if the full path is available (for nested captures)
                    if let Some((_, full_index)) =
                        scopes::lookup_variable(&self.scopes, base, &capture.accessors)
                    {
                        // The full path is already captured, just load it
                        self.codegen.add_instruction(Instruction::load(full_index));
                    } else if let Some((var_type, base_index)) =
                        scopes::lookup_variable(&self.scopes, base, &[])
                    {
                        // Load the base variable
                        self.codegen.add_instruction(Instruction::load(base_index));

                        if !capture.accessors.is_empty() {
                            // Apply accessors to get the final value
                            self.compile_accessor(
                                var_type,
                                capture.accessors.clone(),
                                base,
                                Provenance::Unknown,
                            )?;
                        }
                    }
                }
            }
        }

        self.codegen
            .add_instruction(Instruction::function(function_index));

        // Attach the literal's annotations to the freshly built closure. Evaluated here —
        // in the enclosing scope, once per literal evaluation — so `pre`/`post` contract
        // types are derived from this function's own parameter/result types. The closure
        // is freshly built either way, so its row is exact: absent keys are provably
        // absent (the bare literal gets an exact-empty row).
        self.type_param_suffix = inherited_suffix;

        if !function_annotations.is_empty() {
            return self.compile_annotation_attach(function_annotations, callable_type_id, true);
        }

        Ok(annotations::exact_empty(self.program, callable_type_id))
    }

    #[allow(clippy::too_many_arguments, clippy::result_large_err)]
    #[allow(clippy::too_many_arguments)]
    /// Emit a match's failure path: the scrutinee sits on the stack and must be replaced by the
    /// verdict's nil.
    ///
    /// A match that fails because its scrutinee was *nil* is that nil propagating, not a fresh
    /// failure — re-emitting it keeps the annotations (an `:error` payload) alive, exactly as a
    /// step boundary's short-circuit does. Without this, `expr ~> =pat` mints a fresh nil where
    /// the two-step `expr; =pat` carries the payload: the difference that silently dropped
    /// `%parse`'s furthest-error tracking and the dialect error positions. Any other failure — a
    /// shape, literal, type or pin mismatch — has no incoming failure to carry, so it mints nil.
    ///
    /// Answers the carried nil's type when the scrutinee could have been one, so the caller can
    /// widen the verdict's nil past a provably annotation-free `[]`.
    fn emit_match_failure(&mut self, value_type: usize) -> Option<usize> {
        let members = annotations::nil_members(self.program, value_type);
        let carried = (!members.is_empty()).then(|| typing::union_type_ids(self.program, members));
        // The test is only worth emitting where the scrutinee could actually be nil.
        let carry_jump = carried
            .is_some()
            .then(|| self.codegen.emit_duplicate_jump_if_nil());
        self.codegen.add_instruction(Instruction::pop());
        self.codegen.add_instruction(Instruction::tuple(NIL));
        if let Some(addr) = carry_jump {
            self.codegen.patch_jump_to_here(addr);
        }
        carried
    }

    fn compile_match(
        &mut self,
        pattern: ast::Match,
        value_type: usize,
        value_provenance: Provenance,
        on_no_match: Option<usize>,
        mut narrowing: Option<&mut Narrowing>,
        // Whether the enclosing chain gates control flow on this match's verdict. In a
        // value chain (a tuple field, an argument) the surrounding code runs whether or
        // not the match succeeded, so the scrutinee must not be narrowed there.
        gating: bool,
    ) -> Result<usize, Error> {
        let start_jump_addr = self.codegen.emit_jump_placeholder();
        let fail_jump_addr = self.codegen.emit_jump_placeholder();

        self.codegen.patch_jump_to_here(start_jump_addr);

        // Analyze pattern to get bindings without generating code yet
        let mut env = typing::TypeEnv {
            resolver: self.resolver,
            module_cache: &mut *self.module_cache,
            package: &self.current_package,
        };
        let (bindings, binding_sets, result_type, narrowed_type) = pattern::analyze_pattern(
            &mut env,
            self.program,
            &pattern,
            value_type,
            &self.scopes,
            &value_provenance,
        )?;

        // Record every binding site in this pattern for the language server (go-to-definition
        // and hover). This is the single chokepoint for all bindings: top-level `name = ...`,
        // destructuring (`[x, y] = ...`), mid-chain `=x`, and block branch patterns.
        if self.recorder.is_some() {
            let mut binding_spans = Vec::new();
            collect_binding_spans(&pattern, &mut binding_spans);
            for (name, span) in binding_spans {
                let type_id = bindings.iter().find(|(n, _)| *n == name).map(|(_, ty)| *ty);
                // Define first so the reference below resolves to the binding itself.
                self.record_definition(&name, Some(span));
                if let Some(type_id) = type_id {
                    // A binding site has no accessor path, so the label is just the name.
                    self.record_reference(Some(span), &name, name.clone(), type_id);
                }
            }

            // Pin targets are the pattern's read references (`&name`, `&$x.y`): record each
            // root and accessor so hover, references, and highlight see them exactly like the
            // equivalent expression access. The pattern's own bindings aren't in scope yet,
            // which is right — a pin only ever references pre-existing values.
            let mut pin_targets = Vec::new();
            collect_pin_targets(&pattern, &mut pin_targets);
            for target in pin_targets {
                self.record_pin_target(target);
            }
        }

        // Check if pattern has non-type requirements (literals, variable pins, path equality).
        // Patterns with non-type requirements cannot use complement narrowing.
        // Exception: tuple patterns with a single type-constraining field CAN use
        // field-specific complement narrowing, so don't disable in that case.
        let has_tuple_complement =
            analyze_tuple_pattern_for_complement(&pattern, value_type, self.program).is_some();

        // Complement narrowing is unsound when the pattern constrains a recursive field (the
        // narrowed type can't capture that constraint), so disable it there too.
        let prevents = pattern::prevents_complement_narrowing(&binding_sets, &*self.program)
            || narrowing::pattern_constrains_recursive_field(&pattern, value_type, self.program);

        if prevents
            && !has_tuple_complement
            && let Some(n) = narrowing.as_mut()
        {
            n.disable();
        }

        // If result type is never (empty union), pattern won't match - skip pattern matching code.
        // The failure still carries a nil scrutinee through (see the failure path below): a
        // pattern that provably cannot match a nil is the degenerate case of failing against
        // one, not a different kind of failure.
        if self.is_never(result_type) {
            let carried = self.emit_match_failure(value_type);
            return Ok(carried.unwrap_or_else(|| self.program.register_type(Type::nil())));
        }

        // Register locals for all bindings (indices needed for Load)
        for (variable_name, variable_type) in &bindings {
            let local_index = self.local_count;
            self.local_count += 1;

            // Register in scope
            // For simple identifier bindings (single binding), preserve the value's provenance
            // so tuple field provenance is preserved. For complex patterns (destructuring),
            // use Unknown since path resolution is complex.
            let var_provenance = if bindings.len() == 1 {
                value_provenance.clone()
            } else {
                Provenance::Unknown
            };

            if let Some(scope) = self.scopes.last_mut() {
                scope.bindings.variables.insert(
                    variable_name.clone(),
                    Variable {
                        ty: *variable_type,
                        index: local_index,
                        provenance: var_provenance,
                    },
                );
            }
        }

        // Generate pattern matching code (Store instructions push locals)
        // Use on_no_match if provided (for receive blocks), otherwise use fail_jump_addr
        let fail_target = on_no_match.unwrap_or(fail_jump_addr);
        pattern::generate_pattern_code(
            &mut self.codegen,
            self.program,
            &self.scopes,
            &binding_sets,
            fail_target,
        )?;

        // Apply narrowing to the matched value's provenance if the pattern narrows the type.
        // This is done here on the success path - the type has been narrowed by the pattern.
        // Note: narrowed_type is the success-narrowed type from analyze_pattern — without the
        // failure nil that widens result_type for fallible patterns.
        // Only where the verdict gates control flow: in a value chain (a tuple field)
        // the code after the match runs on failure too, so the narrowed fact doesn't
        // hold there — `[x ~> =T[_], x ~> f]` must compile `f` against x's full type.
        if gating && !self.is_never(narrowed_type) && !self.is_nil(narrowed_type) {
            apply_narrowing(
                &mut self.scopes,
                &value_provenance,
                narrowed_type,
                self.program,
            );
        }

        // Record the narrowing for complement narrowing in blocks.
        // This must happen even when narrowed_type is nil, so that subsequent branches
        // know the value is NOT nil (complement narrowing).
        if !self.is_never(result_type)
            && let Some(n) = narrowing
        {
            // For tuple patterns, check if we should record field-specific narrowing
            // instead of whole-value narrowing.
            // This enables patterns like `=[Nil, ys]` to narrow the first field
            // so subsequent branches know the first field is NOT Nil.
            let field_complement_info =
                analyze_tuple_pattern_for_complement(&pattern, value_type, self.program).and_then(
                    |(field_idx, constrained_type)| {
                        // Use narrowed field type if available (from complement of previous branch),
                        // otherwise fall back to static field type from tuple definition.
                        // This ensures that for subsequent branches, the "original" type
                        // is the narrowed type, so complement calculation is correct.
                        // (Sequential, not an `or_else` closure: `get_field_type` now borrows
                        // `&mut self.program`, which can't be captured alongside `&self.scopes`.)
                        let original =
                            match get_field_narrowing(&self.scopes, &value_provenance, field_idx) {
                                Some(t) => Some(t),
                                None => get_field_type(value_type, field_idx, self.program),
                            }?;
                        Some((field_idx, original, constrained_type))
                    },
                );

            if let Some((field_idx, original_field_type, constrained_type)) = field_complement_info
            {
                // Record field-specific narrowing for complement
                let field_provenance =
                    Provenance::Field(Box::new(value_provenance.clone()), field_idx);
                n.record(
                    &field_provenance,
                    original_field_type,
                    constrained_type,
                    self.program,
                );
            } else {
                // Standard whole-value narrowing
                n.record(&value_provenance, value_type, narrowed_type, self.program);
            }
        }

        // Success path: replace the matched value with the Ok verdict
        self.codegen.add_instruction(Instruction::pop());
        self.codegen.add_instruction(Instruction::tuple(OK));
        let success_jump_addr = self.codegen.emit_jump_placeholder();

        // Only patch fail_jump_addr if we didn't use on_no_match
        if on_no_match.is_none() {
            self.codegen.patch_jump_to_here(fail_jump_addr);

            // The success path stored a local for each binding (sequential `Store`), so a
            // fall-through failure must allocate the same locals — filled with nil, since nothing
            // matched — to keep local indices aligned for whatever follows this match in the
            // chain. (Without this, a `=('int)x` that fails at runtime mid-chain would leave the
            // continuation's `Load`s reading shifted slots.) When `on_no_match` is set the failure
            // jumps elsewhere and resets locals, so no fill is needed.
            for _ in 0..bindings.len() {
                self.codegen.add_instruction(Instruction::tuple(NIL));
                self.codegen.add_instruction(Instruction::store());
            }
        }
        let carried_nil = self.emit_match_failure(value_type);

        self.codegen.patch_jump_to_here(success_jump_addr);

        // Compute the final type — the verdict replacing the matched (success) type with Ok.
        // `result_type` is the matched portion, already widened with nil when the match can fail.
        // A `result_type` that is *exactly* nil has no success component: the match can never
        // succeed. When the value itself can't be nil this means the pattern is unsatisfiable, so
        // the term is statically dead (nil) — emitting `Ok | []` here would wrongly keep a dead
        // branch alive. (If the value can be nil the pattern matches that nil value, so it stays a
        // real success.)
        // A verdict's `Ok` is freshly minted, so its row is exact-empty: provably
        // annotation-free. Its nil is not, in general — a failure against a nil scrutinee
        // re-emits that scrutinee (see the failure path above), so the verdict's nil is
        // either a fresh one (a shape mismatch) or whichever nil members the scrutinee could
        // have been, payload rows intact.
        //
        // Nil in `result_type` is not by itself fallibility: a bare binder (`=x`) on a
        // nil-typed value *matches* the nil and binds it. A pattern is irrefutable when
        // some binding set has no runtime requirements — it types as plain `Ok`.
        let irrefutable = pattern::is_irrefutable(&binding_sets);
        let final_type = if self.is_nil(result_type) && !self.contains_nil(value_type) {
            result_type
        } else if self.contains_nil(result_type) && !irrefutable {
            let closed_ok = annotations::closed_ok(self.program);
            let closed_nil = annotations::closed_nil(self.program);
            let mut members = vec![closed_ok, closed_nil];
            members.extend(carried_nil);
            typing::union_type_ids(self.program, members)
        } else {
            annotations::closed_ok(self.program)
        };

        Ok(final_type)
    }

    /// Compile a block in its own scope: store the incoming value as the scope parameter, then
    /// evaluate each `|` branch (re-loading the parameter) until one yields non-nil. Used for
    /// every braced form — block terms, function bodies, and interpolation holes.
    fn compile_scoped_block(
        &mut self,
        mut block: ast::Block,
        parameter_type: usize,
        parameter_provenance: Provenance,
        on_no_match: Option<usize>,
        scope_kind: ScopeKind,
        is_function_body: bool,
    ) -> Result<usize, Error> {
        // A block's annotation prefix attaches to the block's *result*; it is compiled at the
        // convergence point below. (A function body's annotations attach to the closure and
        // are extracted by `compile_function` before it gets here.)
        let block_annotations = std::mem::take(&mut block.annotations);

        // Debug builds: the site a block-exhaustion stamp points at — the first branch's
        // start, standing in for the block itself (blocks carry no span of their own).
        let block_site_span = block
            .branches
            .first()
            .and_then(|branch| branch.condition.chains().next())
            .and_then(|chain| chain.span.get());

        // Take ownership of the dispatch collection for the duration of this (outermost
        // function-body) block, so nested blocks — compiled via recursive calls with
        // `is_function_body == false` — do not collect into it.
        let mut dispatch = if is_function_body {
            self.collected_dispatch.take()
        } else {
            None
        };

        let mut next_branch_jumps = Vec::new();
        let mut end_jumps = Vec::new();
        let mut branch_types = Vec::new();
        let mut branch_starts = Vec::new();
        // The nil-shaped members (rows preserved) of the last branch's condition type:
        // when no branch matches, the block's runtime result is the last failing
        // condition's nil value, so these type the fall-through precisely.
        let mut last_condition_nils: Vec<usize> = Vec::new();

        // Record locals count before block
        let locals_before = self.local_count;

        // Allocate local for block parameter
        let param_local = self.local_count;
        self.local_count += 1;

        // Store parameter from stack
        self.codegen.add_instruction(Instruction::store());

        // Push new scope with parameter
        self.scopes.push(Scope::new(
            Bindings::default(),
            Some(scopes::Parameter {
                ty: parameter_type,
                index: param_local,
                provenance: parameter_provenance,
            }),
            scope_kind,
        ));

        // Negative narrowing information accumulated across previous branches: reaching a
        // branch means none of the earlier patterns matched, so each entry records "this
        // provenance is no longer compatible with an earlier branch's pattern". Keyed by
        // provenance; a later branch refining the same provenance replaces its entry.
        // Accumulating (rather than carrying only the previous branch's complement) lets
        // per-element tuple narrowings from non-adjacent branches survive.
        let mut accumulated_complements: Vec<(Provenance, usize)> = Vec::new();

        // Parameter guards of the branches that *faithfully* cover their type — i.e. whose
        // pattern narrows structurally (so its complement is a real type). Value-pattern
        // branches (`=A[1]`, `=5`, `=&y`, partials) are excluded: their guard is the whole
        // variant type but at runtime they match only a single value, so they don't actually
        // cover that region. Used to compute the uncovered (fall-through-to-nil) region for a
        // non-exhaustive enumeration's synthetic dispatch branch; see below.
        let mut faithfully_covered: Vec<usize> = Vec::new();

        // Track whether the block exhaustively covers all type variants.
        // Assume not exhaustive until proven otherwise by complement narrowing.
        let mut is_exhaustive = false;

        for (i, branch) in block.branches.iter().enumerate() {
            let is_last_branch = i == block.branches.len() - 1;

            // A branch contributes to the dispatch table only if it is a pure parameter
            // dispatch; any other branch shape invalidates the whole table.
            let dispatch_branch = dispatch.is_some() && branch_is_parameter_dispatch(branch);
            if dispatch.is_some()
                && !dispatch_branch
                && let Some(d) = &mut dispatch
            {
                d.valid = false;
            }

            branch_starts.push(self.codegen.instructions.len());

            if i > 0 {
                self.codegen.add_instruction(Instruction::pop());
                // Don't emit Clear here - branch variables are only allocated if the branch matches
                // So if we jump to this branch, the previous branch's variables were never allocated
                // Reset local count to after parameter (locals from previous branch are "forgotten")
                self.local_count = param_local + 1;
                // Clear scope bindings and narrowings from previous branch
                let scope = self.scopes.last_mut().ok_or_else(|| Error::InternalError {
                    message: "No scope available when compiling block branch".to_string(),
                })?;
                scope.bindings.clear();
                scope.narrowings = provenance::Narrowings::default();
                // The previous branch's CSE slots point at locals that were never
                // allocated on this branch's path (or were reset) — drop them with the
                // bindings, for the same reason.
                scope.cse_slots.clear();

                // Re-apply the complement narrowings accumulated from ALL previous branches.
                // Applying every accumulated complement — not just the immediately preceding
                // branch's — lets per-element tuple narrowings from non-adjacent branches
                // survive: matching `=[Rational[..], _]` then `=[_, Rational[..]]` narrows
                // both elements to 'int in a final `=[a, b]` branch.
                for (prov, complement) in &accumulated_complements {
                    apply_narrowing(&mut self.scopes, prov, *complement, self.program);
                }
            }

            // Create a narrowing instance for this branch's condition. Only a condition
            // whose fall-through proves its pattern didn't match may record a complement
            // (see `condition_complement_faithful`) — a guarded condition falls through
            // on a failed *guard* too, so its pattern's complement (and coverage) must
            // not narrow subsequent branches.
            let mut narrowing = Narrowing::new();
            let complement_faithful = condition_complement_faithful(&branch.condition);

            // Compile the condition expression - it can use ~> to access the parameter
            // We need both the type and provenance for forward narrowing
            let (condition_type, condition_prov) = self.compile_sequence(
                branch.condition.clone(),
                on_no_match,
                complement_faithful.then_some(&mut narrowing),
            )?;

            if is_last_branch {
                last_condition_nils = annotations::nil_members(self.program, condition_type);
            }

            // Capture this branch's parameter guard (now that the condition's pattern has
            // narrowed it) and the branch_types length, so we can pair the guard with the
            // result type pushed below.
            let branch_guard = if dispatch_branch {
                Some(self.branch_parameter_guard(branch))
            } else {
                None
            };
            let branch_types_before = branch_types.len();

            // If complement narrowing is valid, compute the complement for the exhaustiveness
            // check and (for non-last branches) accumulate it to narrow subsequent branches.
            if let Some((prov, original, narrowed)) = narrowing.take() {
                // This branch narrowed structurally (its narrowing wasn't disabled by a value
                // requirement), so it faithfully covers its guard. Record the guard so the
                // uncovered region can be computed as the complement of these — never the
                // over-broad guards of value-pattern branches.
                if let Some(guard) = branch_guard {
                    faithfully_covered.push(guard);
                }
                let complement = compute_complement(original, narrowed, self.program);
                if self.is_never(complement) {
                    // All variants covered - block is exhaustive
                    is_exhaustive = true;
                } else if !is_last_branch {
                    // Accumulate (or refine) this provenance's complement. It was computed
                    // against the already-accumulated narrowing for this provenance, so
                    // replacing any prior entry for the same provenance keeps it correct.
                    if let Some(entry) =
                        accumulated_complements.iter_mut().find(|(p, _)| *p == prov)
                    {
                        entry.1 = complement;
                    } else {
                        accumulated_complements.push((prov, complement));
                    }
                }
            }
            // A branch that records no narrowing of its own leaves the accumulated complements
            // untouched; they persist to subsequent branches automatically.

            // If condition is compile-time NIL (won't match), skip this branch entirely
            if self.is_nil(condition_type) {
                // Only include nil in result type if this is the last branch
                // Otherwise, nil causes fallthrough to the next branch
                if is_last_branch {
                    branch_types.push(condition_type);
                }
                // A statically-dead branch is an unusual shape for a dispatch function; be
                // conservative and abandon the case table rather than reason about it.
                if let Some(d) = &mut dispatch {
                    d.valid = false;
                }
                continue;
            }

            if let Some(ref consequence) = branch.consequence {
                // Branch has a consequence - compile it
                // Use emit_duplicate_jump_if_nil (without pop) to keep the value on stack for consequence
                let next_branch_jump = self.codegen.emit_duplicate_jump_if_nil();

                // Track: jump address, target branch index, whether cleanup needed
                // Cleanup resets locals to branch start (param_local + 1)
                let needs_cleanup = self.local_count > param_local + 1;
                next_branch_jumps.push((next_branch_jump, i + 1, needs_cleanup));

                // Apply forward narrowing: if condition succeeded (non-nil), narrow to exclude nil.
                // This enables type refinement like { a => %num.add[a, 1] } when a: [] | int.
                if self.contains_nil(condition_type) {
                    // Narrow the source value if it has trackable provenance. Narrow it to exclude
                    // nil from *its own current type* — not from `condition_type`. For a plain
                    // truthiness test (`a`) these coincide. For a `=PATTERN` match they don't: the
                    // condition value is the verdict `Ok | []`, whose truthy part `Ok` is unrelated
                    // to the matched value, so narrowing the provenance to `Ok` would collapse it to
                    // never. `compile_match` has already narrowed the provenance to the matched
                    // type, so dropping nil from that current type is the correct refinement.
                    if !matches!(condition_prov, Provenance::Unknown) {
                        let current =
                            get_type_for_provenance(&self.scopes, &condition_prov, self.program);
                        let truthy_type = self.without_nil(current);
                        if !self.is_never(truthy_type) {
                            apply_narrowing(
                                &mut self.scopes,
                                &condition_prov,
                                truthy_type,
                                self.program,
                            );
                        }
                    }
                    // Note: bindings made during the condition are deliberately NOT
                    // blanket-narrowed here. Reaching the consequence proves the
                    // condition's *result* was non-nil, which says nothing about a
                    // bare-binder binding (`=x` succeeds even on nil — the verdict is Ok
                    // either way). The pattern analysis already types each binding for
                    // the success path, and the provenance-based narrowing above covers
                    // the case where the condition result *is* a variable.
                }

                // Pop the condition result - consequence starts fresh with block parameter
                self.codegen.add_instruction(Instruction::pop());

                // Consequence is a new chain that starts with the block's parameter value
                // (not the condition's result). Every chain implicitly starts with the
                // surrounding block's parameter. Pass None for input_type so the consequence
                // loads the parameter via implicit_continuation.
                let (consequence_type, _) = self.compile_sequence(
                    consequence.clone(),
                    None,
                    None, // No narrowing for consequence
                )?;
                branch_types.push(consequence_type);

                // If this is the last branch and condition always succeeds (non-nil), block is exhaustive
                if is_last_branch && !self.contains_nil(condition_type) {
                    is_exhaustive = true;
                }

                // Reset to branch start (param_local + 1), keeping just the parameter
                // This is on the success path - bindings have been stored and should be cleared
                if self.local_count > param_local + 1 {
                    self.codegen
                        .add_instruction(Instruction::reset(param_local + 1));
                }
            } else {
                // No consequence - use condition type
                // For non-last branches, filter out nil since it causes fallthrough to next branch
                if is_last_branch {
                    branch_types.push(condition_type);
                    // If the last branch's condition never returns nil, block is exhaustive
                    if !self.contains_nil(condition_type) {
                        is_exhaustive = true;
                    }
                } else {
                    // Only include non-nil types - nil will be handled by subsequent branches
                    if let Some(ty) = self.program.lookup_type(condition_type) {
                        match ty {
                            Type::Union(types) => {
                                let non_nil_types: Vec<usize> = types
                                    .iter()
                                    .filter(|&type_id| !self.is_nil(*type_id))
                                    .copied()
                                    .collect();
                                if !non_nil_types.is_empty() {
                                    branch_types
                                        .push(typing::union_type_ids(self.program, non_nil_types));
                                }
                            }
                            _ if !self.is_nil(condition_type) => {
                                branch_types.push(condition_type);
                            }
                            _ => {
                                // Condition is purely nil - will always fall through
                            }
                        }
                    }
                }

                // Reset to branch start (param_local + 1), keeping just the parameter
                // Reset is safe regardless of whether condition succeeded: if pattern matching
                // failed (returned nil), no bindings were stored so Reset is a no-op.
                if self.local_count > param_local + 1 {
                    self.codegen
                        .add_instruction(Instruction::reset(param_local + 1));
                }
            }

            // Record this branch's (guard, result) for the dispatch table. Exactly one result
            // type is pushed for a live branch; if the count didn't grow by one (e.g. a
            // non-last branch whose condition was purely nil), abandon the table.
            if let Some(d) = &mut dispatch {
                match branch_guard {
                    Some(guard) if branch_types.len() == branch_types_before + 1 => {
                        d.branches.push((guard, branch_types[branch_types_before]));
                    }
                    _ => d.valid = false,
                }
            }

            if !is_last_branch {
                if branch.consequence.is_some() {
                    let end_jump = self.codegen.emit_jump_placeholder();
                    end_jumps.push(end_jump);
                } else {
                    self.codegen.add_instruction(Instruction::duplicate());
                    let success_jump = self.codegen.emit_jump_if_placeholder();
                    end_jumps.push(success_jump);
                }
            }
        }

        // An annotation-only block (`{ :error X }`) has no branches: it is identity — yield
        // the parameter unchanged — plus the attach compiled at the convergence below.
        if block.branches.is_empty() {
            self.codegen.add_instruction(Instruction::load(param_local));
            branch_types.push(parameter_type);
            is_exhaustive = true;
        }

        // Reset to clear the parameter (branches have already reset their specific locals)
        // Save address for end_jumps patching
        let param_clear_addr = self.codegen.instructions.len();
        self.codegen
            .add_instruction(Instruction::reset(locals_before));

        // Emit cleanup blocks for branches that need to reset locals before jumping.
        // A cleanup is only needed when the target is a next branch or an on_no_match
        // handler; a fall-through to the param clear is truncated by its own Reset.
        let has_cleanup_blocks = next_branch_jumps
            .iter()
            .any(|(_, next_idx, needs_cleanup)| {
                *needs_cleanup && (*next_idx < branch_starts.len() || on_no_match.is_some())
            });

        // If there are cleanup blocks, emit a jump to skip them on the success path
        let skip_cleanup_jump = if has_cleanup_blocks {
            Some(self.codegen.emit_jump_placeholder())
        } else {
            None
        };

        // Process each branch jump
        for (jump_addr, next_branch_idx, needs_cleanup) in next_branch_jumps {
            // Target: the next branch, the on_no_match handler, or — with no handler —
            // the param-clear Reset. Routing the fall-through-to-nil through the Reset
            // truncates the block's locals exactly like the success paths do; it used to
            // land *after* the Reset, leaving the parameter's slot live and every later
            // local index skewed against compile-time numbering (so a subsequent Store
            // landed one slot high, and later Loads read stale values).
            let target_addr = if next_branch_idx < branch_starts.len() {
                branch_starts[next_branch_idx]
            } else if let Some(addr) = on_no_match {
                addr
            } else {
                param_clear_addr
            };

            // The param clear's `Reset(locals_before)` subsumes a branch cleanup's
            // `Reset(param_local + 1)`, so fall-through jumps go direct.
            if needs_cleanup && target_addr != param_clear_addr {
                // Emit cleanup block: Reset locals to branch start, then jump to target
                let cleanup_addr = self.codegen.instructions.len();
                self.codegen
                    .add_instruction(Instruction::reset(param_local + 1));
                self.codegen.emit_jump_to_addr(target_addr);

                // Patch original jump to point to cleanup block
                self.codegen.patch_jump_to_addr(jump_addr, cleanup_addr);
            } else {
                self.codegen.patch_jump_to_addr(jump_addr, target_addr);
            }
        }

        // Patch the skip-cleanup jump to the convergence (past the cleanup blocks)
        if let Some(skip_jump) = skip_cleanup_jump {
            self.codegen.patch_jump_to_here(skip_jump);
        }

        // Patch end_jumps to go to param clear
        for jump_addr in end_jumps {
            self.codegen.patch_jump_to_addr(jump_addr, param_clear_addr);
        }

        // Debug builds: a non-exhaustive block's fall-through nil is a failure result —
        // stamp it here at the convergence. A nil already stamped at a finer site (a
        // failing condition step) keeps its origin; exhaustive blocks whose branch
        // *bodies* yield nil are covered by the step stamps inside those bodies.
        if !is_exhaustive {
            self.emit_stamp(
                block_site_span,
                quiver_core::bytecode::SiteKind::BlockExhausted,
            );
        }

        // All paths have converged with the block's result on the stack: attach the
        // annotation prefix to it (still inside the block's compile-time scope). The
        // fall-through nil (when not exhaustive) reaches the convergence too, so it is
        // part of the carrier. The carrier's members keep their own exactness.
        //
        // The convergence sits *after* the runtime `Reset(locals_before)`, so the
        // compile-time count must be wound back first — an annotation chain's own locals
        // (bindings, interpolation holes) number from `locals_before` to match the
        // truncated frame — and any locals the chains allocate are cleared again after.
        self.local_count = locals_before;
        // The runtime `Reset(locals_before)` above discarded every local the block
        // allocated, so the block scope's CSE slots (all at indices ≥ `locals_before`)
        // are dead — the attach chains compiled next must rebuild, not load them.
        if let Some(scope) = self.scopes.last_mut() {
            scope.cse_slots.clear();
        }
        let annotated_result = if block_annotations.is_empty() {
            None
        } else {
            let mut carrier_members = branch_types.clone();
            if !is_exhaustive {
                carrier_members.extend(last_condition_nils.clone());
                if last_condition_nils.is_empty() {
                    carrier_members.push(self.program.register_type(Type::nil()));
                }
            }
            let carrier_type = typing::union_type_ids(self.program, carrier_members);
            let annotated =
                self.compile_annotation_attach(block_annotations, carrier_type, false)?;
            if self.local_count > locals_before {
                self.codegen
                    .add_instruction(Instruction::reset(locals_before));
                self.local_count = locals_before;
            }
            Some(annotated)
        };

        // Pop scope
        self.scopes.pop();

        // The uncovered (fall-through-to-nil) region of a non-exhaustive enumeration. Computed
        // once and used both to synthesise a dispatch branch and to name the unhandled cases in
        // a `NonExhaustiveReturn` diagnostic. `None` when the block is exhaustive or isn't a
        // clean parameter-dispatch enumeration.
        let mut uncovered_region: Option<usize> = None;

        // If the block is not exhaustive, add nil to the result type (some inputs might
        // not match any branch). The fall-through value is the last failing condition's
        // nil, so its typed nil members (rows preserved) are used when available; the
        // bare nil remains for dispatch bookkeeping and as the fallback.
        if !is_exhaustive {
            let nil_type = self.program.register_type(Type::nil());
            if last_condition_nils.is_empty() {
                branch_types.push(nil_type);
            } else {
                branch_types.extend(last_condition_nils.iter().copied());
            }
            // Inputs matching no explicit branch fall through to nil. For a non-exhaustive
            // *enumeration* (every branch is a pure parameter dispatch) we model that
            // fall-through as a synthetic `uncovered -> nil` dispatch branch rather than
            // abandoning the table. This keeps return-type dispatch valid and precise: an
            // argument fully within the covered region infers its exact branch result, while an
            // argument that reaches the uncovered region (a genuinely unhandled variant, or a
            // nil flowing into an op that only enumerates the non-nil variants) picks up nil —
            // matching the runtime, which yields nil when no branch matches.
            //
            // The uncovered region is the complement of the *faithfully* covered guards, not of
            // every guard: a value-pattern branch (`=A[1]`) has an over-broad guard (`A['int]`)
            // but matches only one value, so its slack must remain in the fall-through set or we
            // would unsoundly type a non-matching argument as non-nil.
            let is_enumeration = dispatch.as_ref().is_some_and(|d| d.valid);
            if is_enumeration {
                let covered = typing::union_type_ids(self.program, faithfully_covered);
                let uncovered = compute_complement(parameter_type, covered, self.program);
                if !self.is_never(uncovered) {
                    uncovered_region = Some(uncovered);
                    dispatch
                        .as_mut()
                        .unwrap()
                        .branches
                        .push((uncovered, nil_type));
                }
            } else if let Some(d) = &mut dispatch {
                d.valid = false;
            }
        }

        if is_function_body {
            // The uncovered region (if any) names the unhandled cases for the return-type check.
            self.last_uncovered = uncovered_region;
            // Hand the dispatch collection back to the enclosing function body.
            self.collected_dispatch = dispatch;
        }

        // An annotation prefix replaces the result type with its annotated form (the
        // attach was compiled at the convergence point above).
        if let Some(annotated) = annotated_result {
            return Ok(annotated);
        }
        Ok(typing::union_type_ids(self.program, branch_types))
    }

    /// Debug builds: emit a failure-provenance stamp. Registers a site at `span` and a
    /// `Stamp` instruction that marks a fresh nil result with it at runtime (fresh-only,
    /// so a propagating failure keeps its original site). No-op in release builds or
    /// without a span.
    fn emit_stamp(&mut self, span: Option<SourceSpan>, kind: quiver_core::bytecode::SiteKind) {
        if !self.debug {
            return;
        }
        let Some(span) = span else {
            return;
        };
        let module_constant = self
            .program
            .register_constant(Constant::Binary(self.current_module.clone().into_bytes()));
        let site = self
            .program
            .register_debug_site(quiver_core::bytecode::Site {
                module_constant,
                line: span.line as u32,
                column: span.column as u32,
                kind,
            });
        self.codegen.add_instruction(Instruction::stamp(site));
    }

    /// Whether the callee's static type carries definite `:pre`/`:post` contract entries.
    /// Contracts flow with the value's row through inferred boundaries; a declared
    /// boundary (a function parameter) erases the row, so a contract is enforced only
    /// where it is statically visible — consistent with annotation retrieval.
    fn contract_presence(&self, type_id: usize, pre_key: usize, post_key: usize) -> (bool, bool) {
        match self.program.lookup_type(type_id) {
            Some(Type::Annotated { entries, .. }) => {
                let has = |key| entries.binary_search_by_key(&key, |(k, _)| *k).is_ok();
                (has(pre_key), has(post_key))
            }
            Some(Type::Union(members)) if members.len() == 1 => {
                self.contract_presence(members[0], pre_key, post_key)
            }
            _ => (false, false),
        }
    }

    /// Emit a function call (`Instruction::call()`), wrapping it with debug-mode `:pre`/
    /// `:post` contract enforcement when the callee's static type carries those keys. The
    /// contract functions are fetched from the closure value itself, applied, and a nil
    /// verdict aborts via `__panic__`. Release builds — and callees with no visible
    /// contract — emit a plain call.
    ///
    /// The pre-contract `#P -> ok?` is applied to a copy of the argument; the post-contract
    /// `#[in: P, out: R] -> ok?` to a copy of `[in: argument, out: result]`. Stack on
    /// entry is `[argument, callable]` and on exit `[result]`, matching a plain call.
    fn emit_call_with_contracts(
        &mut self,
        target_type_id: usize,
        parameter: usize,
        result: usize,
    ) -> Result<(), Error> {
        let (has_pre, has_post) = if self.debug {
            let pre_key = annotations::intern_key(self.program, annotations::PRE);
            let post_key = annotations::intern_key(self.program, annotations::POST);
            self.contract_presence(target_type_id, pre_key, post_key)
        } else {
            (false, false)
        };
        if !has_pre && !has_post {
            self.codegen.add_instruction(Instruction::call());
            return Ok(());
        }
        let pre_key = annotations::intern_key(self.program, annotations::PRE);
        let post_key = annotations::intern_key(self.program, annotations::POST);
        // The `[in: P, out: R]` argument the post-contract receives. Field types are
        // irrelevant at runtime, but describe the contract's parameter honestly.
        let in_out_tuple = self.program.register_tuple(
            None,
            vec![
                (Some("in".to_string()), parameter),
                (Some("out".to_string()), result),
            ],
        );

        // Stash a copy of the argument and the post-contract *beneath* `[arg, callable]`
        // so they survive the call (which consumes both). Two `Rotate(4)`s slide the two
        // fresh copies under the pair: `[arg, callable, arg_c, post_fn]` -> `[arg_c,
        // post_fn, arg, callable]`.
        if has_post {
            self.codegen.add_instruction(Instruction::pick(1));
            self.codegen.add_instruction(Instruction::pick(1));
            self.codegen
                .add_instruction(Instruction::get_annotation(post_key));
            self.codegen.add_instruction(Instruction::rotate(4));
            self.codegen.add_instruction(Instruction::rotate(4));
        }

        // Pre-check: apply the pre-contract to a copy of the argument, leaving the
        // `[arg, callable]` pair (on top) untouched for the real call.
        if has_pre {
            self.codegen.add_instruction(Instruction::pick(1));
            self.codegen.add_instruction(Instruction::pick(1));
            self.codegen
                .add_instruction(Instruction::get_annotation(pre_key));
            self.codegen.add_instruction(Instruction::call());
            self.emit_contract_verdict("Precondition")?;
        }

        // The real call: `[.., arg, callable]` -> `[.., result]`.
        self.codegen.add_instruction(Instruction::call());

        // Post-check: build `[in: arg_c, out: result]` from the stashed argument and the
        // result, apply the post-contract, then drop the two stashed values, leaving
        // `[result]`.
        if has_post {
            self.codegen.add_instruction(Instruction::pick(2)); // arg_c
            self.codegen.add_instruction(Instruction::pick(1)); // result
            self.codegen
                .add_instruction(Instruction::tuple(in_out_tuple));
            self.codegen.add_instruction(Instruction::pick(2)); // post_fn
            self.codegen.add_instruction(Instruction::call());
            self.emit_contract_verdict("Postcondition")?;
            self.codegen.emit_rotate_pop(3); // drop arg_c
            self.codegen.emit_rotate_pop(2); // drop post_fn
        }
        Ok(())
    }

    /// Emit the verdict check for a contract: the verdict is on top of the stack; a non-nil
    /// verdict (the contract holds) is popped and execution continues, while a nil verdict
    /// falls through to a `__panic__` naming the call site.
    fn emit_contract_verdict(&mut self, label: &str) -> Result<(), Error> {
        let ok = self.codegen.emit_jump_if_placeholder();
        let location = self
            .current_span
            .map(|span| format!(" at {}:{}:{}", self.current_module, span.line, span.column))
            .unwrap_or_default();
        self.emit_panic(&format!("{label} violated{location}"))?;
        self.codegen.patch_jump_to_here(ok);
        Ok(())
    }

    /// Emit an unconditional abort: push `message` as a `Str['bin]` and apply `__panic__`,
    /// which never returns (so the surrounding stack state past this point is unreachable).
    fn emit_panic(&mut self, message: &str) -> Result<(), Error> {
        let index = self
            .program
            .register_constant(Constant::Binary(message.as_bytes().to_vec()));
        self.codegen.add_instruction(Instruction::constant(index));
        let binary_type = self.program.register_type(Type::Binary);
        let str_tuple = self
            .program
            .register_tuple(Some("Str".to_string()), vec![(None, binary_type)]);
        self.codegen.add_instruction(Instruction::tuple(str_tuple));
        self.compile_builtin("panic", &[], true)?;
        self.codegen.add_instruction(Instruction::call());
        Ok(())
    }

    /// Compile a step-final `//=> P` assertion. The step's value is on top of the stack and is
    /// left there untouched: the assertion observes, so a nil step still short-circuits and the
    /// value's flow is identical across build modes. The pattern is analyzed in both modes — it
    /// may not bind, and one that can never match is a stale expectation, rejected — but checks
    /// are emitted only in debug builds, where a mismatch aborts like a violated contract.
    fn compile_assertion(
        &mut self,
        assertion: &ast::Assertion,
        value_type: usize,
        value_provenance: &Provenance,
    ) -> Result<(), Error> {
        let mut binding_spans = Vec::new();
        collect_binding_spans(&assertion.pattern, &mut binding_spans);
        if !binding_spans.is_empty() {
            return Err(Error::AssertionBindings {
                bindings: binding_spans.into_iter().map(|(name, _)| name).collect(),
            });
        }
        let mut env = typing::TypeEnv {
            resolver: self.resolver,
            module_cache: &mut *self.module_cache,
            package: &self.current_package,
        };
        let (bindings, binding_sets, result_type, _) = pattern::analyze_pattern(
            &mut env,
            self.program,
            &assertion.pattern,
            value_type,
            &self.scopes,
            value_provenance,
        )?;
        // The syntactic check above names dead-alternative binders too; this one catches
        // bindings with no syntactic site of their own (a star pattern's are type-derived) —
        // which would otherwise emit `Store`s for locals never registered.
        if !bindings.is_empty() {
            return Err(Error::AssertionBindings {
                bindings: bindings.into_iter().map(|(name, _)| name).collect(),
            });
        }
        if self.is_never(result_type) {
            return Err(Error::PatternNoMatchingTypes {
                pattern: crate::format::render_match(&assertion.pattern),
            });
        }
        if !self.debug {
            return Ok(());
        }
        // Mirror `compile_match`'s layout: a trampoline gives the requirement checks a fixed
        // failure address to jump to; the success path falls through with the value untouched.
        let start_jump = self.codegen.emit_jump_placeholder();
        let fail_jump = self.codegen.emit_jump_placeholder();
        self.codegen.patch_jump_to_here(start_jump);
        pattern::generate_pattern_code(
            &mut self.codegen,
            self.program,
            &self.scopes,
            &binding_sets,
            fail_jump,
        )?;
        let ok_jump = self.codegen.emit_jump_placeholder();
        self.codegen.patch_jump_to_here(fail_jump);
        let location = assertion
            .span
            .get()
            .map(|span| format!(" at {}:{}:{}", self.current_module, span.line, span.column))
            .unwrap_or_default();
        self.emit_panic(&format!(
            "Assertion '{}' failed{location}",
            crate::format::render_match(&assertion.pattern)
        ))?;
        self.codegen.patch_jump_to_here(ok_jump);
        Ok(())
    }

    /// Compile a sequence of steps, short-circuiting to nil if any yields nil.
    ///
    /// Every step starts from the **block value** — the enclosing block's parameter, or the value
    /// piped into it — not from the previous step's result. That makes steps consistent with the
    /// two places that already worked this way: a block's branches, and a `=>` consequence (which
    /// never received its condition's result either). What a step's result still does is gate the
    /// boundary: nil ends the sequence, and the sequence evaluates to that nil.
    fn compile_sequence(
        &mut self,
        sequence: ast::Sequence,
        on_no_match: Option<usize>,
        mut narrowing: Option<&mut Narrowing>,
    ) -> Result<(usize, Provenance), Error> {
        // The sequence's result/short-circuit type, accumulated across chains.
        let mut last_type = None;
        let mut last_prov = Provenance::Unknown;
        let mut end_jumps = Vec::new();
        // Type aliases are transparent to the flow, so the step boundary is indexed by *chain*
        // step: the last chain keeps its result (the sequence's value) rather than
        // short-circuiting, and a trailing alias must not make the chain before it look non-final.
        let last_chain_index = sequence
            .steps
            .iter()
            .rposition(|step| matches!(step, ast::Step::Chain(_)));
        let mut chain_ordinal = 0usize;

        if last_chain_index.is_none() {
            // Only type aliases: nothing produces a value, so the sequence is nil — the same
            // answer a sequence gives when it runs out of steps. The aliases still register (a
            // block may exist purely to scope one over an annotation), and nil must be pushed
            // because the caller expects the sequence to leave its result on the stack.
            for step in &sequence.steps {
                let ast::Step::TypeAlias {
                    name,
                    type_parameters,
                    type_definition,
                    ..
                } = step
                else {
                    unreachable!("no chain steps")
                };
                self.compile_type_alias(
                    name.as_deref(),
                    type_parameters.clone(),
                    type_definition.clone(),
                )?;
            }
            self.codegen.add_instruction(Instruction::tuple(NIL));
            return Ok((self.program.register_type(Type::nil()), Provenance::Unknown));
        }

        for (step_index, step) in sequence.steps.iter().enumerate() {
            let chain = match step {
                ast::Step::Chain(chain) => chain,
                ast::Step::TypeAlias {
                    name,
                    type_parameters,
                    type_definition,
                    ..
                } => {
                    // Registers the alias in the current scope — no stack effect, and it does not
                    // participate in threading. Scoped to the enclosing scope, so an alias in a
                    // block or function body is local to it (and cleared between branches, like
                    // any other binding).
                    self.compile_type_alias(
                        name.as_deref(),
                        type_parameters.clone(),
                        type_definition.clone(),
                    )?;
                    continue;
                }
            };
            let is_first_chain = chain_ordinal == 0;
            let is_last_chain = Some(step_index) == last_chain_index;
            chain_ordinal += 1;
            let (chain_type, chain_prov) = self.compile_chain_with_input(
                chain.clone(),
                on_no_match,
                None,
                None, // every step starts from the block value, not the previous step's result
                narrowing.as_deref_mut(),
                true, // implicit_continuation: load the block parameter
                None,
                true, // a step's nil short-circuits the sequence: its result gates
            )?;

            // Debug builds: a nil step result is a failure — stamp it with this step's
            // provenance (a no-op for non-nil results and already-stamped nils). Steps
            // whose type excludes nil skip the instruction entirely.
            if self.debug && self.contains_nil(chain_type) {
                let kind = if chain.binding.is_some()
                    || chain.terms.iter().any(|t| matches!(t, ast::Term::Match(_)))
                {
                    quiver_core::bytecode::SiteKind::NoMatch
                } else {
                    quiver_core::bytecode::SiteKind::NilResult
                };
                self.emit_stamp(chain.span.get(), kind);
            }

            // Step-final `//=> P` assertions observe the step's value in place.
            for assertion in &chain.assertions {
                self.compile_assertion(assertion, chain_type, &chain_prov)?;
            }

            // If a prior chain could short-circuit to nil, the sequence's result includes
            // that nil — the *same* value, so its typed nil members (annotation rows
            // preserved) carry over rather than a fresh bare nil.
            let should_propagate_nil =
                !is_first_chain && last_type.as_ref().is_some_and(|&t| self.contains_nil(t));
            last_type = Some(if should_propagate_nil {
                let mut members = annotations::nil_members(
                    self.program,
                    last_type.expect("checked by should_propagate_nil"),
                );
                if members.is_empty() {
                    members.push(self.program.register_type(Type::nil()));
                }
                members.insert(0, chain_type);
                typing::union_type_ids(self.program, members)
            } else {
                chain_type
            });
            last_prov = chain_prov.clone();

            // If last_type is NIL, subsequent chains are unreachable - break early
            if let Some(last_type_id) = last_type
                && self.is_nil(last_type_id)
            {
                break;
            }

            if !is_last_chain {
                // Short-circuit to the end of the sequence if the result is nil — the jump keeps
                // that nil on the stack as the sequence's value. Otherwise discard it: the next
                // step starts from the block value, so nothing consumes it.
                let end_jump = self.codegen.emit_duplicate_jump_if_nil();
                end_jumps.push(end_jump);
                self.codegen.add_instruction(Instruction::pop());

                // Passing the short-circuit proves this chain's *result* was non-nil, so
                // narrow whatever the result's provenance tracks — the `=x, x, ...`
                // idiom, where a step re-emits a binding precisely to test it. This is
                // sound only when the chain's result IS the tracked value: a chain
                // containing a match yields the *verdict* while its provenance still
                // points at the matched value's source, and a bare binder's verdict is
                // Ok even when it bound nil (`e =x` binds any value, including `[]`) —
                // narrowing the source off the verdict typed nil-holding values non-nil
                // (a stale leftover from the pre-`=PAT` semantics, where a failing
                // chain-match itself short-circuited). To bind-and-guarantee non-nil in
                // one step, ascribe: `e =('int)x`.
                let result_is_tracked_value = chain.binding.is_none()
                    && !chain
                        .terms
                        .iter()
                        .any(|term| matches!(term, ast::Term::Match(_)));
                if result_is_tracked_value
                    && self.contains_nil(chain_type)
                    && !matches!(last_prov, Provenance::Unknown)
                {
                    let current = get_type_for_provenance(&self.scopes, &last_prov, self.program);
                    let truthy_type = self.without_nil(current);
                    if !self.is_never(truthy_type) {
                        apply_narrowing(&mut self.scopes, &last_prov, truthy_type, self.program);
                    }
                }
            }
        }

        let end_addr = self.codegen.instructions.len();
        for jump_addr in end_jumps {
            self.codegen.patch_jump_to_addr(jump_addr, end_addr);
        }

        let result_type = last_type.ok_or_else(|| Error::InternalError {
            message: "Sequence compiled with no chains".to_string(),
        })?;
        Ok((result_type, last_prov))
    }

    fn compile_chain(
        &mut self,
        chain: ast::Chain,
        on_no_match: Option<usize>,
        ripple_context: Option<&RippleContext>,
    ) -> Result<usize, Error> {
        let (ty, _prov) = self.compile_chain_with_provenance(chain, on_no_match, ripple_context)?;
        Ok(ty)
    }

    /// Compile a chain within a tuple field (no implicit continuation).
    /// The field chain has no initial input - it uses ripple_context for `~`.
    fn compile_chain_with_provenance(
        &mut self,
        chain: ast::Chain,
        on_no_match: Option<usize>,
        ripple_context: Option<&RippleContext>,
    ) -> Result<(usize, Provenance), Error> {
        // Tuple fields don't have implicit continuation - they start with no input.
        // Their result is data (nothing gates on it), so matches there don't narrow.
        self.compile_chain_with_input(
            chain,
            on_no_match,
            ripple_context,
            None,
            None,
            false,
            None,
            false,
        )
    }

    /// The type a chain term is expected to produce — the parameter type of whatever consumes its
    /// result. This is what lets an un-annotated function literal infer its parameter:
    /// - the **final** term's consumer is external, so the chain's own `chain_expected` applies;
    /// - any **earlier** term flows its result into the next term, so when that next term is a
    ///   looked-up callable, *its* parameter type is this term's expected type.
    ///
    /// It is the postfix counterpart of an `Apply` argument inferring from its callee: with
    /// application written `[args, #{…}] ~> f`, the `#{…}` infers from `f` exactly as `f [args,
    /// #{…}]` once did — the inference reads off the value flow rather than a bundled call node.
    /// Only `Tuple`/`Function` terms consume an expected type (every other term ignores it), so we
    /// only bother looking ahead for those.
    #[allow(clippy::too_many_arguments)]
    fn compile_chain_with_input(
        &mut self,
        chain: ast::Chain,
        on_no_match: Option<usize>,
        ripple_context: Option<&RippleContext>,
        input: Option<(usize, Provenance)>,
        mut narrowing: Option<&mut Narrowing>,
        implicit_continuation: bool,
        // The type the chain is expected to produce; flows to the final term (the chain's result)
        // so an un-annotated function-literal at the chain's tail can infer its parameter.
        expected: Option<usize>,
        // Whether this chain's result gates control flow — true for a sequence step
        // (its nil short-circuits the sequence, and as a branch condition it selects
        // the branch), false for a value chain (a tuple field, an argument, an
        // annotation value), whose result is data. A match's narrowing (bindings at
        // their success types, scrutinee refinement) is justified only where a failed
        // match prevents the downstream code from running, so it applies only in
        // gating chains; see the fallible-match checks in the term loop.
        gating: bool,
    ) -> Result<(usize, Provenance), Error> {
        // Determine initial value:
        // - If input_type is provided, use it (value already on stack)
        // - If implicit_continuation is true, load the parameter from scope
        // - Otherwise, no initial value (for tuple field chains)
        let (mut current_type, mut current_prov) = if let Some((input_type, input_prov)) = input {
            // Input provided (e.g., from a tuple field, consequence, or previous expression term)
            (Some(input_type), input_prov)
        } else if implicit_continuation {
            // Load the parameter from scope (implicit continuation)
            let (parameter_type, param_local) = scopes::get_parameter(&self.scopes)?;
            self.codegen.add_instruction(Instruction::load(param_local));
            (Some(parameter_type), Provenance::Parameter)
        } else {
            // No initial input (tuple field chains use ripple_context for ~)
            (None, Provenance::Unknown)
        };

        let terms: Vec<_> = chain.terms.into_iter().collect();
        let last_index = terms.len().saturating_sub(1);
        for (i, term) in terms.iter().enumerate() {
            // The statically-resolvable callable a non-final literal term flows into — the
            // piped counterpart of an Apply's head.
            let piped_callee = if i == last_index {
                None
            } else if matches!(term, ast::Term::Tuple(_) | ast::Term::Function(_))
                && let Some(next) = terms.get(i + 1)
            {
                match next {
                    ast::Term::Access(next) => Some(next),
                    // `x ~> f g`: the flow becomes `g`'s argument when `g` is a bare
                    // callable, so `g`'s parameter is the previous literal's expected
                    // type. A ripple head (`~ g`, `^~ g`) consumes the flow itself, so
                    // its argument receives nothing.
                    ast::Term::Apply(head, argument)
                        if !matches!(
                            head.source,
                            Some(ast::AccessSource::Ripple | ast::AccessSource::TailCallRipple)
                        ) =>
                    {
                        match argument.as_ref() {
                            ast::Term::Access(callable) => Some(callable),
                            _ => None,
                        }
                    }
                    _ => None,
                }
            } else {
                None
            };
            // Only the chain's final term produces the chain's value, so only it receives the
            // chain's expected type. An earlier literal term flows its result into the next
            // term, so a piped callable's parameter type is that literal's expected type. This
            // is what lets a piped tuple literal adopt omittable field labels and omit
            // defaulted fields, and a piped `#{…}` literal infer its parameter, exactly as
            // they do at `f [1, 2]` / `f [.., #{…}]`.
            let term_expected = match (i == last_index, piped_callee) {
                (true, _) => expected,
                (false, Some(callee)) => self.callee_parameter_type(callee),
                (false, None) => None,
            };
            // A piped literal fills omitted fields from the callee, as an argument does.
            let filled = match piped_callee {
                Some(callee) => self.fill_defaults(callee, term)?,
                None => None,
            };
            let (term_type, term_prov) = self.compile_term(
                filled.map_or_else(|| term.clone(), ast::Term::Tuple),
                FlowingValue {
                    ty: current_type,
                    provenance: current_prov,
                },
                on_no_match,
                ripple_context,
                narrowing.as_deref_mut(),
                term_expected,
                gating,
            )?;
            // A fallible match's narrowing is sound only where a failed match prevents
            // the downstream code from running, so:
            // - it must be the LAST term of its chain — its verdict then directly gates
            //   the step boundary (`=P; …`) or branch (`=P => …`). Nothing
            //   short-circuits within a chain, so a following term would run whether or
            //   not the match succeeded, observing narrowed facts that don't hold.
            // - in a value chain (gating == false) it must bind nothing — the verdict
            //   is data there (`[ok?: x ~> =T[_]]` is fine) and gates nothing, so its
            //   bindings could never be relied on. (`compile_match` also skips scrutinee
            //   narrowing for these.)
            // A receive filter (`on_no_match`) is exempt: its failure jumps out of the
            // chain entirely, so the continuation runs only on success.
            if on_no_match.is_none()
                && self.contains_nil(term_type)
                && let ast::Term::Match(m) = term
            {
                if i < last_index {
                    return Err(Error::FallibleMatchNotChainFinal);
                }
                if !gating {
                    let mut names = Vec::new();
                    collect_binding_spans(m, &mut names);
                    if !names.is_empty() {
                        return Err(Error::FallibleMatchBindingsInValueChain {
                            bindings: names.into_iter().map(|(n, _)| n).collect(),
                        });
                    }
                }
            }

            // Nil flows through a chain like any other value: within a chain, no term
            // short-circuits on nil (nil as *data* — e.g. an optional field — passes
            // unremarked; the only short-circuit is between `,`-separated chains,
            // handled in `compile_sequence`). So a term's full type — nil included —
            // flows onward, and a subsequent term that cannot accept nil is a genuine
            // type error.
            current_type = Some(term_type);
            current_prov = term_prov;
        }

        let result_type = current_type.ok_or_else(|| Error::InternalError {
            message: "Chain compiled with no terms and no continuation".to_string(),
        })?;

        // If there's a match pattern, apply it. Binding definitions for go-to-definition
        // and hover are recorded inside `compile_match`, which sees every binding site
        // (top-level `name = ...`, destructuring, mid-chain `=x`, and block branches).
        if let Some(pattern) = chain.binding {
            // Index destructured imports (`(double) = %util`) as references to the module's
            // members, before the pattern's own bindings are recorded.
            if let [term] = terms.as_slice() {
                self.record_destructured_import(term, &pattern);
            }
            let ty = self.compile_match(
                pattern,
                result_type,
                current_prov.clone(),
                on_no_match,
                narrowing,
                gating,
            )?;
            Ok((ty, current_prov))
        } else {
            Ok((result_type, current_prov))
        }
    }

    /// Record a destructured import (`(double) = %util`) as references to the module's members,
    /// so find-references on a member includes its destructure sites. Only the partial form
    /// `(a, b) = %module` (the import idiom) is recognized; the right-hand side must be a bare
    /// module import.
    fn record_destructured_import(&mut self, term: &ast::Term, pattern: &ast::Match) {
        if self.recorder.is_none() {
            return;
        }
        let ast::Term::Access(access) = term else {
            return;
        };
        let Some(ast::AccessSource::Import(module)) = &access.source else {
            return;
        };
        if !access.accessors.is_empty() {
            return;
        }
        let ast::Match::Partial(partial) = pattern else {
            return;
        };
        let Ok(resolved) = self.resolver.resolve(&self.current_package, module) else {
            return;
        };
        let ModuleOrigin::Path(module_path) = resolved.origin else {
            return;
        };
        for field in &partial.fields {
            if let (Some(span), Some(recorder)) =
                (field.name_span.get(), self.recorder.as_deref_mut())
            {
                recorder.record_import_member_ref(span, module_path.clone(), field.name.clone());
            }
        }
    }

    /// Resolve an import with optional accessor chain, returning the cached module, resolved
    /// value, type, and the module's origin (for go-to-definition). Emits no instructions.
    /// Fully compile (and cache) every module the parsed unit references by value,
    /// before the unit's own body compiles — link-before-compile. Each module's
    /// registrations then form a contiguous run in the program instead of
    /// interleaving with its importers', and a module's dispatch-table delta
    /// contains only its own contributions. `package` is the unit's own, so
    /// resolution stays hermetic.
    fn precompile_imports(
        &mut self,
        parsed: &ast::Sequence,
        package: &crate::resolver::PackageId,
    ) -> Result<(), Error> {
        for (path, span) in modules::collect_value_imports(parsed) {
            // Point errors (unresolvable module, failed module compile) at the
            // import term that names the module.
            self.current_span = span.get().or(self.current_span);
            self.ensure_import_cached(&path, package)?;
        }
        Ok(())
    }

    fn ensure_import_cached(
        &mut self,
        module: &[String],
        package: &crate::resolver::PackageId,
    ) -> Result<(), Error> {
        let resolved = self
            .resolver
            .resolve(package, module)
            .map_err(Error::ModuleLoad)?;
        self.try_link_from_artifacts(&resolved)?;
        if self.module_cache.get_cached_module(&resolved.id).is_some()
            || self.module_cache.import_stack.contains(&resolved.id)
        {
            // Already compiled — or currently compiling further up the stack, in
            // which case the in-body import reports the cycle exactly as before.
            return Ok(());
        }
        self.module_cache.import_stack.push(resolved.id.clone());
        let result = self.import_and_cache_module(&resolved);
        self.module_cache.import_stack.pop();
        result.map(|_| ())
    }

    /// If the session's artifact store holds an artifact under this module's key,
    /// link it instead of compiling from source; the module-cache lookup that
    /// follows then sees it as compiled. The module's declared value imports are
    /// ensured first (linked or compiled, either way their function maps are
    /// recorded) — the artifact's function imports are a subset of them.
    fn try_link_from_artifacts(
        &mut self,
        resolved: &crate::resolver::ResolvedModule,
    ) -> Result<(), Error> {
        if self.module_cache.get_cached_module(&resolved.id).is_some() {
            return Ok(());
        }
        let Some(store) = self.module_cache.artifact_store.clone() else {
            return Ok(());
        };
        let Some(key) = crate::artifact::key_for_resolved(
            self.resolver,
            self.module_cache,
            resolved,
            self.debug,
        ) else {
            // Unresolvable references or a cyclic graph: compile from source, which
            // reports the real error.
            return Ok(());
        };
        let Some(artifact) = store.load(key) else {
            return Ok(());
        };
        let parsed = self
            .module_cache
            .load_and_cache_ast(&resolved.id, &resolved.source)?;
        for (path, _) in modules::collect_value_imports(&parsed) {
            self.ensure_import_cached(&path, &resolved.package)?;
        }
        // The artifact must name the dependency content this session actually linked.
        // Equal source keys do not yet guarantee equal bytes — compiling against a
        // linked dependency emits differently than against a source-compiled one, and
        // concurrent sessions race their writes into a shared store — so a dependent
        // written beside a different variant of its dependency can arrive here. That is
        // a cache miss, not an error: fall through to the source compile, which builds
        // against what this session holds.
        for (module, key, _) in &artifact.unit.imports {
            if self.module_cache.content_key(module) != Some(*key) {
                return Ok(());
            }
        }
        crate::artifact::link_module(
            &artifact,
            &resolved.id,
            self.program,
            self.module_cache,
            self.builtins,
        )
    }

    fn resolve_import(
        &mut self,
        module: &[String],
        accessors: &[ast::AccessPath],
    ) -> Result<(modules::CachedModule, Value, usize, ModuleOrigin), Error> {
        let module_name = module.join("/");

        // Resolve the import against the package currently being compiled, yielding a canonical
        // id (used for caching and cycle detection) and the package its own imports resolve in.
        let resolved = self
            .resolver
            .resolve(&self.current_package, module)
            .map_err(Error::ModuleLoad)?;
        let id = resolved.id.clone();
        let origin = resolved.origin.clone();
        // An in-body reference the enclosing module compile did not declare (one only
        // a dialect expansion surfaces) marks that module hidden — uncacheable.
        self.module_cache.note_module_reference(&id);

        // Check for circular imports
        if self.module_cache.import_stack.contains(&id) {
            return Err(Error::FeatureUnsupported(
                "Circular import detected".to_string(),
            ));
        }

        self.try_link_from_artifacts(&resolved)?;

        // Get or compute cached module value
        let cached = if let Some(cached) = self.module_cache.get_cached_module(&id) {
            let cached = cached.clone();
            // The module won't be recompiled, so restore the return-type dispatch tables it
            // produced. Without this a freshly-compiled caller in this pass can't specialise the
            // result of the module's dispatch functions (e.g. `num.add`), silently widening their
            // result to the frozen type. Keep any entry this pass already established (a function
            // index is unique to one function, so `fn_case_tables` never genuinely conflicts).
            for (k, v) in &cached.fn_case_tables {
                self.fn_case_tables.entry(*k).or_insert_with(|| v.clone());
            }
            for (k, v) in &cached.case_tables {
                self.case_tables.entry(*k).or_insert(*v);
            }
            for (k, v) in &cached.callable_type_params {
                self.callable_type_params
                    .entry(*k)
                    .or_insert_with(|| v.clone());
            }
            cached
        } else {
            self.module_cache.import_stack.push(id.clone());
            let cached = self.import_and_cache_module(&resolved);
            self.module_cache.import_stack.pop();
            cached?
        };

        // Resolve accessor chain on the cached value
        let module_type_id = self.program.register_type(cached.module_type.clone());
        let (resolved_value, resolved_type) =
            self.resolve_accessors(&cached.value, &module_type_id, accessors, &module_name)?;

        Ok((cached, resolved_value, resolved_type, origin))
    }

    /// Compile an import with optional accessor chain.
    /// Resolves accessors statically on the cached module value, emitting only
    /// instructions needed for the resolved value. Explicit type arguments instantiate
    /// a type-consuming builtin member (`&%data.decode<'ev>`).
    fn compile_import(
        &mut self,
        module: &[String],
        accessors: &[ast::AccessPath],
        type_arguments: &[ast::Type],
    ) -> Result<(usize, ModuleOrigin), Error> {
        let (_cached, resolved_value, resolved_type, origin) =
            self.resolve_import(module, accessors)?;
        let resolved_value =
            self.instantiate_builtin_member(resolved_value, type_arguments, false)?;

        // Hoisted: inside a function whose collector registered this import as a
        // synthetic capture, the built value sits in a slot — load it instead of
        // re-emitting its construction. A miss (top level, an instantiated member, a
        // value too cheap to hoist) falls through to inline emission, so any collector
        // gap degrades to the unhoisted behavior rather than a miscompile.
        if type_arguments.is_empty()
            && let Some((_, index)) = scopes::lookup_variable(
                &self.scopes,
                &variables::CaptureSource::Import(module.to_vec()).scope_name(),
                accessors,
            )
        {
            self.codegen.add_instruction(Instruction::load(index));
            return Ok((resolved_type, origin));
        }

        // Emit instructions for just the resolved value, sharing repeated nodes
        self.emit_value_cse(&resolved_value)?;

        Ok((resolved_type, origin))
    }

    /// Import a module, execute it, and cache the result. The module is compiled in *its own*
    /// package context, so its imports resolve hermetically against its package's manifest.
    fn import_and_cache_module(
        &mut self,
        resolved: &crate::resolver::ResolvedModule,
    ) -> Result<modules::CachedModule, Error> {
        let module_name = resolved.id.display();

        // Parse the module
        let parsed = self
            .module_cache
            .load_and_cache_ast(&resolved.id, &resolved.source)?;

        // Open a recording frame for this compile: the declared reference set (the
        // same set the module's artifact key hashes) catches hidden dependencies —
        // references only a dialect expansion surfaces — which make the compile
        // uncacheable. Every exit below must pop the frame: callers of speculative
        // import probes swallow errors and continue compiling.
        let mut declared = std::collections::HashSet::new();
        for (path, _) in modules::collect_module_references(&parsed) {
            if let Ok(dep) = self.resolver.resolve(&resolved.package, &path) {
                declared.insert(dep.id);
            }
        }
        // The direct value imports seed the module's transitive value-import
        // closure — extraction's import-eligibility set — recorded at the close.
        let direct_imports: Vec<crate::resolver::ModuleId> =
            modules::collect_value_imports(&parsed)
                .into_iter()
                .filter_map(|(path, _)| {
                    self.resolver
                        .resolve(&resolved.package, &path)
                        .ok()
                        .map(|dep| dep.id)
                })
                .collect();
        self.module_cache.recording.push(modules::RecordingFrame {
            id: resolved.id.clone(),
            declared,
            hidden: false,
        });

        // Link-before-compile: compile this module's own imports to completion first,
        // so the body below compiles against fully-built dependencies and the
        // dispatch snapshot taken next captures only this module's own additions.
        if let Err(error) = self.precompile_imports(&parsed, &resolved.package) {
            self.module_cache.recording.pop();
            return Err(error);
        }
        // Functions registered from here on are this module's own (nested compiles
        // and links attribute theirs first; see `record_module_functions`). The dedup
        // floor keeps them the module's own even when structurally identical to an
        // earlier module's: attribution must not depend on what happened to compile
        // first in this session, or artifact content would vary with session history.
        let functions_start = self.program.get_functions().len();
        let previous_floor = self.program.set_function_dedup_floor(functions_start);
        // Suffixes minted during this module's compile derive from a module-scoped
        // counter (see `mint_type_param_suffix`); popped wherever the floor restores.
        self.suffix_scopes.push((module_name.clone(), 0));

        // Save current compiler state
        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_local_count = self.local_count;
        // Resolve this module's own imports against its package, not the importer's.
        let saved_package = std::mem::replace(&mut self.current_package, resolved.package.clone());
        // Provenance sites inside the module name it, not the importing unit.
        let saved_module = std::mem::replace(&mut self.current_module, module_name.clone());
        // NOTE: `type_param_suffix` is deliberately *not* cleared here. Since
        // link-before-compile, modules are pre-compiled at the top level (suffix state
        // `None`, so each top-level definition gets its own suffix — the principled
        // assignment); the only imports still triggered from inside a function body are
        // ones the pre-scan cannot see (e.g. inside dialect expansions), which compile
        // under the importer's suffix as before. Beware that suffix sharing can silently
        // alias distinct definitions' type parameters, masking mis-declared generics —
        // iter.qv's `map` self-type wrongly said `'thunk<'t>` for years because sharing
        // repaired it; per-definition suffixes surfaced it.
        //
        // The module's own top-level functions need the dialect pre-expansion walk even when
        // the import was triggered from inside a function body.
        let saved_function_depth = std::mem::take(&mut self.function_depth);
        // Suppress semantic recording while compiling an imported module: its spans are
        // offsets into the module's own source, which would otherwise collide with the
        // document being indexed. (A module that fails to compile aborts the whole
        // compilation, so not restoring on the error path is acceptable.)
        let saved_recorder = self.recorder.take();
        // Snapshot the dispatch tables so we can capture exactly the entries this module (and its
        // nested imports) adds, to store on the cached module. These tables intentionally persist
        // across the module boundary (the parent must see them when compiling cold), so the delta
        // is what distinguishes this module's contribution.
        let dispatch_fn_before = self.fn_case_tables.clone();
        let dispatch_case_before = self.case_tables.clone();
        let type_params_before = self.callable_type_params.clone();

        // Reset to clean state for module compilation
        self.local_count = 0;

        // A module with chain steps needs a parameter scope for the implicit continuation.
        let has_chains = parsed.chains().next().is_some();

        let scope_parameter = if has_chains {
            let param_local = self.local_count;
            self.local_count += 1;
            self.codegen.add_instruction(Instruction::store());
            let nil_type_id = self.program.register_type(Type::nil());
            Some(scopes::Parameter {
                ty: nil_type_id,
                index: param_local,
                provenance: Provenance::Parameter,
            })
        } else {
            None
        };

        self.scopes = vec![Scope::new(
            Bindings::default(),
            scope_parameter,
            ScopeKind::Root,
        )];

        // Compile the module body as one threaded sequence; the final value is the module value.
        // On failure, restore the saved compiler state before propagating. This is not just
        // hygiene: module imports are also triggered by *speculative* probes (the Apply-site
        // inference's `callee_parameter_type`), whose callers swallow errors and continue — leaving
        // the module's scopes/instructions in place would have the enclosing program compile
        // against the failed module's scope chain (cascading `VariableUndefined`s), and a zeroed
        // `function_depth` panics with a usize underflow in the enclosing `compile_function`,
        // masking the real error.
        let result_type_id = match self.compile_top_level(parsed.steps) {
            Ok(t) => t,
            Err(e) => {
                self.codegen.instructions = saved_instructions;
                self.scopes = saved_scopes;
                self.local_count = saved_local_count;
                self.recorder = saved_recorder;
                self.current_package = saved_package;
                self.current_module = saved_module;
                self.function_depth = saved_function_depth;
                self.program.set_function_dedup_floor(previous_floor);
                self.suffix_scopes.pop();
                self.module_cache.recording.pop();
                return Err(e);
            }
        };
        self.program.set_function_dedup_floor(previous_floor);
        self.suffix_scopes.pop();
        let module_type = self
            .program
            .lookup_type(result_type_id)
            .cloned()
            .unwrap_or_else(Type::nil);

        // Get the compiled module instructions
        let module_instructions = std::mem::take(&mut self.codegen.instructions);

        // Restore original compiler state
        self.codegen.instructions = saved_instructions;
        self.scopes = saved_scopes;
        self.local_count = saved_local_count;
        self.recorder = saved_recorder;
        self.current_package = saved_package;
        self.current_module = saved_module;
        self.function_depth = saved_function_depth;

        // Register the callable type for this module wrapper function
        // (modules take no arguments and return the module value)
        let nil_type_id = self.program.register_type(Type::nil());
        let never_id = self.program.never();
        let callable_type_id = self.program.register_type(Type::Callable {
            parameter: nil_type_id,
            result: result_type_id,
            receive: never_id,
            states: Some(nil_type_id),
        });

        // Generate bytecode, then add the module function
        let mut bytecode = self.program.to_bytecode(None);
        bytecode.functions.push(quiver_core::bytecode::Function {
            instructions: module_instructions,
            captures: 0,
            type_id: callable_type_id,
        });
        bytecode.entry = Some(bytecode.functions.len() - 1);

        // Execute the module to get the result value
        // Modules are executed at compile time only to produce their value; they don't
        // receive messages, so skip the (expensive) parameter-compatibility tables.
        let (module_value, _executor) = match quiver_core::execute_bytecode_sync_with(
            bytecode,
            self.builtins,
            false,
            self.fuel,
            self.cancel.as_deref(),
        ) {
            Ok(result) => result,
            Err(e) => {
                self.module_cache.recording.pop();
                return Err(Error::ModuleExecution {
                    module: module_name.clone(),
                    error: Box::new(e),
                });
            }
        };

        // Extract binary data from the executor

        // Capture the dispatch-table entries this module added (new or changed since the
        // snapshot), so a later cache hit can restore them without recompiling the module.
        // The window also absorbs entries belonging to modules compiled *inside* it —
        // undeclared (dialect-surfaced) imports — which are that module's vocabulary,
        // not this one's: it records its own delta and restores it when imported, so
        // they are filtered here rather than riding (and, in an artifact, escaping the
        // key) with this module. At this point the module's own functions have no owner
        // yet, so "unowned or declared" keeps exactly the module's world.
        let foreign = |function_id: &usize| -> bool {
            let declared = &self
                .module_cache
                .recording
                .last()
                .expect("module recording frame must be open")
                .declared;
            self.module_cache
                .function_owners
                .get(function_id)
                .is_some_and(|(owner, _)| !declared.contains(owner))
        };
        let fn_case_tables: HashMap<usize, Vec<(usize, usize)>> = self
            .fn_case_tables
            .iter()
            .filter(|(k, v)| dispatch_fn_before.get(*k) != Some(*v) && !foreign(k))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        let case_tables: HashMap<usize, usize> = self
            .case_tables
            .iter()
            .filter(|(k, v)| dispatch_case_before.get(*k) != Some(*v) && !foreign(v))
            .map(|(k, v)| (*k, *v))
            .collect();
        let callable_type_params: HashMap<usize, Vec<String>> = self
            .callable_type_params
            .iter()
            .filter(|(k, v)| type_params_before.get(*k) != Some(*v))
            .map(|(k, v)| (*k, v.clone()))
            .collect();

        let cached = modules::CachedModule {
            value: module_value,
            module_type,
            fn_case_tables,
            case_tables,
            callable_type_params,
        };

        // Cache the module
        self.module_cache
            .cache_module(resolved.id.clone(), cached.clone());

        // With a store attached, build the module's type namespace now — the artifact
        // carries it — while the frame is still open: its loads are the module's own
        // declared references, not the importer's.
        if self.module_cache.artifact_store.is_some()
            && let Err(error) = modules::module_type_namespace(
                &resolved.id.name,
                self.resolver,
                self.module_cache,
                &resolved.package,
                self.program,
            )
        {
            self.module_cache.recording.pop();
            return Err(error);
        }

        // Close the recording: attribute the module's own functions, then — when the
        // compile referenced only declared modules — extract its artifact into the
        // store, making this compile the store's entry for its key.
        let frame = self
            .module_cache
            .recording
            .pop()
            .expect("module recording frame must be open");
        self.module_cache.record_module_functions(
            &resolved.id,
            functions_start,
            self.program.get_functions().len(),
        );
        // Record the transitive value-import closure — extraction's
        // import-eligibility set — for every cached module (hidden ones included:
        // importers' closures build on theirs).
        self.module_cache
            .record_value_closure(&resolved.id, direct_imports);
        if !frame.hidden
            && let Some(store) = self.module_cache.artifact_store.clone()
            && let Some(key) = crate::artifact::key_for_resolved(
                self.resolver,
                self.module_cache,
                resolved,
                self.debug,
            )
            && let Some(artifact) =
                crate::artifact::extract(&resolved.id, self.program, self.module_cache)
        {
            store.save(key, artifact);
        }

        Ok(cached)
    }

    /// Expand a dialect invocation `%mod{ … }`: resolve the module, fetch the function its
    /// `:dialect` annotation carries, execute it at compile time with the unescaped brace
    /// content, and convert the returned `'%meta.expr` tree into an AST term — a block
    /// binding the flowing value (for `Ripple`) around the spliced expansion.
    ///
    /// TODO: run dialect evaluation under a restricted (IO-free) builtin registry with an
    /// instruction budget, and cache expansions keyed by (module, content).
    fn expand_dialect(&mut self, dialect: &ast::Dialect) -> Result<ast::Term, Error> {
        let module_name = format!("%{}", dialect.path.join("/"));
        let (content, escapes) = dialect::unescape_content(&dialect.raw);

        let (_cached, module_value, _module_type, _origin) =
            self.resolve_import(&dialect.path, &[])?;
        // Compiling the module (on a cache miss) leaves `current_span` pointing into the
        // module's source; point it back at the invocation for expansion errors.
        self.current_span = dialect.span.get();

        let dialect_key = self.program.register_annotation_key("dialect");
        let Some(function) = module_value.get_annotation(dialect_key).cloned() else {
            return Err(Error::DialectMissing {
                module: module_name,
            });
        };

        // The prefix-parse callbacks go into a clone of the host registry, so dialect
        // evaluation — and only dialect evaluation — can reach the compiler's parser.
        let mut registry = self.builtins.clone();
        dialect::register_callbacks(&mut registry);

        // Load the dialect function and check that it takes the context record
        // `[content: Str['bin], term: <fn>, chain: <fn>]` (a partial parameter type
        // naming only the fields the dialect uses also works).
        let (function_instructions, function_type) =
            self.value_to_instructions_from_cache(&function)?;
        let Some((parameter, result)) = self.callable_signature(function_type) else {
            return Err(Error::DialectFailed {
                module: module_name,
                message: format!(
                    "must carry a function in its :dialect annotation, found {}",
                    quiver_core::format::format_type_by_id(&*self.program, function_type)
                ),
            });
        };
        let str_type = annotations::str_type(self.program);
        let binary_type = self.program.register_type(Type::Binary);
        let integer_type = self.program.register_type(Type::Integer);
        let nil_type_id = self.program.register_type(Type::nil());
        let never_id = self.program.never();
        let callback_argument = self
            .program
            .register_tuple(None, vec![(None, binary_type), (None, integer_type)]);
        let callback_argument_type = self.program.register_type(Type::Tuple(callback_argument));
        let callback_result_type = self
            .program
            .register_type(Type::Union(vec![integer_type, nil_type_id]));
        let callback_type = self.program.register_type(Type::Callable {
            parameter: callback_argument_type,
            result: callback_result_type,
            receive: never_id,
            states: Some(callback_argument_type),
        });
        let context_tuple = self.program.register_tuple(
            None,
            vec![
                (Some("content".to_string()), str_type),
                (Some("term".to_string()), callback_type),
                (Some("chain".to_string()), callback_type),
            ],
        );
        let context_type = self.program.register_type(Type::Tuple(context_tuple));
        if !quiver_core::types::is_compatible(context_type, parameter, &*self.program) {
            return Err(Error::DialectFailed {
                module: module_name,
                message: format!(
                    "must take the dialect context (content: Str['bin], term: …, chain: …) \
                     in its :dialect function, found {}",
                    quiver_core::format::format_type_by_id(&*self.program, parameter)
                ),
            });
        }

        // Assemble a nilary entry applying the dialect function to the context, and run it.
        let str_tuple = self
            .program
            .register_tuple(Some("Str".to_string()), vec![(None, binary_type)]);
        let term_builtin = self
            .program
            .register_builtin(dialect::TERM_CALLBACK.to_string(), &registry);
        let chain_builtin = self
            .program
            .register_builtin(dialect::CHAIN_CALLBACK.to_string(), &registry);
        let entry_type = self.program.register_type(Type::Callable {
            parameter: nil_type_id,
            result,
            receive: never_id,
            states: Some(nil_type_id),
        });
        let mut bytecode = self.program.to_bytecode(None);
        // The content constant goes into the *temporary* bytecode only: registering it on the
        // program would permanently ship every invocation's brace content as dead data.
        let constant = bytecode.constants.len();
        bytecode
            .constants
            .push(Constant::Binary(content.clone().into_bytes()));
        let mut instructions = vec![
            Instruction::constant(constant),
            Instruction::tuple(str_tuple),
            Instruction::builtin(term_builtin),
            Instruction::builtin(chain_builtin),
            Instruction::tuple(context_tuple),
        ];
        instructions.extend(function_instructions);
        instructions.push(Instruction::call());
        bytecode.functions.push(quiver_core::bytecode::Function {
            instructions,
            captures: 0,
            type_id: entry_type,
        });
        bytecode.entry = Some(bytecode.functions.len() - 1);

        let (expr_value, executor) = quiver_core::execute_bytecode_sync_with(
            bytecode,
            &registry,
            false,
            self.fuel,
            self.cancel.as_deref(),
        )
        .map_err(|e| Error::ModuleExecution {
            module: module_name.clone(),
            error: Box::new(e),
        })?;

        let error_key = self.program.register_annotation_key("error");
        if expr_value.is_nil() {
            let message = self.describe_dialect_failure(
                &expr_value,
                error_key,
                dialect,
                &escapes,
                &executor,
                (constant, &content),
            );
            return Err(Error::DialectFailed {
                module: module_name,
                message,
            });
        }

        let program = &*self.program;
        let read_binary = |binary: &Binary| -> Option<Vec<u8>> {
            match binary {
                Binary::Constant(index) => match program.get_constant(*index) {
                    Some(Constant::Binary(bytes)) => Some(bytes.clone()),
                    // The content constant lives only in the expansion's temporary
                    // bytecode; the returned IR may reference the input string's bytes.
                    None if *index == constant => Some(content.clone().into_bytes()),
                    _ => None,
                },
                Binary::Data(data) => Some(data.to_vec()),
            }
        };
        let splicer = dialect::Splicer {
            program,
            read_binary,
            module: module_name,
            path: dialect.path.clone(),
            dialect,
            content: &content,
            escapes: &escapes,
            current_module: self.current_module.clone(),
            holes: std::cell::RefCell::new(Vec::new()),
        };
        let expansion = splicer.value_to_chain(&expr_value)?;
        let holes = splicer.take_holes();
        Ok(dialect::wrap_expansion(holes, expansion))
    }

    /// Recursively expand every dialect invocation in an expression, in place. Run on a
    /// function body before its captures are collected; elsewhere dialects expand lazily
    /// in [`Self::compile_term`].
    fn expand_dialects_in_block(&mut self, block: &mut ast::Block) -> Result<(), Error> {
        for annotation in &mut block.annotations {
            self.expand_dialects_in_chain(&mut annotation.value)?;
        }
        for branch in &mut block.branches {
            self.expand_dialects_in_sequence(&mut branch.condition)?;
            if let Some(consequence) = &mut branch.consequence {
                self.expand_dialects_in_sequence(consequence)?;
            }
        }
        Ok(())
    }

    fn expand_dialects_in_sequence(&mut self, sequence: &mut ast::Sequence) -> Result<(), Error> {
        for chain in sequence.chains_mut() {
            self.expand_dialects_in_chain(chain)?;
        }
        Ok(())
    }

    fn expand_dialects_in_chain(&mut self, chain: &mut ast::Chain) -> Result<(), Error> {
        for term in &mut chain.terms {
            self.expand_dialects_in_term(term)?;
        }
        Ok(())
    }

    fn expand_dialects_in_term(&mut self, term: &mut ast::Term) -> Result<(), Error> {
        match term {
            ast::Term::Dialect(dialect) => {
                *term = self.expand_dialect(dialect)?;
                // A hole may itself contain a dialect invocation; recurse into the
                // replacement. Depth is bounded by the literal content (an `Unquote`
                // span points at user-written text), so no budget is needed.
                self.expand_dialects_in_term(term)?;
            }
            ast::Term::Tuple(tuple) => {
                for field in &mut tuple.fields {
                    if let ast::FieldValue::Chain(chain) = &mut field.value {
                        self.expand_dialects_in_chain(chain)?;
                    }
                }
            }
            ast::Term::String(_, segments) => {
                for segment in segments {
                    if let ast::StrSegment::Hole(block) = segment {
                        self.expand_dialects_in_block(block)?;
                    }
                }
            }
            ast::Term::Block(block) => self.expand_dialects_in_block(block)?,
            ast::Term::Function(function) => {
                if let Some(body) = &mut function.body {
                    self.expand_dialects_in_block(body)?;
                }
            }
            ast::Term::Spawn(inner, arg, _) => {
                self.expand_dialects_in_term(inner)?;
                if let Some(arg) = arg {
                    self.expand_dialects_in_term(arg)?;
                }
            }
            ast::Term::Apply(_, arg) => self.expand_dialects_in_term(arg)?,
            ast::Term::Select(sources, _) => {
                if let Some(sources) = sources {
                    for source in sources {
                        self.expand_dialects_in_chain(source)?;
                    }
                }
            }
            ast::Term::Literal(_)
            | ast::Term::Match(_)
            | ast::Term::Access(_)
            | ast::Term::Self_
            | ast::Term::Process(_)
            | ast::Term::Reference(_)
            | ast::Term::State(..) => {}
        }
        Ok(())
    }

    /// The parameter and result of a callable type, looking through annotation rows and
    /// single-member unions.
    fn callable_signature(&self, type_id: usize) -> Option<(usize, usize)> {
        match self.program.lookup_type(type_id)? {
            Type::Callable {
                parameter, result, ..
            } => Some((*parameter, *result)),
            Type::Annotated { base, .. } => self.callable_signature(*base),
            Type::Union(members) if members.len() == 1 => self.callable_signature(members[0]),
            _ => None,
        }
    }

    /// Describe a dialect function's nil result: fold an `Expected[offset: 'int,
    /// message: Str['bin]]`-shaped `:error` payload into a message, mapping the content
    /// offset to a source position via the invocation's content span. `content_constant`
    /// is the brace content's temporary constant (index, text), which lives only in the
    /// expansion's bytecode — a message referencing it can't be read off the program.
    fn describe_dialect_failure(
        &self,
        value: &Value,
        error_key: usize,
        dialect: &ast::Dialect,
        escapes: &[usize],
        _executor: &quiver_core::executor::Executor<E>,
        content_constant: (usize, &str),
    ) -> String {
        let Some(payload) = value.get_annotation(error_key) else {
            return "failed".to_string();
        };
        let mut offset = None;
        let mut message = None;
        if let Value::Tuple(tuple_id, fields) = payload {
            // Locate each field by label when the tuple carries one, else by position — the
            // same fallback as `Splicer::field`, so a positionally built `Expected[2, "…"]`
            // keeps its offset→source-position mapping.
            let info = self.program.lookup_tuple(*tuple_id);
            let position_of = |label: &str, index: usize| {
                info.and_then(|info| {
                    info.fields
                        .iter()
                        .position(|(l, _)| l.as_deref() == Some(label))
                })
                .unwrap_or(index)
            };
            if let Some(Value::Int(o)) = fields.get(position_of("offset", 0)) {
                offset = usize::try_from(*o).ok();
            }
            if let Some(Value::Tuple(str_id, str_fields)) = fields.get(position_of("message", 1))
                && self
                    .program
                    .lookup_tuple(*str_id)
                    .is_some_and(|info| info.name.as_deref() == Some("Str"))
                && let Some(Value::Binary(binary)) = str_fields.first()
            {
                let bytes = match binary {
                    Binary::Constant(index) => match self.program.get_constant(*index) {
                        Some(Constant::Binary(bytes)) => Some(bytes.clone()),
                        None if *index == content_constant.0 => {
                            Some(content_constant.1.as_bytes().to_vec())
                        }
                        _ => None,
                    },
                    Binary::Data(data) => Some(data.to_vec()),
                };
                message = bytes.map(|b| String::from_utf8_lossy(&b).into_owned());
            }
        }
        let text = message.unwrap_or_else(|| format!("failed with {payload:?}"));
        match offset.and_then(|o| dialect::content_position(dialect, escapes, o)) {
            Some((line, column)) => {
                format!(
                    "failed: {text} (at {}:{line}:{column})",
                    self.current_module
                )
            }
            None => format!("failed: {text}"),
        }
    }

    /// Emit instructions reconstructing a compile-time value directly into the current
    /// stream, sharing repeated nodes ([`reconstruct_value`](Self::reconstruct_value)
    /// with the memo on).
    fn emit_value_cse(&mut self, value: &Value) -> Result<usize, Error> {
        let mut instructions = Vec::new();
        let value_type = self.reconstruct_value(value, &mut instructions, true)?;
        for instruction in instructions {
            self.codegen.add_instruction(instruction);
        }
        Ok(value_type)
    }

    /// Convert a cached runtime value back to instructions that reconstruct it, with no
    /// sharing — for callers whose instructions never join the current frame's stream (a
    /// type probe that discards them; dialect evaluation in its own mini-program), where
    /// memo slots would corrupt `local_count`.
    fn value_to_instructions_from_cache(
        &mut self,
        value: &Value,
    ) -> Result<(Vec<Instruction>, usize), Error> {
        let mut instructions = Vec::new();
        let value_type = self.reconstruct_value(value, &mut instructions, false)?;
        Ok((instructions, value_type))
    }

    /// The one reconstruction emitter: convert a cached runtime value back to
    /// instructions (appended to `out`) that rebuild it, using pre-extracted binary data
    /// instead of an executor.
    ///
    /// With `memo` on, repeated nodes share: the first emission of each non-trivial node
    /// stores the built value into an internal local (a [`scopes::CseKey`] slot — see
    /// `Scope::cse_slots` for the lifetime story) and every later occurrence of the same
    /// payload loads it, so the emitted program (and the runtime values it builds) keep
    /// the sharing the cached value's DAG had instead of expanding it to a tree. The memo
    /// allocates locals as it emits, so `out` must join the current function's stream,
    /// in order, immediately — allocation order is execution order.
    fn reconstruct_value(
        &mut self,
        value: &Value,
        out: &mut Vec<Instruction>,
        memo: bool,
    ) -> Result<usize, Error> {
        let key = if memo { cse_key(value) } else { None };
        if let Some(key) = &key
            && let Some((slot_type, index)) = scopes::lookup_cse_slot(&self.scopes, key)
        {
            out.push(Instruction::load(index));
            return Ok(slot_type);
        }

        let value_type = match value {
            Value::Int(int_value) => {
                let index = self
                    .program
                    .register_constant(Constant::Integer((*int_value).into()));
                out.push(Instruction::constant(index));
                self.program.register_type(Type::Integer)
            }
            Value::BigInt(int_value) => {
                let index = self
                    .program
                    .register_constant(Constant::Integer((**int_value).clone()));
                out.push(Instruction::constant(index));
                self.program.register_type(Type::Integer)
            }
            Value::Binary(binary) => {
                let index = match binary {
                    // Just use the existing constant
                    Binary::Constant(const_idx) => *const_idx,
                    // The value carries its own bytes; nothing to look up.
                    Binary::Data(data) => self
                        .program
                        .register_constant(Constant::Binary(data.to_vec())),
                };
                out.push(Instruction::constant(index));
                self.program.register_type(Type::Binary)
            }
            Value::Tuple(tuple_id, payload) => {
                for field in payload.iter() {
                    self.reconstruct_value(field, out, memo)?;
                }
                out.push(Instruction::tuple(*tuple_id));
                self.reconstruct_annotations(payload, out, memo)?;
                self.program.register_type(Type::Tuple(*tuple_id))
            }
            Value::Function(function, payload) => {
                // The callable type comes straight from the function's table entry —
                // the same index is reused, no re-registration.
                let callable_type_id = self
                    .program
                    .get_function(*function)
                    .ok_or(Error::FunctionUndefined(*function))?
                    .type_id;
                for capture in payload.iter() {
                    self.reconstruct_value(capture, out, memo)?;
                }
                out.push(Instruction::function(*function));
                self.reconstruct_annotations(payload, out, memo)?;
                callable_type_id
            }
            Value::Builtin(builtin_id, payload) => {
                let builtin_info = self
                    .program
                    .get_builtins()
                    .get(*builtin_id)
                    .ok_or_else(|| Error::BuiltinUndefined(format!("builtin_id {builtin_id}")))?;
                let param_type = builtin_info.param_type;
                let result_type = builtin_info.result_type;
                let builtin_name = builtin_info.name.clone();
                let never_id = self.program.never();
                let callable_type_id = self.program.register_type(Type::Callable {
                    parameter: param_type,
                    result: result_type,
                    receive: never_id,
                    // Builtins never tail-call: their states are their parameter.
                    states: Some(param_type),
                });
                // A module-cached instantiated builtin resolves to this program's entry for
                // that instantiation (the static type above stays the generic signature —
                // acceptable while no module exports an instantiated builtin whose
                // *result* depends on it). Registration is idempotent, so an id that is
                // already the right entry answers itself.
                let type_argument = payload.as_deref().and_then(Payload::type_argument);
                let builtin_index = self.program.register_builtin_instantiated(
                    builtin_name,
                    type_argument,
                    self.builtins,
                );
                out.push(Instruction::builtin(builtin_index));
                if let Some(payload) = payload {
                    self.reconstruct_annotations(payload, out, memo)?;
                }
                callable_type_id
            }
            Value::Process(_, _) => {
                return Err(Error::FeatureUnsupported(
                    "Cannot use process in constant context".to_string(),
                ));
            }
            Value::Resource(..) => {
                return Err(Error::FeatureUnsupported(
                    "Cannot use resource in constant context".to_string(),
                ));
            }
            Value::Reference(_) => {
                return Err(Error::FeatureUnsupported(
                    "Cannot use ref in constant context".to_string(),
                ));
            }
        };

        if let Some(key) = key {
            // Allocation order equals execution order — nested nodes registered and
            // stored above land at exactly the local indices `local_count` predicted.
            let index =
                scopes::define_cse_slot(&mut self.scopes, &mut self.local_count, key, value_type);
            out.push(Instruction::store());
            out.push(Instruction::load(index));
        }
        Ok(value_type)
    }

    /// Append each annotation of `payload` (value, then `Annotate`) onto the value the
    /// preceding instructions left on the stack.
    fn reconstruct_annotations(
        &mut self,
        payload: &quiver_core::value::Payload,
        out: &mut Vec<Instruction>,
        memo: bool,
    ) -> Result<(), Error> {
        for (key, value) in payload.annotations() {
            self.reconstruct_value(value, out, memo)?;
            out.push(Instruction::annotate(*key));
        }
        Ok(())
    }

    /// Resolve an accessor chain on a compile-time known value.
    /// Returns the resolved value and its type.
    fn resolve_accessors(
        &mut self,
        value: &Value,
        value_type: &usize,
        accessors: &[ast::AccessPath],
        module_name: &str,
    ) -> Result<(Value, usize), Error> {
        let mut current_value = value.clone();
        let mut current_type = *value_type;

        for accessor in accessors {
            // Annotation retrieval on a compile-time value resolves right here: the value
            // either carries the annotation or the result is nil. The checked form's gate
            // is decided statically too, against the entry value's own reconstructed type.
            if let ast::AccessPath::Annotation(name, expected) = accessor {
                let (key_id, result_type) = match expected {
                    None => annotations::retrieval_type(self.program, current_type, name)?,
                    Some(ast_type) => {
                        let mut env = typing::TypeEnv {
                            resolver: self.resolver,
                            module_cache: &mut *self.module_cache,
                            package: &self.current_package,
                        };
                        let asked = typing::resolve_ast_type(
                            &mut env,
                            &self.scopes,
                            ast_type.clone(),
                            self.program,
                        )?;
                        let (key_id, result_type, _) = annotations::checked_retrieval_type(
                            self.program,
                            current_type,
                            name,
                            asked,
                        );
                        // Apply the gate now: the entry is a compile-time value, so its
                        // precise type is reconstructible and the shape test is static.
                        if let Some(entry) = current_value.get_annotation(key_id).cloned() {
                            let (_, entry_type) = self.value_to_instructions_from_cache(&entry)?;
                            if !quiver_core::types::is_compatible(entry_type, asked, &*self.program)
                            {
                                current_value = Value::nil();
                                current_type = result_type;
                                continue;
                            }
                        }
                        (key_id, result_type)
                    }
                };
                current_value = current_value
                    .get_annotation(key_id)
                    .cloned()
                    .unwrap_or_else(Value::nil);
                current_type = result_type;
                continue;
            }

            let Value::Tuple(tuple_id, fields) = &current_value else {
                return Err(Error::MemberAccessOnNonTuple {
                    target: module_name.to_string(),
                });
            };

            let (index, field_type) = match accessor {
                ast::AccessPath::Field(name) => {
                    let (access, field_types) = type_queries::get_field_by_name(
                        self.program,
                        current_type,
                        name,
                        module_name,
                    )?;
                    let index = match access {
                        type_queries::FieldAccess::Position(index) => index,
                        // A compile-time value is concrete: resolve the name against the
                        // value's own tuple layout.
                        type_queries::FieldAccess::Named { .. } => self
                            .program
                            .lookup_tuple(*tuple_id)
                            .and_then(|info| {
                                info.fields
                                    .iter()
                                    .position(|(fname, _)| fname.as_deref() == Some(name.as_str()))
                            })
                            .ok_or_else(|| Error::MemberFieldNotFound {
                                field_name: name.clone(),
                                target: module_name.to_string(),
                            })?,
                    };
                    (index, typing::union_type_ids(self.program, field_types))
                }
                ast::AccessPath::Index(index) => {
                    let field_types = type_queries::get_field_at_index(
                        &*self.program,
                        current_type,
                        *index,
                        module_name,
                    )?;
                    (*index, typing::union_type_ids(self.program, field_types))
                }
                ast::AccessPath::Annotation(..) => unreachable!("handled above"),
            };

            current_value =
                fields
                    .get(index)
                    .cloned()
                    .ok_or_else(|| Error::MemberAccessOnNonTuple {
                        target: module_name.to_string(),
                    })?;
            current_type = field_type;
        }

        Ok((current_value, current_type))
    }

    /// Validate that a receive function in a select has the correct return type
    /// Compile select sources and return their types
    fn compile_select_sources(
        &mut self,
        sources: &[ast::Chain],
        value_type: Option<&usize>,
    ) -> Result<Vec<usize>, Error> {
        let mut source_types = Vec::new();

        for (i, source) in sources.iter().enumerate() {
            // Pass ripple_context if there's a chained value
            // Chained value is at offset i (number of sources compiled so far)
            // Set owns_value=false since we'll clean it up manually after all sources
            let ripple_ctx;
            let ripple_param = if let Some(val_type) = value_type {
                ripple_ctx = RippleContext {
                    value_type_id: *val_type,
                    stack_offset: i,
                    owns_value: false,
                    provenance: Provenance::Unknown, // Spawn context doesn't need narrowing
                };
                Some(&ripple_ctx)
            } else {
                None
            };

            let source_type = self.compile_chain(source.clone(), None, ripple_param)?;

            source_types.push(source_type);
        }

        Ok(source_types)
    }

    /// Compile spawn operation. The init argument may be supplied three ways: by juxtaposition
    /// (`@f x`, `argument` is `Some`), by the chained value (`x ~> @f`, carried in `value_type`),
    /// or not at all (`@f`, `@{ … }`). For a ripple function (`@~`) the chained value *is* the
    /// function being spawned, so a juxtaposed argument is evaluated without it.
    fn compile_spawn(
        &mut self,
        function: ast::Term,
        value_type: Option<usize>,
        explicit_argument: bool,
    ) -> Result<usize, Error> {
        // `@~`: the chained value is the function to spawn (already on the stack), spawned with
        // nil — so the flowing function must be nilary.
        if function.is_bare_ripple() {
            let fn_type = value_type.ok_or_else(|| {
                Error::FeatureUnsupported("Ripple spawn requires piped value".to_string())
            })?;
            return self.emit_nil_param_spawn(fn_type);
        }

        // The chained value (if any) is the init argument. A nilary process function ignores this
        // implicit flow and is spawned with nil — the value is discarded, like a call.
        let (fn_type, _prov) = self.compile_term(
            function,
            FlowingValue {
                ty: None,
                provenance: Provenance::Unknown,
            },
            None,
            None,
            None,
            None,
            false,
        )?;

        let param_is_nil = matches!(
            self.program
                .lookup_base(fn_type),
            Some(Type::Callable { parameter, .. }) if self.is_nil(*parameter)
        );

        if let Some(arg_type) = value_type {
            if param_is_nil {
                if explicit_argument {
                    // An explicit juxtaposed argument (`@f x`) to a nilary process function is
                    // rejected, exactly as an explicit call argument to a nilary function is —
                    // only the implicit flow (`x ~> @f`) is quietly discarded.
                    return Err(Error::TypeMismatch {
                        expected: "function with nil parameter (no argument)".to_string(),
                        found: format!(
                            "argument {}",
                            quiver_core::format::format_type_by_id(&*self.program, arg_type)
                        ),
                    });
                }
                // Discard the chained value (Stack: [value, function] -> [function]), spawn with nil.
                self.codegen.add_instruction(Instruction::rotate(2));
                self.codegen.add_instruction(Instruction::pop());
                self.emit_nil_param_spawn(fn_type)
            } else {
                self.emit_arg_spawn(fn_type, arg_type)
            }
        } else {
            self.emit_nil_param_spawn(fn_type)
        }
    }

    /// Emit spawn for nil-parameter function (cases 1 and 3)
    /// Stack before: [function]
    fn emit_nil_param_spawn(&mut self, fn_type_id: usize) -> Result<usize, Error> {
        let Some(Type::Callable {
            parameter,
            result,
            receive,
            states,
        }) = self.program.lookup_base(fn_type_id)
        else {
            return Err(Error::FeatureUnsupported(
                "Can only spawn functions".to_string(),
            ));
        };
        let (parameter, result, receive, states) = (*parameter, *result, *receive, *states);

        let nil_type_id = self.program.register_type(Type::nil());
        if parameter != nil_type_id {
            return Err(Error::TypeMismatch {
                expected: "function with nil parameter".to_string(),
                found: format!(
                    "function with parameter {}",
                    quiver_core::format::format_type_by_id(&*self.program, parameter)
                ),
            });
        }

        // Stack: [function] -> [nil, function] -> spawn
        self.codegen.add_instruction(Instruction::tuple(NIL));
        self.codegen.add_instruction(Instruction::rotate(2));
        self.codegen.add_instruction(Instruction::spawn());

        Ok(self.program.register_type(Type::Process {
            send: Some(receive),
            receive: Some(result),
            state: states,
        }))
    }

    /// Emit spawn with argument (case 2)
    /// Stack before: [argument, function]
    fn emit_arg_spawn(&mut self, fn_type_id: usize, arg_type: usize) -> Result<usize, Error> {
        let Some(Type::Callable {
            parameter,
            result,
            receive,
            states,
        }) = self.program.lookup_base(fn_type_id)
        else {
            return Err(Error::FeatureUnsupported(
                "Can only spawn functions".to_string(),
            ));
        };
        let (parameter, result, receive, states) = (*parameter, *result, *receive, *states);

        if !quiver_core::types::is_compatible(arg_type, parameter, &*self.program) {
            return Err(Error::TypeMismatch {
                expected: quiver_core::format::format_type_by_id(&*self.program, parameter),
                found: quiver_core::format::format_type_by_id(&*self.program, arg_type),
            });
        }

        self.codegen.add_instruction(Instruction::spawn());

        Ok(self.program.register_type(Type::Process {
            send: Some(receive),
            receive: Some(result),
            state: states,
        }))
    }

    /// Compile access expression: .x, $.x, foo.x, etc. Records each component (the base symbol
    /// and each accessor) for the LSP, then delegates to the implementation.
    fn compile_access(
        &mut self,
        access: ast::Access,
        value_type: Option<usize>,
        value_provenance: Provenance,
        ripple_context: Option<&RippleContext>,
        implicit_flow: bool,
    ) -> Result<(usize, Provenance), Error> {
        // Capture the components before `access` is moved into the inner compiler.
        let source = access.source.clone();
        let accessors = access.accessors.clone();
        let base_span = access.base_span.get();
        let accessor_spans: Vec<Option<SourceSpan>> =
            access.accessor_spans.iter().map(|s| s.get()).collect();

        // The flowing value a `~` / bare `.field` access reads from — the chained value, or the
        // enclosing tuple's ripple context when there's no direct chain.
        let flowing_value = value_type.or_else(|| ripple_context.map(|c| c.value_type_id));

        let result = self.compile_access_inner(
            access,
            value_type,
            value_provenance,
            ripple_context,
            implicit_flow,
        );

        // Record each component on its own span (`%util` vs `triple`, `foo` vs `bar`, `$` vs `0`)
        // so hover/go-to-definition resolve precisely. This is the sole recorder for accesses.
        if result.is_ok() {
            self.record_access_components(
                &source,
                &accessors,
                base_span,
                &accessor_spans,
                flowing_value,
            );
        }
        result
    }

    /// Record a hover/navigation entry for each component of an access chain: the base symbol
    /// (its own type, with go-to-definition) and each accessor (the type after it).
    fn record_access_components(
        &mut self,
        source: &Option<ast::AccessSource>,
        accessors: &[ast::AccessPath],
        base_span: Option<SourceSpan>,
        accessor_spans: &[Option<SourceSpan>],
        flowing_value: Option<usize>,
    ) {
        if self.recorder.is_none() {
            return;
        }

        // Record the base, and capture its type for resolving accessor types below.
        let mut import_origin = None;
        let mut import_module: Option<Vec<String>> = None;
        let (base_type, base_name) = match source {
            Some(ast::AccessSource::Identifier(name)) => {
                let Some((ty, _)) = scopes::lookup_variable(&self.scopes, name, &[]) else {
                    return;
                };
                self.record_reference(base_span, name, name.clone(), ty);
                (ty, name.clone())
            }
            Some(ast::AccessSource::Parameter { depth: 0 }) => {
                let Ok((ty, _)) = scopes::get_function_parameter(&self.scopes) else {
                    return;
                };
                self.record_typed(base_span, ty, SymbolKind::Parameter, Some("$".to_string()));
                (ty, "$".to_string())
            }
            Some(ast::AccessSource::Parameter { depth }) => {
                // An outer parameter resolves through its captures; when nothing is bare-
                // captured the helper records per-path entries and there's no base type to
                // thread into the shared accessor loop below.
                let name = variables::CaptureSource::OuterParameter(*depth).scope_name();
                match self.record_outer_parameter(&name, accessors, base_span, accessor_spans) {
                    Some(ty) => (ty, name),
                    None => return,
                }
            }
            Some(ast::AccessSource::Import(module)) => {
                let Ok((_, _, ty, origin)) = self.resolve_import(module, &[]) else {
                    return;
                };
                let label = format!("%{}", module.join("/"));
                // Base = the module itself: hover its type, go-to-def to its file, refs module-level.
                self.record_import(
                    base_span,
                    ty,
                    Some(label.clone()),
                    origin.clone(),
                    module,
                    &[],
                );
                import_origin = Some(origin);
                import_module = Some(module.clone());
                (ty, label)
            }
            Some(ast::AccessSource::Builtin(name)) => {
                // The builtin's signature, hovered on its `__name__`.
                let Some((param, result)) = self.builtins.resolve_signature(name, self.program)
                else {
                    return;
                };
                let parameter = self.program.register_type(param);
                let result = self.program.register_type(result);
                let receive = self.program.never();
                let ty = self.program.register_type(Type::Callable {
                    parameter,
                    result,
                    receive,
                    states: Some(parameter),
                });
                self.record_typed(
                    base_span,
                    ty,
                    SymbolKind::Builtin,
                    Some(format!("__{}__", name)),
                );
                (ty, name.clone())
            }
            Some(ast::AccessSource::TailCall(Some(name))) => {
                // `^f` tail-calls the function `f`: hover its type and navigate to its definition.
                let Some((ty, _)) = scopes::lookup_variable(&self.scopes, name, &[]) else {
                    return;
                };
                self.record_reference(base_span, name, name.clone(), ty);
                (ty, name.clone())
            }
            Some(ast::AccessSource::Ripple) => {
                // `~` / `~.field` read off the flowing value; the `~` hovers as its type, and the
                // accessors are resolved against it below.
                let Some(ty) = flowing_value else {
                    return;
                };
                self.record_typed(base_span, ty, SymbolKind::Expression, None);
                (ty, "~".to_string())
            }
            None => {
                // A bare field access (`.field`) reads off the flowing value. There is no base
                // token to hover (it starts with `.`); resolve the accessors against it below.
                let Some(ty) = flowing_value else {
                    return;
                };
                (ty, "value".to_string())
            }
            // Self tail call (`^`) and self (`.`) have nothing to navigate to.
            _ => return,
        };

        // Each accessor: the type after applying it, hovered on its own span.
        for (i, accessor) in accessors.iter().enumerate() {
            let Some(span) = accessor_spans.get(i).copied().flatten() else {
                continue;
            };
            let Ok(ty) = self.peek_accessor_type(base_type, &accessors[..=i], &base_name) else {
                continue;
            };
            let label = match accessor {
                ast::AccessPath::Field(name) => name.clone(),
                ast::AccessPath::Index(index) => index.to_string(),
                ast::AccessPath::Annotation(name, _) => format!(":{name}"),
            };
            // The first accessor of an import is the module member: keep its go-to-def/refs.
            if let (Some(origin), 0) = (&import_origin, i) {
                self.record_import(
                    Some(span),
                    ty,
                    Some(label),
                    origin.clone(),
                    import_module.as_deref().unwrap_or(&[]),
                    accessors,
                );
            } else {
                self.record_typed(Some(span), ty, SymbolKind::Field, Some(label));
            }
        }
    }

    /// Record a pin pattern's components for the language server, mirroring
    /// `record_access_components`: the root (a variable reference with go-to-definition, or
    /// the parameter) and each accessor with the type after applying it. Called from the
    /// pattern-recording chokepoint in `compile_match`, before the pattern's own bindings
    /// enter scope — a pin references pre-existing values only.
    /// Record the components of an outer-parameter access (`$$`, `$$k.q`) from its captures:
    /// the base when the whole argument was captured bare, else each accessor whose exact
    /// path was captured (fields are captured per path, so that is what there is to show).
    /// Returns the base type when the caller's shared accessor loop should proceed.
    fn record_outer_parameter(
        &mut self,
        name: &str,
        accessors: &[ast::AccessPath],
        base_span: Option<SourceSpan>,
        accessor_spans: &[Option<SourceSpan>],
    ) -> Option<usize> {
        if let Some((ty, _)) = scopes::lookup_variable(&self.scopes, name, &[]) {
            self.record_typed(base_span, ty, SymbolKind::Parameter, Some(name.to_string()));
            return Some(ty);
        }
        for (i, accessor) in accessors.iter().enumerate() {
            let Some(span) = accessor_spans.get(i).copied().flatten() else {
                continue;
            };
            let Some((ty, _)) = scopes::lookup_variable(&self.scopes, name, &accessors[..=i])
            else {
                continue;
            };
            let label = match accessor {
                ast::AccessPath::Field(field) => field.clone(),
                ast::AccessPath::Index(index) => index.to_string(),
                ast::AccessPath::Annotation(..) => continue,
            };
            self.record_typed(Some(span), ty, SymbolKind::Field, Some(label));
        }
        None
    }

    fn record_pin_target(&mut self, target: &ast::PinTarget) {
        if self.recorder.is_none() {
            return;
        }

        let (base_type, base_name) = match &target.root {
            ast::PinRoot::Variable(name) => {
                let Some((ty, _)) = scopes::lookup_variable(&self.scopes, name, &[]) else {
                    return;
                };
                self.record_reference(target.base_span.get(), name, name.clone(), ty);
                (ty, name.clone())
            }
            ast::PinRoot::Parameter { depth: 0 } => {
                let Ok((ty, _)) = scopes::get_function_parameter(&self.scopes) else {
                    return;
                };
                self.record_typed(
                    target.base_span.get(),
                    ty,
                    SymbolKind::Parameter,
                    Some("$".to_string()),
                );
                (ty, "$".to_string())
            }
            ast::PinRoot::Parameter { depth } => {
                let name = variables::CaptureSource::OuterParameter(*depth).scope_name();
                let accessor_spans: Vec<Option<SourceSpan>> =
                    target.accessor_spans.iter().map(|s| s.get()).collect();
                match self.record_outer_parameter(
                    &name,
                    &target.accessors,
                    target.base_span.get(),
                    &accessor_spans,
                ) {
                    Some(ty) => (ty, name),
                    None => return,
                }
            }
        };

        for (i, accessor) in target.accessors.iter().enumerate() {
            let Some(span) = target.accessor_spans.get(i).and_then(|s| s.get()) else {
                continue;
            };
            let Ok(ty) = self.peek_accessor_type(base_type, &target.accessors[..=i], &base_name)
            else {
                continue;
            };
            let label = match accessor {
                ast::AccessPath::Field(name) => name.clone(),
                ast::AccessPath::Index(index) => index.to_string(),
                // Pin targets carry no annotation steps.
                ast::AccessPath::Annotation(..) => continue,
            };
            self.record_typed(Some(span), ty, SymbolKind::Field, Some(label));
        }
    }

    fn compile_access_inner(
        &mut self,
        access: ast::Access,
        value_type: Option<usize>,
        value_provenance: Provenance,
        ripple_context: Option<&RippleContext>,
        implicit_flow: bool,
    ) -> Result<(usize, Provenance), Error> {
        // Explicit type arguments (`f<'int>`) instantiate the accessed callable's type —
        // applied once the accessed type is known, before any application, so the pinned
        // parameters are what unification checks the argument against.
        let type_args = access.type_arguments;
        // An access produces a value (a variable, parameter, import member, builtin, or a field
        // of the flowing value). When that value is callable and a flowing value is present
        // (the chained value of the surrounding step), it is invoked with it. The flowing value
        // arrives as `value_type` and sits on the stack.
        match access.source {
            None => {
                // Field/positional access (.x, .0) reads off the flowing value.
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported(
                        "Field/positional access requires a value".to_string(),
                    )
                })?;
                let (accessed_type, accessed_prov) =
                    self.compile_accessor(val_type, access.accessors, "value", value_provenance)?;
                let accessed_type = self.instantiate_type_arguments(accessed_type, &type_args)?;
                Ok((accessed_type, accessed_prov))
            }
            Some(ast::AccessSource::Parameter { depth }) if depth > 0 => {
                // An outer parameter (`$$`, `$$$x`) resolves to the capture the collector
                // recorded for this literal — a local named by its sigil run — and behaves
                // exactly like a captured-variable access from there (a callable field is
                // called with the flowing value, and so on).
                let name = variables::CaptureSource::OuterParameter(depth).scope_name();
                let peeked_type = scopes::lookup_variable(&self.scopes, &name, &access.accessors)
                    .map(|(ty, _)| ty)
                    .or_else(|| {
                        let (base_type, _) = scopes::lookup_variable(&self.scopes, &name, &[])?;
                        self.peek_accessor_type(base_type, &access.accessors, &name)
                            .ok()
                    });
                let Some(peeked_type) = peeked_type else {
                    // Only reachable outside any enclosing literal (module top level): a
                    // closure always carries the captures its collector recorded.
                    return Err(Error::ParameterDepthExceeded {
                        written: ast::parameter_sigils(depth),
                    });
                };
                let is_applicable = self.is_applicable_type(peeked_type);

                // Non-applicable accessed with a flowing value: drop the value before loading.
                if !is_applicable && value_type.is_some() {
                    self.codegen.add_instruction(Instruction::pop());
                }
                let (accessed_type, accessed_prov) =
                    self.compile_member_access(&name, access.accessors)?;
                let accessed_type = self.instantiate_type_arguments(accessed_type, &type_args)?;

                if let (true, Some(val_type)) = (is_applicable, value_type) {
                    let ty =
                        self.apply_value_to_type(accessed_type, val_type, implicit_flow, None)?;
                    Ok((ty, Provenance::Unknown))
                } else {
                    Ok((accessed_type, accessed_prov))
                }
            }
            Some(ast::AccessSource::Parameter { .. }) => {
                // $ accesses the function parameter.
                let (param_type, param_local) = scopes::get_function_parameter(&self.scopes)?;

                // Peek at the accessed type to determine if applicable (without emitting code).
                let peeked_type = self.peek_accessor_type(param_type, &access.accessors, "$");
                let is_applicable = peeked_type.is_ok_and(|ty| self.is_applicable_type(ty));

                // Non-applicable accessed with a flowing value: drop the value before loading.
                if !is_applicable && value_type.is_some() {
                    self.codegen.add_instruction(Instruction::pop());
                }
                self.codegen.add_instruction(Instruction::load(param_local));
                let (accessed_type, accessed_prov) = self.compile_accessor(
                    param_type,
                    access.accessors,
                    "$",
                    Provenance::Parameter,
                )?;
                let accessed_type = self.instantiate_type_arguments(accessed_type, &type_args)?;

                if let (true, Some(val_type)) = (is_applicable, value_type) {
                    let ty =
                        self.apply_value_to_type(accessed_type, val_type, implicit_flow, None)?;
                    Ok((ty, Provenance::Unknown))
                } else {
                    Ok((accessed_type, accessed_prov))
                }
            }
            Some(ast::AccessSource::Identifier(name)) => {
                // Peek at the accessed type to decide whether to call or pop the flowing value.
                // Try the full path as a variable first (for captures), then resolve via types.
                let peeked_type = scopes::lookup_variable(&self.scopes, &name, &access.accessors)
                    .map(|(ty, _)| ty)
                    .or_else(|| {
                        let (base_type, _) = scopes::lookup_variable(&self.scopes, &name, &[])?;
                        self.peek_accessor_type(base_type, &access.accessors, &name)
                            .ok()
                    });
                let is_applicable = peeked_type.is_some_and(|ty| self.is_applicable_type(ty));

                // Non-applicable accessed with a flowing value: drop the value before loading.
                if !is_applicable && value_type.is_some() {
                    self.codegen.add_instruction(Instruction::pop());
                }
                let (accessed_type, accessed_prov) =
                    self.compile_member_access(&name, access.accessors)?;
                let accessed_type = self.instantiate_type_arguments(accessed_type, &type_args)?;

                if let (true, Some(val_type)) = (is_applicable, value_type) {
                    let ty =
                        self.apply_value_to_type(accessed_type, val_type, implicit_flow, None)?;
                    Ok((ty, Provenance::Unknown))
                } else {
                    Ok((accessed_type, accessed_prov))
                }
            }
            Some(ast::AccessSource::Ripple) => {
                if access.accessors.is_empty() {
                    // Bare ~ - the flowing value itself.
                    if let Some(val_type) = value_type {
                        // Already on the stack as the chained value.
                        let val_type = self.instantiate_type_arguments(val_type, &type_args)?;
                        Ok((val_type, value_provenance))
                    } else if let Some(ctx) = ripple_context {
                        // Inherit the ripple context from the enclosing tuple.
                        self.codegen
                            .add_instruction(Instruction::pick(ctx.stack_offset));
                        let ty = self.instantiate_type_arguments(ctx.value_type_id, &type_args)?;
                        Ok((ty, ctx.provenance.clone()))
                    } else {
                        Err(Error::FeatureUnsupported(
                            "Ripple placeholder (~) can only be used when a value is being chained"
                                .to_string(),
                        ))
                    }
                } else {
                    // ~.field - access a field on the flowing value.
                    let piped_type = value_type.ok_or_else(|| {
                        Error::FeatureUnsupported(
                            "Ripple access (~.field) requires a piped value".to_string(),
                        )
                    })?;
                    let (accessed_type, accessed_prov) =
                        self.compile_accessor(piped_type, access.accessors, "~", value_provenance)?;
                    let accessed_type =
                        self.instantiate_type_arguments(accessed_type, &type_args)?;
                    Ok((accessed_type, accessed_prov))
                }
            }
            Some(ast::AccessSource::Import(module)) => {
                // Resolve the import type first (no code emission yet) to check if callable. Hover
                // / go-to-definition entries are recorded per component by `record_access_components`.
                let (_cached, resolved_value, accessed_type, _origin) =
                    self.resolve_import(&module, &access.accessors)?;
                let accessed_type = self.instantiate_type_arguments(accessed_type, &type_args)?;
                // A member holding a type-consuming builtin instantiates here
                // (`%data.decode<'ev>`), so the emitted value carries the type id.
                let resolved_value = self.instantiate_builtin_member(
                    resolved_value,
                    &type_args,
                    value_type.is_some(),
                )?;

                let is_applicable = self.is_applicable_type(accessed_type);

                // Non-applicable accessed with a flowing value: drop the value before loading.
                if !is_applicable && value_type.is_some() {
                    self.codegen.add_instruction(Instruction::pop());
                }
                // Hoisted-slot gate, as in `compile_import`: load the synthetic capture
                // when the enclosing function registered one, else emit inline (sharing
                // repeated nodes).
                if type_args.is_empty()
                    && let Some((_, index)) = scopes::lookup_variable(
                        &self.scopes,
                        &variables::CaptureSource::Import(module.to_vec()).scope_name(),
                        &access.accessors,
                    )
                {
                    self.codegen.add_instruction(Instruction::load(index));
                } else {
                    self.emit_value_cse(&resolved_value)?;
                }

                if let (true, Some(val_type)) = (is_applicable, value_type) {
                    // The resolved member is a concrete function value, so its dispatch table can
                    // be selected exactly by index — vital when its type is shared with another
                    // dispatch function (e.g. `%num.add` vs `%num.div`).
                    let callee_fn = value_fn_index(&resolved_value);
                    let ty = self.apply_value_to_type(
                        accessed_type,
                        val_type,
                        implicit_flow,
                        callee_fn,
                    )?;
                    Ok((ty, Provenance::Unknown))
                } else {
                    Ok((accessed_type, Provenance::Unknown))
                }
            }
            Some(ast::AccessSource::Builtin(name)) => {
                // A builtin (`__integer_add__`): a globally-resolved callable. Its signature is recorded
                // as the access base by `record_access_components`.
                if let Some(span) = access.base_span.get() {
                    self.current_span = Some(span);
                }
                let builtin_type = self.compile_builtin(&name, &type_args, value_type.is_some())?;
                // A builtin has no fields, so accessors (`__x__.field`) fail here as a non-tuple.
                let (callable_type, _) = self.compile_accessor(
                    builtin_type,
                    access.accessors,
                    "__builtin__",
                    Provenance::Unknown,
                )?;
                let callable_type = self.instantiate_type_arguments(callable_type, &type_args)?;

                if let Some(val_type) = value_type {
                    let ty =
                        self.apply_value_to_type(callable_type, val_type, implicit_flow, None)?;
                    Ok((ty, Provenance::Unknown))
                } else {
                    Ok((callable_type, Provenance::Unknown))
                }
            }
            Some(ast::AccessSource::TailCall(identifier)) => {
                // `^` / `^f` / `^f.field` - a tail call (TCO). The flowing value (chained, or the
                // argument of an enclosing `Apply`) is the call argument, already on the stack.
                let ty =
                    self.compile_tail_call(identifier.as_deref(), &access.accessors, value_type)?;
                Ok((ty, Provenance::Unknown))
            }
            Some(ast::AccessSource::TailCallRipple) => {
                // `^~` - tail-call the flowing value (a nilary function) with nil.
                let ty = self.compile_ripple_tail_call(None, value_type)?;
                Ok((ty, Provenance::Unknown))
            }
            Some(ast::AccessSource::Self_) => {
                // Self_ should only appear in Term::Reference, not Term::Access
                Err(Error::InternalError {
                    message: "Self_ source in Access (should use Term::Self_ or Term::Reference)"
                        .to_string(),
                })
            }
        }
    }

    /// Compile select expression: ![sources...] or postfix form (empty sources with piped value)
    ///
    /// Select expects either a single value or a tuple on the stack:
    /// - Single value (process, function, or int): used as the sole source
    /// - Tuple: each element is used as a source
    fn compile_select(
        &mut self,
        sources: Option<Vec<ast::Chain>>,
        value_type: Option<usize>,
    ) -> Result<usize, Error> {
        // Handle the different select forms:
        // - None (bare `!`): postfix form, use chained value as source
        // - Some(vec![]): explicit empty `![]`, discard chained value, return nil
        // - Some(sources): explicit sources, discard chained value
        let sources = match sources {
            None => {
                // Bare `!` - postfix form using chained value
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Bare ! requires a piped value".to_string())
                })?;
                // Value is already on stack
                self.codegen.add_instruction(Instruction::select());
                return self.compute_select_return_type(&[val_type]);
            }
            Some(sources) if sources.is_empty() => {
                // Explicit `![]` - discard chained value, return nil
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::pop());
                }
                self.codegen.add_instruction(Instruction::tuple(NIL));
                return Ok(self.program.register_type(Type::nil()));
            }
            Some(sources) => sources,
        };

        // Compile all sources (explicit sources discard chained value)
        let source_types = self.compile_select_sources(&sources, value_type.as_ref())?;

        // If there was a chained value, remove it from the bottom of the stack
        if value_type.is_some() {
            self.codegen.emit_rotate_pop(sources.len() + 1);
        }

        // For multiple sources, create a tuple; for single source, leave as-is
        if sources.len() > 1 {
            let fields: Vec<(Option<String>, usize)> =
                source_types.iter().map(|&t| (None, t)).collect();
            let tuple_id = self.program.register_tuple(None, fields);
            self.codegen.add_instruction(Instruction::tuple(tuple_id));
        }

        // Emit Select instruction (handles both single value and tuple)
        self.codegen.add_instruction(Instruction::select());

        self.compute_select_return_type(&source_types)
    }

    /// Compute the return type of a select operation from source types
    fn compute_select_return_type(&mut self, source_types: &[usize]) -> Result<usize, Error> {
        let mut result_types = Vec::new();

        for &source_type_id in source_types {
            let source_type =
                self.program
                    .lookup_base(source_type_id)
                    .ok_or_else(|| Error::InternalError {
                        message: format!("Type ID {} not found", source_type_id),
                    })?;

            match source_type {
                Type::Process {
                    receive: Some(recv_type),
                    ..
                } => {
                    // Awaiting a process yields its result — or nil, since `!` is never
                    // lethal: a crashed source answers a `:crash`-stamped nil. The nil
                    // member is exact-rowed (the stamp is row-invisible, like `origin`),
                    // so it doesn't poison bare retrieval of ordinary keys through an
                    // await.
                    result_types.push(*recv_type);
                    result_types.push(annotations::closed_nil(self.program));
                }
                Type::Process { receive: None, .. } => {
                    return Err(Error::TypeMismatch {
                        expected: "process with receive type (awaitable/readable)".to_string(),
                        found: "process without receive type (cannot select)".to_string(),
                    });
                }
                Type::Callable { parameter, .. } => {
                    // Receive function - use its parameter type (the message type being received)
                    result_types.push(*parameter);
                }
                Type::Integer => {
                    // Timeout source: nil, stamped `:timeout` at runtime (row-invisible,
                    // hence the exact row here too).
                    result_types.push(annotations::closed_nil(self.program));
                }
                Type::Resource(name) => {
                    // A stream resource yields its registry-declared event type (a
                    // socket: `Data[sock, data] | Closed[sock]`; a listener:
                    // `Accepted[...] | Closed[...]`) — or nil, stamped `:error`, when
                    // the read fails: only a clean end is an event, so reading a
                    // stream is fallible like every other I/O operation. Resource
                    // kinds without a stream declaration have no externally-timed
                    // next event and are not selectable. Resolution registers the
                    // event tuples on demand — content-addressed, so they match the
                    // runtime's stream table and any source-level twins
                    // (`std/tcp.qv`).
                    let name = name.clone();
                    match self.builtins.stream_spec(&name).cloned() {
                        Some(spec) => {
                            let mut members = Vec::new();
                            if let Some(data) = &spec.data {
                                members.push(data.resolve_to_id(self.program));
                            }
                            if let Some((resource, _)) = &spec.resource {
                                members.push(resource.resolve_to_id(self.program));
                            }
                            members.push(spec.end.resolve_to_id(self.program));
                            members.push(annotations::closed_nil(self.program));
                            result_types.push(typing::union_type_ids(self.program, members));
                        }
                        None => {
                            return Err(Error::TypeMismatch {
                                expected: "a stream resource (one with a declared next event)"
                                    .to_string(),
                                found: format!("\\{name} (not a stream — no next event)"),
                            });
                        }
                    }
                }
                _ => {
                    return Err(Error::TypeMismatch {
                        expected: "process, function, resource, or integer (timeout)".to_string(),
                        found: quiver_core::format::format_type_by_id(
                            &*self.program,
                            source_type_id,
                        ),
                    });
                }
            }
        }

        if result_types.is_empty() {
            return Err(Error::FeatureUnsupported(
                "Select requires at least one source".to_string(),
            ));
        }

        Ok(typing::union_type_ids(self.program, result_types))
    }

    /// Lower a string literal to a `Str` wrapping the concatenation of each segment's bytes. Text
    /// segments contribute a binary constant; each interpolation hole is evaluated and must produce a
    /// `Str`, whose binary is extracted. Like a tuple's fields, a hole receives a copy of the flowing
    /// value as its input (so `~` inside it refers to the chained value); the original is kept beneath
    /// the accumulator and discarded at the end. With no flowing value, a hole's input is nil. Leaves
    /// a single `Str` on the stack — a plain, hole-free string is one text segment, so it compiles to
    /// just a binary constant wrapped in `Str`.
    fn compile_string(
        &mut self,
        segments: Vec<ast::StrSegment>,
        value_type: Option<usize>,
        value_provenance: Provenance,
    ) -> Result<(usize, Provenance), Error> {
        let binary_type = self.program.register_type(Type::Binary);
        // The `Str['bin]` type. The hole assertion target is the *plain* tuple — any `Str`
        // qualifies regardless of its annotation row (an exact-empty target would reject
        // open-rowed strings, e.g. ones typed by a declared parameter). The freshly built
        // result, by contrast, is provably annotation-free, so it carries an exact-empty row.
        let str_tuple = self
            .program
            .register_tuple(Some("Str".to_string()), vec![(None, binary_type)]);
        let plain_str_type = self.program.register_type(Type::Tuple(str_tuple));
        let str_type = annotations::exact_empty(self.program, plain_str_type);
        // The `['bin, 'bin]` argument tuple for each concatenation step.
        let pair_tuple = self
            .program
            .register_tuple(None, vec![(None, binary_type), (None, binary_type)]);

        if segments.is_empty() {
            // The empty string `""`: an empty binary, with nothing to concatenate.
            let empty = self.program.register_constant(Constant::Binary(Vec::new()));
            self.codegen.add_instruction(Instruction::constant(empty));
        }
        for (i, segment) in segments.into_iter().enumerate() {
            match segment {
                ast::StrSegment::Text(bytes) => {
                    let index = self.program.register_constant(Constant::Binary(bytes));
                    self.codegen.add_instruction(Instruction::constant(index));
                }
                ast::StrSegment::Hole(block) => {
                    let (param_type, param_provenance) = match value_type {
                        // Duplicate the flowing value as the hole's input. It sits beneath the
                        // accumulated binary — nothing before the first segment, one value after —
                        // so its offset from the top is 0 for the first segment and 1 thereafter.
                        Some(vt) => {
                            self.codegen
                                .add_instruction(Instruction::pick(usize::from(i > 0)));
                            (vt, value_provenance.clone())
                        }
                        None => {
                            self.codegen.add_instruction(Instruction::tuple(NIL));
                            (self.program.register_type(Type::nil()), Provenance::Unknown)
                        }
                    };
                    let hole_type = self.compile_scoped_block(
                        block,
                        param_type,
                        param_provenance,
                        None,
                        ScopeKind::Block,
                        false,
                    )?;
                    if !quiver_core::types::is_compatible(hole_type, plain_str_type, &*self.program)
                    {
                        return Err(Error::TypeMismatch {
                            expected: "Str".to_string(),
                            found: quiver_core::format::format_type_by_id(
                                &*self.program,
                                hole_type,
                            ),
                        });
                    }
                    // Unwrap `Str[<bin>]` to its binary for concatenation.
                    self.codegen.add_instruction(Instruction::get_positional(0));
                }
            }
            // Fold left: once a second binary is on the stack, concatenate it onto the accumulator.
            if i > 0 {
                self.codegen.add_instruction(Instruction::tuple(pair_tuple));
                // Push and apply the concat builtin, exactly as a `[a, b] __binary_concat__` call.
                self.compile_builtin("binary_concat", &[], true)?;
                self.codegen.add_instruction(Instruction::call());
            }
        }
        // Wrap the accumulated binary as `Str`.
        self.codegen.add_instruction(Instruction::tuple(str_tuple));
        // Discard the flowing value kept beneath the result for `~` holes.
        if value_type.is_some() {
            self.codegen.add_instruction(Instruction::rotate(2));
            self.codegen.add_instruction(Instruction::pop());
        }
        Ok((str_type, Provenance::Unknown))
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_term(
        &mut self,
        term: ast::Term,
        value: FlowingValue,
        on_no_match: Option<usize>,
        ripple_context: Option<&RippleContext>,
        mut narrowing: Option<&mut Narrowing>,
        // The type this term is expected to produce (from a call argument's callee). Only used to
        // infer un-annotated function-literal parameters; ignored by every other term.
        expected: Option<usize>,
        // Whether the enclosing chain's result gates control flow (see
        // `compile_chain_with_input`); consulted only by `Match` terms, whose
        // narrowing is unsound where nothing gates on the verdict.
        gating: bool,
    ) -> Result<(usize, Provenance), Error> {
        let FlowingValue {
            ty: value_type,
            provenance: value_provenance,
        } = value;
        // Track the span of the term being compiled so a compile error can be located.
        if let Some(span) = term.span() {
            self.current_span = Some(span);
        }
        match term {
            ast::Term::Literal(literal) => {
                // Literals don't use the piped value, drop it
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::pop());
                }
                let ty = self.compile_literal(literal)?;
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Tuple(tuple) => {
                let tuple_span = tuple.span.get();
                // Always flow the piped value into the tuple's fields: each field receives a
                // copy as its input, so a callable field is called with it (and `&` is needed
                // to pass a callable by value), while non-callable fields drop it. The original
                // is owned and cleaned up by compile_tuple.
                let ripple_context_value;
                let ripple_context_param = if let Some(vt) = value_type.as_ref() {
                    ripple_context_value = RippleContext {
                        value_type_id: *vt,
                        stack_offset: 0,
                        owns_value: true,
                        provenance: value_provenance,
                    };
                    Some(&ripple_context_value)
                } else {
                    ripple_context
                };

                let (ty, tuple_prov) =
                    self.compile_tuple(tuple.name, tuple.fields, ripple_context_param, expected)?;
                // Hover on the tuple (its `[` / name) shows the constructed composite type.
                self.record_typed(tuple_span, ty, SymbolKind::Expression, None);
                // Return tuple provenance for field access tracking
                Ok((ty, tuple_prov))
            }
            ast::Term::Block(block) => {
                // Blocks can fail for non-type reasons (pattern matches, literal comparisons),
                // so disable complement narrowing
                if let Some(n) = narrowing.as_deref_mut() {
                    n.disable();
                }

                // Blocks take their input as a parameter
                let block_parameter =
                    value_type.unwrap_or_else(|| self.program.register_type(Type::nil()));
                // Pass the provenance from the chain to the block
                let block_provenance = value_provenance.clone();
                if value_type.is_none() {
                    // Blocks without a value need NIL on stack
                    self.codegen.add_instruction(Instruction::tuple(NIL));
                }
                let ty = self.compile_scoped_block(
                    block,
                    block_parameter,
                    block_provenance,
                    None,
                    ScopeKind::Block,
                    false,
                )?;
                // Block results have unknown provenance
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::String(_, segments) => {
                // A string literal produces a `Str`, replacing the flowing value — but, like a tuple,
                // each interpolation hole receives a copy of that value (so `~` works inside a hole).
                // The delimiter style is irrelevant to the compiled value.
                self.compile_string(segments, value_type, value_provenance)
            }
            ast::Term::Function(func) => {
                // Function literals always produce functions - they don't auto-call.
                // To call an inline function, bind it first: f = #'int {...}, 5 f
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::pop());
                }

                let span = func.span.get();
                // An un-annotated literal infers its parameter from the expected callable type.
                let expected_parameter = func
                    .parameter_type
                    .is_none()
                    .then_some(expected)
                    .flatten()
                    .and_then(|exp| match self.program.lookup_type(exp) {
                        Some(Type::Callable { parameter, .. }) => Some(*parameter),
                        _ => None,
                    });
                // With no callable expected type, an un-annotated literal falls back to a nil
                // parameter. If its body then fails while actually reading `$`, the real
                // problem is almost always the missed inference: point at where it works.
                let inference_fell_back = func.parameter_type.is_none()
                    && expected_parameter.is_none()
                    && func.body.as_ref().is_some_and(block_references_parameter);
                let function_type =
                    self.compile_function(func, expected_parameter)
                        .map_err(|e| {
                            if inference_fell_back {
                                Error::Noted {
                                    error: Box::new(e),
                                    note: "this `#{…}` literal's parameter defaulted to nil — \
                                       inference needs a known callee (`f […, #{…}]`, or piped \
                                       directly: `[…, #{…}] ~> f`), so restructure the call or \
                                       annotate the parameter"
                                        .to_string(),
                                }
                            } else {
                                e
                            }
                        })?;
                // Hover on `#` shows the inferred function type.
                self.record_typed(span, function_type, SymbolKind::Expression, None);
                Ok((function_type, Provenance::Unknown))
            }
            ast::Term::Access(access) => {
                // A builtin call can fail for non-type reasons, so disable complement narrowing
                // (matching the tail-call path and the former Term::Builtin arm).
                if matches!(
                    access.source,
                    Some(ast::AccessSource::Builtin(_)) | Some(ast::AccessSource::TailCall(_))
                ) && let Some(n) = narrowing.as_deref_mut()
                {
                    n.disable();
                }
                // A bare access in a chain receives the implicit chain flow.
                self.compile_access(
                    access,
                    value_type,
                    value_provenance.clone(),
                    ripple_context,
                    true,
                )
            }
            ast::Term::Match(pattern) => {
                // Match patterns can create new bindings or check against existing values/types
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Match requires a value".to_string())
                })?;
                // In a value position (gating == false) a fallible match's verdict is
                // data and gates nothing, so its bindings could never be relied on.
                // The chain loop enforces this for chain terms; this covers a match
                // reaching here as a bare term — e.g. an application argument
                // (`f =(T)x`), where the silent alternative was a nil-filled binding.
                let mut value_position_bindings = Vec::new();
                if on_no_match.is_none() && !gating {
                    collect_binding_spans(&pattern, &mut value_position_bindings);
                }
                let ty = self.compile_match(
                    pattern,
                    val_type,
                    value_provenance.clone(),
                    on_no_match,
                    narrowing,
                    gating,
                )?;
                if !value_position_bindings.is_empty() && self.contains_nil(ty) {
                    return Err(Error::FallibleMatchBindingsInValueChain {
                        bindings: value_position_bindings
                            .into_iter()
                            .map(|(n, _)| n)
                            .collect(),
                    });
                }
                // Preserve the matched value's provenance: a chain/branch that follows a
                // `=PATTERN` still narrows the original value (the match recorded its structural
                // narrowing against this provenance inside `compile_match`). The term now yields
                // the verdict `Ok`/`[]` rather than the matched value, so callers that interpret
                // the *verdict* as the provenance's value (the `=>` forward nil-narrowing) guard
                // against the disjoint verdict type collapsing the matched type to never.
                Ok((ty, value_provenance))
            }
            ast::Term::Spawn(function, argument, span) => {
                let ty = match (function.is_bare_ripple(), argument) {
                    (true, Some(argument)) => {
                        // `@~ arg`: the flowing value is the function to spawn; the juxtaposed
                        // argument — compiled without the flow, which the head consumed — is
                        // the init. Stack: [function] -> [function, arg] -> [arg, function].
                        let fn_type = value_type.ok_or_else(|| {
                            Error::FeatureUnsupported(
                                "Ripple spawn requires piped value".to_string(),
                            )
                        })?;
                        let (arg_type, _) = self.compile_term(
                            *argument,
                            FlowingValue {
                                ty: None,
                                provenance: Provenance::Unknown,
                            },
                            on_no_match,
                            None,
                            None,
                            None,
                            false,
                        )?;
                        self.codegen.add_instruction(Instruction::rotate(2));
                        self.emit_arg_spawn(fn_type, arg_type)?
                    }
                    (_, argument) => {
                        // A juxtaposed argument (`@f x`) supplies the init, with the flowing
                        // value flowing into it (`10 ~> @adder [~, 5]`); otherwise the flowing
                        // value itself is the (implicit) init.
                        let explicit = argument.is_some();
                        let init_type = match argument {
                            Some(argument) => Some(
                                self.compile_term(
                                    *argument,
                                    FlowingValue {
                                        ty: value_type,
                                        provenance: value_provenance,
                                    },
                                    on_no_match,
                                    ripple_context,
                                    None,
                                    None,
                                    false,
                                )?
                                .0,
                            ),
                            None => value_type,
                        };
                        self.compile_spawn(*function, init_type, explicit)?
                    }
                };
                // Hover on `@` shows the spawned process's type.
                self.record_typed(span.get(), ty, SymbolKind::Expression, None);
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Apply(access, argument)
                if matches!(access.source, Some(ast::AccessSource::TailCallRipple)) =>
            {
                // `^~ x`: tail-call the flowing value (the function) with `x`. Like the ripple
                // head below, `^~` consumes the flowing value, so the argument is evaluated
                // without it.
                let ty = self.compile_ripple_tail_call(Some(*argument), value_type)?;
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Apply(access, argument)
                if matches!(access.source, Some(ast::AccessSource::Ripple)) =>
            {
                // Ripple head (`~ [args]`, `~.field [args]`): the head consumes the flowing
                // value (`~` *is* it; `~.field` reads the field off it), producing a callable,
                // and the argument is applied to that result. The argument therefore does not
                // receive the flowing value.
                let (callable_type, _) = self.compile_access(
                    access,
                    value_type,
                    value_provenance,
                    ripple_context,
                    true,
                )?;
                let (arg_type, _) = self.compile_term(
                    *argument,
                    FlowingValue {
                        ty: None,
                        provenance: Provenance::Unknown,
                    },
                    on_no_match,
                    None,
                    None,
                    None,
                    false,
                )?;
                // The callable is below the argument on the stack; swap so the call sees it on
                // top. An explicit argument is type-checked, not an implicit flow.
                self.codegen.add_instruction(Instruction::rotate(2));
                let ty = self.apply_value_to_type(callable_type, arg_type, false, None)?;
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Apply(access, argument) => {
                // Looked-up head: the flowing value flows into the argument (so `f [~, 1]`
                // works), and the head is then invoked with the argument's result. The head's
                // parameter type (resolved without emitting code) is the expected type of the
                // argument, so an un-annotated function-literal argument can infer its
                // parameter from it.
                let expected_arg = self.callee_parameter_type(&access);
                // Omitted fields are filled from the callee's `:defaults` before the
                // argument compiles, so the literal reaching `compile_tuple` is complete.
                let argument = match self.fill_defaults(&access, &argument)? {
                    Some(tuple) => Box::new(ast::Term::Tuple(tuple)),
                    None => argument,
                };
                let (arg_type, arg_prov) = self.compile_term(
                    *argument,
                    FlowingValue {
                        ty: value_type,
                        provenance: value_provenance,
                    },
                    on_no_match,
                    ripple_context,
                    None,
                    expected_arg,
                    false,
                )?;
                // A builtin/tail call can fail for non-type reasons, so disable complement
                // narrowing.
                if matches!(
                    access.source,
                    Some(ast::AccessSource::Builtin(_)) | Some(ast::AccessSource::TailCall(_))
                ) && let Some(n) = narrowing.as_deref_mut()
                {
                    n.disable();
                }
                // The head is invoked with the explicit argument, not an implicit flow, so a
                // nilary head rejects it rather than ignoring it.
                self.compile_access(access, Some(arg_type), arg_prov, None, false)
            }
            ast::Term::Select(select, span) => {
                let ty = self.compile_select(select, value_type)?;
                // Hover on `!` shows the received/awaited result type.
                self.record_typed(span.get(), ty, SymbolKind::Expression, None);
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Self_ => {
                self.codegen.add_instruction(Instruction::self_());
                // Return a process type with the current function's receive type.
                // Return type is None since a process can't know its own return type;
                // state is None too — the enclosing *function's* states union is not the
                // spawned process's (the pid may outlive this frame in a helper), so
                // granting it here would be unsound.
                let self_type = self.program.register_type(Type::Process {
                    send: Some(self.current_receive_type_id),
                    receive: None,
                    state: None,
                });

                // Apply value if present (for message sends like `10 ~> .`)
                let result_type = if let Some(val_type) = value_type {
                    self.apply_value_to_type(self_type, val_type, false, None)?
                } else {
                    self_type
                };
                Ok((result_type, Provenance::Unknown))
            }
            ast::Term::Process(process_id) => {
                // Look up process info from the map (REPL-only feature)
                let (process_type, function_index) = self
                    .process_types
                    .get(&process_id)
                    .cloned()
                    .ok_or_else(|| Error::InternalError {
                    message: format!("Process {} not found", process_id),
                })?;

                // The target's id travels as an ordinary integer constant, which `Process`
                // pops; the instruction itself names only the root function.
                let id_constant = self
                    .program
                    .register_constant(Constant::Integer(process_id.into()));
                self.codegen
                    .add_instruction(Instruction::constant(id_constant));
                self.codegen
                    .add_instruction(Instruction::process(function_index));

                // Apply value if present (for message sends like `10 ~> @1`)
                let result_type = if let Some(val_type) = value_type {
                    self.apply_value_to_type(process_type, val_type, false, None)?
                } else {
                    process_type
                };
                Ok((result_type, Provenance::Unknown))
            }
            ast::Term::Dialect(dialect) => {
                // Expand at compile time to an ordinary block term (which receives the
                // flowing value, so `Ripple` works like a block parameter), then compile
                // the expansion in place — splices are type-checked like handwritten code.
                let expansion = self.expand_dialect(&dialect)?;
                self.compile_term(
                    expansion,
                    FlowingValue {
                        ty: value_type,
                        provenance: value_provenance,
                    },
                    on_no_match,
                    ripple_context,
                    narrowing,
                    expected,
                    gating, // the expansion stands in the original term's chain position
                )
            }
            ast::Term::State(access, span) => {
                // `?p` — sample a process's state. The result is
                // the target process type's state component: inferred at spawn sites, or
                // stated with a `?'s` clause on a declared process type. No runtime test —
                // soundness rests on every state write being compile-checked, plus the
                // strict state subtyping at declared boundaries. The flowing value is
                // unused: the target names the process explicitly.
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::pop());
                }

                // Load the target without calling it (exactly as `&p` compiles).
                let (target_type, _) = self.compile_term(
                    ast::Term::Reference(access),
                    FlowingValue {
                        ty: None,
                        provenance: Provenance::Unknown,
                    },
                    None,
                    None,
                    None,
                    None,
                    false,
                )?;
                let Some(Type::Process { state, .. }) = self.program.lookup_base(target_type)
                else {
                    return Err(Error::TypeMismatch {
                        expected: "process".to_string(),
                        found: quiver_core::format::format_type_by_id(&*self.program, target_type),
                    });
                };
                let Some(result) = *state else {
                    return Err(Error::TypeMismatch {
                        expected: "process with a state type (inferred from its spawn, or \
                                   granted by a `?'s` clause on the declared process type)"
                            .to_string(),
                        found: quiver_core::format::format_type_by_id(&*self.program, target_type),
                    });
                };

                self.codegen.add_instruction(Instruction::state());

                self.record_typed(span.get(), result, SymbolKind::Expression, None);
                Ok((result, Provenance::Unknown))
            }
            ast::Term::Reference(access) => {
                // Explicit reference: drop incoming value and load the referenced value without calling
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::pop());
                }

                // The reference's span (`foo` in `&foo`, `%num.add` in `&%num.add`), for
                // hover and go-to-definition on the referenced symbol.
                let ref_span = access.span.get();

                // Explicit type arguments (`&f<'int>`): instantiate the referenced
                // callable's type after loading — the value is untouched.
                let type_args = access.type_arguments;

                // Load the referenced value
                let (ty, prov) = match access.source {
                    Some(ast::AccessSource::Identifier(ref name)) => {
                        let label = accessors_label(name, &access.accessors);
                        let (accessed_type, accessed_prov) =
                            self.compile_member_access(name, access.accessors)?;
                        self.record_reference(ref_span, name, label, accessed_type);
                        Ok((accessed_type, accessed_prov))
                    }
                    Some(ast::AccessSource::Parameter { depth: 0 }) => {
                        // &$ - reference to function parameter
                        let (param_type, param_local) =
                            scopes::get_function_parameter(&self.scopes)?;
                        self.codegen.add_instruction(Instruction::load(param_local));
                        let (accessed_type, accessed_prov) = self.compile_accessor(
                            param_type,
                            access.accessors,
                            "$",
                            Provenance::Parameter,
                        )?;
                        self.record_typed(
                            ref_span,
                            accessed_type,
                            SymbolKind::Parameter,
                            Some("$".to_string()),
                        );
                        Ok((accessed_type, accessed_prov))
                    }
                    Some(ast::AccessSource::Parameter { depth }) => {
                        // `&$$…` references the captured outer value, like a captured variable.
                        let name = variables::CaptureSource::OuterParameter(depth).scope_name();
                        if scopes::lookup_variable(&self.scopes, &name, &access.accessors)
                            .or_else(|| scopes::lookup_variable(&self.scopes, &name, &[]))
                            .is_none()
                        {
                            return Err(Error::ParameterDepthExceeded {
                                written: ast::parameter_sigils(depth),
                            });
                        }
                        let (accessed_type, accessed_prov) =
                            self.compile_member_access(&name, access.accessors)?;
                        self.record_typed(
                            ref_span,
                            accessed_type,
                            SymbolKind::Parameter,
                            Some(name),
                        );
                        Ok((accessed_type, accessed_prov))
                    }
                    Some(ast::AccessSource::Import(ref module)) => {
                        let label =
                            accessors_label(&format!("%{}", module.join("/")), &access.accessors);
                        let (ty, origin) =
                            self.compile_import(module, &access.accessors, &type_args)?;
                        self.record_import(
                            ref_span,
                            ty,
                            Some(label),
                            origin,
                            module,
                            &access.accessors,
                        );
                        Ok((ty, Provenance::Unknown))
                    }
                    Some(ast::AccessSource::Self_) => {
                        // &. - reference to self (current process); state ungranted, as
                        // for bare `.` (see the Self_ term arm).
                        self.codegen.add_instruction(Instruction::self_());
                        let self_type = self.program.register_type(Type::Process {
                            send: Some(self.current_receive_type_id),
                            receive: None,
                            state: None,
                        });
                        Ok((self_type, Provenance::Unknown))
                    }
                    Some(ast::AccessSource::Builtin(ref name)) => {
                        // &__builtin__ - the builtin function value, without applying it. A
                        // type-consuming builtin may be referenced un-instantiated (this is
                        // how a module exports one).
                        let builtin_type = self.compile_builtin(name, &type_args, false)?;
                        self.record_typed(
                            ref_span,
                            builtin_type,
                            SymbolKind::Builtin,
                            Some(format!("__{}__", name)),
                        );
                        Ok((builtin_type, Provenance::Unknown))
                    }
                    Some(ast::AccessSource::Ripple) => Err(Error::FeatureUnsupported(
                        "Cannot reference ripple (~) - use it directly".to_string(),
                    )),
                    Some(ast::AccessSource::TailCall(_) | ast::AccessSource::TailCallRipple) => {
                        Err(Error::FeatureUnsupported(
                            "Cannot reference a tail call (^) - reference the function instead"
                                .to_string(),
                        ))
                    }
                    None => Err(Error::FeatureUnsupported(
                        "Reference requires an identifier (e.g., &f)".to_string(),
                    )),
                }?;
                let ty = self.instantiate_type_arguments(ty, &type_args)?;
                Ok((ty, prov))
            }
        }
    }

    /// Resolve cycles in a function's result type after a call
    ///
    /// When a function is called, its result may contain Cycle(n) references.
    /// A Cycle(n) means "go up n boundaries from here".
    ///
    /// This function traverses the result type, tracking depth (boundaries crossed).
    /// When it encounters Cycle(n) at depth D from the function:
    /// - To reach the function boundary from depth D requires going up (D+1) boundaries
    /// - If n == D+1, the cycle points to the function itself → resolve it
    /// - Otherwise, keep the cycle as-is
    fn resolve_function_cycles(
        &mut self,
        type_id: usize,
        function_type_id: usize,
        depth_from_function: usize,
    ) -> usize {
        let typ = match self.program.lookup_type(type_id) {
            Some(t) => t,
            None => return type_id,
        };

        match typ {
            Type::Cycle(n) => {
                // Cycle(n) means "n boundaries upward from here"
                // We're at depth_from_function boundaries inside the result
                // To reach the function: need to go up (depth_from_function + 1) boundaries
                //   - depth_from_function to exit the result's boundaries
                //   - +1 to exit the function boundary itself
                if *n == depth_from_function + 1 {
                    function_type_id
                } else {
                    // Cycle points elsewhere (could be to a boundary inside the result,
                    // or to something even further out)
                    type_id
                }
            }
            Type::Union(variants) => {
                // Union is a boundary - increment depth
                let variants_clone = variants.clone();
                let mut resolved_variants = Vec::new();
                for v in variants_clone {
                    let resolved =
                        self.resolve_function_cycles(v, function_type_id, depth_from_function + 1);
                    resolved_variants.push(resolved);
                }
                typing::union_type_ids(self.program, resolved_variants)
            }
            Type::Callable {
                parameter,
                result,
                receive,
                states,
            } => {
                // Nested function is a boundary - increment depth
                let param_id = *parameter;
                let result_id = *result;
                let receive_id = *receive;
                let states_id = *states;
                let resolved_param = self.resolve_function_cycles(
                    param_id,
                    function_type_id,
                    depth_from_function + 1,
                );
                let resolved_result = self.resolve_function_cycles(
                    result_id,
                    function_type_id,
                    depth_from_function + 1,
                );
                let resolved_receive = self.resolve_function_cycles(
                    receive_id,
                    function_type_id,
                    depth_from_function + 1,
                );
                let resolved_states = states_id.map(|s| {
                    self.resolve_function_cycles(s, function_type_id, depth_from_function + 1)
                });
                self.program.register_type(Type::Callable {
                    parameter: resolved_param,
                    result: resolved_result,
                    receive: resolved_receive,
                    states: resolved_states,
                })
            }
            Type::Tuple(tuple_id) => {
                // Tuples are not boundaries - maintain depth
                if let Some(type_info) = self.program.lookup_tuple(*tuple_id).cloned() {
                    let new_fields: Vec<_> = type_info
                        .fields
                        .into_iter()
                        .map(|(name, field_type_id)| {
                            let resolved_field_type = self.resolve_function_cycles(
                                field_type_id,
                                function_type_id,
                                depth_from_function,
                            );
                            (name, resolved_field_type)
                        })
                        .collect();
                    let new_tuple_id = self.program.register_tuple(type_info.name, new_fields);
                    self.program.register_type(Type::Tuple(new_tuple_id))
                } else {
                    type_id
                }
            }
            Type::Partial { name, fields } => {
                // Partials are not boundaries - maintain depth
                // Clone upfront to avoid borrow issues with self.resolve_function_cycles
                let partial_name = name.clone();
                let partial_fields = fields.clone();
                let new_fields: Vec<_> = partial_fields
                    .into_iter()
                    .map(|(fname, field_type_id)| {
                        let resolved_field_type = self.resolve_function_cycles(
                            field_type_id,
                            function_type_id,
                            depth_from_function,
                        );
                        (fname, resolved_field_type)
                    })
                    .collect();
                self.program.register_type(Type::Partial {
                    name: partial_name,
                    fields: new_fields,
                })
            }
            Type::Process {
                send,
                receive,
                state,
            } => {
                // Process types don't create boundaries but may contain types with cycles
                let send_id = *send;
                let receive_id = *receive;
                let state_id = *state;
                let resolved_send = send_id.map(|t| {
                    self.resolve_function_cycles(t, function_type_id, depth_from_function)
                });
                let resolved_receive = receive_id.map(|t| {
                    self.resolve_function_cycles(t, function_type_id, depth_from_function)
                });
                let resolved_state = state_id.map(|t| {
                    self.resolve_function_cycles(t, function_type_id, depth_from_function)
                });
                self.program.register_type(Type::Process {
                    send: resolved_send,
                    state: resolved_state,
                    receive: resolved_receive,
                })
            }
            _ => type_id, // Integer, Binary, Variable, Resource don't contain nested types
        }
    }

    /// Materialize the current narrowed type of the block parameter, merging any per-field
    /// narrowings (recorded against `Provenance::Parameter`) onto a single-tuple base. Used to
    /// capture a branch's guard type — the parameter values that reach it — for the dispatch
    /// table. Falls back to the whole-parameter narrowing when the base is not a single tuple.
    fn current_parameter_guard(&mut self) -> usize {
        let base =
            narrowing::get_type_for_provenance(&self.scopes, &Provenance::Parameter, self.program);

        let field_narrowings: Vec<(usize, usize)> = self
            .scopes
            .last()
            .map(|s| {
                s.narrowings
                    .fields
                    .iter()
                    .filter(|(prov, _, _)| matches!(prov, Provenance::Parameter))
                    .map(|(_, idx, ty)| (*idx, *ty))
                    .collect()
            })
            .unwrap_or_default();

        if field_narrowings.is_empty() {
            return base;
        }

        let Some(Type::Tuple(tuple_id)) = self.program.lookup_type(base).cloned() else {
            return base;
        };
        let Some(info) = self.program.lookup_tuple(tuple_id).cloned() else {
            return base;
        };

        let mut fields = info.fields;
        for (idx, ty) in field_narrowings {
            if let Some(field) = fields.get_mut(idx) {
                field.1 = ty;
            }
        }
        let new_tuple_id = self.program.register_tuple(info.name, fields);
        self.program.register_type(Type::Tuple(new_tuple_id))
    }

    /// The parameter guard type for a dispatch branch: the complement-narrowed parameter (from
    /// `current_parameter_guard`, capturing earlier branches' failures) further refined by this
    /// branch's own positive pattern. Per-field narrowing is needed because the general narrowing
    /// machinery only filters top-level union variants, never a single tuple's field types — so a
    /// pattern like `=[Rational[..], y]` would otherwise leave the parameter unrefined.
    fn branch_parameter_guard(&mut self, branch: &ast::Branch) -> usize {
        let base = self.current_parameter_guard();

        let Some(ast::Match::Tuple(pattern)) = leading_match(branch) else {
            return base;
        };
        let Some(Type::Tuple(tuple_id)) = self.program.lookup_type(base).cloned() else {
            return base;
        };
        let Some(info) = self.program.lookup_tuple(tuple_id).cloned() else {
            return base;
        };
        if info.fields.len() != pattern.fields.len() {
            return base;
        }

        let mut fields = info.fields;
        let mut refined = false;
        for (idx, field) in pattern.fields.iter().enumerate() {
            if let ast::Match::Tuple(sub) = &field.pattern
                && let Some(constrained) =
                    narrowing::matching_tuple_type(sub, fields[idx].1, self.program)
            {
                fields[idx].1 = constrained;
                refined = true;
            }
        }
        if !refined {
            return base;
        }
        let new_tuple_id = self.program.register_tuple(info.name, fields);
        self.program.register_type(Type::Tuple(new_tuple_id))
    }

    /// If the called function has a dispatch case table, compute its result type from the
    /// concrete argument type: the union of the result types of every branch whose parameter
    /// guard could match the argument. Returns `None` when there is no table or no branch
    /// applies (the caller then falls back to the function's frozen result type).
    ///
    /// The table is found by the statically-known callee function index when available (always
    /// exact), else by the callable type — which resolves only when that type isn't shared by
    /// dispatch functions with differing tables.
    fn dispatch_result(
        &mut self,
        callable_type_id: usize,
        callee_fn: Option<usize>,
        arg_type: usize,
    ) -> Option<usize> {
        // Case tables are keyed by the bare callable type; an annotated function's
        // exposed type is row-wrapped, so peel it before the lookup.
        let callable_type_id = Type::strip_annotations(callable_type_id, &*self.program);
        let fn_index = callee_fn.or_else(|| self.case_tables.get(&callable_type_id).copied())?;
        let table = self.fn_case_tables.get(&fn_index)?.clone();
        let results: Vec<usize> = table
            .into_iter()
            .filter(|(guard, _)| {
                quiver_core::types::types_overlap(*guard, arg_type, &*self.program)
            })
            .map(|(_, result)| result)
            .collect();
        if results.is_empty() {
            None
        } else {
            Some(typing::union_type_ids(self.program, results))
        }
    }

    /// Whether a flowing value *applies* to a value of this type (a call or a send)
    /// rather than replacing it. Callables and processes apply — and so does a union
    /// with any callable or process member: `apply_value_to_type` then either compiles
    /// the send (a union of only process types) or rejects the application, so a handle
    /// hidden in a union can never silently compile as a replace that discards the
    /// flowing value.
    fn is_applicable_type(&self, type_id: usize) -> bool {
        match self.program.lookup_base(type_id) {
            Some(Type::Callable { .. }) | Some(Type::Process { .. }) => true,
            Some(Type::Union(members)) => members.iter().any(|&member| {
                matches!(
                    self.program.lookup_base(member),
                    Some(Type::Callable { .. }) | Some(Type::Process { .. })
                )
            }),
            _ => false,
        }
    }

    /// Instantiate a callable type's declared type parameters with explicit type
    /// arguments (`f<'int>`): resolve each written argument, bind it to the callable's
    /// corresponding declared parameter (positionally — a prefix is allowed, the rest
    /// stay inferred), and substitute. Purely static: the value is untouched, only the
    /// type the use site sees narrows, so unification then *checks* the pinned
    /// parameters instead of inferring them. An annotation row on the callable rides
    /// through. The declared-parameter list comes from `callable_type_params`, recorded
    /// at the definition — a callable reached through a declared boundary (a written
    /// parameter type) has no entry and cannot be instantiated, like other
    /// definition-carried capabilities.
    fn instantiate_type_arguments(
        &mut self,
        type_id: usize,
        type_arguments: &[ast::Type],
    ) -> Result<usize, Error> {
        if type_arguments.is_empty() {
            return Ok(type_id);
        }

        // An annotation row rides through instantiation: unwrap, substitute, re-wrap.
        let (base_id, row) = match self.program.lookup_type(type_id) {
            Some(Type::Annotated {
                base,
                exact,
                entries,
            }) => (*base, Some((*exact, entries.clone()))),
            _ => (type_id, None),
        };

        let not_applicable = |program: &Program| Error::TypeArgumentsNotApplicable {
            target: quiver_core::format::format_type_by_id(program, type_id),
        };

        if !matches!(
            self.program.lookup_type(base_id),
            Some(Type::Callable { .. })
        ) {
            return Err(not_applicable(self.program));
        }
        // A builtin's parameters are session vocabulary, never carried by cached
        // modules (membership in a module's tables would be session history) — so a
        // builtin member reached through a cached value derives them on demand here,
        // matching the callable against the registry by signature.
        if !self.callable_type_params.contains_key(&base_id)
            && !self.builtin_type_params.contains_key(&base_id)
            && let Some(Type::Callable {
                parameter, result, ..
            }) = self.program.lookup_type(base_id).cloned()
            && let Some(name) = self
                .program
                .get_builtins()
                .iter()
                .find(|info| info.param_type == parameter && info.result_type == result)
                .map(|info| info.name.clone())
        {
            self.record_builtin_type_params(&name, base_id, parameter, result);
        }
        // Function entries take precedence: a module function can share its callable
        // type with a builtin, and the value being instantiated is more likely its own.
        let Some(params) = self
            .callable_type_params
            .get(&base_id)
            .or_else(|| self.builtin_type_params.get(&base_id))
            .cloned()
        else {
            return Err(not_applicable(self.program));
        };
        if type_arguments.len() > params.len() {
            return Err(Error::TypeArgumentsTooMany {
                declared: params.len(),
                given: type_arguments.len(),
            });
        }

        let mut bindings = HashMap::new();
        for (param, argument) in params.iter().zip(type_arguments) {
            let mut env = typing::TypeEnv {
                resolver: self.resolver,
                module_cache: &mut *self.module_cache,
                package: &self.current_package,
            };
            let resolved =
                typing::resolve_ast_type(&mut env, &self.scopes, argument.clone(), self.program)?;
            bindings.insert(param.clone(), resolved);
        }

        let instantiated = typing::substitute(base_id, &bindings, self.program);
        Ok(match row {
            Some((exact, entries)) => self.program.register_type(Type::Annotated {
                base: instantiated,
                exact,
                entries,
            }),
            None => instantiated,
        })
    }

    fn apply_value_to_type(
        &mut self,
        target_type_id: usize,
        value_type: usize,
        implicit_flow: bool,
        callee_fn: Option<usize>,
    ) -> Result<usize, Error> {
        let target_type =
            self.program
                .lookup_base(target_type_id)
                .ok_or_else(|| Error::InternalError {
                    message: format!("Type ID {} not found", target_type_id),
                })?;

        if let Type::Callable {
            parameter,
            result,
            receive,
            states: _,
        } = target_type
        {
            // Function call (a normal call is not a state transition — only tail calls
            // widen the states union)
            let param_id = *parameter;
            let result_id = *result;
            let receive_id = *receive;

            // A nilary callable ignores an implicitly-flowing value: it is called with nil and the
            // value is discarded, like a literal. An explicit argument (`implicit_flow == false`,
            // e.g. `f [5]`) is still type-checked, so handing a value to a nilary callable errors.
            let ignore_value = implicit_flow && self.is_nil(param_id);
            let arg_type = if ignore_value {
                self.program.register_type(Type::nil())
            } else {
                value_type
            };

            // Check if function has type variables - if so, perform unification
            let has_vars_param = typing::contains_variables(param_id, &*self.program);
            let has_vars_result = typing::contains_variables(result_id, &*self.program);

            let result_type = if has_vars_param || has_vars_result {
                // Perform unification to bind type variables
                let mut bindings = HashMap::new();

                typing::unify(&mut bindings, param_id, arg_type, self.program)?;

                // Substitute bindings in the result type, closing unpinned parameters
                // to the empty union (see close_unpinned_result).
                typing::close_unpinned_result(
                    result_id,
                    &mut bindings,
                    self.type_param_suffix,
                    self.program,
                );
                typing::substitute(result_id, &bindings, self.program)
            } else {
                // No type variables - just check compatibility
                if !quiver_core::types::is_compatible(arg_type, param_id, &*self.program) {
                    return Err(Error::TypeMismatch {
                        expected: format!(
                            "function parameter compatible with {}",
                            quiver_core::format::format_type_by_id(&*self.program, param_id)
                        ),
                        found: quiver_core::format::format_type_by_id(&*self.program, value_type),
                    });
                }
                // If this function dispatches on its parameter, specialize the result type to
                // the concrete argument; otherwise fall back to its single frozen result.
                self.dispatch_result(target_type_id, callee_fn, arg_type)
                    .unwrap_or(result_id)
            };

            // Resolve cycles in result type that refer to the function itself
            // Start at depth 0 since we haven't descended into any boundaries yet
            let result_type = self.resolve_function_cycles(result_type, target_type_id, 0);

            // Calling a function executes its receives in this process, so its receive
            // type widens the current context's.
            self.widen_receive_type(receive_id);

            // A nilary callable ignoring a non-nil flow: replace the value on the stack with nil
            // before calling. Stack: [value, callable] -> [callable] -> [nil, callable].
            if ignore_value && !self.is_nil(value_type) {
                self.codegen.add_instruction(Instruction::rotate(2));
                self.codegen.add_instruction(Instruction::pop());
                self.codegen.add_instruction(Instruction::tuple(NIL));
                self.codegen.add_instruction(Instruction::rotate(2));
            }

            // Execute the call, wrapping it with debug-mode `:pre`/`:post` contract
            // enforcement when the callee's static type carries those contracts.
            self.emit_call_with_contracts(target_type_id, param_id, result_id)?;
            Ok(result_type)
        } else if let Type::Process {
            send: send_type, ..
        } = target_type
        {
            // Send to process
            // Type check: ensure it has a send type and value type matches
            let send_id = *send_type;
            if let Some(expected_send_type_id) = send_id {
                // Check if it's the empty union (never accepts sends)
                if self.is_never(expected_send_type_id) {
                    return Err(Error::TypeMismatch {
                        expected: "process with send type".to_string(),
                        found: "process without send type (cannot send to it)".to_string(),
                    });
                }
                if !quiver_core::types::is_compatible(
                    value_type,
                    expected_send_type_id,
                    &*self.program,
                ) {
                    return Err(Error::TypeMismatch {
                        expected: quiver_core::format::format_type_by_id(
                            &*self.program,
                            expected_send_type_id,
                        ),
                        found: quiver_core::format::format_type_by_id(&*self.program, value_type),
                    });
                }
            } else {
                // None means unknown send type
                return Err(Error::TypeMismatch {
                    expected: "process with known send type".to_string(),
                    found: "process with unknown send type".to_string(),
                });
            }

            // Emit send instruction (expects [value, process] on stack)
            self.codegen.add_instruction(Instruction::send());

            Ok(target_type_id)
        } else if let Type::Union(members) = target_type {
            // A union of only process types compiles as a send, checked against every
            // member's send type — whichever member the value turns out to be at
            // runtime must accept the message. Any other function/process-bearing union
            // is an error: applying to it would be a send or call for some members and
            // a replace for others, and a silent replace discards the flowing value.
            let members = members.clone();
            let mut send_ids = Vec::with_capacity(members.len());
            let mut all_processes = !members.is_empty();
            let mut all_functions = !members.is_empty();
            for &member in &members {
                match self.program.lookup_base(member) {
                    Some(Type::Process { send, .. }) => {
                        all_functions = false;
                        send_ids.push(*send);
                    }
                    Some(Type::Callable { .. }) => all_processes = false,
                    _ => {
                        all_processes = false;
                        all_functions = false;
                    }
                }
            }
            if all_processes {
                for send in send_ids {
                    let Some(send_id) = send else {
                        return Err(Error::TypeMismatch {
                            expected: "process with known send type".to_string(),
                            found: format!(
                                "union member with unknown send type in {}",
                                quiver_core::format::format_type_by_id(
                                    &*self.program,
                                    target_type_id
                                )
                            ),
                        });
                    };
                    if self.is_never(send_id) {
                        return Err(Error::TypeMismatch {
                            expected: "process with send type".to_string(),
                            found: format!(
                                "union member without send type (cannot send to it) in {}",
                                quiver_core::format::format_type_by_id(
                                    &*self.program,
                                    target_type_id
                                )
                            ),
                        });
                    }
                    if !quiver_core::types::is_compatible(value_type, send_id, &*self.program) {
                        return Err(Error::TypeMismatch {
                            expected: quiver_core::format::format_type_by_id(
                                &*self.program,
                                send_id,
                            ),
                            found: quiver_core::format::format_type_by_id(
                                &*self.program,
                                value_type,
                            ),
                        });
                    }
                }
                self.codegen.add_instruction(Instruction::send());
                Ok(target_type_id)
            } else {
                Err(Error::UnionApplication {
                    union: quiver_core::format::format_type_by_id(&*self.program, target_type_id),
                    all_functions,
                })
            }
        } else {
            Err(Error::TypeMismatch {
                expected: "function, process, or resource".to_string(),
                found: quiver_core::format::format_type_by_id(&*self.program, target_type_id),
            })
        }
    }

    /// Widen the current context's receive type by a callee's: calling (or
    /// tail-calling) a function executes its receives in this process, so the
    /// enclosing function's receive type must cover them.
    fn widen_receive_type(&mut self, called_receive_type: usize) {
        if self.is_never(called_receive_type) {
            return;
        }
        if self.is_never(self.current_receive_type_id) {
            self.current_receive_type_id = called_receive_type;
        } else if !quiver_core::types::is_compatible(
            called_receive_type,
            self.current_receive_type_id,
            &*self.program,
        ) {
            self.current_receive_type_id =
                self.unify_receive_types(vec![self.current_receive_type_id, called_receive_type]);
        }
    }

    /// Widen the current function's states union by a tail-call target's: entering the
    /// target re-enters the root frame with its argument, so its states become
    /// observable. An unknown-states target (`None`) poisons the whole union — the
    /// compiled function's spawn then grants no sampling.
    fn widen_states(&mut self, target_states: Option<usize>) {
        self.current_states = match (self.current_states, target_states) {
            (Some(current), Some(target)) if current == target => Some(current),
            (Some(current), Some(target)) => {
                Some(typing::union_type_ids(self.program, vec![current, target]))
            }
            _ => None,
        };
    }

    /// Type-check a tail call against the target's callable type: the argument must fit
    /// the parameter (unifying type variables when the target is generic), and the
    /// target's receive type widens the current context's, exactly as a normal call
    /// does. Returns the (substituted) result type.
    fn check_tail_call_types(
        &mut self,
        callable_type_id: usize,
        arg_type: usize,
    ) -> Result<usize, Error> {
        let Some(Type::Callable {
            parameter,
            result,
            receive,
            states,
        }) = self.program.lookup_base(callable_type_id)
        else {
            return Err(Error::TypeMismatch {
                expected: "function".to_string(),
                found: quiver_core::format::format_type_by_id(&*self.program, callable_type_id),
            });
        };
        let (param_id, result_id, receive_id, states_id) = (*parameter, *result, *receive, *states);

        let has_vars = typing::contains_variables(param_id, &*self.program)
            || typing::contains_variables(result_id, &*self.program);
        let result_type = if has_vars {
            let mut bindings = HashMap::new();
            typing::unify(&mut bindings, param_id, arg_type, self.program)?;
            typing::close_unpinned_result(
                result_id,
                &mut bindings,
                self.type_param_suffix,
                self.program,
            );
            typing::substitute(result_id, &bindings, self.program)
        } else {
            if !quiver_core::types::is_compatible(arg_type, param_id, &*self.program) {
                return Err(Error::TypeMismatch {
                    expected: format!(
                        "function parameter compatible with {}",
                        quiver_core::format::format_type_by_id(&*self.program, param_id)
                    ),
                    found: quiver_core::format::format_type_by_id(&*self.program, arg_type),
                });
            }
            result_id
        };

        self.widen_receive_type(receive_id);
        self.widen_states(states_id);
        Ok(result_type)
    }

    fn compile_tail_call(
        &mut self,
        identifier: Option<&str>,
        accessors: &[ast::AccessPath],
        arg_type: Option<usize>,
    ) -> Result<usize, Error> {
        // Handle argument - if none provided, check if function parameter is nil and use that
        let arg_type = if let Some(arg_t) = arg_type {
            arg_t
        } else {
            let (func_param_type, _) = scopes::get_function_parameter_declared(&self.scopes)?;
            if func_param_type == self.program.register_type(Type::nil()) {
                // Push nil onto stack for tail call
                let nil_tuple_id = self.program.register_tuple(None, vec![]);
                self.codegen
                    .add_instruction(Instruction::tuple(nil_tuple_id));
                self.program.register_type(Type::nil())
            } else {
                return Err(Error::FeatureUnsupported(
                    "Tail call requires a value".to_string(),
                ));
            }
        };

        if identifier.is_none() && accessors.is_empty() {
            // Self tail call: the frame is re-entered with the argument, so it must fit
            // the enclosing function's *declared* parameter type — not the current
            // branch's narrowed view (its own receives are already in the receive
            // seed — no widening needed).
            let (func_param_type, _) = scopes::get_function_parameter_declared(&self.scopes)?;
            if !quiver_core::types::is_compatible(arg_type, func_param_type, &*self.program) {
                return Err(Error::TypeMismatch {
                    expected: format!(
                        "function parameter compatible with {}",
                        quiver_core::format::format_type_by_id(&*self.program, func_param_type)
                    ),
                    found: quiver_core::format::format_type_by_id(&*self.program, arg_type),
                });
            }
            self.codegen.add_instruction(Instruction::recurse());
            Ok(self.program.never())
        } else {
            // Tail call to identifier with accessors
            let name = identifier.ok_or_else(|| {
                Error::FeatureUnsupported(
                    "Member access in tail call requires an identifier".to_string(),
                )
            })?;

            let func_type = if accessors.is_empty() {
                // Simple identifier lookup
                match scopes::lookup_variable(&self.scopes, name, &[]) {
                    Some((func_type, index)) => {
                        self.codegen.add_instruction(Instruction::load(index));
                        func_type
                    }
                    None => return Err(Error::VariableUndefined(name.to_string())),
                }
            } else {
                // Use member access compilation for accessors
                self.compile_member_access(name, accessors.to_vec())?.0
            };

            // Verify it's a function, check the argument fits its parameter, and widen
            // the receive type (the callee's receives run in this process).
            let result_type = self.check_tail_call_types(func_type, arg_type)?;
            self.codegen.add_instruction(Instruction::tail_call());
            Ok(result_type)
        }
    }

    /// Compile a ripple tail call (`^~`, `^~ x`): tail-call the flowing value, which must be a
    /// function and is already on the stack. The argument comes by juxtaposition (`^~ x`), or is
    /// nil for the bare form; either way it does not receive the flowing value (which is the
    /// function being called).
    fn compile_ripple_tail_call(
        &mut self,
        argument: Option<ast::Term>,
        value_type: Option<usize>,
    ) -> Result<usize, Error> {
        let fn_type = value_type.ok_or_else(|| {
            Error::FeatureUnsupported("`^~` tail call requires a piped function".to_string())
        })?;
        let Some(Type::Callable {
            parameter,
            result,
            receive,
            states,
        }) = self.program.lookup_base(fn_type)
        else {
            return Err(Error::TypeMismatch {
                expected: "function".to_string(),
                found: quiver_core::format::format_type_by_id(&*self.program, fn_type),
            });
        };
        let (parameter, result, receive, states) = (*parameter, *result, *receive, *states);
        self.widen_receive_type(receive);
        self.widen_states(states);

        match argument {
            Some(argument) => {
                // `^~ x`: the juxtaposed argument is evaluated without the flow (the head
                // consumed it as the function) and type-checked against its parameter.
                let (arg_type, _) = self.compile_term(
                    argument,
                    FlowingValue {
                        ty: None,
                        provenance: Provenance::Unknown,
                    },
                    None,
                    None,
                    None,
                    None,
                    false,
                )?;
                if !quiver_core::types::is_compatible(arg_type, parameter, &*self.program) {
                    return Err(Error::TypeMismatch {
                        expected: quiver_core::format::format_type_by_id(&*self.program, parameter),
                        found: quiver_core::format::format_type_by_id(&*self.program, arg_type),
                    });
                }
            }
            None => {
                // Bare `^~` supplies no argument, so the flowing function must take nil.
                let nil_type_id = self.program.register_type(Type::nil());
                if parameter != nil_type_id {
                    return Err(Error::TypeMismatch {
                        expected: "nilary function".to_string(),
                        found: quiver_core::format::format_type_by_id(&*self.program, fn_type),
                    });
                }
                let nil_tuple_id = self.program.register_tuple(None, vec![]);
                self.codegen
                    .add_instruction(Instruction::tuple(nil_tuple_id));
            }
        }

        // Stack: [function, argument] -> [argument, function], as the tail call expects.
        self.codegen.add_instruction(Instruction::rotate(2));
        self.codegen.add_instruction(Instruction::tail_call());
        Ok(result)
    }

    fn compile_builtin(
        &mut self,
        name: &str,
        type_arguments: &[ast::Type],
        applied: bool,
    ) -> Result<usize, Error> {
        let (param_type, result_type) = self
            .builtins
            .resolve_signature(name, self.program)
            .ok_or_else(|| Error::BuiltinUndefined(name.to_string()))?;

        // Register the types
        let param_type_id = self.program.register_type(param_type);
        let result_type_id = self.program.register_type(result_type);

        // A **type-consuming** builtin (declared type parameters in the registry) needs
        // its type argument at runtime, so a direct *application* must instantiate
        // explicitly with a concrete type; the instantiation is its own builtin-table
        // entry, and the emitted instruction names it. A bare *reference*
        // (`&__data_decode__`, a module export) may stay un-instantiated — the
        // requirement then falls to whichever site names it with type arguments, and
        // calling a never-instantiated one is a runtime error. Static instantiation of
        // the signature happens separately, through the same
        // `instantiate_type_arguments` every access head gets.
        let type_argument = self.resolve_builtin_type_argument(name, type_arguments, applied)?;

        let builtin_index = self.program.register_builtin_instantiated(
            name.to_string(),
            type_argument,
            self.builtins,
        );

        self.codegen
            .add_instruction(Instruction::builtin(builtin_index));

        let never_id = self.program.never();
        let callable_type_id = self.program.register_type(Type::Callable {
            parameter: param_type_id,
            result: result_type_id,
            receive: never_id,
            // Builtins never tail-call: their states are their parameter.
            states: Some(param_type_id),
        });

        self.record_builtin_type_params(name, callable_type_id, param_type_id, result_type_id);

        Ok(callable_type_id)
    }

    /// Resolve a type-consuming builtin's explicit type argument to the concrete type
    /// id its emitted instruction carries — `None` for ordinary builtins, and for a
    /// bare (un-applied) reference given no arguments. Shared between direct builtin
    /// accesses and import members holding a builtin value.
    fn resolve_builtin_type_argument(
        &mut self,
        name: &str,
        type_arguments: &[ast::Type],
        applied: bool,
    ) -> Result<Option<usize>, Error> {
        let declared = self
            .builtins
            .get_type_parameters(name)
            .unwrap_or_default()
            .len();
        if declared == 0 {
            return Ok(None);
        }
        if type_arguments.is_empty() && !applied {
            return Ok(None);
        }
        if type_arguments.len() < declared {
            return Err(Error::TypeArgumentsRequired {
                builtin: name.to_string(),
                declared,
            });
        }
        let mut env = typing::TypeEnv {
            resolver: self.resolver,
            module_cache: &mut *self.module_cache,
            package: &self.current_package,
        };
        let resolved = typing::resolve_ast_type(
            &mut env,
            &self.scopes,
            type_arguments[0].clone(),
            self.program,
        )?;
        if typing::contains_variables(resolved, &*self.program) {
            return Err(Error::TypeArgumentNotConcrete {
                builtin: name.to_string(),
            });
        }
        Ok(Some(resolved))
    }

    /// An import member holding a **type-consuming builtin** value (a module export
    /// like `%data.decode`): explicit type arguments instantiate it at the naming site
    /// — the rebuilt value carries the resolved type id into the emitted push — while
    /// an *applied* member without them errors exactly as a direct application does. A
    /// bare reference passes through un-instantiated. Ordinary members are untouched.
    fn instantiate_builtin_member(
        &mut self,
        value: Value,
        type_arguments: &[ast::Type],
        applied: bool,
    ) -> Result<Value, Error> {
        let Value::Builtin(builtin_id, ref payload) = value else {
            return Ok(value);
        };
        let Some(name) = self
            .program
            .get_builtins()
            .get(builtin_id)
            .map(|b| b.name.clone())
        else {
            return Ok(value);
        };
        let Some(type_argument) =
            self.resolve_builtin_type_argument(&name, type_arguments, applied)?
        else {
            return Ok(value);
        };
        // Rebuild the value carrying the argument, preserving any annotations the
        // module attached (e.g. a `:doc`).
        let annotations = payload
            .as_deref()
            .map(|p| p.annotations().to_vec())
            .unwrap_or_default();
        Ok(Value::Builtin(
            builtin_id,
            Some(
                Payload::with_annotations(vec![], annotations)
                    .with_type_argument(Some(type_argument))
                    .shared(),
            ),
        ))
    }

    /// Record a builtin callable's type parameters for explicit instantiation: the
    /// registry-declared list for a type-consuming builtin (which may not mention them
    /// in its signature at all), else the signature's variables in first-occurrence
    /// order (parameter, then result) — for the current generic builtins these coincide
    /// with declaration order.
    fn record_builtin_type_params(
        &mut self,
        name: &str,
        callable_type_id: usize,
        param: usize,
        result: usize,
    ) {
        if self.builtin_type_params.contains_key(&callable_type_id) {
            return;
        }
        let declared = self.builtins.get_type_parameters(name).unwrap_or_default();
        let names = if !declared.is_empty() {
            declared.to_vec()
        } else {
            let mut names = Vec::new();
            typing::collect_type_variables(param, &*self.program, &mut names);
            typing::collect_type_variables(result, &*self.program, &mut names);
            names
        };
        if !names.is_empty() {
            self.builtin_type_params.insert(callable_type_id, names);
        }
    }

    /// Compiles accessor chain and tracks field provenance.
    fn compile_accessor(
        &mut self,
        mut last_type: usize,
        accessors: Vec<ast::AccessPath>,
        target_name: &str,
        base_provenance: Provenance,
    ) -> Result<(usize, Provenance), Error> {
        let mut current_prov = base_provenance;

        for accessor in accessors {
            // Annotation retrieval: works on tuples and callables, yields value-or-nil.
            // The bare form types by the visibility rules; the checked form `:('t)key`
            // is total, gated by a runtime shape test where the rows can't vouch.
            if let ast::AccessPath::Annotation(name, expected) = &accessor {
                let (key_id, result_type, check) = match expected {
                    None => {
                        let (key_id, result_type) =
                            annotations::retrieval_type(self.program, last_type, name)?;
                        (key_id, result_type, None)
                    }
                    Some(ast_type) => {
                        let mut env = typing::TypeEnv {
                            resolver: self.resolver,
                            module_cache: &mut *self.module_cache,
                            package: &self.current_package,
                        };
                        let asked = typing::resolve_ast_type(
                            &mut env,
                            &self.scopes,
                            ast_type.clone(),
                            self.program,
                        )?;
                        let (key_id, result_type, needs_check) =
                            annotations::checked_retrieval_type(
                                self.program,
                                last_type,
                                name,
                                asked,
                            );
                        (key_id, result_type, needs_check.then_some(asked))
                    }
                };
                self.codegen
                    .add_instruction(Instruction::get_annotation(key_id));
                // The checked form (`x:('t)key`) gates the retrieved entry on its expected
                // shape, answering nil when it does not fit. That is a test the retrieval
                // itself need not know about: keep the entry when it matches, else discard
                // it for nil.
                if let Some(type_id) = check {
                    self.codegen.add_instruction(Instruction::duplicate());
                    self.codegen.add_instruction(Instruction::is_type(type_id));
                    let fits = self.codegen.emit_jump_if_placeholder();
                    self.codegen.add_instruction(Instruction::pop());
                    self.codegen.add_instruction(Instruction::nil());
                    self.codegen.patch_jump_to_here(fits);
                }
                last_type = result_type;
                // The retrieved value is detached from the carrier's fields.
                current_prov = Provenance::Unknown;
                continue;
            }

            let (access, field_types) = match accessor {
                ast::AccessPath::Field(field_name) => type_queries::get_field_by_name(
                    self.program,
                    last_type,
                    &field_name,
                    target_name,
                )?,
                ast::AccessPath::Index(index) => {
                    let field_types = type_queries::get_field_at_index(
                        &*self.program,
                        last_type,
                        index,
                        target_name,
                    )?;
                    (type_queries::FieldAccess::Position(index), field_types)
                }
                ast::AccessPath::Annotation(..) => unreachable!("handled above"),
            };

            self.codegen.add_instruction(match access {
                type_queries::FieldAccess::Position(index) => Instruction::get_positional(index),
                type_queries::FieldAccess::Named { name, .. } => Instruction::get_named(name),
            });
            last_type = typing::union_type_ids(self.program, field_types);
            // Update provenance to track the field access (by the field's index in the
            // static type's field list; without an agreed index the trail ends here).
            current_prov = match access.static_index() {
                Some(index) => current_prov.field(index),
                None => Provenance::Unknown,
            };

            // If the field provenance resolved to a Variable and that variable stores
            // a Tuple provenance, use the Tuple provenance for nested tuple tracking.
            // Keep Variable provenance for non-tuple variables so narrowing works correctly.
            if let Provenance::Variable(ref var_name) = current_prov
                && let Some(stored_prov @ Provenance::Tuple(_)) =
                    scopes::lookup_variable_provenance(&self.scopes, var_name)
            {
                current_prov = stored_prov;
            }
        }

        Ok((last_type, current_prov))
    }

    /// Compiles member access and tracks provenance.
    fn compile_member_access(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
    ) -> Result<(usize, Provenance), Error> {
        // Check if we have a capture for the full path (base + accessors)
        if !accessors.is_empty()
            && let Some((capture_type, capture_index)) =
                scopes::lookup_variable(&self.scopes, target, &accessors)
        {
            // We have a pre-evaluated capture for this exact path
            self.codegen
                .add_instruction(Instruction::load(capture_index));
            // Captures lose provenance tracking
            return Ok((capture_type, Provenance::Unknown));
        }

        // No pre-evaluated capture, use standard member access
        let (last_type, index) = scopes::lookup_variable(&self.scopes, target, &[])
            .ok_or(Error::VariableUndefined(target.to_string()))?;
        self.codegen.add_instruction(Instruction::load(index));

        // Determine the base provenance for this access:
        // - If no field access (empty accessors), use Variable provenance so narrowing affects
        //   this variable directly
        // - If field access and the stored provenance is a Tuple, use it so field access can
        //   resolve through to original source provenances
        // - Otherwise use Variable provenance for proper field narrowing
        let base_prov = if accessors.is_empty() {
            Provenance::Variable(target.to_string())
        } else {
            match scopes::lookup_variable_provenance(&self.scopes, target) {
                Some(prov @ Provenance::Tuple(_)) => prov,
                _ => Provenance::Variable(target.to_string()),
            }
        };
        self.compile_accessor(last_type, accessors, target, base_prov)
    }
}
