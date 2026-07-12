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
pub use modules::ModuleCache;
pub use provenance::{Narrowings, Provenance};
pub use scopes::{Binding, Parameter, Scope, ScopeKind};
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
    value::{Binary, Value},
};

#[derive(Debug, PartialEq)]
pub enum Error {
    // Undefined errors
    VariableUndefined(String),
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
fn expression_references_parameter(expression: &ast::Expression) -> bool {
    let chain = |chain: &ast::Chain| chain.terms.iter().any(term_references_parameter);
    expression.annotations.iter().any(|a| chain(&a.value))
        || expression.branches.iter().any(|branch| {
            branch.condition.chains.iter().any(chain)
                || branch
                    .consequence
                    .as_ref()
                    .is_some_and(|c| c.chains.iter().any(chain))
        })
}

fn term_references_parameter(term: &ast::Term) -> bool {
    let chain = |chain: &ast::Chain| chain.terms.iter().any(term_references_parameter);
    match term {
        ast::Term::Access(access) | ast::Term::Reference(access) => {
            matches!(access.source, Some(ast::AccessSource::Parameter))
        }
        ast::Term::State(access, _) => {
            matches!(access.source, Some(ast::AccessSource::Parameter))
        }
        ast::Term::Tuple(tuple) => tuple.fields.iter().any(|field| match &field.value {
            ast::FieldValue::Chain(c) => chain(c),
            ast::FieldValue::Spread(_) => false,
        }),
        ast::Term::String(_, segments) => segments.iter().any(|segment| match segment {
            ast::StrSegment::Hole(expression) => expression_references_parameter(expression),
            ast::StrSegment::Text(_) => false,
        }),
        ast::Term::Block(expression) => expression_references_parameter(expression),
        // A nested function literal's `$` is its own parameter.
        ast::Term::Function(_) => false,
        ast::Term::Apply(access, argument) => {
            matches!(access.source, Some(ast::AccessSource::Parameter))
                || term_references_parameter(argument)
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
            Error::Noted { error, note } => write!(f, "{error} ({note})"),
            Error::BuiltinUndefined(name) => write!(f, "Undefined builtin: {name}"),
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
                write!(f, "Execution error in module '{module}': {error:?}")
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

/// The products of a successful compilation. The caller-owned `program`, `module_cache`, and
/// (optional) semantic recorder are borrowed by [`Compiler::compile`] and mutated in place,
/// so they are not returned here — only the genuine outputs are.
pub struct Compiled {
    pub instructions: Vec<Instruction>,
    pub result_type: usize,
    pub receive_type: usize,
    pub bindings: HashMap<String, Binding>,
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
}

impl Default for CompileOptions {
    fn default() -> Self {
        CompileOptions {
            debug: false,
            source_name: "main".to_string(),
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
/// contribute to a function's call-site dispatch table.
fn branch_is_parameter_dispatch(branch: &ast::Branch) -> bool {
    branch.condition.chains.len() == 1
        && branch.condition.chains[0]
            .terms
            .iter()
            .all(|t| matches!(t, ast::Term::Match(_)))
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

/// The leading pattern of a branch condition (its binding `match_pattern`, or a first
/// `=pattern` term), through which the branch dispatches on the parameter.
fn leading_match(branch: &ast::Branch) -> Option<&ast::Match> {
    let chain = branch.condition.chains.first()?;
    if let Some(m) = &chain.match_pattern {
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
    /// parameter type, widened at each tail call by the target's states (see
    /// docs/process-state.md). `None` = unknown (poisoned by a `^~` on an unknown-states
    /// callee); baked into the registered `Callable` like `current_receive_type_id`.
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

    // Span of the term currently being compiled, so an error can be located in source.
    // Only set when a recorder is interested (LSP); harmless otherwise.
    current_span: Option<SourceSpan>,
    /// The type-parameter uniquifying suffix of the top-level function definition being
    /// compiled, inherited by nested literals (see `compile_function`).
    type_param_suffix: Option<usize>,
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
        ast_program: ast::Program,
        existing_bindings: &HashMap<String, Binding>,
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
            fn_case_tables: HashMap::new(),
            case_tables: HashMap::new(),
            current_span: None,
            type_param_suffix: None,
            function_depth: 0,
            debug: options.debug,
            current_module: options.source_name,
            recorder,
            _phantom: std::marker::PhantomData,
        };

        // Prepare scope bindings from existing bindings
        let mut scope_bindings = HashMap::new();

        // Clone all existing bindings
        for (name, binding) in existing_bindings {
            scope_bindings.insert(name.clone(), binding.clone());
        }

        // Calculate local_count from variables
        compiler.local_count = existing_bindings
            .values()
            .filter_map(|binding| {
                if let Binding::Variable { index, .. } = binding {
                    Some(*index)
                } else {
                    None
                }
            })
            .max()
            .map(|max_index| max_index + 1)
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

        // Only allocate parameter slot if we have expressions
        // (CType definitions and imports don't need parameters)
        let has_expressions = ast_program
            .statements
            .iter()
            .any(|s| matches!(s, ast::Statement::Expression(_)));

        let scope_parameter = if has_expressions {
            let param_local = compiler.local_count;
            compiler.local_count += 1;

            compiler.codegen.add_instruction(Instruction::Store);

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

        // Extract receive type from statements (like we do for function bodies)
        // This seeds the receive type from explicit selects in the code.
        // Additional receive types may be adopted during compilation when calling
        // functions that have receive types.
        compiler.current_receive_type_id =
            compiler.extract_receive_type_from_statements(&ast_program.statements)?;

        // The recorder is caller-owned, so whatever it gathered before an error (and the program it
        // indexes) is still available to the caller for hover/go-to-definition on the parts that
        // compiled.
        let result_type_id = match compiler.compile_top_level(ast_program.statements) {
            Ok(ty) => ty,
            Err(error) => {
                return Err(LocatedError {
                    error,
                    span: compiler.current_span,
                });
            }
        };

        // Extract bindings from the global scope
        let bindings: HashMap<String, Binding> = compiler.scopes[0]
            .bindings
            .iter()
            .filter(|(name, _)| !name.starts_with('~')) // Exclude internal variables
            .map(|(name, binding)| (name.clone(), binding.clone()))
            .collect();

        Ok(Compiled {
            instructions: compiler.codegen.instructions,
            result_type: result_type_id,
            // Use the final receive type, which may have been widened during compilation
            // when calling functions that have receive types
            receive_type: compiler.current_receive_type_id,
            bindings,
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

    /// Compile a program/module body: a single threaded sequence of expression chains with
    /// type-alias declarations interspersed. Expressions thread (each one starts from the previous
    /// one's result) and short-circuit on nil, just like the chains within a sequence; type aliases
    /// are transparent to that flow. Bindings persist across the whole body. Leaves the final value
    /// on the stack and returns its type (nil if there are no expressions).
    fn compile_top_level(&mut self, statements: Vec<ast::Statement>) -> Result<usize, Error> {
        let last_expr_index = statements
            .iter()
            .rposition(|s| matches!(s, ast::Statement::Expression(_)));
        let mut result_type_id = self.program.register_type(Type::nil());
        let mut threaded: Option<usize> = None;
        let mut end_jumps = Vec::new();
        for (i, statement) in statements.into_iter().enumerate() {
            match statement {
                ast::Statement::TypeAlias {
                    name,
                    type_parameters,
                    type_definition,
                    ..
                } => {
                    // Transparent to the value flow: registers the alias, no stack effect.
                    self.compile_type_alias(name.as_deref(), type_parameters, type_definition)?;
                }
                ast::Statement::Expression(sequence) => {
                    // Thread from the previous expression's result (left on the stack), nil-stripped
                    // since a nil result short-circuits to the end.
                    let input = threaded.map(|t| self.without_nil(t));
                    let (ty, _prov) = self.compile_sequence(sequence, None, input, None)?;
                    result_type_id = ty;
                    // Keep the value on the stack for the next expression and short-circuit on nil,
                    // unless this is the final expression (its result is the module value).
                    if Some(i) != last_expr_index {
                        end_jumps.push(self.codegen.emit_duplicate_jump_if_nil());
                    }
                    threaded = Some(ty);
                }
            }
        }
        let end_addr = self.codegen.instructions.len();
        for jump in end_jumps {
            self.codegen.patch_jump_to_addr(jump, end_addr);
        }
        Ok(result_type_id)
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
        if !annotations::is_annotatable(self.program, carrier_type) {
            return Err(Error::TypeUnresolved(format!(
                "Annotations require a tuple or function carrier, but the annotated value has type {}",
                quiver_core::format::format_type_by_id(&*self.program, carrier_type)
            )));
        }
        let mut entries: Vec<(usize, usize)> = Vec::new();
        for annotation in annotation_list {
            let key_id = annotations::intern_key(self.program, &annotation.name);
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
            self.codegen.add_instruction(Instruction::Tuple(NIL));
            let closed_nil = annotations::closed_nil(self.program);
            let (value_type, _) = self.compile_chain_with_input(
                annotation.value,
                None,
                None,
                Some((closed_nil, Provenance::Unknown)),
                None,
                false,
                expected,
            )?;
            if let Some(expected) = expected
                && !quiver_core::types::is_compatible(value_type, expected, &*self.program)
            {
                return Err(Error::TypeMismatch {
                    expected: quiver_core::format::format_type_by_id(&*self.program, expected),
                    found: quiver_core::format::format_type_by_id(&*self.program, value_type),
                });
            }
            self.codegen.add_instruction(Instruction::Annotate(key_id));
            entries.push((key_id, value_type));
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
                // Recursively validate field types
                for field in &tuple.fields {
                    if let ast::FieldType::Field { type_def, .. } = field {
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
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(self.program.register_type(Type::Integer))
            }
            ast::Literal::Binary(bytes) => {
                let index = self
                    .program
                    .register_constant(Constant::Binary(bytes.clone()));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(self.program.register_type(Type::Binary))
            }
        }
    }

    /// Resolve the name a name-inheriting spread (`~[...]`, `a[...]`) takes from its first
    /// spread's source: the variable's tuple type for `...a`, or the flowing value's for `...`.
    fn inherited_spread_name(
        &self,
        fields: &[ast::TupleField],
        ripple_context: Option<&RippleContext>,
    ) -> Option<String> {
        let source = fields.iter().find_map(|f| match &f.value {
            ast::FieldValue::Spread(s) => Some(s),
            _ => None,
        })?;
        let source_type = match source {
            Some(var) => scopes::lookup_variable(&self.scopes, var, &[]).map(|(ty, _)| ty)?,
            None => ripple_context?.value_type_id,
        };
        let source_type = Type::strip_annotations(source_type, &*self.program);
        match self.program.lookup_type(source_type) {
            Some(Type::Tuple(tuple_id)) => self
                .program
                .lookup_tuple(*tuple_id)
                .and_then(|t| t.name.clone()),
            _ => None,
        }
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
        let expected_fields = expected.and_then(|e| self.expected_tuple_fields(e, fields.len()));
        let mut bindings: HashMap<String, usize> = HashMap::new();

        // Compile field values and collect their types and provenances
        let mut field_types = Vec::new();
        let mut field_provenances = Vec::new();
        for (fields_compiled, field) in fields.iter().enumerate() {
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
                            .add_instruction(Instruction::Pick(ctx.stack_offset + fields_compiled));
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

            field_types.push((field.name.clone(), field_type));
            field_provenances.push(field_prov);
        }

        // Register the tuple type and emit instruction
        let tuple_id = self.program.register_tuple(tuple_name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(tuple_id));

        // Clean up ripple value if we own it
        if let Some(ctx) = ripple_context
            && ctx.owns_value
        {
            self.codegen.add_instruction(Instruction::Rotate(2));
            self.codegen.add_instruction(Instruction::Pop);
        }

        // A tuple literal is freshly built, provably annotation-free: an exact-empty
        // row, so retrieval on it (or on unions containing it) types absent keys as
        // provably absent rather than rejecting them as possibly-erased.
        let result_type = self.program.register_type(Type::Tuple(tuple_id));
        let result_type = annotations::exact_empty(self.program, result_type);

        Ok((result_type, Provenance::Tuple(field_provenances)))
    }

    /// The positional field types of `expected` if it is a tuple type with exactly `arity`
    /// fields, for driving per-field inference. A non-tuple or mismatched arity yields `None`
    /// (no inference), so the existing all-or-nothing call check still produces any real error.
    fn expected_tuple_fields(&self, expected: usize, arity: usize) -> Option<Vec<usize>> {
        let Some(Type::Tuple(tuple_id)) = self.program.lookup_type(expected) else {
            return None;
        };
        let info = self.program.lookup_tuple(*tuple_id)?;
        if info.fields.len() != arity {
            return None;
        }
        Some(info.fields.iter().map(|(_, ty)| *ty).collect())
    }

    /// The parameter type of an applied head (`f` in `f [args]`), resolved without emitting code,
    /// so a function-literal argument can infer its parameter from it. Returns `None` when the
    /// head isn't a statically-resolvable callable (e.g. `~`, `^`, or a non-callable), leaving
    /// the argument to compile without an expected type.
    fn callee_parameter_type(&mut self, access: &ast::Access) -> Option<usize> {
        let callable = match &access.source {
            Some(ast::AccessSource::Identifier(name) | ast::AccessSource::TailCall(Some(name))) => {
                // A captured member (`iter.fold` inside a closure) is bound under its full path, so
                // try that first; otherwise resolve the base binding (`iter`, a local record) and
                // follow the accessors (`.fold`) to the member.
                if let Some((ty, _)) =
                    scopes::lookup_variable(&self.scopes, name, &access.accessors)
                {
                    ty
                } else {
                    let base =
                        scopes::lookup_variable(&self.scopes, name, &[]).map(|(ty, _)| ty)?;
                    self.follow_accessors(base, &access.accessors)?
                }
            }
            Some(ast::AccessSource::Import(module)) => {
                // `resolve_import` applies the accessors, yielding the member type directly.
                self.resolve_import(module, &access.accessors)
                    .ok()
                    .map(|(_, _, ty, _)| ty)?
            }
            Some(ast::AccessSource::Builtin(name)) => {
                // A builtin's signature gives its parameter directly — no need to assemble a
                // Callable just to take it apart again below.
                let (param, _) = self.builtins.resolve_signature(name, self.program)?;
                return Some(self.program.register_type(param));
            }
            Some(ast::AccessSource::Parameter) => {
                let base = scopes::get_function_parameter(&self.scopes)
                    .ok()
                    .map(|(ty, _)| ty)?;
                self.follow_accessors(base, &access.accessors)?
            }
            _ => return None,
        };
        let callable = Type::strip_annotations(callable, &*self.program);
        match self.program.lookup_type(callable)? {
            Type::Callable { parameter, .. } => Some(*parameter),
            _ => None,
        }
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

    fn extract_receive_type(&mut self, body: Option<&ast::Expression>) -> Result<usize, Error> {
        let mut receive_types = Vec::new();
        if let Some(body) = body {
            self.collect_receive_types(body, &mut receive_types)?;
        }

        Ok(self.unify_receive_types(receive_types))
    }

    fn extract_receive_type_from_statements(
        &mut self,
        statements: &[ast::Statement],
    ) -> Result<usize, Error> {
        let mut receive_types = Vec::new();
        for statement in statements {
            if let ast::Statement::Expression(sequence) = statement {
                self.collect_receive_types_from_sequence(sequence, &mut receive_types)?;
            }
        }

        Ok(self.unify_receive_types(receive_types))
    }

    fn collect_receive_types(
        &mut self,
        expression: &ast::Expression,
        receive_types: &mut Vec<usize>,
    ) -> Result<(), Error> {
        for branch in &expression.branches {
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
        for chain in &sequence.chains {
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
                    if let ast::StrSegment::Hole(expression) = segment {
                        self.collect_receive_types(expression, receive_types)?;
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
            self.expand_dialects_in_expression(body)?;
        }

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
        let type_param_suffix = inherited_suffix.unwrap_or_else(|| self.program.type_count());
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
                // site when one is available and usable. A bare type variable (`'t`) is not usable
                // — there's nothing to pin it, so the literal couldn't act on its parameter — so
                // fall back to nil, preserving the `#{ ... }` nilary-function shorthand.
                let usable = expected_parameter
                    .filter(|&ep| !matches!(self.program.lookup_type(ep), Some(Type::Variable(_))));
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
        let mut function_scope_bindings = HashMap::new();
        for scope in &saved_scopes {
            for (name, binding) in &scope.bindings {
                if let Binding::TypeAlias(_) = binding {
                    function_scope_bindings.insert(name.clone(), binding.clone());
                }
            }
        }

        self.scopes = vec![Scope::new(function_scope_bindings, None, ScopeKind::Root)];
        self.codegen.instructions = Vec::new();
        self.local_count = 0;
        self.current_receive_type_id = receive_type;
        // Seed the states union with the parameter (the spawn init / bare-`^` argument);
        // tail calls widen it during body compilation.
        self.current_states = Some(parameter_type);

        // Define captures as first locals in function body scope
        for capture in &unique_captures {
            // Determine the type of the captured value
            // First check if the full path is already available (for nested captures)
            let capture_type = if let Some((full_type, _)) =
                scopes::lookup_variable(&saved_scopes, &capture.base, &capture.accessors)
            {
                // The full path is already available from parent, use its type
                full_type
            } else if capture.accessors.is_empty() {
                // Simple capture - use the base variable's type
                if let Some((var_type, _)) =
                    scopes::lookup_variable(&saved_scopes, &capture.base, &[])
                {
                    var_type
                } else {
                    continue;
                }
            } else {
                // Capture with accessors - need to compute the accessed type
                if let Some((var_type, _)) =
                    scopes::lookup_variable(&saved_scopes, &capture.base, &[])
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
                                    &capture.base,
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
                                    &capture.base,
                                ) {
                                    Ok(types) => types,
                                    _ => continue,
                                }
                            }
                            ast::AccessPath::Annotation(name, expected) => match expected {
                                None => {
                                    match annotations::retrieval_type(self.program, last_type, name)
                                    {
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
            };

            scopes::define_variable(
                &mut self.scopes,
                &mut self.local_count,
                &capture.base,
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
                let body_type = self.compile_scoped_expression(
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
            // First check if the full path is available (for nested captures)
            if let Some((_, full_index)) =
                scopes::lookup_variable(&self.scopes, &capture.base, &capture.accessors)
            {
                // The full path is already captured, just load it
                self.codegen.add_instruction(Instruction::Load(full_index));
            } else if let Some((var_type, base_index)) =
                scopes::lookup_variable(&self.scopes, &capture.base, &[])
            {
                // Load the base variable
                self.codegen.add_instruction(Instruction::Load(base_index));

                if !capture.accessors.is_empty() {
                    // Apply accessors to get the final value
                    self.compile_accessor(
                        var_type,
                        capture.accessors.clone(),
                        &capture.base,
                        Provenance::Unknown,
                    )?;
                }
            }
        }

        self.codegen
            .add_instruction(Instruction::Function(function_index));

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
    fn compile_match(
        &mut self,
        pattern: ast::Match,
        value_type: usize,
        value_provenance: Provenance,
        on_no_match: Option<usize>,
        return_ok: bool,
        mut narrowing: Option<&mut Narrowing>,
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

        // If result type is never (empty union), pattern won't match - skip pattern matching code
        if self.is_never(result_type) {
            self.codegen.add_instruction(Instruction::Pop);
            self.codegen.add_instruction(Instruction::Tuple(NIL));
            return Ok(self.program.register_type(Type::nil()));
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
                scope.bindings.insert(
                    variable_name.clone(),
                    Binding::Variable {
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
        if !self.is_never(narrowed_type) && !self.is_nil(narrowed_type) {
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

        // Success path: leave the value on the stack, or replace with Ok
        if return_ok {
            self.codegen.add_instruction(Instruction::Pop);
            self.codegen.add_instruction(Instruction::Tuple(OK));
        }
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
                self.codegen.add_instruction(Instruction::Tuple(NIL));
                self.codegen.add_instruction(Instruction::Store);
            }
        }
        // Failure path: pop the value and push nil
        self.codegen.add_instruction(Instruction::Pop);
        self.codegen.add_instruction(Instruction::Tuple(NIL));

        self.codegen.patch_jump_to_here(success_jump_addr);

        // Compute final type - if return_ok, replace the matched (success) type with Ok.
        // `result_type` is the matched portion, already widened with nil when the match can fail.
        // A `result_type` that is *exactly* nil has no success component: the match can never
        // succeed. When the value itself can't be nil this means the pattern is unsatisfiable, so
        // the term is statically dead (nil) — emitting `Ok | []` here would wrongly keep a dead
        // branch alive. (If the value can be nil the pattern matches that nil value, so it stays a
        // real success.)
        // Match verdicts are freshly minted Ok/nil values, so their rows are exact-empty:
        // provably annotation-free (a failed match's nil never carries an error payload).
        //
        // Nil in `result_type` is not by itself fallibility: a bare binder (`=x`) on a
        // nil-typed value *matches* the nil and binds it. A pattern is irrefutable when
        // some binding set has no runtime requirements — it types as plain `Ok`.
        let irrefutable = pattern::is_irrefutable(&binding_sets);
        let final_type = if return_ok {
            if self.is_nil(result_type) && !self.contains_nil(value_type) {
                result_type
            } else if self.contains_nil(result_type) && !irrefutable {
                let closed_ok = annotations::closed_ok(self.program);
                let closed_nil = annotations::closed_nil(self.program);
                typing::union_type_ids(self.program, vec![closed_ok, closed_nil])
            } else {
                annotations::closed_ok(self.program)
            }
        } else {
            result_type
        };

        Ok(final_type)
    }

    /// Compile an expression in its own scope: store the incoming value as the scope parameter,
    /// then evaluate each `|` branch (re-loading the parameter) until one yields non-nil. Used for
    /// braced blocks, function bodies, and multi-branch statement expressions.
    fn compile_scoped_expression(
        &mut self,
        mut expression: ast::Expression,
        parameter_type: usize,
        parameter_provenance: Provenance,
        on_no_match: Option<usize>,
        scope_kind: ScopeKind,
        is_function_body: bool,
    ) -> Result<usize, Error> {
        // A block's annotation prefix attaches to the block's *result*; it is compiled at the
        // convergence point below. (A function body's annotations attach to the closure and
        // are extracted by `compile_function` before it gets here.)
        let block_annotations = std::mem::take(&mut expression.annotations);

        // Debug builds: the site a block-exhaustion stamp points at — the first branch's
        // start, standing in for the block itself (blocks carry no span of their own).
        let block_site_span = expression
            .branches
            .first()
            .and_then(|branch| branch.condition.chains.first())
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
        self.codegen.add_instruction(Instruction::Store);

        // Push new scope with parameter
        self.scopes.push(Scope::new(
            HashMap::new(),
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

        for (i, branch) in expression.branches.iter().enumerate() {
            let is_last_branch = i == expression.branches.len() - 1;

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
                self.codegen.add_instruction(Instruction::Pop);
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

                // Re-apply the complement narrowings accumulated from ALL previous branches.
                // Applying every accumulated complement — not just the immediately preceding
                // branch's — lets per-element tuple narrowings from non-adjacent branches
                // survive: matching `=[Rational[..], _]` then `=[_, Rational[..]]` narrows
                // both elements to 'int in a final `=[a, b]` branch.
                for (prov, complement) in &accumulated_complements {
                    apply_narrowing(&mut self.scopes, prov, *complement, self.program);
                }
            }

            // Create a narrowing instance for this branch's condition
            let mut narrowing = Narrowing::new();

            // Compile the condition expression - it can use ~> to access the parameter
            // We need both the type and provenance for forward narrowing
            let (condition_type, condition_prov) = self.compile_sequence(
                branch.condition.clone(),
                on_no_match,
                None,
                Some(&mut narrowing),
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
                self.codegen.add_instruction(Instruction::Pop);

                // Consequence is a new chain that starts with the block's parameter value
                // (not the condition's result). Every chain implicitly starts with the
                // surrounding block's parameter. Pass None for input_type so the consequence
                // loads the parameter via implicit_continuation.
                let (consequence_type, _) = self.compile_sequence(
                    consequence.clone(),
                    None,
                    None, // No input - consequence loads parameter via implicit_continuation
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
                        .add_instruction(Instruction::Reset(param_local + 1));
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
                        .add_instruction(Instruction::Reset(param_local + 1));
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
                    self.codegen.add_instruction(Instruction::Duplicate);
                    let success_jump = self.codegen.emit_jump_if_placeholder();
                    end_jumps.push(success_jump);
                }
            }
        }

        // An annotation-only block (`{ :error X }`) has no branches: it is identity — yield
        // the parameter unchanged — plus the attach compiled at the convergence below.
        if expression.branches.is_empty() {
            self.codegen.add_instruction(Instruction::Load(param_local));
            branch_types.push(parameter_type);
            is_exhaustive = true;
        }

        // Reset to clear the parameter (branches have already reset their specific locals)
        // Save address for end_jumps patching
        let param_clear_addr = self.codegen.instructions.len();
        self.codegen
            .add_instruction(Instruction::Reset(locals_before));

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
                    .add_instruction(Instruction::Reset(param_local + 1));
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
                    .add_instruction(Instruction::Reset(locals_before));
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
        self.codegen.add_instruction(Instruction::Stamp(site));
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

    /// Emit a function call (`Instruction::Call`), wrapping it with debug-mode `:pre`/
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
            self.codegen.add_instruction(Instruction::Call);
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
            self.codegen.add_instruction(Instruction::Pick(1));
            self.codegen.add_instruction(Instruction::Pick(1));
            self.codegen
                .add_instruction(Instruction::GetAnnotation(post_key, None));
            self.codegen.add_instruction(Instruction::Rotate(4));
            self.codegen.add_instruction(Instruction::Rotate(4));
        }

        // Pre-check: apply the pre-contract to a copy of the argument, leaving the
        // `[arg, callable]` pair (on top) untouched for the real call.
        if has_pre {
            self.codegen.add_instruction(Instruction::Pick(1));
            self.codegen.add_instruction(Instruction::Pick(1));
            self.codegen
                .add_instruction(Instruction::GetAnnotation(pre_key, None));
            self.codegen.add_instruction(Instruction::Call);
            self.emit_contract_verdict("Precondition")?;
        }

        // The real call: `[.., arg, callable]` -> `[.., result]`.
        self.codegen.add_instruction(Instruction::Call);

        // Post-check: build `[in: arg_c, out: result]` from the stashed argument and the
        // result, apply the post-contract, then drop the two stashed values, leaving
        // `[result]`.
        if has_post {
            self.codegen.add_instruction(Instruction::Pick(2)); // arg_c
            self.codegen.add_instruction(Instruction::Pick(1)); // result
            self.codegen
                .add_instruction(Instruction::Tuple(in_out_tuple));
            self.codegen.add_instruction(Instruction::Pick(2)); // post_fn
            self.codegen.add_instruction(Instruction::Call);
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
        self.codegen.add_instruction(Instruction::Constant(index));
        let binary_type = self.program.register_type(Type::Binary);
        let str_tuple = self
            .program
            .register_tuple(Some("Str".to_string()), vec![(None, binary_type)]);
        self.codegen.add_instruction(Instruction::Tuple(str_tuple));
        self.compile_builtin("panic")?;
        self.codegen.add_instruction(Instruction::Call);
        Ok(())
    }

    /// Compile a sequence of `,`-separated chains, short-circuiting to nil if any yields nil.
    fn compile_sequence(
        &mut self,
        sequence: ast::Sequence,
        on_no_match: Option<usize>,
        input_type: Option<usize>,
        mut narrowing: Option<&mut Narrowing>,
    ) -> Result<(usize, Provenance), Error> {
        // The sequence's result/short-circuit type, accumulated across chains.
        let mut last_type = input_type;
        let mut last_prov = Provenance::Unknown;
        // The value threaded into the next chain: the previous chain's result, which is left on the
        // stack. `None` means there is no threaded value yet, so the first chain starts from the
        // block parameter (via `implicit_continuation`) — unless the caller supplied an
        // `input_type` (the value is then already on the stack).
        let mut threaded: Option<(usize, Provenance)> =
            input_type.map(|t| (t, Provenance::Unknown));
        let mut end_jumps = Vec::new();

        for (i, chain) in sequence.chains.iter().enumerate() {
            // A chain after the first threads from the previous chain's result (on the stack). That
            // value is non-nil — a nil result short-circuits to the end — so strip nil from its
            // type. The first chain has no threaded value and loads the block parameter instead.
            let chain_input = threaded.map(|(t, p)| (self.without_nil(t), p));

            let (chain_type, chain_prov) = self.compile_chain_with_input(
                chain.clone(),
                on_no_match,
                None,
                chain_input,
                narrowing.as_deref_mut(),
                true, // implicit_continuation: only consulted for the first chain (no threaded input)
                None,
            )?;

            // Debug builds: a nil step result is a failure — stamp it with this step's
            // provenance (a no-op for non-nil results and already-stamped nils). Steps
            // whose type excludes nil skip the instruction entirely.
            if self.debug && self.contains_nil(chain_type) {
                let kind = if chain.match_pattern.is_some()
                    || chain.terms.iter().any(|t| matches!(t, ast::Term::Match(_)))
                {
                    quiver_core::bytecode::SiteKind::NoMatch
                } else {
                    quiver_core::bytecode::SiteKind::NilResult
                };
                self.emit_stamp(chain.span.get(), kind);
            }

            // If a prior chain could short-circuit to nil, the sequence's result includes
            // that nil — the *same* value, so its typed nil members (annotation rows
            // preserved) carry over rather than a fresh bare nil.
            let should_propagate_nil =
                i > 0 && last_type.as_ref().is_some_and(|&t| self.contains_nil(t));
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

            // Thread this chain's result into the next chain.
            threaded = Some((chain_type, chain_prov));

            // If last_type is NIL, subsequent chains are unreachable - break early
            if let Some(last_type_id) = last_type
                && self.is_nil(last_type_id)
            {
                break;
            }

            if i < sequence.chains.len() - 1 {
                // Keep the result on the stack for the next chain (threading); short-circuit to the
                // end of the sequence if it is nil.
                let end_jump = self.codegen.emit_duplicate_jump_if_nil();
                end_jumps.push(end_jump);

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
                let result_is_tracked_value = chain.match_pattern.is_none()
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
        // Tuple fields don't have implicit continuation - they start with no input
        self.compile_chain_with_input(chain, on_no_match, ripple_context, None, None, false, None)
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
            self.codegen.add_instruction(Instruction::Load(param_local));
            (Some(parameter_type), Provenance::Parameter)
        } else {
            // No initial input (tuple field chains use ripple_context for ~)
            (None, Provenance::Unknown)
        };

        let terms: Vec<_> = chain.terms.into_iter().collect();
        let last_index = terms.len().saturating_sub(1);
        for (i, term) in terms.iter().enumerate() {
            // Only the chain's final term produces the chain's value, so only it receives the
            // chain's expected type. `#{…}` parameter inference is Apply-site only: a literal
            // in a call's argument infers from the callee (`f [.., #{…}]`), never from a
            // downstream chain term.
            let term_expected = if i == last_index { expected } else { None };
            let (term_type, term_prov) = self.compile_term(
                term.clone(),
                FlowingValue {
                    ty: current_type,
                    provenance: current_prov,
                },
                on_no_match,
                ripple_context,
                narrowing.as_deref_mut(),
                term_expected,
            )?;
            // Nil flows through a chain like any other value: within a chain, no term
            // short-circuits on nil (a failed mid-chain match yields nil that flows into
            // the next term; the only short-circuit is between `,`-separated chains, handled
            // in `compile_sequence`). So a term's full type — nil included — flows onward, and
            // a subsequent term that cannot accept nil is a genuine type error.
            current_type = Some(term_type);
            current_prov = term_prov;
        }

        let result_type = current_type.ok_or_else(|| Error::InternalError {
            message: "Chain compiled with no terms and no continuation".to_string(),
        })?;

        // If there's a match pattern, apply it. Binding definitions for go-to-definition
        // and hover are recorded inside `compile_match`, which sees every binding site
        // (top-level `name = ...`, destructuring, mid-chain `=x`, and block branches).
        if let Some(pattern) = chain.match_pattern {
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
                true, // Direct assignment returns Ok
                narrowing,
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

        // Check for circular imports
        if self.module_cache.import_stack.contains(&id) {
            return Err(Error::FeatureUnsupported(
                "Circular import detected".to_string(),
            ));
        }

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
            cached
        } else {
            self.module_cache.import_stack.push(id.clone());
            let cached = self.import_and_cache_module(&resolved);
            self.module_cache.import_stack.pop();
            cached?
        };

        // Resolve accessor chain on the cached value
        let module_type_id = self.program.register_type(cached.module_type.clone());
        let (resolved_value, resolved_type) = self.resolve_accessors(
            &cached.value,
            &module_type_id,
            accessors,
            &module_name,
            &cached.binary_data,
        )?;

        Ok((cached, resolved_value, resolved_type, origin))
    }

    /// Compile an import with optional accessor chain.
    /// Resolves accessors statically on the cached module value, emitting only
    /// instructions needed for the resolved value.
    fn compile_import(
        &mut self,
        module: &[String],
        accessors: &[ast::AccessPath],
    ) -> Result<(usize, ModuleOrigin), Error> {
        let (cached, resolved_value, resolved_type, origin) =
            self.resolve_import(module, accessors)?;

        // Emit instructions for just the resolved value
        let (instructions, _) =
            self.value_to_instructions_from_cache(&resolved_value, &cached.binary_data)?;

        for instruction in instructions {
            self.codegen.add_instruction(instruction);
        }

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

        // Save current compiler state
        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_local_count = self.local_count;
        // Resolve this module's own imports against its package, not the importer's.
        let saved_package = std::mem::replace(&mut self.current_package, resolved.package.clone());
        // Provenance sites inside the module name it, not the importing unit.
        let saved_module = std::mem::replace(&mut self.current_module, module_name.clone());
        // NOTE: `type_param_suffix` is deliberately *not* cleared here, although that means a
        // module first imported from inside a function body compiles all its generics under
        // the importer's suffix (import-order-dependent sharing). Clearing it — so each of the
        // module's top-level definitions gets its own suffix, as a top-level import does — is
        // the principled fix, but it currently trips a latent bug in cross-definition generic
        // unification. Repro: `"." %fs.list [~, #'%fs.entry { .name }] %iter.map [~, " | "]
        // %str.join` fails to compile with a leaked `'u | Entry | Entry` element type when
        // %iter is first imported from inside fs.qv's `list` body. Fix that unification bug
        // before clearing the suffix here.
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

        // Reset to clean state for module compilation
        self.local_count = 0;

        // Check if module has expressions (needs a parameter scope for implicit continuation)
        let has_expressions = parsed
            .statements
            .iter()
            .any(|s| matches!(s, ast::Statement::Expression(_)));

        let scope_parameter = if has_expressions {
            let param_local = self.local_count;
            self.local_count += 1;
            self.codegen.add_instruction(Instruction::Store);
            let nil_type_id = self.program.register_type(Type::nil());
            Some(scopes::Parameter {
                ty: nil_type_id,
                index: param_local,
                provenance: Provenance::Parameter,
            })
        } else {
            None
        };

        self.scopes = vec![Scope::new(HashMap::new(), scope_parameter, ScopeKind::Root)];

        // Compile the module body as one threaded sequence; the final value is the module value.
        // On failure, restore the saved compiler state before propagating. This is not just
        // hygiene: module imports are also triggered by *speculative* probes (the Apply-site
        // inference's `callee_parameter_type`), whose callers swallow errors and continue — leaving
        // the module's scopes/instructions in place would have the enclosing program compile
        // against the failed module's scope chain (cascading `VariableUndefined`s), and a zeroed
        // `function_depth` panics with a usize underflow in the enclosing `compile_function`,
        // masking the real error.
        let result_type_id = match self.compile_top_level(parsed.statements) {
            Ok(t) => t,
            Err(e) => {
                self.codegen.instructions = saved_instructions;
                self.scopes = saved_scopes;
                self.local_count = saved_local_count;
                self.recorder = saved_recorder;
                self.current_package = saved_package;
                self.current_module = saved_module;
                self.function_depth = saved_function_depth;
                return Err(e);
            }
        };
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
        let (module_value, executor) =
            // Modules are executed at compile time only to produce their value; they don't
            // receive messages, so skip the (expensive) parameter-compatibility tables.
            quiver_core::execute_bytecode_sync_with(bytecode, self.builtins, false, false).map_err(
                |e| Error::ModuleExecution {
                    module: module_name.clone(),
                    error: Box::new(e),
                },
            )?;

        // Extract binary data from the executor
        let mut binary_data = HashMap::new();
        modules::extract_binary_data(&module_value, &executor, &mut binary_data);

        // Capture the dispatch-table entries this module added (new or changed since the
        // snapshot), so a later cache hit can restore them without recompiling the module.
        let fn_case_tables: HashMap<usize, Vec<(usize, usize)>> = self
            .fn_case_tables
            .iter()
            .filter(|(k, v)| dispatch_fn_before.get(*k) != Some(*v))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        let case_tables: HashMap<usize, usize> = self
            .case_tables
            .iter()
            .filter(|(k, v)| dispatch_case_before.get(*k) != Some(*v))
            .map(|(k, v)| (*k, *v))
            .collect();

        let cached = modules::CachedModule {
            value: module_value,
            module_type,
            binary_data,
            fn_case_tables,
            case_tables,
        };

        // Cache the module
        self.module_cache
            .cache_module(resolved.id.clone(), cached.clone());

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

        let (cached, module_value, _module_type, _origin) =
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
            self.value_to_instructions_from_cache(&function, &cached.binary_data)?;
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
            Instruction::Constant(constant),
            Instruction::Tuple(str_tuple),
            Instruction::Builtin(term_builtin),
            Instruction::Builtin(chain_builtin),
            Instruction::Tuple(context_tuple),
        ];
        instructions.extend(function_instructions);
        instructions.push(Instruction::Call);
        bytecode.functions.push(quiver_core::bytecode::Function {
            instructions,
            captures: 0,
            type_id: entry_type,
        });
        bytecode.entry = Some(bytecode.functions.len() - 1);

        let (expr_value, executor) = quiver_core::execute_bytecode_sync_with(
            bytecode, &registry, false, false,
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
                Binary::Heap(index) => executor.get_heap_binary(*index).map(|data| data.to_vec()),
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
    fn expand_dialects_in_expression(
        &mut self,
        expression: &mut ast::Expression,
    ) -> Result<(), Error> {
        for annotation in &mut expression.annotations {
            self.expand_dialects_in_chain(&mut annotation.value)?;
        }
        for branch in &mut expression.branches {
            self.expand_dialects_in_sequence(&mut branch.condition)?;
            if let Some(consequence) = &mut branch.consequence {
                self.expand_dialects_in_sequence(consequence)?;
            }
        }
        Ok(())
    }

    fn expand_dialects_in_sequence(&mut self, sequence: &mut ast::Sequence) -> Result<(), Error> {
        for chain in &mut sequence.chains {
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
                    if let ast::StrSegment::Hole(expression) = segment {
                        self.expand_dialects_in_expression(expression)?;
                    }
                }
            }
            ast::Term::Block(expression) => self.expand_dialects_in_expression(expression)?,
            ast::Term::Function(function) => {
                if let Some(body) = &mut function.body {
                    self.expand_dialects_in_expression(body)?;
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
        executor: &quiver_core::executor::Executor<E>,
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
                    Binary::Heap(index) => {
                        executor.get_heap_binary(*index).map(|data| data.to_vec())
                    }
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

    /// Append instructions reconstructing a payload's annotations onto the value the
    /// preceding instructions left on the stack (cache-flavoured counterpart of
    /// `Program::annotations_to_instructions`).
    fn annotations_to_instructions_from_cache(
        &mut self,
        instructions: &mut Vec<Instruction>,
        payload: &quiver_core::value::Payload,
        binary_data: &HashMap<usize, Vec<u8>>,
    ) -> Result<(), Error> {
        for (key, value) in payload.annotations() {
            let (value_instructions, _) =
                self.value_to_instructions_from_cache(value, binary_data)?;
            instructions.extend(value_instructions);
            instructions.push(Instruction::Annotate(*key));
        }
        Ok(())
    }

    /// Convert a cached runtime value back to instructions that reconstruct it.
    /// Uses pre-extracted binary data instead of an executor.
    fn value_to_instructions_from_cache(
        &mut self,
        value: &Value,
        binary_data: &HashMap<usize, Vec<u8>>,
    ) -> Result<(Vec<Instruction>, usize), Error> {
        match value {
            Value::Int(int_value) => {
                let index = self
                    .program
                    .register_constant(Constant::Integer((*int_value).into()));
                Ok((
                    vec![Instruction::Constant(index)],
                    self.program.register_type(Type::Integer),
                ))
            }
            Value::BigInt(int_value) => {
                let index = self
                    .program
                    .register_constant(Constant::Integer((**int_value).clone()));
                Ok((
                    vec![Instruction::Constant(index)],
                    self.program.register_type(Type::Integer),
                ))
            }
            Value::Binary(binary) => match binary {
                Binary::Constant(const_idx) => {
                    // Just use the existing constant
                    Ok((
                        vec![Instruction::Constant(*const_idx)],
                        self.program.register_type(Type::Binary),
                    ))
                }
                Binary::Heap(heap_idx) => {
                    // Use pre-extracted binary data from cache
                    let bytes = binary_data
                        .get(heap_idx)
                        .ok_or_else(|| {
                            Error::FeatureUnsupported(format!(
                                "Missing cached binary data for heap index {}",
                                heap_idx
                            ))
                        })?
                        .clone();
                    let constant = Constant::Binary(bytes);
                    let index = self.program.register_constant(constant);
                    Ok((
                        vec![Instruction::Constant(index)],
                        self.program.register_type(Type::Binary),
                    ))
                }
            },
            Value::Tuple(tuple_id, fields) => {
                let mut instructions = Vec::new();
                for field in fields.iter() {
                    let (field_instructions, _) =
                        self.value_to_instructions_from_cache(field, binary_data)?;
                    instructions.extend(field_instructions);
                }
                instructions.push(Instruction::Tuple(*tuple_id));
                self.annotations_to_instructions_from_cache(
                    &mut instructions,
                    fields,
                    binary_data,
                )?;
                Ok((
                    instructions,
                    self.program.register_type(Type::Tuple(*tuple_id)),
                ))
            }
            Value::Function(function, captures) => {
                // Get the function's callable type directly from the function's type_id
                let callable_type_id = self
                    .program
                    .get_function(*function)
                    .ok_or(Error::FunctionUndefined(*function))?
                    .type_id;

                let mut instructions = Vec::new();

                // Push capture values to stack (will be popped by Function instruction)
                for capture_value in captures.iter() {
                    let (capture_instructions, _) =
                        self.value_to_instructions_from_cache(capture_value, binary_data)?;
                    instructions.extend(capture_instructions);
                }

                // Reuse the same function index - no re-registration needed!
                instructions.push(Instruction::Function(*function));
                self.annotations_to_instructions_from_cache(
                    &mut instructions,
                    captures,
                    binary_data,
                )?;

                Ok((instructions, callable_type_id))
            }
            Value::Builtin(builtin_id, payload) => {
                // Get the builtin info to retrieve its type signature
                let builtin_info = self
                    .program
                    .get_builtins()
                    .get(*builtin_id)
                    .ok_or_else(|| Error::BuiltinUndefined(format!("builtin_id {}", builtin_id)))?;
                let param_type = builtin_info.param_type;
                let result_type = builtin_info.result_type;

                let never_id = self.program.never();
                let callable_type_id = self.program.register_type(Type::Callable {
                    parameter: param_type,
                    result: result_type,
                    receive: never_id,
                    // Builtins never tail-call: their states are their parameter.
                    states: Some(param_type),
                });

                let mut instructions = vec![Instruction::Builtin(*builtin_id)];
                if let Some(payload) = payload {
                    self.annotations_to_instructions_from_cache(
                        &mut instructions,
                        payload,
                        binary_data,
                    )?;
                }

                Ok((instructions, callable_type_id))
            }
            Value::Process(_, _) => Err(Error::FeatureUnsupported(
                "Cannot use process in constant context".to_string(),
            )),
            Value::Resource(..) => Err(Error::FeatureUnsupported(
                "Cannot use resource in constant context".to_string(),
            )),
            Value::Reference(_) => Err(Error::FeatureUnsupported(
                "Cannot use ref in constant context".to_string(),
            )),
        }
    }

    /// Resolve an accessor chain on a compile-time known value.
    /// Returns the resolved value and its type.
    fn resolve_accessors(
        &mut self,
        value: &Value,
        value_type: &usize,
        accessors: &[ast::AccessPath],
        module_name: &str,
        binary_data: &HashMap<usize, Vec<u8>>,
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
                            let (_, entry_type) =
                                self.value_to_instructions_from_cache(&entry, binary_data)?;
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
                self.codegen.add_instruction(Instruction::Rotate(2));
                self.codegen.add_instruction(Instruction::Pop);
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
        self.codegen.add_instruction(Instruction::Tuple(NIL));
        self.codegen.add_instruction(Instruction::Rotate(2));
        self.codegen.add_instruction(Instruction::Spawn);

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

        self.codegen.add_instruction(Instruction::Spawn);

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
            Some(ast::AccessSource::Parameter) => {
                let Ok((ty, _)) = scopes::get_function_parameter(&self.scopes) else {
                    return;
                };
                self.record_typed(base_span, ty, SymbolKind::Parameter, Some("$".to_string()));
                (ty, "$".to_string())
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

    fn compile_access_inner(
        &mut self,
        access: ast::Access,
        value_type: Option<usize>,
        value_provenance: Provenance,
        ripple_context: Option<&RippleContext>,
        implicit_flow: bool,
    ) -> Result<(usize, Provenance), Error> {
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
                Ok((accessed_type, accessed_prov))
            }
            Some(ast::AccessSource::Parameter) => {
                // $ accesses the function parameter.
                let (param_type, param_local) = scopes::get_function_parameter(&self.scopes)?;

                // Peek at the accessed type to determine if callable (without emitting code).
                let peeked_type = self.peek_accessor_type(param_type, &access.accessors, "$");
                let is_callable = peeked_type.is_ok_and(|ty| {
                    matches!(
                        self.program.lookup_base(ty),
                        Some(Type::Callable { .. }) | Some(Type::Process { .. })
                    )
                });

                // Non-callable accessed with a flowing value: drop the value before loading.
                if !is_callable && value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }
                self.codegen.add_instruction(Instruction::Load(param_local));
                let (accessed_type, accessed_prov) = self.compile_accessor(
                    param_type,
                    access.accessors,
                    "$",
                    Provenance::Parameter,
                )?;

                if let (true, Some(val_type)) = (is_callable, value_type) {
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
                let is_callable = peeked_type.is_some_and(|ty| {
                    matches!(
                        self.program.lookup_base(ty),
                        Some(Type::Callable { .. }) | Some(Type::Process { .. })
                    )
                });

                // Non-callable accessed with a flowing value: drop the value before loading.
                if !is_callable && value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }
                let (accessed_type, accessed_prov) =
                    self.compile_member_access(&name, access.accessors)?;

                if let (true, Some(val_type)) = (is_callable, value_type) {
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
                        Ok((val_type, value_provenance))
                    } else if let Some(ctx) = ripple_context {
                        // Inherit the ripple context from the enclosing tuple.
                        self.codegen
                            .add_instruction(Instruction::Pick(ctx.stack_offset));
                        Ok((ctx.value_type_id, ctx.provenance.clone()))
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
                    Ok((accessed_type, accessed_prov))
                }
            }
            Some(ast::AccessSource::Import(module)) => {
                // Resolve the import type first (no code emission yet) to check if callable. Hover
                // / go-to-definition entries are recorded per component by `record_access_components`.
                let (cached, resolved_value, accessed_type, _origin) =
                    self.resolve_import(&module, &access.accessors)?;

                let is_callable = matches!(
                    self.program.lookup_base(accessed_type),
                    Some(Type::Callable { .. }) | Some(Type::Process { .. })
                );

                // Non-callable accessed with a flowing value: drop the value before loading.
                if !is_callable && value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }
                let (instructions, _) =
                    self.value_to_instructions_from_cache(&resolved_value, &cached.binary_data)?;
                for instruction in instructions {
                    self.codegen.add_instruction(instruction);
                }

                if let (true, Some(val_type)) = (is_callable, value_type) {
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
                let builtin_type = self.compile_builtin(&name)?;
                // A builtin has no fields, so accessors (`__x__.field`) fail here as a non-tuple.
                let (callable_type, _) = self.compile_accessor(
                    builtin_type,
                    access.accessors,
                    "__builtin__",
                    Provenance::Unknown,
                )?;

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
                self.codegen.add_instruction(Instruction::Select);
                return self.compute_select_return_type(&[val_type]);
            }
            Some(sources) if sources.is_empty() => {
                // Explicit `![]` - discard chained value, return nil
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }
                self.codegen.add_instruction(Instruction::Tuple(NIL));
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
            self.codegen.add_instruction(Instruction::Tuple(tuple_id));
        }

        // Emit Select instruction (handles both single value and tuple)
        self.codegen.add_instruction(Instruction::Select);

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
                    // Process (process or resource) - get its receive type
                    result_types.push(*recv_type);
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
                    // Timeout source
                    result_types.push(self.program.register_type(Type::nil()));
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
            self.codegen.add_instruction(Instruction::Constant(empty));
        }
        for (i, segment) in segments.into_iter().enumerate() {
            match segment {
                ast::StrSegment::Text(bytes) => {
                    let index = self.program.register_constant(Constant::Binary(bytes));
                    self.codegen.add_instruction(Instruction::Constant(index));
                }
                ast::StrSegment::Hole(expression) => {
                    let (param_type, param_provenance) = match value_type {
                        // Duplicate the flowing value as the hole's input. It sits beneath the
                        // accumulated binary — nothing before the first segment, one value after —
                        // so its offset from the top is 0 for the first segment and 1 thereafter.
                        Some(vt) => {
                            self.codegen
                                .add_instruction(Instruction::Pick(usize::from(i > 0)));
                            (vt, value_provenance.clone())
                        }
                        None => {
                            self.codegen.add_instruction(Instruction::Tuple(NIL));
                            (self.program.register_type(Type::nil()), Provenance::Unknown)
                        }
                    };
                    let hole_type = self.compile_scoped_expression(
                        expression,
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
                    self.codegen.add_instruction(Instruction::GetPositional(0));
                }
            }
            // Fold left: once a second binary is on the stack, concatenate it onto the accumulator.
            if i > 0 {
                self.codegen.add_instruction(Instruction::Tuple(pair_tuple));
                // Push and apply the concat builtin, exactly as a `[a, b] __binary_concat__` call.
                self.compile_builtin("binary_concat")?;
                self.codegen.add_instruction(Instruction::Call);
            }
        }
        // Wrap the accumulated binary as `Str`.
        self.codegen.add_instruction(Instruction::Tuple(str_tuple));
        // Discard the flowing value kept beneath the result for `~` holes.
        if value_type.is_some() {
            self.codegen.add_instruction(Instruction::Rotate(2));
            self.codegen.add_instruction(Instruction::Pop);
        }
        Ok((str_type, Provenance::Unknown))
    }

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
                    self.codegen.add_instruction(Instruction::Pop);
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
                    self.codegen.add_instruction(Instruction::Tuple(NIL));
                }
                let ty = self.compile_scoped_expression(
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
                    self.codegen.add_instruction(Instruction::Pop);
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
                // problem is almost always the missed inference: point at the Apply-site rule.
                let inference_fell_back = func.parameter_type.is_none()
                    && expected_parameter.is_none()
                    && func
                        .body
                        .as_ref()
                        .is_some_and(expression_references_parameter);
                let function_type =
                    self.compile_function(func, expected_parameter)
                        .map_err(|e| {
                            if inference_fell_back {
                                Error::Noted {
                                    error: Box::new(e),
                                    note: "this `#{…}` literal's parameter defaulted to nil — \
                                       parameter inference is Apply-site only, so write the \
                                       call callee-first (`f […, #{…}]`) or annotate the \
                                       parameter"
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
                let ty = self.compile_match(
                    pattern,
                    val_type,
                    value_provenance.clone(),
                    on_no_match,
                    true,
                    narrowing,
                )?;
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
                        )?;
                        self.codegen.add_instruction(Instruction::Rotate(2));
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
                )?;
                // The callable is below the argument on the stack; swap so the call sees it on
                // top. An explicit argument is type-checked, not an implicit flow.
                self.codegen.add_instruction(Instruction::Rotate(2));
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
                self.codegen.add_instruction(Instruction::Self_);
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

                // Generate Process instruction
                self.codegen
                    .add_instruction(Instruction::Process(process_id, function_index));

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
                )
            }
            ast::Term::State(access, span) => {
                // `?p` — sample a process's state (docs/process-state.md). The result is
                // the target process type's state component: inferred at spawn sites, or
                // stated with a `?'s` clause on a declared process type. No runtime test —
                // soundness rests on every state write being compile-checked, plus the
                // strict state subtyping at declared boundaries. The flowing value is
                // unused: the target names the process explicitly.
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
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

                self.codegen.add_instruction(Instruction::State);

                self.record_typed(span.get(), result, SymbolKind::Expression, None);
                Ok((result, Provenance::Unknown))
            }
            ast::Term::Reference(access) => {
                // Explicit reference: drop incoming value and load the referenced value without calling
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }

                // The reference's span (`foo` in `&foo`, `%num.add` in `&%num.add`), for
                // hover and go-to-definition on the referenced symbol.
                let ref_span = access.span.get();

                // Load the referenced value
                match access.source {
                    Some(ast::AccessSource::Identifier(ref name)) => {
                        let label = accessors_label(name, &access.accessors);
                        let (accessed_type, accessed_prov) =
                            self.compile_member_access(name, access.accessors)?;
                        self.record_reference(ref_span, name, label, accessed_type);
                        Ok((accessed_type, accessed_prov))
                    }
                    Some(ast::AccessSource::Parameter) => {
                        // &$ - reference to function parameter
                        let (param_type, param_local) =
                            scopes::get_function_parameter(&self.scopes)?;
                        self.codegen.add_instruction(Instruction::Load(param_local));
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
                    Some(ast::AccessSource::Import(ref module)) => {
                        let label =
                            accessors_label(&format!("%{}", module.join("/")), &access.accessors);
                        let (ty, origin) = self.compile_import(module, &access.accessors)?;
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
                        self.codegen.add_instruction(Instruction::Self_);
                        let self_type = self.program.register_type(Type::Process {
                            send: Some(self.current_receive_type_id),
                            receive: None,
                            state: None,
                        });
                        Ok((self_type, Provenance::Unknown))
                    }
                    Some(ast::AccessSource::Builtin(ref name)) => {
                        // &__builtin__ - the builtin function value, without applying it.
                        let builtin_type = self.compile_builtin(name)?;
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
                }
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

                // Substitute bindings in the result type
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
                self.codegen.add_instruction(Instruction::Rotate(2));
                self.codegen.add_instruction(Instruction::Pop);
                self.codegen.add_instruction(Instruction::Tuple(NIL));
                self.codegen.add_instruction(Instruction::Rotate(2));
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
            self.codegen.add_instruction(Instruction::Send);

            Ok(target_type_id)
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
                    .add_instruction(Instruction::Tuple(nil_tuple_id));
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
            self.codegen.add_instruction(Instruction::TailCall(true));
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
                        self.codegen.add_instruction(Instruction::Load(index));
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
            self.codegen.add_instruction(Instruction::TailCall(false));
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
                    .add_instruction(Instruction::Tuple(nil_tuple_id));
            }
        }

        // Stack: [function, argument] -> [argument, function], as the tail call expects.
        self.codegen.add_instruction(Instruction::Rotate(2));
        self.codegen.add_instruction(Instruction::TailCall(false));
        Ok(result)
    }

    fn compile_builtin(&mut self, name: &str) -> Result<usize, Error> {
        let (param_type, result_type) = self
            .builtins
            .resolve_signature(name, self.program)
            .ok_or_else(|| Error::BuiltinUndefined(name.to_string()))?;

        // Register the types
        let param_type_id = self.program.register_type(param_type);
        let result_type_id = self.program.register_type(result_type);

        let builtin_index = self
            .program
            .register_builtin(name.to_string(), self.builtins);

        self.codegen
            .add_instruction(Instruction::Builtin(builtin_index));

        let never_id = self.program.never();
        Ok(self.program.register_type(Type::Callable {
            parameter: param_type_id,
            result: result_type_id,
            receive: never_id,
            // Builtins never tail-call: their states are their parameter.
            states: Some(param_type_id),
        }))
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
                    .add_instruction(Instruction::GetAnnotation(key_id, check));
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
                type_queries::FieldAccess::Position(index) => Instruction::GetPositional(index),
                type_queries::FieldAccess::Named { name, .. } => Instruction::GetNamed(name),
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
                .add_instruction(Instruction::Load(capture_index));
            // Captures lose provenance tracking
            return Ok((capture_type, Provenance::Unknown));
        }

        // No pre-evaluated capture, use standard member access
        let (last_type, index) = scopes::lookup_variable(&self.scopes, target, &[])
            .ok_or(Error::VariableUndefined(target.to_string()))?;
        self.codegen.add_instruction(Instruction::Load(index));

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
