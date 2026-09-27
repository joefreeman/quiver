//! Compiling an entry program: the top level, followed by a call of the function it
//! evaluates to. Shared by `quiv run`'s client, `quiv compile`, and `quiv test`'s
//! program blocks.

use quiver_compiler::compiler::{LocatedError, ModuleCache};
use quiver_compiler::{Compiler, ModuleResolver};
use quiver_core::bytecode::Instruction;
use quiver_core::program::Program;
use quiver_core::types::{Type, TypeLookup};
use std::collections::HashMap;

/// Why an entry program didn't compile.
#[derive(Debug)]
pub enum EntryError {
    Compile(LocatedError),
    /// The program compiled, but doesn't evaluate to a function to run.
    NotExecutable,
}

impl std::fmt::Display for EntryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EntryError::Compile(error) => write!(f, "{error}"),
            EntryError::NotExecutable => {
                write!(f, "Program is not executable. Must evaluate to a function.")
            }
        }
    }
}

impl std::error::Error for EntryError {}

/// Compile source into a Program and a nilary entry function: the program's top level,
/// followed by a call of the function it evaluates to. The top level thus runs at boot, in
/// the root process — compilation never executes user code.
///
/// The `ModuleCache` comes back with the program: unit extraction needs it to tell the
/// compile's own functions from the modules it imported, and to name a version of each.
pub fn compile_entry(
    ast: quiver_compiler::ast::Sequence,
    resolver: &dyn ModuleResolver,
    builtins: &quiver_core::builtins::BuiltinRegistry<quiver_io::NativeEffect>,
    options: quiver_compiler::compiler::CompileOptions,
    artifact_store: Option<std::rc::Rc<quiver_compiler::ArtifactStore>>,
) -> Result<(Program, ModuleCache, usize), EntryError> {
    let mut program = Program::new();
    let mut module_cache = ModuleCache::new();
    module_cache.artifact_store = artifact_store;
    // The top level is a sequence, so each step starts from a block value: nil for a
    // program. This must be a *type* id — `types::NIL` is the nil tuple's id, and passing
    // it types the top-level parameter as whatever type happens to land at id 0.
    let nil_type_id = program.register_type(Type::nil());
    let compilation_result = Compiler::compile(
        ast,
        &quiver_compiler::compiler::Bindings::default(),
        Default::default(),
        &mut module_cache,
        resolver,
        &mut program,
        nil_type_id, // parameter_type_id
        &HashMap::new(),
        builtins,
        None, // no semantic recorder for the CLI
        options,
    )
    .map_err(EntryError::Compile)?;

    let instructions = compilation_result.instructions;
    let receive_type = compilation_result.receive_type;

    // The program must evaluate to a function; check the type rather than running it.
    let Some((call_result, call_receive)) =
        resolve_program_callable(&program, compilation_result.result_type)
    else {
        return Err(EntryError::NotExecutable);
    };

    // The entry runs the top level — leaving the program's function on the stack — then
    // calls it with nil. Top-level bindings live in the entry frame below the call, so
    // they persist for the program's lifetime, exactly as bindings do in any scope.
    //
    // Note the call pushes a frame: the program function runs at depth 2, so its tail
    // calls no longer update the root process's observable state (`record_state` fires
    // only in the root frame). Currently unobservable — `&.` in the entry types as a
    // state-less process, so nothing can sample it — but if root-state sampling ever
    // matters here, the call must become a tail call instead.
    let mut entry_instructions = instructions;
    // A fallible top level short-circuits with nil (carrying its debug `:origin` stamp).
    // Guard the call so that nil becomes the program's *result* — reported like any
    // other failure, stamp intact — instead of an opaque call-on-nil type error.
    let fallible = type_contains_nil(&program, compilation_result.result_type);
    if fallible {
        entry_instructions.push(Instruction::pick(0));
        entry_instructions.push(Instruction::jump_unless(3));
    }
    entry_instructions.push(Instruction::tuple(quiver_core::types::NIL));
    entry_instructions.push(Instruction::rotate(2));
    entry_instructions.push(Instruction::call());

    // Both the top level and the program's function execute in the root process, so the
    // entry's receive covers both.
    let receive = union_types(&mut program, vec![receive_type, call_receive]);

    let result = if fallible {
        union_types(&mut program, vec![call_result, nil_type_id])
    } else {
        call_result
    };
    let callable_type_id = program.register_type(Type::Callable {
        parameter: nil_type_id,
        result,
        receive,
        // The entry is never spawned; grant nothing.
        states: None,
        omittable: Vec::new(),
    });

    let entry = program.register_function(quiver_core::bytecode::Function {
        instructions: entry_instructions,
        captures: 0,
        type_id: callable_type_id,
    });

    Ok((program, module_cache, entry))
}

/// The callable the program's top level evaluates to: its (result, receive) type ids,
/// looking through annotation rows and unions (a fallible top level unions with nil —
/// the entry guards that case and yields the nil as the program's result). A union of
/// several callables takes the FIRST member found: the entry's declared type is
/// informational (nothing narrows against it), but note the receive may under-declare
/// the other members'.
fn resolve_program_callable(program: &Program, type_id: usize) -> Option<(usize, usize)> {
    match program.lookup_type(type_id)? {
        Type::Callable {
            result, receive, ..
        } => Some((*result, *receive)),
        Type::Annotated { base, .. } => resolve_program_callable(program, *base),
        Type::Union(members) => members
            .clone()
            .into_iter()
            .find_map(|member| resolve_program_callable(program, member)),
        _ => None,
    }
}

/// Whether the type can be nil: nil itself, or a union with a nil member (looking
/// through annotation rows) — the fallible-top-level test for the entry's nil guard.
fn type_contains_nil(program: &Program, type_id: usize) -> bool {
    match program.lookup_type(type_id) {
        Some(Type::Tuple(id)) => *id == quiver_core::types::NIL,
        Some(Type::Annotated { base, .. }) => type_contains_nil(program, *base),
        Some(Type::Union(members)) => members
            .clone()
            .iter()
            .any(|member| type_contains_nil(program, *member)),
        _ => false,
    }
}

/// Union of type ids for the entry's declared type, flattening one level of nesting,
/// deduping, and dropping `never` (the empty union) — enough normalization for a type
/// nothing narrows against (the compiler's full normalizer is module-private).
fn union_types(program: &mut Program, ids: Vec<usize>) -> usize {
    let mut members: Vec<usize> = Vec::new();
    for id in ids {
        match program.lookup_type(id) {
            Some(Type::Union(inner)) => members.extend(inner.clone()),
            _ => members.push(id),
        }
    }
    members.dedup();
    let mut seen = std::collections::HashSet::new();
    members.retain(|id| seen.insert(*id));
    match members.len() {
        0 => program.never(),
        1 => members[0],
        _ => program.register_type(Type::Union(members)),
    }
}
