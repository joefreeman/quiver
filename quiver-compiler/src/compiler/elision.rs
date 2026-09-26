//! Wrapper elision: a call to a *trivial forwarder* — a function whose whole body
//! hands its parameter's fields (plus its own constants) to one builtin — compiles as
//! the builtin call itself: no callee push, no frame, no wrapper locals.
//!
//! Eligibility is decided by abstractly interpreting the function's compiled body over
//! a symbolic stack, not by matching instruction sequences, so codegen's incidental
//! stack traffic (flow-value duplication, pick/pop pairs) is irrelevant. Any
//! instruction outside the tracked repertoire — control flow above all — makes the
//! function ineligible, which is what keeps this the safe, closed subset of inlining:
//! a body with no jumps has no local remapping, label fixups, or capture
//! materialization to get wrong.
//!
//! The summary is expressed entirely in the linked program's own table ids (the
//! builtin row, the argument tuple, the wrapper's constants), so an elided call site
//! emits ordinary instructions that the artifact extraction and id-remapping passes
//! handle like any others.

use quiver_core::bytecode::Opcode;
use quiver_core::program::Program;
use quiver_core::types::TypeLookup;

/// Where one field of the forwarded builtin's argument comes from.
#[derive(Clone, Debug, PartialEq)]
pub enum ArgSource {
    /// The wrapper's whole parameter.
    Parameter,
    /// A positional field of the parameter.
    ParameterField(usize),
    /// One of the wrapper's own constants (`get_byte`'s `0` and `8`).
    Constant(usize),
}

/// The builtin's argument, as the wrapper builds it.
#[derive(Clone, Debug, PartialEq)]
pub enum Argument {
    /// The parameter passes through whole (`__data_encode__ $`).
    Parameter,
    /// The wrapper rebuilds a tuple from parameter fields and constants.
    Rebuild {
        tuple_id: usize,
        sources: Vec<ArgSource>,
    },
}

/// An elidable wrapper: `builtin(argument)`, optionally wrapped in result tuples
/// (`__data_encode__ $ ~> Str[~]` records `Str`'s id in `wraps`).
#[derive(Clone, Debug, PartialEq)]
pub struct Forwarder {
    pub builtin: usize,
    pub argument: Argument,
    /// Single-field tuple constructors applied to the result, innermost first.
    pub wraps: Vec<usize>,
}

/// Whether `sources` reads the parameter's fields as an exact in-order prefix run
/// (`f0, f1, …, f(k-1)`) followed only by constants — the shape argument fusion can
/// splice a caller's tuple build into. Answers the field count `k`.
///
/// All-constant sources answer `Some(0)`: a forwarder that ignores its parameter. The
/// caller's build is then spliced out only when it has *no* fields either — fusion
/// matches `k` against the built tuple's arity, so this fires against a literal nil
/// argument and nothing else, and the argument is dropped rather than read.
pub fn fusible_prefix(sources: &[ArgSource]) -> Option<usize> {
    let field_count = sources
        .iter()
        .take_while(|source| matches!(source, ArgSource::ParameterField(_)))
        .count();
    let ordered = sources[..field_count]
        .iter()
        .enumerate()
        .all(|(index, source)| *source == ArgSource::ParameterField(index));
    let rest_constant = sources[field_count..]
        .iter()
        .all(|source| matches!(source, ArgSource::Constant(_)));
    if ordered && rest_constant {
        Some(field_count)
    } else {
        None
    }
}

/// A symbolic stack value during the body walk.
#[derive(Clone, Debug, PartialEq)]
enum Sym {
    Param,
    ParamField(usize),
    Const(usize),
    BuiltinRef(usize),
    ArgTuple { tuple_id: usize, fields: Vec<Sym> },
    Result,
}

/// Analyze a capture-free function body into a [`Forwarder`], or `None` when it is
/// anything more than one. The walk starts as the executor's apply does — the
/// parameter on the stack — and must end with exactly the builtin's result there.
pub fn analyze(program: &Program, function_index: usize) -> Option<Forwarder> {
    let function = program.get_function(function_index)?;
    let mut stack: Vec<Sym> = vec![Sym::Param];
    let mut locals: Vec<Sym> = Vec::new();
    let mut forwarder: Option<Forwarder> = None;

    for instruction in &function.instructions {
        let operand = instruction.operand() as usize;
        match instruction.opcode() {
            // The opening parameter store; any later store is a binding, so ineligible.
            Opcode::Store => {
                if !locals.is_empty() || stack.pop()? != Sym::Param {
                    return None;
                }
                locals.push(Sym::Param);
            }
            Opcode::Load => stack.push(locals.get(operand)?.clone()),
            Opcode::Constant => stack.push(Sym::Const(operand)),
            Opcode::Duplicate => stack.push(stack.last()?.clone()),
            Opcode::Pick => {
                let index = stack.len().checked_sub(1 + operand)?;
                stack.push(stack[index].clone());
            }
            Opcode::Pop => {
                stack.pop()?;
            }
            Opcode::Rotate => {
                let index = stack.len().checked_sub(operand)?;
                let value = stack.remove(index);
                stack.push(value);
            }
            Opcode::Squash => {
                let top = stack.pop()?;
                stack.truncate(stack.len().checked_sub(operand)?);
                stack.push(top);
            }
            Opcode::GetPositional => {
                if stack.pop()? != Sym::Param {
                    return None;
                }
                stack.push(Sym::ParamField(operand));
            }
            Opcode::Tuple => {
                let arity = program.lookup_tuple(operand)?.fields.len();
                let index = stack.len().checked_sub(arity)?;
                let fields = stack.split_off(index);
                // A single-field tuple over the call's result is a result wrap
                // (`Str[~]`); otherwise every field must be forwardable.
                if fields == [Sym::Result] {
                    forwarder.as_mut()?.wraps.push(operand);
                    stack.push(Sym::Result);
                } else if fields
                    .iter()
                    .all(|f| matches!(f, Sym::Param | Sym::ParamField(_) | Sym::Const(_)))
                {
                    stack.push(Sym::ArgTuple {
                        tuple_id: operand,
                        fields,
                    });
                } else {
                    return None;
                }
            }
            Opcode::Builtin => stack.push(Sym::BuiltinRef(operand)),
            Opcode::Call => {
                // Exactly one call, of a builtin, on a forwardable argument.
                if forwarder.is_some() {
                    return None;
                }
                let Some(Sym::BuiltinRef(builtin)) = stack.pop() else {
                    return None;
                };
                let argument = match stack.pop()? {
                    Sym::Param => Argument::Parameter,
                    Sym::ArgTuple { tuple_id, fields } => Argument::Rebuild {
                        tuple_id,
                        sources: fields
                            .into_iter()
                            .map(|field| match field {
                                Sym::Param => ArgSource::Parameter,
                                Sym::ParamField(i) => ArgSource::ParameterField(i),
                                Sym::Const(c) => ArgSource::Constant(c),
                                _ => unreachable!("gated by the Tuple arm"),
                            })
                            .collect(),
                    },
                    _ => return None,
                };
                forwarder = Some(Forwarder {
                    builtin,
                    argument,
                    wraps: vec![],
                });
                stack.push(Sym::Result);
            }
            // Locals truncation (the return path, a scope exit): no stack effect, but
            // modelled rather than ignored so a later `Load` of a cleared local reads
            // as out of range and makes the function ineligible, instead of resolving
            // against a local the executor no longer has.
            Opcode::Reset => locals.truncate(operand),
            // Anything else — control flow, matching, annotation traffic, further
            // calls — is beyond a forwarder.
            _ => return None,
        }
    }

    if stack == [Sym::Result] {
        forwarder
    } else {
        None
    }
}
