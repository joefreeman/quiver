//! Constant folding of a tuple literal: a literal whose every field is itself constant is
//! data, not a build, so it is interned once and pushed as a single `Constant` instead of
//! allocating a payload on every evaluation.
//!
//! The decision is made by abstractly interpreting the literal's just-emitted instruction
//! range over a symbolic stack — the same approach [`super::elision`] takes, and for the same
//! reason: codegen threads the chain's flowing value through a literal's fields, so the range
//! is never a clean run of constant pushes. `[1, 2]` emits
//! `Duplicate; Pop; Constant(1); Pick(1); Pop; Constant(0); Tuple(2)`, and a nested literal
//! adds a `Rotate(2); Pop` that cancels its own prologue. Matching those shapes would be
//! guessing at incidental stack traffic; tracking the stack is not.
//!
//! Folding runs per literal, as each one finishes emitting, which makes it bottom-up for
//! free: an inner literal has already collapsed to one `Constant` by the time the outer walk
//! reaches it. That is what lets a partly-dynamic template still fold — `%html{ … {name} … }`
//! keeps only the spine containing the hole, and every static subtree under it becomes a
//! constant.

use quiver_core::bytecode::{Constant, Instruction, Opcode};
use quiver_core::program::Program;
use quiver_core::types::TypeLookup;

/// A symbolic stack entry.
#[derive(Clone, Copy, PartialEq)]
enum Sym {
    /// A value from below the range — the chain's flowing value, or whatever else the frame
    /// left. It may be copied (`Pick` reads without consuming) but never folded into a
    /// constant, and the range may not consume one.
    Outer,
    /// A value known to be the constant at this index.
    Const(usize),
}

/// The constant this instruction range evaluates to, if it evaluates to one.
///
/// Answers `Some(index)` only when the range's whole effect is to leave exactly one new
/// value on the stack, that value is a known constant, and nothing below the range was
/// consumed. Any instruction outside the small repertoire below — a call, a load, a jump —
/// answers `None`, which is what keeps this the safe subset: a range with no control flow
/// has no branch to mis-model.
///
/// Constants are interned as the walk meets each `Tuple`, rather than being accumulated and
/// committed at the end. That keeps the symbolic stack flat — a thousand-element list
/// literal is a thousand-deep *nest*, and holding it as a tree would need a recursive drop —
/// at the cost of an occasional wasted table row when a walk interns and then bails. Since
/// inner literals have already folded to a single `Constant`, a walk normally interns once,
/// at its final instruction, and so bails before interning anything at all.
pub fn constant_of_range(program: &mut Program, instructions: &[Instruction]) -> Option<usize> {
    let mut stack: Vec<Sym> = Vec::new();

    for instruction in instructions {
        let operand = instruction.operand() as usize;
        match instruction.opcode() {
            Opcode::Constant => stack.push(Sym::Const(operand)),
            // A copy, never a consume: reaching past what the range itself pushed reads a
            // value from below, which stays opaque but is not disturbed.
            Opcode::Pick => stack.push(pick(&stack, operand)),
            // Consuming past the range's own values would take one of the enclosing frame's,
            // which the replacement `Constant` push would not do.
            Opcode::Pop => {
                stack.pop()?;
            }
            Opcode::Rotate => {
                let index = stack.len().checked_sub(operand)?;
                let value = stack.remove(index);
                stack.push(value);
            }
            Opcode::Tuple => {
                let arity = program.lookup_tuple(operand)?.fields.len();
                let index = stack.len().checked_sub(arity)?;
                let fields = stack
                    .split_off(index)
                    .into_iter()
                    .map(|field| match field {
                        Sym::Const(constant) => Some(constant),
                        Sym::Outer => None,
                    })
                    .collect::<Option<Vec<usize>>>()?;
                let constant = program.register_constant(Constant::Tuple {
                    id: operand,
                    fields,
                });
                stack.push(Sym::Const(constant));
            }
            _ => return None,
        }
    }

    match stack[..] {
        [Sym::Const(constant)] => Some(constant),
        _ => None,
    }
}

/// The value `Pick(depth)` copies: one the range pushed, or an opaque one from below it.
fn pick(stack: &[Sym], depth: usize) -> Sym {
    match stack.len().checked_sub(depth + 1) {
        Some(index) => stack[index],
        None => Sym::Outer,
    }
}
