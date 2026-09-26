//! Structural verification of a function body: control flow stays inside it, the operand
//! stack is used consistently, and locals are written before they are read.
//!
//! The executor trusts what it runs — a stack underflow or an unbound local is a runtime
//! error there, found only on the path that hits it. Checking once, over every path, is what
//! lets a host refuse a malformed unit at its trust boundary (see
//! [`crate::artifact::validate_unit`]), and what lets the compiler check its own output
//! (see `Compiler::finish_body`).

use quiver_core::bytecode::{Instruction, Offset, Opcode};
use quiver_core::program::Program;
use quiver_core::types::TypeLookup;

/// The table facts an instruction's stack effect depends on.
pub trait CodeTables {
    /// How many fields a tuple has, which `Tuple` pops.
    fn tuple_arity(&self, tuple: usize) -> Option<usize>;
    /// How many captures a function closes over, which `Function` pops.
    fn function_captures(&self, function: usize) -> Option<usize>;
}

impl CodeTables for Program {
    fn tuple_arity(&self, tuple: usize) -> Option<usize> {
        self.lookup_tuple(tuple).map(|info| info.fields.len())
    }

    fn function_captures(&self, function: usize) -> Option<usize> {
        self.get_function(function)
            .map(|function| function.captures)
    }
}

/// How many values an instruction pops and then pushes. `None` when its operand names a
/// tuple or function the tables don't have.
pub fn stack_effect(instruction: Instruction, tables: &impl CodeTables) -> Option<(usize, usize)> {
    let operand = instruction.operand() as usize;
    Some(match instruction.opcode() {
        Opcode::Constant | Opcode::Pick | Opcode::Load | Opcode::Nil | Opcode::Ok => (0, 1),
        Opcode::Pop | Opcode::Store | Opcode::JumpIf | Opcode::JumpUnless | Opcode::Recurse => {
            (1, 0)
        }
        Opcode::Reset | Opcode::Jump | Opcode::Reclaimed => (0, 0),
        Opcode::Rotate => (operand, operand),
        Opcode::Drop => (operand + 1, 1),
        Opcode::GetPositional
        | Opcode::GetNamed
        | Opcode::IsType
        | Opcode::GetAnnotation
        | Opcode::Stamp
        | Opcode::Select
        | Opcode::Process => (1, 1),
        Opcode::Equal | Opcode::Annotate | Opcode::Call => (2, 1),
        Opcode::TailCall => (2, 0),
        Opcode::Tuple => (tables.tuple_arity(operand)?, 1),
        Opcode::Function => (tables.function_captures(operand)?, 1),
    })
}

/// Whether control never continues past an instruction: a tail call or self-recursion
/// replaces the frame, and a reclaimed trap aborts.
pub fn ends_flow(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::TailCall | Opcode::Recurse | Opcode::Reclaimed
    )
}

/// Which locals a body may read without writing them first.
#[derive(Clone, Copy, Debug)]
pub enum Locals {
    /// The frame's first `n` slots are filled on entry: a function's captures, or the
    /// bindings of the top level's earlier lines.
    Filled(usize),
    /// Not known here — a unit's entry runs in a frame whose earlier bindings the unit
    /// cannot see — so reads are not checked.
    Unknown,
}

/// What is known on arrival at an instruction: the operand-stack depth (above the frame's
/// base), and which locals every path to it has written.
#[derive(Clone, PartialEq)]
struct State {
    depth: usize,
    written: Vec<bool>,
}

/// Verify a function body. A function is entered with its parameter as the stack's one
/// value, and must leave exactly its result: at a return (running off the end) the stack
/// holds one value, and at a tail call only the call's own operands.
pub fn function(
    instructions: &[Instruction],
    locals: Locals,
    tables: &impl CodeTables,
) -> Result<(), String> {
    let len = instructions.len();
    let entry = State {
        depth: 1,
        written: match locals {
            Locals::Filled(count) => vec![true; count],
            Locals::Unknown => Vec::new(),
        },
    };
    let mut states: Vec<Option<State>> = vec![None; len + 1];
    let mut work = vec![(0, entry)];
    while let Some((at, arriving)) = work.pop() {
        let state = match &states[at] {
            None => arriving,
            Some(known) => {
                if known.depth != arriving.depth {
                    return Err(format!(
                        "instruction {at} is reached at stack depths {} and {}",
                        known.depth, arriving.depth
                    ));
                }
                // A local counts as written only if every path to here wrote it.
                let joined = State {
                    depth: known.depth,
                    written: known
                        .written
                        .iter()
                        .zip(&arriving.written)
                        .map(|(a, b)| *a && *b)
                        .collect(),
                };
                if joined == *known {
                    continue;
                }
                joined
            }
        };
        states[at] = Some(state.clone());
        if at == len {
            if state.depth != 1 {
                return Err(format!("returns with {} values on the stack", state.depth));
            }
            continue;
        }
        let instruction = instructions[at];
        let fail = |problem: String| Err(format!("instruction {at} ({instruction:?}) {problem}"));
        let (pops, pushes) = match stack_effect(instruction, tables) {
            Some(effect) => effect,
            None => return fail("names a tuple or function outside the tables".to_string()),
        };
        let operand = instruction.operand() as usize;
        let reads = match instruction.opcode() {
            Opcode::Pick => operand + 1,
            Opcode::Rotate if operand < 2 => return fail("rotates fewer than two".to_string()),
            _ => pops,
        };
        if state.depth < reads {
            return fail(format!(
                "reads {reads} values from a stack of {}",
                state.depth
            ));
        }
        let mut next = State {
            depth: state.depth - pops + pushes,
            written: state.written,
        };
        match instruction.opcode() {
            Opcode::Load
                if matches!(locals, Locals::Filled(_))
                    && !next.written.get(operand).copied().unwrap_or(false) =>
            {
                return fail("reads a local not every path to it has written".to_string());
            }
            Opcode::Store => {
                if next.written.len() <= operand {
                    next.written.resize(operand + 1, false);
                }
                next.written[operand] = true;
            }
            Opcode::Reset => next.written.truncate(operand),
            _ => {}
        }
        // A frame is replaced with nothing of its own left on the stack.
        let leaves = |remaining: usize| -> Result<(), String> {
            if remaining == 0 {
                Ok(())
            } else {
                Err(format!(
                    "instruction {at} ({instruction:?}) leaves the frame with {remaining} values \
                     beneath it"
                ))
            }
        };
        let target = || -> Result<usize, String> {
            let target = at as Offset + 1 + instruction.offset();
            if (0..=len as Offset).contains(&target) {
                Ok(target as usize)
            } else {
                Err(format!(
                    "instruction {at} ({instruction:?}) jumps outside the body"
                ))
            }
        };
        match instruction.opcode() {
            Opcode::Jump => work.push((target()?, next)),
            Opcode::JumpIf | Opcode::JumpUnless => {
                work.push((target()?, next.clone()));
                work.push((at + 1, next));
            }
            Opcode::TailCall | Opcode::Recurse => leaves(next.depth)?,
            Opcode::Reclaimed => {}
            _ => work.push((at + 1, next)),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Tables where every tuple is a pair and every function captures one value.
    struct Pairs;

    impl CodeTables for Pairs {
        fn tuple_arity(&self, _: usize) -> Option<usize> {
            Some(2)
        }

        fn function_captures(&self, _: usize) -> Option<usize> {
            Some(1)
        }
    }

    fn verify(instructions: &[Instruction], locals: Locals) -> Result<(), String> {
        function(instructions, locals, &Pairs)
    }

    #[test]
    fn accepts_a_balanced_body() {
        // Store the parameter; if it is present, pair it with itself, else answer nil.
        let body = [
            Instruction::store(0),
            Instruction::load(0),
            Instruction::jump_unless(4),
            Instruction::load(0),
            Instruction::load(0),
            Instruction::tuple(2),
            Instruction::jump(1),
            Instruction::nil(),
        ];
        assert_eq!(verify(&body, Locals::Filled(0)), Ok(()));
    }

    #[test]
    fn rejects_paths_that_disagree_on_depth() {
        let body = [
            Instruction::pick(0),
            Instruction::jump_if(1),
            Instruction::nil(),
            Instruction::pop(),
        ];
        assert!(verify(&body, Locals::Filled(0)).is_err());
    }

    #[test]
    fn rejects_underflow_and_a_short_return() {
        assert!(verify(&[Instruction::pop(), Instruction::pop()], Locals::Filled(0)).is_err());
        assert!(verify(&[Instruction::pop()], Locals::Filled(0)).is_err());
        assert!(verify(&[Instruction::pick(1)], Locals::Filled(0)).is_err());
    }

    #[test]
    fn rejects_a_jump_outside_the_body() {
        assert!(verify(&[Instruction::jump(5)], Locals::Filled(0)).is_err());
    }

    #[test]
    fn a_local_must_be_written_on_every_path() {
        // Slot 1 is written only when the parameter is present.
        let body = [
            Instruction::store(0),
            Instruction::load(0),
            Instruction::jump_unless(2),
            Instruction::load(0),
            Instruction::store(1),
            Instruction::load(1),
        ];
        assert!(verify(&body, Locals::Filled(0)).is_err());
        // Unless it is filled on entry, or the frame's locals are unknown.
        assert_eq!(verify(&body, Locals::Filled(2)), Ok(()));
        assert_eq!(verify(&body, Locals::Unknown), Ok(()));
        // A reset forgets what was written.
        let reset = [
            Instruction::store(0),
            Instruction::reset(0),
            Instruction::load(0),
        ];
        assert!(verify(&reset, Locals::Filled(0)).is_err());
    }

    #[test]
    fn a_tail_call_leaves_nothing_beneath_its_operands() {
        let clean = [Instruction::load(0), Instruction::tail_call()];
        assert_eq!(verify(&clean, Locals::Filled(1)), Ok(()));
        let leaky = [
            Instruction::load(0),
            Instruction::load(0),
            Instruction::tail_call(),
        ];
        assert!(verify(&leaky, Locals::Filled(1)).is_err());
    }
}
