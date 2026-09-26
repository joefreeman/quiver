//! Optimisation of a finished function body: jump threading, unreachable-code removal,
//! removal of values pushed only to be dropped, and local rewrites of adjacent instructions.
//!
//! Codegen emits structurally — each construct is compiled without regard to its
//! neighbours — so a body is left with pushes that are immediately popped, jumps to jumps,
//! and failure paths nothing reaches. Rewriting those while emitting is unsafe, because a
//! jump's operand is relative and fixed when it is patched: removing an instruction
//! silently retargets every jump that reaches past it. Here the body is complete, so jumps
//! are first made absolute, the body is rewritten freely, and the offsets are recomputed
//! at the end.

use quiver_core::bytecode::{Instruction, Offset, Opcode};
use quiver_core::program::Program;
use quiver_core::types::TypeLookup;

/// An instruction with its jump target (if it has one) held as an absolute index, so
/// instructions around it can be removed without disturbing it. A target may equal the
/// body's length: the jump leaves the body.
#[derive(Clone, Copy, Debug)]
struct Op {
    instruction: Instruction,
    target: Option<usize>,
}

/// Optimise a finished body. Its semantics — including the operand-stack depth at every
/// surviving instruction — are unchanged.
pub fn body(instructions: Vec<Instruction>, program: &Program) -> Vec<Instruction> {
    let mut ops = decode(&instructions);
    loop {
        let threaded = thread_jumps(&mut ops);
        let pruned = remove_unreachable(&mut ops);
        let combined = combine_adjacent(&mut ops);
        let dead = remove_dead_values(&mut ops, program);
        if !(threaded || pruned || combined || dead) {
            break;
        }
    }
    // An empty body is not merely a shorter one: a receive source with no instructions is
    // a type-only receiver, accepting the message without running it as a filter.
    if ops.is_empty() && !instructions.is_empty() {
        return instructions;
    }
    encode(&ops)
}

fn decode(instructions: &[Instruction]) -> Vec<Op> {
    instructions
        .iter()
        .enumerate()
        .map(|(index, &instruction)| {
            let target = matches!(instruction.opcode(), Opcode::Jump | Opcode::JumpIf).then(|| {
                let target = index as Offset + 1 + instruction.offset();
                assert!(
                    (0..=instructions.len() as Offset).contains(&target),
                    "jump at {index} lands outside its body"
                );
                target as usize
            });
            Op {
                instruction,
                target,
            }
        })
        .collect()
}

fn encode(ops: &[Op]) -> Vec<Instruction> {
    ops.iter()
        .enumerate()
        .map(|(index, op)| match op.target {
            Some(target) => {
                let offset = target as Offset - index as Offset - 1;
                match op.instruction.opcode() {
                    Opcode::Jump => Instruction::jump(offset),
                    Opcode::JumpIf => Instruction::jump_if(offset),
                    other => unreachable!("{other:?} carries no jump target"),
                }
            }
            None => op.instruction,
        })
        .collect()
}

/// Drop the ops marked in `removed`, retargeting jumps. A jump to a removed op lands on the
/// next surviving one, which is what the removed op would have fallen through to.
fn compact(ops: &mut Vec<Op>, removed: &[bool]) -> bool {
    if !removed.contains(&true) {
        return false;
    }
    // `remap[i]` is the number of surviving ops before `i` — the new index of `i` if it
    // survives, and of its successor if not. One extra entry covers a jump to the end.
    let mut remap = Vec::with_capacity(ops.len() + 1);
    let mut kept = 0;
    for &gone in removed {
        remap.push(kept);
        kept += usize::from(!gone);
    }
    remap.push(kept);
    let mut index = 0;
    ops.retain(|_| {
        index += 1;
        !removed[index - 1]
    });
    for op in ops.iter_mut() {
        if let Some(target) = &mut op.target {
            *target = remap[*target];
        }
    }
    true
}

/// Point every jump past the unconditional jumps it lands on, then drop a jump to the very
/// next instruction: an unconditional one does nothing, and a conditional one only pops.
fn thread_jumps(ops: &mut Vec<Op>) -> bool {
    let mut changed = false;
    for index in 0..ops.len() {
        let Some(mut target) = ops[index].target else {
            continue;
        };
        // Bounded, so a cycle of jumps (a loop with an empty body) cannot spin.
        for _ in 0..ops.len() {
            match ops.get(target) {
                Some(Op {
                    target: Some(next),
                    instruction,
                }) if instruction.opcode() == Opcode::Jump && *next != target => {
                    target = *next;
                }
                _ => break,
            }
        }
        if ops[index].target != Some(target) {
            ops[index].target = Some(target);
            changed = true;
        }
    }
    let mut removed = vec![false; ops.len()];
    for index in 0..ops.len() {
        if ops[index].target == Some(index + 1) {
            match ops[index].instruction.opcode() {
                Opcode::Jump => removed[index] = true,
                _ => {
                    ops[index] = Op {
                        instruction: Instruction::pop(),
                        target: None,
                    };
                    changed = true;
                }
            }
        }
    }
    compact(ops, &removed) || changed
}

/// Remove the ops no path from the entry reaches.
fn remove_unreachable(ops: &mut Vec<Op>) -> bool {
    let mut reached = vec![false; ops.len()];
    let mut work = vec![0];
    while let Some(index) = work.pop() {
        if index >= ops.len() || reached[index] {
            continue;
        }
        reached[index] = true;
        let op = ops[index];
        match op.instruction.opcode() {
            Opcode::Jump => work.extend(op.target),
            Opcode::JumpIf => work.extend([index + 1].into_iter().chain(op.target)),
            Opcode::TailCall | Opcode::Recurse | Opcode::Reclaimed => {}
            _ => work.push(index + 1),
        }
    }
    let removed: Vec<bool> = reached.iter().map(|reached| !reached).collect();
    compact(ops, &removed)
}

/// What a run of adjacent instructions can be rewritten as.
struct Rewrite {
    /// How many instructions, from the first, the rewrite replaces.
    length: usize,
    replacement: Option<Instruction>,
}

/// Rewrite short runs of adjacent instructions, and drop single instructions that do
/// nothing. A run is only rewritten when no jump lands inside it, since a path entering
/// there runs only its tail.
fn combine_adjacent(ops: &mut Vec<Op>) -> bool {
    let targeted = targeted(ops);
    let mut removed = vec![false; ops.len()];
    let mut changed = false;
    let mut index = 0;
    while index < ops.len() {
        let run: Vec<Instruction> = ops[index..]
            .iter()
            .enumerate()
            .take_while(|(offset, _)| *offset == 0 || !targeted[index + offset])
            .take(3)
            .map(|(_, op)| op.instruction)
            .collect();
        let Some(rewrite) = rewrite_run(&run) else {
            index += 1;
            continue;
        };
        removed[index..index + rewrite.length].fill(true);
        if let Some(instruction) = rewrite.replacement {
            ops[index].instruction = instruction;
            removed[index] = false;
            changed = true;
        }
        index += rewrite.length;
    }
    compact(ops, &removed) || changed
}

fn rewrite_run(run: &[Instruction]) -> Option<Rewrite> {
    let operand = |instruction: Instruction| instruction.operand() as usize;
    let opcodes: Vec<Opcode> = run.iter().map(|instruction| instruction.opcode()).collect();
    let rewrite = |length, replacement| {
        Some(Rewrite {
            length,
            replacement,
        })
    };
    match opcodes.as_slice() {
        [Opcode::Squash, ..] if operand(run[0]) == 0 => rewrite(1, None),
        // Storing a copy and dropping the original stores the original.
        [Opcode::Duplicate, Opcode::Store, Opcode::Pop] => rewrite(3, Some(Instruction::store())),
        // Bringing the second value to the top and popping it drops it from under the top.
        [Opcode::Rotate, Opcode::Pop, ..] if operand(run[0]) == 2 => {
            rewrite(2, Some(Instruction::squash(1)))
        }
        [Opcode::Rotate, Opcode::Rotate, ..] if operand(run[0]) == 2 && operand(run[1]) == 2 => {
            rewrite(2, None)
        }
        [Opcode::Squash, Opcode::Squash, ..] => rewrite(
            2,
            Some(Instruction::squash(operand(run[0]) + operand(run[1]))),
        ),
        // Truncating the locals further subsumes the first truncation.
        [Opcode::Reset, Opcode::Reset, ..] if operand(run[1]) <= operand(run[0]) => {
            rewrite(2, Some(run[1]))
        }
        _ => None,
    }
}

fn targeted(ops: &[Op]) -> Vec<bool> {
    let mut targeted = vec![false; ops.len() + 1];
    for target in ops.iter().filter_map(|op| op.target) {
        targeted[target] = true;
    }
    targeted
}

/// Remove values that are pushed only to be dropped. For each value a `Pop` or `Squash`
/// discards, walk back through the straight-line code before it to the instruction that
/// produced it; if that is a pure push — through any pure transforms of it — the push, the
/// transforms and the drop all go.
///
/// The value may sit beneath others (the flowing value under a term that ignores it), so
/// removing it moves everything below it one place up: a `Pick` in between that reaches
/// past it is adjusted to match.
fn remove_dead_values(ops: &mut Vec<Op>, program: &Program) -> bool {
    let targeted = targeted(ops);
    let mut removed = vec![false; ops.len()];
    let mut changed = false;
    for index in 0..ops.len() {
        let instruction = ops[index].instruction;
        let depth = match instruction.opcode() {
            Opcode::Pop => 0,
            Opcode::Squash if instruction.operand() > 0 => 1,
            _ => continue,
        };
        let Some(dead) = trace_dead_value(ops, &removed, &targeted, program, index, depth) else {
            continue;
        };
        for at in dead.producers {
            removed[at] = true;
        }
        for (at, depth) in dead.picks {
            ops[at].instruction = Instruction::pick(depth);
        }
        match instruction.opcode() {
            Opcode::Squash if instruction.operand() > 1 => {
                ops[index].instruction = Instruction::squash(instruction.operand() as usize - 1)
            }
            _ => removed[index] = true,
        }
        changed = true;
    }
    compact(ops, &removed) || changed
}

/// The instructions that produced a value about to be dropped, and the `Pick`s between
/// them and the drop that must shift to account for its removal.
struct DeadValue {
    producers: Vec<usize>,
    picks: Vec<(usize, usize)>,
}

/// Trace the value at `depth` below the top, just before the instruction at `drop`, back to
/// its producer. Fails on anything that isn't straight-line code, or that reads the value.
fn trace_dead_value(
    ops: &[Op],
    removed: &[bool],
    targeted: &[bool],
    program: &Program,
    drop: usize,
    mut depth: usize,
) -> Option<DeadValue> {
    let mut dead = DeadValue {
        producers: Vec::new(),
        picks: Vec::new(),
    };
    let mut index = drop;
    loop {
        // A path entering at `index` would skip whatever is removed before it.
        if targeted[index] {
            return None;
        }
        index = index.checked_sub(1)?;
        if removed[index] {
            continue;
        }
        let instruction = ops[index].instruction;
        let opcode = instruction.opcode();
        let (pops, pushes) = stack_effect(instruction, program)?;
        if depth >= pushes {
            // Passing over: the instruction works above the value, or reads past it.
            match opcode {
                Opcode::Duplicate if depth == pushes => return None,
                Opcode::Pick => {
                    let reach = instruction.operand() as usize;
                    match reach.cmp(&(depth - pushes)) {
                        std::cmp::Ordering::Less => {}
                        std::cmp::Ordering::Equal => return None,
                        std::cmp::Ordering::Greater => dead.picks.push((index, reach - 1)),
                    }
                }
                _ => {}
            }
            depth = depth - pushes + pops;
            continue;
        }
        // The instruction produced the value.
        dead.producers.push(index);
        if pushes_purely(opcode) {
            return Some(dead);
        }
        if !transforms_purely(opcode) {
            return None;
        }
        // A pure transform of a value is dead with it: trace its input instead.
    }
}

/// How many values an instruction pops and pushes, or `None` for one this pass doesn't
/// trace through (control flow).
fn stack_effect(instruction: Instruction, program: &Program) -> Option<(usize, usize)> {
    let operand = instruction.operand() as usize;
    Some(match instruction.opcode() {
        Opcode::Constant
        | Opcode::Duplicate
        | Opcode::Pick
        | Opcode::Load
        | Opcode::Nil
        | Opcode::Ok
        | Opcode::Builtin
        | Opcode::Self_ => (0, 1),
        Opcode::Pop | Opcode::Store => (1, 0),
        Opcode::Reset => (0, 0),
        Opcode::Rotate => (operand, operand),
        Opcode::Squash => (operand + 1, 1),
        Opcode::GetPositional
        | Opcode::GetNamed
        | Opcode::IsType
        | Opcode::Not
        | Opcode::GetAnnotation
        | Opcode::Stamp
        | Opcode::Select
        | Opcode::Process
        | Opcode::State => (1, 1),
        Opcode::Equal | Opcode::Annotate | Opcode::Call | Opcode::Spawn => (2, 1),
        Opcode::Tuple => (program.lookup_tuple(operand)?.fields.len(), 1),
        Opcode::Function => (program.get_function(operand)?.captures, 1),
        Opcode::Jump | Opcode::JumpIf | Opcode::TailCall | Opcode::Recurse | Opcode::Reclaimed => {
            return None;
        }
    })
}

/// Opcodes that push one value, read nothing beneath it, and have no other effect.
fn pushes_purely(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::Constant
            | Opcode::Nil
            | Opcode::Ok
            | Opcode::Load
            | Opcode::Duplicate
            | Opcode::Pick
            | Opcode::Builtin
    )
}

/// Opcodes that replace the top value with one computed from it, with no other effect.
fn transforms_purely(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::GetPositional
            | Opcode::GetNamed
            | Opcode::IsType
            | Opcode::Not
            | Opcode::GetAnnotation
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(instructions: Vec<Instruction>) -> Vec<Instruction> {
        body(instructions, &Program::new())
    }

    #[test]
    fn removes_pushes_that_are_popped() {
        assert_eq!(
            run(vec![
                Instruction::load(0),
                Instruction::duplicate(),
                Instruction::pop(),
                Instruction::pick(1),
                Instruction::pop(),
            ]),
            vec![Instruction::load(0)]
        );
    }

    #[test]
    fn cascades_through_transforms() {
        assert_eq!(
            run(vec![
                Instruction::nil(),
                Instruction::load(0),
                Instruction::get_positional(1),
                Instruction::pop(),
            ]),
            vec![Instruction::nil()]
        );
    }

    #[test]
    fn rotate_pop_becomes_squash() {
        // A call's result isn't a pure push, so it survives as a squash.
        assert_eq!(
            run(vec![
                Instruction::load(0),
                Instruction::load(1),
                Instruction::call(),
                Instruction::load(2),
                Instruction::rotate(2),
                Instruction::pop(),
            ]),
            vec![
                Instruction::load(0),
                Instruction::load(1),
                Instruction::call(),
                Instruction::load(2),
                Instruction::squash(1)
            ]
        );
    }

    #[test]
    fn removes_a_value_dropped_from_beneath_the_top() {
        // The flowing value (`Load(0)`) under a term that never reads it.
        assert_eq!(
            run(vec![
                Instruction::load(1),
                Instruction::load(0),
                Instruction::pick(1),
                Instruction::constant(0),
                Instruction::equal(),
                Instruction::squash(1),
            ]),
            vec![
                Instruction::load(1),
                Instruction::duplicate(),
                Instruction::constant(0),
                Instruction::equal(),
            ]
        );
    }

    #[test]
    fn keeps_a_value_read_before_it_is_dropped() {
        let body = vec![
            Instruction::load(0),
            Instruction::duplicate(),
            Instruction::squash(1),
        ];
        assert_eq!(run(body.clone()), body);
    }

    #[test]
    fn keeps_a_pair_whose_second_instruction_is_a_jump_target() {
        let body = vec![
            Instruction::load(0),
            Instruction::jump_if(1),
            Instruction::duplicate(),
            Instruction::pop(),
        ];
        assert_eq!(run(body.clone()), body);
    }

    #[test]
    fn threads_jumps_and_drops_what_they_skip() {
        assert_eq!(
            run(vec![
                Instruction::load(0),
                Instruction::jump_if(2), // → 4, itself a jump → 6
                Instruction::nil(),
                Instruction::jump(2), // → 6
                Instruction::jump(1), // → 6, unreachable once the first is threaded
                Instruction::nil(),   // unreachable
                Instruction::load(1),
            ]),
            vec![
                Instruction::load(0),
                Instruction::jump_if(1),
                Instruction::nil(),
                Instruction::load(1),
            ]
        );
    }

    #[test]
    fn a_conditional_jump_to_the_next_instruction_only_pops() {
        assert_eq!(
            run(vec![
                Instruction::load(0),
                Instruction::load(1),
                Instruction::jump_if(0),
            ]),
            vec![Instruction::load(0)]
        );
    }

    #[test]
    fn a_body_is_never_emptied() {
        let body = vec![Instruction::duplicate(), Instruction::pop()];
        assert_eq!(run(body.clone()), body);
    }
}
