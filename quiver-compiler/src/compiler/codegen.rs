use quiver_core::{
    bytecode::{Instruction, Offset, Opcode},
    program::Program,
};

/// Helper struct for managing instruction generation and jumps
pub struct InstructionBuilder {
    pub instructions: Vec<Instruction>,
}

impl Default for InstructionBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl InstructionBuilder {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction)
    }

    /// Whether the instructions from `addr` on may be rewritten: true when no jump
    /// already lands beyond `addr`, so cutting there moves no jump's target. Jump
    /// operands are *relative* and are fixed at patch time, so removing instructions
    /// silently retargets every jump that reaches past the cut — this is what tells a
    /// rewrite when to leave the tail alone.
    ///
    /// A jump *to* `addr` is fine: a rewrite starts its replacement there, so that
    /// target keeps meaning "the code that continues from here".
    ///
    /// Answered by scanning rather than by a watermark the emitters maintain: the
    /// buffer is swapped out and back for every nested body, so a watermark would have
    /// to travel with it, and a rewrite reading a stale one would be silently wrong.
    pub fn rewritable_from(&self, addr: usize) -> bool {
        !self.instructions[..addr].iter().enumerate().any(|(a, i)| {
            matches!(
                i.opcode(),
                Opcode::Jump | Opcode::JumpIf | Opcode::JumpUnless
            ) && (a as Offset) + 1 + i.offset() > addr as Offset
        })
    }

    /// Emits a jump placeholder and returns the address to patch later
    pub fn emit_jump_placeholder(&mut self) -> usize {
        let addr = self.instructions.len();
        self.add_instruction(Instruction::jump(0));
        addr
    }

    /// Emits a conditional jump placeholder and returns the address to patch later (jumps on truthy)
    pub fn emit_jump_if_placeholder(&mut self) -> usize {
        let addr = self.instructions.len();
        self.add_instruction(Instruction::jump_if(0));
        addr
    }

    /// Emits a conditional jump placeholder and returns the address to patch later (jumps on nil)
    pub fn emit_jump_unless_placeholder(&mut self) -> usize {
        let addr = self.instructions.len();
        self.add_instruction(Instruction::jump_unless(0));
        addr
    }

    /// Patches a jump instruction to target the current instruction address
    pub fn patch_jump_to_here(&mut self, jump_addr: usize) {
        let target_addr = self.instructions.len();
        self.patch_jump_to_addr(jump_addr, target_addr);
    }

    /// Patches a jump instruction to target a specific address
    pub fn patch_jump_to_addr(&mut self, jump_addr: usize, target_addr: usize) {
        let offset = (target_addr as Offset) - (jump_addr as Offset) - 1;
        self.instructions[jump_addr] = match self.instructions[jump_addr].opcode() {
            Opcode::Jump => Instruction::jump(offset),
            Opcode::JumpIf => Instruction::jump_if(offset),
            Opcode::JumpUnless => Instruction::jump_unless(offset),
            other => panic!("Cannot patch non-jump instruction {other:?}"),
        };
    }

    /// Emits an unconditional jump that immediately targets the given address
    pub fn emit_jump_to_addr(&mut self, addr: usize) {
        let current_addr = self.instructions.len();
        let offset = (addr as Offset) - (current_addr as Offset) - 1;
        self.add_instruction(Instruction::jump(offset));
    }

    /// Emits a jump on nil that immediately targets the given address
    pub fn emit_jump_unless_to_addr(&mut self, addr: usize) {
        let current_addr = self.instructions.len();
        let offset = (addr as Offset) - (current_addr as Offset) - 1;
        self.add_instruction(Instruction::jump_unless(offset));
    }

    /// Emits Pick(0) -> JumpUnless (returns the jump address for patching), keeping the tested
    /// value on the stack on both paths. Used to short-circuit a sequence on a nil step while
    /// threading the (non-nil) value into the next step, and to keep a condition's value
    /// available to its consequence.
    pub fn emit_duplicate_jump_if_nil(&mut self) -> usize {
        self.add_instruction(Instruction::pick(0));
        self.emit_jump_unless_placeholder()
    }

    /// Emits Rotate followed by Pop - common pattern for cleaning up stack values
    pub fn emit_rotate_pop(&mut self, rotate_count: usize) {
        self.add_instruction(Instruction::rotate(rotate_count));
        self.add_instruction(Instruction::pop());
    }
}

/// How an instruction changes the operand stack's depth, or `None` for one that ends the
/// function's flow here (a tail call, a self-recursion, a trap).
fn stack_effect(instruction: Instruction, program: &Program) -> Result<Option<isize>, String> {
    if crate::verify::ends_flow(instruction.opcode()) {
        return Ok(None);
    }
    let (pops, pushes) = crate::verify::stack_effect(instruction, program)
        .ok_or_else(|| format!("{instruction:?} names an unknown tuple or function"))?;
    Ok(Some(pushes as isize - pops as isize))
}
/// The operand-stack depth before each instruction from `start` on, relative to depth 0 at
/// `start`; `None` where no path from `start` reaches. The jumps at `exits` leave the range —
/// where they land is not this range's to say — as does any jump landing outside it. Codegen is
/// structured, so every instruction has one depth: two paths disagreeing is an error.
pub fn operand_depths(
    instructions: &[Instruction],
    start: usize,
    exits: &[usize],
    program: &Program,
) -> Result<Vec<Option<isize>>, String> {
    let len = instructions.len();
    let mut depths: Vec<Option<isize>> = vec![None; len.saturating_sub(start)];
    let mut work = vec![(start, 0isize)];
    while let Some((addr, depth)) = work.pop() {
        if addr < start || addr >= len {
            continue;
        }
        match depths[addr - start] {
            Some(known) if known == depth => continue,
            Some(known) => {
                return Err(format!(
                    "instruction {addr} reached at depths {known} and {depth}"
                ));
            }
            None => depths[addr - start] = Some(depth),
        }
        let instruction = instructions[addr];
        let Some(effect) = stack_effect(instruction, program)? else {
            continue;
        };
        let after = depth + effect;
        let target = || (addr as Offset + 1 + instruction.offset()) as usize;
        match instruction.opcode() {
            Opcode::Jump if exits.contains(&addr) => {}
            Opcode::Jump => work.push((target(), after)),
            Opcode::JumpIf | Opcode::JumpUnless => {
                work.push((addr + 1, after));
                if !exits.contains(&addr) {
                    work.push((target(), after));
                }
            }
            _ => work.push((addr + 1, after)),
        }
    }
    Ok(depths)
}
