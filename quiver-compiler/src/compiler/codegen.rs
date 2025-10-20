use quiver_core::bytecode::Instruction;

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

    /// Emits a jump placeholder and returns the address to patch later
    pub fn emit_jump_placeholder(&mut self) -> usize {
        let addr = self.instructions.len();
        self.add_instruction(Instruction::Jump(0));
        addr
    }

    /// Emits a conditional jump placeholder and returns the address to patch later (jumps on truthy)
    pub fn emit_jump_if_placeholder(&mut self) -> usize {
        let addr = self.instructions.len();
        self.add_instruction(Instruction::JumpIf(0));
        addr
    }

    /// Patches a jump instruction to target the current instruction address
    pub fn patch_jump_to_here(&mut self, jump_addr: usize) {
        let target_addr = self.instructions.len();
        self.patch_jump_to_addr(jump_addr, target_addr);
    }

    /// Patches a jump instruction to target a specific address
    pub fn patch_jump_to_addr(&mut self, jump_addr: usize, target_addr: usize) {
        let offset = (target_addr as isize) - (jump_addr as isize) - 1;
        self.instructions[jump_addr] = match &self.instructions[jump_addr] {
            Instruction::Jump(_) => Instruction::Jump(offset),
            Instruction::JumpIf(_) => Instruction::JumpIf(offset),
            _ => panic!("Cannot patch non-jump instruction"),
        };
    }

    /// Emits a conditional jump that immediately targets the given address
    pub fn emit_jump_if_to_addr(&mut self, addr: usize) {
        let current_addr = self.instructions.len();
        let offset = (addr as isize) - (current_addr as isize) - 1;
        self.add_instruction(Instruction::JumpIf(offset));
    }

    /// Emits common pattern: Duplicate -> Not -> JumpIf (returns jump address for patching) -> Pop
    /// Used for early termination when value is nil
    pub fn emit_duplicate_jump_if_nil_pop(&mut self) -> usize {
        self.add_instruction(Instruction::Duplicate);
        self.add_instruction(Instruction::Not);
        let jump_addr = self.emit_jump_if_placeholder();
        self.add_instruction(Instruction::Pop);
        jump_addr
    }
}
