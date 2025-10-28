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

    /// Emits Pick followed by Get - common pattern for accessing nested fields
    pub fn emit_pick_and_get(&mut self, depth: usize, index: usize) {
        self.add_instruction(Instruction::Pick(depth));
        self.add_instruction(Instruction::Get(index));
    }

    /// Emits Rotate followed by Pop - common pattern for cleaning up stack values
    pub fn emit_rotate_pop(&mut self, rotate_count: usize) {
        self.add_instruction(Instruction::Rotate(rotate_count));
        self.add_instruction(Instruction::Pop);
    }

    /// Emits type check and branch pattern: Pick -> IsType -> Not -> JumpIf
    /// Returns the jump address for patching later
    /// Used when branching based on type matching
    pub fn emit_type_check_branch(&mut self, depth: usize, type_id: usize) -> usize {
        self.add_instruction(Instruction::Pick(depth));
        self.add_instruction(Instruction::IsType(type_id));
        self.add_instruction(Instruction::Not);
        self.emit_jump_if_placeholder()
    }
}
