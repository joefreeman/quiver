use crate::bytecode::{Instruction, TypeId};

use crate::types::Type;

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

    /// Extracts all fields from a tuple on the stack, placing field values on top.
    /// Assumes tuple is on top of stack. After execution:
    /// Stack: [tuple, valueN-1, ..., value1, value0] (value0 on top)
    pub fn emit_extract_tuple_fields(&mut self, num_fields: usize) {
        for i in (0..num_fields).rev() {
            // Copy tuple from depth (num_fields - 1 - i) to top of stack
            self.add_instruction(Instruction::Copy(num_fields - 1 - i));
            // Extract field i from the copied tuple
            self.add_instruction(Instruction::Get(i));
        }
    }

    /// Extracts specific fields from a tuple by field indices.
    /// Assumes tuple is on top of stack. After execution:
    /// Stack: [tuple, fieldN-1, ..., field1, field0] (field0 on top)
    pub fn emit_extract_specific_fields(&mut self, field_indices: &[usize]) {
        for (i, &field_index) in field_indices.iter().rev().enumerate() {
            // Copy tuple from depth i to top of stack
            self.add_instruction(Instruction::Copy(i));
            // Extract field from the copied tuple
            self.add_instruction(Instruction::Get(field_index));
        }
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
        let offset = (target_addr as isize) - (jump_addr as isize) - 1;
        self.instructions[jump_addr] = match &self.instructions[jump_addr] {
            Instruction::Jump(_) => Instruction::Jump(offset),
            Instruction::JumpIf(_) => Instruction::JumpIf(offset),
            _ => panic!("Cannot patch non-jump instruction"),
        };
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

    /// Emits a sequence of instructions for cleanup pattern (pop values and return NIL)
    pub fn emit_pattern_match_cleanup(&mut self, num_values_to_pop: usize) {
        for _ in 0..num_values_to_pop {
            self.add_instruction(Instruction::Pop);
        }
        self.add_instruction(Instruction::Pop); // Remove duplicated value
        self.add_instruction(Instruction::Tuple(TypeId::NIL));
    }

    /// Emits pattern match success sequence (store variables and return OK)
    pub fn emit_pattern_match_success(&mut self, assignments: &[(String, Type)]) {
        for (variable_name, _variable_type) in assignments {
            self.add_instruction(Instruction::Store(variable_name.clone()));
        }
        self.add_instruction(Instruction::Pop);
        self.add_instruction(Instruction::Tuple(TypeId::OK));
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
