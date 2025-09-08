use crate::bytecode::{Instruction, TypeId};

use super::{type_system::Type, Error};

/// Helper struct for managing instruction generation and jumps
pub struct InstructionBuilder {
    pub instructions: Vec<Instruction>,
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

    /// Emits a conditional jump placeholder and returns the address to patch later
    pub fn emit_jump_if_nil_placeholder(&mut self) -> usize {
        let addr = self.instructions.len();
        self.add_instruction(Instruction::JumpIfNil(0));
        addr
    }

    /// Emits a conditional jump placeholder and returns the address to patch later
    pub fn emit_jump_if_not_nil_placeholder(&mut self) -> usize {
        let addr = self.instructions.len();
        self.add_instruction(Instruction::JumpIfNotNil(0));
        addr
    }

    /// Patches a jump instruction to target the current instruction address
    pub fn patch_jump_to_here(&mut self, jump_addr: usize) {
        let target_addr = self.instructions.len();
        let offset = (target_addr as isize) - (jump_addr as isize) - 1;
        self.instructions[jump_addr] = match &self.instructions[jump_addr] {
            Instruction::Jump(_) => Instruction::Jump(offset),
            Instruction::JumpIfNil(_) => Instruction::JumpIfNil(offset),
            Instruction::JumpIfNotNil(_) => Instruction::JumpIfNotNil(offset),
            _ => panic!("Cannot patch non-jump instruction"),
        };
    }

    /// Patches a jump instruction to target a specific address
    pub fn patch_jump_to_addr(&mut self, jump_addr: usize, target_addr: usize) {
        let offset = (target_addr as isize) - (jump_addr as isize) - 1;
        self.instructions[jump_addr] = match &self.instructions[jump_addr] {
            Instruction::Jump(_) => Instruction::Jump(offset),
            Instruction::JumpIfNil(_) => Instruction::JumpIfNil(offset),
            Instruction::JumpIfNotNil(_) => Instruction::JumpIfNotNil(offset),
            _ => panic!("Cannot patch non-jump instruction"),
        };
    }

    /// Emits a conditional jump that immediately targets the fail address
    pub fn emit_jump_if_nil_to_addr(&mut self, fail_addr: usize) {
        let jump_addr = self.instructions.len();
        self.add_instruction(Instruction::JumpIfNil(0));
        let offset = (fail_addr as isize) - (jump_addr as isize) - 1;
        self.instructions[jump_addr] = Instruction::JumpIfNil(offset);
    }

    /// Emits runtime type check for tuple type
    pub fn emit_runtime_tuple_type_check(&mut self, type_id: TypeId, fail_addr: usize) {
        self.add_instruction(Instruction::Duplicate);
        self.add_instruction(Instruction::IsTuple(type_id));
        self.emit_jump_if_nil_to_addr(fail_addr);
        self.add_instruction(Instruction::Pop);
    }

    /// Emits a sequence of instructions for cleanup pattern (pop values and return NIL)
    pub fn emit_pattern_match_cleanup(&mut self, num_values_to_pop: usize) {
        for _ in 0..num_values_to_pop {
            self.add_instruction(Instruction::Pop);
        }
        self.add_instruction(Instruction::Pop); // Remove duplicated value
        self.add_instruction(Instruction::Tuple(TypeId::NIL, 0));
    }

    /// Emits pattern match success sequence (store variables and return OK)
    pub fn emit_pattern_match_success(&mut self, assignments: &[(String, Type)], define_variable: &mut dyn FnMut(&str, Type)) {
        for (variable_name, variable_type) in assignments {
            self.add_instruction(Instruction::Store(variable_name.clone()));
            define_variable(variable_name, variable_type.clone());
        }
        self.add_instruction(Instruction::Pop); // Remove duplicated value
        self.add_instruction(Instruction::Tuple(TypeId::OK, 0));
    }
}