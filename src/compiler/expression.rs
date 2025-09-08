use std::collections::HashMap;

use crate::{ast, bytecode::TypeId};

use super::{
    Error,
    codegen::InstructionBuilder,
    typing::{Type, narrow_types},
};

pub struct ExpressionCompiler<'a> {
    pub codegen: &'a mut InstructionBuilder,
    pub scopes: &'a mut Vec<HashMap<String, Type>>,
}

impl<'a> ExpressionCompiler<'a> {
    pub fn new(
        codegen: &'a mut InstructionBuilder,
        scopes: &'a mut Vec<HashMap<String, Type>>,
    ) -> Self {
        Self { codegen, scopes }
    }

    pub fn compile_block(
        &mut self,
        block: ast::Block,
        _parameter_type: Type,
    ) -> Result<Type, Error> {
        let mut next_branch_jumps = Vec::new();
        let mut end_jumps = Vec::new();
        let mut branch_types = Vec::new();
        let mut branch_starts = Vec::new();

        self.codegen
            .add_instruction(crate::bytecode::Instruction::Enter);
        self.scopes.push(HashMap::new());

        for (i, branch) in block.branches.iter().enumerate() {
            let is_last_branch = i == block.branches.len() - 1;

            branch_starts.push(self.codegen.instructions.len());

            if i > 0 {
                self.codegen
                    .add_instruction(crate::bytecode::Instruction::Pop);
                self.codegen
                    .add_instruction(crate::bytecode::Instruction::Reset);
                self.scopes.last_mut().unwrap().clear();
            }

            // This would need to be implemented with proper expression compilation
            let condition_type = Type::Resolved(crate::types::Type::Integer); // Placeholder

            // If condition is compile-time NIL (won't match), skip this branch entirely
            if condition_type == Type::Resolved(crate::types::Type::Tuple(TypeId::NIL)) {
                // Don't add to branch_types, continue to next branch
                continue;
            }

            if branch.consequence.is_some() {
                // Branch has a consequence - compile it
                self.codegen
                    .add_instruction(crate::bytecode::Instruction::Duplicate);
                let next_branch_jump = self.codegen.emit_jump_if_nil_placeholder();
                next_branch_jumps.push((next_branch_jump, i + 1));
                self.codegen
                    .add_instruction(crate::bytecode::Instruction::Pop);

                // This would need proper consequence compilation
                let consequence_type = Type::Resolved(crate::types::Type::Integer); // Placeholder
                branch_types.push(consequence_type);
            } else {
                // No consequence - use condition type
                branch_types.push(condition_type);
            }

            if !is_last_branch {
                if branch.consequence.is_some() {
                    let end_jump = self.codegen.emit_jump_placeholder();
                    end_jumps.push(end_jump);
                } else {
                    self.codegen
                        .add_instruction(crate::bytecode::Instruction::Duplicate);
                    let success_jump = self.codegen.emit_jump_if_not_nil_placeholder();
                    end_jumps.push(success_jump);
                }
            }
        }

        let end_addr = self.codegen.instructions.len();

        self.codegen
            .add_instruction(crate::bytecode::Instruction::Exit);
        self.scopes.pop();

        for (jump_addr, next_branch_idx) in next_branch_jumps {
            let target_addr = if next_branch_idx >= branch_starts.len() {
                end_addr
            } else {
                branch_starts[next_branch_idx]
            };
            self.codegen.patch_jump_to_addr(jump_addr, target_addr);
        }

        for jump_addr in end_jumps {
            self.codegen.patch_jump_to_addr(jump_addr, end_addr);
        }

        narrow_types(branch_types)
    }

    pub fn compile_expression(
        &mut self,
        expression: ast::Expression,
        _parameter_type: Type,
    ) -> Result<Type, Error> {
        let mut last_type = None;
        let mut end_jumps = Vec::new();

        for (i, term) in expression.terms.iter().enumerate() {
            last_type = Some(match term {
                ast::Term::Assignment {
                    pattern: _,
                    value: _,
                } => {
                    // This would need proper assignment compilation
                    Type::Resolved(crate::types::Type::Integer) // Placeholder
                }
                ast::Term::Chain(_chain) => {
                    // This would need proper chain compilation
                    Type::Resolved(crate::types::Type::Integer) // Placeholder
                }
            });

            // If last_type is NIL, subsequent terms are unreachable - break early
            if let Some(Type::Resolved(crate::types::Type::Tuple(TypeId::NIL))) = last_type {
                break;
            }

            if i < expression.terms.len() - 1 {
                self.codegen
                    .add_instruction(crate::bytecode::Instruction::Duplicate);
                let end_jump = self.codegen.emit_jump_if_nil_placeholder();
                end_jumps.push(end_jump);
                self.codegen
                    .add_instruction(crate::bytecode::Instruction::Pop);
            }
        }

        let end_addr = self.codegen.instructions.len();
        for jump_addr in end_jumps {
            self.codegen.patch_jump_to_addr(jump_addr, end_addr);
        }

        Ok(last_type.unwrap_or(Type::Resolved(crate::types::Type::Tuple(TypeId::NIL))))
    }
}
