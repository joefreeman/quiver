use crate::{
    ast,
    bytecode::{Constant, TypeId},
    types, vm,
};

use super::{
    Error,
    codegen::InstructionBuilder,
    typing::{Type, TypeContext},
};

pub struct PatternCompiler<'a> {
    pub codegen: &'a mut InstructionBuilder,
    pub type_context: &'a TypeContext<'a>,
    pub vm: &'a mut vm::VM,
}

impl<'a> PatternCompiler<'a> {
    pub fn new(
        codegen: &'a mut InstructionBuilder,
        type_context: &'a TypeContext<'a>,
        vm: &'a mut vm::VM,
    ) -> Self {
        Self {
            codegen,
            type_context,
            vm,
        }
    }

    pub fn compile_pattern_match(
        &mut self,
        pattern: &ast::Pattern,
        value_type: &Type,
        fail_addr: usize,
    ) -> Result<Option<Vec<(String, Type)>>, Error> {
        match pattern {
            ast::Pattern::Literal(literal) => {
                self.compile_literal_pattern_match(literal, fail_addr)
            }
            ast::Pattern::Identifier(name) => {
                self.compile_identifier_pattern_match(name, value_type)
            }
            ast::Pattern::Placeholder => {
                // Placeholder always matches, just consume the value
                Ok(Some(vec![]))
            }
            ast::Pattern::Tuple(tuple_pattern) => {
                self.compile_tuple_pattern_match(tuple_pattern, value_type, fail_addr)
            }
            ast::Pattern::Partial(field_names) => {
                self.compile_partial_pattern_match(field_names, value_type, fail_addr)
            }
            ast::Pattern::Star => self.compile_star_pattern_match(value_type, fail_addr),
        }
    }

    fn compile_literal_pattern_match(
        &mut self,
        literal: &ast::Literal,
        fail_addr: usize,
    ) -> Result<Option<Vec<(String, Type)>>, Error> {
        // Duplicate the value for comparison
        self.codegen
            .add_instruction(crate::bytecode::Instruction::Duplicate);

        // Push the literal onto the stack
        match literal {
            ast::Literal::Integer(int_val) => {
                let index = self.vm.register_constant(Constant::Integer(*int_val));
                self.codegen
                    .add_instruction(crate::bytecode::Instruction::Constant(index));
            }
            ast::Literal::Binary(bytes) => {
                let index = self.vm.register_constant(Constant::Binary(bytes.clone()));
                self.codegen
                    .add_instruction(crate::bytecode::Instruction::Constant(index));
            }
            ast::Literal::String(string) => {
                let bytes = string.as_bytes().to_vec();
                let index = self.vm.register_constant(Constant::Binary(bytes));
                self.codegen
                    .add_instruction(crate::bytecode::Instruction::Constant(index));
            }
        }

        // Compare using Equal
        self.codegen
            .add_instruction(crate::bytecode::Instruction::Equal(2));

        // If comparison fails (result is NIL), jump to fail address
        self.codegen
            .add_instruction(crate::bytecode::Instruction::Duplicate);
        self.codegen.emit_jump_if_nil_to_addr(fail_addr);

        // Pop comparison result
        self.codegen
            .add_instruction(crate::bytecode::Instruction::Pop);

        Ok(Some(vec![]))
    }

    fn compile_identifier_pattern_match(
        &mut self,
        name: &str,
        value_type: &Type,
    ) -> Result<Option<Vec<(String, Type)>>, Error> {
        // Determine the variable type
        let var_type = match value_type {
            Type::Resolved(_) => value_type.clone(),
            Type::Unresolved(types) if !types.is_empty() => value_type.clone(),
            _ => {
                // Fallback for unknown types
                Type::Unresolved(vec![
                    types::Type::Integer,
                    types::Type::Binary,
                    types::Type::Tuple(TypeId::NIL),
                ])
            }
        };

        Ok(Some(vec![(name.to_string(), var_type)]))
    }

    fn compile_tuple_pattern_match(
        &mut self,
        tuple_pattern: &ast::TuplePattern,
        value_type: &Type,
        fail_addr: usize,
    ) -> Result<Option<Vec<(String, Type)>>, Error> {
        match value_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                    // For resolved tuple types, check if the pattern structure matches
                    if let Some(ref tuple_name) = tuple_pattern.name {
                        // Named tuple pattern - check the exact tuple name matches
                        if tuple_info.0.as_ref() != Some(tuple_name) {
                            // Type names don't match - this is a compile-time failure
                            return Ok(None);
                        }
                    }

                    // Check field count matches
                    if tuple_pattern.fields.len() != tuple_info.1.len() {
                        // Field count mismatch - this is a compile-time failure
                        return Ok(None);
                    }

                    // Collect field types to avoid borrow checker issues
                    let field_types: Vec<types::Type> = tuple_info
                        .1
                        .iter()
                        .map(|(_, field_type)| field_type.clone())
                        .collect();

                    // Match each field in the tuple pattern and accumulate assignments
                    let mut all_assignments = Vec::new();

                    // Extract all field values from tuple
                    self.codegen
                        .emit_extract_tuple_fields(tuple_pattern.fields.len());
                    // Final stack: [tuple, valueN-1, ..., value1, value0] (value0 on top)

                    // Now create pending assignments for each field
                    // Stack is: [tuple, valueN-1, ..., value1, value0] (value0 on top)
                    // Process in forward order: field 0 first (takes value0), field 1 second (takes value1)
                    for (field_index, field) in tuple_pattern.fields.iter().enumerate() {
                        // Get the field type
                        let field_type = if let Some(field_type) = field_types.get(field_index) {
                            Type::Resolved(field_type.clone())
                        } else {
                            Type::Unresolved(vec![])
                        };

                        // Match the pattern against this field value (top of stack)
                        match self.compile_pattern_match(&field.pattern, &field_type, fail_addr)? {
                            Some(assignments) => {
                                all_assignments.extend(assignments);
                            }
                            None => {
                                // If any field can't match, the whole pattern can't match
                                return Ok(None);
                            }
                        }
                    }
                    Ok(Some(all_assignments))
                } else {
                    // Tuple type not found in registry - treat as compile-time failure
                    Ok(None)
                }
            }
            Type::Unresolved(types) => {
                // For unresolved types, check if we can find a matching tuple type
                if let Some(ref tuple_name) = tuple_pattern.name {
                    // Find the first matching tuple type from the unresolved types
                    let mut matching_type_id = None;
                    for tuple_type in types {
                        if let types::Type::Tuple(type_id) = tuple_type {
                            if let Some(tuple_info) =
                                self.type_context.type_registry.lookup_type(type_id)
                            {
                                if tuple_info.0.as_ref() == Some(tuple_name) {
                                    matching_type_id = Some(*type_id);
                                    break;
                                }
                            }
                        }
                    }

                    if let Some(expected_type_id) = matching_type_id {
                        // Emit runtime type check
                        self.codegen
                            .emit_runtime_tuple_type_check(expected_type_id, fail_addr);

                        // Now get the tuple info for field matching
                        if let Some(tuple_info) = self
                            .type_context
                            .type_registry
                            .lookup_type(&expected_type_id)
                        {
                            // Check field count matches
                            if tuple_pattern.fields.len() != tuple_info.1.len() {
                                // Length mismatch, jump to fail
                                let jump_addr = self.codegen.instructions.len();
                                self.codegen
                                    .add_instruction(crate::bytecode::Instruction::Jump(0));
                                let offset = (fail_addr as isize) - (jump_addr as isize) - 1;
                                self.codegen.instructions[jump_addr] =
                                    crate::bytecode::Instruction::Jump(offset);

                                // Field count mismatch with runtime check - still need to generate code but mark as might match
                                return Ok(Some(self.extract_bindings_from_pattern(
                                    &ast::Pattern::Tuple(tuple_pattern.clone()),
                                    Some(value_type),
                                )));
                            }

                            // Collect field types to avoid borrow checker issues
                            let field_types: Vec<types::Type> = tuple_info
                                .1
                                .iter()
                                .map(|(_, field_type)| field_type.clone())
                                .collect();

                            // Match each field in the tuple pattern and accumulate assignments
                            let mut all_assignments = Vec::new();

                            // Extract all field values from tuple
                            self.codegen
                                .emit_extract_tuple_fields(tuple_pattern.fields.len());

                            // Now create assignments for each field in forward order
                            for (field_index, field) in tuple_pattern.fields.iter().enumerate() {
                                // Get the field type
                                let field_type =
                                    if let Some(field_type) = field_types.get(field_index) {
                                        Type::Resolved(field_type.clone())
                                    } else {
                                        Type::Unresolved(vec![])
                                    };

                                // Match the pattern against this field value (top of stack)
                                match self.compile_pattern_match(
                                    &field.pattern,
                                    &field_type,
                                    fail_addr,
                                )? {
                                    Some(assignments) => {
                                        all_assignments.extend(assignments);
                                    }
                                    None => {
                                        // If any field can't match, the whole pattern can't match
                                        return Ok(None);
                                    }
                                }
                            }
                            return Ok(Some(all_assignments));
                        }
                    }
                }

                // For anonymous tuple patterns or when type not found - might match at runtime
                let jump_addr = self.codegen.instructions.len();
                self.codegen
                    .add_instruction(crate::bytecode::Instruction::Jump(0));
                let offset = (fail_addr as isize) - (jump_addr as isize) - 1;
                self.codegen.instructions[jump_addr] = crate::bytecode::Instruction::Jump(offset);
                Ok(Some(vec![]))
            }
            _ => {
                // Non-tuple type - this is a compile-time failure
                Ok(None)
            }
        }
    }

    fn compile_partial_pattern_match(
        &mut self,
        field_names: &[String],
        value_type: &Type,
        fail_addr: usize,
    ) -> Result<Option<Vec<(String, Type)>>, Error> {
        match value_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                // For resolved tuple types, extract the named fields
                let mut field_data = Vec::new();
                if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                    for field_name in field_names {
                        if let Some((field_index, (_, field_type))) = tuple_info
                            .1
                            .iter()
                            .enumerate()
                            .find(|(_, field)| field.0.as_deref() == Some(field_name))
                        {
                            field_data.push((field_name.clone(), field_index, field_type.clone()));
                        } else {
                            // Field not found - this is a compile-time failure for resolved types
                            return Ok(None);
                        }
                    }
                } else {
                    // Tuple type not found in registry - this is a compile-time failure
                    return Ok(None);
                }

                // Extract all field values with correct stack management
                let field_indices: Vec<usize> = field_data.iter().map(|(_, idx, _)| *idx).collect();
                self.codegen.emit_extract_specific_fields(&field_indices);

                // Create assignments for extracted fields (fields will be processed in forward order)
                let mut assignments = Vec::new();
                for (field_name, _, field_type) in field_data.iter().rev() {
                    assignments.push((field_name.clone(), Type::Resolved(field_type.clone())));
                }
                // Reverse assignments to match the order fields were requested in
                assignments.reverse();
                Ok(Some(assignments))
            }
            Type::Unresolved(types) => {
                // For unresolved types, enumerate through all possible tuple types
                // and use IsTuple instructions to handle each case
                let mut possible_matches = Vec::new();

                // Find all tuple types that have all the requested fields
                for tuple_type in types {
                    if let types::Type::Tuple(type_id) = tuple_type {
                        if let Some(tuple_info) =
                            self.type_context.type_registry.lookup_type(type_id)
                        {
                            let has_all_fields = field_names.iter().all(|field_name| {
                                tuple_info.1.iter().any(|(field_name_opt, _)| {
                                    field_name_opt.as_deref() == Some(field_name)
                                })
                            });
                            if has_all_fields {
                                possible_matches.push(*type_id);
                            }
                        }
                    }
                }

                if possible_matches.is_empty() {
                    // No tuple types have all the required fields - compile-time failure
                    return Ok(None);
                }

                // Generate code to check each possible tuple type
                let mut next_check_jumps = Vec::new();
                let mut success_jumps = Vec::new();

                for (i, type_id) in possible_matches.iter().enumerate() {
                    let is_last = i == possible_matches.len() - 1;

                    if !is_last {
                        // Duplicate value for type check with placeholder for next check
                        self.codegen
                            .add_instruction(crate::bytecode::Instruction::Duplicate);
                        self.codegen
                            .add_instruction(crate::bytecode::Instruction::IsTuple(*type_id));
                        let next_check_jump = self.codegen.emit_jump_if_nil_placeholder();
                        next_check_jumps.push(next_check_jump);
                        self.codegen
                            .add_instruction(crate::bytecode::Instruction::Pop);
                    } else {
                        // Last option - emit runtime type check with direct fail
                        self.codegen
                            .emit_runtime_tuple_type_check(*type_id, fail_addr);
                    }

                    // Extract fields for this tuple type
                    if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                        let mut field_data = Vec::new();
                        for field_name in field_names {
                            if let Some((field_index, (_, field_type))) = tuple_info
                                .1
                                .iter()
                                .enumerate()
                                .find(|(_, field)| field.0.as_deref() == Some(field_name))
                            {
                                field_data.push((
                                    field_name.clone(),
                                    field_index,
                                    field_type.clone(),
                                ));
                            }
                        }

                        // Extract specific fields from tuple
                        let field_indices: Vec<usize> =
                            field_data.iter().map(|(_, idx, _)| *idx).collect();
                        self.codegen.emit_extract_specific_fields(&field_indices);

                        // Jump to success (will be patched later)
                        let success_jump = self.codegen.emit_jump_placeholder();
                        success_jumps.push(success_jump);

                        // Patch the "next check" jump to point here (start of next iteration)
                        if i > 0 {
                            let next_check_addr = self.codegen.instructions.len();
                            let prev_jump_addr = next_check_jumps[i - 1];
                            self.codegen
                                .patch_jump_to_addr(prev_jump_addr, next_check_addr);
                        }
                    }
                }

                // Success address - all success jumps point here
                for jump_addr in success_jumps {
                    self.codegen.patch_jump_to_here(jump_addr);
                }

                // Create assignments based on the first matching type (they should all have the same field structure for the requested fields)
                let first_type_id = possible_matches[0];
                let mut assignments = Vec::new();
                if let Some(tuple_info) =
                    self.type_context.type_registry.lookup_type(&first_type_id)
                {
                    for field_name in field_names.iter().rev() {
                        if let Some((_, (_, field_type))) = tuple_info
                            .1
                            .iter()
                            .enumerate()
                            .find(|(_, field)| field.0.as_deref() == Some(field_name))
                        {
                            assignments
                                .push((field_name.clone(), Type::Resolved(field_type.clone())));
                        }
                    }
                }
                assignments.reverse();
                Ok(Some(assignments))
            }
            _ => {
                // Non-tuple type - this is a compile-time failure
                Ok(None)
            }
        }
    }

    fn compile_star_pattern_match(
        &mut self,
        value_type: &Type,
        fail_addr: usize,
    ) -> Result<Option<Vec<(String, Type)>>, Error> {
        match value_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                // Star pattern extracts all named fields from resolved tuple types
                let mut named_fields = Vec::new();
                if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                    for (field_index, (field_name, field_type)) in tuple_info.1.iter().enumerate() {
                        if let Some(name) = field_name {
                            named_fields.push((name.clone(), field_index, field_type.clone()));
                        }
                    }
                } else {
                    // Tuple type not found in registry - compile-time failure
                    return Ok(None);
                }

                // Extract all named fields with correct stack management
                let field_indices: Vec<usize> =
                    named_fields.iter().map(|(_, idx, _)| *idx).collect();
                self.codegen.emit_extract_specific_fields(&field_indices);

                // Create assignments for extracted fields (fields will be processed in forward order)
                let mut assignments = Vec::new();
                for (name, _, field_type) in named_fields.iter().rev() {
                    assignments.push((name.clone(), Type::Resolved(field_type.clone())));
                }
                // Reverse assignments to match the order fields were requested in
                assignments.reverse();
                Ok(Some(assignments))
            }
            Type::Unresolved(types) => {
                // For star patterns with unresolved types, enumerate through all possible tuple types
                // and use IsTuple instructions to handle each case
                let mut possible_matches = Vec::new();

                // Find all tuple types (star pattern extracts all named fields from any tuple)
                for tuple_type in types {
                    if let types::Type::Tuple(type_id) = tuple_type {
                        if self
                            .type_context
                            .type_registry
                            .lookup_type(type_id)
                            .is_some()
                        {
                            possible_matches.push(*type_id);
                        }
                    }
                }

                if possible_matches.is_empty() {
                    // No tuple types found - compile-time failure
                    return Ok(None);
                }

                // Generate code to check each possible tuple type
                let mut next_check_jumps = Vec::new();
                let mut success_jumps = Vec::new();

                for (i, type_id) in possible_matches.iter().enumerate() {
                    let is_last = i == possible_matches.len() - 1;

                    if !is_last {
                        // Duplicate value for type check with placeholder for next check
                        self.codegen
                            .add_instruction(crate::bytecode::Instruction::Duplicate);
                        self.codegen
                            .add_instruction(crate::bytecode::Instruction::IsTuple(*type_id));
                        let next_check_jump = self.codegen.emit_jump_if_nil_placeholder();
                        next_check_jumps.push(next_check_jump);
                        self.codegen
                            .add_instruction(crate::bytecode::Instruction::Pop);
                    } else {
                        // Last option - emit runtime type check with direct fail
                        self.codegen
                            .emit_runtime_tuple_type_check(*type_id, fail_addr);
                    }

                    // Extract all named fields for this tuple type
                    if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                        let mut named_fields = Vec::new();
                        for (field_index, (field_name, field_type)) in
                            tuple_info.1.iter().enumerate()
                        {
                            if let Some(name) = field_name {
                                named_fields.push((name.clone(), field_index, field_type.clone()));
                            }
                        }

                        // Extract named fields from tuple
                        let field_indices: Vec<usize> =
                            named_fields.iter().map(|(_, idx, _)| *idx).collect();
                        self.codegen.emit_extract_specific_fields(&field_indices);

                        // Jump to success (will be patched later)
                        let success_jump = self.codegen.emit_jump_placeholder();
                        success_jumps.push(success_jump);

                        // Patch the "next check" jump to point here (start of next iteration)
                        if i > 0 {
                            let next_check_addr = self.codegen.instructions.len();
                            let prev_jump_addr = next_check_jumps[i - 1];
                            self.codegen
                                .patch_jump_to_addr(prev_jump_addr, next_check_addr);
                        }
                    }
                }

                // Success address - all success jumps point here
                for jump_addr in success_jumps {
                    self.codegen.patch_jump_to_here(jump_addr);
                }

                // Create assignments based on the first matching type
                // For star patterns, we need to collect all named fields from whichever type matches
                let first_type_id = possible_matches[0];
                let mut assignments = Vec::new();
                if let Some(tuple_info) =
                    self.type_context.type_registry.lookup_type(&first_type_id)
                {
                    for (_, (field_name, field_type)) in tuple_info.1.iter().enumerate().rev() {
                        if let Some(name) = field_name {
                            assignments.push((name.clone(), Type::Resolved(field_type.clone())));
                        }
                    }
                }
                Ok(Some(assignments))
            }
            _ => {
                // Non-tuple type - this is a compile-time failure
                Ok(None)
            }
        }
    }

    // Extract bindings from patterns recursively, with type information when available
    fn extract_bindings_from_pattern(
        &self,
        pattern: &ast::Pattern,
        expected_type: Option<&Type>,
    ) -> Vec<(String, Type)> {
        let mut bindings = Vec::new();
        match pattern {
            ast::Pattern::Identifier(name) => {
                let var_type = expected_type.cloned().unwrap_or(Type::Unresolved(vec![]));
                bindings.push((name.clone(), var_type));
            }
            ast::Pattern::Literal(_) => {}  // No variables
            ast::Pattern::Placeholder => {} // No variables
            ast::Pattern::Tuple(tuple_pattern) => {
                // Try to get field types from expected tuple type
                let field_types =
                    if let Some(Type::Resolved(types::Type::Tuple(type_id))) = expected_type {
                        self.type_context
                            .type_registry
                            .lookup_type(type_id)
                            .map(|info| {
                                info.1
                                    .iter()
                                    .map(|(_, t)| Type::Resolved(t.clone()))
                                    .collect::<Vec<_>>()
                            })
                            .unwrap_or_default()
                    } else {
                        vec![]
                    };

                for (i, field) in tuple_pattern.fields.iter().enumerate() {
                    let field_type = field_types.get(i);
                    bindings.extend(self.extract_bindings_from_pattern(&field.pattern, field_type));
                }
            }
            ast::Pattern::Partial(field_names) => {
                for name in field_names {
                    bindings.push((name.clone(), Type::Unresolved(vec![])));
                }
            }
            ast::Pattern::Star => {} // No variables in star pattern itself
        }
        bindings
    }
}
