use crate::{
    ast,
    bytecode::{Constant, Instruction, TypeId},
    types::Type,
    vm::VM,
};

use super::{
    Error,
    codegen::InstructionBuilder,
    typing::{TypeContext, TypeSet},
};

/// Represents a check that must be performed at runtime
#[derive(Debug, Clone)]
enum RuntimeCheck {
    TupleType(TypeId),
    Literal(ast::Literal),
}

/// Represents a path to access a value within a data structure
#[derive(Debug, Clone)]
enum AccessPath {
    Root,                          // The value itself
    Field(Box<AccessPath>, usize), // Access field of a tuple
}

/// Information about a variable binding
#[derive(Debug, Clone)]
struct Binding {
    name: String,
    path: AccessPath,
    var_type: TypeSet,
}

/// Represents conditional bindings based on runtime type
#[derive(Debug, Clone)]
struct ConditionalBindings {
    type_id: TypeId,
    bindings: Vec<Binding>,
}

/// Result of analyzing a pattern
#[derive(Debug)]
struct PatternAnalysis {
    can_match: bool, // false if pattern cannot match at compile time
    runtime_checks: Vec<(AccessPath, RuntimeCheck)>, // Checks to perform at runtime
    bindings: Vec<Binding>, // Variable bindings to create (unconditional)
    conditional_bindings: Vec<ConditionalBindings>, // Bindings that depend on runtime type
}

pub struct PatternCompiler<'a> {
    pub codegen: &'a mut InstructionBuilder,
    pub type_context: &'a TypeContext<'a>,
    pub vm: &'a mut VM,
}

impl<'a> PatternCompiler<'a> {
    pub fn new(
        codegen: &'a mut InstructionBuilder,
        type_context: &'a TypeContext<'a>,
        vm: &'a mut VM,
    ) -> Self {
        Self {
            codegen,
            type_context,
            vm,
        }
    }

    /// Main entry point for pattern matching compilation
    pub fn compile_pattern_match(
        &mut self,
        pattern: &ast::Pattern,
        value_type: &TypeSet,
        fail_addr: usize,
    ) -> Result<Option<Vec<(String, TypeSet)>>, Error> {
        // Phase 1: Analyze the pattern
        let analysis = self.analyze_pattern(pattern, value_type, AccessPath::Root)?;

        // Phase 2: Check if match is possible
        if !analysis.can_match {
            return Ok(None);
        }

        // Phase 3: Generate code
        self.generate_pattern_code(&analysis, fail_addr)?;

        // Return bindings for caller to handle
        // For conditional bindings, we need to collect all possible bindings
        let mut all_bindings = Vec::new();

        // Add unconditional bindings
        for b in &analysis.bindings {
            all_bindings.push((b.name.clone(), b.var_type.clone()));
        }

        // Add conditional bindings - collect unique variable names from all branches
        // Since different types might have the same fields in different positions,
        // we need to ensure all variables are defined
        if !analysis.conditional_bindings.is_empty() {
            // Use the first conditional binding set for variable names
            // All branches should define the same variables
            if let Some(first_cond) = analysis.conditional_bindings.first() {
                for b in &first_cond.bindings {
                    all_bindings.push((b.name.clone(), b.var_type.clone()));
                }
            }
        }

        Ok(Some(all_bindings))
    }

    /// Phase 1: Analyze pattern and determine what's needed
    fn analyze_pattern(
        &self,
        pattern: &ast::Pattern,
        value_type: &TypeSet,
        path: AccessPath,
    ) -> Result<PatternAnalysis, Error> {
        match pattern {
            ast::Pattern::Literal(literal) => self.analyze_literal_pattern(literal.clone(), path),
            ast::Pattern::Identifier(name) => {
                self.analyze_identifier_pattern(name.clone(), value_type.clone(), path)
            }
            ast::Pattern::Placeholder => Ok(PatternAnalysis {
                can_match: true,
                runtime_checks: vec![],
                bindings: vec![],
                conditional_bindings: vec![],
            }),
            ast::Pattern::Tuple(tuple_pattern) => {
                self.analyze_tuple_pattern(tuple_pattern, value_type, path)
            }
            ast::Pattern::Partial(field_names) => {
                self.analyze_partial_pattern(field_names, value_type, path)
            }
            ast::Pattern::Star => self.analyze_star_pattern(value_type, path),
        }
    }

    fn analyze_literal_pattern(
        &self,
        literal: ast::Literal,
        path: AccessPath,
    ) -> Result<PatternAnalysis, Error> {
        Ok(PatternAnalysis {
            can_match: true,
            runtime_checks: vec![(path, RuntimeCheck::Literal(literal))],
            bindings: vec![],
            conditional_bindings: vec![],
        })
    }

    fn analyze_identifier_pattern(
        &self,
        name: String,
        value_type: TypeSet,
        path: AccessPath,
    ) -> Result<PatternAnalysis, Error> {
        let var_type = if value_type.len() > 0 {
            value_type
        } else {
            TypeSet::unresolved(vec![Type::Integer, Type::Binary, Type::Tuple(TypeId::NIL)])?
        };

        Ok(PatternAnalysis {
            can_match: true,
            runtime_checks: vec![],
            bindings: vec![Binding {
                name,
                path,
                var_type,
            }],
            conditional_bindings: vec![],
        })
    }

    fn analyze_tuple_pattern(
        &self,
        tuple_pattern: &ast::TuplePattern,
        value_type: &TypeSet,
        path: AccessPath,
    ) -> Result<PatternAnalysis, Error> {
        let mut analysis = PatternAnalysis {
            can_match: false,
            runtime_checks: vec![],
            bindings: vec![],
            conditional_bindings: vec![],
        };

        // Collect matching types with their field mappings
        let mut matching_types = Vec::new();

        for typ in value_type.iter() {
            if let Type::Tuple(type_id) = typ {
                if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                    // Check if this type matches the pattern
                    if let Some(ref pattern_name) = tuple_pattern.name {
                        if tuple_info.0.as_ref() != Some(pattern_name) {
                            continue; // Try next type
                        }
                    }

                    if tuple_pattern.fields.len() != tuple_info.1.len() {
                        continue; // Try next type
                    }

                    // This type could match - collect field mappings
                    let mut field_mappings = Vec::new();

                    // For named patterns like (x, y), we need to match by field name
                    for (pattern_idx, field_pattern) in tuple_pattern.fields.iter().enumerate() {
                        if let ast::Pattern::Identifier(field_name) = &field_pattern.pattern {
                            // Find the actual index of this field in the tuple type
                            let actual_idx = tuple_info
                                .1
                                .iter()
                                .position(|(name, _)| name.as_ref() == Some(field_name))
                                .unwrap_or(pattern_idx); // Fall back to positional if no name match
                            field_mappings.push((pattern_idx, actual_idx));
                        } else {
                            // For non-identifier patterns, use positional matching
                            field_mappings.push((pattern_idx, pattern_idx));
                        }
                    }

                    matching_types.push((*type_id, field_mappings));
                }
            }
        }

        if matching_types.is_empty() {
            return Ok(analysis); // No matching types
        }

        analysis.can_match = true;

        // If all matching types have the same field mapping, we can use unconditional bindings
        let all_same_mapping = if matching_types.len() > 1 {
            let first_mapping = &matching_types[0].1;
            matching_types[1..]
                .iter()
                .all(|(_, mapping)| mapping == first_mapping)
        } else {
            true
        };

        if all_same_mapping {
            // All types have the same field layout - use unconditional bindings
            let (type_id, field_mappings) = &matching_types[0];

            if value_type.len() > 1 {
                // Still need runtime check if there are other non-matching types
                analysis
                    .runtime_checks
                    .push((path.clone(), RuntimeCheck::TupleType(*type_id)));
            }

            if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                for (pattern_idx, actual_idx) in field_mappings {
                    let field_pattern = &tuple_pattern.fields[*pattern_idx].pattern;
                    let field_type = TypeSet::resolved(tuple_info.1[*actual_idx].1.clone());
                    let field_path = AccessPath::Field(Box::new(path.clone()), *actual_idx);

                    let field_analysis =
                        self.analyze_pattern(field_pattern, &field_type, field_path)?;

                    if !field_analysis.can_match {
                        analysis.can_match = false;
                        break;
                    }

                    analysis
                        .runtime_checks
                        .extend(field_analysis.runtime_checks);
                    analysis.bindings.extend(field_analysis.bindings);
                }
            }
        } else {
            // Different types have different field layouts - use conditional bindings
            for (type_id, field_mappings) in &matching_types {
                let mut type_bindings = Vec::new();

                if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                    for (pattern_idx, actual_idx) in field_mappings {
                        let field_pattern = &tuple_pattern.fields[*pattern_idx].pattern;

                        if let ast::Pattern::Identifier(field_name) = field_pattern {
                            let field_type = TypeSet::resolved(tuple_info.1[*actual_idx].1.clone());
                            let field_path = AccessPath::Field(Box::new(path.clone()), *actual_idx);

                            type_bindings.push(Binding {
                                name: field_name.clone(),
                                path: field_path,
                                var_type: field_type,
                            });
                        }
                    }
                }

                analysis.conditional_bindings.push(ConditionalBindings {
                    type_id: *type_id,
                    bindings: type_bindings,
                });
            }
        }

        // Special case: unresolved types
        if value_type.len() == 0 {
            // Assume it could match at runtime
            analysis.can_match = true;

            // Collect bindings without type info
            for (i, field_pattern) in tuple_pattern.fields.iter().enumerate() {
                let field_path = AccessPath::Field(Box::new(path.clone()), i);
                let field_type = TypeSet::unresolved(vec![Type::Tuple(TypeId::NIL)])?;

                let field_analysis =
                    self.analyze_pattern(&field_pattern.pattern, &field_type, field_path)?;

                analysis
                    .runtime_checks
                    .extend(field_analysis.runtime_checks);
                analysis.bindings.extend(field_analysis.bindings);
            }
        }

        Ok(analysis)
    }

    fn analyze_partial_pattern(
        &self,
        field_names: &[String],
        value_type: &TypeSet,
        path: AccessPath,
    ) -> Result<PatternAnalysis, Error> {
        let mut analysis = PatternAnalysis {
            can_match: false,
            runtime_checks: vec![],
            bindings: vec![],
            conditional_bindings: vec![],
        };

        // Collect all types that have all required fields
        let mut matching_types = Vec::new();

        for typ in value_type.iter() {
            if let Type::Tuple(type_id) = typ {
                if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                    let mut field_indices = Vec::new();
                    let mut all_fields_found = true;

                    for field_name in field_names {
                        if let Some((idx, _)) = tuple_info
                            .1
                            .iter()
                            .enumerate()
                            .find(|(_, (name, _))| name.as_deref() == Some(field_name))
                        {
                            field_indices.push(idx);
                        } else {
                            all_fields_found = false;
                            break;
                        }
                    }

                    if all_fields_found {
                        matching_types.push((*type_id, field_indices));
                    }
                }
            }
        }

        if matching_types.is_empty() {
            return Ok(analysis); // No matching types
        }

        analysis.can_match = true;

        // If there's only one possible type, we can use its field positions directly
        if matching_types.len() == 1 {
            let (type_id, field_indices) = &matching_types[0];

            if value_type.len() > 1 {
                // Still need runtime check if there are other non-matching types
                analysis
                    .runtime_checks
                    .push((path.clone(), RuntimeCheck::TupleType(*type_id)));
            }

            // Get tuple info for field types
            if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                for (i, field_name) in field_names.iter().enumerate() {
                    let idx = field_indices[i];
                    let field_type = &tuple_info.1[idx].1;
                    let field_path = AccessPath::Field(Box::new(path.clone()), idx);
                    analysis.bindings.push(Binding {
                        name: field_name.clone(),
                        path: field_path,
                        var_type: TypeSet::resolved(field_type.clone()),
                    });
                }
            }
        } else {
            // Multiple possible types - need conditional extraction based on runtime type
            for (type_id, field_indices) in &matching_types {
                let mut type_bindings = Vec::new();

                if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                    for (i, field_name) in field_names.iter().enumerate() {
                        let idx = field_indices[i];
                        let field_type = &tuple_info.1[idx].1;
                        let field_path = AccessPath::Field(Box::new(path.clone()), idx);
                        type_bindings.push(Binding {
                            name: field_name.clone(),
                            path: field_path,
                            var_type: TypeSet::resolved(field_type.clone()),
                        });
                    }
                }

                analysis.conditional_bindings.push(ConditionalBindings {
                    type_id: *type_id,
                    bindings: type_bindings,
                });
            }
        }

        Ok(analysis)
    }

    fn analyze_star_pattern(
        &self,
        value_type: &TypeSet,
        path: AccessPath,
    ) -> Result<PatternAnalysis, Error> {
        let mut analysis = PatternAnalysis {
            can_match: false,
            runtime_checks: vec![],
            bindings: vec![],
            conditional_bindings: vec![],
        };

        // Collect all tuple types
        let mut tuple_types = Vec::new();
        for typ in value_type.iter() {
            if let Type::Tuple(type_id) = typ {
                if self
                    .type_context
                    .type_registry
                    .lookup_type(type_id)
                    .is_some()
                {
                    tuple_types.push(*type_id);
                }
            }
        }

        if tuple_types.is_empty() {
            return Ok(analysis); // No tuple types
        }

        analysis.can_match = true;

        // If there's only one tuple type, extract its named fields
        if tuple_types.len() == 1 {
            let type_id = tuple_types[0];

            if value_type.len() > 1 {
                // Need runtime check if there are other non-tuple types
                analysis
                    .runtime_checks
                    .push((path.clone(), RuntimeCheck::TupleType(type_id)));
            }

            if let Some(tuple_info) = self.type_context.type_registry.lookup_type(&type_id) {
                for (idx, (name, field_type)) in tuple_info.1.iter().enumerate() {
                    if let Some(field_name) = name {
                        let field_path = AccessPath::Field(Box::new(path.clone()), idx);
                        analysis.bindings.push(Binding {
                            name: field_name.clone(),
                            path: field_path,
                            var_type: TypeSet::resolved(field_type.clone()),
                        });
                    }
                }
            }
        } else {
            // Multiple tuple types - need conditional extraction based on runtime type
            // First, collect all unique field names from all types
            let mut all_field_names = std::collections::BTreeSet::new();
            for type_id in &tuple_types {
                if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                    for (_, (name, _)) in tuple_info.1.iter().enumerate() {
                        if let Some(field_name) = name {
                            all_field_names.insert(field_name.clone());
                        }
                    }
                }
            }

            // Now create bindings for each type, extracting fields in alphabetical order
            for type_id in &tuple_types {
                let mut type_bindings = Vec::new();

                if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                    // For each field name in alphabetical order, find its index in this type
                    for field_name in &all_field_names {
                        if let Some((idx, (_, field_type))) = tuple_info
                            .1
                            .iter()
                            .enumerate()
                            .find(|(_, (name, _))| name.as_ref() == Some(field_name))
                        {
                            let field_path = AccessPath::Field(Box::new(path.clone()), idx);
                            type_bindings.push(Binding {
                                name: field_name.clone(),
                                path: field_path,
                                var_type: TypeSet::resolved(field_type.clone()),
                            });
                        }
                    }
                }

                analysis.conditional_bindings.push(ConditionalBindings {
                    type_id: *type_id,
                    bindings: type_bindings,
                });
            }
        }

        Ok(analysis)
    }

    /// Phase 3: Generate code based on analysis
    fn generate_pattern_code(
        &mut self,
        analysis: &PatternAnalysis,
        fail_addr: usize,
    ) -> Result<(), Error> {
        // Generate runtime checks
        for (path, check) in &analysis.runtime_checks {
            self.generate_runtime_check(path, check, fail_addr)?;
        }

        // Handle unconditional bindings
        if !analysis.bindings.is_empty() {
            // Extract in reverse order so they're on the stack in the right order for Store instructions
            for (i, binding) in analysis.bindings.iter().rev().enumerate() {
                self.generate_value_extraction_with_depth(&binding.path, i)?;
            }
        }

        // Handle conditional bindings if present
        if !analysis.conditional_bindings.is_empty() {
            self.generate_conditional_bindings(&analysis.conditional_bindings)?;
        }

        Ok(())
    }

    fn generate_conditional_bindings(
        &mut self,
        conditional_bindings: &[ConditionalBindings],
    ) -> Result<(), Error> {
        // We need to check the runtime type and extract fields based on it
        // The value is on top of the stack

        let mut end_jumps = Vec::new();

        for (i, cond_binding) in conditional_bindings.iter().enumerate() {
            // Check if it's this type
            self.codegen.add_instruction(Instruction::Duplicate);
            self.codegen
                .add_instruction(Instruction::IsTuple(cond_binding.type_id));

            // Jump to next check if false
            self.codegen.add_instruction(Instruction::Not);
            let next_check = self.codegen.emit_jump_if_placeholder();

            // Extract bindings for this type
            // The bindings need to be extracted in reverse order for the stack
            for (j, binding) in cond_binding.bindings.iter().rev().enumerate() {
                self.generate_value_extraction_with_depth(&binding.path, j)?;
            }

            // Jump to end after extraction
            let end_jump = self.codegen.emit_jump_placeholder();
            end_jumps.push(end_jump);

            // Patch the "next check" jump to here (for the next iteration)
            self.codegen.patch_jump_to_here(next_check);
        }

        // If we get here, none of the types matched - this shouldn't happen
        // but we need to handle it gracefully

        // Patch all end jumps to here
        for end_jump in end_jumps {
            self.codegen.patch_jump_to_here(end_jump);
        }

        Ok(())
    }

    fn generate_runtime_check(
        &mut self,
        path: &AccessPath,
        check: &RuntimeCheck,
        fail_addr: usize,
    ) -> Result<(), Error> {
        // First, get the value at the path onto the stack
        self.generate_value_access(path)?;

        match check {
            RuntimeCheck::TupleType(type_id) => {
                // Check if value is the expected tuple type
                self.codegen.add_instruction(Instruction::IsTuple(*type_id));
                self.codegen.add_instruction(Instruction::Not);
                let jump = self.codegen.emit_jump_if_placeholder();
                self.codegen.patch_jump_to_addr(jump, fail_addr);
            }
            RuntimeCheck::Literal(literal) => {
                // Push the literal for comparison
                match literal {
                    ast::Literal::Integer(val) => {
                        let idx = self.vm.register_constant(Constant::Integer(*val));
                        self.codegen.add_instruction(Instruction::Constant(idx));
                    }
                    ast::Literal::Binary(bytes) => {
                        let idx = self.vm.register_constant(Constant::Binary(bytes.clone()));
                        self.codegen.add_instruction(Instruction::Constant(idx));
                    }
                    ast::Literal::String(s) => {
                        let bytes = s.as_bytes().to_vec();
                        let idx = self.vm.register_constant(Constant::Binary(bytes));
                        self.codegen.add_instruction(Instruction::Constant(idx));
                    }
                }

                // Compare
                self.codegen.add_instruction(Instruction::Equal(2));

                // Jump if not equal (Equal returns NIL on failure)
                self.codegen.add_instruction(Instruction::Duplicate);
                self.codegen.emit_not_jump_if_to_addr(fail_addr);
                self.codegen.add_instruction(Instruction::Pop);
            }
        }

        Ok(())
    }

    fn generate_value_access(&mut self, path: &AccessPath) -> Result<(), Error> {
        match path {
            AccessPath::Root => {
                // Value is already on top of stack, duplicate it
                self.codegen.add_instruction(Instruction::Duplicate);
            }
            AccessPath::Field(parent, index) => {
                // First get parent value
                self.generate_value_access(parent)?;
                // Then extract the field
                self.codegen.add_instruction(Instruction::Get(*index));
            }
        }
        Ok(())
    }

    fn generate_value_extraction_with_depth(
        &mut self,
        path: &AccessPath,
        depth: usize,
    ) -> Result<(), Error> {
        // Extract value for binding, accounting for previously extracted values on stack
        match path {
            AccessPath::Root => {
                // Value is on stack, duplicate it for binding
                if depth == 0 {
                    self.codegen.add_instruction(Instruction::Duplicate);
                } else {
                    // Original value is buried under 'depth' extracted values
                    self.codegen.add_instruction(Instruction::Copy(depth));
                }
            }
            AccessPath::Field(parent, index) => {
                match &**parent {
                    AccessPath::Root => {
                        // The tuple is at position 'depth' on the stack
                        self.codegen.add_instruction(Instruction::Copy(depth));
                        self.codegen.add_instruction(Instruction::Get(*index));
                    }
                    AccessPath::Field(grandparent, parent_index) => {
                        // Nested field access - extract parent first, then child
                        // The root is still at position 'depth'
                        match &**grandparent {
                            AccessPath::Root => {
                                // Extract parent field from root
                                self.codegen.add_instruction(Instruction::Copy(depth));
                                self.codegen
                                    .add_instruction(Instruction::Get(*parent_index));
                                // Now extract the nested field
                                self.codegen.add_instruction(Instruction::Get(*index));
                            }
                            _ => {
                                // Even deeper nesting - fall back for now
                                self.generate_value_access(parent)?;
                                self.codegen.add_instruction(Instruction::Get(*index));
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
