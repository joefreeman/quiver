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


/// A set of bindings that can be created if certain runtime checks pass
#[derive(Debug, Clone)]
struct BindingSet {
    runtime_checks: Vec<(AccessPath, RuntimeCheck)>, // Checks that must pass for these bindings
    bindings: Vec<Binding>, // Variable bindings to create if checks pass
}

/// Result of analyzing a pattern
#[derive(Debug)]
struct PatternAnalysis {
    can_match: bool, // false if pattern cannot match at compile time
    binding_sets: Vec<BindingSet>, // Alternative sets of bindings, each with their own checks
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
        // Collect all unique variable names from all binding sets
        let mut all_bindings = Vec::new();
        let mut seen_names = std::collections::HashSet::new();

        // Collect bindings from all binding sets
        // All binding sets should define the same variables
        for binding_set in &analysis.binding_sets {
            for binding in &binding_set.bindings {
                if !seen_names.contains(&binding.name) {
                    seen_names.insert(binding.name.clone());
                    all_bindings.push((binding.name.clone(), binding.var_type.clone()));
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
                binding_sets: vec![BindingSet {
                    runtime_checks: vec![],
                    bindings: vec![],
                }],
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
            binding_sets: vec![BindingSet {
                runtime_checks: vec![(path, RuntimeCheck::Literal(literal))],
                bindings: vec![],
            }],
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
            binding_sets: vec![BindingSet {
                runtime_checks: vec![],
                bindings: vec![Binding {
                    name,
                    path,
                    var_type,
                }],
            }],
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
            binding_sets: vec![],
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

        // For each matching type, create binding sets
        for (type_id, field_mappings) in &matching_types {
            if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                // Start with a binding set for this type
                let mut base_checks = vec![];
                // Add type check if the value could be multiple different tuple types
                // (not just if this pattern matches multiple types)
                let num_tuple_types = value_type.iter().filter(|t| matches!(t, Type::Tuple(_))).count();
                if num_tuple_types > 1 {
                    // The value could be multiple tuple types, so we need a runtime check
                    base_checks.push((path.clone(), RuntimeCheck::TupleType(*type_id)));
                }
                
                let mut current_binding_sets = vec![BindingSet {
                    runtime_checks: base_checks,
                    bindings: vec![],
                }];

                // Process each field pattern
                for (pattern_idx, actual_idx) in field_mappings {
                    let field_pattern = &tuple_pattern.fields[*pattern_idx].pattern;
                    let field_type = TypeSet::resolved(tuple_info.1[*actual_idx].1.clone());
                    let field_path = AccessPath::Field(Box::new(path.clone()), *actual_idx);

                    // Recursively analyze the field pattern
                    let field_analysis = self.analyze_pattern(field_pattern, &field_type, field_path)?;
                    
                    if !field_analysis.can_match {
                        // This type variant can't match, skip it entirely
                        current_binding_sets.clear();
                        break;
                    }

                    // Combine field binding sets with current binding sets (cartesian product)
                    let mut new_binding_sets = vec![];
                    for current_set in &current_binding_sets {
                        for field_set in &field_analysis.binding_sets {
                            let mut combined_checks = current_set.runtime_checks.clone();
                            combined_checks.extend(field_set.runtime_checks.clone());
                            
                            let mut combined_bindings = current_set.bindings.clone();
                            combined_bindings.extend(field_set.bindings.clone());
                            
                            new_binding_sets.push(BindingSet {
                                runtime_checks: combined_checks,
                                bindings: combined_bindings,
                            });
                        }
                    }
                    current_binding_sets = new_binding_sets;
                }

                // Add all binding sets from this type to the analysis
                analysis.binding_sets.extend(current_binding_sets);
            }
        }
        
        // If we found matching types but ended up with no binding sets,
        // the pattern can't actually match
        if analysis.can_match && analysis.binding_sets.is_empty() && !matching_types.is_empty() {
            analysis.can_match = false;
        }

        // Special case: unresolved types  
        if value_type.len() == 0 && analysis.binding_sets.is_empty() {
            // Assume it could match at runtime
            analysis.can_match = true;

            let mut current_binding_set = BindingSet {
                runtime_checks: vec![],
                bindings: vec![],
            };

            // Collect bindings without type info
            for (i, field_pattern) in tuple_pattern.fields.iter().enumerate() {
                let field_path = AccessPath::Field(Box::new(path.clone()), i);
                let field_type = TypeSet::unresolved(vec![Type::Tuple(TypeId::NIL)])?;

                let field_analysis =
                    self.analyze_pattern(&field_pattern.pattern, &field_type, field_path)?;

                // Combine with field binding sets
                if !field_analysis.binding_sets.is_empty() {
                    // For simplicity, just take the first binding set
                    let field_set = &field_analysis.binding_sets[0];
                    current_binding_set.runtime_checks.extend(field_set.runtime_checks.clone());
                    current_binding_set.bindings.extend(field_set.bindings.clone());
                }
            }
            
            analysis.binding_sets.push(current_binding_set);
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
            binding_sets: vec![],
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

        // Create a binding set for each matching type
        for (type_id, field_indices) in &matching_types {
            let mut runtime_checks = vec![];
            
            // Add type check if there are multiple matching types
            if matching_types.len() > 1 {
                runtime_checks.push((path.clone(), RuntimeCheck::TupleType(*type_id)));
            }

            let mut bindings = Vec::new();
            
            if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                for (i, field_name) in field_names.iter().enumerate() {
                    let idx = field_indices[i];
                    let field_type = &tuple_info.1[idx].1;
                    let field_path = AccessPath::Field(Box::new(path.clone()), idx);
                    bindings.push(Binding {
                        name: field_name.clone(),
                        path: field_path,
                        var_type: TypeSet::resolved(field_type.clone()),
                    });
                }
            }

            analysis.binding_sets.push(BindingSet {
                runtime_checks,
                bindings,
            });
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
            binding_sets: vec![],
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

        // For star patterns with multiple types, we need to ensure consistent field ordering
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

        // Create a binding set for each tuple type
        for type_id in &tuple_types {
            let mut runtime_checks = vec![];
            
            // Add type check if there are multiple tuple types
            if tuple_types.len() > 1 {
                runtime_checks.push((path.clone(), RuntimeCheck::TupleType(*type_id)));
            }

            let mut bindings = Vec::new();
            
            if let Some(tuple_info) = self.type_context.type_registry.lookup_type(type_id) {
                // Extract fields in alphabetical order (from all_field_names)
                for field_name in &all_field_names {
                    // Find this field in the current type
                    if let Some((idx, (_, field_type))) = tuple_info
                        .1
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _))| name.as_ref() == Some(field_name))
                    {
                        let field_path = AccessPath::Field(Box::new(path.clone()), idx);
                        bindings.push(Binding {
                            name: field_name.clone(),
                            path: field_path,
                            var_type: TypeSet::resolved(field_type.clone()),
                        });
                    }
                }
            }

            analysis.binding_sets.push(BindingSet {
                runtime_checks,
                bindings,
            });
        }

        Ok(analysis)
    }

    /// Phase 3: Generate code based on analysis
    fn generate_pattern_code(
        &mut self,
        analysis: &PatternAnalysis,
        fail_addr: usize,
    ) -> Result<(), Error> {
        // Generate code for binding sets
        self.generate_binding_sets(&analysis.binding_sets, fail_addr)?;
        Ok(())
    }

    fn generate_binding_sets(
        &mut self,
        binding_sets: &[BindingSet],
        fail_addr: usize,
    ) -> Result<(), Error> {
        if binding_sets.is_empty() {
            return Ok(());
        }

        if binding_sets.len() == 1 {
            // Simple case: only one binding set
            let binding_set = &binding_sets[0];
            
            // Check all runtime checks
            for (path, check) in &binding_set.runtime_checks {
                self.generate_runtime_check(path, check, fail_addr)?;
            }
            
            // Extract all bindings in reverse order for the stack
            for (i, binding) in binding_set.bindings.iter().rev().enumerate() {
                self.generate_value_extraction_with_depth(&binding.path, i)?;
            }
        } else {
            // Multiple binding sets - try each one until one succeeds
            let mut end_jumps = Vec::new();
            let mut next_set_jumps = Vec::new();
            
            for (i, binding_set) in binding_sets.iter().enumerate() {
                // Patch jumps from previous iteration that should skip to this binding set
                for jump in next_set_jumps.drain(..) {
                    self.codegen.patch_jump_to_here(jump);
                }
                
                let is_last = i == binding_sets.len() - 1;
                
                // Check all runtime checks for this binding set
                for (path, check) in &binding_set.runtime_checks {
                    match check {
                        RuntimeCheck::TupleType(type_id) => {
                            self.generate_value_access(path)?;
                            self.codegen.add_instruction(Instruction::IsTuple(*type_id));
                            
                            if !is_last {
                                // If not this type, try next binding set
                                self.codegen.add_instruction(Instruction::Not);
                                let skip = self.codegen.emit_jump_if_placeholder();
                                next_set_jumps.push(skip);
                            } else {
                                // Last set - if check fails, fail the whole pattern
                                self.codegen.add_instruction(Instruction::Not);
                                let fail_jump = self.codegen.emit_jump_if_placeholder();
                                self.codegen.patch_jump_to_addr(fail_jump, fail_addr);
                            }
                        }
                        RuntimeCheck::Literal(_) => {
                            // For literal checks, use the standard runtime check
                            // which will jump to fail_addr if the check fails
                            self.generate_runtime_check(path, check, fail_addr)?;
                        }
                    }
                }
                
                // If we get here, all checks passed - extract bindings
                for (j, binding) in binding_set.bindings.iter().rev().enumerate() {
                    self.generate_value_extraction_with_depth(&binding.path, j)?;
                }
                
                // Jump to end (unless this is the last set)
                if !is_last {
                    let end_jump = self.codegen.emit_jump_placeholder();
                    end_jumps.push(end_jump);
                }
            }
            
            // Patch any remaining next_set_jumps to fail
            for jump in next_set_jumps {
                self.codegen.patch_jump_to_addr(jump, fail_addr);
            }
            
            // Patch all end jumps to here
            for end_jump in end_jumps {
                self.codegen.patch_jump_to_here(end_jump);
            }
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
