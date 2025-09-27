use crate::{
    ast,
    bytecode::{Constant, Instruction, TypeId},
    types::Type,
    vm::VM,
};

use super::{Error, codegen::InstructionBuilder, typing::TypeContext};

/// Represents the certainty of a pattern match
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchCertainty {
    /// Pattern cannot possibly match the value type
    WontMatch,
    /// Pattern might match, requires runtime checks
    MightMatch,
    /// Pattern will definitely match, no runtime checks needed
    WillMatch,
}

/// Represents a check that must be performed at runtime
#[derive(Debug, Clone)]
enum RuntimeCheck {
    TupleType(TypeId),
    Literal(ast::Literal),
}

/// A requirement that must be satisfied for a pattern to match
#[derive(Debug, Clone)]
struct Requirement {
    path: AccessPath,
    check: RuntimeCheck,
}

/// Represents a path to access a value within a data structure
/// Empty vector means root, otherwise it's a sequence of field indices
type AccessPath = Vec<usize>;

/// Information about a variable binding
#[derive(Debug, Clone)]
struct Binding {
    name: String,
    path: AccessPath,
    var_type: Type,
}

/// A set of bindings that can be created if certain requirements are met
#[derive(Debug, Clone)]
struct BindingSet {
    requirements: Vec<Requirement>, // Requirements that must be satisfied
    bindings: Vec<Binding>,         // Variable bindings to create if requirements are met
}

// Pattern analysis returns a Vec<BindingSet>
// Empty vec means pattern cannot match
// Each BindingSet represents an alternative way the pattern could match

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

    /// Check if we need a runtime type check for the given value type
    fn needs_runtime_type_check(&self, value_type: &Type) -> bool {
        let tuple_types = value_type.extract_tuple_types();
        tuple_types.len() > 1
    }

    /// Find tuple types that match the given pattern
    fn find_matching_tuple_types(
        &self,
        tuple_pattern: &ast::TuplePattern,
        value_type: &Type,
    ) -> Result<Vec<(TypeId, Vec<(usize, usize)>)>, Error> {
        let mut matching_types = Vec::new();

        let types_to_check = match value_type {
            Type::Union(types) => types.as_slice(),
            single => std::slice::from_ref(single),
        };

        for typ in types_to_check {
            if let Type::Tuple(type_id) = typ {
                let tuple_info = self
                    .vm
                    .lookup_type(type_id)
                    .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

                // Check name match if specified
                if let Some(ref pattern_name) = tuple_pattern.name {
                    if tuple_info.0.as_ref() != Some(pattern_name) {
                        continue;
                    }
                }

                // Check field count
                if tuple_pattern.fields.len() != tuple_info.1.len() {
                    continue;
                }

                // Build field mappings
                let field_mappings = self.build_field_mappings(tuple_pattern, &tuple_info.1)?;
                matching_types.push((*type_id, field_mappings));
            }
        }

        Ok(matching_types)
    }

    /// Build field index mappings for pattern matching
    fn build_field_mappings(
        &self,
        tuple_pattern: &ast::TuplePattern,
        tuple_fields: &[(Option<String>, Type)],
    ) -> Result<Vec<(usize, usize)>, Error> {
        let mut mappings = Vec::new();

        for (pattern_idx, field_pattern) in tuple_pattern.fields.iter().enumerate() {
            // For identifier patterns, try to match by field name
            if let ast::Pattern::Identifier(field_name) = &field_pattern.pattern {
                match tuple_fields
                    .iter()
                    .position(|(name, _)| name.as_ref() == Some(field_name))
                {
                    Some(idx) => mappings.push((pattern_idx, idx)),
                    None => {
                        // If we have a named field pattern but can't find it, use positional
                        // This is actually valid for tuple patterns with identifier bindings
                        // that aren't field names, so we fall back to positional matching
                        mappings.push((pattern_idx, pattern_idx));
                    }
                }
            } else {
                // For non-identifier patterns, use positional matching
                mappings.push((pattern_idx, pattern_idx));
            }
        }

        Ok(mappings)
    }

    /// Main entry point for pattern matching compilation
    pub fn compile_pattern_match(
        &mut self,
        pattern: &ast::Pattern,
        value_type: &Type,
        fail_addr: usize,
    ) -> Result<(MatchCertainty, Vec<(String, Type)>), Error> {
        // Phase 1: Analyze the pattern
        let binding_sets = self.analyze_pattern(pattern, value_type, vec![])?;

        // Phase 2: Determine match certainty
        let certainty = if binding_sets.is_empty() {
            MatchCertainty::WontMatch
        } else if binding_sets.iter().any(|bs| bs.requirements.is_empty()) {
            // Any binding set with no requirements means at least one path will definitely match
            MatchCertainty::WillMatch
        } else {
            // All binding sets have requirements - might match
            MatchCertainty::MightMatch
        };

        // Phase 3: If match is impossible, return early
        if certainty == MatchCertainty::WontMatch {
            return Ok((MatchCertainty::WontMatch, Vec::new()));
        }

        // Phase 4: Generate code
        self.generate_pattern_code(&binding_sets, fail_addr)?;

        // Return bindings for caller to handle
        // Since all binding sets should define the same variables,
        // we can just take bindings from the first set
        let all_bindings = if let Some(first_set) = binding_sets.first() {
            first_set
                .bindings
                .iter()
                .map(|b| (b.name.clone(), b.var_type.clone()))
                .collect()
        } else {
            Vec::new()
        };

        Ok((certainty, all_bindings))
    }

    /// Phase 1: Analyze pattern and determine what's needed
    fn analyze_pattern(
        &self,
        pattern: &ast::Pattern,
        value_type: &Type,
        path: AccessPath,
    ) -> Result<Vec<BindingSet>, Error> {
        match pattern {
            ast::Pattern::Literal(literal) => self.analyze_literal_pattern(literal.clone(), path),
            ast::Pattern::Identifier(name) => {
                self.analyze_identifier_pattern(name.clone(), value_type.clone(), path)
            }
            ast::Pattern::Placeholder => Ok(vec![BindingSet {
                requirements: vec![],
                bindings: vec![],
            }]),
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
    ) -> Result<Vec<BindingSet>, Error> {
        Ok(vec![BindingSet {
            requirements: vec![Requirement {
                path,
                check: RuntimeCheck::Literal(literal),
            }],
            bindings: vec![],
        }])
    }

    fn analyze_identifier_pattern(
        &self,
        name: String,
        value_type: Type,
        path: AccessPath,
    ) -> Result<Vec<BindingSet>, Error> {
        // Empty Type should never happen - it indicates an internal error
        if matches!(&value_type, Type::Union(types) if types.is_empty()) {
            return Err(Error::InternalError {
                message: format!(
                    "analyze_identifier_pattern received empty Type for identifier '{}'",
                    name
                ),
            });
        }

        let var_type = value_type;

        Ok(vec![BindingSet {
            requirements: vec![],
            bindings: vec![Binding {
                name,
                path,
                var_type,
            }],
        }])
    }

    fn analyze_tuple_pattern(
        &self,
        tuple_pattern: &ast::TuplePattern,
        value_type: &Type,
        path: AccessPath,
    ) -> Result<Vec<BindingSet>, Error> {
        let mut binding_sets = vec![];

        // Collect matching types with their field mappings
        let matching_types = self.find_matching_tuple_types(tuple_pattern, value_type)?;

        if matching_types.is_empty() {
            return Ok(binding_sets); // No matching types
        }

        // For each matching type, create binding sets
        for (type_id, field_mappings) in &matching_types {
            // Get tuple info and extract field types upfront to avoid borrow issues
            let tuple_fields: Vec<Type> = {
                let tuple_info = self
                    .vm
                    .lookup_type(type_id)
                    .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;
                tuple_info.1.iter().map(|(_, t)| t.clone()).collect()
            };

            // Start with a binding set for this type
            let mut base_requirements = vec![];
            // Add type check if needed
            if self.needs_runtime_type_check(value_type) {
                base_requirements.push(Requirement {
                    path: path.clone(),
                    check: RuntimeCheck::TupleType(*type_id),
                });
            }

            let mut current_binding_sets = vec![BindingSet {
                requirements: base_requirements,
                bindings: vec![],
            }];

            // Process each field pattern
            for (pattern_idx, actual_idx) in field_mappings {
                let field_pattern = &tuple_pattern.fields[*pattern_idx].pattern;
                let raw_field_type = &tuple_fields[*actual_idx];

                // Resolve Type::Cycle references to the actual type
                let field_type = if let Type::Cycle(_) = raw_field_type {
                    // Type::Cycle in a recursive type refers back to the original type
                    // In the context of pattern matching, this should be the union of all variants
                    value_type.clone()
                } else {
                    raw_field_type.clone()
                };

                let mut field_path = path.clone();
                field_path.push(*actual_idx);

                // Recursively analyze the field pattern
                let field_binding_sets =
                    self.analyze_pattern(field_pattern, &field_type, field_path)?;

                if field_binding_sets.is_empty() {
                    // This type variant can't match, skip it entirely
                    current_binding_sets.clear();
                    break;
                }

                // Combine field binding sets with current binding sets (cartesian product)
                let mut new_binding_sets = vec![];
                for current_set in &current_binding_sets {
                    for field_set in &field_binding_sets {
                        let mut combined_requirements = current_set.requirements.clone();
                        combined_requirements.extend(field_set.requirements.clone());

                        let mut combined_bindings = current_set.bindings.clone();
                        combined_bindings.extend(field_set.bindings.clone());

                        new_binding_sets.push(BindingSet {
                            requirements: combined_requirements,
                            bindings: combined_bindings,
                        });
                    }
                }
                current_binding_sets = new_binding_sets;
            }

            // Add all binding sets from this type to the result
            binding_sets.extend(current_binding_sets);
        }

        // Empty Type should never happen - it indicates an internal error
        if matches!(&value_type, Type::Union(types) if types.is_empty()) {
            return Err(Error::InternalError {
                message: format!(
                    "analyze_tuple_pattern received empty Type for pattern: {:?}",
                    tuple_pattern
                ),
            });
        }

        Ok(binding_sets)
    }

    fn analyze_partial_pattern(
        &self,
        field_names: &[String],
        value_type: &Type,
        path: AccessPath,
    ) -> Result<Vec<BindingSet>, Error> {
        let mut binding_sets = vec![];

        // Find types that have all required fields
        let matching_types = self.find_types_with_fields(field_names, value_type)?;

        if matching_types.is_empty() {
            return Ok(binding_sets); // No matching types
        }

        // Create a binding set for each matching type
        for (type_id, field_indices) in &matching_types {
            let mut requirements = vec![];

            // Add type check if there are multiple matching types
            if matching_types.len() > 1 {
                requirements.push(Requirement {
                    path: path.clone(),
                    check: RuntimeCheck::TupleType(*type_id),
                });
            }

            let mut bindings = Vec::new();

            let tuple_info = self
                .vm
                .lookup_type(type_id)
                .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

            for (i, field_name) in field_names.iter().enumerate() {
                let idx = field_indices[i];
                let field_type = &tuple_info.1[idx].1;
                let mut field_path = path.clone();
                field_path.push(idx);
                bindings.push(Binding {
                    name: field_name.clone(),
                    path: field_path,
                    var_type: field_type.clone(),
                });
            }

            binding_sets.push(BindingSet {
                requirements,
                bindings,
            });
        }

        Ok(binding_sets)
    }

    fn analyze_star_pattern(
        &self,
        value_type: &Type,
        path: AccessPath,
    ) -> Result<Vec<BindingSet>, Error> {
        let mut binding_sets = vec![];

        // Collect all tuple types
        let tuple_types = value_type.extract_tuple_types();
        if tuple_types.is_empty() {
            return Ok(binding_sets);
        }

        // For star patterns with multiple types, we need to ensure consistent field ordering
        // First, collect all unique field names from all types
        let mut all_field_names = std::collections::BTreeSet::new();
        for type_id in &tuple_types {
            let tuple_info = self
                .vm
                .lookup_type(type_id)
                .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

            for (_, (name, _)) in tuple_info.1.iter().enumerate() {
                if let Some(field_name) = name {
                    all_field_names.insert(field_name.clone());
                }
            }
        }

        // Create a binding set for each tuple type
        for type_id in &tuple_types {
            let mut requirements = vec![];

            // Add type check if there are multiple tuple types
            if tuple_types.len() > 1 {
                requirements.push(Requirement {
                    path: path.clone(),
                    check: RuntimeCheck::TupleType(*type_id),
                });
            }

            let mut bindings = Vec::new();

            let tuple_info = self
                .vm
                .lookup_type(type_id)
                .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

            // Extract fields in alphabetical order (from all_field_names)
            for field_name in &all_field_names {
                // Find this field in the current type
                if let Some((idx, (_, field_type))) = tuple_info
                    .1
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name.as_ref() == Some(field_name))
                {
                    let mut field_path = path.clone();
                    field_path.push(idx);
                    bindings.push(Binding {
                        name: field_name.clone(),
                        path: field_path,
                        var_type: field_type.clone(),
                    });
                }
            }

            binding_sets.push(BindingSet {
                requirements,
                bindings,
            });
        }

        Ok(binding_sets)
    }

    /// Phase 3: Generate code based on analysis
    fn generate_pattern_code(
        &mut self,
        binding_sets: &[BindingSet],
        fail_addr: usize,
    ) -> Result<(), Error> {
        let mut end_jumps = Vec::new();
        let mut next_set_jumps = Vec::new();

        for (i, binding_set) in binding_sets.iter().enumerate() {
            // Patch jumps from previous iteration that should skip to this binding set
            for jump in next_set_jumps.drain(..) {
                self.codegen.patch_jump_to_here(jump);
            }

            let is_last = i == binding_sets.len() - 1;

            // Check all requirements for this binding set
            for requirement in &binding_set.requirements {
                match &requirement.check {
                    RuntimeCheck::TupleType(type_id) if !is_last => {
                        // Special handling for non-last tuple type checks:
                        // we want to try the next binding set if this check fails

                        // Generate value access duplicates the root and accesses the path
                        self.generate_value_access(&requirement.path)?;
                        // Stack: [root, value_at_path]

                        self.codegen.add_instruction(Instruction::IsTuple(*type_id));
                        // IsTuple consumes value and pushes boolean result
                        // Stack: [root, is_match]

                        self.codegen.add_instruction(Instruction::Not);
                        // Stack: [root, !is_match]

                        // If not this type, try next binding set
                        let skip = self.codegen.emit_jump_if_placeholder();
                        next_set_jumps.push(skip);
                        // If we jump, stack is [root] (JumpIf consumes the boolean)
                        // If we don't jump, stack is also [root]
                    }
                    _ => {
                        // For all other cases (literals and last tuple type check),
                        // use the standard runtime check which jumps to fail_addr
                        self.generate_runtime_check(
                            &requirement.path,
                            &requirement.check,
                            fail_addr,
                        )?;
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
                // After generate_value_access:
                // Stack: [root, value_at_path]

                // Check if value is the expected tuple type
                self.codegen.add_instruction(Instruction::IsTuple(*type_id));
                // IsTuple consumes value and pushes bool result
                // Stack: [root, bool]

                self.codegen.add_instruction(Instruction::Not);
                // Stack: [root, !bool]

                let jump = self.codegen.emit_jump_if_placeholder();
                self.codegen.patch_jump_to_addr(jump, fail_addr);
                // JumpIf consumes the boolean
                // Stack: [root]
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
                self.codegen.add_instruction(Instruction::Not);
                self.codegen.emit_jump_if_to_addr(fail_addr);
                self.codegen.add_instruction(Instruction::Pop);
            }
        }

        Ok(())
    }

    fn generate_value_access(&mut self, path: &AccessPath) -> Result<(), Error> {
        // Start with duplicating the root value
        self.codegen.add_instruction(Instruction::Duplicate);

        // Apply each field access in sequence
        for &index in path {
            self.codegen.add_instruction(Instruction::Get(index));
        }

        Ok(())
    }

    fn generate_value_extraction_with_depth(
        &mut self,
        path: &AccessPath,
        depth: usize,
    ) -> Result<(), Error> {
        if depth > 0 {
            self.codegen.add_instruction(Instruction::Copy(depth));
        }

        // Apply field accesses in order
        for &index in path {
            self.codegen.add_instruction(Instruction::Get(index));
        }

        Ok(())
    }

    /// Find tuple types that contain all specified fields
    fn find_types_with_fields(
        &self,
        field_names: &[String],
        value_type: &Type,
    ) -> Result<Vec<(TypeId, Vec<usize>)>, Error> {
        let mut matching_types = Vec::new();

        let types_to_check = match value_type {
            Type::Union(types) => types.as_slice(),
            single => std::slice::from_ref(single),
        };

        for typ in types_to_check {
            if let Type::Tuple(type_id) = typ {
                let tuple_info = self
                    .vm
                    .lookup_type(type_id)
                    .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

                if let Some(indices) = self.find_field_indices(field_names, &tuple_info.1) {
                    matching_types.push((*type_id, indices));
                }
            }
        }

        Ok(matching_types)
    }

    /// Find indices of specified fields in a tuple type
    fn find_field_indices(
        &self,
        field_names: &[String],
        tuple_fields: &[(Option<String>, Type)],
    ) -> Option<Vec<usize>> {
        let mut indices = Vec::new();

        for field_name in field_names {
            match tuple_fields
                .iter()
                .position(|(name, _)| name.as_deref() == Some(field_name))
            {
                Some(idx) => indices.push(idx),
                None => return None,
            }
        }

        Some(indices)
    }
}
