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
                if let Some(field_mappings) = self.check_tuple_match(tuple_pattern, *type_id)? {
                    matching_types.push((*type_id, field_mappings));
                }
            }
        }

        Ok(matching_types)
    }

    /// Check if a specific tuple type matches the pattern and return field mappings
    fn check_tuple_match(
        &self,
        tuple_pattern: &ast::TuplePattern,
        type_id: TypeId,
    ) -> Result<Option<Vec<(usize, usize)>>, Error> {
        let tuple_info = self
            .vm
            .lookup_type(&type_id)
            .ok_or_else(|| Error::TypeNotInRegistry { type_id })?;

        if tuple_pattern.name.as_ref() != tuple_info.0.as_ref()
            || tuple_pattern.fields.len() != tuple_info.1.len()
        {
            return Ok(None);
        }

        let mut field_mappings = Vec::new();
        for (pattern_idx, field_pattern) in tuple_pattern.fields.iter().enumerate() {
            let tuple_field = &tuple_info.1[pattern_idx];
            if field_pattern.name.as_ref() != tuple_field.0.as_ref() {
                return Ok(None);
            }

            // For now, use positional mapping (pattern_idx -> pattern_idx)
            // Later we might support reordering based on field names
            field_mappings.push((pattern_idx, pattern_idx));
        }

        Ok(Some(field_mappings))
    }

    /// Main entry point for pattern matching compilation
    pub fn compile_pattern_match(
        &mut self,
        pattern: &ast::Pattern,
        value_type: &Type,
        fail_addr: usize,
    ) -> Result<(MatchCertainty, Vec<(String, Type)>), Error> {
        let binding_sets = self.analyze_pattern(pattern, value_type, vec![])?;

        if binding_sets.is_empty() {
            return Ok((MatchCertainty::WontMatch, Vec::new()));
        }

        self.generate_pattern_code(&binding_sets, fail_addr)?;

        let certainty = if binding_sets.iter().any(|bs| bs.requirements.is_empty()) {
            // Any binding set with no requirements means at least one path will definitely match
            MatchCertainty::WillMatch
        } else {
            // All binding sets have requirements - might match
            MatchCertainty::MightMatch
        };

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
            ast::Pattern::Partial(partial_pattern) => {
                self.analyze_partial_pattern(partial_pattern, value_type, path)
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
            // Add runtime check if needed
            if value_type.extract_tuple_types().len() > 1 {
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
        partial_pattern: &ast::PartialPattern,
        value_type: &Type,
        path: AccessPath,
    ) -> Result<Vec<BindingSet>, Error> {
        let mut binding_sets = vec![];

        // Find types that have all required fields (and optionally matching tuple name)
        let matching_types = self.find_types_with_fields_and_name(
            &partial_pattern.fields,
            partial_pattern.name.as_ref(),
            value_type,
        )?;

        // Create a binding set for each matching type
        for (type_id, field_indices) in &matching_types {
            let mut requirements = vec![];

            if value_type.extract_tuple_types().len() > 1 {
                requirements.push(Requirement {
                    path: path.clone(),
                    check: RuntimeCheck::TupleType(*type_id),
                });
            }

            let tuple_info = self
                .vm
                .lookup_type(type_id)
                .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

            let mut bindings = Vec::new();
            for (i, field_name) in partial_pattern.fields.iter().enumerate() {
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
        // Collect all tuple types
        let tuple_types = value_type.extract_tuple_types();

        // Create a binding set for each tuple type
        let mut binding_sets = vec![];
        for type_id in &tuple_types {
            let tuple_info = self
                .vm
                .lookup_type(type_id)
                .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

            // Add type check if there are multiple tuple types
            let mut requirements = vec![];
            if tuple_types.len() > 1 {
                requirements.push(Requirement {
                    path: path.clone(),
                    check: RuntimeCheck::TupleType(*type_id),
                });
            }

            let mut bindings = Vec::new();
            for (idx, (name, field_type)) in tuple_info.1.iter().enumerate() {
                if let Some(field_name) = name {
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
                self.generate_value_access(&requirement.path)?;

                match &requirement.check {
                    RuntimeCheck::TupleType(type_id) => {
                        self.codegen.add_instruction(Instruction::IsTuple(*type_id));
                    }
                    RuntimeCheck::Literal(literal) => {
                        match literal {
                            ast::Literal::Integer(val) => {
                                let idx = self.vm.register_constant(Constant::Integer(*val));
                                self.codegen.add_instruction(Instruction::Constant(idx));
                            }
                            ast::Literal::Binary(bytes) => {
                                let idx =
                                    self.vm.register_constant(Constant::Binary(bytes.clone()));
                                self.codegen.add_instruction(Instruction::Constant(idx));
                            }
                            ast::Literal::String(s) => {
                                let bytes = s.as_bytes().to_vec();
                                let idx = self.vm.register_constant(Constant::Binary(bytes));
                                self.codegen.add_instruction(Instruction::Constant(idx));
                            }
                        }

                        self.codegen.add_instruction(Instruction::Equal(2));
                    }
                }

                self.codegen.add_instruction(Instruction::Not);
                if is_last {
                    self.codegen.emit_jump_if_to_addr(fail_addr);
                } else {
                    let skip = self.codegen.emit_jump_if_placeholder();
                    next_set_jumps.push(skip);
                }
            }

            // If we get here, all checks passed - extract bindings
            for binding in binding_set.bindings.iter() {
                self.generate_value_access(&binding.path)?;
                self.codegen
                    .add_instruction(Instruction::Store(binding.name.clone()));
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

    fn generate_value_access(&mut self, path: &AccessPath) -> Result<(), Error> {
        // Start with duplicating the root value
        self.codegen.add_instruction(Instruction::Duplicate);

        // Apply each field access in sequence
        for &index in path {
            self.codegen.add_instruction(Instruction::Get(index));
        }

        Ok(())
    }

    fn find_types_with_fields_and_name(
        &self,
        field_names: &[String],
        tuple_name: Option<&String>,
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

                // Check if tuple name matches (if specified)
                if let Some(expected_name) = tuple_name {
                    if tuple_info.0.as_ref() != Some(expected_name) {
                        continue; // Skip this type if name doesn't match
                    }
                }

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
