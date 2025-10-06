use super::*;

impl<'a> Compiler<'a> {
    pub(super) fn compile_tuple_merge(
        &mut self,
        merge_name: Option<String>,
        merge_fields: Vec<ast::TupleField>,
        source_type: Type,
    ) -> Result<Type, Error> {
        // Extract source tuple type(s)
        let source_type_ids = source_type.extract_tuple_types();

        if source_type_ids.is_empty() {
            return Err(Error::TypeMismatch {
                expected: "tuple".to_string(),
                found: crate::format::format_type(&self.program, &source_type),
            });
        }

        // For now, handle single type (unions would need more complex logic)
        let source_type_id = source_type_ids[0];
        let (source_name, source_fields) = self
            .program
            .lookup_type(&source_type_id)
            .ok_or_else(|| Error::TypeNotInRegistry {
                type_id: source_type_id,
            })?
            .clone();

        // If merge tuple has a name, check compatibility
        let needs_name_check = if let Some(ref merge_name) = merge_name {
            if let Some(ref source_name) = source_name {
                // Both have names - they must match
                if merge_name != source_name {
                    // Names don't match - return NIL
                    self.codegen.add_instruction(Instruction::Pop);
                    self.codegen
                        .add_instruction(Instruction::Tuple(TypeId::NIL));
                    return Ok(Type::nil());
                }
                true
            } else {
                // Merge has name but source doesn't - won't match
                self.codegen.add_instruction(Instruction::Pop);
                self.codegen
                    .add_instruction(Instruction::Tuple(TypeId::NIL));
                return Ok(Type::nil());
            }
        } else {
            false
        };

        // Check for nested merge name mismatches at compile time
        if self.has_nested_name_mismatch(&merge_fields, &source_fields)? {
            self.codegen.add_instruction(Instruction::Pop);
            self.codegen
                .add_instruction(Instruction::Tuple(TypeId::NIL));
            return Ok(Type::nil());
        }

        // Emit type check wrapper if needed
        let fail_jump = if needs_name_check {
            self.codegen.add_instruction(Instruction::Duplicate);
            self.codegen
                .add_instruction(Instruction::IsTuple(source_type_id));
            self.codegen.add_instruction(Instruction::Not);
            Some(self.codegen.emit_jump_if_placeholder())
        } else {
            None
        };

        // Perform the merge (common logic)
        self.compile_merge_fields(&merge_fields, &source_fields, source_type_id)?;

        // Handle success/fail paths based on name check
        if let Some(fail_jump) = fail_jump {
            let success_jump = self.codegen.emit_jump_placeholder();

            // Fail path: pop and return NIL
            self.codegen.patch_jump_to_here(fail_jump);
            self.codegen.add_instruction(Instruction::Pop);
            self.codegen
                .add_instruction(Instruction::Tuple(TypeId::NIL));

            self.codegen.patch_jump_to_here(success_jump);

            Ok(Type::from_types(vec![
                Type::Tuple(source_type_id),
                Type::nil(),
            ]))
        } else {
            Ok(Type::Tuple(source_type_id))
        }
    }

    fn has_nested_name_mismatch(
        &self,
        merge_fields: &[ast::TupleField],
        source_fields: &[(Option<String>, Type)],
    ) -> Result<bool, Error> {
        for (merge_idx, field) in merge_fields.iter().enumerate() {
            // Skip if not a nested merge
            if !Self::is_nested_merge(field) {
                continue;
            }

            // Extract nested tuple info using pattern matching
            let ast::FieldValue::Chain(chain) = &field.value else {
                continue;
            };
            let ast::Term::Tuple(nested_tuple) = &chain.terms[0] else {
                continue;
            };
            let Some(nested_name) = &nested_tuple.name else {
                continue;
            };

            // Get position
            let position = if let Some(ref field_name) = field.name {
                Self::find_field_position(source_fields, field_name)?
            } else {
                merge_idx
            };

            // Get field type
            let Some((_, field_type)) = source_fields.get(position) else {
                continue;
            };

            // Check name mismatch
            let field_type_ids = field_type.extract_tuple_types();
            if field_type_ids.is_empty() {
                continue;
            }

            let Some((field_tuple_name, _)) = self.program.lookup_type(&field_type_ids[0]) else {
                continue;
            };
            let Some(ftn) = field_tuple_name else {
                continue;
            };

            if nested_name != ftn {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn compile_merge_fields(
        &mut self,
        merge_fields: &[ast::TupleField],
        source_fields: &[(Option<String>, Type)],
        source_type_id: TypeId,
    ) -> Result<(), Error> {
        // Build update map: position -> merge field
        let mut update_map = HashMap::new();
        for (merge_idx, field) in merge_fields.iter().enumerate() {
            let position = if let Some(ref field_name) = field.name {
                Self::find_field_position(source_fields, field_name)?
            } else {
                merge_idx
            };
            update_map.insert(position, field);
        }

        // For each field in source (in order), either use new value or extract from source
        for (i, (_, field_type)) in source_fields.iter().enumerate() {
            if let Some(merge_field) = update_map.get(&i) {
                // Field is being updated
                if Self::is_nested_merge(merge_field) {
                    // Nested merge: extract field, recursively merge, swap
                    self.codegen.add_instruction(Instruction::Duplicate);
                    self.codegen.add_instruction(Instruction::Get(i));

                    // Extract the nested tuple from merge_field
                    let ast::FieldValue::Chain(chain) = &merge_field.value else {
                        unreachable!("is_nested_merge ensures this is a chain");
                    };
                    let ast::Term::Tuple(nested_tuple) = &chain.terms[0] else {
                        unreachable!("is_nested_merge ensures this is a tuple");
                    };

                    self.compile_tuple(
                        nested_tuple.name.clone(),
                        nested_tuple.fields.clone(),
                        Some(field_type.clone()),
                    )?;

                    self.codegen.add_instruction(Instruction::Swap);
                } else {
                    // Simple update: compile new value, swap
                    let ast::FieldValue::Chain(chain) = &merge_field.value else {
                        unreachable!("merge fields must have chain values");
                    };
                    self.compile_chain(chain.clone(), None)?;
                    self.codegen.add_instruction(Instruction::Swap);
                }
            } else {
                // Field not updated: extract from source
                self.codegen.add_instruction(Instruction::Duplicate);
                self.codegen.add_instruction(Instruction::Get(i));
                self.codegen.add_instruction(Instruction::Swap);
            }
        }

        // Pop the source tuple
        self.codegen.add_instruction(Instruction::Pop);

        // Build the result tuple
        self.codegen
            .add_instruction(Instruction::Tuple(source_type_id));

        Ok(())
    }

    fn is_nested_merge(field: &ast::TupleField) -> bool {
        match &field.value {
            ast::FieldValue::Chain(chain) => {
                chain.terms.len() == 1
                    && matches!(&chain.terms[0], ast::Term::Tuple(t)
                        if !Self::tuple_contains_ripple(&t.fields))
            }
            _ => false,
        }
    }

    fn find_field_position(
        source_fields: &[(Option<String>, Type)],
        field_name: &str,
    ) -> Result<usize, Error> {
        source_fields
            .iter()
            .position(|(name, _)| name.as_ref().map(|n| n == field_name).unwrap_or(false))
            .ok_or_else(|| Error::FieldNotFound {
                field_name: field_name.to_string(),
                type_name: "source tuple".to_string(),
            })
    }
}
