use crate::ast;
use quiver_core::{
    bytecode::Instruction,
    program::Program,
    types::{TupleLookup, Type},
};

use super::{Compiler, Error, RippleContext};

/// Represents a compiled field or spread value on the stack
#[derive(Debug)]
enum CompiledValue {
    Field {
        name: Option<String>,
        ty: Type,
        stack_offset: usize,
    },
    Spread {
        ty: Type,
        stack_offset: usize,
    },
}

impl CompiledValue {
    fn stack_offset(&self) -> usize {
        match self {
            Self::Field { stack_offset, .. } | Self::Spread { stack_offset, .. } => *stack_offset,
        }
    }
}

/// Information about a result variant (used when spreads create multiple possibilities)
#[derive(Debug, Clone)]
struct VariantInfo {
    fields: Vec<(Option<String>, Type)>,
    spread_type_ids: Vec<usize>,
}

/// Tracks where a final field value comes from
#[derive(Debug, Clone)]
enum FieldSource {
    CompiledField(usize),
    SpreadField { spread_idx: usize, field_idx: usize },
}

/// Compile all field values and spread sources, returning compiled values and stack size
fn compile_field_values(
    compiler: &mut Compiler,
    fields: &[ast::TupleField],
    ripple_context: Option<&RippleContext>,
) -> Result<(Vec<CompiledValue>, usize), Error> {
    let mut compiled_values = Vec::new();
    let mut stack_size = 0;

    for field in fields {
        match &field.value {
            ast::FieldValue::Chain(chain) => {
                // Pass ripple_context with incremented offset
                let ripple_context_value;
                let ripple_context_param = if let Some(ctx) = ripple_context {
                    ripple_context_value = RippleContext {
                        value_type: ctx.value_type.clone(),
                        stack_offset: ctx.stack_offset + stack_size,
                        owns_value: false,
                    };
                    Some(&ripple_context_value)
                } else {
                    None
                };
                let ty = compiler.compile_chain(chain.clone(), None, ripple_context_param)?;
                compiled_values.push(CompiledValue::Field {
                    name: field.name.clone(),
                    ty,
                    stack_offset: stack_size,
                });
                stack_size += 1;
            }
            ast::FieldValue::Spread(spread_source) => {
                let spread_type = match spread_source {
                    ast::SpreadSource::Ripple => {
                        let ctx = ripple_context.ok_or_else(|| {
                            Error::FeatureUnsupported(
                                "Ripple spread (~) requires a piped value".to_string(),
                            )
                        })?;
                        compiler
                            .codegen
                            .add_instruction(Instruction::Pick(ctx.stack_offset + stack_size));
                        ctx.value_type.clone()
                    }
                    ast::SpreadSource::Identifier(id) => {
                        let (var_type, var_index) =
                            super::scopes::lookup_variable(&compiler.scopes, id, &[])
                                .ok_or_else(|| Error::VariableUndefined(id.clone()))?;
                        compiler
                            .codegen
                            .add_instruction(Instruction::Load(var_index));
                        var_type
                    }
                    ast::SpreadSource::Chained => {
                        let ctx = ripple_context.ok_or_else(|| {
                            Error::FeatureUnsupported(
                                "Chained spread (...) requires a piped value".to_string(),
                            )
                        })?;
                        compiler
                            .codegen
                            .add_instruction(Instruction::Pick(ctx.stack_offset + stack_size));
                        ctx.value_type.clone()
                    }
                };

                compiled_values.push(CompiledValue::Spread {
                    ty: spread_type,
                    stack_offset: stack_size,
                });
                stack_size += 1;
            }
        }
    }

    Ok((compiled_values, stack_size))
}

/// Build all possible field variants from compiled values
/// When spreads have union types, this creates a cartesian product of possibilities
fn build_field_variants(
    fields: &[ast::TupleField],
    compiled_values: &[CompiledValue],
    program: &Program,
) -> Result<Vec<VariantInfo>, Error> {
    // Build index mappings for cleaner lookup
    let mut field_to_compiled: Vec<usize> = Vec::new();
    let mut spread_to_compiled: Vec<usize> = Vec::new();

    for (compiled_idx, compiled) in compiled_values.iter().enumerate() {
        match compiled {
            CompiledValue::Field { .. } => field_to_compiled.push(compiled_idx),
            CompiledValue::Spread { .. } => spread_to_compiled.push(compiled_idx),
        }
    }

    let mut variants = vec![VariantInfo {
        fields: Vec::new(),
        spread_type_ids: Vec::new(),
    }];

    // Process fields left to right, maintaining insertion order
    let mut field_count = 0;
    let mut spread_count = 0;

    for field in fields {
        match &field.value {
            ast::FieldValue::Chain(_) => {
                let compiled_idx = field_to_compiled[field_count];
                field_count += 1;

                if let CompiledValue::Field { name, ty, .. } = &compiled_values[compiled_idx] {
                    // Add this field to all variants
                    for variant in &mut variants {
                        // Check if this named field should replace an existing one
                        if let Some(field_name) = name
                            && let Some(pos) = variant
                                .fields
                                .iter()
                                .position(|(n, _)| n.as_ref() == Some(field_name))
                        {
                            variant.fields[pos].1 = ty.clone();
                            continue;
                        }
                        variant.fields.push((name.clone(), ty.clone()));
                    }
                }
            }
            ast::FieldValue::Spread(_) => {
                let compiled_idx = spread_to_compiled[spread_count];
                spread_count += 1;

                if let CompiledValue::Spread { ty, .. } = &compiled_values[compiled_idx] {
                    let spread_type_ids = ty.extract_tuples();

                    if spread_type_ids.is_empty() {
                        return Err(Error::TypeMismatch {
                            expected: "tuple".to_string(),
                            found: format!("{:?}", ty),
                        });
                    }

                    // For each existing variant, create new variants for each spread type
                    let mut new_variants = Vec::new();

                    for existing_variant in &variants {
                        for &spread_tuple_id in &spread_type_ids {
                            let spread_info = program.lookup_tuple(spread_tuple_id).ok_or(
                                Error::TupleNotInRegistry {
                                    tuple_id: spread_tuple_id,
                                },
                            )?;

                            let mut new_variant = existing_variant.clone();
                            new_variant.spread_type_ids.push(spread_tuple_id);

                            // Track where to insert new fields (at current position)
                            let insertion_point = new_variant.fields.len();
                            let mut inserted_count = 0;

                            // Add each field from the spread
                            for (field_name, field_type) in &spread_info.fields {
                                if let Some(name) = field_name
                                    && let Some(pos) = new_variant
                                        .fields
                                        .iter()
                                        .position(|(n, _)| n.as_ref() == Some(name))
                                {
                                    // Field exists - replace it
                                    new_variant.fields[pos].1 = field_type.clone();
                                    continue;
                                }
                                // Field doesn't exist - insert at insertion point
                                new_variant.fields.insert(
                                    insertion_point + inserted_count,
                                    (field_name.clone(), field_type.clone()),
                                );
                                inserted_count += 1;
                            }

                            new_variants.push(new_variant);
                        }
                    }

                    variants = new_variants;
                }
            }
        }
    }

    Ok(variants)
}

/// Build field sources mapping for a specific variant
/// Maps each final field to where its value comes from (compiled field or spread field)
fn build_field_sources_for_variant(
    variant: &VariantInfo,
    compiled_values: &[CompiledValue],
    program: &Program,
) -> Vec<Option<FieldSource>> {
    let mut field_sources: Vec<Option<FieldSource>> = vec![None; variant.fields.len()];
    let mut unnamed_sources: Vec<FieldSource> = Vec::new();

    // Collect all unnamed field sources in order
    let mut spread_variant_idx = 0;
    for (compiled_idx, compiled) in compiled_values.iter().enumerate() {
        match compiled {
            CompiledValue::Field { name: None, .. } => {
                unnamed_sources.push(FieldSource::CompiledField(compiled_idx));
            }
            CompiledValue::Spread { .. } => {
                let spread_tuple_id = variant.spread_type_ids[spread_variant_idx];
                let spread_info = program.lookup_tuple(spread_tuple_id).unwrap();

                for (field_idx, (spread_field_name, _)) in spread_info.fields.iter().enumerate() {
                    if spread_field_name.is_none() {
                        unnamed_sources.push(FieldSource::SpreadField {
                            spread_idx: compiled_idx,
                            field_idx,
                        });
                    }
                }
                spread_variant_idx += 1;
            }
            _ => {}
        }
    }

    // Map final fields to sources
    let mut unnamed_idx = 0;
    for (final_idx, (final_name, _)) in variant.fields.iter().enumerate() {
        if let Some(name) = final_name {
            // Named field - find the rightmost source
            let mut local_spread_idx = 0;
            for (compiled_idx, compiled) in compiled_values.iter().enumerate() {
                match compiled {
                    CompiledValue::Field {
                        name: field_name, ..
                    } => {
                        if field_name.as_ref() == Some(name) {
                            field_sources[final_idx] =
                                Some(FieldSource::CompiledField(compiled_idx));
                        }
                    }
                    CompiledValue::Spread { .. } => {
                        let spread_tuple_id = variant.spread_type_ids[local_spread_idx];
                        let spread_info = program.lookup_tuple(spread_tuple_id).unwrap();

                        for (field_idx, (spread_field_name, _)) in
                            spread_info.fields.iter().enumerate()
                        {
                            if spread_field_name.as_ref() == Some(name) {
                                field_sources[final_idx] = Some(FieldSource::SpreadField {
                                    spread_idx: compiled_idx,
                                    field_idx,
                                });
                                break;
                            }
                        }
                        local_spread_idx += 1;
                    }
                }
            }
        } else {
            // Unnamed field - use next from unnamed_sources
            if unnamed_idx < unnamed_sources.len() {
                field_sources[final_idx] = Some(unnamed_sources[unnamed_idx].clone());
                unnamed_idx += 1;
            }
        }
    }

    field_sources
}

/// Emit instructions to extract field values from the stack
fn emit_field_extraction_code(
    compiler: &mut Compiler,
    field_sources: &[Option<FieldSource>],
    compiled_values: &[CompiledValue],
    stack_size: usize,
) -> Result<(), Error> {
    let mut values_added = 0;
    for source in field_sources {
        let source = source.as_ref().ok_or_else(|| {
            Error::FeatureUnsupported("Failed to resolve field source in spread".to_string())
        })?;

        match source {
            FieldSource::CompiledField(idx) => {
                let depth = stack_size + values_added - 1 - compiled_values[*idx].stack_offset();
                compiler.codegen.add_instruction(Instruction::Pick(depth));
                values_added += 1;
            }
            FieldSource::SpreadField {
                spread_idx,
                field_idx,
            } => {
                let depth =
                    stack_size + values_added - 1 - compiled_values[*spread_idx].stack_offset();
                compiler.codegen.add_instruction(Instruction::Pick(depth));
                compiler
                    .codegen
                    .add_instruction(Instruction::Get(*field_idx));
                values_added += 1;
            }
        }
    }
    Ok(())
}

/// Emit instructions to clean up temporary values from the stack
fn emit_stack_cleanup_code(compiler: &mut Compiler, count: usize) {
    for _ in 0..count {
        compiler.codegen.add_instruction(Instruction::Rotate(2));
        compiler.codegen.add_instruction(Instruction::Pop);
    }
}

pub fn compile_tuple_with_spread(
    compiler: &mut Compiler,
    tuple_name: Option<String>,
    fields: Vec<ast::TupleField>,
    ripple_context: Option<&RippleContext>,
) -> Result<Type, Error> {
    // Validate: chained spreads require ripple context
    let has_chained_spread = fields.iter().any(|f| {
        matches!(
            &f.value,
            ast::FieldValue::Spread(ast::SpreadSource::Chained | ast::SpreadSource::Ripple)
        )
    });

    if has_chained_spread && ripple_context.is_none() {
        return Err(Error::FeatureUnsupported(
            "Chained spread (...) can only be used when a value is piped into the tuple"
                .to_string(),
        ));
    }

    // Step 1: Compile all field values and track their types
    let (compiled_values, stack_size) = compile_field_values(compiler, &fields, ripple_context)?;

    // Step 2: Resolve final field layout (handling union spreads)
    let variants = build_field_variants(&fields, &compiled_values, &compiler.program)?;

    // Step 3: Generate bytecode based on number of variants
    let result_type = if variants.len() == 1 {
        emit_single_variant_tuple(
            compiler,
            &variants[0],
            &compiled_values,
            stack_size,
            tuple_name.clone(),
        )?
    } else {
        emit_multi_variant_tuples(
            compiler,
            &variants,
            &compiled_values,
            stack_size,
            tuple_name.clone(),
        )?
    };

    // Clean up ripple value if we own it
    if let Some(ctx) = ripple_context
        && ctx.owns_value
    {
        compiler.codegen.add_instruction(Instruction::Rotate(2));
        compiler.codegen.add_instruction(Instruction::Pop);
    }

    Ok(result_type)
}

/// Emit bytecode for a single variant tuple (no branching needed)
fn emit_single_variant_tuple(
    compiler: &mut Compiler,
    variant: &VariantInfo,
    compiled_values: &[CompiledValue],
    stack_size: usize,
    tuple_name: Option<String>,
) -> Result<Type, Error> {
    let field_sources =
        build_field_sources_for_variant(variant, compiled_values, &compiler.program);
    emit_field_extraction_code(compiler, &field_sources, compiled_values, stack_size)?;

    let tuple_id = compiler
        .program
        .register_tuple(tuple_name, variant.fields.clone(), false);
    compiler
        .codegen
        .add_instruction(Instruction::Tuple(tuple_id));

    emit_stack_cleanup_code(compiler, stack_size);

    Ok(Type::Tuple(tuple_id))
}

/// Emit bytecode for multiple variant tuples (with type checking and branching)
fn emit_multi_variant_tuples(
    compiler: &mut Compiler,
    variants: &[VariantInfo],
    compiled_values: &[CompiledValue],
    stack_size: usize,
    tuple_name: Option<String>,
) -> Result<Type, Error> {
    let mut end_jumps = Vec::new();

    for (variant_idx, variant) in variants.iter().enumerate() {
        let is_last = variant_idx == variants.len() - 1;
        let field_sources =
            build_field_sources_for_variant(variant, compiled_values, &compiler.program);

        if !is_last {
            // Check if spreads match this variant's types
            let mut fail_jumps = Vec::new();

            for (spread_field_idx, spread_compiled_idx) in compiled_values
                .iter()
                .enumerate()
                .filter(|(_, cv)| matches!(cv, CompiledValue::Spread { .. }))
                .enumerate()
            {
                let spread_tuple_id = variant.spread_type_ids[spread_field_idx];
                let spread_stack_idx = compiled_values[spread_compiled_idx.0].stack_offset();

                // Pick the spread value and check its type
                let depth = stack_size - 1 - spread_stack_idx;
                compiler.codegen.add_instruction(Instruction::Pick(depth));
                // Register the tuple type as a check type
                let tuple_id = compiler.program.register_type(Type::Tuple(spread_tuple_id));
                compiler
                    .codegen
                    .add_instruction(Instruction::IsType(tuple_id));
                compiler.codegen.add_instruction(Instruction::Not);

                let fail_jump = compiler.codegen.emit_jump_if_placeholder();
                fail_jumps.push(fail_jump);
            }

            // All checks passed - construct this variant
            emit_field_extraction_code(compiler, &field_sources, compiled_values, stack_size)?;

            let tuple_id =
                compiler
                    .program
                    .register_tuple(tuple_name.clone(), variant.fields.clone(), false);
            compiler
                .codegen
                .add_instruction(Instruction::Tuple(tuple_id));

            end_jumps.push(compiler.codegen.emit_jump_placeholder());

            // Patch all fail jumps to here (next variant)
            for fail_jump in fail_jumps {
                compiler.codegen.patch_jump_to_here(fail_jump);
            }
        } else {
            // Last variant - no need to check, just construct it
            emit_field_extraction_code(compiler, &field_sources, compiled_values, stack_size)?;

            let tuple_id =
                compiler
                    .program
                    .register_tuple(tuple_name.clone(), variant.fields.clone(), false);
            compiler
                .codegen
                .add_instruction(Instruction::Tuple(tuple_id));
        }
    }

    // Patch all end jumps
    for jump in end_jumps {
        compiler.codegen.patch_jump_to_here(jump);
    }

    emit_stack_cleanup_code(compiler, stack_size);

    // Build union type from all variants
    let union_types: Vec<Type> = variants
        .iter()
        .map(|variant| {
            let tuple_id =
                compiler
                    .program
                    .register_tuple(tuple_name.clone(), variant.fields.clone(), false);
            Type::Tuple(tuple_id)
        })
        .collect();

    Ok(Type::from_types(union_types))
}
