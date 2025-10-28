use crate::bytecode::{BuiltinInfo, Bytecode, Constant, Function, Instruction, TypeId};
use crate::types::{TupleTypeInfo, Type};
use std::collections::HashMap;

/// Recursively mark all tuple TypeIds referenced within a Type structure
fn mark_tuple_type_references(
    typ: &Type,
    used_tuple_types: &mut [bool],
    tuple_type_queue: &mut Vec<TypeId>,
) {
    match typ {
        Type::Tuple(type_id) | Type::Partial(type_id) => {
            if type_id.0 < used_tuple_types.len() && !used_tuple_types[type_id.0] {
                tuple_type_queue.push(*type_id);
            }
        }
        Type::Union(types_vec) => {
            for t in types_vec {
                mark_tuple_type_references(t, used_tuple_types, tuple_type_queue);
            }
        }
        Type::Callable(callable) => {
            mark_tuple_type_references(&callable.parameter, used_tuple_types, tuple_type_queue);
            mark_tuple_type_references(&callable.result, used_tuple_types, tuple_type_queue);
            mark_tuple_type_references(&callable.receive, used_tuple_types, tuple_type_queue);
        }
        Type::Process(process) => {
            if let Some(receive) = &process.receive {
                mark_tuple_type_references(receive, used_tuple_types, tuple_type_queue);
            }
            if let Some(returns) = &process.returns {
                mark_tuple_type_references(returns, used_tuple_types, tuple_type_queue);
            }
        }
        Type::Integer | Type::Binary | Type::Cycle(_) | Type::Variable(_) => {
            // These don't reference other types
        }
    }
}

/// Remap all tuple TypeIds within a Type structure according to the provided remap table
fn remap_tuple_type_ids(typ: Type, tuple_type_remap: &HashMap<TypeId, TypeId>) -> Type {
    match typ {
        Type::Tuple(old_id) => Type::Tuple(
            *tuple_type_remap
                .get(&old_id)
                .expect("Tuple type ID should be in remap table after tree shaking"),
        ),
        Type::Partial(old_id) => Type::Partial(
            *tuple_type_remap
                .get(&old_id)
                .expect("Partial type ID should be in remap table after tree shaking"),
        ),
        Type::Union(types_vec) => Type::Union(
            types_vec
                .into_iter()
                .map(|t| remap_tuple_type_ids(t, tuple_type_remap))
                .collect(),
        ),
        Type::Callable(callable) => Type::Callable(Box::new(crate::types::CallableType {
            parameter: remap_tuple_type_ids(callable.parameter, tuple_type_remap),
            result: remap_tuple_type_ids(callable.result, tuple_type_remap),
            receive: remap_tuple_type_ids(callable.receive, tuple_type_remap),
        })),
        Type::Process(process) => Type::Process(Box::new(crate::types::ProcessType {
            receive: process
                .receive
                .map(|t| Box::new(remap_tuple_type_ids(*t, tuple_type_remap))),
            returns: process
                .returns
                .map(|t| Box::new(remap_tuple_type_ids(*t, tuple_type_remap))),
        })),
        Type::Integer | Type::Binary | Type::Cycle(_) | Type::Variable(_) => typ,
    }
}

/// Perform tree shaking on program data to remove unused functions, constants, types, and builtins
/// Returns the collections needed to build an optimized Bytecode
pub fn tree_shake(
    functions: &[Function],
    constants: &[Constant],
    tuple_types: &[TupleTypeInfo],
    check_types: &[Type],
    builtins: &[BuiltinInfo],
    entry_fn: usize,
) -> Bytecode {
    // Mark phase: find all reachable functions, constants, types, and builtins
    let mut used_functions = vec![false; functions.len()];
    let mut used_constants = vec![false; constants.len()];
    let mut used_tuple_types = vec![false; tuple_types.len()];
    let mut used_check_types = vec![false; check_types.len()];
    let mut used_builtins = vec![false; builtins.len()];

    // Separate queues for functions, tuple types, and check types
    let mut function_queue = vec![entry_fn];
    let mut tuple_type_queue = Vec::new();
    let mut check_type_queue = Vec::new();

    // Always mark NIL and OK as used (they're built-in control flow types)
    if TypeId::NIL.0 < used_tuple_types.len() {
        tuple_type_queue.push(TypeId::NIL);
    }
    if TypeId::OK.0 < used_tuple_types.len() {
        tuple_type_queue.push(TypeId::OK);
    }

    // Process all queues until all are empty
    while !function_queue.is_empty() || !tuple_type_queue.is_empty() || !check_type_queue.is_empty()
    {
        // Process functions
        while let Some(fn_id) = function_queue.pop() {
            // Skip if already processed
            if fn_id >= used_functions.len() || used_functions[fn_id] {
                continue;
            }
            used_functions[fn_id] = true;

            // Get the function (if it exists)
            let Some(function) = functions.get(fn_id) else {
                continue;
            };

            // Scan instructions for references
            for instruction in &function.instructions {
                match instruction {
                    Instruction::Function(id) => {
                        function_queue.push(*id);
                    }
                    Instruction::Constant(id) => {
                        if *id < used_constants.len() {
                            used_constants[*id] = true;
                        }
                    }
                    Instruction::Tuple(type_id) => {
                        // Tuple instruction references tuple_types
                        if type_id.0 < used_tuple_types.len() && !used_tuple_types[type_id.0] {
                            tuple_type_queue.push(*type_id);
                        }
                    }
                    Instruction::IsType(type_id) => {
                        // IsType instruction references check_types
                        if type_id.0 < used_check_types.len() && !used_check_types[type_id.0] {
                            check_type_queue.push(*type_id);
                        }
                    }
                    Instruction::Builtin(id) => {
                        if *id < used_builtins.len() {
                            used_builtins[*id] = true;
                        }
                    }
                    _ => {}
                }
            }
        }

        // Process tuple types
        while let Some(type_id) = tuple_type_queue.pop() {
            // Skip if already processed
            if type_id.0 >= used_tuple_types.len() || used_tuple_types[type_id.0] {
                continue;
            }
            used_tuple_types[type_id.0] = true;

            // Get the tuple type info (if it exists)
            let Some(type_info) = tuple_types.get(type_id.0) else {
                continue;
            };

            // Recursively mark all tuple types referenced in this type's fields
            for (_, field_type) in &type_info.fields {
                mark_tuple_type_references(
                    field_type,
                    &mut used_tuple_types,
                    &mut tuple_type_queue,
                );
            }
        }

        // Process check types
        while let Some(type_id) = check_type_queue.pop() {
            // Skip if already processed
            if type_id.0 >= used_check_types.len() || used_check_types[type_id.0] {
                continue;
            }
            used_check_types[type_id.0] = true;

            // Get the check type (if it exists)
            let Some(check_type) = check_types.get(type_id.0) else {
                continue;
            };

            // Recursively mark all tuple types referenced in this check type
            mark_tuple_type_references(check_type, &mut used_tuple_types, &mut tuple_type_queue);
        }
    }

    // If nothing is used (shouldn't happen), return original without tree shaking
    if !used_functions.iter().any(|&used| used) {
        return Bytecode {
            constants: constants.to_vec(),
            functions: functions.to_vec(),
            builtins: builtins.to_vec(),
            entry: Some(entry_fn),
            tuple_types: tuple_types.to_vec(),
            types: check_types.to_vec(),
        };
    }

    // Sweep phase: build remapping tables and collect used items
    let mut function_remap = HashMap::new();
    let mut new_functions = Vec::new();

    for (old_id, function) in functions.iter().enumerate() {
        if used_functions[old_id] {
            function_remap.insert(old_id, new_functions.len());
            new_functions.push(function.clone());
        }
    }

    let mut constant_remap = HashMap::new();
    let mut new_constants = Vec::new();

    for (old_id, constant) in constants.iter().enumerate() {
        if used_constants[old_id] {
            constant_remap.insert(old_id, new_constants.len());
            new_constants.push(constant.clone());
        }
    }

    let mut builtin_remap = HashMap::new();
    let mut new_builtins = Vec::new();

    for (old_id, builtin) in builtins.iter().enumerate() {
        if used_builtins[old_id] {
            builtin_remap.insert(old_id, new_builtins.len());
            new_builtins.push(builtin.clone());
        }
    }

    let mut tuple_type_remap = HashMap::new();
    let mut new_tuple_types = Vec::new();

    for (old_id, type_info) in tuple_types.iter().enumerate() {
        if used_tuple_types[old_id] {
            tuple_type_remap.insert(TypeId(old_id), TypeId(new_tuple_types.len()));
            new_tuple_types.push(type_info.clone());
        }
    }

    // Remap TypeIds within the tuple type structures themselves
    let remapped_tuple_types = new_tuple_types
        .into_iter()
        .map(|mut type_info| {
            type_info.fields = type_info
                .fields
                .into_iter()
                .map(|(name, typ)| (name, remap_tuple_type_ids(typ, &tuple_type_remap)))
                .collect();
            type_info
        })
        .collect();

    let mut check_type_remap = HashMap::new();
    let mut new_check_types = Vec::new();

    for (old_id, check_type) in check_types.iter().enumerate() {
        if used_check_types[old_id] {
            check_type_remap.insert(TypeId(old_id), TypeId(new_check_types.len()));
            new_check_types.push(check_type.clone());
        }
    }

    // Remap TypeIds within the check types themselves
    let remapped_check_types = new_check_types
        .into_iter()
        .map(|typ| remap_tuple_type_ids(typ, &tuple_type_remap))
        .collect();

    // Remap phase: update function, constant, builtin, and type indices
    let remapped_functions = new_functions
        .into_iter()
        .map(|mut function| {
            function.instructions = function
                .instructions
                .into_iter()
                .map(|instruction| match instruction {
                    Instruction::Function(old_id) => Instruction::Function(
                        *function_remap
                            .get(&old_id)
                            .expect("Function ID should be in remap table after tree shaking"),
                    ),
                    Instruction::Constant(old_id) => Instruction::Constant(
                        *constant_remap
                            .get(&old_id)
                            .expect("Constant ID should be in remap table after tree shaking"),
                    ),
                    Instruction::Builtin(old_id) => Instruction::Builtin(
                        *builtin_remap
                            .get(&old_id)
                            .expect("Builtin ID should be in remap table after tree shaking"),
                    ),
                    Instruction::Tuple(old_type_id) => Instruction::Tuple(
                        *tuple_type_remap
                            .get(&old_type_id)
                            .expect("Tuple type ID should be in remap table after tree shaking"),
                    ),
                    Instruction::IsType(old_type_id) => Instruction::IsType(
                        *check_type_remap
                            .get(&old_type_id)
                            .expect("IsType type ID should be in remap table after tree shaking"),
                    ),
                    other => other,
                })
                .collect();
            function
        })
        .collect();

    // Remap entry point
    let new_entry = function_remap.get(&entry_fn).copied();

    Bytecode {
        constants: new_constants,
        functions: remapped_functions,
        builtins: new_builtins,
        entry: new_entry,
        tuple_types: remapped_tuple_types,
        types: remapped_check_types,
    }
}
