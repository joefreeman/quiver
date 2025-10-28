use crate::bytecode::{BuiltinInfo, Bytecode, Constant, Function, Instruction};
use crate::types::{NIL, OK, TupleTypeInfo, Type};
use std::collections::HashMap;

fn mark_tuple_references(typ: &Type, used_tuples: &mut [bool], tuple_queue: &mut Vec<usize>) {
    match typ {
        Type::Tuple(type_id) | Type::Partial(type_id) => {
            if *type_id < used_tuples.len() && !used_tuples[*type_id] {
                tuple_queue.push(*type_id);
            }
        }
        Type::Union(types_vec) => {
            for t in types_vec {
                mark_tuple_references(t, used_tuples, tuple_queue);
            }
        }
        Type::Callable(callable) => {
            mark_tuple_references(&callable.parameter, used_tuples, tuple_queue);
            mark_tuple_references(&callable.result, used_tuples, tuple_queue);
            mark_tuple_references(&callable.receive, used_tuples, tuple_queue);
        }
        Type::Process(process) => {
            if let Some(receive) = &process.receive {
                mark_tuple_references(receive, used_tuples, tuple_queue);
            }
            if let Some(returns) = &process.returns {
                mark_tuple_references(returns, used_tuples, tuple_queue);
            }
        }
        Type::Integer | Type::Binary | Type::Cycle(_) | Type::Variable(_) => {
            // These don't reference other types
        }
    }
}

fn remap_tuple_ids(typ: Type, tuple_remap: &HashMap<usize, usize>) -> Type {
    match typ {
        Type::Tuple(old_id) => Type::Tuple(
            *tuple_remap
                .get(&old_id)
                .expect("Tuple type ID should be in remap table after tree shaking"),
        ),
        Type::Partial(old_id) => Type::Partial(
            *tuple_remap
                .get(&old_id)
                .expect("Partial type ID should be in remap table after tree shaking"),
        ),
        Type::Union(types_vec) => Type::Union(
            types_vec
                .into_iter()
                .map(|t| remap_tuple_ids(t, tuple_remap))
                .collect(),
        ),
        Type::Callable(callable) => Type::Callable(Box::new(crate::types::CallableType {
            parameter: remap_tuple_ids(callable.parameter, tuple_remap),
            result: remap_tuple_ids(callable.result, tuple_remap),
            receive: remap_tuple_ids(callable.receive, tuple_remap),
        })),
        Type::Process(process) => Type::Process(Box::new(crate::types::ProcessType {
            receive: process
                .receive
                .map(|t| Box::new(remap_tuple_ids(*t, tuple_remap))),
            returns: process
                .returns
                .map(|t| Box::new(remap_tuple_ids(*t, tuple_remap))),
        })),
        Type::Integer | Type::Binary | Type::Cycle(_) | Type::Variable(_) => typ,
    }
}

/// Perform tree shaking on program data to remove unused functions, constants, types, and builtins
/// Returns the collections needed to build an optimized Bytecode
pub fn tree_shake(
    functions: &[Function],
    constants: &[Constant],
    tuples: &[TupleTypeInfo],
    types: &[Type],
    builtins: &[BuiltinInfo],
    entry_fn: usize,
) -> Bytecode {
    // Mark phase: find all reachable functions, constants, types, and builtins
    let mut used_functions = vec![false; functions.len()];
    let mut used_constants = vec![false; constants.len()];
    let mut used_tuples = vec![false; tuples.len()];
    let mut used_types = vec![false; types.len()];
    let mut used_builtins = vec![false; builtins.len()];

    // Separate queues for functions, tuple types, and check types
    let mut function_queue = vec![entry_fn];
    let mut tuple_queue = Vec::new();
    let mut type_queue = Vec::new();

    // Always mark NIL and OK as used (they're built-in control flow types)
    if NIL < used_tuples.len() {
        tuple_queue.push(NIL);
    }
    if OK < used_tuples.len() {
        tuple_queue.push(OK);
    }

    // Process all queues until all are empty
    while !function_queue.is_empty() || !tuple_queue.is_empty() || !type_queue.is_empty() {
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
                        // Tuple instruction references tuples
                        if *type_id < used_tuples.len() && !used_tuples[*type_id] {
                            tuple_queue.push(*type_id);
                        }
                    }
                    Instruction::IsType(type_id) => {
                        // IsType instruction references types
                        if *type_id < used_types.len() && !used_types[*type_id] {
                            type_queue.push(*type_id);
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
        while let Some(type_id) = tuple_queue.pop() {
            // Skip if already processed
            if type_id >= used_tuples.len() || used_tuples[type_id] {
                continue;
            }
            used_tuples[type_id] = true;

            // Get the tuple type info (if it exists)
            let Some(type_info) = tuples.get(type_id) else {
                continue;
            };

            // Recursively mark all tuple types referenced in this type's fields
            for (_, field_type) in &type_info.fields {
                mark_tuple_references(field_type, &mut used_tuples, &mut tuple_queue);
            }
        }

        // Process check types
        while let Some(type_id) = type_queue.pop() {
            // Skip if already processed
            if type_id >= used_types.len() || used_types[type_id] {
                continue;
            }
            used_types[type_id] = true;

            // Get the check type (if it exists)
            let Some(check_type) = types.get(type_id) else {
                continue;
            };

            // Recursively mark all tuple types referenced in this check type
            mark_tuple_references(check_type, &mut used_tuples, &mut tuple_queue);
        }
    }

    // If nothing is used (shouldn't happen), return original without tree shaking
    if !used_functions.iter().any(|&used| used) {
        return Bytecode {
            constants: constants.to_vec(),
            functions: functions.to_vec(),
            builtins: builtins.to_vec(),
            entry: Some(entry_fn),
            tuples: tuples.to_vec(),
            types: types.to_vec(),
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

    let mut tuple_remap = HashMap::new();
    let mut new_tuples = Vec::new();

    for (old_id, type_info) in tuples.iter().enumerate() {
        if used_tuples[old_id] {
            tuple_remap.insert(old_id, new_tuples.len());
            new_tuples.push(type_info.clone());
        }
    }

    let remapped_tuples = new_tuples
        .into_iter()
        .map(|mut type_info| {
            type_info.fields = type_info
                .fields
                .into_iter()
                .map(|(name, typ)| (name, remap_tuple_ids(typ, &tuple_remap)))
                .collect();
            type_info
        })
        .collect();

    let mut type_remap = HashMap::new();
    let mut new_types = Vec::new();

    for (old_id, check_type) in types.iter().enumerate() {
        if used_types[old_id] {
            type_remap.insert(old_id, new_types.len());
            new_types.push(check_type.clone());
        }
    }

    let remapped_types = new_types
        .into_iter()
        .map(|typ| remap_tuple_ids(typ, &tuple_remap))
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
                        *tuple_remap
                            .get(&old_type_id)
                            .expect("Tuple type ID should be in remap table after tree shaking"),
                    ),
                    Instruction::IsType(old_type_id) => Instruction::IsType(
                        *type_remap
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
        tuples: remapped_tuples,
        types: remapped_types,
    }
}
