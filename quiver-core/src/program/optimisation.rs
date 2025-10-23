use crate::bytecode::{BuiltinInfo, Bytecode, Constant, Function, Instruction, TypeId};
use crate::types::{TupleTypeInfo, Type};
use std::collections::HashMap;

/// Recursively mark all TypeIds referenced within a Type structure
fn mark_type_references(typ: &Type, used_types: &mut [bool], type_queue: &mut Vec<TypeId>) {
    match typ {
        Type::Tuple(type_id) | Type::Partial(type_id) => {
            if type_id.0 < used_types.len() && !used_types[type_id.0] {
                type_queue.push(*type_id);
            }
        }
        Type::Union(types_vec) => {
            for t in types_vec {
                mark_type_references(t, used_types, type_queue);
            }
        }
        Type::Callable(callable) => {
            mark_type_references(&callable.parameter, used_types, type_queue);
            mark_type_references(&callable.result, used_types, type_queue);
            mark_type_references(&callable.receive, used_types, type_queue);
        }
        Type::Process(process) => {
            if let Some(receive) = &process.receive {
                mark_type_references(receive, used_types, type_queue);
            }
            if let Some(returns) = &process.returns {
                mark_type_references(returns, used_types, type_queue);
            }
        }
        Type::Integer | Type::Binary | Type::Cycle(_) | Type::Variable(_) => {
            // These don't reference other types
        }
    }
}

/// Remap all TypeIds within a Type structure according to the provided remap table
fn remap_type_ids(typ: Type, type_remap: &HashMap<TypeId, TypeId>) -> Type {
    match typ {
        Type::Tuple(old_id) => Type::Tuple(
            *type_remap
                .get(&old_id)
                .expect("Tuple type ID should be in remap table after tree shaking"),
        ),
        Type::Partial(old_id) => Type::Partial(
            *type_remap
                .get(&old_id)
                .expect("Partial type ID should be in remap table after tree shaking"),
        ),
        Type::Union(types_vec) => Type::Union(
            types_vec
                .into_iter()
                .map(|t| remap_type_ids(t, type_remap))
                .collect(),
        ),
        Type::Callable(callable) => Type::Callable(Box::new(crate::types::CallableType {
            parameter: remap_type_ids(callable.parameter, type_remap),
            result: remap_type_ids(callable.result, type_remap),
            receive: remap_type_ids(callable.receive, type_remap),
        })),
        Type::Process(process) => Type::Process(Box::new(crate::types::ProcessType {
            receive: process
                .receive
                .map(|t| Box::new(remap_type_ids(*t, type_remap))),
            returns: process
                .returns
                .map(|t| Box::new(remap_type_ids(*t, type_remap))),
        })),
        Type::Integer | Type::Binary | Type::Cycle(_) | Type::Variable(_) => typ,
    }
}

/// Perform tree shaking on program data to remove unused functions, constants, types, and builtins
/// Returns the collections needed to build an optimized Bytecode
pub fn tree_shake(
    functions: &[Function],
    constants: &[Constant],
    types: &[TupleTypeInfo],
    builtins: &[BuiltinInfo],
    entry_fn: usize,
) -> Bytecode {
    // Mark phase: find all reachable functions, constants, types, and builtins
    let mut used_functions = vec![false; functions.len()];
    let mut used_constants = vec![false; constants.len()];
    let mut used_types = vec![false; types.len()];
    let mut used_builtins = vec![false; builtins.len()];

    // Separate queues for functions and types
    let mut function_queue = vec![entry_fn];
    let mut type_queue = Vec::new();

    // Always mark NIL and OK as used (they're built-in control flow types)
    if TypeId::NIL.0 < used_types.len() {
        type_queue.push(TypeId::NIL);
    }
    if TypeId::OK.0 < used_types.len() {
        type_queue.push(TypeId::OK);
    }

    // Process both function and type queues until both are empty
    while !function_queue.is_empty() || !type_queue.is_empty() {
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
                    Instruction::Tuple(type_id) | Instruction::IsTuple(type_id) => {
                        if type_id.0 < used_types.len() && !used_types[type_id.0] {
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

        // Process types
        while let Some(type_id) = type_queue.pop() {
            // Skip if already processed
            if type_id.0 >= used_types.len() || used_types[type_id.0] {
                continue;
            }
            used_types[type_id.0] = true;

            // Get the type info (if it exists)
            let Some(type_info) = types.get(type_id.0) else {
                continue;
            };

            // Recursively mark all types referenced in this type's fields
            for (_, field_type) in &type_info.fields {
                mark_type_references(field_type, &mut used_types, &mut type_queue);
            }
        }
    }

    // If nothing is used (shouldn't happen), return original without tree shaking
    if !used_functions.iter().any(|&used| used) {
        return Bytecode {
            constants: constants.to_vec(),
            functions: functions.to_vec(),
            builtins: builtins.to_vec(),
            entry: Some(entry_fn),
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

    let mut type_remap = HashMap::new();
    let mut new_types = Vec::new();

    for (old_id, type_info) in types.iter().enumerate() {
        if used_types[old_id] {
            type_remap.insert(TypeId(old_id), TypeId(new_types.len()));
            new_types.push(type_info.clone());
        }
    }

    // Remap TypeIds within the type structures themselves
    let remapped_types = new_types
        .into_iter()
        .map(|mut type_info| {
            type_info.fields = type_info
                .fields
                .into_iter()
                .map(|(name, typ)| (name, remap_type_ids(typ, &type_remap)))
                .collect();
            type_info
        })
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
                        *type_remap
                            .get(&old_type_id)
                            .expect("Tuple type ID should be in remap table after tree shaking"),
                    ),
                    Instruction::IsTuple(old_type_id) => Instruction::IsTuple(
                        *type_remap
                            .get(&old_type_id)
                            .expect("IsTuple type ID should be in remap table after tree shaking"),
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
        types: remapped_types,
    }
}
