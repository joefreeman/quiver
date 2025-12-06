use crate::bytecode::{Bytecode, Function, Instruction};
use crate::types::{BuiltinInfo, TupleTypeInfo, Type};
use std::collections::{HashMap, HashSet, VecDeque};

/// Tree shake bytecode to remove unreachable code.
/// Returns an optimized Bytecode with only reachable functions, constants, builtins, tuples, and types.
pub fn tree_shake(bytecode: Bytecode, entry: usize) -> Bytecode {
    // Helper to collect type_ids transitively from a Type
    fn collect_type_refs(
        type_id: usize,
        types: &[Type],
        used_types: &mut HashSet<usize>,
        used_tuples: &mut HashSet<usize>,
        used_resources: &mut HashSet<String>,
    ) {
        if !used_types.insert(type_id) {
            return; // Already processed
        }
        let Some(typ) = types.get(type_id) else {
            return;
        };
        match typ {
            Type::Integer | Type::Binary | Type::Reference | Type::Variable(_) | Type::Cycle(_) => {
            }
            Type::Tuple(tuple_id) => {
                used_tuples.insert(*tuple_id);
            }
            Type::Partial { fields, .. } => {
                // Collect type references from partial fields
                for (_, field_type_id) in fields {
                    collect_type_refs(
                        *field_type_id,
                        types,
                        used_types,
                        used_tuples,
                        used_resources,
                    );
                }
            }
            Type::Callable {
                parameter,
                result,
                receive,
            } => {
                collect_type_refs(*parameter, types, used_types, used_tuples, used_resources);
                collect_type_refs(*result, types, used_types, used_tuples, used_resources);
                collect_type_refs(*receive, types, used_types, used_tuples, used_resources);
            }
            Type::Union(type_ids) => {
                for &tid in type_ids {
                    collect_type_refs(tid, types, used_types, used_tuples, used_resources);
                }
            }
            Type::Process { send, receive } => {
                if let Some(tid) = send {
                    collect_type_refs(*tid, types, used_types, used_tuples, used_resources);
                }
                if let Some(tid) = receive {
                    collect_type_refs(*tid, types, used_types, used_tuples, used_resources);
                }
            }
            Type::Resource(name) => {
                used_resources.insert(name.clone());
            }
        }
    }

    // Mark phase: find all reachable items
    let mut used_functions: HashSet<usize> = HashSet::new();
    let mut used_constants: HashSet<usize> = HashSet::new();
    let mut used_tuples: HashSet<usize> = HashSet::new();
    let mut used_types: HashSet<usize> = HashSet::new();
    let mut used_builtins: HashSet<usize> = HashSet::new();
    let mut used_resources: HashSet<String> = HashSet::new();

    // Always keep NIL and OK tuples (indices 0 and 1)
    used_tuples.insert(0);
    used_tuples.insert(1);

    // BFS through reachable functions
    let mut queue: VecDeque<usize> = VecDeque::new();
    queue.push_back(entry);

    while let Some(fn_id) = queue.pop_front() {
        if !used_functions.insert(fn_id) {
            continue; // Already processed
        }

        let Some(function) = bytecode.functions.get(fn_id) else {
            continue;
        };

        // Collect type_ids from function's type
        collect_type_refs(
            function.type_id,
            &bytecode.types,
            &mut used_types,
            &mut used_tuples,
            &mut used_resources,
        );

        for instruction in &function.instructions {
            match instruction {
                Instruction::Function(id) => {
                    queue.push_back(*id);
                }
                Instruction::Constant(id) => {
                    used_constants.insert(*id);
                }
                Instruction::Tuple(id) => {
                    used_tuples.insert(*id);
                    // Also mark the corresponding Type::Tuple as used for IsType compatibility checks
                    if let Some(type_id) = bytecode
                        .types
                        .iter()
                        .position(|t| matches!(t, Type::Tuple(tid) if *tid == *id))
                    {
                        used_types.insert(type_id);
                    }
                }
                Instruction::IsType(id) => {
                    collect_type_refs(
                        *id,
                        &bytecode.types,
                        &mut used_types,
                        &mut used_tuples,
                        &mut used_resources,
                    );
                }
                Instruction::Builtin(id) => {
                    used_builtins.insert(*id);
                }
                Instruction::Process(_, func_id) => {
                    queue.push_back(*func_id);
                }
                _ => {}
            }
        }
    }

    // Collect types from builtins
    for &builtin_id in &used_builtins {
        if let Some(builtin) = bytecode.builtins.get(builtin_id) {
            collect_type_refs(
                builtin.param_type,
                &bytecode.types,
                &mut used_types,
                &mut used_tuples,
                &mut used_resources,
            );
            collect_type_refs(
                builtin.result_type,
                &bytecode.types,
                &mut used_types,
                &mut used_tuples,
                &mut used_resources,
            );
        }
    }

    // Collect types from tuple fields
    let tuples_snapshot: Vec<usize> = used_tuples.iter().copied().collect();
    for tuple_id in tuples_snapshot {
        if let Some(tuple_info) = bytecode.tuples.get(tuple_id) {
            for (_, field_type_id) in &tuple_info.fields {
                collect_type_refs(
                    *field_type_id,
                    &bytecode.types,
                    &mut used_types,
                    &mut used_tuples,
                    &mut used_resources,
                );
            }
        }
    }

    // Build remap tables
    let mut sorted_functions: Vec<usize> = used_functions.into_iter().collect();
    sorted_functions.sort();
    let function_remap: HashMap<usize, usize> = sorted_functions
        .iter()
        .enumerate()
        .map(|(new_id, &old_id)| (old_id, new_id))
        .collect();

    let mut sorted_constants: Vec<usize> = used_constants.into_iter().collect();
    sorted_constants.sort();
    let constant_remap: HashMap<usize, usize> = sorted_constants
        .iter()
        .enumerate()
        .map(|(new_id, &old_id)| (old_id, new_id))
        .collect();

    let mut sorted_tuples: Vec<usize> = used_tuples.into_iter().collect();
    sorted_tuples.sort();
    let tuple_remap: HashMap<usize, usize> = sorted_tuples
        .iter()
        .enumerate()
        .map(|(new_id, &old_id)| (old_id, new_id))
        .collect();

    let mut sorted_types: Vec<usize> = used_types.into_iter().collect();
    sorted_types.sort();
    let type_remap: HashMap<usize, usize> = sorted_types
        .iter()
        .enumerate()
        .map(|(new_id, &old_id)| (old_id, new_id))
        .collect();

    let mut sorted_builtins: Vec<usize> = used_builtins.into_iter().collect();
    sorted_builtins.sort();
    let builtin_remap: HashMap<usize, usize> = sorted_builtins
        .iter()
        .enumerate()
        .map(|(new_id, &old_id)| (old_id, new_id))
        .collect();

    // Collect sorted resource names (resources are now strings, not IDs)
    let mut sorted_resources: Vec<String> = used_resources.into_iter().collect();
    sorted_resources.sort();

    // Helper to remap a Type
    let remap_type = |typ: &Type| -> Type {
        match typ {
            Type::Integer => Type::Integer,
            Type::Binary => Type::Binary,
            Type::Reference => Type::Reference,
            Type::Tuple(id) => Type::Tuple(*tuple_remap.get(id).unwrap_or(id)),
            Type::Partial { name, fields } => Type::Partial {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|(fname, ftype)| (fname.clone(), *type_remap.get(ftype).unwrap_or(ftype)))
                    .collect(),
            },
            Type::Callable {
                parameter,
                result,
                receive,
            } => Type::Callable {
                parameter: *type_remap.get(parameter).unwrap_or(parameter),
                result: *type_remap.get(result).unwrap_or(result),
                receive: *type_remap.get(receive).unwrap_or(receive),
            },
            Type::Cycle(depth) => Type::Cycle(*depth),
            Type::Union(type_ids) => Type::Union(
                type_ids
                    .iter()
                    .map(|id| *type_remap.get(id).unwrap_or(id))
                    .collect(),
            ),
            Type::Process { send, receive } => Type::Process {
                send: send.map(|id| *type_remap.get(&id).unwrap_or(&id)),
                receive: receive.map(|id| *type_remap.get(&id).unwrap_or(&id)),
            },
            Type::Resource(name) => Type::Resource(name.clone()),
            Type::Variable(name) => Type::Variable(name.clone()),
        }
    };

    // Build new functions with remapped instructions and type_id
    let new_functions: Vec<Function> = sorted_functions
        .iter()
        .map(|&old_id| {
            let old_func = &bytecode.functions[old_id];
            let new_instructions: Vec<Instruction> = old_func
                .instructions
                .iter()
                .map(|instr| match instr {
                    Instruction::Function(id) => {
                        Instruction::Function(*function_remap.get(id).unwrap())
                    }
                    Instruction::Constant(id) => {
                        Instruction::Constant(*constant_remap.get(id).unwrap())
                    }
                    Instruction::Tuple(id) => Instruction::Tuple(*tuple_remap.get(id).unwrap()),
                    Instruction::IsType(id) => Instruction::IsType(*type_remap.get(id).unwrap()),
                    Instruction::Builtin(id) => {
                        Instruction::Builtin(*builtin_remap.get(id).unwrap())
                    }
                    Instruction::Process(pid, fid) => {
                        Instruction::Process(*pid, *function_remap.get(fid).unwrap())
                    }
                    other => other.clone(),
                })
                .collect();
            Function {
                instructions: new_instructions,
                captures: old_func.captures,
                type_id: *type_remap
                    .get(&old_func.type_id)
                    .unwrap_or(&old_func.type_id),
            }
        })
        .collect();

    // Build new constants
    let new_constants: Vec<_> = sorted_constants
        .iter()
        .map(|&old_id| bytecode.constants[old_id].clone())
        .collect();

    // Build new tuples with remapped field type_ids
    let new_tuples: Vec<TupleTypeInfo> = sorted_tuples
        .iter()
        .map(|&old_id| {
            let old_tuple = &bytecode.tuples[old_id];
            TupleTypeInfo {
                name: old_tuple.name.clone(),
                fields: old_tuple
                    .fields
                    .iter()
                    .map(|(name, type_id)| {
                        (name.clone(), *type_remap.get(type_id).unwrap_or(type_id))
                    })
                    .collect(),
            }
        })
        .collect();

    // Build new builtins with remapped type_ids
    let new_builtins: Vec<BuiltinInfo> = sorted_builtins
        .iter()
        .map(|&old_id| {
            let old_builtin = &bytecode.builtins[old_id];
            BuiltinInfo {
                name: old_builtin.name.clone(),
                param_type: *type_remap
                    .get(&old_builtin.param_type)
                    .unwrap_or(&old_builtin.param_type),
                result_type: *type_remap
                    .get(&old_builtin.result_type)
                    .unwrap_or(&old_builtin.result_type),
            }
        })
        .collect();

    // Build new types with remapped references
    let new_types: Vec<Type> = sorted_types
        .iter()
        .map(|&old_id| remap_type(&bytecode.types[old_id]))
        .collect();

    // Resources are now strings directly (no remapping needed)
    let new_resources: Vec<String> = sorted_resources;

    Bytecode {
        constants: new_constants,
        functions: new_functions,
        builtins: new_builtins,
        entry: Some(*function_remap.get(&entry).unwrap()),
        tuples: new_tuples,
        types: new_types,
        resources: new_resources,
    }
}
