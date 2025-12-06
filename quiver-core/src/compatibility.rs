use crate::bytecode::{ConcreteType, Function, Instruction};
use crate::types::{BuiltinInfo, TupleTypeInfo, Type, TypeLookup, is_compatible};
use std::collections::HashSet;

/// TypeLookup implementation for compatibility computation
pub struct TypeLookupImpl<'a> {
    types: &'a [Type],
    tuples: &'a [TupleTypeInfo],
}

impl<'a> TypeLookupImpl<'a> {
    pub fn new(types: &'a [Type], tuples: &'a [TupleTypeInfo]) -> Self {
        TypeLookupImpl { types, tuples }
    }
}

impl<'a> TypeLookup for TypeLookupImpl<'a> {
    fn lookup_type(&self, type_id: usize) -> Option<&Type> {
        self.types.get(type_id)
    }

    fn lookup_tuple(&self, tuple_id: usize) -> Option<&TupleTypeInfo> {
        self.tuples.get(tuple_id)
    }
}

/// Information needed to compute compatibility for a program.
/// Function type information is derived from Function::type_id and the types registry.
pub struct CompatibilityInput<'a> {
    /// Types registered (index = type_id)
    pub types: &'a [Type],
    /// Full tuple type information
    pub tuples: &'a [TupleTypeInfo],
    /// Functions (to scan for IsType instructions and derive function types)
    pub functions: &'a [Function],
    /// Builtin information
    pub builtins: &'a [BuiltinInfo],
    /// Resource type names (index = resource_type_id)
    pub resource_names: &'a [String],
}

/// Extract function type components from a function's type_id.
/// Returns (parameter_type_id, callable_type_id, process_send, process_receive).
fn extract_function_type_info(
    func: &Function,
    types: &[Type],
) -> (usize, usize, Option<usize>, Option<usize>) {
    let type_id = func.type_id;
    match types.get(type_id) {
        Some(Type::Callable {
            parameter,
            result,
            receive,
        }) => (*parameter, type_id, Some(*receive), Some(*result)),
        _ => (0, type_id, None, None),
    }
}

/// Compute type compatibility table for all pattern types used in the program.
/// This precomputes which ConcreteTypes are compatible with which pattern types,
/// allowing O(1) runtime type checking instead of recursive type traversal.
/// Returns a Vec where index is type_id and value is the set of compatible concrete types.
pub fn compute_type_compatibility(input: &CompatibilityInput) -> Vec<HashSet<ConcreteType>> {
    let lookup = TypeLookupImpl::new(input.types, input.tuples);

    // Collect all pattern type IDs (types used in IsType instructions)
    let mut pattern_type_ids = HashSet::new();

    for function in input.functions {
        for instruction in &function.instructions {
            if let Instruction::IsType(type_id) = instruction {
                pattern_type_ids.insert(*type_id);
            }
        }
    }

    // Initialize the compatibility table with entries for all pattern types
    let mut compatible_with: Vec<HashSet<ConcreteType>> = vec![HashSet::new(); input.types.len()];

    // For each pattern type, compute which ConcreteTypes are compatible with it
    for &pattern_id in &pattern_type_ids {
        if pattern_id >= input.types.len() {
            continue; // Skip invalid IDs
        }

        compatible_with[pattern_id] = compute_compatible_concrete_types(pattern_id, input, &lookup);
    }

    compatible_with
}

/// Compute parameter compatibility for mailbox filtering.
/// For each function and builtin, computes which ConcreteTypes are compatible with its parameter type.
/// Returns (function_param_compatibility, builtin_param_compatibility).
pub fn compute_param_compatibility(
    input: &CompatibilityInput,
) -> (Vec<HashSet<ConcreteType>>, Vec<HashSet<ConcreteType>>) {
    let lookup = TypeLookupImpl::new(input.types, input.tuples);

    // Compute function parameter compatibility by extracting type info from each function
    let function_params: Vec<HashSet<ConcreteType>> = input
        .functions
        .iter()
        .map(|func| {
            let (parameter, _, _, _) = extract_function_type_info(func, input.types);
            compute_compatible_concrete_types(parameter, input, &lookup)
        })
        .collect();

    // Compute builtin parameter compatibility
    let builtin_params: Vec<HashSet<ConcreteType>> = input
        .builtins
        .iter()
        .map(|builtin_info| {
            compute_compatible_concrete_types(builtin_info.param_type, input, &lookup)
        })
        .collect();

    (function_params, builtin_params)
}

/// Compute which ConcreteTypes are compatible with a given pattern type ID.
fn compute_compatible_concrete_types(
    pattern_id: usize,
    input: &CompatibilityInput,
    lookup: &TypeLookupImpl,
) -> HashSet<ConcreteType> {
    let mut compat_set = HashSet::new();

    // We need to register primitive types to check compatibility
    // For Integer and Binary, we check by constructing their type IDs on-the-fly
    // and using is_compatible

    // Check Integer - find or assume Integer type ID
    if let Some(int_id) = input.types.iter().position(|t| matches!(t, Type::Integer)) {
        if is_compatible(int_id, pattern_id, lookup) {
            compat_set.insert(ConcreteType::Integer);
        }
    } else if let Some(pattern) = lookup.lookup_type(pattern_id)
        && (matches!(pattern, Type::Integer) || matches!(pattern, Type::Union(v) if v.is_empty()))
    {
        // Integer not in types vec, check if pattern matches Integer directly
        compat_set.insert(ConcreteType::Integer);
    }

    // Check Binary
    if let Some(bin_id) = input.types.iter().position(|t| matches!(t, Type::Binary)) {
        if is_compatible(bin_id, pattern_id, lookup) {
            compat_set.insert(ConcreteType::Binary);
        }
    } else if let Some(pattern) = lookup.lookup_type(pattern_id)
        && (matches!(pattern, Type::Binary) || matches!(pattern, Type::Union(v) if v.is_empty()))
    {
        compat_set.insert(ConcreteType::Binary);
    }

    // Check Reference
    if let Some(ref_id) = input
        .types
        .iter()
        .position(|t| matches!(t, Type::Reference))
    {
        if is_compatible(ref_id, pattern_id, lookup) {
            compat_set.insert(ConcreteType::Reference);
        }
    } else if let Some(pattern) = lookup.lookup_type(pattern_id)
        && (matches!(pattern, Type::Reference) || matches!(pattern, Type::Union(v) if v.is_empty()))
    {
        compat_set.insert(ConcreteType::Reference);
    }

    // Check all Tuples - find or construct type IDs for each tuple
    for tuple_id in 0..input.tuples.len() {
        let found_type_id = input
            .types
            .iter()
            .position(|t| matches!(t, Type::Tuple(id) if *id == tuple_id));
        if let Some(type_id) = found_type_id
            && is_compatible(type_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Tuple(tuple_id));
        }
    }

    // Check all Functions - use their callable type ID from type_id
    for (func_id, func) in input.functions.iter().enumerate() {
        let (_, callable, _, _) = extract_function_type_info(func, input.types);
        if is_compatible(callable, pattern_id, lookup) {
            compat_set.insert(ConcreteType::Function(func_id));
        }
    }

    // Check all Builtins - construct callable type for each
    for (builtin_id, builtin_info) in input.builtins.iter().enumerate() {
        // Find or create a callable type for this builtin
        // Look for an existing callable type with matching param/result
        let builtin_callable = input.types.iter().position(|t| {
            matches!(t, Type::Callable { parameter, result, receive }
                if *parameter == builtin_info.param_type
                && *result == builtin_info.result_type
                && lookup.lookup_type(*receive).map(|t| t.is_never()).unwrap_or(false))
        });
        if let Some(callable_id) = builtin_callable
            && is_compatible(callable_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Builtin(builtin_id));
        }
    }

    // Check all Processes - derive process type from function's callable type
    for (func_id, func) in input.functions.iter().enumerate() {
        let (_, _, process_send, process_receive) = extract_function_type_info(func, input.types);
        // Look for process type matching this function's send/receive
        let process_type = input.types.iter().position(|t| {
            matches!(t, Type::Process { send, receive }
                if *send == process_send && *receive == process_receive)
        });
        if let Some(process_id) = process_type
            && is_compatible(process_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Process(func_id));
        }
    }

    // Check all Resources
    for (resource_id, resource_name) in input.resource_names.iter().enumerate() {
        if let Some(type_id) = input
            .types
            .iter()
            .position(|t| matches!(t, Type::Resource(name) if name == resource_name))
            && is_compatible(type_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Resource(resource_id));
        }
    }

    compat_set
}
