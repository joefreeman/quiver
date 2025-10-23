use crate::error::Error;
use crate::executor::Executor;
use crate::program::Program;
use crate::types::Type;
use crate::value::Value;
use std::collections::HashMap;
use std::sync::LazyLock;

pub mod binary;
pub mod math;

/// Type specification for lazy type resolution
#[derive(Clone, Debug)]
pub enum TypeSpec {
    Integer,
    Binary,
    Tuple(Option<&'static str>, Vec<(Option<&'static str>, TypeSpec)>),
    Union(Vec<TypeSpec>),
}

impl TypeSpec {
    /// Resolve this type specification to a concrete Type using the Program's type registry
    pub fn resolve(&self, program: &mut Program) -> Type {
        match self {
            TypeSpec::Integer => Type::Integer,
            TypeSpec::Binary => Type::Binary,
            TypeSpec::Tuple(name, field_specs) => {
                let fields: Vec<(Option<String>, Type)> = field_specs
                    .iter()
                    .map(|(field_name, spec)| {
                        (field_name.map(|s| s.to_string()), spec.resolve(program))
                    })
                    .collect();
                let type_id = program.register_type(name.map(|s| s.to_string()), fields);
                Type::Tuple(type_id)
            }
            TypeSpec::Union(specs) => {
                let types: Vec<Type> = specs.iter().map(|spec| spec.resolve(program)).collect();
                Type::Union(types)
            }
        }
    }
}

/// Function signature for builtin implementations
pub type BuiltinFn = fn(&Value, &mut Executor) -> Result<Value, Error>;

/// Helper to coerce function item to function pointer (avoids rust-analyzer warnings)
const fn coerce_builtin(f: BuiltinFn) -> BuiltinFn {
    f
}

/// Macro for registering a single builtin function
macro_rules! register_builtin {
    ($functions:expr, $fn_name:literal, $impl:path, $param:expr => $result:expr) => {
        $functions.insert($fn_name, (coerce_builtin($impl), $param, $result));
    };
}

/// Registry of all available builtin functions
pub struct BuiltinRegistry {
    /// Function name -> (implementation, param_spec, result_spec)
    functions: HashMap<&'static str, (BuiltinFn, TypeSpec, TypeSpec)>,
}

impl BuiltinRegistry {
    /// Get the implementation function for a builtin by function name
    pub fn get_implementation(&self, function: &str) -> Option<BuiltinFn> {
        self.functions.get(function).map(|(impl_fn, _, _)| *impl_fn)
    }

    /// Resolve and get the type signature for a builtin by function name
    pub fn resolve_signature(&self, function: &str, program: &mut Program) -> Option<(Type, Type)> {
        self.functions
            .get(function)
            .map(|(_, param_spec, result_spec)| {
                let param_type = param_spec.resolve(program);
                let result_type = result_spec.resolve(program);
                (param_type, result_type)
            })
    }

    /// Get the type specs for a builtin by function name (without resolving)
    pub fn get_specs(&self, function: &str) -> Option<(&TypeSpec, &TypeSpec)> {
        self.functions
            .get(function)
            .map(|(_, param_spec, result_spec)| (param_spec, result_spec))
    }

    /// Get all available function names
    pub fn get_function_names(&self) -> Vec<&'static str> {
        let mut functions: Vec<&'static str> = self.functions.keys().copied().collect();
        functions.sort_unstable();
        functions
    }
}

fn create_builtin_registry() -> BuiltinRegistry {
    let mut functions = HashMap::new();

    // Common type specifications
    let int_int = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Integer), (None, TypeSpec::Integer)],
    );
    let bin_int = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Binary), (None, TypeSpec::Integer)],
    );
    let bin_bin = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Binary), (None, TypeSpec::Binary)],
    );
    let bin_int_int = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
        ],
    );

    // Math functions
    register_builtin!(functions, "abs", math::builtin_math_abs, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(functions, "sqrt", math::builtin_math_sqrt, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(functions, "sin", math::builtin_math_sin, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(functions, "cos", math::builtin_math_cos, TypeSpec::Integer => TypeSpec::Integer);

    // Arithmetic operations - operate on [int, int] tuples
    register_builtin!(functions, "add", math::builtin_add, int_int.clone() => TypeSpec::Integer);
    register_builtin!(functions, "subtract", math::builtin_subtract, int_int.clone() => TypeSpec::Integer);
    register_builtin!(functions, "multiply", math::builtin_multiply, int_int.clone() => TypeSpec::Integer);
    register_builtin!(functions, "divide", math::builtin_divide, int_int.clone() => TypeSpec::Integer);
    register_builtin!(functions, "modulo", math::builtin_modulo, int_int.clone() => TypeSpec::Integer);

    // Comparison operations - operate on [int, int] tuples
    register_builtin!(functions, "compare", math::builtin_compare, int_int.clone() => TypeSpec::Integer);

    // Binary functions
    register_builtin!(functions, "binary_new", binary::builtin_binary_new, TypeSpec::Integer => TypeSpec::Binary);
    register_builtin!(functions, "binary_length", binary::builtin_binary_length, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(functions, "binary_get_byte", binary::builtin_binary_get_byte, bin_int.clone() => TypeSpec::Integer);
    register_builtin!(functions, "binary_concat", binary::builtin_binary_concat, bin_bin.clone() => TypeSpec::Binary);

    // Bitwise operations
    register_builtin!(functions, "binary_and", binary::builtin_binary_and, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(functions, "binary_or", binary::builtin_binary_or, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(functions, "binary_xor", binary::builtin_binary_xor, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(functions, "binary_not", binary::builtin_binary_not, TypeSpec::Binary => TypeSpec::Binary);

    // Shift operations
    register_builtin!(functions, "binary_shift_left", binary::builtin_binary_shift_left, bin_int.clone() => TypeSpec::Binary);
    register_builtin!(functions, "binary_shift_right", binary::builtin_binary_shift_right, bin_int.clone() => TypeSpec::Binary);

    // Bit-level operations
    register_builtin!(functions, "binary_get_bit_pos", binary::builtin_binary_get_bit_pos, bin_int.clone() => TypeSpec::Integer);
    register_builtin!(functions, "binary_set_bit", binary::builtin_binary_set_bit, bin_int_int.clone() => TypeSpec::Binary);
    register_builtin!(functions, "binary_popcount", binary::builtin_binary_popcount, TypeSpec::Binary => TypeSpec::Integer);

    // Multi-byte operations
    register_builtin!(functions, "binary_get_u32", binary::builtin_binary_get_u32, bin_int.clone() => TypeSpec::Integer);
    register_builtin!(functions, "binary_set_u32", binary::builtin_binary_set_u32, bin_int_int.clone() => TypeSpec::Binary);
    register_builtin!(functions, "binary_get_u64", binary::builtin_binary_get_u64, bin_int.clone() => TypeSpec::Integer);
    register_builtin!(functions, "binary_set_u64", binary::builtin_binary_set_u64, bin_int_int.clone() => TypeSpec::Binary);

    // Slicing operations
    register_builtin!(functions, "binary_slice", binary::builtin_binary_slice, bin_int_int.clone() => TypeSpec::Binary);
    register_builtin!(functions, "binary_take", binary::builtin_binary_take, bin_int.clone() => TypeSpec::Binary);
    register_builtin!(functions, "binary_drop", binary::builtin_binary_drop, bin_int.clone() => TypeSpec::Binary);
    register_builtin!(functions, "binary_pad", binary::builtin_binary_pad, bin_int_int.clone() => TypeSpec::Binary);

    // Hashing operations
    register_builtin!(functions, "binary_hash32", binary::builtin_binary_hash32, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(functions, "binary_hash64", binary::builtin_binary_hash64, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(functions, "string_hash", binary::builtin_string_hash, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(functions, "hash_chunk", binary::builtin_hash_chunk, bin_int.clone() => TypeSpec::Integer);

    BuiltinRegistry { functions }
}

/// Global builtin registry instance
pub static BUILTIN_REGISTRY: LazyLock<BuiltinRegistry> = LazyLock::new(create_builtin_registry);
