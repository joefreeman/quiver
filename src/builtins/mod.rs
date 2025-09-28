use crate::bytecode::Constant;
use crate::bytecode::TypeId;
use crate::types::Type;
use crate::vm::{Error, VM, Value};
use std::collections::HashMap;

pub mod binary;
pub mod io;
pub mod math;

/// Type specification for lazy type resolution
#[derive(Clone, Debug)]
pub enum TypeSpec {
    Integer,
    Binary,
    Nil,
    Ok,
    Tuple(Vec<TypeSpec>),
    Union(Vec<TypeSpec>),
}

impl TypeSpec {
    /// Resolve this type specification to a concrete Type using the VM's type registry
    pub fn resolve(&self, vm: &mut VM) -> Type {
        match self {
            TypeSpec::Integer => Type::Integer,
            TypeSpec::Binary => Type::Binary,
            TypeSpec::Nil => Type::Tuple(TypeId::NIL),
            TypeSpec::Ok => Type::Tuple(TypeId::OK),
            TypeSpec::Tuple(specs) => {
                let fields: Vec<(Option<String>, Type)> =
                    specs.iter().map(|spec| (None, spec.resolve(vm))).collect();
                let type_id = vm.register_type(None, fields);
                Type::Tuple(type_id)
            }
            TypeSpec::Union(specs) => {
                let types: Vec<Type> = specs.iter().map(|spec| spec.resolve(vm)).collect();
                Type::Union(types)
            }
        }
    }
}

/// Macro for registering a single builtin function
macro_rules! register_builtin {
    ($functions:expr, $fn_name:literal, $impl:path, $param:expr => $result:expr) => {
        $functions.insert($fn_name, ($impl as BuiltinFn, $param, $result));
    };
}

/// Function signature for builtin implementations
pub type BuiltinFn = fn(&Value, &[Constant]) -> Result<Value, Error>;

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
    pub fn resolve_signature(&self, function: &str, vm: &mut VM) -> Option<(Type, Type)> {
        self.functions
            .get(function)
            .map(|(_, param_spec, result_spec)| {
                let param_type = param_spec.resolve(vm);
                let result_type = result_spec.resolve(vm);
                (param_type, result_type)
            })
    }

    /// Get the type signature for a builtin by function name (legacy, returns None)
    pub fn get_signature(&self, _function: &str) -> Option<(Type, Type)> {
        // This method is deprecated - use resolve_signature instead
        None
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

    // Math functions
    register_builtin!(functions, "abs", math::builtin_math_abs, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(functions, "sqrt", math::builtin_math_sqrt, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(functions, "sin", math::builtin_math_sin, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(functions, "cos", math::builtin_math_cos, TypeSpec::Integer => TypeSpec::Integer);

    // Arithmetic operations - operate on [int, int] tuples
    register_builtin!(functions, "add", math::builtin_add, TypeSpec::Tuple(vec![TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Integer);
    register_builtin!(functions, "subtract", math::builtin_subtract, TypeSpec::Tuple(vec![TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Integer);
    register_builtin!(functions, "multiply", math::builtin_multiply, TypeSpec::Tuple(vec![TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Integer);
    register_builtin!(functions, "divide", math::builtin_divide, TypeSpec::Tuple(vec![TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Integer);
    register_builtin!(functions, "modulo", math::builtin_modulo, TypeSpec::Tuple(vec![TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Integer);

    // Comparison operations - operate on [int, int] tuples
    register_builtin!(functions, "compare", math::builtin_compare, TypeSpec::Tuple(vec![TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Integer);

    // Binary functions
    register_builtin!(functions, "binary_new", binary::builtin_binary_new, TypeSpec::Integer => TypeSpec::Binary);
    register_builtin!(functions, "binary_length", binary::builtin_binary_length, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(functions, "binary_get_byte", binary::builtin_binary_get_byte, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer]) => TypeSpec::Integer);
    register_builtin!(functions, "binary_concat", binary::builtin_binary_concat, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Binary]) => TypeSpec::Binary);

    // Bitwise operations
    register_builtin!(functions, "binary_and", binary::builtin_binary_and, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Binary]) => TypeSpec::Binary);
    register_builtin!(functions, "binary_or", binary::builtin_binary_or, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Binary]) => TypeSpec::Binary);
    register_builtin!(functions, "binary_xor", binary::builtin_binary_xor, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Binary]) => TypeSpec::Binary);
    register_builtin!(functions, "binary_not", binary::builtin_binary_not, TypeSpec::Binary => TypeSpec::Binary);

    // Shift operations
    register_builtin!(functions, "binary_shift_left", binary::builtin_binary_shift_left, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer]) => TypeSpec::Binary);
    register_builtin!(functions, "binary_shift_right", binary::builtin_binary_shift_right, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer]) => TypeSpec::Binary);

    // Bit-level operations
    register_builtin!(functions, "binary_get_bit_pos", binary::builtin_binary_get_bit_pos, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer]) => TypeSpec::Integer);
    register_builtin!(functions, "binary_set_bit", binary::builtin_binary_set_bit, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Binary);
    register_builtin!(functions, "binary_popcount", binary::builtin_binary_popcount, TypeSpec::Binary => TypeSpec::Integer);

    // Multi-byte operations
    register_builtin!(functions, "binary_get_u32", binary::builtin_binary_get_u32, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer]) => TypeSpec::Integer);
    register_builtin!(functions, "binary_set_u32", binary::builtin_binary_set_u32, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Binary);
    register_builtin!(functions, "binary_get_u64", binary::builtin_binary_get_u64, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer]) => TypeSpec::Integer);
    register_builtin!(functions, "binary_set_u64", binary::builtin_binary_set_u64, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Binary);

    // Slicing operations
    register_builtin!(functions, "binary_slice", binary::builtin_binary_slice, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Binary);
    register_builtin!(functions, "binary_take", binary::builtin_binary_take, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer]) => TypeSpec::Binary);
    register_builtin!(functions, "binary_drop", binary::builtin_binary_drop, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer]) => TypeSpec::Binary);
    register_builtin!(functions, "binary_pad", binary::builtin_binary_pad, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer, TypeSpec::Integer]) => TypeSpec::Binary);

    // Hashing operations
    register_builtin!(functions, "binary_hash32", binary::builtin_binary_hash32, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(functions, "binary_hash64", binary::builtin_binary_hash64, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(functions, "string_hash", binary::builtin_string_hash, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(functions, "hash_chunk", binary::builtin_hash_chunk, TypeSpec::Tuple(vec![TypeSpec::Binary, TypeSpec::Integer]) => TypeSpec::Integer);

    // IO functions
    register_builtin!(functions, "print", io::builtin_io_print, TypeSpec::Union(vec![TypeSpec::Integer, TypeSpec::Binary]) => TypeSpec::Ok);
    register_builtin!(functions, "println", io::builtin_io_println, TypeSpec::Union(vec![TypeSpec::Integer, TypeSpec::Binary]) => TypeSpec::Ok);

    BuiltinRegistry { functions }
}

/// Global builtin registry instance
pub static BUILTIN_REGISTRY: std::sync::LazyLock<BuiltinRegistry> =
    std::sync::LazyLock::new(create_builtin_registry);
