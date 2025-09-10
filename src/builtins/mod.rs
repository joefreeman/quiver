use crate::bytecode::Constant;
use crate::bytecode::TypeId;
use crate::types::Type;
use crate::vm::{Error, Value};
use std::collections::HashMap;

pub mod binary;
pub mod io;
pub mod math;

/// Macro for registering a single builtin function
macro_rules! register_builtin {
    ($functions:expr, $fn_name:literal, $impl:path, [$($param:expr),*] -> [$($result:expr),*]) => {
        $functions.insert(
            $fn_name,
            (
                $impl as BuiltinFn,
                &[$($param),*] as &[Type],
                &[$($result),*] as &[Type],
            ),
        );
    };
}

/// Function signature for builtin implementations
pub type BuiltinFn = fn(&Value, &[Constant]) -> Result<Value, Error>;

/// Registry of all available builtin functions
pub struct BuiltinRegistry {
    /// Function name -> (implementation, param_types, result_types)
    functions: HashMap<&'static str, (BuiltinFn, &'static [Type], &'static [Type])>,
}

impl BuiltinRegistry {
    /// Get the implementation function for a builtin by function name
    pub fn get_implementation(&self, function: &str) -> Option<BuiltinFn> {
        self.functions.get(function).map(|(impl_fn, _, _)| *impl_fn)
    }

    /// Get the type signature for a builtin by function name
    pub fn get_signature(&self, function: &str) -> Option<(Vec<Type>, Vec<Type>)> {
        self.functions
            .get(function)
            .map(|(_, param_types, result_types)| (param_types.to_vec(), result_types.to_vec()))
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
    register_builtin!(functions, "abs", math::builtin_math_abs, [Type::Integer] -> [Type::Integer]);
    register_builtin!(functions, "sqrt", math::builtin_math_sqrt, [Type::Integer] -> [Type::Integer]);
    register_builtin!(functions, "sin", math::builtin_math_sin, [Type::Integer] -> [Type::Integer]);
    register_builtin!(functions, "cos", math::builtin_math_cos, [Type::Integer] -> [Type::Integer]);

    // Arithmetic operations - operate on [int, int] tuples
    register_builtin!(functions, "add", math::builtin_add, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);
    register_builtin!(functions, "subtract", math::builtin_subtract, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);
    register_builtin!(functions, "multiply", math::builtin_multiply, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);
    register_builtin!(functions, "divide", math::builtin_divide, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);
    register_builtin!(functions, "modulo", math::builtin_modulo, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);

    // Comparison operations - operate on [int, int] tuples
    register_builtin!(functions, "compare", math::builtin_compare, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);

    // Binary functions
    register_builtin!(functions, "binary_new", binary::builtin_binary_new, [Type::Integer] -> [Type::Binary]);
    register_builtin!(functions, "binary_length", binary::builtin_binary_length, [Type::Binary] -> [Type::Integer]);
    register_builtin!(functions, "binary_get_byte", binary::builtin_binary_get_byte, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);
    register_builtin!(functions, "binary_concat", binary::builtin_binary_concat, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);

    // Bitwise operations
    register_builtin!(functions, "binary_and", binary::builtin_binary_and, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);
    register_builtin!(functions, "binary_or", binary::builtin_binary_or, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);
    register_builtin!(functions, "binary_xor", binary::builtin_binary_xor, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);
    register_builtin!(functions, "binary_not", binary::builtin_binary_not, [Type::Binary] -> [Type::Binary]);

    // Shift operations
    register_builtin!(functions, "binary_shift_left", binary::builtin_binary_shift_left, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);
    register_builtin!(functions, "binary_shift_right", binary::builtin_binary_shift_right, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);

    // Bit-level operations
    register_builtin!(functions, "binary_get_bit_pos", binary::builtin_binary_get_bit_pos, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);
    register_builtin!(functions, "binary_set_bit", binary::builtin_binary_set_bit, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);
    register_builtin!(functions, "binary_popcount", binary::builtin_binary_popcount, [Type::Binary] -> [Type::Integer]);

    // Multi-byte operations
    register_builtin!(functions, "binary_get_u32", binary::builtin_binary_get_u32, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);
    register_builtin!(functions, "binary_set_u32", binary::builtin_binary_set_u32, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);
    register_builtin!(functions, "binary_get_u64", binary::builtin_binary_get_u64, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);
    register_builtin!(functions, "binary_set_u64", binary::builtin_binary_set_u64, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);

    // Slicing operations
    register_builtin!(functions, "binary_slice", binary::builtin_binary_slice, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);
    register_builtin!(functions, "binary_take", binary::builtin_binary_take, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);
    register_builtin!(functions, "binary_drop", binary::builtin_binary_drop, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);
    register_builtin!(functions, "binary_pad", binary::builtin_binary_pad, [Type::Tuple(TypeId::NIL)] -> [Type::Binary]);

    // Hashing operations
    register_builtin!(functions, "binary_hash32", binary::builtin_binary_hash32, [Type::Binary] -> [Type::Integer]);
    register_builtin!(functions, "binary_hash64", binary::builtin_binary_hash64, [Type::Binary] -> [Type::Integer]);
    register_builtin!(functions, "string_hash", binary::builtin_string_hash, [Type::Binary] -> [Type::Integer]);
    register_builtin!(functions, "hash_chunk", binary::builtin_hash_chunk, [Type::Tuple(TypeId::NIL)] -> [Type::Integer]);

    // IO functions
    register_builtin!(functions, "print", io::builtin_io_print, [Type::Integer, Type::Binary] -> [Type::Tuple(TypeId::OK)]);
    register_builtin!(functions, "println", io::builtin_io_println, [Type::Integer, Type::Binary] -> [Type::Tuple(TypeId::OK)]);

    BuiltinRegistry { functions }
}

/// Global builtin registry instance
pub static BUILTIN_REGISTRY: std::sync::LazyLock<BuiltinRegistry> =
    std::sync::LazyLock::new(create_builtin_registry);
