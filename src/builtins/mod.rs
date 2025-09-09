//! Builtin function registry and definitions
//!
//! This module provides a unified way to define, register, and access builtin functions.
//! All builtin information (name, type signature, implementation) is defined in one place.

use crate::bytecode::Constant;
use crate::bytecode::TypeId;
use crate::types;
use crate::vm::{Error, Value};
use std::collections::HashMap;

pub mod io;
pub mod math;

/// Macro for building a module's builtin function registry
macro_rules! builtin_module {
    ($($fn_name:literal : $impl:path : [$($param:expr),*] -> [$($result:expr),*]),* $(,)?) => {{
        let mut functions = HashMap::new();
        $(
            functions.insert(
                $fn_name,
                (
                    $impl as BuiltinFn,
                    &[$($param),*] as &[types::Type],
                    &[$($result),*] as &[types::Type],
                ),
            );
        )*
        functions
    }};
}

/// Function signature for builtin implementations
pub type BuiltinFn = fn(&Value, &[Constant]) -> Result<Value, Error>;

/// Registry of all available builtin functions
pub struct BuiltinRegistry {
    /// Module name -> Function name -> (implementation, param_types, result_types)
    modules: HashMap<
        &'static str,
        HashMap<&'static str, (BuiltinFn, &'static [types::Type], &'static [types::Type])>,
    >,
}

impl BuiltinRegistry {
    /// Get the implementation function for a builtin by module and function name
    pub fn get_implementation(&self, module: &str, function: &str) -> Option<BuiltinFn> {
        self.modules
            .get(module)?
            .get(function)
            .map(|(impl_fn, _, _)| *impl_fn)
    }

    /// Get the type signature for a builtin by module and function name
    pub fn get_signature(
        &self,
        module: &str,
        function: &str,
    ) -> Option<(Vec<types::Type>, Vec<types::Type>)> {
        self.modules
            .get(module)?
            .get(function)
            .map(|(_, param_types, result_types)| (param_types.to_vec(), result_types.to_vec()))
    }

    /// Get all function names for a specific module
    pub fn get_module_functions(&self, module_name: &str) -> Option<Vec<&'static str>> {
        self.modules
            .get(module_name)
            .map(|functions| functions.keys().copied().collect())
    }

    /// Get all available modules
    pub fn get_modules(&self) -> Vec<&'static str> {
        let mut modules: Vec<&'static str> = self.modules.keys().copied().collect();
        modules.sort_unstable();
        modules
    }

    /// Check if a module exists
    pub fn module_exists(&self, module_name: &str) -> bool {
        self.modules.contains_key(module_name)
    }
}

/// Create the builtin registry with hierarchical module structure
fn create_builtin_registry() -> BuiltinRegistry {
    let mut modules = HashMap::new();

    modules.insert(
        "math",
        builtin_module! {
            "abs": math::builtin_math_abs: [types::Type::Integer] -> [types::Type::Integer],
            "sqrt": math::builtin_math_sqrt: [types::Type::Integer] -> [types::Type::Integer],
            "sin": math::builtin_math_sin: [types::Type::Integer] -> [types::Type::Integer],
            "cos": math::builtin_math_cos: [types::Type::Integer] -> [types::Type::Integer],
        },
    );

    modules.insert(
        "io",
        builtin_module! {
            "print": io::builtin_io_print: [types::Type::Integer, types::Type::Binary] -> [types::Type::Tuple(TypeId::OK)],
            "println": io::builtin_io_println: [types::Type::Integer, types::Type::Binary] -> [types::Type::Tuple(TypeId::OK)],
        }
    );

    BuiltinRegistry { modules }
}

/// Global builtin registry instance
pub static BUILTIN_REGISTRY: std::sync::LazyLock<BuiltinRegistry> =
    std::sync::LazyLock::new(create_builtin_registry);
