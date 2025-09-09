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

    // Math module
    let mut math_functions = HashMap::new();
    math_functions.insert(
        "abs",
        (
            math::builtin_math_abs as BuiltinFn,
            &[types::Type::Integer] as &[types::Type],
            &[types::Type::Integer] as &[types::Type],
        ),
    );
    math_functions.insert(
        "sqrt",
        (
            math::builtin_math_sqrt as BuiltinFn,
            &[types::Type::Integer],
            &[types::Type::Integer],
        ),
    );
    math_functions.insert(
        "sin",
        (
            math::builtin_math_sin as BuiltinFn,
            &[types::Type::Integer],
            &[types::Type::Integer],
        ),
    );
    math_functions.insert(
        "cos",
        (
            math::builtin_math_cos as BuiltinFn,
            &[types::Type::Integer],
            &[types::Type::Integer],
        ),
    );
    modules.insert("math", math_functions);

    // IO module
    let mut io_functions = HashMap::new();
    io_functions.insert(
        "print",
        (
            io::builtin_io_print as BuiltinFn,
            &[types::Type::Integer, types::Type::Binary] as &[types::Type],
            &[types::Type::Tuple(TypeId::OK)] as &[types::Type],
        ),
    );
    io_functions.insert(
        "println",
        (
            io::builtin_io_println as BuiltinFn,
            &[types::Type::Integer, types::Type::Binary] as &[types::Type],
            &[types::Type::Tuple(TypeId::OK)] as &[types::Type],
        ),
    );
    modules.insert("io", io_functions);

    BuiltinRegistry { modules }
}

/// Global builtin registry instance
pub static BUILTIN_REGISTRY: std::sync::LazyLock<BuiltinRegistry> =
    std::sync::LazyLock::new(create_builtin_registry);
