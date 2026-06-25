use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::{Action, ProcessId};
use crate::program::Program;
use crate::types::Type;
use crate::value::Value;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::collections::HashMap;

/// Convert a `BigInt` to `i64`, erroring if it doesn't fit. Used by the bounded
/// (bitwise/binary) builtins, whose semantics operate on machine integers.
pub fn bigint_to_i64(n: &BigInt) -> Result<i64, Error> {
    n.to_i64().ok_or_else(|| {
        Error::InvalidArgument(format!("Integer {n} does not fit in a 64-bit value"))
    })
}

/// Convert a `BigInt` to `usize`, erroring if it's negative or too large.
pub fn bigint_to_usize(n: &BigInt) -> Result<usize, Error> {
    n.to_usize().ok_or_else(|| {
        Error::InvalidArgument(format!("Integer {n} does not fit in an unsigned index"))
    })
}

/// Convert a `BigInt` to `u8`, erroring if it's out of the `0..=255` byte range.
pub fn bigint_to_u8(n: &BigInt) -> Result<u8, Error> {
    n.to_u8()
        .ok_or_else(|| Error::InvalidArgument(format!("Integer {n} is not a byte (0..=255)")))
}

/// Build a `BigInt` from an `i64`. Lets downstream crates construct integer values
/// without depending on `num-bigint` directly.
pub fn bigint_from_i64(n: i64) -> BigInt {
    BigInt::from(n)
}

/// Parse a decimal string into a `BigInt`. Used to carry arbitrary-precision integers
/// across a boundary (e.g. the wasm/JS value bridge) as strings, so downstream crates
/// need not depend on `num-bigint` directly. The pairing `i.to_string()` round-trips.
pub fn bigint_from_str(s: &str) -> Result<BigInt, Error> {
    s.parse::<BigInt>()
        .map_err(|_| Error::InvalidArgument(format!("Invalid integer string: {s}")))
}

pub mod binary;
pub mod integer;
pub mod io;
pub mod vector;

/// Result of a builtin function execution
#[derive(Debug)]
pub enum BuiltinResult<E: Effect> {
    /// Immediate value result
    Value(Value),
    /// Action that requires Environment coordination
    Action(Action<E>),
}

/// Type specification for lazy type resolution
#[derive(Clone, Debug)]
pub enum TypeSpec {
    Integer,
    Binary,
    Tuple(Option<&'static str>, Vec<(Option<&'static str>, TypeSpec)>),
    Union(Vec<TypeSpec>),
    Process(Option<Box<TypeSpec>>, Option<Box<TypeSpec>>), // Process type: (send, receive)
    Resource(String), // Opaque resource type identifier (e.g., "File", "TcpSocket")
}

impl TypeSpec {
    /// Resolve this type specification to a type ID in the Program's type registry
    pub fn resolve_to_id(&self, program: &mut Program) -> usize {
        match self {
            TypeSpec::Integer => program.register_type(Type::Integer),
            TypeSpec::Binary => program.register_type(Type::Binary),
            TypeSpec::Tuple(name, field_specs) => {
                let fields: Vec<(Option<String>, usize)> = field_specs
                    .iter()
                    .map(|(field_name, spec)| {
                        (
                            field_name.map(|s| s.to_string()),
                            spec.resolve_to_id(program),
                        )
                    })
                    .collect();
                let tuple_id = program.register_tuple(name.map(|s| s.to_string()), fields);
                program.register_type(Type::Tuple(tuple_id))
            }
            TypeSpec::Union(specs) => {
                let type_ids: Vec<usize> = specs
                    .iter()
                    .map(|spec| spec.resolve_to_id(program))
                    .collect();
                program.register_type(Type::Union(type_ids))
            }
            TypeSpec::Process(send, receive) => {
                let send_id = send.as_ref().map(|s| s.resolve_to_id(program));
                let receive_id = receive.as_ref().map(|r| r.resolve_to_id(program));
                program.register_type(Type::Process {
                    send: send_id,
                    receive: receive_id,
                })
            }
            TypeSpec::Resource(name) => program.register_type(Type::Resource(name.clone())),
        }
    }

    /// Resolve this type specification to a concrete Type using the Program's type registry
    /// Note: This returns the Type itself, not a type ID
    pub fn resolve(&self, program: &mut Program) -> Type {
        match self {
            TypeSpec::Integer => Type::Integer,
            TypeSpec::Binary => Type::Binary,
            TypeSpec::Tuple(name, field_specs) => {
                let fields: Vec<(Option<String>, usize)> = field_specs
                    .iter()
                    .map(|(field_name, spec)| {
                        (
                            field_name.map(|s| s.to_string()),
                            spec.resolve_to_id(program),
                        )
                    })
                    .collect();
                let tuple_id = program.register_tuple(name.map(|s| s.to_string()), fields);
                Type::Tuple(tuple_id)
            }
            TypeSpec::Union(specs) => {
                let type_ids: Vec<usize> = specs
                    .iter()
                    .map(|spec| spec.resolve_to_id(program))
                    .collect();
                Type::Union(type_ids)
            }
            TypeSpec::Process(send, receive) => {
                let send_id = send.as_ref().map(|s| s.resolve_to_id(program));
                let receive_id = receive.as_ref().map(|r| r.resolve_to_id(program));
                Type::Process {
                    send: send_id,
                    receive: receive_id,
                }
            }
            TypeSpec::Resource(name) => Type::Resource(name.clone()),
        }
    }
}

/// Function signature for builtin implementations
pub type BuiltinFn<E> = fn(ProcessId, &Value, &mut Executor<E>) -> Result<BuiltinResult<E>, Error>;

/// Function signature for builtin module registration
pub type BuiltinModule<E> = fn(&mut BuiltinRegistry<E>);

/// Helper to coerce function item to function pointer (avoids rust-analyzer warnings)
const fn coerce_builtin<E: Effect>(f: BuiltinFn<E>) -> BuiltinFn<E> {
    f
}

/// Macro for registering a single builtin function
macro_rules! register_builtin {
    ($registry:expr, $fn_name:literal, $impl:path, $param:expr => $result:expr) => {
        $registry.register($fn_name.to_string(), coerce_builtin($impl), $param, $result);
    };
}

/// Registry of all available builtin functions
#[derive(Clone)]
pub struct BuiltinRegistry<E: Effect> {
    /// Function name -> (implementation, param_spec, result_spec)
    functions: HashMap<String, (BuiltinFn<E>, TypeSpec, TypeSpec)>,
}

impl<E: Effect> Default for BuiltinRegistry<E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<E: Effect> BuiltinRegistry<E> {
    /// Create a new empty builtin registry
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    /// Register a single builtin function
    pub fn register(
        &mut self,
        name: String,
        impl_fn: BuiltinFn<E>,
        param: TypeSpec,
        result: TypeSpec,
    ) {
        self.functions.insert(name, (impl_fn, param, result));
    }

    /// Attach (replace) the implementation of an already-registered builtin, keeping its
    /// signature. This is how an executing host backs a builtin whose *signature* is part of the
    /// universal contract but whose *implementation* it provides — e.g. the IO builtins, whose
    /// signatures are registered everywhere (via [`core_modules`]) but whose runtime differs per
    /// host (native io-uring, a web backend, or none in a type-checker).
    pub fn attach_implementation(&mut self, name: &str, impl_fn: BuiltinFn<E>) {
        match self.functions.get_mut(name) {
            Some((existing, _, _)) => *existing = impl_fn,
            None => debug_assert!(
                false,
                "attaching an implementation for unregistered builtin `{name}`; its signature \
                 must be registered first (see core_modules)"
            ),
        }
    }

    /// Merge another registry into this one
    pub fn merge(&mut self, other: Self) {
        self.functions.extend(other.functions);
    }

    /// Create a registry from a list of builtin module functions
    pub fn with_modules(modules: &[BuiltinModule<E>]) -> Self {
        let mut registry = Self::new();
        for module in modules {
            module(&mut registry);
        }
        registry
    }

    /// Get the implementation function for a builtin by function name
    pub fn get_implementation(&self, function: &str) -> Option<BuiltinFn<E>> {
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
    pub fn get_function_names(&self) -> Vec<String> {
        let mut functions: Vec<String> = self.functions.keys().cloned().collect();
        functions.sort_unstable();
        functions
    }
}

/// Register all binary builtins (binary data manipulation)
pub fn register_binary_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    // Common type specifications
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
    let bin_int_int_int = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
        ],
    );
    let bin_int_int_int_int = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
        ],
    );

    // Binary functions
    register_builtin!(registry, "binary_new", binary::builtin_binary_new, TypeSpec::Integer => TypeSpec::Binary);
    register_builtin!(registry, "binary_length", binary::builtin_binary_length, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(registry, "binary_concat", binary::builtin_binary_concat, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(registry, "binary_repeat", binary::builtin_binary_repeat, bin_int.clone() => TypeSpec::Binary);

    // Bitwise operations
    register_builtin!(registry, "binary_and", binary::builtin_binary_and, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(registry, "binary_or", binary::builtin_binary_or, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(registry, "binary_xor", binary::builtin_binary_xor, bin_bin.clone() => TypeSpec::Binary);
    register_builtin!(registry, "binary_not", binary::builtin_binary_not, TypeSpec::Binary => TypeSpec::Binary);

    // Shift operations
    register_builtin!(registry, "binary_shift", binary::builtin_binary_shift, bin_int => TypeSpec::Binary);

    // Bit-level operations
    register_builtin!(registry, "binary_popcount", binary::builtin_binary_popcount, TypeSpec::Binary => TypeSpec::Integer);

    // Multi-byte operations (unified bit/byte access)
    register_builtin!(registry, "binary_get", binary::builtin_binary_get, bin_int_int_int => TypeSpec::Integer);
    register_builtin!(registry, "binary_set", binary::builtin_binary_set, bin_int_int_int_int => TypeSpec::Binary);

    // Slicing operations
    register_builtin!(registry, "binary_slice", binary::builtin_binary_slice, bin_int_int.clone() => TypeSpec::Binary);

    // Byte search: index of a byte at or after an offset, or nil
    register_builtin!(registry, "binary_index", binary::builtin_binary_index, bin_int_int.clone() => TypeSpec::Union(vec![TypeSpec::Integer, TypeSpec::Tuple(None, vec![])]));

    // Hashing operations
    register_builtin!(registry, "binary_hash32", binary::builtin_binary_hash32, TypeSpec::Binary => TypeSpec::Integer);
    register_builtin!(registry, "binary_hash64", binary::builtin_binary_hash64, TypeSpec::Binary => TypeSpec::Integer);

    // Append operation
    register_builtin!(registry, "binary_append", binary::builtin_binary_append, bin_int_int => TypeSpec::Binary);
}

/// Register all integer builtins: arithmetic/math (arbitrary-precision) and bitwise (64-bit).
pub fn register_integer_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    // Common type specification
    let int_int = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Integer), (None, TypeSpec::Integer)],
    );

    // Unary math functions
    register_builtin!(registry, "integer_abs", integer::builtin_integer_abs, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(registry, "integer_sqrt", integer::builtin_integer_sqrt, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(registry, "integer_sin", integer::builtin_integer_sin, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(registry, "integer_cos", integer::builtin_integer_cos, TypeSpec::Integer => TypeSpec::Integer);

    // Arithmetic operations - operate on [int, int] tuples
    register_builtin!(registry, "integer_add", integer::builtin_integer_add, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_subtract", integer::builtin_integer_subtract, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_multiply", integer::builtin_integer_multiply, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_divide", integer::builtin_integer_divide, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_modulo", integer::builtin_integer_modulo, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_gcd", integer::builtin_integer_gcd, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_compare", integer::builtin_integer_compare, int_int.clone() => TypeSpec::Integer);

    // Integer bitwise operations
    register_builtin!(registry, "integer_and", integer::builtin_integer_and, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_or", integer::builtin_integer_or, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_xor", integer::builtin_integer_xor, int_int.clone() => TypeSpec::Integer);
    register_builtin!(registry, "integer_not", integer::builtin_integer_not, TypeSpec::Integer => TypeSpec::Integer);
    register_builtin!(registry, "integer_shift", integer::builtin_integer_shift, int_int => TypeSpec::Integer);
    register_builtin!(registry, "integer_popcount", integer::builtin_integer_popcount, TypeSpec::Integer => TypeSpec::Integer);
}

/// Register all packed-vector kernels (schema-agnostic byte-buffer numerics; see
/// [`vector`]). Lanes are little-endian two's-complement; widths are 4 or 8 bytes.
pub fn register_vector_builtins<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let nil = || TypeSpec::Tuple(None, vec![]);
    let bin_or_nil = TypeSpec::Union(vec![TypeSpec::Binary, nil()]);
    let int_or_nil = TypeSpec::Union(vec![TypeSpec::Integer, nil()]);
    // [bin, bin, width]
    let bin_bin_int = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
        ],
    );
    // [bin, width, int] — used for get (index) and push (value)
    let bin_int_int = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
        ],
    );
    let bin_int = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Binary), (None, TypeSpec::Integer)],
    );
    // [data, width, mask] — used for take (gather)
    let bin_int_bin = TypeSpec::Tuple(
        None,
        vec![
            (None, TypeSpec::Binary),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Binary),
        ],
    );

    register_builtin!(registry, "vector_add", vector::builtin_vector_add, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_subtract", vector::builtin_vector_subtract, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_multiply", vector::builtin_vector_multiply, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_less_than", vector::builtin_vector_less_than, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_equal", vector::builtin_vector_equal, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_greater_than", vector::builtin_vector_greater_than, bin_bin_int.clone() => bin_or_nil.clone());
    register_builtin!(registry, "vector_dot", vector::builtin_vector_dot, bin_bin_int => int_or_nil.clone());
    register_builtin!(registry, "vector_take", vector::builtin_vector_take, bin_int_bin => bin_or_nil.clone());
    register_builtin!(registry, "vector_get", vector::builtin_vector_get, bin_int_int.clone() => int_or_nil.clone());
    register_builtin!(registry, "vector_push", vector::builtin_vector_push, bin_int_int => bin_or_nil);
    register_builtin!(registry, "vector_sum", vector::builtin_vector_sum, bin_int => int_or_nil);
}

/// Get all core builtin modules. This establishes the full builtin *contract* every host shares:
/// the pure builtins (integer/binary/vector) with their universal implementations, and the IO
/// builtins' signatures (with placeholder implementations that executing hosts replace via
/// [`BuiltinRegistry::attach_implementation`]).
pub fn core_modules<E: Effect>() -> Vec<BuiltinModule<E>> {
    vec![
        register_binary_builtins,
        register_integer_builtins,
        register_vector_builtins,
        io::register_io_signatures,
    ]
}
