use crate::bytecode::{Bytecode, Constant, Function, TypeId};
use crate::error::Error;
use crate::types::{TupleTypeInfo, Type, TypeLookup};
use crate::value::{Binary, MAX_BINARY_SIZE};
use std::collections::HashMap;

mod optimisation;

/// Program represents the compiled program data that is static during execution.
/// It contains constants, functions, builtins, and type information.
/// The compiler writes to this structure, and the VM reads from it.
#[derive(Debug, Clone)]
pub struct Program {
    constants: Vec<Constant>,
    functions: Vec<Function>,
    builtins: Vec<String>,
    types: HashMap<TypeId, TupleTypeInfo>,
    next_type_id: usize,
}

impl TypeLookup for Program {
    fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo> {
        self.types.get(type_id)
    }
}

impl Program {
    pub fn get_constant(&self, index: usize) -> Option<&Constant> {
        self.constants.get(index)
    }

    pub fn with_binary_bytes<F, R>(&self, binary: &Binary, f: F) -> Result<R, Error>
    where
        F: FnOnce(&[u8]) -> Result<R, Error>,
    {
        match binary {
            Binary::Constant(index) => match self.constants.get(*index) {
                Some(Constant::Binary(bytes)) => f(bytes),
                Some(_) => Err(Error::TypeMismatch {
                    expected: "binary constant".to_string(),
                    found: "non-binary constant".to_string(),
                }),
                None => Err(Error::ConstantUndefined(*index)),
            },
            Binary::Heap(_) => Err(Error::InvalidArgument(
                "Heap binaries cannot be accessed from Program (no scheduler context)".to_string(),
            )),
        }
    }

    /// Convenience method that clones the binary data
    /// For cases where the closure API would be cumbersome (e.g., multiple binary accesses)
    pub fn get_binary_bytes(&self, binary: &Binary) -> Result<Vec<u8>, Error> {
        self.with_binary_bytes(binary, |bytes| Ok(bytes.to_vec()))
    }

    pub fn allocate_binary(&mut self, bytes: Vec<u8>) -> Result<Binary, Error> {
        if bytes.len() > MAX_BINARY_SIZE {
            return Err(Error::InvalidArgument(format!(
                "Binary size {} exceeds maximum {}",
                bytes.len(),
                MAX_BINARY_SIZE
            )));
        }
        let index = self.register_constant(Constant::Binary(bytes));
        Ok(Binary::Constant(index))
    }

    pub fn new() -> Self {
        let mut program = Self {
            constants: Vec::new(),
            functions: Vec::new(),
            builtins: Vec::new(),
            types: HashMap::new(),
            next_type_id: 0,
        };

        // Register built-in types
        let nil_type_id = program.register_type(None, vec![]);
        assert_eq!(nil_type_id, TypeId::NIL);

        let ok_type_id = program.register_type(Some("Ok".to_string()), vec![]);
        assert_eq!(ok_type_id, TypeId::OK);

        program
    }

    pub fn from_bytecode(bytecode: Bytecode) -> Self {
        let next_type_id = bytecode.types.keys().map(|id| id.0).max().unwrap_or(0) + 1;

        Self {
            constants: bytecode.constants,
            functions: bytecode.functions,
            builtins: bytecode.builtins,
            types: bytecode.types,
            next_type_id,
        }
    }

    pub fn register_constant(&mut self, constant: Constant) -> usize {
        if let Some(index) = self.constants.iter().position(|c| c == &constant) {
            index
        } else {
            self.constants.push(constant);
            self.constants.len() - 1
        }
    }

    pub fn get_constants(&self) -> &Vec<Constant> {
        &self.constants
    }

    pub fn register_function(&mut self, function: Function) -> usize {
        if let Some(index) = self.functions.iter().position(|f| f == &function) {
            index
        } else {
            self.functions.push(function);
            self.functions.len() - 1
        }
    }

    pub fn get_functions(&self) -> &Vec<Function> {
        &self.functions
    }

    pub fn get_function(&self, index: usize) -> Option<&Function> {
        self.functions.get(index)
    }

    pub fn register_builtin(&mut self, function: String) -> usize {
        if let Some(index) = self.builtins.iter().position(|b| b == &function) {
            index
        } else {
            self.builtins.push(function);
            self.builtins.len() - 1
        }
    }

    pub fn get_builtins(&self) -> &Vec<String> {
        &self.builtins
    }

    pub fn get_builtin(&self, index: usize) -> Option<&String> {
        self.builtins.get(index)
    }

    pub fn register_type(
        &mut self,
        name: Option<String>,
        fields: Vec<(Option<String>, Type)>,
    ) -> TypeId {
        // Check if type already exists
        for (&existing_id, existing_type) in &self.types {
            if existing_type.0 == name && existing_type.1 == fields {
                return existing_id;
            }
        }

        let type_id = TypeId(self.next_type_id);
        self.next_type_id += 1;

        self.types.insert(type_id, (name, fields));
        type_id
    }

    pub fn get_types(&self) -> HashMap<TypeId, TupleTypeInfo> {
        self.types.clone()
    }

    /// Convert this program to bytecode format with an optional entry point
    /// Automatically performs tree shaking to remove unused functions, constants, and types
    pub fn to_bytecode(&self, entry: Option<usize>) -> Bytecode {
        // If there's no entry point, return as-is (nothing to tree shake)
        let Some(entry_fn) = entry else {
            return Bytecode {
                constants: self.constants.clone(),
                functions: self.functions.clone(),
                builtins: self.builtins.clone(),
                entry: None,
                types: self.types.clone(),
            };
        };

        optimisation::tree_shake(
            &self.functions,
            &self.constants,
            &self.types,
            &self.builtins,
            entry_fn,
        )
    }
}
