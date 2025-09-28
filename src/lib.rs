pub mod ast;
pub mod builtins;
pub mod bytecode;
pub mod compiler;
pub mod modules;
pub mod parser;
pub mod types;
pub mod vm;

use std::collections::HashMap;

use bytecode::{Function, TypeId};
use compiler::Compiler;
use modules::{FileSystemModuleLoader, InMemoryModuleLoader, ModuleLoader};
use types::{TupleTypeInfo, Type};
use vm::{VM, Value};

pub struct Quiver {
    type_aliases: HashMap<String, Type>,
    module_loader: Box<dyn ModuleLoader>,
    vm: VM,
}

impl Quiver {
    pub fn new(modules: Option<HashMap<String, String>>) -> Self {
        Self {
            // TODO: constant/function pools?
            type_aliases: HashMap::new(),
            module_loader: match modules {
                Some(modules) => Box::new(InMemoryModuleLoader::new(modules)),
                None => Box::new(FileSystemModuleLoader::new()),
            },
            vm: VM::new(None),
        }
    }

    pub fn evaluate(
        &mut self,
        source: &str,
        module_path: Option<std::path::PathBuf>,
    ) -> Result<Option<Value>, Error> {
        let program = parser::parse(source).map_err(|e| Error::ParseError(Box::new(e)))?;

        let instructions = Compiler::compile(
            program,
            &mut self.type_aliases,
            self.module_loader.as_ref(),
            &mut self.vm,
            module_path,
        )
        .map_err(Error::CompileError)?;

        let result = self
            .vm
            .execute_instructions(instructions)
            .map_err(Error::RuntimeError)?;

        Ok(result)
    }

    pub fn get_stack(&self) -> Vec<Value> {
        self.vm.get_stack()
    }

    pub fn frame_count(&self) -> usize {
        self.vm.frame_count()
    }

    pub fn scope_count(&self) -> usize {
        self.vm.scope_count()
    }

    pub fn list_variables(&self) -> Vec<(String, Value)> {
        self.vm.list_variables()
    }

    pub fn list_types(&self) -> Vec<(String, bytecode::TypeId)> {
        self.vm
            .get_types()
            .iter()
            .map(|(&type_id, (name, _fields))| {
                let display_name = name
                    .as_deref()
                    .unwrap_or(&format!("Type{}", type_id.0))
                    .to_string();
                (display_name, type_id)
            })
            .collect()
    }

    pub fn compile(
        &mut self,
        source: &str,
        module_path: Option<std::path::PathBuf>,
    ) -> Result<bytecode::Bytecode, Error> {
        let program = parser::parse(source).map_err(|e| Error::ParseError(Box::new(e)))?;

        let instructions = Compiler::compile(
            program,
            &mut self.type_aliases,
            self.module_loader.as_ref(),
            &mut self.vm,
            module_path,
        )
        .map_err(Error::CompileError)?;

        let result = self
            .vm
            .execute_instructions(instructions)
            .map_err(Error::RuntimeError)?;

        let entry = match result {
            Some(Value::Function {
                function,
                captures: _,
            }) => Some(function),
            _ => None,
        };

        let bytecode = bytecode::Bytecode {
            constants: self.vm.get_constants().clone(),
            functions: self.vm.get_functions().clone(),
            builtins: self.vm.get_builtins().clone(),
            entry,
            types: self.vm.get_types(),
        };

        Ok(bytecode)
    }

    pub fn execute(&mut self, bytecode: bytecode::Bytecode) -> Result<Option<Value>, Error> {
        // Get entry point before moving bytecode
        let entry = bytecode.entry;

        // Replace VM with a fresh one loaded with bytecode
        self.vm = VM::new(Some(bytecode));

        // Execute entry point if present
        if let Some(entry) = entry {
            self.vm.execute_function(entry).map_err(Error::RuntimeError)
        } else {
            Ok(None)
        }
    }

    pub fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo> {
        self.vm.lookup_type(type_id)
    }

    pub fn get_types(&self) -> &HashMap<TypeId, TupleTypeInfo> {
        self.vm.type_registry().get_types()
    }

    pub fn get_function(&self, function: usize) -> Option<&Function> {
        self.vm.get_functions().get(function)
    }

    pub fn get_binary_bytes(&self, binary_ref: &vm::BinaryRef) -> Result<Vec<u8>, Error> {
        self.vm
            .get_binary_bytes(binary_ref)
            .map(|bytes| bytes.to_vec())
            .map_err(Error::RuntimeError)
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    ParseError(Box<parser::Error>),
    RuntimeError(vm::Error),
    CompileError(compiler::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ParseError(err) => write!(f, "Parse error: {:?}", err),
            Error::RuntimeError(err) => write!(f, "Runtime error: {:?}", err),
            Error::CompileError(err) => write!(f, "Compile error: {:?}", err),
        }
    }
}

impl std::error::Error for Error {}
