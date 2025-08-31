pub mod ast;
pub mod bytecode;
pub mod compiler;
pub mod modules;
pub mod parser;
pub mod types;
pub mod vm;

use std::collections::HashMap;

use compiler::Compiler;
use modules::{FileSystemModuleLoader, InMemoryModuleLoader, ModuleLoader};
use types::TypeRegistry;
use vm::{VM, Value};

pub struct Quiver {
    type_registry: TypeRegistry,
    module_loader: Box<dyn ModuleLoader>,
    vm: VM,
}

impl Quiver {
    pub fn new(modules: Option<HashMap<String, String>>) -> Self {
        Self {
            // TODO: constant/function pools?
            type_registry: TypeRegistry::new(),
            module_loader: match modules {
                Some(modules) => Box::new(InMemoryModuleLoader::new(modules)),
                None => Box::new(FileSystemModuleLoader::new()),
            },
            vm: VM::new(),
        }
    }

    pub fn evaluate(&mut self, source: &str) -> Result<Option<Value>, Error> {
        let program = parser::parse(source).map_err(Error::ParseError)?;

        let instructions = Compiler::compile(
            program,
            &mut self.type_registry,
            self.module_loader.as_ref(),
            &mut self.vm,
            None,
        )
        .map_err(Error::CompileError)?;

        let result = self
            .vm
            .execute_instructions(instructions, false)
            .map_err(Error::RuntimeError)?;

        Ok(result)
    }

    pub fn list_variables(&self) -> Vec<(String, Value)> {
        self.vm.list_variables()
    }

    pub fn list_type_aliases(&self) -> Vec<(String, bytecode::TypeId)> {
        // self.compiler.list_type_aliases()
        todo!()
    }

    pub fn compile_to_bytecode(
        &mut self,
        source: &str,
        use_filesystem: bool,
    ) -> Result<bytecode::Bytecode, Error> {
        let program = parser::parse(source).map_err(Error::ParseError)?;

        let instructions = Compiler::compile(
            program,
            &mut self.type_registry,
            self.module_loader.as_ref(),
            &mut self.vm,
            None, // TODO: module path
        )
        .map_err(Error::CompileError)?;

        let bytecode = bytecode::Bytecode {
            constants: Vec::new(), // TODO: extract constants from VM
            functions: Vec::new(), // TODO: extract functions from VM
            entry: None,           // TODO: set entry point if needed
        };

        Ok(bytecode)
    }
}

#[derive(Debug)]
pub enum Error {
    ParseError(parser::Error),
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
