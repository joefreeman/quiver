pub mod ast;
pub mod bytecode;
pub mod compiler;
pub mod modules;
pub mod parser;
pub mod vm;

use compiler::Compiler;
use vm::{VM, Value};
use modules::{InMemoryModuleLoader, FileSystemModuleLoader};

pub struct Quiver {
    vm: VM,
    compiler: Compiler,
}

impl Quiver {
    pub fn new() -> Self {
        Self {
            vm: VM::new(),
            compiler: Compiler::new(),
        }
    }

    pub fn evaluate(&mut self, source: &str) -> Result<Option<Value>, Error> {
        // Parse the source code
        let program = parser::parse(source).map_err(Error::ParseError)?;
        
        // Create a module loader (using in-memory for now)
        let module_loader = Box::new(InMemoryModuleLoader::new());
        
        // Compile to bytecode
        let instructions = self.compiler.compile(&program, &mut self.vm, module_loader, None)
            .map_err(Error::CompileError)?;
        
        // Execute the bytecode
        let result = self.vm.execute_instructions(instructions)
            .map_err(Error::RuntimeError)?;
        
        Ok(result)
    }
    
    pub fn list_variables(&self) -> Vec<(String, Value)> {
        self.vm.list_variables()
    }
    
    pub fn list_type_aliases(&self) -> Vec<(String, bytecode::TypeId)> {
        self.compiler.list_type_aliases()
    }
    
    pub fn compile_to_bytecode(&mut self, source: &str, use_filesystem: bool) -> Result<bytecode::Bytecode, Error> {
        // Parse the source code
        let program = parser::parse(source).map_err(Error::ParseError)?;
        
        // Create appropriate module loader
        let module_loader: Box<dyn modules::ModuleLoader> = if use_filesystem {
            Box::new(FileSystemModuleLoader::new())
        } else {
            Box::new(InMemoryModuleLoader::new())
        };
        
        // Compile to instructions
        let _instructions = self.compiler.compile(&program, &mut self.vm, module_loader, None)
            .map_err(Error::CompileError)?;
        
        // Create bytecode structure
        let bytecode = bytecode::Bytecode {
            constants: Vec::new(), // TODO: extract constants from VM
            functions: Vec::new(),  // TODO: extract functions from VM
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
            Error::ParseError(err) => write!(f, "Parse error: {}", err),
            Error::RuntimeError(err) => write!(f, "Runtime error: {:?}", err),
            Error::CompileError(err) => write!(f, "Compile error: {}", err),
        }
    }
}

impl std::error::Error for Error {}
