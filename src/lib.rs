pub mod ast;
pub mod builtins;
pub mod bytecode;
pub mod compiler;
pub mod format;
pub mod modules;
pub mod parser;
pub mod program;
pub mod types;
pub mod vm;

use std::collections::HashMap;

use compiler::Compiler;
use modules::{FileSystemModuleLoader, InMemoryModuleLoader, ModuleLoader};
use program::Program;
use types::Type;
use vm::{VM, Value};

pub struct Quiver {
    type_aliases: HashMap<String, Type>,
    module_loader: Box<dyn ModuleLoader>,
    program: Program,
}

impl Quiver {
    pub fn new(modules: Option<HashMap<String, String>>) -> Self {
        Self {
            type_aliases: HashMap::new(),
            module_loader: match modules {
                Some(modules) => Box::new(InMemoryModuleLoader::new(modules)),
                None => Box::new(FileSystemModuleLoader::new()),
            },
            program: Program::new(),
        }
    }

    pub fn evaluate(
        &mut self,
        source: &str,
        module_path: Option<std::path::PathBuf>,
        variables: Option<&HashMap<String, usize>>,
        parameter: Option<&Value>,
    ) -> Result<(Option<Value>, HashMap<String, usize>), Error> {
        let ast_program = parser::parse(source).map_err(|e| Error::ParseError(Box::new(e)))?;

        // Create a temporary VM for getting existing variable values
        let temp_vm = VM::new(&self.program);
        let existing_locals = if let Some(vars) = variables {
            temp_vm
                .get_variables(vars)
                .map(|vals| vals.values().cloned().collect::<Vec<_>>())
                .ok()
        } else {
            None
        };

        let (instructions, variables) = Compiler::compile(
            ast_program,
            &mut self.type_aliases,
            self.module_loader.as_ref(),
            &mut self.program,
            module_path,
            variables,
            existing_locals.as_deref(),
            parameter,
        )
        .map_err(Error::CompileError)?;

        // Create a fresh VM for execution
        let mut vm = VM::new(&self.program);
        let result = vm
            .execute_instructions(instructions, parameter.cloned())
            .map_err(Error::RuntimeError)?;

        // Compact locals and get updated variable indices
        let compacted_variables = vm.cleanup_locals(&variables);

        Ok((result, compacted_variables))
    }

    pub fn compile(
        &mut self,
        source: &str,
        module_path: Option<std::path::PathBuf>,
    ) -> Result<bytecode::Bytecode, Error> {
        let ast_program = parser::parse(source).map_err(|e| Error::ParseError(Box::new(e)))?;

        let (instructions, _) = Compiler::compile(
            ast_program,
            &mut self.type_aliases,
            self.module_loader.as_ref(),
            &mut self.program,
            module_path,
            None,
            None,
            None,
        )
        .map_err(Error::CompileError)?;

        // Create a temporary VM for execution
        let mut vm = VM::new(&self.program);
        let result = vm
            .execute_instructions(instructions, None)
            .map_err(Error::RuntimeError)?;

        let entry = match result {
            Some(Value::Function(main_func, captures)) => {
                if !captures.is_empty() {
                    self.program.inject_function_captures(main_func, captures);
                }
                Some(main_func)
            }
            _ => None,
        };

        Ok(self.program.to_bytecode(entry))
    }

    pub fn execute(&mut self, bytecode: bytecode::Bytecode) -> Result<Option<Value>, Error> {
        // Get entry point before moving bytecode
        let entry = bytecode.entry;

        // Replace program with one loaded from bytecode
        self.program = Program::from_bytecode(bytecode);

        // Execute entry point if present
        if let Some(entry) = entry {
            let mut vm = VM::new(&self.program);
            vm.execute_function(entry).map_err(Error::RuntimeError)
        } else {
            Ok(None)
        }
    }

    pub fn get_variables(&self, variables: &HashMap<String, usize>) -> Vec<(String, Value)> {
        // Variables are stored in VM locals, which is per-instance
        // We'd need to create a temporary VM to access them
        let vm = VM::new(&self.program);
        vm.get_variables(variables)
            .map(|map| map.into_iter().collect())
            .unwrap_or_default()
    }

    pub fn get_types(&self) -> HashMap<bytecode::TypeId, types::TupleTypeInfo> {
        self.program.get_types()
    }

    pub fn format_value(&self, value: &Value) -> String {
        format::format_value(&self.program, value)
    }

    pub fn format_type(&self, type_def: &Type) -> String {
        format::format_type(&self.program, type_def)
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
