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
    vm: VM,
    repl_process_id: vm::ProcessId,
}

impl Quiver {
    pub fn new(modules: Option<HashMap<String, String>>) -> Self {
        let program = Program::new();
        let mut vm = VM::new(program);
        let repl_process_id = vm.spawn_process(true);

        Self {
            type_aliases: HashMap::new(),
            module_loader: match modules {
                Some(modules) => Box::new(InMemoryModuleLoader::new(modules)),
                None => Box::new(FileSystemModuleLoader::new()),
            },
            vm,
            repl_process_id,
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

        let existing_locals = if let Some(vars) = variables {
            self.vm
                .get_variables(self.repl_process_id, vars)
                .ok()
                .map(|vals_map| {
                    // Build a Vec with values in order of their indices
                    let max_index = vars.values().copied().max().unwrap_or(0);
                    let mut result = vec![Value::nil(); max_index + 1];
                    for (name, &index) in vars {
                        if let Some(value) = vals_map.get(name) {
                            result[index] = value.clone();
                        }
                    }
                    result
                })
        } else {
            None
        };

        let (instructions, variables) = Compiler::compile(
            ast_program,
            &mut self.type_aliases,
            self.module_loader.as_ref(),
            self.vm.program_mut(),
            module_path,
            variables,
            existing_locals.as_deref(),
            parameter,
        )
        .map_err(Error::CompileError)?;

        let result = self
            .vm
            .execute_instructions(instructions, parameter.cloned(), Some(self.repl_process_id))
            .map_err(Error::RuntimeError)?;

        // Compact locals and get updated variable indices
        let compacted_variables = self.vm.cleanup_locals(self.repl_process_id, &variables);

        Ok((result, compacted_variables))
    }

    pub fn get_variables(&self, variables: &HashMap<String, usize>) -> Vec<(String, Value)> {
        self.vm
            .get_variables(self.repl_process_id, variables)
            .map(|map| map.into_iter().collect())
            .unwrap_or_default()
    }

    pub fn get_types(&self) -> HashMap<bytecode::TypeId, types::TupleTypeInfo> {
        self.vm.program().get_types()
    }

    pub fn format_value(&self, value: &Value) -> String {
        format::format_value(self.vm.program(), value)
    }

    pub fn format_type(&self, type_def: &Type) -> String {
        format::format_type(self.vm.program(), type_def)
    }

    pub fn get_process_statuses(&self) -> HashMap<vm::ProcessId, vm::ProcessStatus> {
        self.vm.get_process_statuses()
    }
}

pub fn compile(
    source: &str,
    module_path: Option<std::path::PathBuf>,
    modules: Option<HashMap<String, String>>,
) -> Result<bytecode::Bytecode, Error> {
    let ast_program = parser::parse(source).map_err(|e| Error::ParseError(Box::new(e)))?;

    let module_loader: Box<dyn ModuleLoader> = match modules {
        Some(modules) => Box::new(InMemoryModuleLoader::new(modules)),
        None => Box::new(FileSystemModuleLoader::new()),
    };

    let mut type_aliases = HashMap::new();
    let mut program = Program::new();

    let (instructions, _) = Compiler::compile(
        ast_program,
        &mut type_aliases,
        module_loader.as_ref(),
        &mut program,
        module_path,
        None,
        None,
        None,
    )
    .map_err(Error::CompileError)?;

    // Create a temporary VM for execution
    let mut vm = VM::new(program);
    let result = vm
        .execute_instructions(instructions, None, None)
        .map_err(Error::RuntimeError)?;

    let entry = match result {
        Some(Value::Function(main_func, captures)) => {
            if !captures.is_empty() {
                vm.program_mut()
                    .inject_function_captures(main_func, captures);
            }
            Some(main_func)
        }
        _ => None,
    };

    Ok(vm.program().to_bytecode(entry))
}

pub fn execute(bytecode: bytecode::Bytecode) -> Result<Option<Value>, Error> {
    // Get entry point before moving bytecode
    let entry = bytecode.entry;

    // Create program and VM from bytecode
    let program = Program::from_bytecode(bytecode);
    let mut vm = VM::new(program);

    // Execute entry point if present
    if let Some(entry) = entry {
        vm.execute_function(entry).map_err(Error::RuntimeError)
    } else {
        Ok(None)
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
