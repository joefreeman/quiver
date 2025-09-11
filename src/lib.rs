pub mod ast;
pub mod builtins;
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
    type_aliases: HashMap<String, Vec<types::Type>>,
    module_loader: Box<dyn ModuleLoader>,
    vm: VM,
}

impl Quiver {
    pub fn new(modules: Option<HashMap<String, String>>) -> Self {
        Self {
            // TODO: constant/function pools?
            type_registry: TypeRegistry::new(),
            type_aliases: HashMap::new(),
            module_loader: match modules {
                Some(modules) => Box::new(InMemoryModuleLoader::new(modules)),
                None => Box::new(FileSystemModuleLoader::new()),
            },
            vm: VM::new(),
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
            &mut self.type_registry,
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

    pub fn list_variables(&self) -> Vec<(String, Value)> {
        self.vm.list_variables()
    }

    pub fn list_types(&self) -> Vec<(String, bytecode::TypeId)> {
        self.type_registry
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
            &mut self.type_registry,
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
            tuples: Some(self.type_registry.get_types().clone()),
        };

        Ok(bytecode)
    }

    pub fn execute(&mut self, bytecode: bytecode::Bytecode) -> Result<Option<Value>, Error> {
        for constant in bytecode.constants {
            self.vm.register_constant(constant);
        }

        for function in bytecode.functions {
            self.vm.register_function(function);
        }
        for function in bytecode.builtins {
            self.vm.register_builtin(function);
        }

        if let Some(entry) = bytecode.entry {
            self.vm.execute_function(entry).map_err(Error::RuntimeError)
        } else {
            Ok(None)
        }
    }

    pub fn get_type_info(&self, type_id: &bytecode::TypeId) -> Option<&types::TupleTypeInfo> {
        self.type_registry.lookup_type(type_id)
    }

    pub fn format_value(&self, value: &Value) -> String {
        match value {
            Value::Function { function, .. } => {
                if let Some(func_def) = self.vm.get_functions().get(*function) {
                    if let Some(func_type) = &func_def.function_type {
                        return self
                            .type_registry
                            .format_type(&types::Type::Function(Box::new(func_type.clone())));
                    }
                }
                "<function>".to_string()
            }
            _ => value.format_with_types(&self.type_registry),
        }
    }

    pub fn format_type(&self, type_id: &bytecode::TypeId) -> String {
        self.type_registry
            .format_type(&types::Type::Tuple(*type_id))
    }
}

#[derive(Debug)]
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
