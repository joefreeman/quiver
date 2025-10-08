pub mod repl;
pub mod runtime;

// Re-exports for easier access in tests
pub mod vm {
    pub use quiver_core::error::Error;
    pub use quiver_core::value::Value;
    pub use quiver_core::{ProcessId, ProcessInfo, ProcessStatus};
}

pub mod compiler {
    pub use quiver_compiler::compiler::Error;
}

pub mod parser {
    pub use quiver_compiler::parser::Error;
}

pub use quiver_compiler::compiler::Error as CompilerError;
pub use quiver_compiler::parser::Error as ParserError;

use std::collections::HashMap;

use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::{
    Compiler, FileSystemModuleLoader, InMemoryModuleLoader, ModuleLoader, format, parse,
};
use quiver_core::program::Program;
use quiver_core::types::Type;
use quiver_core::value::Value;
use quiver_core::{ProcessId, ProcessInfo, ProcessStatus};
use runtime::NativeRuntime;

pub struct Quiver {
    type_aliases: HashMap<String, Type>,
    module_cache: ModuleCache,
    module_loader: Box<dyn ModuleLoader>,
    runtime: NativeRuntime,
    repl_process_id: ProcessId,
}

impl Quiver {
    pub fn new(modules: Option<HashMap<String, String>>) -> Self {
        let program = Program::new();
        let executor_count = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4);
        let runtime = NativeRuntime::new(program, executor_count);
        let repl_process_id = runtime
            .spawn_process(true)
            .expect("Failed to spawn REPL process");

        Self {
            type_aliases: HashMap::new(),
            module_cache: ModuleCache::new(),
            module_loader: match modules {
                Some(modules) => Box::new(InMemoryModuleLoader::new(modules)),
                None => Box::new(FileSystemModuleLoader::new()),
            },
            runtime,
            repl_process_id,
        }
    }

    pub fn evaluate(
        &mut self,
        source: &str,
        module_path: Option<std::path::PathBuf>,
        variables: Option<&HashMap<String, (Type, usize)>>,
        parameter_type: Option<Type>,
    ) -> Result<(Option<Value>, Type, HashMap<String, (Type, usize)>), Error> {
        let ast_program = parse(source).map_err(|e| Error::ParseError(Box::new(e)))?;

        // Get current program state before compilation
        let old_program = self.runtime.program().read().unwrap().clone();

        let parameter_type = parameter_type.unwrap_or_else(Type::nil);

        let (instructions, result_type, variables, new_program, new_type_aliases, new_module_cache) =
            Compiler::compile(
                ast_program,
                self.type_aliases.clone(),
                self.module_cache.clone(),
                self.module_loader.as_ref(),
                &old_program,
                module_path,
                variables,
                parameter_type,
            )
            .map_err(Error::CompileError)?;

        // On success: update program and update type_aliases and module_cache
        self.runtime.update_program(new_program);
        self.type_aliases = new_type_aliases;
        self.module_cache = new_module_cache;

        let (result, compacted_variables) = self
            .runtime
            .execute_instructions(instructions, Some(self.repl_process_id), Some(variables))
            .map_err(Error::RuntimeError)?;

        // Variables were compacted as part of execute
        let compacted_variables = compacted_variables.unwrap_or_default();

        Ok((result, result_type, compacted_variables))
    }

    pub fn get_variables(
        &self,
        variables: &HashMap<String, (Type, usize)>,
    ) -> Vec<(String, Value)> {
        // Build ordered list of (name, index) pairs
        let mut var_list: Vec<(String, usize)> = variables
            .iter()
            .map(|(name, (_, index))| (name.clone(), *index))
            .collect();
        var_list.sort_by_key(|(_, index)| *index);

        // Extract just the indices for Runtime lookup
        let indices: Vec<usize> = var_list.iter().map(|(_, index)| *index).collect();

        // Get values and zip back with names
        self.runtime
            .get_locals(self.repl_process_id, &indices)
            .map(|values| {
                var_list
                    .into_iter()
                    .zip(values)
                    .map(|((name, _), value)| (name, value))
                    .collect()
            })
            .unwrap_or_default()
    }

    pub fn get_types(
        &self,
    ) -> HashMap<quiver_core::bytecode::TypeId, quiver_core::types::TupleTypeInfo> {
        self.runtime.program().read().unwrap().get_types()
    }

    pub fn format_value(&self, value: &Value) -> String {
        format::format_value(&self.runtime.executor(), value)
    }

    pub fn format_type(&self, type_def: &Type) -> String {
        format::format_type(&*self.runtime.executor(), type_def)
    }

    pub fn get_process_statuses(&self) -> Result<HashMap<ProcessId, ProcessStatus>, Error> {
        self.runtime
            .get_process_statuses()
            .map_err(Error::RuntimeError)
    }

    pub fn get_process_info(&self, id: ProcessId) -> Result<Option<ProcessInfo>, Error> {
        self.runtime
            .get_process_info(id)
            .map_err(Error::RuntimeError)
    }
}

pub fn compile(
    source: &str,
    module_path: Option<std::path::PathBuf>,
    modules: Option<HashMap<String, String>>,
) -> Result<quiver_core::bytecode::Bytecode, Error> {
    let ast_program = parse(source).map_err(|e| Error::ParseError(Box::new(e)))?;

    let module_loader: Box<dyn ModuleLoader> = match modules {
        Some(modules) => Box::new(InMemoryModuleLoader::new(modules)),
        None => Box::new(FileSystemModuleLoader::new()),
    };

    let type_aliases = HashMap::new();
    let module_cache = ModuleCache::new();
    let program = Program::new();

    let (instructions, _, _, new_program, _, _) = Compiler::compile(
        ast_program,
        type_aliases,
        module_cache,
        module_loader.as_ref(),
        &program,
        module_path,
        None,
        Type::nil(),
    )
    .map_err(Error::CompileError)?;

    // Create a temporary Runtime for execution with the new program
    // Use a single executor for one-time script execution
    let runtime = NativeRuntime::new(new_program, 1);
    let (result, _) = runtime
        .execute_instructions(instructions, None, None)
        .map_err(Error::RuntimeError)?;

    let entry = match result {
        Some(Value::Function(main_func, captures)) => {
            let func_index = if !captures.is_empty() {
                let executor = runtime.executor();
                let program_arc = runtime.program();
                let mut program = program_arc.write().unwrap();
                program.inject_function_captures(main_func, captures, &executor)
            } else {
                main_func
            };
            Some(func_index)
        }
        _ => None,
    };

    Ok(runtime.program().read().unwrap().to_bytecode(entry))
}

#[derive(Debug, PartialEq)]
pub enum Error {
    ParseError(Box<ParserError>),
    RuntimeError(quiver_core::error::Error),
    CompileError(CompilerError),
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
