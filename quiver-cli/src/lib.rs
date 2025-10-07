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
use quiver_compiler::{Compiler, FileSystemModuleLoader, InMemoryModuleLoader, ModuleLoader};
use quiver_compiler::{format, parse};
use quiver_core::bytecode::{Constant, Function, Instruction};
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
        let runtime = NativeRuntime::new(program);
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
        parameter: Option<(&Value, Type)>,
    ) -> Result<(Option<Value>, Type, HashMap<String, (Type, usize)>), Error> {
        let ast_program = parse(source).map_err(|e| Error::ParseError(Box::new(e)))?;

        // Get parameter type
        let parameter_type = parameter
            .as_ref()
            .map(|(_, typ)| typ.clone())
            .unwrap_or_else(Type::nil);

        // Get current program state before compilation
        let old_program = self.runtime.program().read().unwrap().clone();

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
            .execute_instructions(
                instructions,
                parameter.map(|(v, _)| v.clone()),
                Some(self.repl_process_id),
                Some(variables),
            )
            .map_err(Error::RuntimeError)?;

        // Variables were compacted as part of execute
        let compacted_variables = compacted_variables.unwrap_or_default();

        Ok((result, result_type, compacted_variables))
    }

    pub fn get_variables(
        &self,
        variables: &HashMap<String, (Type, usize)>,
    ) -> Vec<(String, Value)> {
        // Extract just the indices for Runtime lookup
        let var_indices: HashMap<String, usize> = variables
            .iter()
            .map(|(name, (_, index))| (name.clone(), *index))
            .collect();

        self.runtime
            .get_variables(self.repl_process_id, &var_indices)
            .map(|map| map.into_iter().collect())
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
    let runtime = NativeRuntime::new(new_program);
    let (result, _) = runtime
        .execute_instructions(instructions, None, None, None)
        .map_err(Error::RuntimeError)?;

    let entry = match result {
        Some(Value::Function(main_func, captures)) => {
            let func_index = if !captures.is_empty() {
                inject_function_captures(&runtime, main_func, captures)
            } else {
                main_func
            };
            Some(func_index)
        }
        _ => None,
    };

    Ok(runtime.program().read().unwrap().to_bytecode(entry))
}

/// Inject function captures into a function, returning a new Function
fn inject_function_captures(
    runtime: &NativeRuntime,
    function_index: usize,
    captures: Vec<Value>,
) -> usize {
    let program_arc = runtime.program();
    let program = program_arc.read().unwrap();

    let mut instructions = Vec::new();
    instructions.push(Instruction::Allocate(captures.len()));

    for (i, capture_value) in captures.iter().enumerate() {
        instructions.extend(value_to_instructions(runtime, capture_value));
        instructions.push(Instruction::Store(i));
    }

    let func = program
        .get_function(function_index)
        .expect("Function should exist during capture injection");
    instructions.extend(func.instructions.clone());
    let function_type = func.function_type.clone();

    drop(program);

    runtime.register_function(Function {
        instructions,
        function_type,
        captures: Vec::new(),
    })
}

/// Convert a runtime value to instructions that reconstruct it
fn value_to_instructions(runtime: &NativeRuntime, value: &Value) -> Vec<Instruction> {
    match value {
        Value::Integer(n) => {
            let program_arc = runtime.program();
            let mut program = program_arc.write().unwrap();
            let const_idx = program.register_constant(Constant::Integer(*n));
            vec![Instruction::Constant(const_idx)]
        }
        Value::Binary(bin_ref) => {
            // Get the binary bytes using the executor (handles both Constant and Heap cases)
            let executor = runtime.executor();
            let bytes = executor
                .get_binary_bytes(bin_ref)
                .expect("Binary should be valid during capture injection")
                .to_vec();
            drop(executor);

            let program_arc = runtime.program();
            let mut program = program_arc.write().unwrap();
            let const_idx = program.register_constant(Constant::Binary(bytes));
            vec![Instruction::Constant(const_idx)]
        }
        Value::Tuple(type_id, elements) => {
            let mut instrs = Vec::new();
            for elem in elements {
                instrs.extend(value_to_instructions(runtime, elem));
            }
            instrs.push(Instruction::Tuple(*type_id));
            instrs
        }
        Value::Function(function, captures) => {
            let func_index = if !captures.is_empty() {
                inject_function_captures(runtime, *function, captures.clone())
            } else {
                *function
            };
            vec![Instruction::Function(func_index)]
        }
        Value::Builtin(name) => {
            let program_arc = runtime.program();
            let mut program = program_arc.write().unwrap();
            let builtin_idx = program.register_builtin(name.clone());
            vec![Instruction::Builtin(builtin_idx)]
        }
        Value::Pid(_) => {
            panic!("Cannot convert pid to instructions")
        }
    }
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
