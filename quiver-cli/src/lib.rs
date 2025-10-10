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

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
        let mut runtime = NativeRuntime::new(program, executor_count);

        let repl_process_id = Rc::new(RefCell::new(None));
        let repl_process_id_clone = repl_process_id.clone();
        runtime.execute(vec![], true, move |result| {
            *repl_process_id_clone.borrow_mut() =
                Some(result.expect("Failed to spawn REPL process"));
        });

        // Wait for callback (during initialization, panic if interrupted)
        runtime
            .wait_for_callbacks()
            .expect("Interrupted during initialization");

        let repl_process_id = repl_process_id
            .borrow()
            .expect("Execute callback not called");

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
    ) -> Result<
        (
            Option<Value>,
            Type,
            HashMap<String, (Type, usize)>,
            Vec<Vec<u8>>,
        ),
        Error,
    > {
        let ast_program = parse(source).map_err(|e| Error::ParseError(Box::new(e)))?;

        // Get current program state before compilation
        let old_program = self.runtime.program().clone();

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

        // Wake the REPL process with new instructions
        let wake_result = Rc::new(RefCell::new(None));
        let wake_result_clone = wake_result.clone();
        self.runtime
            .wake(self.repl_process_id, instructions, move |result| {
                *wake_result_clone.borrow_mut() = Some(result);
            });

        // Wait for wake callback
        self.runtime
            .wait_for_callbacks()
            .map_err(Error::RuntimeError)?;

        wake_result
            .borrow()
            .as_ref()
            .expect("Wake callback not called")
            .clone()
            .map_err(Error::RuntimeError)?;

        // Get the result with its heap data
        let value_result = Rc::new(RefCell::new(None));
        let value_result_clone = value_result.clone();
        self.runtime
            .get_result(self.repl_process_id, move |result| {
                *value_result_clone.borrow_mut() = Some(result);
            });

        // Wait for get_result callback
        self.runtime
            .wait_for_callbacks()
            .map_err(Error::RuntimeError)?;

        let (value, heap_data) = value_result
            .borrow()
            .as_ref()
            .expect("GetResult callback not called")
            .clone()
            .map_err(Error::RuntimeError)?;

        // Compact locals to keep only referenced variables
        // This prevents locals from growing unbounded across REPL evaluations
        if !variables.is_empty() {
            let mut sorted_vars: Vec<_> = variables.iter().collect();
            sorted_vars.sort_by_key(|(_, (_, idx))| *idx);
            let referenced_indices: Vec<usize> =
                sorted_vars.iter().map(|(_, (_, idx))| *idx).collect();

            let compact_result = Rc::new(RefCell::new(None));
            let compact_result_clone = compact_result.clone();
            self.runtime
                .compact_locals(self.repl_process_id, &referenced_indices, move |result| {
                    *compact_result_clone.borrow_mut() = Some(result);
                });

            // Wait for compact_locals callback
            self.runtime
                .wait_for_callbacks()
                .map_err(Error::RuntimeError)?;

            compact_result
                .borrow()
                .as_ref()
                .expect("CompactLocals callback not called")
                .clone()
                .map_err(Error::RuntimeError)?;

            // Build compacted variables map with new indices
            let mut compacted_variables = HashMap::new();
            for (new_idx, (name, (typ, _))) in sorted_vars.into_iter().enumerate() {
                compacted_variables.insert(name.clone(), (typ.clone(), new_idx));
            }

            Ok((Some(value), result_type, compacted_variables, heap_data))
        } else {
            Ok((Some(value), result_type, variables, heap_data))
        }
    }

    pub fn get_variables(
        &mut self,
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
        let locals_result = Rc::new(RefCell::new(None));
        let locals_result_clone = locals_result.clone();
        self.runtime
            .get_locals(self.repl_process_id, &indices, move |result| {
                *locals_result_clone.borrow_mut() = Some(result);
            });

        // Wait for callback (return empty on interrupt since we can't return an error)
        if self.runtime.wait_for_callbacks().is_err() {
            return vec![];
        }

        locals_result
            .borrow()
            .as_ref()
            .expect("GetLocals callback not called")
            .clone()
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
        self.runtime.program().get_types()
    }

    pub fn format_value(&self, value: &Value, heap: &[Vec<u8>]) -> String {
        let constants = self.runtime.program().get_constants();
        format::format_value(value, heap, constants, self.runtime.program())
    }

    pub fn format_type(&self, type_def: &Type) -> String {
        format::format_type(self.runtime.program(), type_def)
    }

    pub fn executor_count(&self) -> usize {
        self.runtime.executor_count()
    }

    pub fn get_process_statuses_for_executor(
        &mut self,
        executor_id: usize,
    ) -> Result<HashMap<ProcessId, ProcessStatus>, Error> {
        let result = Rc::new(RefCell::new(None));
        let result_clone = result.clone();
        self.runtime.get_process_statuses(executor_id, move |r| {
            *result_clone.borrow_mut() = Some(r);
        });

        // Wait for callback
        self.runtime
            .wait_for_callbacks()
            .map_err(Error::RuntimeError)?;

        result
            .borrow()
            .as_ref()
            .expect("GetProcessStatuses callback not called")
            .clone()
            .map_err(Error::RuntimeError)
    }

    pub fn get_process_statuses(&mut self) -> Result<HashMap<ProcessId, ProcessStatus>, Error> {
        let executor_count = self.executor_count();
        let mut all_statuses = HashMap::new();

        for executor_id in 0..executor_count {
            let statuses = self.get_process_statuses_for_executor(executor_id)?;
            all_statuses.extend(statuses);
        }

        Ok(all_statuses)
    }

    pub fn get_process_info(&mut self, id: ProcessId) -> Result<Option<ProcessInfo>, Error> {
        let result = Rc::new(RefCell::new(None));
        let result_clone = result.clone();
        self.runtime.get_process_info(id, move |r| {
            *result_clone.borrow_mut() = Some(r);
        });

        // Wait for callback
        self.runtime
            .wait_for_callbacks()
            .map_err(Error::RuntimeError)?;

        result
            .borrow()
            .as_ref()
            .expect("GetProcessInfo callback not called")
            .clone()
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
    let mut runtime = NativeRuntime::new(new_program, 1);

    // Execute instructions in a new non-persistent process
    let process_id_result = Rc::new(RefCell::new(None));
    let process_id_result_clone = process_id_result.clone();
    runtime.execute(instructions, false, move |result| {
        *process_id_result_clone.borrow_mut() = Some(result);
    });

    // Wait for callback
    runtime.wait_for_callbacks().map_err(Error::RuntimeError)?;

    let process_id = process_id_result
        .borrow()
        .as_ref()
        .expect("Execute callback not called")
        .clone()
        .map_err(Error::RuntimeError)?;

    // Get the result with its heap data
    let value_result = Rc::new(RefCell::new(None));
    let value_result_clone = value_result.clone();
    runtime.get_result(process_id, move |result| {
        *value_result_clone.borrow_mut() = Some(result);
    });

    // Wait for callback
    runtime.wait_for_callbacks().map_err(Error::RuntimeError)?;

    let (value, _heap_data) = value_result
        .borrow()
        .as_ref()
        .expect("GetResult callback not called")
        .clone()
        .map_err(Error::RuntimeError)?;

    let entry = match value {
        Value::Function(main_func, captures) => {
            let func_index = if !captures.is_empty() {
                let executor = runtime.executor();
                let program = runtime.program_mut();
                program.inject_function_captures(main_func, captures, &executor)
            } else {
                main_func
            };
            Some(func_index)
        }
        _ => None,
    };

    Ok(runtime.program().to_bytecode(entry))
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
