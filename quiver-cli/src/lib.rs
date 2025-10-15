pub mod repl;
pub mod runtime;

// Re-exports for easier access
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
pub use quiver_environment::Environment;
pub use runtime::NativeRuntime;

use quiver_compiler::{
    Compiler, FileSystemModuleLoader, InMemoryModuleLoader, ModuleLoader, parse,
};
use quiver_core::program::Program;
use quiver_core::types::Type;
use quiver_core::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::Duration;

/// Wait for all pending callbacks to complete on a native environment.
pub fn wait_for_callbacks_native(
    environment: &mut Environment<NativeRuntime>,
) -> Result<(), quiver_core::error::Error> {
    while environment.has_pending() {
        environment.runtime_mut().poll();
        if environment.process_pending_events() == 0 {
            std::thread::sleep(Duration::from_millis(10));
        }
    }

    // Continue polling for routing events
    let mut idle_iterations = 0;
    while idle_iterations < 5 {
        environment.runtime_mut().poll();
        if environment.process_pending_events() > 0 {
            idle_iterations = 0;
        } else {
            idle_iterations += 1;
            std::thread::sleep(Duration::from_millis(2));
        }
    }

    Ok(())
}

/// Compile a Quiver program to bytecode
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
    let module_cache = quiver_compiler::compiler::ModuleCache::new();
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
    let runtime = NativeRuntime::new();
    let mut environment = Environment::new(runtime, new_program, 1);

    // Execute instructions in a new non-persistent process
    let process_id_result = Rc::new(RefCell::new(None));
    let process_id_result_clone = process_id_result.clone();
    environment.execute(instructions, false, move |result| {
        *process_id_result_clone.borrow_mut() = Some(result);
    });

    wait_for_callbacks_native(&mut environment).map_err(Error::RuntimeError)?;

    let process_id = process_id_result
        .borrow()
        .as_ref()
        .expect("Execute callback not called")
        .clone()
        .map_err(Error::RuntimeError)?;

    // Get the result with its heap data
    let value_result = Rc::new(RefCell::new(None));
    let value_result_clone = value_result.clone();
    environment.get_result(process_id, move |result| {
        *value_result_clone.borrow_mut() = Some(result);
    });

    wait_for_callbacks_native(&mut environment).map_err(Error::RuntimeError)?;

    let (value, _heap_data) = value_result
        .borrow()
        .as_ref()
        .expect("GetResult callback not called")
        .clone()
        .map_err(Error::RuntimeError)?;

    let entry = match value {
        Value::Function(main_func, captures) => {
            let func_index = if !captures.is_empty() {
                let executor = environment.executor();
                let program = environment.program_mut();
                program.inject_function_captures(main_func, captures, &executor)
            } else {
                main_func
            };
            Some(func_index)
        }
        _ => None,
    };

    Ok(environment.program().to_bytecode(entry))
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
