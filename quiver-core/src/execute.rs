use crate::bytecode::Instruction;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::Frame;
use crate::program::Program;
use crate::value::Value;

/// Execute instructions synchronously and return the result along with the executor.
/// This is a convenience function for compile-time execution where you need
/// a simple, blocking execution model without process management complexity.
///
/// Creates a temporary executor, spawns a single process, executes the instructions,
/// and returns both the result value and the executor (which may contain
/// heap-allocated data that the value references).
pub fn execute_instructions_sync(
    program: &Program,
    instructions: Vec<Instruction>,
    result_type: crate::types::Type,
) -> Result<(Value, Executor), Error> {
    let mut executor = Executor::new();

    // Register the instructions as a temporary function to get a function_index
    let mut functions = program.get_functions().clone();
    let function_index = functions.len();
    functions.push(crate::bytecode::Function {
        instructions,
        function_type: crate::types::CallableType {
            parameter: crate::types::Type::nil(),
            result: result_type,
            receive: crate::types::Type::Union(vec![]), // Bottom type (never)
        },
        captures: vec![],
    });

    // Skip the built-in types (NIL and OK at indices 0 and 1) since Executor::new() already has them
    let types = program.get_types();
    let types_to_send = if types.len() > 2 {
        types[2..].to_vec()
    } else {
        vec![]
    };

    executor.update_program(
        program.get_constants().clone(),
        functions,
        types_to_send,
        program.get_builtins().clone(),
    );

    let process_id = 0;

    // Spawn a process with the temporary function
    executor.spawn_process(process_id, function_index, false);

    // Set up the process with the instructions
    {
        let process = executor
            .get_process_mut(process_id)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Push NIL as the initial parameter
        process.stack.push(Value::nil());

        // Create a frame with the function index
        let frame = Frame::new(function_index, 0, 0);
        process.frames.push(frame);
    }

    // Execute until completion
    loop {
        // Step with a reasonable batch size
        // TODO: check step result
        // Use time 0 since compile-time execution doesn't need real time tracking
        let _step_result = executor.step(1000, 0);

        // Check if the process has completed
        let process = executor
            .get_process(process_id)
            .ok_or(Error::InvalidArgument("Process disappeared".to_string()))?;

        // Check if execution is complete by looking at process.result
        if let Some(result) = &process.result {
            match result {
                Ok(value) => return Ok((value.clone(), executor)),
                Err(e) => return Err(e.clone()),
            }
        }
    }
}
