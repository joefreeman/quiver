use crate::bytecode::Instruction;
use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::program::Program;
use crate::value::Value;

/// Execute instructions synchronously and return the result along with the executor.
/// This is a convenience function for compile-time execution where you need
/// a simple, blocking execution model without process management complexity.
///
/// Creates a temporary executor, spawns a single process, executes the instructions,
/// and returns both the result value and the executor (which may contain
/// heap-allocated data that the value references).
pub fn execute_instructions_sync<E: Effect>(
    program: &Program,
    instructions: Vec<Instruction>,
    result_type: crate::types::Type,
    builtins: &crate::builtins::BuiltinRegistry<E>,
) -> Result<(Value, Executor<E>), Error> {
    let mut executor = Executor::new(builtins.clone(), false);

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
    let tuples = program.get_tuples();
    let tuples_to_send = if tuples.len() > 2 {
        tuples[2..].to_vec()
    } else {
        vec![]
    };

    executor.update_program(
        program.get_constants().clone(),
        functions,
        tuples_to_send,
        program.get_types().clone(),
        program.get_builtins().clone(),
    );

    let process_id = 0;

    // Spawn a process with the temporary function (no captures, nil argument)
    executor.spawn_process(
        process_id,
        function_index,
        vec![],
        Value::nil(),
        vec![],
        false,
    )?;

    // Execute until completion
    loop {
        // Step with a reasonable batch size
        // Use time 0 since compile-time execution doesn't need real time tracking
        let (_did_work, _action) = executor.step(1000, 0);

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
