use crate::bytecode::Instruction;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::{Frame, ProcessId};
use crate::program::Program;
use crate::value::Value;

/// Execute instructions synchronously and return the result along with the executor.
/// This is a convenience function for compile-time execution where you need
/// a simple, blocking execution model without process management complexity.
///
/// Creates a temporary executor, spawns a single process, executes the instructions,
/// and returns both the result value from the stack and the executor (which may contain
/// heap-allocated data that the value references).
pub fn execute_instructions_sync(
    program: &Program,
    instructions: Vec<Instruction>,
) -> Result<(Option<Value>, Executor), Error> {
    let mut executor = Executor::new(program);

    if instructions.is_empty() {
        return Ok((None, executor));
    }
    let process_id = ProcessId(0);

    // Spawn a process
    executor.spawn_process(process_id, false);

    // Set up the process with the instructions
    {
        let process = executor
            .get_process_mut(process_id)
            .ok_or(Error::InvalidArgument("Process not found".to_string()))?;

        // Push NIL as the initial parameter
        process.stack.push(Value::nil());

        // Create a frame with the instructions
        let frame = Frame::new(instructions, 0, 0);
        process.frames.push(frame);
    }

    // Execute until completion
    loop {
        // Step with a reasonable batch size
        // TODO: check step result
        let _step_result = executor.step(1000)?;

        // Check if the process has completed
        let process = executor
            .get_process(process_id)
            .ok_or(Error::InvalidArgument("Process disappeared".to_string()))?;

        // Check if execution is complete
        let is_complete = process
            .frames
            .last()
            .map_or(true, |frame| frame.counter >= frame.instructions.len());

        if is_complete {
            if !process.stack.is_empty() {
                // Get the result from the stack
                let result = process.stack.last().cloned();
                return Ok((result, executor));
            } else {
                // If the process completed with an empty stack, return None
                return Ok((None, executor));
            }
        }
    }
}
