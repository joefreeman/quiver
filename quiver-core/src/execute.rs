use crate::bytecode::Bytecode;
use crate::compatibility::{
    CompatibilityInput, compute_canonical_tuples, compute_field_offsets,
    compute_param_compatibility, compute_type_compatibility,
};
use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::executor::ProgramUpdate;
use crate::process::Action;
use crate::value::Value;

/// Execute bytecode synchronously, returning the result value and executor.
///
/// Computes type_compatibility and parameter_compatibility from the bytecode's
/// type information for O(1) type checking at runtime.
pub fn execute_bytecode_sync<E: Effect>(
    bytecode: Bytecode,
    builtins: &crate::builtins::BuiltinRegistry<E>,
    profile: bool,
) -> Result<(Value, Executor<E>), Error> {
    execute_bytecode_sync_with(bytecode, builtins, profile, true)
}

/// As [`execute_bytecode_sync`], but `param_compat` controls whether parameter-compatibility
/// tables (used only for mailbox message filtering during select/receive) are computed.
///
/// Computing them is O(functions × types) and is the dominant cost of compiling modules,
/// which are executed at compile time purely to produce a value and do not receive messages.
/// Skipping it leaves the tables empty, which `check_message_compatible` treats permissively.
pub fn execute_bytecode_sync_with<E: Effect>(
    bytecode: Bytecode,
    builtins: &crate::builtins::BuiltinRegistry<E>,
    profile: bool,
    param_compat: bool,
) -> Result<(Value, Executor<E>), Error> {
    let entry = bytecode
        .entry
        .ok_or_else(|| Error::InvalidArgument("Bytecode has no entry point".to_string()))?;

    // Use worker_id 0 for single-threaded execution
    let mut executor = Executor::new(builtins.clone(), profile, 0);

    // Compute type compatibility for O(1) runtime type checks
    assert!(
        bytecode.tuples.len() >= 2,
        "Bytecode must have at least NIL and OK tuples"
    );

    let input = CompatibilityInput {
        types: &bytecode.types,
        tuples: &bytecode.tuples,
        functions: &bytecode.functions,
        builtins: &bytecode.builtins,
        resource_names: &bytecode.resources,
    };

    let type_compatibility = compute_type_compatibility(&input);
    let canonical_tuples = compute_canonical_tuples(&bytecode.tuples);
    let field_offsets = compute_field_offsets(&bytecode.field_names, &bytecode.tuples);
    let (function_param_compatibility, builtin_param_compatibility) = if param_compat {
        compute_param_compatibility(&input)
    } else {
        (Vec::new(), Vec::new())
    };

    let program_update = ProgramUpdate {
        constants: bytecode.constants,
        functions: bytecode.functions,
        // Skip first two tuples (NIL and OK) since Executor is pre-initialized with them
        tuples: bytecode.tuples[2..].to_vec(),
        types: bytecode.types,
        builtins: bytecode.builtins,
        resources: bytecode.resources,
        type_compatibility,
        function_param_compatibility,
        builtin_param_compatibility,
        field_offsets,
        canonical_tuples,
        debug: bytecode.debug,
    };

    executor.update_program(program_update);

    let process_id = 0;

    // Spawn a process with the entry function (no captures, nil argument)
    executor.spawn_process(process_id, Some(entry), vec![], Value::nil(), vec![], false)?;

    // Execute until completion
    loop {
        let (did_work, action) = executor.step(1000, 0);

        let process = executor
            .get_process(process_id)
            .ok_or_else(|| Error::InvalidArgument("Process disappeared".to_string()))?;

        if let Some(result) = &process.result {
            match result {
                Ok(value) => {
                    let value = value.clone();
                    // Validate the reference-count wiring against the tracing oracle on every
                    // compile-time/sync execution (debug only). A violation means a value-movement
                    // site is mis-wired; this is the end-to-end check for the refcount work.
                    if cfg!(debug_assertions)
                        && let Err(e) = executor.check_refcounts()
                    {
                        panic!("refcount invariant violated after sync execution: {e}");
                    }
                    return Ok((value, executor));
                }
                Err(e) => return Err(e.clone()),
            }
        }

        // This driver has no action router, so a routing request can never be serviced.
        // Reject the operation precisely at its source instead of stalling (the runtime
        // environment services these; compile-time execution is single-process by design).
        if let Some(action) = action {
            let operation = match action {
                Action::Spawn { .. } => "spawning a process",
                Action::Deliver { .. } => "sending a message",
                Action::Await { .. } => "awaiting a process",
                Action::RequestEffect { .. } => "performing an effect",
                Action::ReadState { .. } => "reading a process's state",
            };
            return Err(Error::OperationNotAllowed {
                operation: operation.to_string(),
                context: "compile-time execution (a program's top level and module bodies run \
                          at compile time — move process and effect work inside the entry \
                          function)"
                    .to_string(),
            });
        }

        // Backstop: a fully idle step with no result means execution can never progress —
        // with every routing request rejected above, the only way here is a receive/select
        // waiting on a message that cannot arrive (no sender can exist, and time is frozen
        // so timeouts never expire).
        if !did_work {
            return Err(Error::OperationNotAllowed {
                operation: "waiting to receive a message that can never arrive".to_string(),
                context: "compile-time execution (a program's top level and module bodies run \
                          at compile time — receive inside the entry function instead)"
                    .to_string(),
            });
        }
    }
}
