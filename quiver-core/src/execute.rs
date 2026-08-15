use crate::bytecode::Bytecode;
use crate::compatibility::{
    CompatibilityInput, compute_canonical_tuples, compute_field_offsets,
    compute_param_compatibility, compute_type_compatibility,
};
use crate::effects::Effect;
use crate::error::{Error, Operation};
use crate::executor::Executor;
use crate::executor::{ProgramUpdate, TableUpdate};
use crate::process::Action;
use crate::value::Value;
use crate::wire::WireValue;
use std::sync::Arc;

/// Execute bytecode synchronously, returning the result value and executor.
///
/// Computes type_compatibility and parameter_compatibility from the bytecode's
/// type information for O(1) type checking at runtime.
pub fn execute_bytecode_sync<E: Effect>(
    bytecode: Bytecode,
    builtins: &crate::builtins::BuiltinRegistry<E>,
) -> Result<(Value, Executor<E>), Error> {
    execute_bytecode_sync_with(bytecode, builtins, true, u64::MAX, None)
}

/// As [`execute_bytecode_sync`], but `param_compat` controls whether parameter-compatibility
/// tables (used only for mailbox message filtering during select/receive) are computed,
/// and execution is bounded:
///
/// Computing them is O(functions × types) and is the dominant cost of compiling modules,
/// which are executed at compile time purely to produce a value and do not receive messages.
/// Skipping it leaves the tables empty, which `check_message_compatible` treats permissively.
///
/// `fuel` is the step budget in executor units; running past it answers
/// [`Error::ExhaustedAtCompileTime`], the bound that turns an infinite loop at a
/// module's top level into an error instead of a hang. `cancel` is polled between
/// execution slices; a host sets it to abandon the evaluation, answering
/// [`Error::CancelledAtCompileTime`].
pub fn execute_bytecode_sync_with<E: Effect>(
    bytecode: Bytecode,
    builtins: &crate::builtins::BuiltinRegistry<E>,
    param_compat: bool,
    fuel: u64,
    cancel: Option<&std::sync::atomic::AtomicBool>,
) -> Result<(Value, Executor<E>), Error> {
    let entry = bytecode
        .entry
        .ok_or_else(|| Error::InvalidArgument("Bytecode has no entry point".to_string()))?;

    // Use worker_id 0 for single-threaded execution
    let mut executor = Executor::new(builtins.clone(), 0);
    // Compile-time execution must be deterministic: host-state reads (clock, entropy)
    // are rejected at the builtin dispatch site.
    executor.compile_time = true;

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
        field_names: &bytecode.field_names,
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
        // Appended, not shared: this drives one executor that is already pre-seeded with the
        // NIL and OK tuples, so it takes what is new rather than a whole table. Sharing buys
        // nothing here — there is no second worker to share with.
        constants: TableUpdate::Appended(bytecode.constants),
        functions: TableUpdate::Appended(bytecode.functions),
        // Skip first two tuples (NIL and OK) since Executor is pre-initialized with them
        tuples: TableUpdate::Appended(bytecode.tuples[2..].to_vec()),
        types: TableUpdate::Appended(bytecode.types),
        builtins: TableUpdate::Appended(bytecode.builtins),
        resources: bytecode.resources,
        // Whole tables: this executor applies exactly one update, from empty.
        compatibility: crate::executor::CompatibilityUpdate::Shared {
            type_compatibility: Arc::new(type_compatibility),
            function_params: Arc::new(function_param_compatibility),
            builtin_params: Arc::new(builtin_param_compatibility),
            canonical_tuples: Arc::new(canonical_tuples),
            field_offsets: Arc::new(field_offsets),
        },
        debug: bytecode.debug,
        // The sync driver rejects spawn/send/await, so no runtime-delivered values
        // arrive here: a select timeout falls back to a bare nil, and stream arms
        // are rejected as actions.
        runtime: None,
        patched_functions: Vec::new(),
        patched_constants: Vec::new(),
    };

    executor.update_program(program_update);

    let process_id = 0;

    // Spawn a process with the entry function (no captures, nil argument)
    executor.spawn_process(process_id, Some(entry), vec![], WireValue::nil(), false)?;

    // Execute until completion
    const SLICE: u64 = 1000;
    let mut spent: u64 = 0;
    loop {
        if let Some(cancel) = cancel
            && cancel.load(std::sync::atomic::Ordering::Relaxed)
        {
            return Err(Error::CancelledAtCompileTime);
        }
        if spent >= fuel {
            return Err(Error::ExhaustedAtCompileTime);
        }
        spent = spent.saturating_add(SLICE);

        let (did_work, action) = executor.step(SLICE as usize, 0);

        let process = executor
            .get_process(process_id)
            .ok_or_else(|| Error::InvalidArgument("Process disappeared".to_string()))?;

        if let Some(result) = &process.result {
            match result {
                Ok(value) => {
                    let value = value.clone();
                    return Ok((value, executor));
                }
                Err(e) => return Err((**e).clone()),
            }
        }

        // This driver has no action router, so a routing request can never be serviced.
        // Reject the operation precisely at its source instead of stalling (the runtime
        // environment services these; compile-time execution is single-process by design).
        if let Some(action) = action {
            let operation = match action {
                Action::Spawn { .. } => Operation::Spawn,
                Action::Deliver { .. } => Operation::Send,
                Action::Await { .. } => Operation::Await,
                Action::RequestEffect { .. } => Operation::Effect,
                Action::ReadState { .. } => Operation::ReadState,
                Action::Kill { .. } => Operation::Kill,
                Action::Link { .. } => Operation::Link,
            };
            return Err(Error::UnsupportedAtCompileTime { operation });
        }

        // Backstop: a fully idle step with no result means execution can never progress —
        // with every routing request rejected above, the only way here is a receive/select
        // waiting on a message that cannot arrive (no sender can exist, and time is frozen
        // so timeouts never expire).
        if !did_work {
            return Err(Error::StalledAtCompileTime);
        }
    }
}
