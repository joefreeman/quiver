use clap::{Parser, Subcommand};
use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::{Compiler, FileSystemModuleLoader, parse};
use quiver_core::bytecode;
use quiver_core::format;
use quiver_core::program::Program;
use quiver_core::types::{NIL, Type};
use quiver_core::value::Value;
use quiver_environment::{Environment, WorkerHandle};
use std::collections::HashMap;
use std::fs;
use std::io::{self, IsTerminal, Read};

mod diagnostics;
mod native_transport;
mod repl_cli;
use native_transport::spawn_worker;
use repl_cli::ReplCli;

/// Execution result with optional profiling stats
type ExecutionResult = Result<
    (
        Value,
        Vec<Vec<u8>>,
        Option<quiver_core::executor::ExecutionStats>,
    ),
    Box<dyn std::error::Error>,
>;

/// Build complete builtin registry including core builtins and network builtins
pub fn build_builtin_registry() -> quiver_core::builtins::BuiltinRegistry<quiver_io::NativeEffect> {
    let mut registry = quiver_core::builtins::BuiltinRegistry::with_modules(
        &quiver_core::builtins::core_modules(),
    );
    // Add I/O builtins from quiver-io
    quiver_io::register_network_builtins(&mut registry);
    quiver_io::register_file_builtins(&mut registry);
    registry
}

/// Create an effect backend for the new effects system
pub fn create_effect_backend()
-> Option<Box<dyn quiver_core::effects::EffectBackend<E = quiver_io::NativeEffect>>> {
    quiver_io::NativeEffectBackend::new(256)
        .ok()
        .map(|backend| {
            Box::new(backend)
                as Box<dyn quiver_core::effects::EffectBackend<E = quiver_io::NativeEffect>>
        })
}

#[derive(Parser)]
#[command(name = "quiv", version, about = "Quiver CLI")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Repl,

    Compile {
        input: Option<String>,

        #[arg(short, long)]
        output: Option<String>,

        #[arg(short, long)]
        eval: Option<String>,
    },

    Run {
        input: Option<String>,

        #[arg(short, long)]
        eval: Option<String>,

        #[arg(short, long)]
        quiet: bool,

        #[arg(long)]
        profile: bool,
    },

    Inspect {
        input: Option<String>,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    match cli.command {
        Some(Commands::Repl) => run_repl()?,
        Some(Commands::Compile {
            input,
            output,
            eval,
        }) => compile_command(input, output, eval)?,
        Some(Commands::Run {
            input,
            eval,
            quiet,
            profile,
        }) => run_command(input, eval, quiet, profile)?,
        Some(Commands::Inspect { input }) => inspect_command(input)?,
        None => run_repl()?,
    }

    Ok(())
}

fn run_repl() -> Result<(), Box<dyn std::error::Error>> {
    let repl = ReplCli::new()?;
    repl.run()?;
    Ok(())
}

/// Handle parse error with visual formatting if in a TTY, otherwise plain text
fn handle_parse_error(err: quiver_compiler::parser::Error, source: &str, source_id: &str) -> ! {
    // Check if stderr is a terminal and NO_COLOR is not set
    let use_color = std::io::stderr().is_terminal() && std::env::var("NO_COLOR").is_err();

    if use_color {
        // Use ariadne for visual error display
        diagnostics::eprint(&err, source_id, source);
    } else {
        // Plain text fallback
        eprintln!("Error: {}", err);
    }
    std::process::exit(1);
}

fn compile_command(
    input: Option<String>,
    output: Option<String>,
    eval: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let (source, source_id) = if let Some(code) = eval {
        (code, "eval".to_string())
    } else if let Some(path) = input.clone() {
        (fs::read_to_string(&path)?, path)
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        (buffer, "stdin".to_string())
    };

    // Compile source to bytecode
    let module_loader = FileSystemModuleLoader::new();
    let module_path = std::env::current_dir().ok();

    let ast = match parse(&source) {
        Ok(ast) => ast,
        Err(e) => handle_parse_error(e, &source, &source_id),
    };
    // Build registry from core modules and network builtins
    let builtins = build_builtin_registry();
    let result = Compiler::compile(
        ast,
        &HashMap::new(), // No existing bindings
        ModuleCache::new(),
        &module_loader,
        vec![],
        module_path,
        Type::nil(),
        &HashMap::new(), // No process types (not a REPL)
        &builtins,
    )
    .map_err(|e| format!("Compile error: {:?}", e))?;

    let instructions = result.instructions;
    let result_type = result.result_type;
    let mut program = result.program;

    // Execute the instructions synchronously to get the function value
    let (result, executor) =
        quiver_core::execute_instructions_sync(&program, instructions, result_type, &builtins)
            .map_err(|e| format!("Execution error: {:?}", e))?;

    // Extract the entry function from the result
    let entry = match result {
        Value::Function(func_index, captures) => {
            if !captures.is_empty() {
                // Inject captures into the program to create a new function
                Some(program.inject_function_captures(func_index, captures, &executor))
            } else {
                Some(func_index)
            }
        }
        _ => None, // Program didn't evaluate to a function
    };

    let bytecode = program.to_bytecode(entry);
    let json = serde_json::to_string_pretty(&bytecode)?;

    if let Some(output_path) = output {
        fs::write(output_path, json)?;
    } else {
        println!("{}", json);
    }

    Ok(())
}

fn run_command(
    input: Option<String>,
    eval: Option<String>,
    quiet: bool,
    profile: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(code) = eval {
        compile_execute(&code, quiet, profile)?;
    } else if let Some(path) = input {
        let content = fs::read_to_string(&path)?;

        if path.ends_with(".qv") {
            compile_execute(&content, quiet, profile)?;
        } else if path.ends_with(".qx") {
            execute_bytecode(&content, quiet, profile)?;
        } else {
            eprintln!(
                "Error: Unsupported file extension - expected .qv for source or .qx for bytecode."
            );
            std::process::exit(1);
        }
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;

        // Try to parse as bytecode first
        if buffer.trim_start().starts_with('{') {
            match serde_json::from_str::<bytecode::Bytecode>(&buffer) {
                Ok(_) => execute_bytecode(&buffer, quiet, profile)?,
                Err(_) => compile_execute(&buffer, quiet, profile)?,
            }
        } else {
            compile_execute(&buffer, quiet, profile)?;
        }
    }

    Ok(())
}

fn compile_execute(
    source: &str,
    quiet: bool,
    profile: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let module_loader = FileSystemModuleLoader::new();
    let module_path = std::env::current_dir().ok();

    let ast = match parse(source) {
        Ok(ast) => ast,
        Err(e) => handle_parse_error(e, source, "input"),
    };
    // Build registry from core modules and network builtins
    let builtins = build_builtin_registry();
    let compilation_result = Compiler::compile(
        ast,
        &HashMap::new(), // No existing bindings
        ModuleCache::new(),
        &module_loader,
        vec![],
        module_path,
        Type::nil(),
        &HashMap::new(), // No process types (not a REPL)
        &builtins,
    )
    .map_err(|e| format!("Compile error: {:?}", e))?;

    let instructions = compilation_result.instructions;
    let result_type = compilation_result.result_type;
    let mut program = compilation_result.program;

    // Execute the instructions synchronously to get the function value
    let (result, executor) =
        quiver_core::execute_instructions_sync(&program, instructions, result_type, &builtins)
            .map_err(|e| format!("Execution error: {:?}", e))?;

    // Extract the entry function from the result
    let entry = match result {
        Value::Function(func_index, captures) => {
            if !captures.is_empty() {
                // Inject captures into the program to create a new function
                Some(program.inject_function_captures(func_index, captures, &executor))
            } else {
                Some(func_index)
            }
        }
        _ => None, // Program didn't evaluate to a function
    };

    let bytecode = program.to_bytecode(entry);

    if bytecode.entry.is_none() {
        eprintln!("Error: Program is not executable. Must evaluate to a function.");
        std::process::exit(1);
    }

    let json = serde_json::to_string(&bytecode)?;
    execute_bytecode(&json, quiet, profile)
}

fn execute_bytecode(
    bytecode_json: &str,
    quiet: bool,
    profile: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let bytecode_data: bytecode::Bytecode = serde_json::from_str(bytecode_json)?;
    let entry = bytecode_data.entry;
    let program = Program::from_bytecode(bytecode_data);

    if let Some(entry_idx) = entry {
        // Build instructions to call the entry function with NIL
        let instructions = vec![
            bytecode::Instruction::Tuple(NIL),
            bytecode::Instruction::Function(entry_idx),
            bytecode::Instruction::Call,
        ];

        // Get the result type from the entry function
        let result_type = program
            .get_function(entry_idx)
            .map(|f| f.function_type.result.clone())
            .unwrap_or_else(|| Type::Union(vec![])); // Top type as fallback

        let (value, heap, stats_opt) =
            execute_with_environment(&program, instructions, result_type, profile)
                .map_err(|e| format!("Execution error: {:?}", e))?;

        // Print profiling report if enabled
        if let Some(stats) = stats_opt {
            print_profile_report(&stats, &program);
        }

        // Check if result is NIL tuple (exit with error)
        if value.is_nil() {
            std::process::exit(1);
        }

        // Print result unless quiet or OK/NIL
        if !quiet && !value.is_ok() && !value.is_nil() {
            let lookup = format::HeapAndProgramLookup {
                heap: &heap,
                program: &program,
            };
            println!("{}", format::format_value(&value, &lookup, &program));
        }
    }

    Ok(())
}

/// Execute instructions using the Environment architecture with multi-worker support.
/// This is similar to execute_instructions_sync but uses the Environment/worker infrastructure
/// needed for process spawning and other concurrent operations.
fn execute_with_environment(
    program: &Program,
    instructions: Vec<bytecode::Instruction>,
    result_type: Type,
    profile: bool,
) -> ExecutionResult {
    // Create workers
    let num_workers = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(2);

    // Build registry from core modules and network builtins
    let builtins = build_builtin_registry();

    let mut workers: Vec<Box<dyn WorkerHandle<quiver_io::NativeEffect>>> = Vec::new();
    for _ in 0..num_workers {
        workers.push(Box::new(spawn_worker(
            || {
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_millis() as u64
            },
            builtins.clone(),
            profile,
        )));
    }

    // Create environment with effect backend (new system)
    let effect_backend = create_effect_backend();
    let mut environment = Environment::<quiver_io::NativeEffect>::new(workers, builtins);

    // Set the effect backend
    if let Some(backend) = effect_backend {
        environment.set_effect_backend(backend);
    }

    // Create a wrapper function with our instructions
    let mut prog = program.clone();
    let function = bytecode::Function {
        instructions,
        function_type: quiver_core::types::CallableType {
            parameter: Type::nil(),
            result: result_type,
            receive: Type::Union(vec![]), // Bottom type (never)
        },
        captures: vec![],
    };
    let function_index = prog.register_function(function);

    // Create bytecode with this function as the entry point
    let mut bytecode_data = prog.to_bytecode(Some(function_index));
    bytecode_data.entry = Some(function_index);

    // Start the process in the environment
    let process_id = environment
        .start_process(bytecode_data)
        .map_err(|e| format!("Failed to start process: {:?}", e))?;

    // Request the result
    let request_id = environment
        .request_result(process_id)
        .map_err(|e| format!("Failed to request result: {:?}", e))?;

    loop {
        let did_work = environment.step().unwrap_or(false);

        match environment.poll_request(request_id) {
            Ok(Some(quiver_environment::RequestResult::Result(Ok((value, heap))))) => {
                // Collect execution stats if profiling is enabled
                let stats_opt = if profile {
                    let stats_request_id = environment
                        .request_execution_stats()
                        .map_err(|e| format!("Failed to request stats: {:?}", e))?;

                    // Poll for stats result
                    loop {
                        environment.step().unwrap_or(false);
                        match environment.poll_request(stats_request_id) {
                            Ok(Some(quiver_environment::RequestResult::ExecutionStats(stats))) => {
                                break Some(stats);
                            }
                            Ok(Some(_)) => {
                                return Err("Unexpected result type for stats request".into());
                            }
                            Ok(None) => {
                                std::thread::sleep(std::time::Duration::from_millis(1));
                            }
                            Err(e) => {
                                return Err(format!("Stats collection error: {:?}", e).into());
                            }
                        }
                    }
                } else {
                    None
                };

                return Ok((value, heap, stats_opt));
            }
            Ok(Some(quiver_environment::RequestResult::Result(Err(e)))) => {
                return Err(format!("Runtime error: {:?}", e).into());
            }
            Ok(Some(_)) => {
                return Err("Unexpected result type".into());
            }
            Ok(None) => {
                if !did_work {
                    std::thread::sleep(std::time::Duration::from_millis(1));
                }
            }
            Err(e) => return Err(format!("Environment error: {:?}", e).into()),
        }
    }
}

fn print_profile_report(stats: &quiver_core::executor::ExecutionStats, program: &Program) {
    eprintln!("\n=== Execution Profile ===\n");

    // Calculate totals
    let total_instruction_count: u64 = stats
        .instruction_stats
        .values()
        .map(|(count, _)| count)
        .sum();
    let total_instruction_time: u64 = stats.instruction_stats.values().map(|(_, time)| time).sum();
    let total_builtin_count: u64 = stats.builtin_stats.values().map(|(count, _)| count).sum();
    let total_builtin_time: u64 = stats.builtin_stats.values().map(|(_, time)| time).sum();

    eprintln!("Total instructions executed: {}", total_instruction_count);
    eprintln!(
        "Total instruction time: {:.3}ms",
        total_instruction_time as f64 / 1_000_000.0
    );
    eprintln!("Total builtin calls: {}", total_builtin_count);
    eprintln!(
        "Total builtin time: {:.3}ms\n",
        total_builtin_time as f64 / 1_000_000.0
    );

    // Sort instructions by count
    let mut instr_by_count: Vec<_> = stats.instruction_stats.iter().collect();
    instr_by_count.sort_by_key(|(_, (count, _))| std::cmp::Reverse(*count));

    eprintln!("Top instructions by count:");
    for (instr_type, (count, time)) in instr_by_count.iter().take(10) {
        let percent = (*count as f64 / total_instruction_count as f64) * 100.0;
        let avg_ns = if *count > 0 { *time / *count } else { 0 };
        eprintln!(
            "  {:?}: {} ({:.1}%) - avg {:.0}ns",
            instr_type, count, percent, avg_ns
        );
    }

    // Sort instructions by time
    let mut instr_by_time: Vec<_> = stats.instruction_stats.iter().collect();
    instr_by_time.sort_by_key(|(_, (_, time))| std::cmp::Reverse(*time));

    eprintln!("\nTop instructions by total time:");
    for (instr_type, (count, time)) in instr_by_time.iter().take(10) {
        let percent = (*time as f64 / total_instruction_time as f64) * 100.0;
        let avg_ns = if *count > 0 { *time / *count } else { 0 };
        eprintln!(
            "  {:?}: {:.3}ms ({:.1}%) - avg {:.0}ns from {} calls",
            instr_type,
            *time as f64 / 1_000_000.0,
            percent,
            avg_ns,
            count
        );
    }

    // Print builtin stats if any
    if !stats.builtin_stats.is_empty() {
        eprintln!("\nBuiltin statistics:");

        // Sort builtins by count
        let mut builtin_by_count: Vec<_> = stats.builtin_stats.iter().collect();
        builtin_by_count.sort_by_key(|(_, (count, _))| std::cmp::Reverse(*count));

        eprintln!("  Top builtins by count:");
        for (builtin_idx, (count, time)) in builtin_by_count.iter().take(10) {
            let avg_ns = if *count > 0 { *time / *count } else { 0 };
            let builtin_name = program
                .get_builtins()
                .get(**builtin_idx)
                .map(|b| b.name.as_str())
                .unwrap_or("<unknown>");
            eprintln!(
                "    {}: {} calls - avg {:.0}ns",
                builtin_name, count, avg_ns
            );
        }

        // Sort builtins by time
        let mut builtin_by_time: Vec<_> = stats.builtin_stats.iter().collect();
        builtin_by_time.sort_by_key(|(_, (_, time))| std::cmp::Reverse(*time));

        eprintln!("\n  Top builtins by total time:");
        for (builtin_idx, (count, time)) in builtin_by_time.iter().take(10) {
            let avg_ns = if *count > 0 { *time / *count } else { 0 };
            let builtin_name = program
                .get_builtins()
                .get(**builtin_idx)
                .map(|b| b.name.as_str())
                .unwrap_or("<unknown>");
            eprintln!(
                "    {}: {:.3}ms - avg {:.0}ns from {} calls",
                builtin_name,
                *time as f64 / 1_000_000.0,
                avg_ns,
                count
            );
        }
    }

    eprintln!();
}

fn inspect_command(input: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let content = if let Some(path) = input {
        fs::read_to_string(&path)?
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    };

    let bytecode_data: bytecode::Bytecode = serde_json::from_str(&content)?;

    println!("Constants:");
    for (i, constant) in bytecode_data.constants.iter().enumerate() {
        let formatted = match constant {
            bytecode::Constant::Integer(n) => n.to_string(),
            bytecode::Constant::Binary(bytes) => format_binary(bytes),
        };
        println!("  {}: {}", i, formatted);
    }

    let entry = bytecode_data.entry;

    // Create program from bytecode
    let program = Program::from_bytecode(bytecode_data);

    let types_vec = program.get_tuples();
    if !types_vec.is_empty() {
        println!("\nTypes:");
        for (index, _type_info) in types_vec.iter().enumerate() {
            println!(
                "  {}: {}",
                index,
                format::format_type(&program, &Type::Tuple(index))
            );
        }
    }

    for (i, function) in program.get_functions().iter().enumerate() {
        let mut header = format!("\nFunction{}", i);

        if entry == Some(i) {
            header.push('*');
        }

        header.push(':');
        println!("{}", header);

        let max_width = if function.instructions.is_empty() {
            1
        } else {
            format!("{:x}", function.instructions.len() - 1).len()
        };

        for (j, instruction) in function.instructions.iter().enumerate() {
            println!("  {:0width$x}: {:?}", j, instruction, width = max_width);
        }
    }

    Ok(())
}

fn format_binary(bytes: &[u8]) -> String {
    if bytes.len() <= 8 {
        let hex: String = bytes.iter().map(|b| format!("{:02x}", b)).collect();
        format!("'{}'", hex)
    } else {
        let hex: String = bytes[..8].iter().map(|b| format!("{:02x}", b)).collect();
        format!("'{}…'", hex)
    }
}
