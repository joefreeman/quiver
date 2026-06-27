use clap::{Parser, Subcommand};
use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::{Compiler, ModuleResolver, PackageResolver, parse};
use quiver_core::bytecode;
use quiver_core::format;
use quiver_core::program::Program;
use quiver_core::types::Type;
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

/// Build complete builtin registry including core builtins and network builtins
pub fn build_builtin_registry() -> quiver_core::builtins::BuiltinRegistry<quiver_io::NativeEffect> {
    let mut registry = quiver_core::builtins::BuiltinRegistry::with_modules(
        &quiver_core::builtins::core_modules(),
    );
    // Add I/O builtins from quiver-io
    // Signatures came from `core_modules`; attach the native implementations.
    quiver_io::attach_network_builtins(&mut registry);
    quiver_io::attach_file_builtins(&mut registry);
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

    Format {
        input: Option<String>,

        #[arg(short, long)]
        eval: Option<String>,
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
        Some(Commands::Format { input, eval }) => format_command(input, eval)?,
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

/// Compile source code and extract the entry function, returning a Program and entry index.
/// This handles the common pattern of compiling instructions, executing them to get a function value,
/// and handling captures by injecting them into the program.
/// Build a resolver for an entry program: discover the project from the file's location, or —
/// for inline `--eval`/stdin with no path — the default (stdlib-only) package.
fn entry_resolver(input_path: Option<&str>) -> PackageResolver {
    match input_path {
        Some(path) => PackageResolver::for_entry_file(std::path::Path::new(path)),
        None => PackageResolver::inline(),
    }
}

fn compile_and_extract_entry(
    source: &str,
    resolver: &dyn ModuleResolver,
    builtins: &quiver_core::builtins::BuiltinRegistry<quiver_io::NativeEffect>,
) -> Result<(Program, usize), Box<dyn std::error::Error>> {
    let ast = match parse(source) {
        Ok(ast) => ast,
        Err(e) => handle_parse_error(e, source, "input"),
    };

    let mut program = Program::new();
    let mut module_cache = ModuleCache::new();
    let compilation_result = Compiler::compile(
        ast,
        &HashMap::new(),
        &mut module_cache,
        resolver,
        &mut program,
        quiver_core::types::NIL, // parameter_type_id - use pre-registered nil type
        &HashMap::new(),
        builtins,
        None, // no semantic recorder for the CLI
    )
    .map_err(|e| format!("Compile error: {:?}", e.error))?;

    let instructions = compilation_result.instructions;
    let receive_type = compilation_result.receive_type;

    // Register the callable type for this wrapper function
    // Use the receive type extracted from the program (allows top-level code to receive messages)
    let nil_type_id = program.register_type(Type::nil());
    let callable_type_id = program.register_type(Type::Callable {
        parameter: nil_type_id,
        result: compilation_result.result_type,
        receive: receive_type,
    });

    // Register the instructions as a temporary function
    let function_index = program.register_function(quiver_core::bytecode::Function {
        instructions,
        captures: 0,
        type_id: callable_type_id,
    });

    // Create bytecode with this function as entry
    let bytecode = program.to_bytecode(Some(function_index));

    // Execute to get the function value
    let (result, executor) = quiver_core::execute_bytecode_sync(bytecode, builtins, false)
        .map_err(|e| format!("Execution error: {:?}", e))?;

    // Extract the entry function from the result
    let entry = match result {
        Value::Function(func_index, captures) => {
            if !captures.is_empty() {
                // Inject captures into the program to create a new function
                program.inject_function_captures(func_index, (*captures).clone(), &executor)
            } else {
                func_index
            }
        }
        _ => {
            return Err("Program is not executable. Must evaluate to a function.".into());
        }
    };

    Ok((program, entry))
}

fn compile_command(
    input: Option<String>,
    output: Option<String>,
    eval: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let (source, source_id, resolver_path) = if let Some(code) = eval {
        (code, "eval".to_string(), None)
    } else if let Some(path) = input.clone() {
        (fs::read_to_string(&path)?, path.clone(), Some(path))
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        (buffer, "stdin".to_string(), None)
    };

    // Build registry from core modules and network builtins
    let builtins = build_builtin_registry();
    let resolver = entry_resolver(resolver_path.as_deref());

    // Compile and extract entry function
    // Note: compile_command allows programs that don't evaluate to a function
    let (program, entry) = match compile_and_extract_entry(&source, &resolver, &builtins) {
        Ok((program, entry)) => (program, Some(entry)),
        Err(_) => {
            // If it doesn't evaluate to a function, compile without an entry point
            let ast = match parse(&source) {
                Ok(ast) => ast,
                Err(e) => handle_parse_error(e, &source, &source_id),
            };
            let mut program = Program::new();
            let mut module_cache = ModuleCache::new();
            Compiler::compile(
                ast,
                &HashMap::new(),
                &mut module_cache,
                &resolver,
                &mut program,
                quiver_core::types::NIL, // parameter_type_id
                &HashMap::new(),
                &builtins,
                None, // no semantic recorder for the CLI
            )
            .map_err(|e| format!("Compile error: {:?}", e.error))?;
            (program, None)
        }
    };

    let bytecode = match entry {
        Some(entry_fn) => program.to_bytecode_optimized(entry_fn),
        None => program.to_bytecode(None),
    };
    let json = serde_json::to_string_pretty(&bytecode)?;

    if let Some(output_path) = output {
        fs::write(output_path, json)?;
    } else {
        println!("{}", json);
    }

    Ok(())
}

/// Parse source and print it back as canonical argument-first Quiver source.
fn format_command(
    input: Option<String>,
    eval: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let (source, source_id) = if let Some(code) = eval {
        (code, "eval".to_string())
    } else if let Some(path) = input {
        (fs::read_to_string(&path)?, path)
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        (buffer, "stdin".to_string())
    };

    let ast = match parse(&source) {
        Ok(ast) => ast,
        Err(e) => handle_parse_error(e, &source, &source_id),
    };

    println!("{}", quiver_compiler::format_program(&ast));

    Ok(())
}

fn run_command(
    input: Option<String>,
    eval: Option<String>,
    quiet: bool,
    profile: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(code) = eval {
        compile_execute(&code, None, quiet, profile)?;
    } else if let Some(path) = input {
        let content = fs::read_to_string(&path)?;

        if path.ends_with(".qv") {
            compile_execute(&content, Some(&path), quiet, profile)?;
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
                Err(_) => compile_execute(&buffer, None, quiet, profile)?,
            }
        } else {
            compile_execute(&buffer, None, quiet, profile)?;
        }
    }

    Ok(())
}

fn compile_execute(
    source: &str,
    input_path: Option<&str>,
    quiet: bool,
    profile: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // Build registry from core modules and network builtins
    let builtins = build_builtin_registry();
    let resolver = entry_resolver(input_path);

    // Compile and extract entry function (this will error if not a function)
    let (program, entry) = compile_and_extract_entry(source, &resolver, &builtins)?;

    // Convert to bytecode
    let bytecode = program.to_bytecode_optimized(entry);

    // Execute using shared bytecode execution path
    execute_bytecode_with_environment(bytecode, quiet, profile)
}

/// Execute bytecode using the Environment architecture with multi-worker support and effects.
/// This is the unified execution path for both direct bytecode and compiled source.
fn execute_bytecode_with_environment(
    bytecode: bytecode::Bytecode,
    quiet: bool,
    profile: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if bytecode.entry.is_none() {
        return Err("Bytecode has no entry point".into());
    }

    // Create workers
    let num_workers = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(2);

    let builtins = build_builtin_registry();

    let mut workers: Vec<Box<dyn WorkerHandle<quiver_io::NativeEffect>>> = Vec::new();
    for i in 0..num_workers {
        workers.push(Box::new(spawn_worker(
            || {
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_millis() as u64
            },
            builtins.clone(),
            profile,
            i as u16,
        )));
    }

    // Create environment with effect backend
    let effect_backend = create_effect_backend();
    let mut environment = Environment::<quiver_io::NativeEffect>::new(workers);

    if let Some(backend) = effect_backend {
        environment.set_effect_backend(backend);
    }

    // Extract data before consuming bytecode
    let builtin_names: Vec<String> = bytecode.builtins.iter().map(|b| b.name.clone()).collect();
    let bytecode_for_format = bytecode.clone();

    // Start process from bytecode
    let start_time = std::time::Instant::now();
    let process_id = environment
        .start_process(Some(bytecode))
        .map_err(|e| format!("Failed to start process: {:?}", e))?;

    // Request the result
    let request_id = environment
        .request_result(process_id, None)
        .map_err(|e| format!("Failed to request result: {:?}", e))?;

    // Event loop
    loop {
        let did_work = environment.step().unwrap_or(false);

        match environment.poll_request(request_id) {
            Ok(Some(quiver_environment::RequestResult::Result(Ok((value, heap)), stats))) => {
                let wall_time = start_time.elapsed();

                // Print profiling report if enabled
                if let Some(stats) = stats {
                    print_bytecode_profile_report(&stats, &builtin_names, wall_time);
                }

                // Check if result is NIL tuple (exit with error)
                if value.is_nil() {
                    std::process::exit(1);
                }

                // Print result unless quiet or OK/NIL
                if !quiet && !value.is_ok() && !value.is_nil() {
                    let binary_lookup = format::BytecodeBinaryLookup {
                        constants: &bytecode_for_format.constants,
                        heap: &heap,
                    };
                    println!(
                        "{}",
                        format::format_value(&value, &bytecode_for_format, &binary_lookup)
                    );
                }

                return Ok(());
            }
            Ok(Some(quiver_environment::RequestResult::Result(Err(e), _))) => {
                return Err(format!("Runtime error: {:?}", e).into());
            }
            Ok(Some(_)) => {
                return Err("Unexpected result type".into());
            }
            Ok(None) => {
                if !did_work {
                    std::thread::sleep(std::time::Duration::from_millis(5));
                }
            }
            Err(e) => return Err(format!("Environment error: {:?}", e).into()),
        }
    }
}

fn execute_bytecode(
    bytecode_json: &str,
    quiet: bool,
    profile: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let bytecode: bytecode::Bytecode = serde_json::from_str(bytecode_json)?;

    if bytecode.entry.is_none() {
        return Err("Bytecode has no entry point".into());
    }

    execute_bytecode_with_environment(bytecode, quiet, profile)
}

/// Print a profile report for bytecode execution (without full Program type info).
/// Uses builtin names from the bytecode instead of looking them up in Program.
fn print_bytecode_profile_report(
    stats: &quiver_core::executor::ExecutionStats,
    builtin_names: &[String],
    wall_time: std::time::Duration,
) {
    // Calculate totals
    let total_instr_count: u64 = stats.instruction_stats.values().map(|(c, _)| c).sum();
    let total_instr_time: u64 = stats.instruction_stats.values().map(|(_, t)| t).sum();
    let total_builtin_count: u64 = stats.builtin_stats.values().map(|(c, _)| c).sum();
    let total_builtin_time: u64 = stats.builtin_stats.values().map(|(_, t)| t).sum();

    let wall_ms = wall_time.as_secs_f64() * 1000.0;
    let exec_ms = (total_instr_time + total_builtin_time) as f64 / 1_000_000.0;
    let exec_percent = if wall_ms > 0.0 {
        (exec_ms / wall_ms) * 100.0
    } else {
        0.0
    };

    eprintln!(
        "Total time: {:.2}ms (execution: {:.2}ms; {:.1}%)",
        wall_ms, exec_ms, exec_percent
    );

    // Instructions sorted by time
    if !stats.instruction_stats.is_empty() {
        let mut instrs: Vec<_> = stats.instruction_stats.iter().collect();
        instrs.sort_by_key(|(_, (_, time))| std::cmp::Reverse(*time));

        eprintln!(
            "\nInstructions ({}; {:.2}ms):",
            total_instr_count,
            total_instr_time as f64 / 1_000_000.0
        );
        for (instr_type, (count, time)) in instrs.iter().take(10) {
            let time_percent = if total_instr_time > 0 {
                (*time as f64 / total_instr_time as f64) * 100.0
            } else {
                0.0
            };
            let avg_ns = time.checked_div(*count).unwrap_or(0);
            eprintln!(
                "  {:?}: {} calls, {:.3}ms ({:.1}%), avg {:.0}ns",
                instr_type,
                count,
                *time as f64 / 1_000_000.0,
                time_percent,
                avg_ns
            );
        }
    }

    // Builtins sorted by time
    if !stats.builtin_stats.is_empty() {
        let mut builtins: Vec<_> = stats.builtin_stats.iter().collect();
        builtins.sort_by_key(|(_, (_, time))| std::cmp::Reverse(*time));

        eprintln!(
            "\nBuiltins ({}; {:.2}ms):",
            total_builtin_count,
            total_builtin_time as f64 / 1_000_000.0
        );
        for (builtin_idx, (count, time)) in builtins.iter().take(10) {
            let builtin_name = builtin_names
                .get(**builtin_idx)
                .map(|s| s.as_str())
                .unwrap_or("<unknown>");
            let time_percent = if total_builtin_time > 0 {
                (*time as f64 / total_builtin_time as f64) * 100.0
            } else {
                0.0
            };
            let avg_ns = time.checked_div(*count).unwrap_or(0);
            eprintln!(
                "  {}: {} calls, {:.3}ms ({:.1}%), avg {:.0}ns",
                builtin_name,
                count,
                *time as f64 / 1_000_000.0,
                time_percent,
                avg_ns
            );
        }
    }

    // Memory peaks
    if stats.peak_stack_size > 0 || stats.peak_locals_size > 0 || stats.peak_frame_count > 0 {
        eprintln!("\nMemory peaks:");
        eprintln!("  Stack: {}", stats.peak_stack_size);
        eprintln!("  Locals: {}", stats.peak_locals_size);
        eprintln!("  Frames: {}", stats.peak_frame_count);
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

    // Print tuple type information
    if !bytecode_data.tuples.is_empty() {
        println!("\nTuples:");
        for (index, tuple_info) in bytecode_data.tuples.iter().enumerate() {
            let formatted = format::format_tuple_info(&bytecode_data, tuple_info);
            println!("  {}: {}", index, formatted);
        }
    }

    // Print builtin names
    if !bytecode_data.builtins.is_empty() {
        println!("\nBuiltins:");
        for (index, builtin) in bytecode_data.builtins.iter().enumerate() {
            println!("  {}: {}", index, builtin.name);
        }
    }

    // Print resource names
    if !bytecode_data.resources.is_empty() {
        println!("\nResources:");
        for (index, name) in bytecode_data.resources.iter().enumerate() {
            println!("  {}: {}", index, name);
        }
    }

    for (i, function) in bytecode_data.functions.iter().enumerate() {
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
        format!("0x{}", hex)
    } else {
        let hex: String = bytes[..8].iter().map(|b| format!("{:02x}", b)).collect();
        format!("0x{}… ({} bytes)", hex, bytes.len())
    }
}
