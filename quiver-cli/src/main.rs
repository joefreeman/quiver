use clap::{Parser, Subcommand};
use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::{Compiler, PackageResolver, parse};
use quiver_core::bytecode;
use quiver_core::format;
use quiver_core::program::Program;
use quiver_core::types::Type;
use quiver_environment::{Environment, WorkerHandle};
use std::collections::HashMap;
use std::fs;
use std::io::{self, IsTerminal, Read};

mod diagnostics;
use quiver_cli::{build_builtin_registry, create_effect_backend};
mod repl_cli;
mod server_cli;
mod test_cli;
use quiver_cli::native_transport;
use quiver_cli::spawn_worker;
use repl_cli::ReplCli;

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

        /// Debug build: stamp nil results with failure provenance (`origin` annotations).
        #[arg(long)]
        debug: bool,
    },

    Run {
        input: Option<String>,

        #[arg(short, long)]
        eval: Option<String>,

        #[arg(short, long)]
        quiet: bool,

        #[arg(long)]
        profile: bool,

        /// Release build: skip failure-provenance stamps (`run` compiles debug by default).
        #[arg(long)]
        release: bool,
    },

    Inspect {
        input: Option<String>,
    },

    /// Format Quiver source. With file arguments, rewrites each in place; with none, reads stdin
    /// and writes the result to stdout (as does `--eval`).
    Format {
        /// Files to format in place.
        input: Vec<String>,

        #[arg(short, long)]
        eval: Option<String>,

        /// Don't write anything; exit non-zero if any input is not already formatted (for CI).
        #[arg(long)]
        check: bool,
    },

    /// Run the Quiver code embedded in a Markdown document, checking its `//=>` assertions.
    /// Each `##` chapter is one accumulating session, and exits non-zero if any check fails.
    Test {
        /// Markdown documents to run.
        input: Vec<String>,
    },

    /// Run the persistent server: one shared environment that client sessions connect
    /// to over a unix socket. Foreground; stop with `quiv server stop` or SIGTERM.
    /// (`quiv run` and `quiv repl` spawn one automatically when none is listening.)
    Server {
        #[command(subcommand)]
        action: Option<ServerAction>,

        /// Listen on this socket path instead of the per-user default.
        #[arg(long)]
        socket: Option<String>,

        /// Include a code phase in a reclamation round after this many function/
        /// constant registrations (a testing/tuning knob).
        #[arg(long)]
        code_collection_threshold: Option<usize>,
    },
}

#[derive(Subcommand)]
enum ServerAction {
    /// Whether a server is listening, and which build it is.
    Status,
    /// Stop the server (its sessions' processes die with it, detached ones included).
    Stop,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    match cli.command {
        Some(Commands::Repl) => run_repl()?,
        Some(Commands::Compile {
            input,
            output,
            eval,
            debug,
        }) => compile_command(input, output, eval, debug)?,
        Some(Commands::Run {
            input,
            eval,
            quiet,
            profile,
            release,
        }) => run_command(input, eval, quiet, profile, release)?,
        Some(Commands::Inspect { input }) => inspect_command(input)?,
        Some(Commands::Format { input, eval, check }) => format_command(input, eval, check)?,
        Some(Commands::Test { input }) => test_cli::test_command(input)?,
        Some(Commands::Server {
            action,
            socket,
            code_collection_threshold,
        }) => match action {
            None => server_cli::server_command(socket, code_collection_threshold)?,
            Some(ServerAction::Status) => server_cli::status_command(socket)?,
            Some(ServerAction::Stop) => server_cli::stop_command(socket)?,
        },
        None => run_repl()?,
    }

    Ok(())
}

fn run_repl() -> Result<(), Box<dyn std::error::Error>> {
    let repl = ReplCli::new()?;
    repl.run()?;
    Ok(())
}

/// Print a parse error with visual formatting if in a TTY, otherwise plain text.
fn report_parse_error(err: &quiver_compiler::parser::Error, source: &str, source_id: &str) {
    // Check if stderr is a terminal and NO_COLOR is not set
    let use_color = std::io::stderr().is_terminal() && std::env::var("NO_COLOR").is_err();

    if use_color {
        // Use ariadne for visual error display
        diagnostics::eprint(err, source_id, source);
    } else {
        // Plain text fallback
        eprintln!("Error: {}", err);
    }
}

/// Report a parse error and exit.
fn handle_parse_error(err: quiver_compiler::parser::Error, source: &str, source_id: &str) -> ! {
    report_parse_error(&err, source, source_id);
    std::process::exit(1);
}

/// Build a resolver for an entry program: discover the project from the file's location, or —
/// for inline `--eval`/stdin with no path — the default (stdlib-only) package.
fn entry_resolver(input_path: Option<&str>) -> PackageResolver {
    match input_path {
        Some(path) => PackageResolver::for_entry_file(std::path::Path::new(path)),
        None => PackageResolver::inline(),
    }
}

fn compile_command(
    input: Option<String>,
    output: Option<String>,
    eval: Option<String>,
    debug: bool,
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
    let options = quiver_compiler::compiler::CompileOptions {
        debug,
        source_name: source_id.clone(),
        ..Default::default()
    };

    // Build registry from core modules and network builtins
    let builtins = build_builtin_registry();
    let resolver = entry_resolver(resolver_path.as_deref());

    // Compile and extract entry function
    // Note: compile_command allows programs that don't evaluate to a function
    let parsed = match parse(&source) {
        Ok(ast) => ast,
        Err(e) => handle_parse_error(e, &source, &source_id),
    };
    let (program, entry) = match quiver_cli::compile::compile_entry(
        parsed.clone(),
        &resolver,
        &builtins,
        options.clone(),
        None,
    ) {
        Ok((program, entry)) => (program, Some(entry)),
        Err(_) => {
            // If it doesn't evaluate to a function, compile without an entry point
            let ast = parsed;
            let mut program = Program::new();
            let mut module_cache = ModuleCache::new();
            let nil_type_id = program.register_type(Type::nil());
            Compiler::compile(
                ast,
                &quiver_compiler::compiler::Bindings::default(),
                Default::default(),
                &mut module_cache,
                &resolver,
                &mut program,
                nil_type_id, // parameter_type_id
                &HashMap::new(),
                &builtins,
                None, // no semantic recorder for the CLI
                options,
            )
            .map_err(|e| match e.span {
                Some(span) => format!(
                    "Compile error at {}:{}: {:?}",
                    span.line, span.column, e.error
                ),
                None => format!("Compile error: {:?}", e.error),
            })?;
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
/// Format `source`, reporting a parse error (and returning `None`) if it does not parse.
fn format_source(source: &str, source_id: &str) -> Option<String> {
    match parse(source) {
        Ok(ast) => Some(quiver_compiler::format_program(&ast, source)),
        Err(e) => {
            report_parse_error(&e, source, source_id);
            None
        }
    }
}

fn format_command(
    inputs: Vec<String>,
    eval: Option<String>,
    check: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // `--eval` and stdin have no file to write back to, so they print to stdout (or, with `--check`,
    // just verify and signal via the exit code).
    if let Some(code) = eval {
        return format_stream(&code, "eval", check);
    }
    if inputs.is_empty() {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        return format_stream(&buffer, "stdin", check);
    }

    // File arguments are formatted in place (or, with `--check`, checked).
    let mut changed = false;
    let mut failed = false;
    for path in &inputs {
        let source = match fs::read_to_string(path) {
            Ok(source) => source,
            Err(e) => {
                eprintln!("{}: {}", path, e);
                failed = true;
                continue;
            }
        };
        let Some(formatted) = format_source(&source, path) else {
            failed = true;
            continue;
        };
        if formatted == source {
            continue;
        }
        changed = true;
        if check {
            println!("would reformat: {}", path);
        } else if let Err(e) = fs::write(path, formatted) {
            eprintln!("{}: {}", path, e);
            failed = true;
        }
    }

    if failed || (check && changed) {
        std::process::exit(1);
    }
    Ok(())
}

/// Format a single source with no backing file: print it to stdout, or with `--check` exit non-zero
/// if it is not already formatted.
fn format_stream(
    source: &str,
    source_id: &str,
    check: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let Some(formatted) = format_source(source, source_id) else {
        std::process::exit(1);
    };
    if check {
        if formatted != source {
            std::process::exit(1);
        }
    } else {
        print!("{}", formatted);
    }
    Ok(())
}

fn run_command(
    input: Option<String>,
    eval: Option<String>,
    quiet: bool,
    profile: bool,
    release: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let debug = !release;

    // Profiling measures a dedicated runtime — per-worker instrumentation the shared
    // server cannot switch on per request — so it keeps the in-process path. Every
    // ordinary run goes through the server.
    if profile {
        if let Some(code) = eval {
            compile_execute(&code, None, quiet, profile, debug)?;
        } else if let Some(path) = input {
            let content = fs::read_to_string(&path)?;
            if path.ends_with(".qx") {
                execute_bytecode(&content, quiet, profile)?;
            } else {
                compile_execute(&content, Some(&path), quiet, profile, debug)?;
            }
        } else {
            let mut buffer = String::new();
            io::stdin().read_to_string(&mut buffer)?;
            compile_execute(&buffer, None, quiet, profile, debug)?;
        }
        return Ok(());
    }

    // Compile client-side (parse and compile errors are local, with the usual
    // diagnostics), or take bytecode as given; the server only ever sees bytecode.
    let bytecode = {
        let (source, source_id, path): (String, String, Option<String>) = if let Some(code) = eval {
            (code, "eval".to_string(), None)
        } else if let Some(path) = input {
            let content = fs::read_to_string(&path)?;
            if path.ends_with(".qx") {
                let bytecode: bytecode::Bytecode = serde_json::from_str(&content)?;
                if bytecode.entry.is_none() {
                    return Err("Bytecode has no entry point".into());
                }
                run_on_server(bytecode, quiet)?;
                return Ok(());
            } else if path.ends_with(".qv") {
                (content, path.clone(), Some(path))
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
            if buffer.trim_start().starts_with('{')
                && let Ok(bytecode) = serde_json::from_str::<bytecode::Bytecode>(&buffer)
            {
                if bytecode.entry.is_none() {
                    return Err("Bytecode has no entry point".into());
                }
                run_on_server(bytecode, quiet)?;
                return Ok(());
            }
            (buffer, "stdin".to_string(), None)
        };

        let ast = match parse(&source) {
            Ok(ast) => ast,
            Err(e) => handle_parse_error(e, &source, &source_id),
        };
        let resolver = entry_resolver(path.as_deref());
        let options = quiver_compiler::compiler::CompileOptions {
            debug,
            source_name: source_id,
            ..Default::default()
        };
        let store = std::rc::Rc::new(quiver_compiler::ArtifactStore::cache());
        let (program, entry) = quiver_cli::compile::compile_entry(
            ast,
            &resolver,
            &build_builtin_registry(),
            options,
            Some(store),
        )?;
        program.to_bytecode_optimized(entry)
    };

    run_on_server(bytecode, quiet)
}

/// Execute compiled bytecode on the shared server: create a root process, resume it
/// with the program, delete it afterwards. Ctrl-C cancels the run (the server stops
/// the root, the resume answers `Interrupted`) and still deletes — lifecycle is
/// explicit now that nothing ties it to a connection.
fn run_on_server(
    bytecode: bytecode::Bytecode,
    quiet: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    use quiver_cli::protocol::Outcome;
    use std::sync::atomic::{AtomicBool, Ordering};

    let client = quiver_cli::client::connect_or_spawn(
        &quiver_cli::protocol::default_socket_path(),
        &std::env::current_exe()?,
    )?;
    let pid = client.create_process().map_err(|e| e.to_string())?;

    // Forward Ctrl-C as a cancel; the second Ctrl-C hard-exits (conditional shutdown
    // sees the flag from the previous one).
    let interrupt = std::sync::Arc::new(AtomicBool::new(false));
    signal_hook::flag::register_conditional_shutdown(
        signal_hook::consts::SIGINT,
        130,
        std::sync::Arc::clone(&interrupt),
    )?;
    signal_hook::flag::register(
        signal_hook::consts::SIGINT,
        std::sync::Arc::clone(&interrupt),
    )?;
    {
        let client = client.clone();
        let interrupt = std::sync::Arc::clone(&interrupt);
        std::thread::spawn(move || {
            let mut sent = false;
            loop {
                std::thread::sleep(std::time::Duration::from_millis(30));
                if interrupt.load(Ordering::Relaxed) && !sent {
                    let _ = client.cancel(pid);
                    sent = true;
                }
            }
        });
    }

    let outcome = client.resume(pid, bytecode, None);
    let _ = client.delete_process(pid);

    match outcome.map_err(|e| e.to_string())? {
        Outcome::Value {
            rendered,
            origin,
            is_nil,
            ..
        } => {
            // Nil is the failing result: surface its provenance (debug builds) and
            // exit non-zero, exactly as the in-process runner did.
            if is_nil {
                if !quiet && let Some(origin) = origin {
                    eprintln!("[]  ({origin})");
                }
                std::process::exit(1);
            }
            if !quiet {
                println!("{rendered}");
            }
        }
        Outcome::Error { message } => {
            eprintln!("Runtime error: {message}");
            std::process::exit(1);
        }
        Outcome::Interrupted => std::process::exit(130),
    }
    Ok(())
}

fn compile_execute(
    source: &str,
    input_path: Option<&str>,
    quiet: bool,
    profile: bool,
    debug: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // Build registry from core modules and network builtins
    let builtins = build_builtin_registry();
    let resolver = entry_resolver(input_path);
    let options = quiver_compiler::compiler::CompileOptions {
        debug,
        source_name: input_path
            .map(|path| {
                std::path::Path::new(path)
                    .file_name()
                    .map(|name| name.to_string_lossy().into_owned())
                    .unwrap_or_else(|| path.to_string())
            })
            .unwrap_or_else(|| "main".to_string()),
        ..Default::default()
    };

    // Compile and extract entry function (this will error if not a function)
    let ast = match parse(source) {
        Ok(ast) => ast,
        Err(e) => handle_parse_error(e, source, &options.source_name),
    };
    let (program, entry) =
        quiver_cli::compile::compile_entry(ast, &resolver, &builtins, options, None)?;

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

    // One wake signal for every worker: the environment loop blocks on it instead of polling.
    let (waker, wake) = native_transport::wake_channel();

    let mut workers: Vec<Box<dyn WorkerHandle<quiver_io::NativeEffect>>> = Vec::new();
    for i in 0..num_workers {
        workers.push(Box::new(spawn_worker(
            native_transport::SystemClock,
            builtins.clone(),
            profile,
            i as u16,
            waker.clone(),
        )));
    }

    // Create environment with effect backend
    let effect_backend = create_effect_backend();
    let mut environment = Environment::<quiver_io::NativeEffect>::new(workers);
    environment.set_runtime_declarations(builtins.runtime_declarations().clone());

    if let Some(backend) = effect_backend {
        environment.set_effect_backend(backend);
    }

    // Extract data before consuming bytecode
    let builtin_names: Vec<String> = bytecode.builtins.iter().map(|b| b.name.clone()).collect();

    // Start process from bytecode
    let start_time = std::time::Instant::now();
    let process_id = environment
        .start_process(Some(bytecode))
        .map_err(|e| format!("Failed to start process: {:?}", e))?;

    // Request the result
    let request_id = environment
        .request_result(process_id, None)
        .map_err(|e| format!("Failed to request result: {:?}", e))?;

    // Event loop. Every message the environment routes crosses this loop, so how it idles is on
    // the latency of every spawn, await and select wake — hence the wake signal.
    loop {
        let did_work = environment.step().unwrap_or(false);

        match environment.poll_request(request_id) {
            Ok(Some(quiver_environment::RequestResult::Result(Ok(value), stats))) => {
                let wall_time = start_time.elapsed();

                // Print profiling report if enabled
                if let Some(stats) = stats {
                    print_bytecode_profile_report(&stats, &builtin_names, wall_time);
                }

                // Check if result is NIL tuple (exit with error). Debug builds stamp nil
                // results with their failure site — surface it before exiting.
                // Formatting uses the environment's merged program: the authoritative
                // id space for the value (the loaded bytecode's ids were remapped).
                if value.is_nil() {
                    if !quiet && let Some(origin) = environment.describe_origin(&value) {
                        eprintln!("[]  ({origin})");
                    }
                    std::process::exit(1);
                }

                // Print the result. Nil already returned above, so this prints every
                // value the program can answer with — `Ok` included, since a program
                // whose last step is a successful match has `Ok` as its result and
                // printing nothing reads as "no output" rather than "matched".
                if !quiet {
                    println!("{}", environment.format_value(&value));
                }

                return Ok(());
            }
            Ok(Some(quiver_environment::RequestResult::Result(Err(e), _))) => {
                return Err(format!("Runtime error: {}", e.crash_message()).into());
            }
            Ok(Some(_)) => {
                return Err("Unexpected result type".into());
            }
            Ok(None) => {
                if !did_work {
                    wake.wait(environment.io_in_flight());
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

    // Print interned field names (the ids GetNamed instructions carry)
    if !bytecode_data.field_names.is_empty() {
        println!("\nField names:");
        for (index, name) in bytecode_data.field_names.iter().enumerate() {
            println!("  {}: {}", index, name);
        }
    }

    // Print failure-provenance sites (debug builds): the targets of Stamp instructions.
    if let Some(table) = &bytecode_data.debug {
        println!("\nSites:");
        for (index, site) in table.sites.iter().enumerate() {
            let module = match bytecode_data.constants.get(site.module_constant) {
                Some(bytecode::Constant::Binary(bytes)) => {
                    String::from_utf8_lossy(bytes).into_owned()
                }
                _ => format!("constant#{}", site.module_constant),
            };
            println!(
                "  {}: {}:{}:{} ({:?})",
                index, module, site.line, site.column, site.kind
            );
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
