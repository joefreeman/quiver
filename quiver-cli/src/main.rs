use clap::{Parser, Subcommand};
use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::{Compiler, PackageResolver, parse};
use quiver_core::bytecode;
use quiver_core::format;
use quiver_core::program::Program;
use quiver_core::types::Type;
use std::collections::HashMap;
use std::fs;
use std::io::{self, IsTerminal, Read};

mod diagnostics;
use quiver_cli::build_builtin_registry;
mod repl_cli;
mod server_cli;
mod test_cli;
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

        /// Inline imported modules into the entry unit, shaken to what it reaches: a
        /// smaller, self-contained output that no longer names module identities a warm
        /// host could recognise. The default bundles each module's unit whole.
        #[arg(long)]
        inline: bool,
    },

    Run {
        input: Option<String>,

        #[arg(short, long)]
        eval: Option<String>,

        #[arg(short, long)]
        quiet: bool,

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

        /// Additionally listen for browser clients on a loopback TCP address. Takes
        /// `host:port` or a bare port (host defaults to 127.0.0.1); with no value at
        /// all, 127.0.0.1:2192 (U+2192: →, the arrow — what a quiver is full of) — the
        /// endpoint the web client suggests. Every request
        /// must carry the bearer token written beside the socket; non-loopback
        /// addresses are refused.
        #[arg(long, value_name = "ADDRESS", num_args = 0..=1, default_missing_value = "2192")]
        listen: Option<String>,

        /// Allow this browser origin on the TCP listener (repeatable).
        /// https://quiver.run and localhost origins are always allowed.
        #[arg(long = "allow-origin")]
        allow_origins: Vec<String>,

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
            inline,
        }) => compile_command(input, output, eval, debug, inline)?,
        Some(Commands::Run {
            input,
            eval,
            quiet,
            release,
        }) => run_command(input, eval, quiet, release)?,
        Some(Commands::Inspect { input }) => inspect_command(input)?,
        Some(Commands::Format { input, eval, check }) => format_command(input, eval, check)?,
        Some(Commands::Test { input }) => test_cli::test_command(input)?,
        Some(Commands::Server {
            action,
            socket,
            listen,
            allow_origins,
            code_collection_threshold,
        }) => match action {
            None => server_cli::server_command(
                socket,
                listen,
                allow_origins,
                code_collection_threshold,
            )?,
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
    inline: bool,
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
    let store = std::rc::Rc::new(quiver_compiler::ArtifactStore::cache());

    // Compile and extract entry function
    // Note: compile_command allows programs that don't evaluate to a function
    let parsed = match parse(&source) {
        Ok(ast) => ast,
        Err(e) => handle_parse_error(e, &source, &source_id),
    };
    let (program, module_cache, entry) = match quiver_cli::compile::compile_entry(
        parsed.clone(),
        &resolver,
        &builtins,
        options.clone(),
        Some(std::rc::Rc::clone(&store)),
    ) {
        Ok((program, module_cache, entry)) => (program, module_cache, Some(entry)),
        Err(_) => {
            // Not executable: compile the top level alone, so the result is still
            // inspectable. The unit then has no entry rather than no content.
            let ast = parsed;
            let mut program = Program::new();
            let mut module_cache = ModuleCache::new();
            module_cache.artifact_store = Some(std::rc::Rc::clone(&store));
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
            (program, module_cache, None)
        }
    };

    // A `.qx` is a self-contained program: this compile's own code, plus the units of
    // every module it imports — or, inlined, one unit with the module code shaken into
    // it. `own_floor` is 0 — the program is freshly compiled, so everything no module
    // owns is its own.
    let imports = if inline {
        quiver_compiler::Imports::Inline
    } else {
        quiver_compiler::Imports::Bundle
    };
    let compiled = quiver_compiler::extract_program(&program, &module_cache, entry, 0, imports);
    // Compact, not pretty: this is machine output — `quiv inspect` is the readable view,
    // and indentation was over half the file.
    let json = serde_json::to_string(&compiled)?;

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
    release: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let debug = !release;

    // Compile client-side (parse and compile errors are local, with the usual
    // diagnostics), or take a compiled program as given. Either way the server is handed
    // a unit and the modules it imports, and links each once.
    let compiled = {
        let (source, source_id, path): (String, String, Option<String>) = if let Some(code) = eval {
            (code, "eval".to_string(), None)
        } else if let Some(path) = input {
            let content = fs::read_to_string(&path)?;
            if path.ends_with(".qx") {
                let compiled: quiver_compiler::CompiledProgram = serde_json::from_str(&content)?;
                if compiled.unit.entry.is_none() {
                    return Err("Compiled program has no entry point".into());
                }
                run_on_server(compiled, quiet)?;
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
            // Try to parse as a compiled program first
            if buffer.trim_start().starts_with('{')
                && let Ok(compiled) =
                    serde_json::from_str::<quiver_compiler::CompiledProgram>(&buffer)
            {
                if compiled.unit.entry.is_none() {
                    return Err("Compiled program has no entry point".into());
                }
                run_on_server(compiled, quiet)?;
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
        let (program, module_cache, entry) = quiver_cli::compile::compile_entry(
            ast,
            &resolver,
            &build_builtin_registry(),
            options,
            Some(store),
        )?;
        quiver_compiler::extract_program(
            &program,
            &module_cache,
            Some(entry),
            0,
            quiver_compiler::Imports::Bundle,
        )
    };

    run_on_server(compiled, quiet)
}

/// Execute a compiled program on the shared server: create a root process, resume it
/// with the program's unit and the modules it imports, delete it afterwards. Ctrl-C cancels the run (the server stops
/// the root, the resume answers `Interrupted`) and still deletes — lifecycle is
/// explicit now that nothing ties it to a connection.
fn run_on_server(
    compiled: quiver_compiler::CompiledProgram,
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

    // A one-shot run has no record of what this server holds, so it attaches the whole
    // closure and lets the server skip the keys it already has.
    let outcome = client.resume(
        pid,
        quiver_cli::protocol::ResumePayload {
            unit: compiled.unit,
            modules: compiled.modules,
        },
        None,
    );
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
fn inspect_command(input: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let content = if let Some(path) = input {
        fs::read_to_string(&path)?
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    };

    // A `.qx` is relocatable code, so it is linked into a fresh program before being
    // rendered: the ids shown are the ones a host would assign it.
    let compiled: quiver_compiler::CompiledProgram = serde_json::from_str(&content)?;
    let mut program = Program::new();
    let entry_id =
        quiver_compiler::link_program(&compiled, &mut program, &build_builtin_registry())
            .map_err(|e| format!("Cannot link the program: {e:?}"))?;
    let bytecode_data = program.to_bytecode(entry_id);

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
        format!("<{}>", hex)
    } else {
        let hex: String = bytes[..8].iter().map(|b| format!("{:02x}", b)).collect();
        format!("<{}…> ({} bytes)", hex, bytes.len())
    }
}
