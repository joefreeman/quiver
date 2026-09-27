use clap::{Parser, Subcommand};
use quiver_cli::compile::EntryError;
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
mod format_cli;
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

    /// Compile a program to bytecode. A `.qv` file is written beside it as `.qx`; code from
    /// `--eval` or stdin is written to stdout.
    Compile {
        #[command(flatten)]
        input: ProgramInput,

        /// Write the bytecode here instead, or `-` for stdout.
        #[arg(short, long)]
        output: Option<String>,

        /// Debug build: stamp nil results with failure provenance (`origin` annotations).
        #[arg(long)]
        debug: bool,

        /// Inline imported modules into the entry unit, shaken to what it reaches: a
        /// smaller, self-contained output that no longer names module identities a warm
        /// host could recognise. The default bundles each module's unit whole.
        #[arg(long)]
        inline: bool,
    },

    /// Run a program, from source or bytecode.
    Run {
        #[command(flatten)]
        input: ProgramInput,

        #[arg(short, long)]
        quiet: bool,

        /// Release build: skip failure-provenance stamps (`run` compiles debug by default).
        #[arg(long)]
        release: bool,
    },

    /// Print a program's bytecode, compiling it first if given source.
    Inspect {
        #[command(flatten)]
        input: ProgramInput,

        /// Debug build: stamp nil results with failure provenance (`origin` annotations).
        #[arg(long)]
        debug: bool,
    },

    /// Format Quiver source in place. Each argument is a file, or a directory to walk for
    /// `.qv` files; with no arguments, the current directory. `-` reads stdin and writes the
    /// result to stdout, as does `--eval`.
    Format {
        /// Files and directories to format in place, or `-` for stdin.
        input: Vec<String>,

        #[arg(short, long)]
        eval: Option<String>,

        /// Don't write anything; list what would be reformatted and exit 1 (for CI).
        #[arg(long)]
        check: bool,
    },

    /// Run the Quiver code embedded in a Markdown document, checking its `//=` assertions.
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

/// Where a command's program comes from: exactly one of a file, `-` for stdin, or code
/// given with `--eval`.
#[derive(clap::Args)]
#[group(required = true, multiple = false)]
struct ProgramInput {
    /// A `.qv` source or `.qx` bytecode file, or `-` to read either from stdin.
    input: Option<String>,

    /// Source code given on the command line.
    #[arg(short, long)]
    eval: Option<String>,
}

/// Quiver source to compile, named for diagnostics. `path` is set when it was read from a
/// file, which locates the project it belongs to.
struct Source {
    text: String,
    id: String,
    path: Option<String>,
}

enum Loaded {
    Source(Source),
    Compiled(quiver_compiler::CompiledProgram),
}

impl ProgramInput {
    /// Read the program. A file's extension says whether it is source or bytecode; stdin
    /// is bytecode when it parses as a compiled program, and source otherwise.
    fn load(self) -> Result<Loaded, Box<dyn std::error::Error>> {
        if let Some(code) = self.eval {
            return Ok(Loaded::Source(Source {
                text: code,
                id: "eval".to_string(),
                path: None,
            }));
        }
        let path = self.input.expect("clap requires an input or --eval");
        if path == "-" {
            let mut buffer = String::new();
            io::stdin().read_to_string(&mut buffer)?;
            if buffer.trim_start().starts_with('{')
                && let Ok(compiled) = serde_json::from_str(&buffer)
            {
                return Ok(Loaded::Compiled(compiled));
            }
            return Ok(Loaded::Source(Source {
                text: buffer,
                id: "stdin".to_string(),
                path: None,
            }));
        }
        let content =
            fs::read_to_string(&path).map_err(|e| format!("Cannot read '{path}': {e}"))?;
        match std::path::Path::new(&path)
            .extension()
            .and_then(|e| e.to_str())
        {
            Some("qv") => Ok(Loaded::Source(Source {
                text: content,
                id: path.clone(),
                path: Some(path),
            })),
            Some("qx") => Ok(Loaded::Compiled(serde_json::from_str(&content)?)),
            _ => Err(format!(
                "Unsupported file extension for '{path}': expected .qv for source or .qx for bytecode"
            )
            .into()),
        }
    }
}

fn main() {
    // Print the error's own message rather than returning it: the default `Termination`
    // renders a `Box<dyn Error>` with `Debug`, which for the `String` errors these commands
    // produce wraps the whole message in escaped quotes.
    if let Err(error) = run() {
        eprintln!("{error}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    match cli.command {
        Some(Commands::Repl) => run_repl()?,
        Some(Commands::Compile {
            input,
            output,
            debug,
            inline,
        }) => {
            exit_on_broken_pipe();
            compile_command(input, output, debug, inline)?
        }
        Some(Commands::Run {
            input,
            quiet,
            release,
        }) => run_command(input, quiet, release)?,
        Some(Commands::Inspect { input, debug }) => {
            exit_on_broken_pipe();
            inspect_command(input, debug)?
        }
        Some(Commands::Format { input, eval, check }) => {
            format_cli::format_command(input, eval, check)
        }
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

/// Let a closed stdout end the process quietly, as it does any Unix filter, so that output
/// piped into `head` doesn't panic `println!`. Rust ignores `SIGPIPE` by default, and this is
/// only for commands that write nothing but stdout: with the signal restored, a peer closing
/// the server socket would kill the process rather than surface an error.
fn exit_on_broken_pipe() {
    // SAFETY: called before any threads are spawned, and resets a signal to its default
    // disposition without installing a handler.
    unsafe { libc::signal(libc::SIGPIPE, libc::SIG_DFL) };
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
        // Plain text fallback. It names the source, which the ariadne report does for itself
        // and which a run over many files needs to place the error at all.
        eprintln!("{}: {}", source_id, err);
    }
}

/// Report a compile error and exit.
fn handle_compile_error(
    err: &quiver_compiler::compiler::LocatedError,
    source: &str,
    source_id: &str,
) -> ! {
    diagnostics::eprint_compile_error(err, source_id, source);
    std::process::exit(1);
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

/// Whether a compile must produce an executable program.
enum Entry {
    /// The program must evaluate to a function; anything else is an error.
    Required,
    /// A program that doesn't evaluate to a function compiles its top level alone, so it
    /// is still inspectable. The unit then has no entry rather than no content.
    Optional,
}

/// Compile source into a self-contained program: its own code, plus the units of every
/// module it imports — or, inlined, one unit with the module code shaken into it.
fn compile_source(
    source: Source,
    debug: bool,
    imports: quiver_compiler::Imports,
    entry: Entry,
) -> Result<quiver_compiler::CompiledProgram, Box<dyn std::error::Error>> {
    let parsed = match parse(&source.text) {
        Ok(ast) => ast,
        Err(e) => handle_parse_error(e, &source.text, &source.id),
    };
    let options = quiver_compiler::compiler::CompileOptions {
        debug,
        source_name: source.id.clone(),
        ..Default::default()
    };
    let builtins = build_builtin_registry();
    let resolver = entry_resolver(source.path.as_deref());
    let store = std::rc::Rc::new(quiver_compiler::ArtifactStore::cache());

    let (program, module_cache, entry) = match quiver_cli::compile::compile_entry(
        parsed.clone(),
        &resolver,
        &builtins,
        options.clone(),
        Some(std::rc::Rc::clone(&store)),
    ) {
        Ok(compiled) => {
            diagnostics::eprint_warnings(&compiled.warnings, &source.id, &source.text);
            (
                compiled.program,
                compiled.module_cache,
                Some(compiled.entry),
            )
        }
        Err(EntryError::Compile(e)) if matches!(entry, Entry::Required) => {
            handle_compile_error(&e, &source.text, &source.id)
        }
        Err(e @ EntryError::NotExecutable) if matches!(entry, Entry::Required) => {
            return Err(e.into());
        }
        Err(_) => {
            let mut program = Program::new();
            let mut module_cache = ModuleCache::new();
            module_cache.artifact_store = Some(store);
            let nil_type_id = program.register_type(Type::nil());
            match Compiler::compile(
                parsed,
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
            ) {
                Ok(compiled) => {
                    diagnostics::eprint_warnings(&compiled.warnings, &source.id, &source.text)
                }
                Err(e) => handle_compile_error(&e, &source.text, &source.id),
            }
            (program, module_cache, None)
        }
    };

    // `own_floor` is 0 — the program is freshly compiled, so everything no module owns is
    // its own.
    Ok(quiver_compiler::extract_program(
        &program,
        &module_cache,
        entry,
        0,
        imports,
    ))
}

fn compile_command(
    input: ProgramInput,
    output: Option<String>,
    debug: bool,
    inline: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let Loaded::Source(source) = input.load()? else {
        return Err("The input is already compiled".into());
    };
    // A source file compiles to the `.qx` beside it; code with no file of its own goes to
    // stdout.
    let output = output.or_else(|| {
        source.path.as_ref().map(|path| {
            std::path::Path::new(path)
                .with_extension("qx")
                .to_string_lossy()
                .into_owned()
        })
    });
    let imports = if inline {
        quiver_compiler::Imports::Inline
    } else {
        quiver_compiler::Imports::Bundle
    };
    let compiled = compile_source(source, debug, imports, Entry::Optional)?;
    // Compact, not pretty: this is machine output — `quiv inspect` is the readable view,
    // and indentation was over half the file.
    let json = serde_json::to_string(&compiled)?;

    match output.as_deref() {
        None | Some("-") => println!("{json}"),
        Some(path) => fs::write(path, json)?,
    }

    Ok(())
}

fn run_command(
    input: ProgramInput,
    quiet: bool,
    release: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // Compile client-side (parse and compile errors are local, with the usual
    // diagnostics), or take a compiled program as given. Either way the server is handed
    // a unit and the modules it imports, and links each once.
    let compiled = match input.load()? {
        Loaded::Source(source) => compile_source(
            source,
            !release,
            quiver_compiler::Imports::Bundle,
            Entry::Required,
        )?,
        Loaded::Compiled(compiled) => {
            if compiled.unit.entry.is_none() {
                return Err("Compiled program has no entry point".into());
            }
            compiled
        }
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
    // Leased: a run that is killed outright — a SIGKILL, a closed terminal — never gets
    // to delete its root, and the beat below is what stops that root outliving it.
    let pid = client
        .create_process(Some(quiver_cli::protocol::CLIENT_LEASE))
        .map_err(|e| e.to_string())?;

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
            let mut beat = std::time::Instant::now();
            loop {
                std::thread::sleep(std::time::Duration::from_millis(30));
                if interrupt.load(Ordering::Relaxed) && !sent {
                    let _ = client.cancel(pid);
                    sent = true;
                }
                // The main thread spends the whole run blocked on the resume's
                // response, so the lease is kept from here.
                if beat.elapsed() >= quiver_cli::protocol::CLIENT_HEARTBEAT {
                    let _ = client.heartbeat(pid);
                    beat = std::time::Instant::now();
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
            // Nil is the failing result: report it and exit non-zero, exactly as the
            // in-process runner did. The nil itself is always printed — a release build
            // carries no provenance, and exiting 1 with nothing said reads as a silent
            // failure — with its `:error` and the site a debug build stamped appended.
            if is_nil {
                if !quiet {
                    match origin {
                        Some(origin) => eprintln!("[]  ({origin})"),
                        None => eprintln!("[]"),
                    }
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
fn inspect_command(input: ProgramInput, debug: bool) -> Result<(), Box<dyn std::error::Error>> {
    // Source is compiled exactly as `quiv compile` would, so what is shown is what its
    // `.qx` holds.
    let compiled = match input.load()? {
        Loaded::Source(source) => compile_source(
            source,
            debug,
            quiver_compiler::Imports::Bundle,
            Entry::Optional,
        )?,
        Loaded::Compiled(compiled) => compiled,
    };

    // A `.qx` is relocatable code, so it is linked into a fresh program before being
    // rendered: the ids shown are the ones a host would assign it.
    let mut program = Program::new();
    let entry_id =
        quiver_compiler::link_program(&compiled, &mut program, &build_builtin_registry())
            .map_err(|e| format!("Cannot link the program: {e:?}"))?;
    let bytecode_data = program.to_bytecode(entry_id);

    // Table sizes up front. Types are the largest table in a linked program by some margin
    // and the only one with no section below, so without this line the biggest thing in a
    // program is invisible here.
    println!(
        "Tables: {} types, {} tuples, {} constants, {} functions, {} builtins",
        bytecode_data.types.len(),
        bytecode_data.tuples.len(),
        bytecode_data.constants.len(),
        bytecode_data.functions.len(),
        bytecode_data.builtins.len(),
    );
    println!();

    println!("Constants:");
    for (i, constant) in bytecode_data.constants.iter().enumerate() {
        // Composites name their children by index, so they print as `#n` references rather
        // than being expanded — the table is a graph, and showing it as one is the point.
        let refs = |indices: &[usize]| {
            indices
                .iter()
                .map(|index| format!("#{index}"))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let formatted = match constant {
            bytecode::Constant::Integer(n) => n.to_string(),
            bytecode::Constant::Binary(bytes) => format_binary(bytes),
            bytecode::Constant::Tuple { id, fields } => {
                let name = bytecode_data
                    .tuples
                    .get(*id)
                    .and_then(|info| info.name.clone())
                    .unwrap_or_default();
                format!("{name}[{}]", refs(fields))
            }
            bytecode::Constant::Function { id, captures } => {
                format!("Function{id}({})", refs(captures))
            }
            bytecode::Constant::Builtin { id } => format!("Builtin{id}"),
            bytecode::Constant::Annotated { value, entries } => {
                let entries = entries
                    .iter()
                    .map(|(key, index)| {
                        let key = bytecode_data
                            .annotation_keys
                            .get(*key)
                            .cloned()
                            .unwrap_or_else(|| key.to_string());
                        format!(":{key} #{index}")
                    })
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("#{value} {entries}")
            }
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
