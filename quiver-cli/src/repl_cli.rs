//! The interactive REPL — a client of `quiv server` that owns the whole compiler
//! side of the session ([`LineCompiler`]): bindings, types, modules, display policy.
//! The server hosts one persistent process for it; each line compiles locally and
//! crosses the API as tree-shaken bytecode (`resume`), so parse and compile errors
//! are local errors with full diagnostics.
//!
//! Ctrl-C during a line first aborts a client-side compile (the interrupt flag is the
//! compiler's cancellation token), and otherwise cancels the in-flight resume — the
//! server stops the session process and the client resets the session, exactly the
//! REPL's historical semantics. A second Ctrl-C with the first still pending
//! hard-exits the client only. At the prompt, rustyline's raw mode swallows `^C`
//! before it becomes a signal.

use colored::Colorize;
use quiver_cli::client::Client;
use quiver_cli::protocol::Outcome;
use quiver_compiler::{PackageResolver, find_project_root};
use quiver_environment::{LineCompiler, ReplError};
use quiver_io::NativeEffect;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::io::IsTerminal;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

const HISTORY_FILE: &str = ".quiv_history";

/// The project root for the current directory, if the REPL was launched inside a
/// project (shown in the banner and used to root the module resolver).
fn current_project_root() -> Option<std::path::PathBuf> {
    find_project_root(&std::env::current_dir().ok()?)
}

fn session_resolver() -> Box<dyn quiver_compiler::ModuleResolver> {
    match std::env::current_dir() {
        Ok(dir) => Box::new(PackageResolver::for_dir(&dir)),
        Err(_) => Box::new(PackageResolver::inline()),
    }
}

pub struct ReplCli {
    editor: Editor<(), rustyline::history::DefaultHistory>,
    client: Client,
    compiler: LineCompiler<NativeEffect>,
    artifact_store: Rc<quiver_compiler::ArtifactStore>,
    /// The server-side session process. Atomic and shared with the cancel thread,
    /// which must target whichever process a reset most recently created.
    process_id: Arc<AtomicU64>,
    /// Set by Ctrl-C during a line. Doubles as the compiler's cancellation token
    /// (client-side compiles abort on it); the cancel thread forwards it to the
    /// server while a resume is in flight; and a second Ctrl-C with it still set
    /// hard-exits. Registration order matters: the conditional shutdown sees the
    /// flag from the *previous* Ctrl-C, not its own.
    interrupt: Arc<AtomicBool>,
    /// Whether a resume is awaiting its response — the window a server-side cancel
    /// targets.
    in_flight: Arc<AtomicBool>,
    /// Latches so one interrupt sends at most one cancel per resume.
    cancel_sent: Arc<AtomicBool>,
}

impl ReplCli {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut editor = Editor::<(), rustyline::history::DefaultHistory>::new()?;
        let _ = editor.load_history(HISTORY_FILE);

        let interrupt = Arc::new(AtomicBool::new(false));
        signal_hook::flag::register_conditional_shutdown(
            signal_hook::consts::SIGINT,
            130,
            Arc::clone(&interrupt),
        )?;
        signal_hook::flag::register(signal_hook::consts::SIGINT, Arc::clone(&interrupt))?;

        let client = quiver_cli::client::connect_or_spawn(
            &quiver_cli::protocol::default_socket_path(),
            &std::env::current_exe()?,
        )?;
        let process_id = Arc::new(AtomicU64::new(
            client.create_process().map_err(|e| e.to_string())?,
        ));

        let in_flight = Arc::new(AtomicBool::new(false));
        let cancel_sent = Arc::new(AtomicBool::new(false));
        {
            // The cancel thread: while a resume is in flight, an interrupt becomes a
            // `POST …/cancel` — its own request, so it needs nothing from the main
            // thread, which is blocked reading the resume's response.
            let client = client.clone();
            let process_id = Arc::clone(&process_id);
            let interrupt = Arc::clone(&interrupt);
            let in_flight = Arc::clone(&in_flight);
            let cancel_sent = Arc::clone(&cancel_sent);
            std::thread::spawn(move || {
                loop {
                    std::thread::sleep(std::time::Duration::from_millis(30));
                    if in_flight.load(Ordering::Relaxed)
                        && interrupt.load(Ordering::Relaxed)
                        && !cancel_sent.swap(true, Ordering::Relaxed)
                    {
                        let _ = client.cancel(process_id.load(Ordering::Relaxed));
                    }
                }
            });
        }

        let artifact_store = Rc::new(quiver_compiler::ArtifactStore::cache());
        let mut repl = ReplCli {
            editor,
            client,
            compiler: LineCompiler::new(session_resolver(), quiver_cli::build_builtin_registry()),
            artifact_store,
            process_id,
            interrupt,
            in_flight,
            cancel_sent,
        };
        repl.configure_compiler();
        repl.banner();
        Ok(repl)
    }

    /// (Re)apply the session compile configuration: debug mode, the interrupt flag as
    /// the compile-cancellation token, the shared artifact store.
    fn configure_compiler(&mut self) {
        self.compiler
            .set_compile_options(quiver_compiler::compiler::CompileOptions {
                debug: true,
                source_name: "repl".to_string(),
                cancel: Some(Arc::clone(&self.interrupt)),
                ..Default::default()
            });
        self.compiler
            .set_artifact_store(Rc::clone(&self.artifact_store));
    }

    fn banner(&self) {
        if std::io::stdin().is_terminal() {
            println!("Quiver v{} - REPL", env!("CARGO_PKG_VERSION"));
            match current_project_root() {
                Some(root) => println!("{}", format!("Project: {}", root.display()).bright_black()),
                None => println!("{}", "No project (standard library only)".bright_black()),
            }
            println!("Type \\? for help or \\q to exit");
            println!();
        }
    }

    pub fn run(mut self) -> Result<(), ReadlineError> {
        loop {
            let readline = self.editor.readline(&self.get_prompt());
            match readline {
                Ok(line) => {
                    let line = line.trim();
                    if line.is_empty() {
                        continue;
                    }

                    self.editor.add_history_entry(line)?;

                    if let Some(command) = line.strip_prefix('\\') {
                        if !self.handle_command(command) {
                            break;
                        }
                    } else {
                        self.evaluate(line);
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    println!("{}", "(Use \\q to quit)".bright_black());
                }
                Err(ReadlineError::Eof) => {
                    break;
                }
                Err(error) => {
                    eprintln!("Error: {}", error);
                    break;
                }
            }
        }

        // Best-effort teardown: the session process (and everything it owns short of
        // detached services) should not outlive an orderly exit.
        let _ = self
            .client
            .delete_process(self.process_id.load(Ordering::Relaxed));

        self.editor.save_history(HISTORY_FILE)?;

        if std::io::stdin().is_terminal() {
            println!("Bye!")
        }

        Ok(())
    }

    fn get_prompt(&self) -> String {
        format!("{} ", ">>-".white().bold())
    }

    /// Stop the session process and start over — the REPL's reset semantics (bindings
    /// lost), used by `\!` and after interrupts and runtime errors.
    fn reset(&mut self) {
        let old = self.process_id.load(Ordering::Relaxed);
        let _ = self.client.delete_process(old);
        match self.client.create_process() {
            Ok(id) => self.process_id.store(id, Ordering::Relaxed),
            Err(e) => {
                eprintln!("{}", format!("Server connection lost: {e}").red());
                std::process::exit(1);
            }
        }
        self.compiler = LineCompiler::new(session_resolver(), quiver_cli::build_builtin_registry());
        self.configure_compiler();
    }

    fn evaluate(&mut self, line: &str) {
        self.interrupt.store(false, Ordering::Relaxed);
        self.cancel_sent.store(false, Ordering::Relaxed);
        let pid = self.process_id.load(Ordering::Relaxed);

        // Prepare and compile locally: errors here are local, with full diagnostics.
        let prepared = match self.compiler.prepare(line) {
            Ok(prepared) => prepared,
            Err(ReplError::Parser(e)) => {
                let use_color =
                    std::io::stderr().is_terminal() && std::env::var("NO_COLOR").is_err();
                if use_color {
                    crate::diagnostics::eprint(&e, "repl", line);
                } else {
                    eprintln!("{}", format!("Parse error: {e}").yellow());
                }
                return;
            }
            Err(e) => {
                eprintln!("{}", format!("{e}").red());
                return;
            }
        };
        if let Err(e) = self.client.compact(pid, prepared.compact_keep().to_vec()) {
            eprintln!("{}", format!("Server connection lost: {e}").red());
            std::process::exit(1);
        }
        let compiled = match self.compiler.compile(prepared) {
            Ok(compiled) => compiled,
            Err(ReplError::Compiler(e)) => {
                // An interrupted compile aborted cleanly — nothing was committed, so
                // the session needs no reset.
                if self.interrupt.swap(false, Ordering::Relaxed) {
                    println!("{}", "Interrupted".red());
                    println!();
                } else {
                    eprintln!("{}", format!("Compile error: {e}").yellow());
                }
                return;
            }
            Err(e) => {
                eprintln!("{}", format!("{e}").red());
                return;
            }
        };
        let Some(committed) = self.compiler.commit_line(compiled) else {
            return; // Type definitions only.
        };

        // Ship the line; the cancel thread covers the wait.
        self.in_flight.store(true, Ordering::Relaxed);
        let outcome = self
            .client
            .resume(pid, committed.bytecode, Some(committed.keep_indices));
        self.in_flight.store(false, Ordering::Relaxed);
        self.interrupt.store(false, Ordering::Relaxed);

        match outcome {
            Ok(Outcome::Value {
                rendered,
                type_rendered,
                origin,
                ..
            }) => {
                // Show type for functions, builtins, and processes; failure
                // provenance for stamped nil results (debug builds).
                match type_rendered.or(origin) {
                    Some(note) => println!("{} {}", rendered, format!("({note})").bright_black()),
                    None => println!("{rendered}"),
                }
            }
            Ok(Outcome::Interrupted) => {
                println!("{}", "Interrupted".red());
                println!();
                self.reset();
            }
            Ok(Outcome::Error { message }) => {
                // The session process crashed; reset, as the REPL has always done.
                eprintln!("{}", format!("Runtime error: {message}").red());
                println!();
                self.reset();
            }
            Err(e) => {
                eprintln!("{}", format!("Server connection lost: {e}").red());
                std::process::exit(1);
            }
        }
    }

    fn inspect(&mut self, path: &str) {
        match self.client.inspect(path) {
            Ok(text) => {
                for line in text.lines() {
                    println!("{}", line.bright_black());
                }
            }
            Err(e) => eprintln!("{}", format!("{e}").red()),
        }
    }

    fn handle_command(&mut self, command: &str) -> bool {
        let parts: Vec<&str> = command.split_whitespace().collect();

        match parts.as_slice() {
            ["?"] => {
                println!("{}", "Available commands:".bright_black());
                println!("{}", "  \\? - Show this help message".bright_black());
                println!("{}", "  \\q - Exit the REPL".bright_black());
                println!("{}", "  \\! - Reset the REPL".bright_black());
                println!(
                    "{}",
                    "  \\r - Reload project modules (keeps variables)".bright_black()
                );
                println!("{}", "  \\v - List variables".bright_black());
                println!("{}", "  \\p - List processes".bright_black());
                println!("{}", "  \\p X - Inspect process with ID X".bright_black());
                println!("{}", "  \\w - List workers".bright_black());
                println!("{}", "  \\w X - Inspect worker with ID X".bright_black());
                println!(
                    "{}",
                    "  \\x - Show (compile-time) type of last expression".bright_black()
                );
            }

            ["q"] => {
                return false;
            }

            ["!"] => {
                self.reset();
                self.banner();
            }

            ["r"] => {
                self.compiler.reload_modules(session_resolver());
                println!("{}", "Project modules reloaded".bright_black());
            }

            ["v"] => {
                let variables = self.compiler.get_variables();
                if variables.is_empty() {
                    println!("{}", "No variables defined".bright_black());
                } else {
                    println!("{}", "Variables:".bright_black());
                    for (name, formatted_type) in variables {
                        println!("{}", format!("  {name}: {formatted_type}").bright_black());
                    }
                }
            }
            ["x"] => {
                let last = self.compiler.get_last_result_type().clone();
                println!("{}", self.compiler.format_type(&last).bright_black());
            }
            ["p"] => self.inspect("/processes"),
            ["p", id_str] => match id_str.parse::<u64>() {
                Ok(id) => self.inspect(&format!("/processes/{id}")),
                Err(_) => eprintln!("{}", format!("Invalid process ID: {id_str}").red()),
            },
            ["w"] => self.inspect("/workers"),
            ["w", id_str] => match id_str.parse::<u64>() {
                Ok(id) => self.inspect(&format!("/workers/{id}")),
                Err(_) => eprintln!("{}", format!("Invalid worker ID: {id_str}").red()),
            },

            _ => {
                eprintln!("{}", format!("Unknown command: \\{}", command).red());
            }
        }

        true
    }
}
