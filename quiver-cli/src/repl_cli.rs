use colored::Colorize;
use quiver_cli::spawn_worker;
use quiver_compiler::{ModuleResolver, PackageResolver, find_project_root};
use quiver_core::wire::WireValue;
use quiver_environment::{Environment, Repl, ReplError, RequestResult, WorkerHandle};
use quiver_io::NativeEffect;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::io::IsTerminal;
use std::rc::Rc;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};
use std::thread::{self, JoinHandle};

const HISTORY_FILE: &str = ".quiv_history";

struct EvaluationResult {
    value: WireValue,
}

pub struct ReplCli {
    editor: Editor<(), rustyline::history::DefaultHistory>,
    environment: Arc<Mutex<Environment<NativeEffect>>>,
    repl: Option<Repl<NativeEffect>>,
    stepping_thread: Option<JoinHandle<()>>,
    shutdown_signal: Arc<AtomicBool>,
    /// Set by Ctrl-C during an evaluation (at the prompt, rustyline's raw mode
    /// swallows `^C` before it becomes a signal). The evaluation wait polls it and
    /// answers by stopping the session process; the line loop then resets the session.
    interrupt: Arc<AtomicBool>,
    /// Kept so shutdown can wake the stepping thread. An idle thread parks in
    /// `WakeSignal::wait`, which blocks on a channel receive, so setting the flag alone never
    /// returns it — and the join below would hang after `\q`.
    waker: quiver_cli::native_transport::Waker,
    /// The content-addressed artifact store every REPL session links std (and, in
    /// time, project) modules from — and extracts freshly-compiled modules into.
    /// Attaching it is monotonic (it only affects future imports), so sessions use it
    /// from the start with no readiness ceremony.
    artifact_store: Rc<quiver_compiler::ArtifactStore>,
}

/// Build the resolver for a REPL session: project-aware when launched inside a project (the
/// nearest `quiver.toml` from the current directory), otherwise the standard library only.
fn repl_resolver() -> Box<dyn ModuleResolver> {
    match std::env::current_dir() {
        Ok(dir) => Box::new(PackageResolver::for_dir(&dir)),
        Err(_) => Box::new(PackageResolver::inline()),
    }
}

/// The project root for the current directory, if the REPL was launched inside a project.
fn current_project_root() -> Option<std::path::PathBuf> {
    find_project_root(&std::env::current_dir().ok()?)
}

impl ReplCli {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut editor = Editor::<(), rustyline::history::DefaultHistory>::new()?;
        let _ = editor.load_history(HISTORY_FILE);

        // Create workers with system time
        let num_workers = 4;

        // Build registry from core modules and network builtins
        let builtins = crate::build_builtin_registry();

        let (waker, wake) = quiver_cli::native_transport::wake_channel();

        let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
        for i in 0..num_workers {
            workers.push(Box::new(spawn_worker(
                quiver_cli::native_transport::SystemClock,
                builtins.clone(),
                false, // Don't enable profiling in REPL
                i as u16,
                waker.clone(),
            )));
        }
        let effect_backend = crate::create_effect_backend();
        let mut environment = Environment::<NativeEffect>::new(workers);
        environment.set_runtime_declarations(builtins.runtime_declarations().clone());

        // Set the effect backend
        if let Some(backend) = effect_backend {
            environment.set_effect_backend(backend);
        }

        // Wrap environment in Arc<Mutex> for shared access
        let environment = Arc::new(Mutex::new(environment));
        let shutdown_signal = Arc::new(AtomicBool::new(false));

        // Ctrl-C during an evaluation: the first sets the flag (the evaluation wait
        // answers it by stopping the session process); a second, with the flag still
        // set, hard-exits — the escape hatch if the runtime itself is wedged.
        // Registration order matters: actions run in it, so the conditional shutdown
        // sees the flag from the *previous* Ctrl-C, not its own.
        let interrupt = Arc::new(AtomicBool::new(false));
        signal_hook::flag::register_conditional_shutdown(
            signal_hook::consts::SIGINT,
            130,
            Arc::clone(&interrupt),
        )?;
        signal_hook::flag::register(signal_hook::consts::SIGINT, Arc::clone(&interrupt))?;

        // Spawn background thread to step the environment continuously
        let env_clone = Arc::clone(&environment);
        let shutdown_clone = Arc::clone(&shutdown_signal);
        let stepping_thread = Some(thread::spawn(move || {
            while !shutdown_clone.load(Ordering::Relaxed) {
                let did_work = if let Ok(mut env) = env_clone.lock() {
                    env.step().unwrap_or(false)
                } else {
                    false
                };

                // Block until a worker signals (or a completion may be due) rather than
                // sleeping a fixed interval, which used to sit on the latency of every routed
                // message. The shutdown flag is checked on each wake, so a signal is all it
                // takes to leave; `io_in_flight` bounds the wait so a completion is still
                // noticed.
                if !did_work {
                    let io_in_flight = env_clone
                        .lock()
                        .map(|env| env.io_in_flight())
                        .unwrap_or(true);
                    wake.wait(io_in_flight);
                }
            }
        }));

        // The artifact store is a lazily-probed cache directory — ready immediately.
        // Warm it with the standard library in the background: on a cold cache
        // (first run per compiler build) this compiles std once, per-module, so
        // later imports — this session's included, once each module's artifact
        // lands — link instead of compiling. Sessions never wait on it: a miss just
        // compiles from source (and saves the artifact itself). Debug to match the
        // REPL's compile options.
        let artifact_store = Rc::new(quiver_compiler::ArtifactStore::cache());
        {
            // The warmer gets its OWN store handle rather than sharing this one. A module
            // artifact holds a compile-time `Value`, and a `Value`'s payload is `Rc` — so an
            // `ArtifactStore` is not `Sync`. Sharing happens through the cache *directory*,
            // which is content-addressed: the warmer's writes are exactly what this session's
            // store reads. The only cost is that a warmed artifact reaches this session from
            // disk rather than from its in-memory map.
            thread::spawn(move || {
                let store = Rc::new(quiver_compiler::ArtifactStore::cache());
                quiver_compiler::warm_std_store(
                    &store,
                    &crate::build_builtin_registry(),
                    quiver_compiler::compiler::CompileOptions {
                        debug: true,
                        source_name: "std".to_string(),
                    },
                );
            });
        }

        Ok(Self {
            editor,
            environment,
            repl: None,
            stepping_thread,
            shutdown_signal,
            interrupt,
            waker,
            artifact_store,
        })
    }

    fn reload_modules(&mut self) {
        let Some(repl) = self.repl.as_mut() else {
            return;
        };
        repl.reload_modules(repl_resolver());
        match current_project_root() {
            Some(root) => println!(
                "{}",
                format!("Reloaded modules (project: {})", root.display()).bright_black()
            ),
            None => println!(
                "{}",
                "Reloaded modules (standard library only)".bright_black()
            ),
        }
    }

    fn reset_repl(&mut self) -> Result<(), ReplError> {
        // Tear down the outgoing session's process before starting the next: an
        // abandoned persistent process is a permanent GC root, its heap never
        // reclaimed and everything it spawned still running. The stop's ownership
        // cascade takes the whole subtree with it.
        if let Some(old_pid) = self.repl.as_ref().map(|repl| repl.process_id()) {
            let _ = self.environment.lock().unwrap().stop_process(old_pid);
        }

        let repl = {
            let mut env = self.environment.lock().unwrap();
            let builtins = crate::build_builtin_registry();
            let mut repl = Repl::new(&mut env, repl_resolver(), builtins)?;
            // The REPL is a dev loop: compile debug, so nil results explain themselves.
            repl.set_compile_options(quiver_compiler::compiler::CompileOptions {
                debug: true,
                source_name: "repl".to_string(),
            });
            repl.set_artifact_store(self.artifact_store.clone());
            repl
        };

        if std::io::stdin().is_terminal() {
            println!(
                "Quiver v{} - REPL @{}",
                env!("CARGO_PKG_VERSION"),
                repl.process_id()
            );
            match current_project_root() {
                Some(root) => println!("{}", format!("Project: {}", root.display()).bright_black()),
                None => println!("{}", "No project (standard library only)".bright_black()),
            }
            println!("Type \\? for help or \\q to exit");
            println!();
        }

        self.repl = Some(repl);
        Ok(())
    }

    pub fn run(mut self) -> Result<(), ReadlineError> {
        if let Err(e) = self.reset_repl() {
            eprintln!("Fatal: Failed to start REPL: {}", e);
            return Ok(());
        }

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
                        self.interrupt.store(false, Ordering::Relaxed);
                        let outcome = self.evaluate(line);
                        if self.interrupt.swap(false, Ordering::Relaxed) {
                            // Ctrl-C is deterministic: however the race with completion
                            // fell, the session process was stopped (here or by the
                            // reset below), so report the interruption and restart.
                            println!("{}", "Interrupted".red());
                            println!();
                            if let Err(e) = self.reset_repl() {
                                eprintln!("Fatal: Failed to restart REPL: {}", e);
                                break;
                            }
                        } else {
                            match outcome {
                                Ok(result) => self.print(result),
                                Err(e) => {
                                    let is_runtime_error = matches!(e, ReplError::Runtime(_));
                                    self.print_error(e, line);
                                    if is_runtime_error {
                                        println!();
                                        if let Err(e) = self.reset_repl() {
                                            eprintln!("Fatal: Failed to restart REPL: {}", e);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
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

        self.editor.save_history(HISTORY_FILE)?;

        if std::io::stdin().is_terminal() {
            println!("Bye!")
        }

        Ok(())
    }

    fn get_prompt(&self) -> String {
        format!("{} ", ">>-".white().bold())
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
                println!();
                if let Err(e) = self.reset_repl() {
                    eprintln!("Fatal: Failed to restart REPL: {}", e);
                    return false;
                }
            }

            ["r"] => {
                self.reload_modules();
            }

            ["v"] => {
                self.list_variables();
            }

            ["x"] => {
                // The result type is compiler-side (REPL id space), so format it with the
                // REPL's program, not the environment's merged one.
                let repl = self.repl.as_ref().unwrap();
                let formatted_type = repl.format_type(repl.get_last_result_type());
                println!("{}", formatted_type.bright_black());
            }

            ["p"] => {
                self.list_processes();
            }

            ["p", process_id_str] => match process_id_str.parse::<usize>() {
                Ok(id) => self.inspect_process(id),
                Err(_) => eprintln!(
                    "{}",
                    format!("Invalid process ID: {}", process_id_str).red()
                ),
            },

            ["w"] => {
                self.list_workers();
            }

            ["w", worker_id_str] => match worker_id_str.parse::<usize>() {
                Ok(id) => self.inspect_worker(id),
                Err(_) => {
                    eprintln!("{}", format!("Invalid worker ID: {}", worker_id_str).red())
                }
            },

            _ => {
                eprintln!("{}", format!("Unknown command: \\{}", command).red());
            }
        }

        true
    }

    fn wait_for_result(
        &mut self,
        request_id: u64,
    ) -> Result<RequestResult, quiver_environment::EnvironmentError> {
        loop {
            // The background thread is now stepping, but we still want to
            // step here to ensure progress on this request
            self.environment.lock().unwrap().step()?;

            match self.environment.lock().unwrap().poll_request(request_id)? {
                Some(result) => return Ok(result),
                None => std::thread::sleep(std::time::Duration::from_micros(10)),
            }
        }
    }

    /// Wait for an evaluation result, honouring Ctrl-C: on interrupt, stop the session
    /// process — host teardown, so everything it spawned dies with it — and keep
    /// waiting. The pending request then resolves (with the `Killed` error if the stop
    /// beat the result), and the line loop reads the still-set interrupt flag to report
    /// the interruption and reset the session.
    fn wait_for_eval_result(
        &mut self,
        request_id: u64,
        process_id: quiver_core::process::ProcessId,
    ) -> Result<RequestResult, quiver_environment::EnvironmentError> {
        let mut stopped = false;
        loop {
            if !stopped && self.interrupt.load(Ordering::Relaxed) {
                self.environment.lock().unwrap().stop_process(process_id)?;
                stopped = true;
            }

            self.environment.lock().unwrap().step()?;

            match self.environment.lock().unwrap().poll_request(request_id)? {
                Some(result) => return Ok(result),
                None => std::thread::sleep(std::time::Duration::from_micros(10)),
            }
        }
    }

    fn list_variables(&self) {
        let vars = self.repl.as_ref().unwrap().get_variables();
        if vars.is_empty() {
            println!("{}", "No variables defined".bright_black());
        } else {
            println!("{}", "Variables:".bright_black());
            for (name, formatted_type) in vars {
                println!(
                    "{}",
                    format!("  {}: {}", name, formatted_type).bright_black()
                );
            }
        }
    }

    fn list_processes(&mut self) {
        let request_id = match self.environment.lock().unwrap().request_statuses() {
            Ok(id) => id,
            Err(e) => {
                eprintln!(
                    "{}",
                    format!("Error requesting process statuses: {}", e).red()
                );
                return;
            }
        };

        match self.wait_for_result(request_id) {
            Ok(RequestResult::Statuses(statuses)) => {
                if statuses.is_empty() {
                    println!("{}", "No processes".bright_black());
                } else {
                    println!("{}", "Processes:".bright_black());
                    let mut processes: Vec<_> = statuses.into_iter().collect();
                    processes.sort_by_key(|(id, _)| *id);
                    for (id, status) in processes {
                        println!("{}", format!("  {}: {:?}", id, status).bright_black());
                    }
                }
            }
            Ok(_) => eprintln!("{}", "Unexpected result type".red()),
            Err(e) => eprintln!("{}", format!("Error getting statuses: {}", e).red()),
        }
    }

    fn inspect_process(&mut self, id: usize) {
        let request_id = match self.environment.lock().unwrap().request_process_info(id) {
            Ok(id) => id,
            Err(e) => {
                eprintln!("{}", format!("Error requesting process info: {}", e).red());
                return;
            }
        };

        match self.wait_for_result(request_id) {
            Ok(RequestResult::ProcessInfo(Some(info))) => {
                println!("{}", format!("Process {}:", id).bright_black());

                // Show status with persistent annotation
                let status_line = if info.persistent {
                    format!("  Status: {:?} (persistent)", info.status)
                } else {
                    format!("  Status: {:?}", info.status)
                };
                println!("{}", status_line.bright_black());
                println!(
                    "{}",
                    format!(
                        "  Stack: {} ({})",
                        info.stack_size,
                        format_bytes(info.heap.stack.bytes)
                    )
                    .bright_black()
                );
                println!(
                    "{}",
                    format!(
                        "  Locals: {} ({})",
                        info.locals_count,
                        format_bytes(info.heap.locals.bytes)
                    )
                    .bright_black()
                );
                println!(
                    "{}",
                    format!("  Frames: {}", info.frames_count).bright_black()
                );
                println!(
                    "{}",
                    format!(
                        "  Mailbox: {} ({})",
                        info.mailbox_size,
                        format_bytes(info.heap.mailbox.bytes)
                    )
                    .bright_black()
                );
                // Distinct across all roots, so it is ≤ the sum of the per-root figures above
                // (a binary referenced from two roots is counted once here).
                println!(
                    "{}",
                    format!(
                        "  Binaries: {} · {}",
                        info.heap.total.binaries,
                        format_bytes(info.heap.total.bytes)
                    )
                    .bright_black()
                );

                // Show type
                let type_str = info
                    .function_index
                    .and_then(|idx| self.environment.lock().unwrap().format_process_type(idx))
                    .unwrap_or_else(|| "―".to_string());
                println!("{}", format!("  Type: {}", type_str).bright_black());

                if let Some(Ok(ref value)) = info.result {
                    println!(
                        "{}",
                        format!(
                            "  Result: {}",
                            self.environment.lock().unwrap().format_value(value)
                        )
                        .bright_black()
                    );
                } else if let Some(Err(ref err)) = info.result {
                    println!("{}", format!("  Result: Error({:?})", err).bright_black());
                } else {
                    println!("{}", "  Result: ―".bright_black());
                }
            }
            Ok(RequestResult::ProcessInfo(None)) => {
                eprintln!("{}", format!("Process {} not found", id).red());
            }
            Ok(_) => eprintln!("{}", "Unexpected result type".red()),
            Err(e) => eprintln!("{}", format!("Error inspecting process: {}", e).red()),
        }
    }

    fn list_workers(&mut self) {
        let request_id = match self.environment.lock().unwrap().request_worker_info() {
            Ok(id) => id,
            Err(e) => {
                eprintln!("{}", format!("Error requesting worker info: {}", e).red());
                return;
            }
        };

        match self.wait_for_result(request_id) {
            Ok(RequestResult::WorkerInfo(workers)) => {
                println!("{}", format!("Workers ({}):", workers.len()).bright_black());
                for worker in workers {
                    let proc_count = worker.process_ids.len();
                    let mut line = format!(
                        "  Worker {}: {} proc{} · {} binar{} · {}",
                        worker.worker_id,
                        proc_count,
                        if proc_count == 1 { "" } else { "s" },
                        worker.live_binaries,
                        if worker.live_binaries == 1 {
                            "y"
                        } else {
                            "ies"
                        },
                        format_bytes(worker.live_bytes)
                    );
                    if worker.shared_bytes > 0 {
                        line.push_str(&format!(" ({} shared)", format_bytes(worker.shared_bytes)));
                    }
                    println!("{}", line.bright_black());
                }
            }
            Ok(_) => eprintln!("{}", "Unexpected result type".red()),
            Err(e) => eprintln!("{}", format!("Error getting worker info: {}", e).red()),
        }
    }

    fn inspect_worker(&mut self, id: usize) {
        let request_id = match self.environment.lock().unwrap().request_worker_info() {
            Ok(id) => id,
            Err(e) => {
                eprintln!("{}", format!("Error requesting worker info: {}", e).red());
                return;
            }
        };

        match self.wait_for_result(request_id) {
            Ok(RequestResult::WorkerInfo(workers)) => {
                match workers.iter().find(|w| w.worker_id as usize == id) {
                    Some(worker) => {
                        println!("{}", format!("Worker {}:", worker.worker_id).bright_black());
                        let pids: Vec<String> =
                            worker.process_ids.iter().map(|p| p.to_string()).collect();
                        println!(
                            "{}",
                            format!(
                                "  Processes: {}  [{}]",
                                worker.process_ids.len(),
                                pids.join(", ")
                            )
                            .bright_black()
                        );
                        // Distinct buffers, counted by identity: one shared between two
                        // processes — or two workers — appears once.
                        println!(
                            "{}",
                            format!(
                                "  Binaries: {} · {}",
                                worker.live_binaries,
                                format_bytes(worker.live_bytes)
                            )
                            .bright_black()
                        );
                        // Bytes whose allocation has another holder — another value here, or
                        // one on another worker, since a send passes the handle.
                        println!(
                            "{}",
                            format!("  Shared: {}", format_bytes(worker.shared_bytes))
                                .bright_black()
                        );
                        // Unrealised ropes. Every read realises one and nothing caches the
                        // result, so a deep rope read repeatedly redoes the work each time.
                        if worker.rope_binaries > 0 {
                            println!(
                                "{}",
                                format!(
                                    "  Ropes: {} unrealised · max depth {}",
                                    worker.rope_binaries, worker.max_rope_depth
                                )
                                .bright_black()
                            );
                        }
                        println!(
                            "{}",
                            format!(
                                "  Constants: {} · {}",
                                worker.constant_binaries,
                                format_bytes(worker.constant_bytes)
                            )
                            .bright_black()
                        );
                    }
                    None => {
                        eprintln!("{}", format!("Worker {} not found", id).red());
                    }
                }
            }
            Ok(_) => eprintln!("{}", "Unexpected result type".red()),
            Err(e) => eprintln!("{}", format!("Error inspecting worker: {}", e).red()),
        }
    }

    fn evaluate(&mut self, line: &str) -> Result<Option<EvaluationResult>, ReplError> {
        // First, fetch all process types
        let types_request_id = self
            .environment
            .lock()
            .unwrap()
            .request_process_types()
            .map_err(ReplError::Environment)?;

        // Wait for process types
        let process_types = match self
            .wait_for_result(types_request_id)
            .map_err(ReplError::Environment)?
        {
            RequestResult::ProcessTypes(types) => types,
            _ => {
                return Err(ReplError::Environment(
                    quiver_environment::EnvironmentError::UnexpectedResultType,
                ));
            }
        };

        // Now evaluate with the process types
        let request_id = match self.repl.as_mut().unwrap().evaluate(
            &mut *self.environment.lock().unwrap(),
            line,
            process_types,
        )? {
            Some(id) => id,
            None => {
                // No executable code (e.g., only type definitions)
                return Ok(None);
            }
        };
        let repl_pid = self.repl.as_ref().unwrap().process_id();

        // Wait for the evaluation result
        match self
            .wait_for_eval_result(request_id, repl_pid)
            .map_err(ReplError::Environment)?
        {
            RequestResult::Result(Ok(value), _) => {
                // The worker has already released this line's orphaned locals as part of delivering
                // the result (the keep-set was handed to it via `request_result`), so `\p`/`\w`
                // reflect the post-line heap with no extra round-trip here.
                Ok(Some(EvaluationResult { value }))
            }
            RequestResult::Result(Err(e), _) => Err(ReplError::Runtime(e)),
            _ => Err(ReplError::Environment(
                quiver_environment::EnvironmentError::UnexpectedResultType,
            )),
        }
    }

    fn print(&self, result: Option<EvaluationResult>) {
        match result {
            Some(EvaluationResult { value }) => {
                let formatted_value = self.environment.lock().unwrap().format_value(&value);

                // Show type for functions, builtins, and processes; failure provenance
                // for stamped nil results (debug builds).
                let output = match &value {
                    WireValue::Function(..) | WireValue::Builtin(..) | WireValue::Process(..) => {
                        let mut env = self.environment.lock().unwrap();
                        // Type derivation reads only ids and the discriminant, both of which
                        // the display form preserves.
                        let value_type = env.value_to_type(&value.for_display().0);
                        let formatted_type = env.format_type(&value_type);
                        format!(
                            "{} {}",
                            formatted_value,
                            format!("({})", formatted_type).bright_black()
                        )
                    }
                    value if value.is_nil() => {
                        let env = self.environment.lock().unwrap();
                        match env.describe_origin(value) {
                            Some(origin) => format!(
                                "{} {}",
                                formatted_value,
                                format!("({})", origin).bright_black()
                            ),
                            None => formatted_value,
                        }
                    }
                    _ => formatted_value,
                };

                println!("{}", output);
            }
            None => {
                // No executable code (e.g., only type definitions)
            }
        }
    }

    fn print_error(&self, error: ReplError, source: &str) {
        // Yellow for parser/compiler errors (non-fatal, no side effects)
        // Red for runtime errors (fatal, terminates the REPL process)
        match error {
            ReplError::Parser(e) => {
                // Check if stderr is a terminal and NO_COLOR is not set
                let use_color =
                    std::io::stderr().is_terminal() && std::env::var("NO_COLOR").is_err();

                if use_color {
                    // Use ariadne for visual error display
                    crate::diagnostics::eprint(&e, "repl", source);
                } else {
                    // Plain text fallback
                    eprintln!("{}", format!("Parse error: {}", e).yellow());
                }
            }
            ReplError::Compiler(e) => {
                eprintln!("{}", format!("Compile error: {:?}", e).yellow());
            }
            ReplError::Runtime(e) => {
                eprintln!("{}", format!("Runtime error: {}", e.crash_message()).red());
            }
            ReplError::Environment(e) => {
                eprintln!("{}", format!("Environment error: {}", e).red());
            }
        }
    }
}

impl Drop for ReplCli {
    fn drop(&mut self) {
        // Signal the background thread to stop, then wake it so it observes the flag: parked in
        // `WakeSignal::wait`, it is blocked on a receive the flag cannot interrupt.
        self.shutdown_signal.store(true, Ordering::Relaxed);
        self.waker.wake();

        // Wait for the thread to finish
        if let Some(handle) = self.stepping_thread.take() {
            let _ = handle.join();
        }
    }
}

/// Human-readable byte count for the REPL inspectors (e.g. `0 B`, `1.2 KB`, `3.4 MB`).
fn format_bytes(bytes: usize) -> String {
    const KB: f64 = 1024.0;
    const MB: f64 = 1024.0 * 1024.0;
    if bytes < 1024 {
        format!("{bytes} B")
    } else if (bytes as f64) < MB {
        format!("{:.1} KB", bytes as f64 / KB)
    } else {
        format!("{:.1} MB", bytes as f64 / MB)
    }
}
