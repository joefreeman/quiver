use colored::Colorize;
use quiver_cli::spawn_worker;
use quiver_compiler::FileSystemModuleLoader;
use quiver_core::value::Value;
use quiver_environment::{Environment, Repl, ReplError, RequestResult, WorkerHandle};
use quiver_io::NativeEffect;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::io::IsTerminal;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};
use std::thread::{self, JoinHandle};

const HISTORY_FILE: &str = ".quiv_history";

struct EvaluationResult {
    value: Value,
    heap: Vec<Vec<u8>>,
}

pub struct ReplCli {
    editor: Editor<(), rustyline::history::DefaultHistory>,
    environment: Arc<Mutex<Environment<NativeEffect>>>,
    repl: Option<Repl<NativeEffect>>,
    stepping_thread: Option<JoinHandle<()>>,
    shutdown_signal: Arc<AtomicBool>,
}

impl ReplCli {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut editor = Editor::<(), rustyline::history::DefaultHistory>::new()?;
        let _ = editor.load_history(HISTORY_FILE);

        // Create workers with system time
        let num_workers = 4;

        // Build registry from core modules and network builtins
        let builtins = crate::build_builtin_registry();

        let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
        for _ in 0..num_workers {
            workers.push(Box::new(spawn_worker(
                || {
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .expect("System time before Unix epoch")
                        .as_millis() as u64
                },
                builtins.clone(),
                false, // Don't enable profiling in REPL
            )));
        }
        let effect_backend = crate::create_effect_backend();
        let mut environment = Environment::<NativeEffect>::new(workers, builtins.clone());

        // Set the effect backend
        if let Some(backend) = effect_backend {
            environment.set_effect_backend(backend);
        }

        // Wrap environment in Arc<Mutex> for shared access
        let environment = Arc::new(Mutex::new(environment));
        let shutdown_signal = Arc::new(AtomicBool::new(false));

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

                // Adaptive sleep: short when active, longer when idle
                let sleep_duration = if did_work {
                    std::time::Duration::from_micros(100) // 100μs when busy
                } else {
                    std::time::Duration::from_millis(10) // 10ms when idle
                };
                thread::sleep(sleep_duration);
            }
        }));

        Ok(Self {
            editor,
            environment,
            repl: None,
            stepping_thread,
            shutdown_signal,
        })
    }

    fn reset_repl(&mut self) -> Result<(), ReplError> {
        let repl = {
            let mut env = self.environment.lock().unwrap();
            let module_loader = Box::new(FileSystemModuleLoader::new());
            let builtins = crate::build_builtin_registry();
            Repl::new(&mut env, module_loader, builtins)?
        };

        if std::io::stdin().is_terminal() {
            println!(
                "Quiver v{} - REPL @{}",
                env!("CARGO_PKG_VERSION"),
                repl.process_id()
            );
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
                        match self.evaluate(line) {
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
                println!("{}", "  \\v - List variables".bright_black());
                println!("{}", "  \\p - List processes".bright_black());
                println!("{}", "  \\p X - Inspect process with ID X".bright_black());
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

            ["v"] => {
                self.list_variables();
            }

            ["x"] => {
                let result_type = self.repl.as_ref().unwrap().get_last_result_type();
                let formatted_type = self.environment.lock().unwrap().format_type(result_type);
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

    fn list_variables(&self) {
        let vars = self.repl.as_ref().unwrap().get_variables();
        if vars.is_empty() {
            println!("{}", "No variables defined".bright_black());
        } else {
            println!("{}", "Variables:".bright_black());
            for (name, ty) in vars {
                let formatted_type = self.environment.lock().unwrap().format_type(&ty);
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
                println!("{}", format!("  Stack: {}", info.stack_size).bright_black());
                println!(
                    "{}",
                    format!("  Locals: {}", info.locals_count).bright_black()
                );
                println!(
                    "{}",
                    format!("  Frames: {}", info.frames_count).bright_black()
                );
                println!(
                    "{}",
                    format!("  Mailbox: {}", info.mailbox_size).bright_black()
                );

                // Show type
                if let Some(function_index) = info.function_index {
                    if let Some(type_str) = self
                        .environment
                        .lock()
                        .unwrap()
                        .format_process_type(function_index)
                    {
                        println!("{}", format!("  Type: {}", type_str).bright_black());
                    }
                } else {
                    println!("{}", "  Type: ―".bright_black());
                }

                if let Some(Ok((ref value, ref heap))) = info.result {
                    println!(
                        "{}",
                        format!(
                            "  Result: {}",
                            self.environment.lock().unwrap().format_value(value, heap)
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

        // Wait for the evaluation result
        match self
            .wait_for_result(request_id)
            .map_err(ReplError::Environment)?
        {
            RequestResult::Result(Ok((value, heap)), _) => {
                // Compact locals after successful evaluation (optimization)
                self.repl
                    .as_mut()
                    .unwrap()
                    .compact(&mut *self.environment.lock().unwrap());

                Ok(Some(EvaluationResult { value, heap }))
            }
            RequestResult::Result(Err(e), _) => Err(ReplError::Runtime(e)),
            _ => Err(ReplError::Environment(
                quiver_environment::EnvironmentError::UnexpectedResultType,
            )),
        }
    }

    fn print(&self, result: Option<EvaluationResult>) {
        match result {
            Some(EvaluationResult { value, heap }) => {
                let formatted_value = self.environment.lock().unwrap().format_value(&value, &heap);

                // Show type for functions, builtins, and processes
                let output = match &value {
                    Value::Function(_, _) | Value::Builtin(_) | Value::Process(_, _) => {
                        let env = self.environment.lock().unwrap();
                        let value_type = env.value_to_type(&value);
                        let formatted_type = env.format_type(&value_type);
                        format!(
                            "{} {}",
                            formatted_value,
                            format!("({})", formatted_type).bright_black()
                        )
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
                    eprintln!("{}", format!("Parse error: {}", e).red());
                }
            }
            ReplError::Compiler(e) => {
                eprintln!("{}", format!("Compile error: {:?}", e).red());
            }
            ReplError::Runtime(e) => {
                eprintln!("{}", format!("Runtime error: {:?}", e).red());
            }
            ReplError::Environment(e) => {
                eprintln!("{}", format!("Environment error: {}", e).red());
            }
        }
    }
}

impl Drop for ReplCli {
    fn drop(&mut self) {
        // Signal the background thread to stop
        self.shutdown_signal.store(true, Ordering::Relaxed);

        // Wait for the thread to finish
        if let Some(handle) = self.stepping_thread.take() {
            let _ = handle.join();
        }
    }
}
