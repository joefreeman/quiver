use colored::Colorize;
use quiver_cli::spawn_worker;
use quiver_compiler::FileSystemModuleLoader;
use quiver_core::program::Program;
use quiver_core::value::Value;
use quiver_environment::{Repl, ReplError, RequestResult, WorkerHandle};
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::io::IsTerminal;

const HISTORY_FILE: &str = ".quiv_history";

pub struct ReplCli {
    editor: Editor<(), rustyline::history::DefaultHistory>,
    repl: Repl,
    last_was_nil: bool,
}

impl ReplCli {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut editor = Editor::<(), rustyline::history::DefaultHistory>::new()?;
        let _ = editor.load_history(HISTORY_FILE);

        // Create workers
        let num_workers = 4;
        let mut workers: Vec<Box<dyn WorkerHandle>> = Vec::new();
        for _ in 0..num_workers {
            workers.push(Box::new(spawn_worker()));
        }

        // Create REPL
        let program = Program::new();
        let module_loader = Box::new(FileSystemModuleLoader::new());
        let repl = Repl::new(workers, program, module_loader)
            .map_err(|e| format!("Failed to create REPL: {}", e))?;

        Ok(Self {
            editor,
            repl,
            last_was_nil: false,
        })
    }

    pub fn run(mut self) -> Result<(), ReadlineError> {
        if std::io::stdin().is_terminal() {
            println!("Quiver v{}", env!("CARGO_PKG_VERSION"));
            println!("Type \\? for help or \\q to exit");
            println!();
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

        self.editor.save_history(HISTORY_FILE)?;

        if std::io::stdin().is_terminal() {
            println!("Bye!")
        }

        Ok(())
    }

    fn get_prompt(&self) -> String {
        let prompt = if self.last_was_nil {
            ">>-".red()
        } else {
            ">>-".white()
        };
        format!("{} ", prompt.bold())
    }

    fn handle_command(&mut self, command: &str) -> bool {
        let parts: Vec<&str> = command.split_whitespace().collect();

        match parts.as_slice() {
            ["?"] => {
                println!("{}", "Available commands:".bright_black());
                println!("{}", "  \\? - Show this help message".bright_black());
                println!("{}", "  \\q - Exit the REPL".bright_black());
                println!("{}", "  \\! - Reset the environment".bright_black());
                println!("{}", "  \\v - List variables".bright_black());
                println!("{}", "  \\p - List processes".bright_black());
                println!("{}", "  \\p X - Inspect process with ID X".bright_black());
            }

            ["q"] => {
                return false;
            }

            ["!"] => match Self::new() {
                Ok(new_repl) => {
                    *self = new_repl;
                    println!("{}", "Environment reset".bright_black());
                }
                Err(e) => {
                    eprintln!("{}", format!("Error resetting environment: {}", e).red());
                }
            },

            ["v"] => {
                self.list_variables();
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
            self.repl.step()?;

            match self.repl.poll_request(request_id)? {
                Some(result) => return Ok(result),
                None => std::thread::sleep(std::time::Duration::from_micros(10)),
            }
        }
    }

    fn list_variables(&self) {
        let vars = self.repl.get_variables();
        if vars.is_empty() {
            println!("{}", "No variables defined".bright_black());
        } else {
            println!("{}", "Variables:".bright_black());
            for (name, ty) in vars {
                let formatted_type = self.repl.format_type(&ty);
                println!(
                    "{}",
                    format!("  {}: {}", name, formatted_type).bright_black()
                );
            }
        }
    }

    fn list_processes(&mut self) {
        let request_id = match self.repl.request_process_statuses() {
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
        let request_id = match self.repl.request_process_info(id) {
            Ok(id) => id,
            Err(e) => {
                eprintln!("{}", format!("Error requesting process info: {}", e).red());
                return;
            }
        };

        match self.wait_for_result(request_id) {
            Ok(RequestResult::ProcessInfo(Some(info))) => {
                println!("{}", format!("Process {}:", id).bright_black());
                println!("{}", format!("  Status: {:?}", info.status).bright_black());
                println!(
                    "{}",
                    format!("  Persistent: {}", info.persistent).bright_black()
                );
                println!("{}", format!("  Stack: {}", info.stack_size).bright_black());
                println!(
                    "{}",
                    format!("  Locals: {}", info.locals_size).bright_black()
                );
                println!(
                    "{}",
                    format!("  Frames: {}", info.frames_count).bright_black()
                );
                println!(
                    "{}",
                    format!("  Mailbox: {}", info.mailbox_size).bright_black()
                );
                if let Some(Ok(ref result)) = info.result {
                    println!(
                        "{}",
                        format!("  Result: {}", self.repl.format_value(result, &[])).bright_black()
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

    fn evaluate(&mut self, line: &str) {
        let request_id = match self.repl.evaluate(line) {
            Ok(Some(id)) => id,
            Ok(None) => {
                // No executable code (e.g., only type definitions)
                self.last_was_nil = false;
                return;
            }
            Err(e) => {
                self.last_was_nil = false;
                eprintln!("{}", self.format_error(e).red());
                return;
            }
        };

        match self.wait_for_result(request_id) {
            Ok(RequestResult::Result(Ok((value, heap)))) => {
                // Track if result was nil for colored prompt
                self.last_was_nil = value.is_nil();

                let formatted_value = self.repl.format_value(&value, &heap);

                // Show type for functions, builtins, and processes
                let output = match &value {
                    Value::Function(_, _) | Value::Builtin(_) | Value::Process(_, _) => {
                        let value_type = self.repl.value_to_type(&value);
                        let formatted_type = self.repl.format_type(&value_type);
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
            Ok(RequestResult::Result(Err(e))) => {
                self.last_was_nil = false;
                eprintln!("{}", self.format_error(ReplError::Runtime(e)).red());
            }
            Ok(_) => {
                self.last_was_nil = false;
                eprintln!("{}", "Unexpected result type".red());
            }
            Err(e) => {
                self.last_was_nil = false;
                eprintln!("{}", self.format_error(ReplError::Environment(e)).red());
            }
        }
    }

    fn format_error(&self, error: ReplError) -> String {
        match error {
            ReplError::Parser(e) => format!("Parser error: {:?}", e),
            ReplError::Compiler(e) => format!("Compiler error: {:?}", e),
            ReplError::Runtime(e) => format!("Runtime error: {:?}", e),
            ReplError::Environment(e) => format!("Environment error: {}", e),
        }
    }
}
