use colored::Colorize;
use quiver_cli::spawn_worker;
use quiver_compiler::FileSystemModuleLoader;
use quiver_core::bytecode::TypeId;
use quiver_core::program::Program;
use quiver_core::types::Type;
use quiver_environment::{Repl, ReplError, WorkerHandle};
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::io::IsTerminal;

const HISTORY_FILE: &str = ".quiv_history";

pub struct ReplCli {
    editor: Editor<(), rustyline::history::DefaultHistory>,
    repl: Repl,
    result_type: Option<Type>,
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
            result_type: None,
        })
    }

    pub fn run(mut self) -> Result<(), ReadlineError> {
        if std::io::stdin().is_terminal() {
            println!("Quiver v0.1.0");
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
        let prompt = match &self.result_type {
            None => ">>-".white(),
            Some(Type::Tuple(type_id)) if *type_id == TypeId::NIL => ">>-".red(),
            Some(Type::Tuple(type_id)) if *type_id == TypeId::OK => ">>-".green(),
            Some(_) => ">>-".blue(),
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

    fn list_variables(&self) {
        let vars = self.repl.get_variables();
        if vars.is_empty() {
            println!("{}", "No variables defined".bright_black());
        } else {
            println!("{}", "Variables:".bright_black());
            for (name, ty) in vars {
                let formatted_type = self.repl.format_type(&ty);
                println!("{}", format!("  {} : {}", name, formatted_type).bright_black());
            }
        }
    }

    fn list_processes(&mut self) {
        match self.repl.get_process_statuses() {
            Ok(statuses) => {
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
            Err(e) => {
                eprintln!("{}", format!("Error listing processes: {}", e).red());
            }
        }
    }

    fn inspect_process(&mut self, id: usize) {
        match self.repl.get_process_info(id) {
            Ok(Some(info)) => {
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
            Ok(None) => {
                eprintln!("{}", format!("Process {} not found", id).red());
            }
            Err(e) => {
                eprintln!("{}", format!("Error inspecting process: {}", e).red());
            }
        }
    }

    fn evaluate(&mut self, line: &str) {
        let request = match self.repl.evaluate(line) {
            Ok(req) => req,
            Err(e) => {
                eprintln!("{}", self.format_error(e).red());
                return;
            }
        };

        match self.repl.wait_evaluate(request) {
            Ok((value, heap)) => {
                // Update result type for colored prompt
                match &value {
                    quiver_core::value::Value::Tuple(type_id, _) if *type_id == TypeId::NIL => {
                        self.result_type = Some(Type::nil());
                    }
                    quiver_core::value::Value::Tuple(type_id, _) if *type_id == TypeId::OK => {
                        self.result_type = Some(Type::ok());
                    }
                    _ => {
                        self.result_type = None;
                    }
                }

                // Don't print OK or NIL tuples
                if !matches!(value, quiver_core::value::Value::Tuple(type_id, _) if type_id == TypeId::NIL || type_id == TypeId::OK)
                {
                    println!("{}", self.repl.format_value(&value, &heap));
                }
            }
            Err(e) => {
                eprintln!("{}", self.format_error(e).red());
            }
        }
    }

    fn format_error(&self, error: ReplError) -> String {
        match error {
            ReplError::ParseError(e) => format!("Parse error: {:?}", e),
            ReplError::CompileError(e) => format!("Compile error: {:?}", e),
            ReplError::RuntimeError(e) => format!("Runtime error: {:?}", e),
            ReplError::Other(msg) => format!("Error: {}", msg),
        }
    }
}
