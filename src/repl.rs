use colored::Colorize;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::collections::HashMap;
use std::io::IsTerminal;

use crate::Quiver;
use crate::bytecode::TypeId;
use crate::types::Type;
use crate::vm::{ProcessId, ProcessStatus, Value};

const HISTORY_FILE: &str = ".quiv_history";

pub struct Repl {
    editor: Editor<(), rustyline::history::DefaultHistory>,
    quiver: Quiver,
    variables: HashMap<String, (Type, usize)>,
    last_result: Option<(Value, Type)>,
}

impl Repl {
    pub fn new() -> Result<Self, ReadlineError> {
        let mut editor = Editor::<(), rustyline::history::DefaultHistory>::new()?;
        let _ = editor.load_history(HISTORY_FILE);

        Ok(Self {
            editor,
            quiver: Quiver::new(None),
            variables: HashMap::new(),
            last_result: None,
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
        let prompt = match &self.last_result {
            None => ">>-".white(),
            Some((Value::Tuple(type_id, _), _)) if *type_id == TypeId::NIL => ">>-".red(),
            Some((Value::Tuple(type_id, _), _)) if *type_id == TypeId::OK => ">>-".green(),
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
                println!("{}", "  \\t - List type aliases".bright_black());
                println!("{}", "  \\p - List processes".bright_black());
                println!("{}", "  \\p X - Inspect process with ID X".bright_black());
            }

            ["q"] => {
                return false;
            }

            ["!"] => {
                self.quiver = Quiver::new(None);
                self.variables.clear();
                self.last_result = None;
                println!("{}", "Environment reset".bright_black());
            }

            ["v"] => {
                self.list_variables();
            }

            ["t"] => {
                self.list_types();
            }

            ["p"] => {
                self.list_processes();
            }

            ["p", process_id_str] => match process_id_str.parse::<usize>() {
                Ok(id) => self.inspect_process(ProcessId(id)),
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
        let vars = self.quiver.get_variables(&self.variables);
        if vars.is_empty() {
            println!("{}", "No variables defined".bright_black());
        } else {
            // Sort by index to maintain definition order
            let mut sorted_vars: Vec<_> = vars.into_iter().collect();
            sorted_vars.sort_by_key(|(name, _)| self.variables[name].1);

            println!("{}", "Variables:".bright_black());
            for (name, value) in sorted_vars {
                let (var_type, _) = &self.variables[name.as_str()];
                println!(
                    "{}",
                    format!(
                        "  {} = {} ({})",
                        name,
                        self.quiver.format_value(&value),
                        self.quiver.format_type(var_type)
                    )
                    .bright_black()
                );
            }
        }
    }

    fn list_types(&self) {
        let types = self.quiver.get_types();
        if types.is_empty() {
            println!("{}", "No types defined".bright_black());
        } else {
            // Sort by TypeId
            let mut types: Vec<_> = types.iter().collect();
            types.sort_by_key(|(id, _)| id.0);
            println!("{}", "Types:".bright_black());
            for (type_id, _) in types {
                println!(
                    "{}",
                    format!(
                        "  {}: {}",
                        type_id.0,
                        self.quiver.format_type(&Type::Tuple(*type_id))
                    )
                    .bright_black()
                )
            }
        }
    }

    fn list_processes(&self) {
        match self.quiver.get_process_statuses() {
            Ok(statuses) => {
                if statuses.is_empty() {
                    println!("{}", "No processes".bright_black());
                } else {
                    let mut processes: Vec<_> = statuses.into_iter().collect();
                    processes.sort_by_key(|(id, _)| id.0);
                    println!("{}", "Processes:".bright_black());
                    for (id, status) in processes {
                        let status_str = match status {
                            ProcessStatus::Running => "running",
                            ProcessStatus::Queued => "queued",
                            ProcessStatus::Waiting => "waiting",
                            ProcessStatus::Sleeping => "sleeping",
                            ProcessStatus::Terminated => "terminated",
                        };
                        println!("{}", format!("  {}: {}", id.0, status_str).bright_black());
                    }
                }
            }
            Err(e) => {
                eprintln!("{}", format!("Error listing processes: {:?}", e).red());
            }
        }
    }

    fn inspect_process(&self, id: ProcessId) {
        match self.quiver.get_process_info(id) {
            Ok(Some(info)) => {
                let status_str = match info.status {
                    ProcessStatus::Running => "running",
                    ProcessStatus::Queued => "queued",
                    ProcessStatus::Waiting => "waiting",
                    ProcessStatus::Sleeping => "sleeping",
                    ProcessStatus::Terminated => "terminated",
                };
                let persistent_indicator = if info.persistent { " (persistent)" } else { "" };

                println!("{}", format!("Process {}:", id.0).bright_black());
                println!(
                    "{}",
                    format!("  Status: {}{}", status_str, persistent_indicator).bright_black()
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

                if let Some(result) = &info.result {
                    println!(
                        "{}",
                        format!("  Result: {}", self.quiver.format_value(result)).bright_black()
                    );
                } else {
                    println!("{}", "  Result: ―".bright_black());
                }
            }
            Ok(None) => {
                eprintln!("{}", format!("Process {} not found", id.0).red());
            }
            Err(e) => {
                eprintln!("{}", format!("Error inspecting process: {:?}", e).red());
            }
        }
    }

    fn evaluate(&mut self, line: &str) {
        let module_path = std::env::current_dir().ok();

        match self.quiver.evaluate(
            line,
            module_path,
            Some(&self.variables),
            self.last_result.as_ref().map(|(v, t)| (v, t.clone())),
        ) {
            Ok((result, result_type, new_variables)) => {
                self.variables = new_variables;

                if let Some(value) = &result {
                    if !matches!(value, Value::Tuple(type_id, _) if *type_id == TypeId::NIL || *type_id == TypeId::OK)
                    {
                        println!(
                            "{} {}",
                            self.quiver.format_value(value),
                            format!("({})", self.quiver.format_type(&result_type)).bright_black()
                        );
                    }
                }

                self.last_result = result.map(|v| (v, result_type));
            }
            Err(error) => eprintln!("{}", error.to_string().red()),
        }
    }
}
