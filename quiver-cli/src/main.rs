use quiver_cli::spawn_worker;
use quiver_compiler::FileSystemModuleLoader;
use quiver_core::program::Program;
use quiver_environment::{Repl, ReplError, WorkerHandle};
use std::io::{self, Write};

fn format_error(error: ReplError) -> String {
    match error {
        ReplError::ParseError(e) => format!("Parse error: {:?}", e),
        ReplError::CompileError(e) => format!("Compile error: {:?}", e),
        ReplError::RuntimeError(e) => format!("Runtime error: {:?}", e),
        ReplError::Other(msg) => format!("Error: {}", msg),
    }
}

fn main() {
    let num_workers = 4;

    println!("Quiver REPL");
    println!("Type expressions to evaluate, or :help for commands\n");

    // Create an empty program
    let program = Program::new();

    // Spawn workers
    let mut workers: Vec<Box<dyn WorkerHandle>> = Vec::new();
    for _ in 0..num_workers {
        workers.push(Box::new(spawn_worker()));
    }

    // Create REPL with filesystem module loader
    let module_loader = Box::new(FileSystemModuleLoader::new());
    let mut repl = match Repl::new(workers, program, module_loader) {
        Ok(repl) => repl,
        Err(e) => {
            eprintln!("Failed to create REPL: {}", e);
            return;
        }
    };

    // Main REPL loop
    loop {
        // Print prompt
        print!("> ");
        if let Err(e) = io::stdout().flush() {
            eprintln!("Failed to flush stdout: {}", e);
            break;
        }

        // Read line
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => {
                // EOF encountered (Ctrl-D)
                println!("\nGoodbye!");
                break;
            }
            Ok(_) => {}
            Err(e) => {
                eprintln!("Failed to read input: {}", e);
                break;
            }
        }

        let input = input.trim();

        // Handle empty input
        if input.is_empty() {
            continue;
        }

        // Handle commands
        if input.starts_with(':') {
            match input {
                ":quit" | ":q" => {
                    println!("Goodbye!");
                    break;
                }
                ":help" | ":h" => {
                    println!("Commands:");
                    println!("  :help, :h      - Show this help");
                    println!("  :quit, :q      - Exit REPL");
                    println!("  :vars, :v      - List all variables");
                    continue;
                }
                ":vars" | ":v" => {
                    let vars = repl.list_variables();
                    if vars.is_empty() {
                        println!("No variables defined");
                    } else {
                        println!("Variables:");
                        for (name, ty) in vars {
                            println!("  {} : {:?}", name, ty);
                        }
                    }
                    continue;
                }
                _ => {
                    eprintln!("Unknown command: {}", input);
                    eprintln!("Type :help for available commands");
                    continue;
                }
            }
        }

        // Evaluate expression
        let request = match repl.evaluate(input) {
            Ok(req) => req,
            Err(e) => {
                eprintln!("{}", format_error(e));
                continue;
            }
        };

        // Wait for result
        match repl.wait_evaluate(request) {
            Ok((value, heap)) => {
                let formatted = repl.format_value(&value, &heap);
                println!("{}", formatted);
            }
            Err(e) => {
                eprintln!("{}", format_error(e));
            }
        }
    }
}
