use clap::{CommandFactory, Parser, Subcommand};
use quiver::Quiver;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::fs;
use std::io::{self, IsTerminal, Read};

const HISTORY_FILE: &str = ".quiv_history";

#[derive(Parser)]
#[command(name = "quiv")]
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
        debug: bool,

        #[arg(short, long)]
        eval: Option<String>,
    },

    Run {
        input: Option<String>,

        #[arg(short, long)]
        eval: Option<String>,
    },

    Inspect {
        input: Option<String>,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    match cli.command {
        Some(Commands::Repl) => run_repl()?,
        Some(Commands::Compile {
            input,
            output,
            debug,
            eval,
        }) => compile_command(input, output, debug, eval)?,
        Some(Commands::Run { input, eval }) => run_command(input, eval)?,
        Some(Commands::Inspect { input }) => inspect_command(input)?,
        None => Cli::command().print_help().unwrap(),
    }

    Ok(())
}

fn run_repl() -> Result<(), ReadlineError> {
    if std::io::stdin().is_terminal() {
        println!("Quiver v0.1.0");
        println!("Type \\? for help or \\q to exit");
        println!();
    }

    let mut rl = Editor::<(), rustyline::history::DefaultHistory>::new()?;
    let _ = rl.load_history(HISTORY_FILE);
    let mut quiver = Quiver::new(None);

    loop {
        let readline = rl.readline(">>- ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                rl.add_history_entry(line)?;

                match line {
                    "\\?" => {
                        println!("Available commands:");
                        println!("  \\? - Show this help message");
                        println!("  \\q - Exit the REPL");
                        println!("  \\! - Reset the environment");
                        println!("  \\v - List all variables");
                        println!("  \\t - List all type aliases");
                        continue;
                    }

                    "\\q" => {
                        break;
                    }

                    "\\!" => {
                        quiver = Quiver::new(None);
                        println!("Environment reset");
                        continue;
                    }

                    "\\v" => {
                        let variables = quiver.list_variables();
                        if variables.is_empty() {
                            println!("No variables defined");
                        } else {
                            println!("Variables:");
                            for (name, value) in variables {
                                println!("  {} = {}", name, quiver.format_value(&value));
                            }
                        }
                        continue;
                    }

                    "\\t" => {
                        let types = quiver.list_types();
                        if types.is_empty() {
                            println!("No types defined");
                        } else {
                            println!("Type:");
                            for (_name, type_id) in types {
                                println!("  {}", quiver.format_type(&type_id));
                            }
                        }
                        continue;
                    }

                    _ => {
                        let module_path = std::env::current_dir().ok();
                        match quiver.evaluate(line, module_path) {
                            Ok(Some(value)) => println!("{}", quiver.format_value(&value)),
                            Ok(None) => {}
                            Err(error) => eprintln!("{}", error),
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("(Use \\q to quit)");
                continue;
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

    rl.save_history(HISTORY_FILE)?;

    if std::io::stdin().is_terminal() {
        println!("Bye!")
    }

    Ok(())
}

fn compile_command(
    input: Option<String>,
    output: Option<String>,
    debug: bool,
    eval: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut quiver = Quiver::new(None);

    let (source, module_path) = if let Some(code) = eval {
        (code, Some(std::env::current_dir()?))
    } else if let Some(path) = input {
        let script_path = std::path::Path::new(&path);
        let module_path = script_path.parent().map(|p| p.to_path_buf());
        (fs::read_to_string(&path)?, module_path)
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        (buffer, Some(std::env::current_dir()?))
    };

    let mut bytecode = quiver.compile(&source, module_path)?;

    if !debug {
        bytecode = bytecode.without_debug_info();
    }

    let json = if debug {
        serde_json::to_string_pretty(&bytecode)?
    } else {
        serde_json::to_string(&bytecode)?
    };

    if let Some(output_path) = output {
        fs::write(output_path, json)?;
    } else {
        println!("{}", json);
    }

    Ok(())
}

fn handle_result(result: Result<Option<quiver::vm::Value>, quiver::Error>, quiver: &Quiver) {
    match result {
        Ok(Some(value)) => println!("{}", quiver.format_value(&value)),
        Ok(None) => {}
        Err(err) => {
            eprintln!("Error: {}", err);
            std::process::exit(1);
        }
    }
}

fn run_command(
    input: Option<String>,
    eval: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut quiver = Quiver::new(None);

    if let Some(code) = eval {
        let module_path = Some(std::env::current_dir()?);
        handle_result(quiver.evaluate(&code, module_path), &quiver);
    } else if let Some(path) = input {
        let content = fs::read_to_string(&path)?;

        if path.ends_with(".qv") {
            let script_path = std::path::Path::new(&path);
            let module_path = script_path.parent().map(|p| p.to_path_buf());
            handle_result(quiver.evaluate(&content, module_path), &quiver);
        } else if path.ends_with(".qx") {
            let bytecode: quiver::bytecode::Bytecode = serde_json::from_str(&content)?;
            handle_result(quiver.execute(bytecode), &quiver);
        } else {
            eprintln!(
                "Error: Unsupported file extension - expected .qv for source or .qx for bytecode."
            );
            std::process::exit(1);
        }
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;

        if buffer.trim_start().starts_with('{') {
            match serde_json::from_str::<quiver::bytecode::Bytecode>(&buffer) {
                Ok(bytecode) => handle_result(quiver.execute(bytecode), &quiver),
                Err(_) => {
                    let module_path = Some(std::env::current_dir()?);
                    handle_result(quiver.evaluate(&buffer, module_path), &quiver);
                }
            }
        } else {
            let module_path = Some(std::env::current_dir()?);
            handle_result(quiver.evaluate(&buffer, module_path), &quiver);
        }
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

    let bytecode: quiver::bytecode::Bytecode = serde_json::from_str(&content)?;

    println!("Bytecode Inspection:");
    println!("==================");
    println!("Constants: {}", bytecode.constants.len());
    for (i, constant) in bytecode.constants.iter().enumerate() {
        println!("  {}: {:?}", i, constant);
    }

    println!("\nFunctions: {}", bytecode.functions.len());
    for (i, function) in bytecode.functions.iter().enumerate() {
        println!("  Function {}:", i);
        println!("    Captures: {:?}", function.captures);
        println!("    Instructions: {}", function.instructions.len());
        for (j, instruction) in function.instructions.iter().enumerate() {
            println!("      {}: {:?}", j, instruction);
        }
    }

    if let Some(entry) = bytecode.entry {
        println!("\nEntry point: Function {}", entry);
    } else {
        println!("\nNo entry point defined");
    }

    Ok(())
}
