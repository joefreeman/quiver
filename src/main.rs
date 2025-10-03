use clap::{CommandFactory, Parser, Subcommand};
use colored::Colorize;
use quiver::Quiver;
use quiver::types::Type;
use quiver::vm::Value;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::collections::HashMap;
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
    let mut variables: HashMap<String, usize> = HashMap::new();
    let mut last_result: Option<Value> = None;

    loop {
        let readline = rl.readline(&format!("{} ", ">>-".blue().bold()));
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                rl.add_history_entry(line)?;

                match line {
                    "\\?" => {
                        println!("{}", "Available commands:".bright_black());
                        println!("{}", "  \\? - Show this help message".bright_black());
                        println!("{}", "  \\q - Exit the REPL".bright_black());
                        println!("{}", "  \\! - Reset the environment".bright_black());
                        println!("{}", "  \\v - List all variables".bright_black());
                        println!("{}", "  \\t - List all type aliases".bright_black());
                    }

                    "\\q" => {
                        break;
                    }

                    "\\!" => {
                        quiver = Quiver::new(None);
                        variables.clear();
                        last_result = None;
                        println!("{}", "Environment reset".bright_black());
                    }

                    "\\v" => {
                        let vars = quiver.get_variables(&variables);
                        if vars.is_empty() {
                            println!("{}", "No variables defined".bright_black());
                        } else {
                            // Sort by index to maintain definition order
                            let mut sorted_vars: Vec<_> = vars.into_iter().collect();
                            sorted_vars.sort_by_key(|(name, _)| variables[name]);

                            println!("{}", "Variables:".bright_black());
                            for (name, value) in sorted_vars {
                                println!(
                                    "{}",
                                    format!("  {} = {}", name, quiver.format_value(&value))
                                        .bright_black()
                                );
                            }
                        }
                    }

                    "\\t" => {
                        let mut types = quiver.list_types();
                        if types.is_empty() {
                            println!("{}", "No types defined".bright_black());
                        } else {
                            // Sort by TypeId
                            types.sort_by_key(|(_, id)| id.0);
                            println!("{}", "Types:".bright_black());
                            for (_name, type_id) in types {
                                println!(
                                    "{}",
                                    format!(
                                        "  {}: {}",
                                        type_id.0,
                                        quiver.format_type(&Type::Tuple(type_id))
                                    )
                                    .bright_black()
                                )
                            }
                        }
                    }

                    _ => {
                        let module_path = std::env::current_dir().ok();

                        match quiver.evaluate(
                            line,
                            module_path,
                            Some(&variables),
                            last_result.as_ref(),
                        ) {
                            Ok((result, new_variables)) => {
                                variables = new_variables;

                                if let Some(value) = &result {
                                    println!("{}", quiver.format_value(value));
                                }

                                last_result = result;
                            }
                            Err(error) => eprintln!("{}", error.to_string().red()),
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
        compile_execute(&mut quiver, &code, module_path)?;
    } else if let Some(path) = input {
        let content = fs::read_to_string(&path)?;

        if path.ends_with(".qv") {
            let script_path = std::path::Path::new(&path);
            let module_path = script_path.parent().map(|p| p.to_path_buf());
            compile_execute(&mut quiver, &content, module_path)?;
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
                    compile_execute(&mut quiver, &buffer, module_path)?;
                }
            }
        } else {
            let module_path = Some(std::env::current_dir()?);
            compile_execute(&mut quiver, &buffer, module_path)?;
        }
    }

    Ok(())
}

fn compile_execute(
    quiver: &mut Quiver,
    source: &str,
    module_path: Option<std::path::PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    let bytecode = quiver.compile(source, module_path)?;

    if bytecode.entry.is_none() {
        return Err("Program is not executable. Must evaluate to a function.".into());
    }

    let result = quiver.execute(bytecode)?;
    handle_result(Ok(result), quiver);

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

    println!("Constants:");
    for (i, constant) in bytecode.constants.iter().enumerate() {
        let formatted = match constant {
            quiver::bytecode::Constant::Integer(n) => n.to_string(),
            quiver::bytecode::Constant::Binary(bytes) => format_binary(bytes),
        };
        println!("  {}: {}", i, formatted);
    }

    let entry = bytecode.entry;

    // Create program from bytecode
    use quiver::program::Program;
    let program = Program::from_bytecode(bytecode);

    let types_map = program.get_types();
    if !types_map.is_empty() {
        println!("\nTypes:");
        let mut types: Vec<_> = types_map.iter().collect();
        types.sort_by_key(|(id, _)| id.0);
        for (type_id, _type_info) in types {
            println!(
                "  {}: {}",
                type_id.0,
                quiver::format::format_type(&program, &Type::Tuple(*type_id))
            );
        }
    }

    for (i, function) in program.get_functions().iter().enumerate() {
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
        format!("'{}'", hex)
    } else {
        let hex: String = bytes[..8].iter().map(|b| format!("{:02x}", b)).collect();
        format!("'{}...'", hex)
    }
}
