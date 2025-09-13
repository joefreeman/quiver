use clap::{CommandFactory, Parser, Subcommand};
use quiver::Quiver;
use quiver::bytecode::TypeId;
use quiver::types::Type;
use quiver::vm::Value;
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
                                println!("  {} = {}", name, format_value(&quiver, &value));
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
                                println!("  {}", format_type(&quiver, &Type::Tuple(type_id)))
                            }
                        }
                        continue;
                    }

                    _ => {
                        let module_path = std::env::current_dir().ok();
                        match quiver.evaluate(line, module_path) {
                            Ok(Some(value)) => println!("{}", format_value(&quiver, &value)),
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
        Ok(Some(value)) => println!("{}", format_value(quiver, &value)),
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
        return Err("Program not executable".into());
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

fn format_type(quiver: &Quiver, type_def: &Type) -> String {
    match type_def {
        Type::Integer => "int".to_string(),
        Type::Binary => "bin".to_string(),
        Type::Tuple(type_id) => {
            if let Some((name, fields)) = quiver.lookup_type(type_id) {
                if fields.is_empty() {
                    name.as_deref().unwrap_or("[]").to_string()
                } else {
                    let field_strs: Vec<String> = fields
                        .iter()
                        .map(|(field_name, field_type)| {
                            if let Some(field_name) = field_name {
                                format!("{}: {}", field_name, format_type(quiver, field_type))
                            } else {
                                format_type(quiver, field_type)
                            }
                        })
                        .collect();

                    if let Some(type_name) = name {
                        format!("{}[{}]", type_name, field_strs.join(", "))
                    } else {
                        format!("[{}]", field_strs.join(", "))
                    }
                }
            } else {
                format!("Type{}", type_id.0)
            }
        }
        Type::Function(func_type) => {
            let param_str = if func_type.parameter.len() == 1 {
                format_type(quiver, &func_type.parameter[0])
            } else {
                let params: Vec<String> = func_type
                    .parameter
                    .iter()
                    .map(|t| format_type(quiver, t))
                    .collect();
                format!("({})", params.join(", "))
            };
            let result_str = if func_type.result.len() == 1 {
                format_type(quiver, &func_type.result[0])
            } else {
                let results: Vec<String> = func_type
                    .result
                    .iter()
                    .map(|t| format_type(quiver, t))
                    .collect();
                format!("({})", results.join(", "))
            };
            format!("#{} -> {}", param_str, result_str)
        }
    }
}

fn format_value(quiver: &Quiver, value: &Value) -> String {
    match value {
        Value::Function { function, .. } => {
            if let Some(func_def) = quiver.get_function(*function) {
                if let Some(func_type) = &func_def.function_type {
                    return format_type(quiver, &Type::Function(Box::new(func_type.clone())));
                }
            }
            "<function>".to_string()
        }
        Value::Integer(i) => i.to_string(),
        Value::Binary(_) => "<binary>".to_string(),
        Value::Tuple(type_id, elements) => {
            if *type_id == TypeId::NIL {
                return "[]".to_string();
            }
            if *type_id == TypeId::OK {
                return "Ok".to_string();
            }

            if let Some((name, fields)) = quiver.lookup_type(type_id) {
                if elements.is_empty() {
                    return name.as_deref().unwrap_or("[]").to_string();
                }

                let prefix = if let Some(type_name) = name {
                    format!("{}[", type_name)
                } else {
                    "[".to_string()
                };

                let mut result = prefix;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    if i < fields.len() {
                        if let Some(field_name) = &fields[i].0 {
                            result.push_str(&format!(
                                "{}: {}",
                                field_name,
                                format_value(quiver, element)
                            ));
                        } else {
                            result.push_str(&format_value(quiver, element));
                        }
                    } else {
                        result.push_str(&format_value(quiver, element));
                    }
                }
                result.push(']');
                result
            } else {
                let type_name = format!("Type{}", type_id.0);
                if elements.is_empty() {
                    return type_name;
                }

                let mut result = format!("{}[", type_name);
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&format_value(quiver, element));
                }
                result.push(']');
                result
            }
        }
    }
}
