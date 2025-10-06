use clap::{CommandFactory, Parser, Subcommand};
use quiver::bytecode::TypeId;
use quiver::repl::Repl;
use quiver::types::Type;
use std::fs;
use std::io::{self, Read};

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

        #[arg(short, long)]
        quiet: bool,
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
        Some(Commands::Run { input, eval, quiet }) => run_command(input, eval, quiet)?,
        Some(Commands::Inspect { input }) => inspect_command(input)?,
        None => Cli::command().print_help().unwrap(),
    }

    Ok(())
}

fn run_repl() -> Result<(), Box<dyn std::error::Error>> {
    let repl = Repl::new()?;
    repl.run()?;
    Ok(())
}

fn compile_command(
    input: Option<String>,
    output: Option<String>,
    debug: bool,
    eval: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
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

    let mut bytecode = quiver::compile(&source, module_path, None)?;

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

fn handle_result(
    vm: &quiver::vm::VM,
    result: Result<Option<quiver::vm::Value>, quiver::Error>,
    quiet: bool,
) {
    use quiver::vm::Value;

    match result {
        Ok(Some(value)) => {
            // Check if result is NIL tuple (exit with error)
            if matches!(value, Value::Tuple(type_id, _) if type_id == TypeId::NIL) {
                std::process::exit(1);
            }

            if !quiet
                && !matches!(value, Value::Tuple(type_id, _) if type_id == TypeId::OK || type_id == TypeId::NIL)
            {
                println!("{}", quiver::format::format_value(&vm.scheduler(), &value));
            }
        }
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
    quiet: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(code) = eval {
        let module_path = Some(std::env::current_dir()?);
        compile_execute(&code, module_path, quiet)?;
    } else if let Some(path) = input {
        let content = fs::read_to_string(&path)?;

        if path.ends_with(".qv") {
            let script_path = std::path::Path::new(&path);
            let module_path = script_path.parent().map(|p| p.to_path_buf());
            compile_execute(&content, module_path, quiet)?;
        } else if path.ends_with(".qx") {
            let bytecode: quiver::bytecode::Bytecode = serde_json::from_str(&content)?;
            let entry = bytecode.entry;
            let program = quiver::program::Program::from_bytecode(bytecode);
            let vm = quiver::vm::VM::new(program);

            let result = if let Some(entry) = entry {
                vm.execute_function(entry)
                    .map_err(quiver::Error::RuntimeError)
            } else {
                Ok(None)
            };
            handle_result(&vm, result, quiet);
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
                Ok(bytecode) => {
                    let entry = bytecode.entry;
                    let program = quiver::program::Program::from_bytecode(bytecode);
                    let vm = quiver::vm::VM::new(program);

                    let result = if let Some(entry) = entry {
                        vm.execute_function(entry)
                            .map_err(quiver::Error::RuntimeError)
                    } else {
                        Ok(None)
                    };
                    handle_result(&vm, result, quiet);
                }
                Err(_) => {
                    let module_path = Some(std::env::current_dir()?);
                    compile_execute(&buffer, module_path, quiet)?;
                }
            }
        } else {
            let module_path = Some(std::env::current_dir()?);
            compile_execute(&buffer, module_path, quiet)?;
        }
    }

    Ok(())
}

fn compile_execute(
    source: &str,
    module_path: Option<std::path::PathBuf>,
    quiet: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let bytecode = quiver::compile(source, module_path, None)?;

    let entry = bytecode
        .entry
        .ok_or("Program is not executable. Must evaluate to a function.")?;

    let program = quiver::program::Program::from_bytecode(bytecode);
    let vm = quiver::vm::VM::new(program);

    let result = vm
        .execute_function(entry)
        .map_err(quiver::Error::RuntimeError);
    handle_result(&vm, result, quiet);

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
        format!("'{}…'", hex)
    }
}
