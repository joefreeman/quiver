use clap::{CommandFactory, Parser, Subcommand};
use quiver::compile;
use quiver::repl;
use quiver_core::bytecode;
use std::cell::RefCell;
use std::fs;
use std::io::{self, Read};
use std::rc::Rc;

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
    let repl = repl::ReplCli::new()?;
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

    let mut bytecode = compile(&source, module_path, None)?;

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
    environment: &quiver::Environment<quiver::NativeRuntime>,
    result: Result<Option<(quiver_core::value::Value, Vec<Vec<u8>>)>, quiver::Error>,
    quiet: bool,
) {
    use quiver_core::value::Value;

    match result {
        Ok(Some((value, heap_data))) => {
            // Check if result is NIL tuple (exit with error)
            if matches!(value, Value::Tuple(type_id, _) if type_id == bytecode::TypeId::NIL) {
                std::process::exit(1);
            }

            if !quiet
                && !matches!(value, Value::Tuple(type_id, _) if type_id == bytecode::TypeId::OK || type_id == bytecode::TypeId::NIL)
            {
                let constants = environment.program().get_constants();
                println!(
                    "{}",
                    quiver_core::format::format_value(
                        &value,
                        &heap_data,
                        constants,
                        environment.program()
                    )
                );
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
            let bytecode_data: bytecode::Bytecode = serde_json::from_str(&content)?;
            let entry = bytecode_data.entry;
            let program = quiver_core::program::Program::from_bytecode(bytecode_data);
            // Use single executor for bytecode execution
            let runtime = quiver::NativeRuntime::new();
            let mut environment = quiver::Environment::new(runtime, program, 1);

            let result = if let Some(entry) = entry {
                execute_function(&mut environment, entry).map_err(quiver::Error::RuntimeError)
            } else {
                Ok(None)
            };
            handle_result(&environment, result, quiet);
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
            match serde_json::from_str::<bytecode::Bytecode>(&buffer) {
                Ok(bytecode_data) => {
                    let entry = bytecode_data.entry;
                    let program = quiver_core::program::Program::from_bytecode(bytecode_data);
                    // Use single executor for bytecode execution
                    let runtime = quiver::NativeRuntime::new();
                    let mut environment = quiver::Environment::new(runtime, program, 1);

                    let result = if let Some(entry) = entry {
                        execute_function(&mut environment, entry)
                            .map_err(quiver::Error::RuntimeError)
                    } else {
                        Ok(None)
                    };
                    handle_result(&environment, result, quiet);
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
    let bytecode_data = compile(source, module_path, None)?;

    let entry = bytecode_data
        .entry
        .ok_or("Program is not executable. Must evaluate to a function.")?;

    let program = quiver_core::program::Program::from_bytecode(bytecode_data);
    // Use single executor for bytecode execution
    let runtime = quiver::NativeRuntime::new();
    let mut environment = quiver::Environment::new(runtime, program, 1);

    let result = execute_function(&mut environment, entry).map_err(quiver::Error::RuntimeError);
    handle_result(&environment, result, quiet);

    Ok(())
}

fn execute_function(
    environment: &mut quiver::Environment<quiver::NativeRuntime>,
    entry: usize,
) -> Result<Option<(quiver_core::value::Value, Vec<Vec<u8>>)>, quiver_core::error::Error> {
    use quiver_core::error::Error;

    let function = environment
        .program()
        .get_function(entry)
        .ok_or(Error::FunctionUndefined(entry))?
        .clone();

    // Execute the function in a new non-persistent process
    let process_id_result = Rc::new(RefCell::new(None));
    let process_id_result_clone = process_id_result.clone();
    environment.execute(function.instructions, false, move |result| {
        *process_id_result_clone.borrow_mut() = Some(result);
    });

    // Wait for callback
    quiver::wait_for_callbacks_native(environment)?;

    let process_id = process_id_result
        .borrow()
        .as_ref()
        .expect("Execute callback not called")
        .clone()?;

    // Get the result with heap data
    let value_result = Rc::new(RefCell::new(None));
    let value_result_clone = value_result.clone();
    environment.get_result(process_id, move |result| {
        *value_result_clone.borrow_mut() = Some(result);
    });

    // Wait for callback
    quiver::wait_for_callbacks_native(environment)?;

    let (value, heap_data) = value_result
        .borrow()
        .as_ref()
        .expect("GetResult callback not called")
        .clone()?;

    Ok(Some((value, heap_data)))
}

fn inspect_command(input: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    let content = if let Some(path) = input {
        fs::read_to_string(&path)?
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    };

    let bytecode_data: bytecode::Bytecode = serde_json::from_str(&content)?;

    println!("Constants:");
    for (i, constant) in bytecode_data.constants.iter().enumerate() {
        let formatted = match constant {
            bytecode::Constant::Integer(n) => n.to_string(),
            bytecode::Constant::Binary(bytes) => format_binary(bytes),
        };
        println!("  {}: {}", i, formatted);
    }

    let entry = bytecode_data.entry;

    // Create program from bytecode
    use quiver_core::program::Program;
    use quiver_core::types::Type;
    let program = Program::from_bytecode(bytecode_data);

    let types_map = program.get_types();
    if !types_map.is_empty() {
        println!("\nTypes:");
        let mut types: Vec<_> = types_map.iter().collect();
        types.sort_by_key(|(id, _)| id.0);
        for (type_id, _type_info) in types {
            println!(
                "  {}: {}",
                type_id.0,
                quiver_core::format::format_type(&program, &Type::Tuple(*type_id))
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
