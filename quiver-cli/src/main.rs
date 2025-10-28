use clap::{Parser, Subcommand};
use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::{Compiler, FileSystemModuleLoader, parse};
use quiver_core::bytecode;
use quiver_core::format;
use quiver_core::program::Program;
use quiver_core::types::{NIL, Type};
use quiver_core::value::Value;
use std::collections::HashMap;
use std::fs;
use std::io::{self, Read};

mod repl_cli;
use repl_cli::ReplCli;

#[derive(Parser)]
#[command(name = "quiv", version, about = "Quiver CLI")]
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
            eval,
        }) => compile_command(input, output, eval)?,
        Some(Commands::Run { input, eval, quiet }) => run_command(input, eval, quiet)?,
        Some(Commands::Inspect { input }) => inspect_command(input)?,
        None => run_repl()?,
    }

    Ok(())
}

fn run_repl() -> Result<(), Box<dyn std::error::Error>> {
    let repl = ReplCli::new()?;
    repl.run()?;
    Ok(())
}

fn compile_command(
    input: Option<String>,
    output: Option<String>,
    eval: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = if let Some(code) = eval {
        code
    } else if let Some(path) = input {
        fs::read_to_string(&path)?
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        buffer
    };

    // Compile source to bytecode
    let module_loader = FileSystemModuleLoader::new();
    let module_path = std::env::current_dir().ok();

    let ast = parse(&source).map_err(|e| format!("Parse error: {:?}", e))?;
    let result = Compiler::compile(
        ast,
        &HashMap::new(), // No existing bindings
        ModuleCache::new(),
        &module_loader,
        vec![],
        module_path,
        Type::nil(),
    )
    .map_err(|e| format!("Compile error: {:?}", e))?;

    let instructions = result.instructions;
    let result_type = result.result_type;
    let mut program = result.program;

    // Execute the instructions synchronously to get the function value
    let (result, executor) =
        quiver_core::execute_instructions_sync(&program, instructions, result_type)
            .map_err(|e| format!("Execution error: {:?}", e))?;

    // Extract the entry function from the result
    let entry = match result {
        Value::Function(func_index, captures) => {
            if !captures.is_empty() {
                // Inject captures into the program to create a new function
                Some(program.inject_function_captures(func_index, captures, &executor))
            } else {
                Some(func_index)
            }
        }
        _ => None, // Program didn't evaluate to a function
    };

    let bytecode = program.to_bytecode(entry);
    let json = serde_json::to_string_pretty(&bytecode)?;

    if let Some(output_path) = output {
        fs::write(output_path, json)?;
    } else {
        println!("{}", json);
    }

    Ok(())
}

fn run_command(
    input: Option<String>,
    eval: Option<String>,
    quiet: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(code) = eval {
        compile_execute(&code, quiet)?;
    } else if let Some(path) = input {
        let content = fs::read_to_string(&path)?;

        if path.ends_with(".qv") {
            compile_execute(&content, quiet)?;
        } else if path.ends_with(".qx") {
            execute_bytecode(&content, quiet)?;
        } else {
            eprintln!(
                "Error: Unsupported file extension - expected .qv for source or .qx for bytecode."
            );
            std::process::exit(1);
        }
    } else {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;

        // Try to parse as bytecode first
        if buffer.trim_start().starts_with('{') {
            match serde_json::from_str::<bytecode::Bytecode>(&buffer) {
                Ok(_) => execute_bytecode(&buffer, quiet)?,
                Err(_) => compile_execute(&buffer, quiet)?,
            }
        } else {
            compile_execute(&buffer, quiet)?;
        }
    }

    Ok(())
}

fn compile_execute(source: &str, quiet: bool) -> Result<(), Box<dyn std::error::Error>> {
    let module_loader = FileSystemModuleLoader::new();
    let module_path = std::env::current_dir().ok();

    let ast = parse(source).map_err(|e| format!("Parse error: {:?}", e))?;
    let compilation_result = Compiler::compile(
        ast,
        &HashMap::new(), // No existing bindings
        ModuleCache::new(),
        &module_loader,
        vec![],
        module_path,
        Type::nil(),
    )
    .map_err(|e| format!("Compile error: {:?}", e))?;

    let instructions = compilation_result.instructions;
    let result_type = compilation_result.result_type;
    let mut program = compilation_result.program;

    // Execute the instructions synchronously to get the function value
    let (result, executor) =
        quiver_core::execute_instructions_sync(&program, instructions, result_type)
            .map_err(|e| format!("Execution error: {:?}", e))?;

    // Extract the entry function from the result
    let entry = match result {
        Value::Function(func_index, captures) => {
            if !captures.is_empty() {
                // Inject captures into the program to create a new function
                Some(program.inject_function_captures(func_index, captures, &executor))
            } else {
                Some(func_index)
            }
        }
        _ => None, // Program didn't evaluate to a function
    };

    let bytecode = program.to_bytecode(entry);

    if bytecode.entry.is_none() {
        eprintln!("Error: Program is not executable. Must evaluate to a function.");
        std::process::exit(1);
    }

    let json = serde_json::to_string(&bytecode)?;
    execute_bytecode(&json, quiet)
}

fn execute_bytecode(bytecode_json: &str, quiet: bool) -> Result<(), Box<dyn std::error::Error>> {
    let bytecode_data: bytecode::Bytecode = serde_json::from_str(bytecode_json)?;
    let entry = bytecode_data.entry;
    let program = Program::from_bytecode(bytecode_data);

    if let Some(entry_idx) = entry {
        // Build instructions to call the entry function with NIL
        let instructions = vec![
            bytecode::Instruction::Tuple(NIL),
            bytecode::Instruction::Function(entry_idx),
            bytecode::Instruction::Call,
        ];

        // Get the result type from the entry function
        let result_type = program
            .get_function(entry_idx)
            .map(|f| f.function_type.result.clone())
            .unwrap_or_else(|| Type::Union(vec![])); // Top type as fallback

        let (value, _executor) =
            quiver_core::execute_instructions_sync(&program, instructions, result_type)
                .map_err(|e| format!("Execution error: {:?}", e))?;

        // Check if result is NIL tuple (exit with error)
        if value.is_nil() {
            std::process::exit(1);
        }

        // Print result unless quiet or OK/NIL
        if !quiet && !value.is_ok() && !value.is_nil() {
            println!("{}", format::format_value(&value, &[], &program));
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
    let program = Program::from_bytecode(bytecode_data);

    let types_vec = program.get_tuple_types();
    if !types_vec.is_empty() {
        println!("\nTypes:");
        for (index, _type_info) in types_vec.iter().enumerate() {
            println!(
                "  {}: {}",
                index,
                format::format_type(&program, &Type::Tuple(index))
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
