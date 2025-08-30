use clap::{CommandFactory, Parser, Subcommand};
use quiver::Quiver;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::io::IsTerminal;

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
    let mut quiver = Quiver::new();

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
                        quiver = Quiver::new();
                    }

                    "\\v" => {
                        // TODO
                        continue;
                    }

                    "\\t" => {
                        // TODO
                        continue;
                    }

                    _ => {
                        match quiver.evaluate(line) {
                            Ok(Some(value)) => {
                                // TODO: show result
                            }
                            Ok(None) => {}
                            // TODO: show error
                            Err(error) => eprintln!("Error"),
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
                // TODO: show error
                eprintln!("Error");
                break;
            }
        }
    }

    rl.save_history(".quiver_history")?;

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
    // TODO
    Ok(())
}

fn run_command(
    input: Option<String>,
    eval: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    // TODO
    Ok(())
}

fn inspect_command(input: Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    // TODO
    Ok(())
}
