use colored::Colorize;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::IsTerminal;
use std::rc::Rc;

use crate::{NativeRuntime, wait_for_callbacks};
use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::{
    Compiler, FileSystemModuleLoader, InMemoryModuleLoader, ModuleLoader, parse,
};
use quiver_core::bytecode::TypeId;
use quiver_core::error::Error as CoreError;
use quiver_core::format;
use quiver_core::types::Type;
use quiver_core::value::Value;
use quiver_core::{ProcessId, ProcessStatus};
use quiver_environment::Environment;

const HISTORY_FILE: &str = ".quiv_history";

/// CLI-specific REPL
pub struct ReplCli {
    editor: Editor<(), rustyline::history::DefaultHistory>,
    runtime: NativeRuntime,
    environment: Environment<crate::runtime::NativeCommandSender>,
    repl_process_id: ProcessId,
    type_aliases: HashMap<String, Type>,
    module_cache: ModuleCache,
    module_loader: Box<dyn ModuleLoader>,
    variables: HashMap<String, (Type, usize)>,
    result_type: Option<Type>,
}

impl ReplCli {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Self::with_modules(None)
    }

    pub fn with_modules(
        modules: Option<HashMap<String, String>>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let mut editor = Editor::<(), rustyline::history::DefaultHistory>::new()?;
        let _ = editor.load_history(HISTORY_FILE);

        let program = quiver_core::program::Program::new();
        let executor_count = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4);

        let mut runtime = NativeRuntime::new();
        for _ in 0..executor_count {
            runtime.start_executor(&program).map_err(|e| format!("{:?}", e))?;
        }
        let command_sender = runtime.command_sender();
        let mut environment = Environment::new(command_sender, program, executor_count);

        // Initialize REPL process
        let repl_process_id = Rc::new(RefCell::new(None));
        let repl_process_id_clone = repl_process_id.clone();
        environment.execute(vec![], true, move |result| {
            *repl_process_id_clone.borrow_mut() =
                Some(result.expect("Failed to spawn REPL process"));
        });

        wait_for_callbacks(&mut runtime, &mut environment).map_err(|e| format!("Runtime error: {:?}", e))?;

        let repl_process_id = repl_process_id
            .borrow()
            .expect("Execute callback not called");

        Ok(Self {
            editor,
            runtime,
            environment,
            repl_process_id,
            type_aliases: HashMap::new(),
            module_cache: ModuleCache::new(),
            module_loader: match modules {
                Some(modules) => Box::new(InMemoryModuleLoader::new(modules)),
                None => Box::new(FileSystemModuleLoader::new()),
            },
            variables: HashMap::new(),
            result_type: None,
        })
    }

    /// Wait for all pending callbacks to complete (native-specific)
    fn wait_for_callbacks(&mut self) -> Result<(), CoreError> {
        wait_for_callbacks(&mut self.runtime, &mut self.environment)
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
                println!("{}", "  \\t - List type aliases".bright_black());
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

    fn list_variables(&mut self) {
        let vars = self.get_variables(&self.variables.clone());
        if vars.is_empty() {
            println!("{}", "No variables defined".bright_black());
        } else {
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
                        self.format_value(&value, &[]),
                        self.format_type(var_type)
                    )
                    .bright_black()
                );
            }
        }
    }

    fn get_variables(
        &mut self,
        variables: &HashMap<String, (Type, usize)>,
    ) -> Vec<(String, Value)> {
        let mut var_list: Vec<(String, usize)> = variables
            .iter()
            .map(|(name, (_, index))| (name.clone(), *index))
            .collect();
        var_list.sort_by_key(|(_, index)| *index);

        let indices: Vec<usize> = var_list.iter().map(|(_, index)| *index).collect();

        let locals_result = Rc::new(RefCell::new(None));
        let locals_result_clone = locals_result.clone();
        self.environment
            .get_locals(self.repl_process_id, &indices, move |result| {
                *locals_result_clone.borrow_mut() = Some(result);
            });

        if self.wait_for_callbacks().is_err() {
            return vec![];
        }

        locals_result
            .borrow()
            .as_ref()
            .expect("GetLocals callback not called")
            .clone()
            .map(|values| {
                var_list
                    .into_iter()
                    .zip(values)
                    .map(|((name, _), value)| (name, value))
                    .collect()
            })
            .unwrap_or_default()
    }

    fn format_type(&self, type_def: &Type) -> String {
        format::format_type(self.environment.program(), type_def)
    }

    fn list_types(&self) {
        let types = self.environment.program().get_types();
        if types.is_empty() {
            println!("{}", "No types defined".bright_black());
        } else {
            let mut types: Vec<_> = types.iter().collect();
            types.sort_by_key(|(id, _)| id.0);
            println!("{}", "Types:".bright_black());
            for (type_id, _) in types {
                println!(
                    "{}",
                    format!(
                        "  {}: {}",
                        type_id.0,
                        self.format_type(&Type::Tuple(*type_id))
                    )
                    .bright_black()
                )
            }
        }
    }

    fn list_processes(&mut self) {
        match self.get_process_statuses() {
            Ok(statuses) => {
                if statuses.is_empty() {
                    println!("{}", "No processes".bright_black());
                } else {
                    let mut processes: Vec<_> = statuses.into_iter().collect();
                    processes.sort_by_key(|(id, _)| id.0);
                    println!("{}", "Processes:".bright_black());
                    for (id, status) in processes {
                        let status_str = match status {
                            ProcessStatus::Active => "active",
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

    fn get_process_statuses(&mut self) -> Result<HashMap<ProcessId, ProcessStatus>, CoreError> {
        let executor_count = self.environment.executor_count();
        let mut all_statuses = HashMap::new();

        for executor_id in 0..executor_count {
            let result = Rc::new(RefCell::new(None));
            let result_clone = result.clone();
            self.environment
                .get_process_statuses(executor_id, move |r| {
                    *result_clone.borrow_mut() = Some(r);
                });

            self.wait_for_callbacks()?;

            let statuses = result
                .borrow()
                .as_ref()
                .expect("GetProcessStatuses callback not called")
                .clone()?;

            all_statuses.extend(statuses);
        }

        Ok(all_statuses)
    }

    fn inspect_process(&mut self, id: ProcessId) {
        match self.get_process_info(id) {
            Ok(Some(info)) => {
                let status_str = match info.status {
                    ProcessStatus::Active => "active",
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
                        format!("  Result: {}", self.format_value(result, &[])).bright_black()
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

    fn get_process_info(
        &mut self,
        id: ProcessId,
    ) -> Result<Option<quiver_core::process::ProcessInfo>, CoreError> {
        let result = Rc::new(RefCell::new(None));
        let result_clone = result.clone();
        self.environment.get_process_info(id, move |r| {
            *result_clone.borrow_mut() = Some(r);
        });

        self.wait_for_callbacks()?;

        result
            .borrow()
            .as_ref()
            .expect("GetProcessInfo callback not called")
            .clone()
    }

    fn evaluate(&mut self, line: &str) {
        let module_path = std::env::current_dir().ok();
        let parameter_type = self.result_type.clone();

        match self.evaluate_impl(line, module_path, parameter_type) {
            Ok((result, result_type, new_variables, heap_data)) => {
                self.variables = new_variables;

                if let Some(value) = &result {
                    if !matches!(value, Value::Tuple(type_id, _) if *type_id == TypeId::NIL || *type_id == TypeId::OK)
                    {
                        println!(
                            "{} {}",
                            self.format_value(value, &heap_data),
                            format!("({})", self.format_type(&result_type)).bright_black()
                        );
                    }
                }

                self.result_type = Some(result_type);
            }
            Err(error) => eprintln!("{}", error.to_string().red()),
        }
    }

    fn evaluate_impl(
        &mut self,
        source: &str,
        module_path: Option<std::path::PathBuf>,
        parameter_type: Option<Type>,
    ) -> Result<
        (
            Option<Value>,
            Type,
            HashMap<String, (Type, usize)>,
            Vec<Vec<u8>>,
        ),
        String,
    > {
        let ast_program = parse(source).map_err(|e| format!("Parse error: {:?}", e))?;

        let old_program = self.environment.program().clone();
        let parameter_type = parameter_type.unwrap_or_else(Type::nil);

        let (instructions, result_type, variables, new_program, new_type_aliases, new_module_cache) =
            Compiler::compile(
                ast_program,
                self.type_aliases.clone(),
                self.module_cache.clone(),
                self.module_loader.as_ref(),
                &old_program,
                module_path,
                Some(&self.variables),
                parameter_type,
            )
            .map_err(|e| format!("Compile error: {:?}", e))?;

        self.environment.update_program(new_program);
        self.type_aliases = new_type_aliases;
        self.module_cache = new_module_cache;

        let wake_result = Rc::new(RefCell::new(None));
        let wake_result_clone = wake_result.clone();
        self.environment
            .wake(self.repl_process_id, instructions, move |result| {
                *wake_result_clone.borrow_mut() = Some(result);
            });

        self.wait_for_callbacks()
            .map_err(|e| format!("Runtime error: {:?}", e))?;

        wake_result
            .borrow()
            .as_ref()
            .expect("Wake callback not called")
            .clone()
            .map_err(|e| format!("Runtime error: {:?}", e))?;

        let value_result = Rc::new(RefCell::new(None));
        let value_result_clone = value_result.clone();
        self.environment
            .get_result(self.repl_process_id, move |result| {
                *value_result_clone.borrow_mut() = Some(result);
            });

        self.wait_for_callbacks()
            .map_err(|e| format!("Runtime error: {:?}", e))?;

        let (value, heap_data) = value_result
            .borrow()
            .as_ref()
            .expect("GetResult callback not called")
            .clone()
            .map_err(|e| format!("Runtime error: {:?}", e))?;

        if !variables.is_empty() {
            let mut sorted_vars: Vec<_> = variables.iter().collect();
            sorted_vars.sort_by_key(|(_, (_, idx))| *idx);
            let referenced_indices: Vec<usize> =
                sorted_vars.iter().map(|(_, (_, idx))| *idx).collect();

            let compact_result = Rc::new(RefCell::new(None));
            let compact_result_clone = compact_result.clone();
            self.environment.compact_locals(
                self.repl_process_id,
                &referenced_indices,
                move |result| {
                    *compact_result_clone.borrow_mut() = Some(result);
                },
            );

            self.wait_for_callbacks()
                .map_err(|e| format!("Runtime error: {:?}", e))?;

            compact_result
                .borrow()
                .as_ref()
                .expect("CompactLocals callback not called")
                .clone()
                .map_err(|e| format!("Runtime error: {:?}", e))?;

            let mut compacted_variables = HashMap::new();
            for (new_idx, (name, (typ, _))) in sorted_vars.into_iter().enumerate() {
                compacted_variables.insert(name.clone(), (typ.clone(), new_idx));
            }

            Ok((Some(value), result_type, compacted_variables, heap_data))
        } else {
            Ok((Some(value), result_type, variables, heap_data))
        }
    }

    /// Evaluate for testing purposes - converts errors to a testable format
    pub fn evaluate_for_test(
        &mut self,
        source: &str,
    ) -> Result<Option<(Value, Vec<Vec<u8>>)>, TestError> {
        use quiver_compiler::parse;

        // Parse
        let ast_program = parse(source).map_err(|e| TestError::ParseError(Box::new(e)))?;

        let old_program = self.environment.program().clone();
        let parameter_type = Type::nil();

        // Compile with current variable state
        let (
            instructions,
            _result_type,
            variables,
            new_program,
            new_type_aliases,
            new_module_cache,
        ) = Compiler::compile(
            ast_program,
            self.type_aliases.clone(),
            self.module_cache.clone(),
            self.module_loader.as_ref(),
            &old_program,
            None,
            Some(&self.variables),
            parameter_type,
        )
        .map_err(TestError::CompileError)?;

        self.environment.update_program(new_program);
        self.type_aliases = new_type_aliases;
        self.module_cache = new_module_cache;

        // Execute
        let wake_result = Rc::new(RefCell::new(None));
        let wake_result_clone = wake_result.clone();
        self.environment
            .wake(self.repl_process_id, instructions, move |result| {
                *wake_result_clone.borrow_mut() = Some(result);
            });

        self.wait_for_callbacks().map_err(TestError::RuntimeError)?;

        wake_result
            .borrow()
            .as_ref()
            .expect("Wake callback not called")
            .clone()
            .map_err(TestError::RuntimeError)?;

        // Get result
        let value_result = Rc::new(RefCell::new(None));
        let value_result_clone = value_result.clone();
        self.environment
            .get_result(self.repl_process_id, move |result| {
                *value_result_clone.borrow_mut() = Some(result);
            });

        self.wait_for_callbacks().map_err(TestError::RuntimeError)?;

        let (value, heap_data) = value_result
            .borrow()
            .as_ref()
            .expect("GetResult callback not called")
            .clone()
            .map_err(TestError::RuntimeError)?;

        // Handle variable compaction like evaluate_impl does
        if !variables.is_empty() {
            let mut sorted_vars: Vec<_> = variables.iter().collect();
            sorted_vars.sort_by_key(|(_, (_, idx))| *idx);
            let referenced_indices: Vec<usize> =
                sorted_vars.iter().map(|(_, (_, idx))| *idx).collect();

            let compact_result = Rc::new(RefCell::new(None));
            let compact_result_clone = compact_result.clone();
            self.environment.compact_locals(
                self.repl_process_id,
                &referenced_indices,
                move |result| {
                    *compact_result_clone.borrow_mut() = Some(result);
                },
            );

            self.wait_for_callbacks().map_err(TestError::RuntimeError)?;

            compact_result
                .borrow()
                .as_ref()
                .expect("CompactLocals callback not called")
                .clone()
                .map_err(TestError::RuntimeError)?;

            let mut compacted_variables = HashMap::new();
            for (new_idx, (name, (typ, _))) in sorted_vars.into_iter().enumerate() {
                compacted_variables.insert(name.clone(), (typ.clone(), new_idx));
            }

            self.variables = compacted_variables;
        } else {
            self.variables = variables;
        }

        Ok(Some((value, heap_data)))
    }

    pub fn format_value(&self, value: &Value, heap: &[Vec<u8>]) -> String {
        let constants = self.environment.program().get_constants();
        format::format_value(value, heap, constants, self.environment.program())
    }
}

#[derive(Debug)]
pub enum TestError {
    ParseError(Box<quiver_compiler::parser::Error>),
    CompileError(quiver_compiler::compiler::Error),
    RuntimeError(quiver_core::error::Error),
    Other(String),
}

impl std::fmt::Display for TestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestError::ParseError(e) => write!(f, "Parse error: {:?}", e),
            TestError::CompileError(e) => write!(f, "Compile error: {:?}", e),
            TestError::RuntimeError(e) => write!(f, "Runtime error: {:?}", e),
            TestError::Other(s) => write!(f, "{}", s),
        }
    }
}
