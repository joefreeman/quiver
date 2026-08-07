//! `quiv test` — run the Quiver code embedded in a Markdown document.
//!
//! The document is executed, not rendered. Its expected values are already written into it as
//! `//=> P` assertions, which the compiler checks itself, so running produces exactly one thing
//! the file does not already say: whether it still holds.
//!
//! A chapter is the unit of scope and of reporting. Its steps are fed to a [`Repl`] one at a
//! time rather than compiled as one program, which is what a reader does and what the language
//! requires: a step that evaluates to nil ends its *sequence*, so a concatenated chapter would
//! stop at the first `//=> []` and silently skip every assertion below it.

use colored::Colorize;
use quiver_cli::spawn_worker;
use quiver_compiler::PackageResolver;
use quiver_environment::{Environment, Repl, ReplError, RequestResult, WorkerHandle};
use quiver_io::NativeEffect;
use quiver_markdown::{Block, Chapter, Document, Mode};
use std::collections::HashMap;
use std::io::{IsTerminal, Write};
use std::path::Path;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

/// How a step failed. The two are reported apart because they mean different things: a failed
/// assertion is the document disagreeing with the language, while anything else is the document
/// being broken.
enum Cause {
    /// A `//=> P` assertion did not match. Carries the pattern, and the value the step actually
    /// produced when re-run without the check (absent if that re-run failed too).
    Assertion {
        pattern: String,
        actual: Option<String>,
    },
    Error(String),
}

struct Failure {
    line: usize,
    step: String,
    cause: Cause,
}

struct ChapterReport {
    title: String,
    passed: usize,
    total: usize,
    runnable_blocks: usize,
    skipped_blocks: usize,
    failures: Vec<Failure>,
    /// Assertions below the point a runtime error abandoned the chapter — never run, and so
    /// neither passed nor failed. Reported so a single failure does not read as costing one
    /// assertion when it in fact hid several.
    unreached: usize,
}

impl ChapterReport {
    fn ok(&self) -> bool {
        self.failures.is_empty()
    }
}

pub fn test_command(paths: Vec<String>) -> Result<(), Box<dyn std::error::Error>> {
    if paths.is_empty() {
        return Err("No documents given.".into());
    }

    let mut runner = Runner::new();
    let mut failed = false;
    for (index, path) in paths.iter().enumerate() {
        if index > 0 {
            println!();
        }
        failed |= !runner.run_document(Path::new(path))?;
    }

    if failed {
        std::process::exit(1);
    }
    Ok(())
}

struct Runner {
    environment: Arc<Mutex<Environment<NativeEffect>>>,
    shutdown: Arc<AtomicBool>,
    /// Kept so shutdown can wake the stepping thread. An idle thread parks in
    /// `WakeSignal::wait`, which blocks on a channel receive — so setting the flag alone
    /// never returns, and joining would hang exactly when the run has *passed*.
    waker: quiver_cli::native_transport::Waker,
    /// Signalled by the stepping thread after every productive step, so request waits
    /// sleep instead of polling.
    progress: Arc<quiver_cli::native_transport::Progress>,
    stepping: Option<JoinHandle<()>>,
    artifact_store: Rc<quiver_compiler::ArtifactStore>,
}

impl Runner {
    fn new() -> Self {
        let builtins = crate::build_builtin_registry();
        let (waker, wake) = quiver_cli::native_transport::wake_channel();

        let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
        for i in 0..4 {
            workers.push(Box::new(spawn_worker(
                quiver_cli::native_transport::SystemClock,
                builtins.clone(),
                false,
                i as u16,
                waker.clone(),
            )));
        }

        let mut environment = Environment::<NativeEffect>::new(workers);
        environment.set_runtime_declarations(builtins.runtime_declarations().clone());
        if let Some(backend) = crate::create_effect_backend() {
            environment.set_effect_backend(backend);
        }

        let environment = Arc::new(Mutex::new(environment));
        let shutdown = Arc::new(AtomicBool::new(false));

        let progress = Arc::new(quiver_cli::native_transport::Progress::new());
        let env_clone = Arc::clone(&environment);
        let shutdown_clone = Arc::clone(&shutdown);
        let progress_clone = Arc::clone(&progress);
        let stepping = Some(thread::spawn(move || {
            while !shutdown_clone.load(Ordering::Relaxed) {
                let did_work = env_clone
                    .lock()
                    .map(|mut env| env.step().unwrap_or(false))
                    .unwrap_or(false);
                if did_work {
                    progress_clone.notify();
                } else {
                    let in_flight = env_clone
                        .lock()
                        .map(|env| env.io_in_flight())
                        .unwrap_or(true);
                    wake.wait(in_flight);
                }
            }
        }));

        // A chapter is a fresh session, so a document re-imports the standard library once per
        // chapter. The artifact store is what keeps that from being a recompile each time.
        let artifact_store = Rc::new(quiver_compiler::ArtifactStore::cache());

        Runner {
            environment,
            shutdown,
            waker,
            progress,
            stepping,
            artifact_store,
        }
    }

    /// Run one document, printing chapter-by-chapter progress. Answers whether it passed.
    fn run_document(&mut self, path: &Path) -> Result<bool, Box<dyn std::error::Error>> {
        let text = std::fs::read_to_string(path)?;
        let document = Document::parse(&text);
        let name = path.display().to_string();
        println!("{}", name.bold());

        // A chapter's verdict is only known once it has run, but a slow one (the first, which
        // imports the standard library; any that spawns processes) should show where it is
        // rather than look hung. On a terminal, name it under a placeholder marker and rewrite
        // the line in place; anywhere else, emit the finished line alone.
        let interactive = std::io::stdout().is_terminal();
        let mut reports = Vec::new();
        for chapter in &document.chapters {
            if interactive {
                print!("{}  {}", "·".bright_black(), title_of(chapter));
                let _ = std::io::stdout().flush();
            }

            let report = self.run_chapter(chapter, path);

            if interactive {
                print!("\r\x1b[2K");
            }
            println!("{}", summarise(&report));
            reports.push(report);
        }

        for report in reports.iter().filter(|r| !r.ok()) {
            for failure in &report.failures {
                println!();
                print_failure(&name, &report.title, failure);
            }
            if report.unreached > 0 {
                println!(
                    "{}",
                    format!(
                        "    {} later assertion{} in this chapter did not run",
                        report.unreached,
                        if report.unreached == 1 { "" } else { "s" }
                    )
                    .bright_black()
                );
            }
        }

        println!();
        println!("{}", total_line(&reports));
        Ok(reports.iter().all(ChapterReport::ok))
    }

    fn run_chapter(&mut self, chapter: &Chapter, path: &Path) -> ChapterReport {
        let mut report = ChapterReport {
            title: title_of(chapter),
            passed: 0,
            total: 0,
            runnable_blocks: chapter.runnable().count(),
            skipped_blocks: chapter.skipped(),
            failures: Vec::new(),
            unreached: 0,
        };

        let mut session = match self.session(path) {
            Ok(session) => session,
            Err(error) => {
                report.failures.push(Failure {
                    line: 0,
                    step: String::new(),
                    cause: Cause::Error(format!("could not start a session: {error}")),
                });
                return report;
            }
        };
        // The steps run so far, kept so a failed assertion can be replayed without its check to
        // recover the value it saw.
        let mut history: Vec<String> = Vec::new();

        for block in chapter.runnable() {
            if block.mode == Mode::Program {
                self.run_program(block, path, &mut report);
                continue;
            }

            let steps = match block.steps() {
                Ok(steps) => steps,
                Err(error) => {
                    report.failures.push(Failure {
                        line: block.line,
                        step: String::new(),
                        cause: Cause::Error(format!("{error}")),
                    });
                    continue;
                }
            };

            for step in &steps {
                report.total += step.assertions;
                let source = step.positioned();
                match self.evaluate(&mut session, &source) {
                    Ok(()) => {
                        report.passed += step.assertions;
                        history.push(source);
                    }
                    // A parse or compile error leaves the session intact — the REPL commits
                    // state only on a successful compile — so the chapter carries on and the
                    // rest of it is still checked.
                    Err(Fault::Static(message)) => report.failures.push(Failure {
                        line: step.line,
                        step: step.source.clone(),
                        cause: Cause::Error(message),
                    }),
                    // A runtime error killed the session's process. Report it, count what it
                    // hid, and abandon the chapter.
                    Err(Fault::Runtime(message)) => {
                        report.failures.push(Failure {
                            line: step.line,
                            step: step.source.clone(),
                            cause: self.explain(&message, &history, &source, path),
                        });
                        report.unreached = remaining_assertions(chapter, step.line);
                        report.total += report.unreached;
                        return report;
                    }
                }
            }
        }

        report
    }

    /// Run a ```` ```quiver program ```` block: compiled and executed whole, as a file, because
    /// its last step is the entry-point function `quiv run` would call rather than a value to
    /// evaluate in a session. Its assertions sit inside that function and check themselves.
    fn run_program(&mut self, block: &Block, path: &Path, report: &mut ChapterReport) {
        let source = format!("{}{}", "\n".repeat(block.line - 1), block.source);
        report.total += count_assertions(block);

        let result = (|| -> Result<(), String> {
            let builtins = crate::build_builtin_registry();
            let ast = quiver_compiler::parse(&source).map_err(|e| format!("{e}"))?;
            let (program, entry) = crate::compile_entry(
                ast,
                &resolver_for(path),
                &builtins,
                quiver_compiler::compiler::CompileOptions {
                    debug: true,
                    source_name: source_name(path),
                    ..Default::default()
                },
            )
            .map_err(|e| format!("{e}"))?;
            let bytecode = program.to_bytecode_optimized(entry);
            let mut env = self.environment.lock().unwrap();
            let pid = env
                .start_process(Some(bytecode))
                .map_err(|e| format!("{e:?}"))?;
            let request = env
                .request_result(pid, None)
                .map_err(|e| format!("{e:?}"))?;
            drop(env);
            match self.wait(request) {
                RequestResult::Result(Ok(_), _) => Ok(()),
                RequestResult::Result(Err(error), _) => Err(error.crash_message()),
                _ => Err("unexpected result".to_string()),
            }
        })();

        match result {
            Ok(()) => report.passed += count_assertions(block),
            Err(message) => report.failures.push(Failure {
                line: block.line,
                step: String::new(),
                cause: match assertion_pattern(&message) {
                    Some(pattern) => Cause::Assertion {
                        pattern,
                        actual: None,
                    },
                    None => Cause::Error(message),
                },
            }),
        }
    }

    /// Classify a runtime failure, and — when it is an assertion — recover the value the check
    /// saw by replaying the chapter and re-running the step without it.
    fn explain(&mut self, message: &str, history: &[String], source: &str, path: &Path) -> Cause {
        let Some(pattern) = assertion_pattern(message) else {
            return Cause::Error(message.to_string());
        };
        Cause::Assertion {
            pattern,
            actual: self.replay(history, source, path),
        }
    }

    fn replay(&mut self, history: &[String], source: &str, path: &Path) -> Option<String> {
        let stripped = quiver_markdown::without_assertions(source).ok()?;
        let mut session = self.session(path).ok()?;
        for step in history {
            self.evaluate(&mut session, step).ok()?;
        }
        let value = self.evaluate_value(&mut session, &stripped).ok()??;
        Some(self.environment.lock().unwrap().format_value(&value))
    }

    fn session(&mut self, path: &Path) -> Result<Repl<NativeEffect>, ReplError> {
        let builtins = crate::build_builtin_registry();
        let mut env = self.environment.lock().unwrap();
        let mut repl = Repl::new(&mut env, Box::new(resolver_for(path)), builtins)?;
        // Assertions are checked in debug builds only, which is the whole point of the run.
        repl.set_compile_options(quiver_compiler::compiler::CompileOptions {
            debug: true,
            source_name: source_name(path),
            ..Default::default()
        });
        repl.set_artifact_store(self.artifact_store.clone());
        Ok(repl)
    }

    fn evaluate(&mut self, session: &mut Repl<NativeEffect>, source: &str) -> Result<(), Fault> {
        self.evaluate_value(session, source).map(|_| ())
    }

    fn evaluate_value(
        &mut self,
        session: &mut Repl<NativeEffect>,
        source: &str,
    ) -> Result<Option<quiver_core::wire::WireValue>, Fault> {
        let types_request = self
            .environment
            .lock()
            .unwrap()
            .request_process_types()
            .map_err(|e| Fault::Runtime(format!("{e}")))?;
        let process_types = match self.wait(types_request) {
            RequestResult::ProcessTypes(types) => types,
            _ => HashMap::new(),
        };

        let request = session
            .evaluate(&mut self.environment.lock().unwrap(), source, process_types)
            .map_err(Fault::from)?;

        // A step with nothing to execute — a type alias — produces no request.
        let Some(request) = request else {
            return Ok(None);
        };

        match self.wait(request) {
            RequestResult::Result(Ok(value), _) => Ok(Some(value)),
            RequestResult::Result(Err(error), _) => Err(Fault::Runtime(error.crash_message())),
            _ => Err(Fault::Runtime("unexpected result".to_string())),
        }
    }

    fn wait(&self, request: u64) -> RequestResult {
        // The stepping thread does the stepping; this thread only polls, sleeping on
        // the progress signal between polls. The timeout is a safety bound, not a poll
        // interval — a resolution wakes the wait immediately.
        loop {
            let seen = self.progress.generation();
            if let Ok(Some(result)) = self.environment.lock().unwrap().poll_request(request) {
                return result;
            }
            self.progress
                .wait_past(seen, std::time::Duration::from_millis(100));
        }
    }
}

impl Drop for Runner {
    fn drop(&mut self) {
        self.shutdown.store(true, Ordering::Relaxed);
        // Wake the thread so it observes the flag; without this it stays parked and the join
        // below never returns.
        self.waker.wake();
        if let Some(handle) = self.stepping.take() {
            let _ = handle.join();
        }
    }
}

/// A step that did not complete. The distinction is whether the session survived it.
enum Fault {
    Static(String),
    Runtime(String),
}

impl From<ReplError> for Fault {
    fn from(error: ReplError) -> Self {
        match error {
            ReplError::Parser(e) => Fault::Static(format!("{e}")),
            ReplError::Compiler(e) => Fault::Static(format!("{e:?}")),
            ReplError::Runtime(e) => Fault::Runtime(e.crash_message()),
            ReplError::Environment(e) => Fault::Runtime(format!("{e}")),
        }
    }
}

/// The pattern from an assertion failure's message, or `None` if the panic was anything else
/// (`__panic__`, a builtin's argument rejection). The message is built by the compiler when it
/// emits the check.
fn assertion_pattern(message: &str) -> Option<String> {
    let rest = message.strip_prefix("Assertion '")?;
    let end = rest.rfind("' failed")?;
    Some(rest[..end].to_string())
}

fn resolver_for(path: &Path) -> PackageResolver {
    match path.parent() {
        Some(dir) if !dir.as_os_str().is_empty() => PackageResolver::for_dir(dir),
        _ => PackageResolver::inline(),
    }
}

/// The name the compiler stamps into spans. Steps are compiled at the position they occupy in
/// the document, so this plus the compiler's own line and column is a usable reference.
fn source_name(path: &Path) -> String {
    path.file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| path.display().to_string())
}

fn title_of(chapter: &Chapter) -> String {
    chapter
        .title
        .clone()
        .unwrap_or_else(|| "(preamble)".to_string())
}

fn count_assertions(block: &Block) -> usize {
    quiver_markdown::count_assertions(&block.source).unwrap_or(0)
}

/// The assertions a chapter carries at or below `line` — what abandoning it costs.
fn remaining_assertions(chapter: &Chapter, line: usize) -> usize {
    chapter
        .runnable()
        .filter_map(|block| block.steps().ok())
        .flatten()
        .filter(|step| step.line > line)
        .map(|step| step.assertions)
        .sum()
}

/// One chapter's line: a verdict marker, the title, then its tally. A chapter with nothing to
/// run is neither passed nor failed — it is unchecked, and marked as its own third thing so it
/// cannot be read as green.
fn summarise(report: &ChapterReport) -> String {
    // `✔`/`✘` are East-Asian *narrow*; the en dash is *ambiguous*, so a terminal configured to
    // render ambiguous-width characters double will indent this row's title by one.
    let marker = if !report.ok() {
        "✘".red()
    } else if report.runnable_blocks == 0 {
        "–".yellow()
    } else {
        "✔".green()
    };

    let skipped = format!("{} skipped", report.skipped_blocks);
    let tally = match (report.runnable_blocks, report.skipped_blocks) {
        (0, _) => format!("({skipped})"),
        (_, 0) => format!("({}/{})", report.passed, report.total),
        _ => format!("({}/{}; {skipped})", report.passed, report.total),
    };

    format!("{marker}  {} {}", report.title, tally.bright_black())
}

fn print_failure(document: &str, chapter: &str, failure: &Failure) {
    println!(
        "{}  {}",
        format!("{document}:{}", failure.line).bold(),
        format!("in {chapter}").bright_black()
    );
    for line in failure.step.lines() {
        println!("      {line}");
    }
    match &failure.cause {
        Cause::Assertion { pattern, actual } => {
            println!("    expected  {pattern}");
            match actual {
                Some(actual) => println!("    actual    {}", actual.red()),
                None => println!(
                    "    {}",
                    "actual    (could not be recovered)".bright_black()
                ),
            }
        }
        Cause::Error(message) => println!("    {}", message.red()),
    }
}

fn total_line(reports: &[ChapterReport]) -> String {
    let passed: usize = reports.iter().map(|r| r.passed).sum();
    let total: usize = reports.iter().map(|r| r.total).sum();
    let skipped: usize = reports.iter().map(|r| r.skipped_blocks).sum();
    let chapters = reports.len();

    let head = format!("{passed}/{total} assertions in {chapters} chapters");
    let head = if passed == total {
        head.green().bold()
    } else {
        head.red().bold()
    };
    if skipped > 0 {
        format!(
            "{head}, {}",
            format!(
                "{skipped} block{} skipped",
                if skipped == 1 { "" } else { "s" }
            )
            .bright_black()
        )
    } else {
        head.to_string()
    }
}
