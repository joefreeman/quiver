//! `quiv test` — run the Quiver code embedded in a Markdown document.
//!
//! The document is executed, not rendered. Its expected values are already written into it as
//! `//= P` assertions, which the compiler checks itself, so running produces exactly one thing
//! the file does not already say: whether it still holds.
//!
//! A chapter is the unit of scope and of reporting. Its steps are fed to a [`Repl`] one at a
//! time rather than compiled as one program, which is what a reader does and what the language
//! requires: a step that evaluates to nil ends its *sequence*, so a concatenated chapter would
//! stop at the first `//= []` and silently skip every assertion below it.

use colored::Colorize;
use quiver_cli::spawn_worker;
use quiver_compiler::PackageResolver;
use quiver_environment::{Environment, Repl, ReplError, RequestResult, WorkerHandle};
use quiver_io::NativeEffect;
use quiver_markdown::{Assertions, Block, Chapter, Document, Mode};
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
    /// A `//= P` assertion did not match. Carries the pattern, and the value the step actually
    /// produced when re-run without the check (absent if that re-run failed too).
    Assertion {
        pattern: String,
        actual: Option<String>,
        /// Where the check itself is written, when that is not the step being reported — which
        /// is the case whenever the step called a function holding the assertion.
        site: Option<usize>,
    },
    /// A `//! text` step did not fail as promised: either it succeeded (`actual` absent), or it
    /// failed with an error that does not mention `expected`.
    Failure {
        expected: String,
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
    /// Assertions written inside a function body, which is not evaluated by the step that
    /// defines it. They are checked if and when something calls that function — a later step, a
    /// combinator it is handed to, or nothing at all — and the runner cannot see which, so they
    /// are reported apart rather than counted as passed. One that does run and fails is not
    /// lost: it aborts its caller's step, which is reported as that step's failure.
    deferred: usize,
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
        let builtins = quiver_cli::build_builtin_registry();
        let (waker, wake) = quiver_cli::native_transport::wake_channel();

        let mut workers: Vec<Box<dyn WorkerHandle<NativeEffect>>> = Vec::new();
        for i in 0..4 {
            workers.push(Box::new(spawn_worker(
                quiver_cli::native_transport::SystemClock,
                builtins.clone(),
                i as u16,
                waker.clone(),
            )));
        }

        let mut environment = Environment::<NativeEffect>::new(workers);
        environment.set_runtime_declarations(builtins.runtime_declarations().clone());
        if let Some(backend) = quiver_cli::create_effect_backend() {
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
            deferred: 0,
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
                let source = step.positioned();

                // A `//!` step promises to fail, so its verdict is the opposite one and it
                // contributes nothing to the session either way — it is not added to `history`.
                if let Some(expected) = &step.expect_failure {
                    report.total += 1;
                    match self.evaluate(&mut session, &source) {
                        Ok(()) => report.failures.push(Failure {
                            line: step.line,
                            step: step.source.clone(),
                            cause: Cause::Failure {
                                expected: expected.clone(),
                                actual: None,
                            },
                        }),
                        Err(fault) => {
                            let message = fault.message();
                            if message.contains(expected.as_str()) {
                                report.passed += 1;
                            } else {
                                report.failures.push(Failure {
                                    line: step.line,
                                    step: step.source.clone(),
                                    cause: Cause::Failure {
                                        expected: expected.clone(),
                                        actual: Some(message.clone()),
                                    },
                                });
                            }
                            // A runtime error killed the session's process, so an expected one
                            // still has to be recovered from: rebuild the session and replay
                            // what got us here, and the rest of the chapter carries on. A
                            // static error left it intact and needs none of this.
                            if matches!(fault, Fault::Runtime(_))
                                && let Some(rebuilt) = self.restart(path, &history)
                            {
                                session = rebuilt;
                            }
                        }
                    }
                    continue;
                }

                report.total += step.assertions.checked;
                report.deferred += step.assertions.deferred;
                match self.evaluate(&mut session, &source) {
                    Ok(()) => {
                        report.passed += step.assertions.checked;
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
                            cause: self.explain(&message, &history, &source, path, step.line),
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
        let assertions = program_assertions(block);
        report.total += assertions.checked;
        report.deferred += assertions.deferred;

        let result = (|| -> Result<(), String> {
            let builtins = quiver_cli::build_builtin_registry();
            let ast = quiver_compiler::parse(&source).map_err(|e| format!("{e}"))?;
            let (program, module_cache, entry) = quiver_cli::compile::compile_entry(
                ast,
                &resolver_for(path),
                &builtins,
                quiver_compiler::compiler::CompileOptions {
                    debug: true,
                    source_name: source_name(path),
                    ..Default::default()
                },
                Some(Rc::clone(&self.artifact_store)),
            )
            .map_err(|e| format!("{e}"))?;
            // A unit, like every other path: the chapter sessions beside this one already
            // link their modules once into the shared environment, and a program block
            // should reach that same code rather than merge a private copy of it.
            let unit = quiver_compiler::extract_unit(
                &program,
                &module_cache,
                Some(entry),
                0,
                quiver_compiler::Imports::Bundle,
            );
            let store = module_cache
                .artifact_store
                .as_ref()
                .expect("the runner attaches one");
            let modules = quiver_compiler::module_closure(store, &module_cache, &unit)
                .map_err(|module| format!("no stored artifact for module {}", module.display()))?;
            let mut env = self.environment.lock().unwrap();
            for (key, artifact) in &modules {
                env.link_module_unit(*key, &artifact.unit, &builtins)
                    .map_err(|e| format!("{e:?}"))?;
            }
            let pid = env
                .start_process_unit(&unit, &builtins)
                .map_err(|e| format!("{e:?}"))?;
            let request = env
                .request_result(pid, None)
                .map_err(|e| format!("{e:?}"))?;
            drop(env);
            let outcome = match self.wait(request) {
                RequestResult::Result(Ok(_)) => Ok(()),
                RequestResult::Result(Err(error)) => Err(error.to_string()),
                _ => Err("unexpected result".to_string()),
            };
            // Retire it for the same reason a session is retired: this process is persistent
            // too, so completing is not enough to make it collectable.
            let _ = self.environment.lock().unwrap().stop_process(pid);
            outcome
        })();

        match result {
            Ok(()) => report.passed += assertions.checked,
            // A program block is run whole, so there is no step to name: a failed check is
            // blamed on the line the compiler stamped into it, which is the assertion itself.
            Err(message) => report.failures.push(match assertion_site(&message) {
                Some((pattern, line)) => Failure {
                    line: line.unwrap_or(block.line),
                    step: String::new(),
                    cause: Cause::Assertion {
                        pattern,
                        actual: None,
                        site: None,
                    },
                },
                None => Failure {
                    line: block.line,
                    step: String::new(),
                    cause: Cause::Error(message),
                },
            }),
        }
    }

    /// Classify a runtime failure, and — when it is an assertion — recover the value the check
    /// saw by replaying the chapter and re-running the step without it.
    fn explain(
        &mut self,
        message: &str,
        history: &[String],
        source: &str,
        path: &Path,
        step_line: usize,
    ) -> Cause {
        let Some((pattern, line)) = assertion_site(message) else {
            return Cause::Error(message.to_string());
        };
        // A check written above the step that ran is one inside a function the step called. Its
        // value lives in that call's frame, so re-running the step recovers nothing — the site
        // is what there is to report.
        let inside_step = line.is_none_or(|line| line >= step_line);
        Cause::Assertion {
            pattern,
            actual: inside_step
                .then(|| self.replay(history, source, path, line))
                .flatten(),
            site: line.filter(|_| !inside_step),
        }
    }

    fn replay(
        &mut self,
        history: &[String],
        source: &str,
        path: &Path,
        line: Option<usize>,
    ) -> Option<String> {
        // A check observes the value at the end of *its* line, so a chain continuing below it is
        // cut back to that line — every prefix of a multi-line chain is itself a chain. A cut
        // that doesn't parse (an assertion nested inside a block, say) falls back to the whole
        // step, whose value is the right one to report for a check that ends it.
        let stripped = line
            .and_then(|line| truncate_to_line(source, line))
            .and_then(|cut| quiver_markdown::without_assertions(&cut).ok())
            .or_else(|| quiver_markdown::without_assertions(source).ok())?;
        let mut session = self.session(path).ok()?;
        for step in history {
            self.evaluate(&mut session, step).ok()?;
        }
        let value = self.evaluate_value(&mut session, &stripped).ok()??;
        Some(self.environment.lock().unwrap().format_value(&value))
    }

    /// A fresh session with `history` replayed into it — what a chapter needs after a runtime
    /// error has killed the process it was using. `None` if the rebuild itself fails, in which
    /// case the caller keeps the dead session and the chapter's remaining steps report.
    fn restart(&mut self, path: &Path, history: &[String]) -> Option<Session> {
        let mut session = self.session(path).ok()?;
        for step in history {
            self.evaluate(&mut session, step).ok()?;
        }
        Some(session)
    }

    fn session(&mut self, path: &Path) -> Result<Session, ReplError> {
        let builtins = quiver_cli::build_builtin_registry();
        let mut env = self.environment.lock().unwrap();
        let mut repl = Repl::new(&mut env, Box::new(resolver_for(path)), builtins)?;
        // Assertions are checked in debug builds only, which is the whole point of the run.
        repl.set_compile_options(quiver_compiler::compiler::CompileOptions {
            debug: true,
            source_name: source_name(path),
            ..Default::default()
        });
        repl.set_artifact_store(self.artifact_store.clone());
        Ok(Session {
            repl: Some(repl),
            environment: Arc::clone(&self.environment),
        })
    }

    fn evaluate(&mut self, session: &mut Session, source: &str) -> Result<(), Fault> {
        self.evaluate_value(session, source).map(|_| ())
    }

    fn evaluate_value(
        &mut self,
        session: &mut Session,
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
            .repl()
            .evaluate(&mut self.environment.lock().unwrap(), source, process_types)
            .map_err(Fault::from)?;

        // A step with nothing to execute — a type alias — produces no request.
        let Some(request) = request else {
            return Ok(None);
        };

        match self.wait(request) {
            RequestResult::Result(Ok(value)) => Ok(Some(value)),
            RequestResult::Result(Err(error)) => Err(Fault::Runtime(error.to_string())),
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

/// A REPL session, retired when it goes out of scope.
///
/// Retirement is not housekeeping. A host-started process is created `persistent`, and the
/// reclamation sweep categorises a persistent process as a *root* — it is never collected, at
/// any age, until something calls `stop_process` on it. The environment outlives every session
/// in a run, so an unretired session stays in its process table for the rest of the run, where
/// every later chapter's process-type fetch reads it and imports its type. The cost of a chapter
/// then grows with the number of chapters already run.
///
/// Tying that to scope rather than to a convention between call sites is what makes it hold: a
/// session is retired because it was dropped, not because the next one remembered to.
struct Session {
    /// `Some` until `Drop` takes it.
    repl: Option<Repl<NativeEffect>>,
    environment: Arc<Mutex<Environment<NativeEffect>>>,
}

impl Session {
    fn repl(&mut self) -> &mut Repl<NativeEffect> {
        self.repl.as_mut().expect("session dropped")
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        if let Some(repl) = self.repl.take()
            && let Ok(mut environment) = self.environment.lock()
        {
            let _ = environment.stop_process(repl.process_id());
        }
    }
}

/// A step that did not complete. The distinction is whether the session survived it.
enum Fault {
    Static(String),
    Runtime(String),
}

impl Fault {
    fn message(&self) -> &String {
        match self {
            Fault::Static(message) | Fault::Runtime(message) => message,
        }
    }
}

impl From<ReplError> for Fault {
    fn from(error: ReplError) -> Self {
        match error {
            ReplError::Parser(e) => Fault::Static(format!("{e}")),
            ReplError::Compiler(e) => Fault::Static(format!("{e}")),
            ReplError::Runtime(e) => Fault::Runtime(e.to_string()),
            ReplError::Environment(e) => Fault::Runtime(format!("{e}")),
        }
    }
}

/// The pattern and source line from an assertion failure's message, or `None` if the panic was
/// anything else (`__panic__`, a builtin's argument rejection). The message is built by the
/// compiler when it emits the check; the line is absent when the assertion carried no span.
fn assertion_site(message: &str) -> Option<(String, Option<usize>)> {
    let rest = message.strip_prefix("Assertion '")?;
    let end = rest.rfind("' failed")?;
    let line = rest[end..]
        .strip_prefix("' failed at ")
        .and_then(|location| {
            // `<module>:<line>:<column>`, read from the right so a module name is left alone.
            let mut fields = location.rsplitn(3, ':');
            fields.next()?;
            fields.next()?.parse().ok()
        });
    Some((rest[..end].to_string(), line))
}

/// The first `line` lines of `source`, or `None` when it has no more than that — nothing to cut.
fn truncate_to_line(source: &str, line: usize) -> Option<String> {
    let mut lines = source.lines();
    let head: Vec<_> = lines.by_ref().take(line).collect();
    lines.next()?;
    Some(head.join("\n"))
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

fn program_assertions(block: &Block) -> Assertions {
    quiver_markdown::count_program_assertions(&block.source).unwrap_or_default()
}

/// The assertions a chapter carries at or below `line` — what abandoning it costs.
fn remaining_assertions(chapter: &Chapter, line: usize) -> usize {
    chapter
        .runnable()
        .filter_map(|block| block.steps().ok())
        .flatten()
        .filter(|step| step.line > line)
        .map(|step| step.assertions.checked)
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

    let mut notes = Vec::new();
    if report.deferred > 0 {
        notes.push(format!("{} deferred", report.deferred));
    }
    if report.skipped_blocks > 0 {
        notes.push(format!("{} skipped", report.skipped_blocks));
    }

    let tally = if report.runnable_blocks == 0 {
        format!("({})", notes.join("; "))
    } else {
        let counts = format!("{}/{}", report.passed, report.total);
        match notes.is_empty() {
            true => format!("({counts})"),
            false => format!("({counts}; {})", notes.join("; ")),
        }
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
        Cause::Assertion {
            pattern,
            actual,
            site,
        } => {
            if let Some(site) = site {
                println!(
                    "    {}",
                    format!("checked at {document}:{site}, inside a function this step called")
                        .bright_black()
                );
            }
            println!("    expected  {pattern}");
            match actual {
                Some(actual) => println!("    actual    {}", actual.red()),
                None => println!(
                    "    {}",
                    "actual    (could not be recovered)".bright_black()
                ),
            }
        }
        Cause::Failure { expected, actual } => {
            if expected.is_empty() {
                println!("    expected  any failure");
            } else {
                println!("    expected  a failure mentioning {expected}");
            }
            match actual {
                Some(actual) => println!("    actual    {}", actual.red()),
                None => println!("    actual    {}", "the step succeeded".red()),
            }
        }
        Cause::Error(message) => println!("    {}", message.red()),
    }
}

fn total_line(reports: &[ChapterReport]) -> String {
    let passed: usize = reports.iter().map(|r| r.passed).sum();
    let total: usize = reports.iter().map(|r| r.total).sum();
    let deferred: usize = reports.iter().map(|r| r.deferred).sum();
    let skipped: usize = reports.iter().map(|r| r.skipped_blocks).sum();
    let chapters = reports.len();

    // Coloured by the verdict rather than by the counts: a check that ran only because
    // something called the function holding it is not in `total`, so a failing document can
    // still have every counted assertion pass.
    let head = format!("{passed}/{total} assertions in {chapters} chapters");
    let head = if reports.iter().all(ChapterReport::ok) {
        head.green().bold()
    } else {
        head.red().bold()
    };

    let mut notes = Vec::new();
    if deferred > 0 {
        notes.push(format!("{deferred} deferred to a call"));
    }
    if skipped > 0 {
        notes.push(format!(
            "{skipped} block{} skipped",
            if skipped == 1 { "" } else { "s" }
        ));
    }
    if notes.is_empty() {
        head.to_string()
    } else {
        format!("{head}, {}", notes.join(", ").bright_black())
    }
}
