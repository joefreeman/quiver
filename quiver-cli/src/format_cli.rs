//! The `quiv format` command: rewriting Quiver source in place over files and directory
//! trees, or formatting a stream to stdout.

use ignore::WalkBuilder;
use quiver_compiler::parse;
use std::collections::HashSet;
use std::ffi::OsStr;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

/// The extension a walked directory yields. An explicitly named file is formatted whatever
/// it is called.
const SOURCE_EXTENSION: &str = "qv";

/// Files that are not formatted as they stand. Distinct from [`EXIT_ERROR`] so that CI can
/// tell "these files need formatting" from "the formatter could not do its job".
const EXIT_DRIFT: i32 = 1;

/// The formatter could not read, parse or write something it was asked to.
const EXIT_ERROR: i32 = 2;

pub fn format_command(inputs: Vec<String>, eval: Option<String>, check: bool) {
    // `--eval` and stdin have no file to write back to, so they print to stdout (or, with
    // `--check`, just verify and signal via the exit code).
    if let Some(code) = eval {
        format_stream(&code, "eval", check);
        return;
    }
    if inputs.iter().any(|input| input == "-") {
        if inputs.len() > 1 {
            fail("`-` reads stdin and cannot be combined with paths");
        }
        let mut buffer = String::new();
        if let Err(e) = io::stdin().read_to_string(&mut buffer) {
            fail(&format!("stdin: {}", e));
        }
        format_stream(&buffer, "stdin", check);
        return;
    }

    let mut failed = false;
    let files = collect(&inputs, &mut failed);

    let mut drifted = 0;
    for path in files {
        let display = path.display();
        let source = match fs::read_to_string(&path) {
            Ok(source) => source,
            Err(e) => {
                eprintln!("{}: {}", display, e);
                failed = true;
                continue;
            }
        };
        let Some(formatted) = format_source(&source, &display.to_string()) else {
            failed = true;
            continue;
        };
        if formatted == source {
            continue;
        }
        drifted += 1;
        if check {
            println!("would reformat: {}", display);
        } else if let Err(e) = fs::write(&path, formatted) {
            eprintln!("{}: {}", display, e);
            failed = true;
        }
    }

    if check && drifted > 0 {
        println!(
            "{} file{} would be reformatted",
            drifted,
            if drifted == 1 { "" } else { "s" }
        );
    }

    if failed {
        std::process::exit(EXIT_ERROR);
    }
    if check && drifted > 0 {
        std::process::exit(EXIT_DRIFT);
    }
}

/// Report a message the command cannot continue past.
fn fail(message: &str) -> ! {
    eprintln!("{}", message);
    std::process::exit(EXIT_ERROR);
}

/// The files named by `inputs` — a file as given, a directory walked for `.qv` sources — with
/// no arguments meaning the current directory. Paths repeated across arguments are visited
/// once. Anything missing sets `failed`, since a path the caller named and the formatter
/// never saw would otherwise pass silently.
fn collect(inputs: &[String], failed: &mut bool) -> Vec<PathBuf> {
    let default = [".".to_string()];
    let roots: &[String] = if inputs.is_empty() { &default } else { inputs };

    let mut files = Vec::new();
    let mut seen = HashSet::new();
    for root in roots {
        let path = Path::new(root);
        match fs::metadata(path) {
            Ok(metadata) if metadata.is_dir() => walk(path, &mut files, &mut seen, failed),
            // An explicitly named file is formatted whether or not an ignore rule covers it,
            // and whatever its extension: naming it is the intent.
            Ok(_) => push(path.to_path_buf(), &mut files, &mut seen),
            Err(e) => {
                eprintln!("{}: {}", path.display(), e);
                *failed = true;
            }
        }
    }
    files
}

/// Walk `root` for `.qv` files, skipping hidden entries and anything the surrounding
/// repository's ignore rules cover.
fn walk(root: &Path, files: &mut Vec<PathBuf>, seen: &mut HashSet<PathBuf>, failed: &mut bool) {
    let walk = WalkBuilder::new(root).sort_by_file_name(OsStr::cmp).build();
    for entry in walk {
        match entry {
            Ok(entry) => {
                let is_source = entry.file_type().is_some_and(|kind| kind.is_file())
                    && entry.path().extension() == Some(OsStr::new(SOURCE_EXTENSION));
                if is_source {
                    push(entry.into_path(), files, seen);
                }
            }
            Err(e) => {
                eprintln!("{}", e);
                *failed = true;
            }
        }
    }
}

/// Add `path` unless some earlier argument already reached the same file.
fn push(path: PathBuf, files: &mut Vec<PathBuf>, seen: &mut HashSet<PathBuf>) {
    let identity = fs::canonicalize(&path).unwrap_or_else(|_| path.clone());
    if seen.insert(identity) {
        files.push(path);
    }
}

/// Format `source`, reporting a parse error (and returning `None`) if it does not parse.
fn format_source(source: &str, source_id: &str) -> Option<String> {
    match parse(source) {
        Ok(ast) => Some(quiver_compiler::format_program(&ast, source)),
        Err(e) => {
            crate::report_parse_error(&e, source, source_id);
            None
        }
    }
}

/// Format a single source with no backing file: print it to stdout, or with `--check` exit
/// non-zero if it is not already formatted.
fn format_stream(source: &str, source_id: &str, check: bool) {
    let Some(formatted) = format_source(source, source_id) else {
        std::process::exit(EXIT_ERROR);
    };
    if check {
        if formatted != source {
            std::process::exit(EXIT_DRIFT);
        }
    } else {
        print!("{}", formatted);
    }
}
