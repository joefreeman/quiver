//! `quiv proc` and `quiv kill`: inspecting and stopping the processes a running server
//! hosts. None spawns a server — with none listening there is nothing to inspect.

use colored::Colorize;
use quiver_cli::client::{Client, RequestError, connect};
use quiver_cli::inspection::{self, Style};
use quiver_cli::protocol::{ProcessListing, default_socket_path};
use quiver_core::process::WorkerInfo;
use std::io::{IsTerminal, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, mpsc};
use std::time::{Duration, Instant};

fn client() -> Result<Client, Box<dyn std::error::Error>> {
    Ok(connect(&default_socket_path())?)
}

/// List the server's processes (live ones, or `all` of them), as a table or an
/// ownership tree under a summary of the processes and workers; or, given a pid, show
/// that process in detail.
pub fn list_command(
    pid: Option<u64>,
    tree: bool,
    all: bool,
    json: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let client = client()?;
    let listing = client.processes()?;
    if let Some(pid) = pid {
        let detail = client.process(pid).map_err(|e| not_found(e, pid))?;
        if json {
            println!("{}", serde_json::to_string_pretty(&detail)?);
        } else {
            println!(
                "{}",
                inspection::render_detail(&detail, &inspection::owned_by(&listing, pid))
            );
        }
        return Ok(());
    }
    if json {
        let processes: Vec<_> = listing
            .processes
            .iter()
            .filter(|process| all || inspection::is_live(process.status))
            .collect();
        println!("{}", serde_json::to_string_pretty(&processes)?);
    } else {
        // On a terminal, each line is cut to its width, since a process type alone can
        // run to hundreds of characters; piped, the listing is left whole.
        if std::io::stdout().is_terminal() {
            let listing = render_listing(&listing, &client.workers()?, tree, all, Style::Terminal);
            let (_, columns) = terminal_size();
            for line in listing.lines() {
                println!("{}", clip(line, columns));
            }
        } else {
            println!(
                "{}",
                render_listing(&listing, &client.workers()?, tree, all, Style::Plain)
            );
        }
    }
    Ok(())
}

/// Stop each process, and the subtree it owns — a client's root included, on the
/// host's authority.
pub fn kill_command(pids: Vec<u64>) -> Result<(), Box<dyn std::error::Error>> {
    let client = client()?;
    let mut failed = false;
    for pid in pids {
        if let Err(e) = client.delete_process(pid) {
            eprintln!("{}", not_found(e, pid));
            failed = true;
        }
    }
    if failed {
        std::process::exit(1);
    }
    Ok(())
}

fn not_found(error: RequestError, pid: u64) -> Box<dyn std::error::Error> {
    match error {
        RequestError::Http { status: 404, .. } => format!("no process {pid}").into(),
        other => other.into(),
    }
}

/// A live view of the server's processes and workers, redrawn as they change: the
/// `/events` stream says when, and a periodic refresh catches what it does not report
/// (mailboxes fill without a status changing). Runs until interrupted.
pub fn watch_command(tree: bool, all: bool) -> Result<(), Box<dyn std::error::Error>> {
    if !std::io::stdout().is_terminal() {
        return Err("quiv proc --watch needs a terminal".into());
    }
    let client = client()?;
    let events = client.events("processes=true&workers=true")?;
    let (changed, changes) = mpsc::channel();
    std::thread::spawn(move || {
        for event in events {
            if changed.send(event.is_ok()).is_err() {
                return;
            }
        }
        let _ = changed.send(false);
    });

    let stop = Arc::new(AtomicBool::new(false));
    for signal in [signal_hook::consts::SIGINT, signal_hook::consts::SIGTERM] {
        signal_hook::flag::register(signal, Arc::clone(&stop))?;
    }

    let mut screen = Screen::enter();
    let mut last_draw: Option<Instant> = None;
    let mut dirty = true;
    let mut size = terminal_size();
    let result = loop {
        if stop.load(Ordering::Relaxed) {
            break Ok(());
        }
        match changes.recv_timeout(Duration::from_millis(100)) {
            Ok(true) => dirty = true,
            Ok(false) | Err(mpsc::RecvTimeoutError::Disconnected) => {
                break Err("the server went away".into());
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {}
        }
        let resized = terminal_size();
        if resized != size {
            size = resized;
            dirty = true;
        }
        if last_draw.is_none_or(|drawn| drawn.elapsed() >= REFRESH) {
            dirty = true;
        }
        // Updates come in bursts; batching them to a frame rate keeps a busy server
        // from being answered with a listing per event.
        if dirty && last_draw.is_none_or(|drawn| drawn.elapsed() >= FRAME) {
            let frame = match client.processes().and_then(|listing| {
                Ok(render_listing(
                    &listing,
                    &client.workers()?,
                    tree,
                    all,
                    Style::Terminal,
                ))
            }) {
                Ok(frame) => frame,
                Err(e) => break Err(e.into()),
            };
            screen.draw(&frame, size);
            last_draw = Some(Instant::now());
            dirty = false;
        }
    };
    drop(screen);
    result
}

/// How often `--watch` redraws when nothing reports a change.
const REFRESH: Duration = Duration::from_secs(1);
/// The shortest interval between redraws.
const FRAME: Duration = Duration::from_millis(100);

/// The process listing, under a summary of the processes and workers. Live processes
/// come first, so a view clipped to the terminal loses the terminated ones before them.
fn render_listing(
    listing: &ProcessListing,
    workers: &[WorkerInfo],
    tree: bool,
    all: bool,
    style: Style,
) -> String {
    let mut processes: Vec<_> = listing
        .processes
        .iter()
        .filter(|process| all || inspection::is_live(process.status))
        .collect();
    processes.sort_by_key(|process| !inspection::is_live(process.status));
    let body = if processes.is_empty() {
        "No processes".to_string()
    } else if tree {
        inspection::render_tree(&processes, style)
    } else {
        inspection::render_table(&processes, style)
    };
    format!(
        "{}\n\n{body}",
        inspection::render_summary(listing, workers, style)
    )
}

/// The terminal's alternate screen, left on drop — however `--watch` ends — so the shell's
/// own screen comes back untouched.
///
/// Nothing reads the terminal's input meanwhile, so its echo is turned off: the arrow keys
/// a terminal sends for the mouse wheel on the alternate screen would otherwise be printed
/// over the frame. The settings it replaces are restored on drop.
struct Screen {
    input: Option<libc::termios>,
}

impl Screen {
    fn enter() -> Screen {
        // SAFETY: tcgetattr writes the terminal's settings into the `termios` passed;
        // it fails (and the settings are left alone) when stdin is not a terminal.
        let mut input: libc::termios = unsafe { std::mem::zeroed() };
        let input = (unsafe { libc::tcgetattr(libc::STDIN_FILENO, &mut input) } == 0).then(|| {
            let mut quiet = input;
            // Line editing off too, so input is not held back for a newline; signals
            // stay on, so Ctrl-C still ends the view.
            quiet.c_lflag &= !(libc::ECHO | libc::ICANON);
            // SAFETY: applies settings read from this same terminal above.
            unsafe { libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &quiet) };
            input
        });
        // Alternate screen, cursor hidden.
        print!("\x1b[?1049h\x1b[?25l");
        let _ = std::io::stdout().flush();
        Screen { input }
    }

    /// Replace the screen's contents with `frame`, clipped to the terminal. When the
    /// frame is too tall, its last row says how many lines did not fit.
    fn draw(&mut self, frame: &str, (rows, columns): (usize, usize)) {
        let lines: Vec<&str> = frame.lines().collect();
        let shown = if lines.len() > rows {
            rows.saturating_sub(1)
        } else {
            lines.len()
        };
        let mut out = lines[..shown]
            .iter()
            .map(|line| clip(line, columns))
            .collect::<Vec<_>>();
        if shown < lines.len() {
            let more = format!("… {} more", lines.len() - shown);
            out.push(clip(&more.dimmed().to_string(), columns));
        }
        print!("\x1b[H\x1b[2J{}", out.join("\n"));
        let _ = std::io::stdout().flush();
    }
}

impl Drop for Screen {
    fn drop(&mut self) {
        print!("\x1b[?25h\x1b[?1049l");
        let _ = std::io::stdout().flush();
        if let Some(input) = &self.input {
            // Discard what was typed or scrolled meanwhile, so it doesn't reach the shell,
            // then restore the settings.
            // SAFETY: both act on stdin, which `enter` found to be a terminal.
            unsafe {
                libc::tcflush(libc::STDIN_FILENO, libc::TCIFLUSH);
                libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, input);
            }
        }
    }
}

/// `line` cut to `columns` visible characters. Styling escapes take no columns, and a
/// line that is cut has its styling reset, so it cannot bleed into the next.
fn clip(line: &str, columns: usize) -> String {
    let mut out = String::new();
    let mut visible = 0;
    let mut chars = line.chars();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            // A control sequence: `ESC [`, parameters, then a final byte in `@`..=`~`.
            out.push(c);
            for c in chars.by_ref() {
                out.push(c);
                if c != '[' && ('@'..='~').contains(&c) {
                    break;
                }
            }
            continue;
        }
        if visible == columns {
            out.push_str("\x1b[0m");
            break;
        }
        out.push(c);
        visible += 1;
    }
    out
}

/// The terminal's `(rows, columns)`.
fn terminal_size() -> (usize, usize) {
    // SAFETY: TIOCGWINSZ writes a `winsize` into the one passed.
    let mut size: libc::winsize = unsafe { std::mem::zeroed() };
    if unsafe { libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut size) } == 0
        && size.ws_row > 0
        && size.ws_col > 0
    {
        (size.ws_row as usize, size.ws_col as usize)
    } else {
        (24, 80)
    }
}

#[cfg(test)]
mod tests {
    use super::clip;

    #[test]
    fn clip_counts_only_visible_characters() {
        let styled = "\x1b[36mWaiting\x1b[0m  ok";
        assert_eq!(clip(styled, 20), styled, "a line that fits is untouched");
        assert_eq!(clip(styled, 9), "\x1b[36mWaiting\x1b[0m  \x1b[0m");
    }

    #[test]
    fn clip_resets_styling_it_cuts_through() {
        assert_eq!(clip("\x1b[2m… 4 more\x1b[0m", 3), "\x1b[2m… 4\x1b[0m");
        assert_eq!(clip("\x1b[2mdimmed\x1b[0m", 3), "\x1b[2mdim\x1b[0m");
    }
}
