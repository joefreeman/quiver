//! Text renderings of the server's inspection resources — the process listing and
//! tree, a process's detail, and the workers — shared by `quiv proc` and the REPL's
//! `\p` and `\w` commands.

use crate::protocol::{Outcome, ProcessDetail, ProcessEntry, ProcessListing};
use colored::{ColoredString, Colorize};
use quiver_core::process::{ProcessStatus, WorkerInfo};
use std::collections::HashMap;

/// How a listing is drawn: as plain text, or styled for a terminal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Style {
    Plain,
    Terminal,
}

impl Style {
    fn paint(self, text: &str, apply: impl FnOnce(ColoredString) -> ColoredString) -> String {
        match self {
            Style::Terminal if !text.is_empty() => apply(text.normal()).to_string(),
            _ => text.to_string(),
        }
    }

    /// `text` in the colour of `status`.
    fn status(self, status: ProcessStatus, text: &str) -> String {
        self.paint(text, |text| match status {
            ProcessStatus::Active => text.green(),
            ProcessStatus::Waiting => text.cyan(),
            ProcessStatus::Sleeping => text.blue(),
            ProcessStatus::Failed => text.red(),
            ProcessStatus::Completed => text.dimmed(),
        })
    }

    /// `text` as a detail of a process in `status`: dimmed once it has terminated.
    fn detail(self, status: ProcessStatus, text: &str) -> String {
        if is_live(status) {
            text.to_string()
        } else {
            self.paint(text, |text| text.dimmed())
        }
    }
}

/// Whether a process is still running, rather than a tombstone awaiting reclamation.
pub fn is_live(status: ProcessStatus) -> bool {
    matches!(
        status,
        ProcessStatus::Active | ProcessStatus::Waiting | ProcessStatus::Sleeping
    )
}

/// A summary of the listing and the workers: how many processes there are, by status,
/// then the workers on one line.
pub fn render_summary(listing: &ProcessListing, workers: &[WorkerInfo], style: Style) -> String {
    let mut counts: Vec<(ProcessStatus, usize)> = Vec::new();
    for process in &listing.processes {
        match counts
            .iter_mut()
            .find(|(status, _)| *status == process.status)
        {
            Some((_, count)) => *count += 1,
            None => counts.push((process.status, 1)),
        }
    }
    counts.sort_by_key(|(status, _)| *status as u8);
    let total = listing.processes.len();
    let mut line = format!(
        "{} process{}",
        style.paint(&total.to_string(), |text| text.bold()),
        if total == 1 { "" } else { "es" }
    );
    if !counts.is_empty() {
        let breakdown: Vec<String> = counts
            .iter()
            .map(|(status, count)| {
                let label = format!("{count} {}", format!("{status:?}").to_lowercase());
                style.status(*status, &label)
            })
            .collect();
        line.push_str(&format!(": {}", breakdown.join(", ")));
    }
    format!(
        "{line}\n{}",
        style.paint(&render_workers_summary(workers), |text| text.dimmed())
    )
}

/// The listing as a table, one row per process.
pub fn render_table(processes: &[&ProcessEntry], style: Style) -> String {
    let header = [
        "PID",
        "STATUS",
        "OWNER",
        "MAILBOX",
        "RESOURCES",
        "NAMES",
        "TYPE",
    ];
    let rows: Vec<[String; 7]> = processes
        .iter()
        .map(|process| {
            [
                process.id.to_string(),
                format!("{:?}", process.status),
                process
                    .owner
                    .map_or_else(|| "-".to_string(), |owner| owner.to_string()),
                process.mailbox_size.to_string(),
                process.resources.to_string(),
                or_dash(process.names.join(", ")),
                describe(process),
            ]
        })
        .collect();
    let mut widths = header.map(str::len);
    for row in &rows {
        for (width, cell) in widths.iter_mut().zip(row) {
            *width = (*width).max(cell.chars().count());
        }
    }
    // Padded before painting, so the escapes styling adds take no width.
    let line = |cells: &[&str], paint: &dyn Fn(usize, &str) -> String| {
        let mut line = String::new();
        for (i, (cell, width)) in cells.iter().zip(widths).enumerate() {
            let cell = if i + 1 == cells.len() {
                cell.trim_end().to_string()
            } else {
                format!("{cell:<width$}  ")
            };
            line.push_str(&paint(i, &cell));
        }
        line.trim_end().to_string()
    };
    let mut lines = vec![line(&header, &|_, cell| {
        style.paint(cell, |text| text.bold())
    })];
    lines.extend(processes.iter().zip(&rows).map(|(process, row)| {
        line(&row.each_ref().map(String::as_str), &|i, cell| match i {
            1 => style.status(process.status, cell),
            _ => style.detail(process.status, cell),
        })
    }));
    lines.join("\n")
}

/// The listing as an ownership forest: each process under the one that owns it, and
/// clients' roots and detached processes at the top level.
pub fn render_tree(processes: &[&ProcessEntry], style: Style) -> String {
    let present: HashMap<u64, &ProcessEntry> = processes.iter().map(|p| (p.id, *p)).collect();
    let mut children: HashMap<u64, Vec<&ProcessEntry>> = HashMap::new();
    let mut tops = Vec::new();
    for process in processes {
        match process.owner.filter(|owner| present.contains_key(owner)) {
            Some(owner) => children.entry(owner).or_default().push(process),
            None => tops.push(*process),
        }
    }
    let mut lines = Vec::new();
    for top in tops {
        tree_lines(top, "", None, &children, style, &mut lines);
    }
    lines.join("\n")
}

fn tree_lines(
    process: &ProcessEntry,
    indent: &str,
    last: Option<bool>,
    children: &HashMap<u64, Vec<&ProcessEntry>>,
    style: Style,
    lines: &mut Vec<String>,
) {
    let branch = match last {
        None => "",
        Some(true) => "└─ ",
        Some(false) => "├─ ",
    };
    let detail = |text: &str| style.detail(process.status, text);
    let annotation = |text: &str| style.paint(text, |text| text.dimmed());
    let mut line = format!(
        "{}{}  {}",
        annotation(&format!("{indent}{branch}")),
        detail(&process.id.to_string()),
        style.status(process.status, &format!("{:?}", process.status)),
    );
    if !process.names.is_empty() {
        line.push_str(&detail(&format!("  [{}]", process.names.join(", "))));
    }
    // A client's root leaves the roots table when it ends, so a terminated process
    // without an owner may have been either; only a live one is known to be detached.
    if process.root.is_none() && process.owner.is_none() && is_live(process.status) {
        line.push_str(&annotation("  (detached)"));
    }
    if !process.links.is_empty() {
        line.push_str(&annotation(&format!(
            "  linked: {}",
            join_pids(&process.links)
        )));
    }
    // Last, as the longest part: a line cut to the terminal loses only its end.
    line.push_str(&format!("  {}", detail(&describe(process))));
    lines.push(line);
    let nested = match last {
        None => indent.to_string(),
        Some(true) => format!("{indent}   "),
        Some(false) => format!("{indent}│  "),
    };
    if let Some(kids) = children.get(&process.id) {
        for (i, kid) in kids.iter().enumerate() {
            tree_lines(
                kid,
                &nested,
                Some(i + 1 == kids.len()),
                children,
                style,
                lines,
            );
        }
    }
}

/// What a process is: a client's session, or its process type.
fn describe(process: &ProcessEntry) -> String {
    match (&process.root, &process.process_type) {
        (Some(root), _) => match root.lease_ms {
            Some(lease) => format!("session (lease {}s)", lease / 1000),
            None => "session".to_string(),
        },
        (None, Some(process_type)) => process_type.clone(),
        (None, None) => "-".to_string(),
    }
}

/// One process's detail. `owned` comes from the listing, since a process records its
/// owner but not its children.
pub fn render_detail(detail: &ProcessDetail, owned: &[u64]) -> String {
    let mut lines = vec![format!("Process {}:", detail.id)];
    lines.push(if detail.persistent {
        format!("  Status: {:?} (persistent)", detail.status)
    } else {
        format!("  Status: {:?}", detail.status)
    });
    lines.push(format!(
        "  Type: {}",
        detail.process_type.as_deref().unwrap_or("-")
    ));
    lines.push(format!(
        "  Owner: {}",
        detail
            .owner
            .map_or_else(|| "-".to_string(), |owner| owner.to_string())
    ));
    lines.push(format!("  Owns: {}", or_dash(join_pids(owned))));
    lines.push(format!("  Links: {}", or_dash(join_pids(&detail.links))));
    lines.push(format!("  Names: {}", or_dash(detail.names.join(", "))));
    lines.push(format!("  Resources: {}", detail.resources));
    lines.push(format!(
        "  Stack: {} ({})",
        detail.stack_size,
        format_bytes(detail.heap.stack.bytes)
    ));
    lines.push(format!(
        "  Locals: {} ({})",
        detail.locals_count,
        format_bytes(detail.heap.locals.bytes)
    ));
    lines.push(format!("  Frames: {}", detail.frames_count));
    lines.push(format!(
        "  Mailbox: {} ({})",
        detail.mailbox_size,
        format_bytes(detail.heap.mailbox.bytes)
    ));
    // Distinct across all roots, so it is ≤ the sum of the per-root figures above
    // (a binary referenced from two roots is counted once here).
    lines.push(format!(
        "  Binaries: {} · {}",
        detail.heap.total.binaries,
        format_bytes(detail.heap.total.bytes)
    ));
    lines.push(format!(
        "  Result: {}",
        match &detail.result {
            Some(Outcome::Value { rendered, .. }) => rendered.clone(),
            Some(Outcome::Error { message }) => format!("Error({message})"),
            Some(Outcome::Interrupted) => "Interrupted".to_string(),
            None => "-".to_string(),
        }
    ));
    lines.join("\n")
}

/// The processes `listing` says `id` owns.
pub fn owned_by(listing: &ProcessListing, id: u64) -> Vec<u64> {
    listing
        .processes
        .iter()
        .filter(|process| process.owner == Some(id))
        .map(|process| process.id)
        .collect()
}

pub fn render_workers(workers: &[WorkerInfo]) -> String {
    let mut lines = vec![format!("Workers ({}):", workers.len())];
    for worker in workers {
        let count = worker.process_ids.len();
        let mut line = format!(
            "  Worker {}: {} proc{} · {} binar{} · {}",
            worker.worker_id,
            count,
            if count == 1 { "" } else { "s" },
            worker.live_binaries,
            if worker.live_binaries == 1 {
                "y"
            } else {
                "ies"
            },
            format_bytes(worker.live_bytes)
        );
        if worker.shared_bytes > 0 {
            line.push_str(&format!(" ({} shared)", format_bytes(worker.shared_bytes)));
        }
        lines.push(line);
    }
    lines.join("\n")
}

/// All the workers on one line: how many, and the binaries they hold between them.
pub fn render_workers_summary(workers: &[WorkerInfo]) -> String {
    let binaries: usize = workers.iter().map(|worker| worker.live_binaries).sum();
    let bytes: usize = workers.iter().map(|worker| worker.live_bytes).sum();
    format!(
        "{} worker{} · {} binar{} · {}",
        workers.len(),
        if workers.len() == 1 { "" } else { "s" },
        binaries,
        if binaries == 1 { "y" } else { "ies" },
        format_bytes(bytes)
    )
}

pub fn render_worker(worker: &WorkerInfo) -> String {
    let mut lines = vec![format!("Worker {}:", worker.worker_id)];
    lines.push(format!(
        "  Processes: {}  [{}]",
        worker.process_ids.len(),
        join_pids(&worker.process_ids)
    ));
    // Distinct buffers, counted by identity: one shared between two processes — or
    // two workers — appears once.
    lines.push(format!(
        "  Binaries: {} · {}",
        worker.live_binaries,
        format_bytes(worker.live_bytes)
    ));
    // Bytes whose allocation has another holder — another value here, or one on
    // another worker, since a send passes the handle.
    lines.push(format!("  Shared: {}", format_bytes(worker.shared_bytes)));
    // Unrealised ropes. Every read realises one and nothing caches the result, so a
    // deep rope read repeatedly redoes the work each time.
    if worker.rope_binaries > 0 {
        lines.push(format!(
            "  Ropes: {} unrealised · max depth {}",
            worker.rope_binaries, worker.max_rope_depth
        ));
    }
    lines.push(format!(
        "  Constants: {} · {}",
        worker.constant_binaries,
        format_bytes(worker.constant_bytes)
    ));
    lines.join("\n")
}

fn join_pids<T: ToString>(pids: &[T]) -> String {
    pids.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

fn or_dash(text: String) -> String {
    if text.is_empty() {
        "-".to_string()
    } else {
        text
    }
}

fn format_bytes(bytes: usize) -> String {
    const KB: f64 = 1024.0;
    const MB: f64 = 1024.0 * 1024.0;
    if bytes < 1024 {
        format!("{bytes} B")
    } else if (bytes as f64) < MB {
        format!("{:.1} KB", bytes as f64 / KB)
    } else {
        format!("{:.1} MB", bytes as f64 / MB)
    }
}
