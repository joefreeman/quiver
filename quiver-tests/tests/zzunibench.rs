// Runtime benchmark for unicode/string operations.
// Reuses ONE environment (then_evaluate) so the std graph is compiled once and we
// measure execution, not compilation. Run with:
//   cargo test -p quiver-tests --release --test zzunibench bench_unicode -- --exact --nocapture --ignored
mod common;
use common::*;
use std::time::Instant;

// Each profile uses a single-codepoint unit so char-count == repeat-count.
const PROFILES: &[(&str, &str)] = &[
    ("ascii", "a"),  // 1 byte
    ("latin1", "é"), // 2 bytes
    ("cjk", "本"),   // 3 bytes
    ("emoji", "🚀"), // 4 bytes
];

const SIZE: usize = 2000;
const REPS: usize = 5;

// closure building Quiver source for a given string literal body and char count
type OpFn = fn(&str, usize) -> String;

// op name -> source-building closure
fn ops() -> Vec<(&'static str, OpFn)> {
    vec![
        ("length", |s, _n| format!(r#""{s}" %str.length"#)),
        ("iter_count", |s, _n| {
            format!(r#""{s}" %str.iter %iter.count"#)
        }),
        ("iter_collect", |s, _n| {
            format!(r#""{s}" %str.iter %str.collect"#)
        }),
        ("slice_all", |s, n| format!(r#"["{s}", 0, {n}] %str.slice"#)),
    ]
}

#[test]
#[ignore]
fn bench_unicode() {
    // Warm: first evaluate compiles the std graph once.
    let mut tr = quiver().evaluate(r#""warm" %str.length"#);

    println!(
        "\n{:<14} {:<8} {:>12} {:>12} {:>10}",
        "op", "profile", "total(ms)", "base(ms)", "ns/char"
    );
    println!("{}", "-".repeat(60));

    for (op_name, build) in ops() {
        for (pname, unit) in PROFILES {
            let big: String = unit.repeat(SIZE);
            let one: String = unit.to_string();

            // baseline: per-call overhead (snippet compile + fixed exec) on a 1-char string
            let base = {
                let src = build(&one, 1);
                let mut best = f64::MAX;
                for _ in 0..REPS {
                    let t = Instant::now();
                    tr = tr.then_evaluate(&src);
                    best = best.min(t.elapsed().as_secs_f64());
                }
                best
            };

            let total = {
                let src = build(&big, SIZE);
                let mut best = f64::MAX;
                for _ in 0..REPS {
                    let t = Instant::now();
                    tr = tr.then_evaluate(&src);
                    best = best.min(t.elapsed().as_secs_f64());
                }
                best
            };

            let per_char_ns = ((total - base) / SIZE as f64) * 1e9;
            println!(
                "{:<14} {:<8} {:>12.2} {:>12.2} {:>10.1}",
                op_name,
                pname,
                total * 1e3,
                base * 1e3,
                per_char_ns
            );
        }
    }
    let _ = tr;
}
