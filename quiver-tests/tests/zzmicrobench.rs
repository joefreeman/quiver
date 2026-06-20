// Micro-benchmarks to attribute per-call interpreter cost.
// Run: cargo test -p quiver-tests --release --test zzmicrobench micro -- --exact --nocapture --ignored
mod common;
use common::*;
use std::time::Instant;

const N: i64 = 100_000;

// Measure execution of `body` with the loop bound substituted for {N}, cancelling
// fixed per-call overhead by subtracting the same program run with a tiny bound.
fn per_iter_ns(body_tmpl: &str) -> f64 {
    let big = body_tmpl.replace("{N}", &N.to_string());
    let small = body_tmpl.replace("{N}", "2");

    let mut best_big = f64::MAX;
    let mut best_small = f64::MAX;
    for _ in 0..3 {
        let t = Instant::now();
        quiver().evaluate(&big);
        best_big = best_big.min(t.elapsed().as_secs_f64());

        let t = Instant::now();
        quiver().evaluate(&small);
        best_small = best_small.min(t.elapsed().as_secs_f64());
    }
    (best_big - best_small) / (N - 2) as f64 * 1e9
}

#[test]
#[ignore]
fn micro() {
    // Each loop iteration: match(=0) + build arg tuple(s) + builtin call(s) + tail-call.
    // Top-level (not wrapped in #{...}) so the harness actually executes the call.
    let loop_1b = r#"f = #int { | =0 => Ok | %math.sub [~, 1] ~> ^ }, {N} ~> f"#;
    let loop_2b =
        r#"f = #int { | =0 => Ok | %math.sub [~, 1] ~> %math.add [~, 0] ~> ^ }, {N} ~> f"#;
    let loop_3b = r#"f = #int { | =0 => Ok | %math.sub [~, 1] ~> %math.add [~, 0] ~> %math.add [~, 0] ~> ^ }, {N} ~> f"#;

    let t1 = per_iter_ns(loop_1b);
    let t2 = per_iter_ns(loop_2b);
    let t3 = per_iter_ns(loop_3b);

    println!("\n{N} iterations per run");
    println!("1 builtin/iter : {:>8.1} ns/iter", t1);
    println!("2 builtin/iter : {:>8.1} ns/iter", t2);
    println!("3 builtin/iter : {:>8.1} ns/iter", t3);
    println!("---");
    println!("per extra builtin call (2b-1b): {:>8.1} ns", t2 - t1);
    println!("per extra builtin call (3b-2b): {:>8.1} ns", t3 - t2);
    let per_builtin = ((t2 - t1) + (t3 - t2)) / 2.0;
    println!("avg per builtin call          : {:>8.1} ns", per_builtin);
    println!(
        "loop structure (match+tailcall+1 tuple, no builtin): {:>8.1} ns/iter",
        t1 - per_builtin
    );
}
