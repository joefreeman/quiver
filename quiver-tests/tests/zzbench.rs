// General execution benchmark suite.
// Measures per-workload execution time, cancelling compile/fixed overhead by subtracting
// the same program run at trivial size. Run with:
//   cargo test -p quiver-tests --release --test zzbench bench_workloads -- --exact --nocapture --ignored
mod common;
use common::*;
use std::time::Instant;

// (name, template with {N}, big N, small N, note)
// `note` flags which optimisation a workload is expected to exercise.
const WORKLOADS: &[(&str, &str, i64, i64, &str)] = &[
    (
        "arith_loop",
        r#"f = #int { | =0 => Ok | %num.sub [~, 1] ^ }, {N} f"#,
        300_000,
        2,
        "builtin/dispatch",
    ),
    (
        "fib_naive",
        r#"fib = #[#^ -> int, int] { =[self, n], { %num.lt? [n, 2] => n | [self [self, %num.sub [n, 1]], self [self, %num.sub [n, 2]]] %num.add } }, [fib, {N}] fib"#,
        28,
        2,
        "call/frame churn (per-unit N/A)",
    ),
    (
        "range_count",
        r#"[0, {N}] %range.between %range.iter %iter.count"#,
        100_000,
        2,
        "iterator/closures",
    ),
    (
        // Manual O(N) Cons build + destructure-sum: isolates tuple alloc/clone/destructure
        // without std list.collect (which is O(N^2)).
        "list_build_sum",
        r#"list : Nil | Cons[int, ^],
build = #[list, int] { | =[acc, 0] => acc | =[acc, n] => [Cons[n, acc], %num.sub [n, 1]] ^ },
sum = #[list, int] { | =[Nil, acc] => acc | =[Cons[h, t], acc] => [t, %num.add [acc, h]] ^ },
xs = [Nil, {N}] build,
[xs, 0] sum"#,
        1_000,
        2,
        "A: cons build + destructure (O(N^2) today via deep clone)",
    ),
    (
        "tuple_match",
        r#"f = #[int, [int, int]] { | =[0, _] => Ok | =[n, [a, b]] => [%num.sub [n, 1], [a, b]] ^ }, [{N}, [3, 4]] f"#,
        300_000,
        2,
        "A: nested tuple clone/destructure",
    ),
    (
        "binary_const_loop",
        r#"f = #int { | =0 => Ok | b = 'deadbeefcafe0011223344', %num.sub [~, 1] ^ }, {N} f"#,
        300_000,
        2,
        "C: constant binary load",
    ),
];

const REPS: usize = 3;

fn best_eval_secs(src: &str, reps: usize) -> f64 {
    let mut best = f64::MAX;
    for _ in 0..reps {
        let t = Instant::now();
        quiver().evaluate(src);
        best = best.min(t.elapsed().as_secs_f64());
    }
    best
}

#[test]
#[ignore]
fn bench_workloads() {
    println!(
        "\n{:<20} {:>10} {:>10} {:>12}   targets",
        "workload", "work(ms)", "big(ms)", "per-unit(ns)"
    );
    println!("{}", "-".repeat(78));

    for (name, template, big_n, small_n, note) in WORKLOADS {
        let big_src = template.replace("{N}", &big_n.to_string());
        let small_src = template.replace("{N}", &small_n.to_string());

        let big = best_eval_secs(&big_src, REPS);
        let small = best_eval_secs(&small_src, REPS);
        let work = (big - small).max(0.0);
        let per_unit = work / (*big_n as f64) * 1e9;

        println!(
            "{:<20} {:>10.2} {:>10.2} {:>12.1}   {}",
            name,
            work * 1e3,
            big * 1e3,
            per_unit,
            note
        );
    }
}
