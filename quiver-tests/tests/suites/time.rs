//! What a document cannot assert about `%time`.
//!
//! The module's semantics — the calendar, arithmetic, comparison, formatting and parsing —
//! are specified and checked in `std/docs/time.md`, which `quiv test` runs. What stays here
//! needs the harness: the host clocks and zone database read through real builtins
//! (`with_io`), the purity gate that rejects them in restricted contexts and at compile time,
//! and where a `%time{ … }` compile error points.

use crate::common::*;
use quiver_compiler::compiler::Error;
use std::collections::HashMap;

#[test]
fn test_now_is_plausible() {
    // Between mid-2025 and the year 3000 — catches unit mistakes (the builtin answers
    // nanoseconds) and epoch mix-ups without depending on the actual date.
    quiver()
        .with_io()
        .evaluate(
            "n = %time.now [];
             %time.after? [n, %time{ 2025-06-01T00:00Z }];
             %time.before? [n, %time{ 3000-01-01T00:00Z }];
             Ok",
        )
        .expect("Ok");
}

#[test]
fn test_monotonic_reads_the_scheduler_clock() {
    // The harness runs workers on a virtual clock, which is what select timeouts (and so
    // `%proc.sleep`) measure — and `monotonic` reads the same clock, so a program timing a
    // sleep sees the sleep's own length, however little real time passed.
    quiver()
        .evaluate(
            "a = %time.monotonic [];
             %proc.sleep %time{ 5s };
             elapsed = %time.since a ~> %time.to_ms;
             __integer_compare__ [elapsed, 5000] ~> =(0 | 1);
             __integer_compare__ [elapsed, 5100] ~> =-1;
             Ok",
        )
        .expect("Ok")
        .expect_duration(5000, 5100);
}

#[test]
fn test_monotonic_never_goes_backwards() {
    quiver()
        .evaluate(
            "a = %time.monotonic [];
             b = %time.monotonic [];
             %time.compare [b, a] ~> =(0 | 1);
             %time.since a ~> %time.compare [~, %time.nanos 0] ~> =(0 | 1);
             Ok",
        )
        .expect("Ok");
}

#[test]
fn test_clock_rejected_in_receive_filter() {
    // A filter may be re-evaluated, so its verdict must be stable: reading the clock
    // inside one is rejected by the purity gate, like a send or an effect.
    quiver()
        .with_io()
        .evaluate(
            r#"
            p = @#[] { !'int { %time.now []; Ok } } []
            p 10
            r = !p
            r:crash<(message: Str['bin])> ~> =(message: m)
            m
            "#,
        )
        .expect("\"host read is not allowed in receive function\"");
}

#[test]
fn test_dialect_error_points_into_the_content() {
    // `2024-13-01` is well-formed but no date: the failure is positioned at the date's
    // start, column 8 (after `%time{ `).
    let error = quiver()
        .evaluate("%time{ 2024-13-01 }")
        .expect_located_compile_error();
    assert_eq!(
        error.error,
        Error::DialectFailed {
            module: "%time".to_string(),
            message: "failed: a valid date".to_string(),
        }
    );
    assert_eq!(error.span.map(|s| (s.line, s.column)), Some((1, 8)));
}

#[test]
fn test_dialect_error_at_a_malformed_component() {
    // The minutes are one digit short: the error points at them, not at the literal.
    let error = quiver()
        .evaluate("%time{ 09:3 }")
        .expect_located_compile_error();
    assert_eq!(
        error.error,
        Error::DialectFailed {
            module: "%time".to_string(),
            message: "failed: two-digit minutes".to_string(),
        }
    );
    assert_eq!(error.span.map(|s| (s.line, s.column)), Some((1, 11)));
}

#[test]
fn test_zone_names_stay_inside_the_database() {
    // A name is a path into the zoneinfo directory, so one that climbs out of it is no zone
    // at all — whether or not the file it would reach exists.
    quiver()
        .with_io()
        .evaluate(r#"%time.zone "../../../etc/passwd" ~> :error<'%time.error>"#)
        .expect(r#"UnknownZone["../../../etc/passwd"]"#);
    quiver()
        .with_io()
        .evaluate(r#"%time.zone "/etc/localtime" ~> :error<'%time.error>"#)
        .expect(r#"UnknownZone["/etc/localtime"]"#);
}

#[test]
fn test_zone_lookup_rejected_at_compile_time() {
    // A module is evaluated at compile time, where host state can't be read: a literal naming
    // an IANA zone compiles to a lookup, and so fails there, while one at a fixed offset is a
    // constant and doesn't.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["meeting".to_string()],
        "[at: %time{ 2024-07-10T09:30[Europe/London] }]".to_string(),
    );
    quiver()
        .with_io()
        .with_modules(modules)
        .evaluate("%meeting.at")
        .expect_error_containing("reading host state is not supported");

    let mut modules = HashMap::new();
    modules.insert(
        vec!["meeting".to_string()],
        "[at: %time{ 2024-07-10T09:30+01:00[+01:00] }]".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("%meeting.at ~> %time.format")
        .expect(r#""2024-07-10T09:30:00+01:00[+01:00]""#);
}
