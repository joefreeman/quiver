use crate::common::*;

// `%time` (std/time.qv): the host clocks (`now`, `monotonic` — the
// `__time_*__` builtins, so those tests need with_io) and the pure calendar functions
// (`parts`, `http_date`, `iso8601`), which are fully deterministic.

#[test]
fn test_parts_of_epoch() {
    quiver()
        .evaluate("0 ~> %time.parts ~")
        .expect("[year: 1970, month: 1, day: 1, hour: 0, minute: 0, second: 0, millisecond: 0, weekday: Thu]");
}

#[test]
fn test_parts_of_known_timestamp() {
    // 2024-01-01T00:00:00Z was a Monday.
    quiver()
        .evaluate("1704067200000 ~> %time.parts ~")
        .expect("[year: 2024, month: 1, day: 1, hour: 0, minute: 0, second: 0, millisecond: 0, weekday: Mon]");
    // A leap day, late in the day: 2024-02-29T23:59:59.500Z (Thursday).
    quiver()
        .evaluate("1709251199500 ~> %time.parts ~")
        .expect("[year: 2024, month: 2, day: 29, hour: 23, minute: 59, second: 59, millisecond: 500, weekday: Thu]");
}

#[test]
fn test_parts_before_the_epoch() {
    // Floored (not truncating) division: one millisecond before the epoch is the last
    // millisecond of 1969, not a mangled day.
    quiver()
        .evaluate("-1 ~> %time.parts ~")
        .expect("[year: 1969, month: 12, day: 31, hour: 23, minute: 59, second: 59, millisecond: 999, weekday: Wed]");
}

#[test]
fn test_http_date() {
    // RFC 9110's canonical IMF-fixdate example.
    quiver()
        .evaluate("784111777000 ~> %time.http_date ~")
        .expect(r#""Sun, 06 Nov 1994 08:49:37 GMT""#);
}

#[test]
fn test_iso8601() {
    quiver()
        .evaluate("1704067200123 ~> %time.iso8601 ~")
        .expect(r#""2024-01-01T00:00:00.123Z""#);
}

#[test]
fn test_now_is_plausible() {
    // Between mid-2025 and the year 3000 — catches unit mistakes (seconds vs millis)
    // and epoch mix-ups without depending on the actual date.
    quiver()
        .with_io()
        .evaluate(
            "n = %time.now [];
             [n, 1750000000000] ~> __integer_compare__ ~ ~> =1;
             [n, 32503680000000] ~> __integer_compare__ ~ ~> =-1;
             Ok",
        )
        .expect("Ok");
}

#[test]
fn test_monotonic_never_goes_backwards() {
    quiver()
        .with_io()
        .evaluate(
            "a = %time.monotonic [];
             b = %time.monotonic [];
             [b, a] ~> __integer_compare__ ~ ~> =(0 | 1);
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
            %proc.send [p, 10]
            r = !p
            r:((message: Str['bin]))crash ~> =(message: m)
            m
            "#,
        )
        .expect("\"host read is not allowed in receive function\"");
}
