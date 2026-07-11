mod common;
use common::*;

// `%random` (std/random.qv; docs/random-time.md): host entropy via `__random_bytes__`
// (all tests need with_io), with unbiased integer draws and hex tokens on top.

#[test]
fn test_bytes_length() {
    quiver()
        .with_io()
        .evaluate("16 %random.bytes %bin.length")
        .expect("16");
    quiver()
        .with_io()
        .evaluate("0 %random.bytes %bin.length")
        .expect("0");
}

#[test]
fn test_hex_token() {
    quiver()
        .with_io()
        .evaluate("8 %random.hex =Str[b], b %bin.length")
        .expect("16");
    // Two independent 16-byte tokens colliding would be a broken entropy source.
    quiver()
        .with_io()
        .evaluate("a = 16 %random.hex, b = 16 %random.hex, { | a =&b => Same | Different }")
        .expect("Different");
}

#[test]
fn test_below_stays_in_range() {
    // 40 draws from [0, 10), each checked; also covers the rejection-sampling path
    // (10 doesn't divide 256).
    quiver()
        .with_io()
        .evaluate(
            "chk = #'int {
               | =0 => Ok
               | {
                 10 %random.below =('int)v
                 [v, 10] __integer_compare__ =-1
                 [$, 1] __integer_subtract__ ^
               }
             },
             40 chk",
        )
        .expect("Ok");
}

#[test]
fn test_below_edges() {
    quiver().with_io().evaluate("1 %random.below").expect("0");
    quiver().with_io().evaluate("0 %random.below").expect("[]");
}

#[test]
fn test_between_edges() {
    quiver()
        .with_io()
        .evaluate("[5, 5] %random.between")
        .expect("5");
    quiver()
        .with_io()
        .evaluate("[7, 3] %random.between")
        .expect("[]");
}
