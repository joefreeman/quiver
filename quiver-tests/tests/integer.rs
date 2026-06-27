mod common;
use common::*;

#[test]
fn test_and() {
    quiver().evaluate("[255, 240] %int.and").expect("240");
    quiver().evaluate("[170, 204] %int.and").expect("136");
    quiver().evaluate("[255, 240] %int.and").expect("240");
}

#[test]
fn test_or() {
    quiver().evaluate("[240, 15] %int.or").expect("255");
    quiver().evaluate("[170, 85] %int.or").expect("255");
    quiver().evaluate("[0, 0] %int.or").expect("0");
}

#[test]
fn test_xor() {
    quiver().evaluate("[255, 240] %int.xor").expect("15");
    quiver().evaluate("[170, 170] %int.xor").expect("0");
    quiver().evaluate("[123, 0] %int.xor").expect("123");
}

#[test]
fn test_not() {
    quiver().evaluate("0 %int.not").expect("-1");
    quiver().evaluate("-1 %int.not").expect("0");
    quiver().evaluate("1 %int.not").expect("-2");
}

#[test]
fn test_shift_left() {
    quiver().evaluate("[1, 1] %int.shift").expect("2");
    quiver().evaluate("[15, 4] %int.shift").expect("240");
    quiver().evaluate("[1, 8] %int.shift").expect("256");
}

#[test]
fn test_shift_right() {
    quiver().evaluate("[2, -1] %int.shift").expect("1");
    quiver().evaluate("[240, -4] %int.shift").expect("15");
    quiver().evaluate("[256, -8] %int.shift").expect("1");
}

#[test]
fn test_shift_zero() {
    quiver().evaluate("[123, 0] %int.shift").expect("123");
    quiver().evaluate("[0, 0] %int.shift").expect("0");
}

#[test]
fn test_shift_large() {
    // Shift left by >= 64 bits
    quiver().evaluate("[123, 64] %int.shift").expect("0");
    quiver().evaluate("[123, 100] %int.shift").expect("0");

    // Shift right by >= 64 bits (positive number)
    quiver().evaluate("[123, -64] %int.shift").expect("0");

    // Shift right by >= 64 bits (negative number becomes -1)
    quiver().evaluate("[-123, -64] %int.shift").expect("-1");
}

#[test]
fn test_shift_negative_numbers() {
    // Arithmetic right shift preserves sign
    quiver().evaluate("[-8, -1] %int.shift").expect("-4");
    quiver().evaluate("[-16, -2] %int.shift").expect("-4");
    quiver().evaluate("[-1, -1] %int.shift").expect("-1");
}

#[test]
fn test_extract_hash_bits() {
    // HAMT pattern: extract 5-bit chunk from hash at different depths
    quiver()
        .evaluate(
            r#"
            hash = 305419896,
            [hash, 31] %int.and
            "#,
        )
        .expect("24"); // Lower 5 bits: 0x78 & 0x1f = 24

    quiver()
        .evaluate(
            r#"
            hash = 305419896,
            depth = 1,
            shift = [depth, 5] __integer_multiply__,
            shifted = [hash, [0, shift] __integer_subtract__] %int.shift,
            [shifted, 31] %int.and
            "#,
        )
        .expect("19"); // (0x12345678 >> 5) & 0x1f = 19
}

#[test]
fn test_chained_operations() {
    quiver()
        .evaluate(
            r#"
            a = [255, 240] %int.and,
            b = [a, 15] %int.or,
            c = b %int.not,
            [c, 8] %int.shift
            "#,
        )
        .expect("-65536"); // -256 << 8
}

#[test]
fn test_div() {
    quiver().evaluate("[20, 4] %int.div").expect("5");
    quiver().evaluate("[17, 5] %int.div").expect("3"); // truncates toward zero
}

#[test]
fn test_div_by_zero_returns_nil() {
    quiver().evaluate("[10, 0] %int.div").expect("[]");
}

#[test]
fn test_mod() {
    quiver().evaluate("[17, 5] %int.mod").expect("2");
}

#[test]
fn test_mod_by_zero_returns_nil() {
    quiver().evaluate("[10, 0] %int.mod").expect("[]");
}

#[test]
fn test_mod_by_zero_with_fallback() {
    quiver().evaluate("{ [10, 0] %int.mod | -1 }").expect("-1");
}

#[test]
fn test_sqrt() {
    quiver().evaluate("16 %int.sqrt").expect("4");
}

#[test]
fn test_sqrt_truncates() {
    quiver().evaluate("17 %int.sqrt").expect("4");
}

#[test]
fn test_sqrt_zero() {
    quiver().evaluate("0 %int.sqrt").expect("0");
}

#[test]
fn test_sqrt_negative_returns_nil() {
    quiver().evaluate("-4 %int.sqrt").expect("[]");
}

#[test]
fn test_sqrt_negative_with_fallback() {
    quiver().evaluate("-4 { %int.sqrt | 0 }").expect("0");
}
