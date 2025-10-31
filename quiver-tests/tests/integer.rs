mod common;
use common::*;

#[test]
fn test_and() {
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [255, 240] ~> integer.and
            "#,
        )
        .expect("240");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [170, 204] ~> integer.and
            "#,
        )
        .expect("136");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [0xff, 0xf0] ~> integer.and
            "#,
        )
        .expect("240");
}

#[test]
fn test_or() {
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [240, 15] ~> integer.or
            "#,
        )
        .expect("255");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [170, 85] ~> integer.or
            "#,
        )
        .expect("255");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [0, 0] ~> integer.or
            "#,
        )
        .expect("0");
}

#[test]
fn test_xor() {
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [255, 240] ~> integer.xor
            "#,
        )
        .expect("15");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [170, 170] ~> integer.xor
            "#,
        )
        .expect("0");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [123, 0] ~> integer.xor
            "#,
        )
        .expect("123");
}

#[test]
fn test_not() {
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            0 ~> integer.not
            "#,
        )
        .expect("-1");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            -1 ~> integer.not
            "#,
        )
        .expect("0");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            1 ~> integer.not
            "#,
        )
        .expect("-2");
}

#[test]
fn test_shift_left() {
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [1, 1] ~> integer.shift
            "#,
        )
        .expect("2");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [15, 4] ~> integer.shift
            "#,
        )
        .expect("240");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [1, 8] ~> integer.shift
            "#,
        )
        .expect("256");
}

#[test]
fn test_shift_right() {
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [2, -1] ~> integer.shift
            "#,
        )
        .expect("1");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [240, -4] ~> integer.shift
            "#,
        )
        .expect("15");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [256, -8] ~> integer.shift
            "#,
        )
        .expect("1");
}

#[test]
fn test_shift_zero() {
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [123, 0] ~> integer.shift
            "#,
        )
        .expect("123");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [0, 0] ~> integer.shift
            "#,
        )
        .expect("0");
}

#[test]
fn test_shift_large() {
    // Shift left by >= 64 bits
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [123, 64] ~> integer.shift
            "#,
        )
        .expect("0");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [123, 100] ~> integer.shift
            "#,
        )
        .expect("0");

    // Shift right by >= 64 bits (positive number)
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [123, -64] ~> integer.shift
            "#,
        )
        .expect("0");

    // Shift right by >= 64 bits (negative number becomes -1)
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [-123, -64] ~> integer.shift
            "#,
        )
        .expect("-1");
}

#[test]
fn test_shift_negative_numbers() {
    // Arithmetic right shift preserves sign
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [-8, -1] ~> integer.shift
            "#,
        )
        .expect("-4");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [-16, -2] ~> integer.shift
            "#,
        )
        .expect("-4");

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            [-1, -1] ~> integer.shift
            "#,
        )
        .expect("-1");
}

#[test]
fn test_extract_hash_bits() {
    // HAMT pattern: extract 5-bit chunk from hash at different depths
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            hash = 0x12345678,
            [hash, 31] ~> integer.and
            "#,
        )
        .expect("24"); // Lower 5 bits: 0x78 & 0x1f = 24

    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            hash = 0x12345678,
            depth = 1,
            shift = [depth, 5] ~> __multiply__,
            shifted = [hash, [0, shift] ~> __subtract__] ~> integer.shift,
            [shifted, 31] ~> integer.and
            "#,
        )
        .expect("19"); // (0x12345678 >> 5) & 0x1f = 19
}

#[test]
fn test_chained_operations() {
    quiver()
        .evaluate(
            r#"
            integer = %"integer"
            a = [0xff, 0xf0] ~> integer.and,
            b = [a, 0x0f] ~> integer.or,
            c = b ~> integer.not,
            [c, 8] ~> integer.shift
            "#,
        )
        .expect("-65536"); // -256 << 8
}
