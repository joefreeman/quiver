mod common;
use common::*;

#[test]
fn test_new() {
    quiver().evaluate("5 ~> %bin.new").expect("0x0000000000");
    quiver().evaluate("0 ~> %bin.new").expect("0x");
}

#[test]
fn test_length() {
    quiver()
        .evaluate("5 ~> %bin.new ~> %bin.length")
        .expect("5");
    quiver().evaluate("0x68656c6c6f ~> %bin.length").expect("5");
    quiver()
        .evaluate("0 ~> %bin.new ~> %bin.length")
        .expect("0");
}

#[test]
fn test_concat() {
    quiver()
        .evaluate("[0x68656c, 0x6c6f] ~> %bin.concat")
        .expect("0x68656c6c6f");

    quiver()
        .evaluate(
            r#"
            a = 2 ~> %bin.new,
            b = 3 ~> %bin.new,
            [a, b] ~> %bin.concat
            "#,
        )
        .expect("0x0000000000");

    quiver()
        .evaluate("[0x, 0xff] ~> %bin.concat")
        .expect("0xff");
}

#[test]
fn test_and() {
    quiver().evaluate("[0xff, 0xf0] ~> %bin.and").expect("0xf0");
    quiver().evaluate("[0xaa, 0xcc] ~> %bin.and").expect("0x88");
}

#[test]
fn test_or() {
    quiver().evaluate("[0xf0, 0x0f] ~> %bin.or").expect("0xff");
    quiver().evaluate("[0xaa, 0x55] ~> %bin.or").expect("0xff");
}

#[test]
fn test_xor() {
    quiver().evaluate("[0xff, 0xf0] ~> %bin.xor").expect("0x0f");
    quiver().evaluate("[0xaa, 0xaa] ~> %bin.xor").expect("0x00");
}

#[test]
fn test_not() {
    quiver().evaluate("0x00 ~> %bin.not").expect("0xff");
    quiver().evaluate("0xff ~> %bin.not").expect("0x00");
    quiver().evaluate("0xf0 ~> %bin.not").expect("0x0f");
}

#[test]
fn test_shift() {
    // Test left shift (positive)
    quiver().evaluate("[0x01, 1] ~> %bin.shift").expect("0x02");
    quiver().evaluate("[0x0f, 4] ~> %bin.shift").expect("0xf0");

    // Test right shift (negative)
    quiver().evaluate("[0x02, -1] ~> %bin.shift").expect("0x01");
    quiver().evaluate("[0xf0, -4] ~> %bin.shift").expect("0x0f");

    // Test zero shift
    quiver().evaluate("[0xff, 0] ~> %bin.shift").expect("0xff");
}

#[test]
fn test_get_byte() {
    quiver()
        .evaluate("[0x68656c6c6f, 0] ~> %bin.get_byte")
        .expect("104"); // 0x68 = 104
    quiver()
        .evaluate("[0x68656c6c6f, 1] ~> %bin.get_byte")
        .expect("101"); // 0x65 = 101
    quiver()
        .evaluate("[0x68656c6c6f, 4] ~> %bin.get_byte")
        .expect("111"); // 0x6f = 111
}

#[test]
fn test_get_bit() {
    quiver().evaluate("[0x80, 0] ~> %bin.get_bit").expect("1"); // MSB of 0x80 = 10000000
    quiver().evaluate("[0x80, 7] ~> %bin.get_bit").expect("0"); // LSB of 0x80
    quiver().evaluate("[0xff, 3] ~> %bin.get_bit").expect("1"); // Bit 3 of 0xFF

    // Test bit across byte boundary
    quiver().evaluate("[0xff00, 8] ~> %bin.get_bit").expect("0"); // First bit of second byte (0x00)
}

#[test]
fn test_set_byte() {
    quiver()
        .evaluate("[0x00000000, 0, 255] ~> %bin.set_byte")
        .expect("0xff000000");
    quiver()
        .evaluate("[0x00000000, 2, 170] ~> %bin.set_byte")
        .expect("0x0000aa00");

    quiver()
        .evaluate(
            r#"
            b = 3 ~> %bin.new,
            [b, 1, 255] ~> %bin.set_byte
            "#,
        )
        .expect("0x00ff00");
}

#[test]
fn test_set_bit() {
    quiver()
        .evaluate("[0x00, 0, 1] ~> %bin.set_bit")
        .expect("0x80"); // Set MSB
    quiver()
        .evaluate("[0x00, 7, 1] ~> %bin.set_bit")
        .expect("0x01"); // Set LSB
    quiver()
        .evaluate("[0xff, 0, 0] ~> %bin.set_bit")
        .expect("0x7f"); // Clear MSB

    // Test bit across byte boundary
    quiver()
        .evaluate("[0x0000, 8, 1] ~> %bin.set_bit")
        .expect("0x0080"); // Set first bit of second byte
}

#[test]
fn test_slice() {
    quiver()
        .evaluate("[0x68656c6c6f, 0, 3] ~> %bin.slice")
        .expect("0x68656c"); // "hel"
    quiver()
        .evaluate("[0x68656c6c6f, 2, 5] ~> %bin.slice")
        .expect("0x6c6c6f"); // "llo"
    quiver()
        .evaluate("[0x68656c6c6f, 1, 4] ~> %bin.slice")
        .expect("0x656c6c"); // "ell"

    // Test empty slice
    quiver()
        .evaluate("[0x68656c6c6f, 2, 2] ~> %bin.slice")
        .expect("0x");

    // Test full slice
    quiver()
        .evaluate("[0x68656c6c6f, 0, 5] ~> %bin.slice")
        .expect("0x68656c6c6f");
}

#[test]
fn test_chained_operations() {
    // Test combining multiple operations
    quiver()
        .evaluate(
            r#"
            a = 2 ~> %bin.new,
            b = [a, 0, 255] ~> %bin.set_byte,
            c = [b, 1, 170] ~> %bin.set_byte,
            [0xff00, c] ~> %bin.concat
            "#,
        )
        .expect("0xff00ffaa");

    // Test bitwise operations chain
    quiver()
        .evaluate(
            r#"
            a = 0xaa,
            b = 0xff,
            [a, b] ~> %bin.and ~> %bin.not
            "#,
        )
        .expect("0x55");
}

#[test]
fn test_bit_manipulation_pattern() {
    // Simulate HAMT-like bit operations
    quiver()
        .evaluate(
            r#"
            bitmap = 1 ~> %bin.new,
            step1 = [bitmap, 0, 1] ~> %bin.set_bit,
            step2 = [step1, 3, 1] ~> %bin.set_bit,
            step3 = [step2, 7, 1] ~> %bin.set_bit,
            step3
            "#,
        )
        .expect("0x91"); // 10010001 in binary
}

#[test]
fn test_append() {
    // Append single byte
    quiver()
        .evaluate("[0x68656c, 108, 1] ~> %bin.append")
        .expect("0x68656c6c");

    // Append multi-byte value
    quiver()
        .evaluate("[0x, 1751477356, 4] ~> %bin.append")
        .expect("0x68656c6c");

    // Build string by appending characters (UTF-8)
    quiver()
        .evaluate(
            r#"
            step1 = [0x, 65, 1] ~> %bin.append,
            step2 = [step1, 50089, 2] ~> %bin.append,
            step3 = [step2, 14844588, 3] ~> %bin.append,
            step3
            "#,
        )
        .expect("0x41c3a9e282ac"); // "Aé€"
}

#[test]
fn test_index() {
    // 'hello' = 68 65 6c 6c 6f; find 'l' (0x6c) and 'o' (0x6f).
    quiver()
        .evaluate("[0x68656c6c6f, 108, 0] ~> %bin.index")
        .expect("2");
    // Search respects the offset: the second 'l' is at index 3.
    quiver()
        .evaluate("[0x68656c6c6f, 108, 3] ~> %bin.index")
        .expect("3");
    quiver()
        .evaluate("[0x68656c6c6f, 111, 0] ~> %bin.index")
        .expect("4");
    // Absent byte yields nil.
    quiver()
        .evaluate("[0x68656c6c6f, 122, 0] ~> %bin.index")
        .expect("[]");
    // Offset past the end yields nil.
    quiver()
        .evaluate("[0x68656c6c6f, 104, 5] ~> %bin.index")
        .expect("[]");
}

#[test]
fn test_index_across_concat() {
    // Concatenation builds a rope; search must cross the boundary. '6162' ++ '0a63' -> ab\nc.
    quiver()
        .evaluate("[0x6162, 0x0a63] ~> %bin.concat ~> [~, 10, 0] ~> %bin.index")
        .expect("2");
}
