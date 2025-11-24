mod common;
use common::*;

#[test]
fn test_new() {
    quiver().evaluate("5 ~> %binary.new").expect("'0000000000'");
    quiver().evaluate("0 ~> %binary.new").expect("''");
}

#[test]
fn test_length() {
    quiver()
        .evaluate("5 ~> %binary.new ~> %binary.length")
        .expect("5");
    quiver()
        .evaluate("'68656c6c6f' ~> %binary.length")
        .expect("5");
    quiver()
        .evaluate("0 ~> %binary.new ~> %binary.length")
        .expect("0");
}

#[test]
fn test_concat() {
    quiver()
        .evaluate("['68656c', '6c6f'] ~> %binary.concat")
        .expect("'68656c6c6f'");

    quiver()
        .evaluate(
            r#"
            a = 2 ~> %binary.new,
            b = 3 ~> %binary.new,
            [a, b] ~> %binary.concat
            "#,
        )
        .expect("'0000000000'");

    quiver()
        .evaluate("['', 'ff'] ~> %binary.concat")
        .expect("'ff'");
}

#[test]
fn test_and() {
    quiver()
        .evaluate("['ff', 'f0'] ~> %binary.and")
        .expect("'f0'");
    quiver()
        .evaluate("['aa', 'cc'] ~> %binary.and")
        .expect("'88'");
}

#[test]
fn test_or() {
    quiver()
        .evaluate("['f0', '0f'] ~> %binary.or")
        .expect("'ff'");
    quiver()
        .evaluate("['aa', '55'] ~> %binary.or")
        .expect("'ff'");
}

#[test]
fn test_xor() {
    quiver()
        .evaluate("['ff', 'f0'] ~> %binary.xor")
        .expect("'0f'");
    quiver()
        .evaluate("['aa', 'aa'] ~> %binary.xor")
        .expect("'00'");
}

#[test]
fn test_not() {
    quiver().evaluate("'00' ~> %binary.not").expect("'ff'");
    quiver().evaluate("'ff' ~> %binary.not").expect("'00'");
    quiver().evaluate("'f0' ~> %binary.not").expect("'0f'");
}

#[test]
fn test_shift() {
    // Test left shift (positive)
    quiver()
        .evaluate("['01', 1] ~> %binary.shift")
        .expect("'02'");
    quiver()
        .evaluate("['0f', 4] ~> %binary.shift")
        .expect("'f0'");

    // Test right shift (negative)
    quiver()
        .evaluate("['02', -1] ~> %binary.shift")
        .expect("'01'");
    quiver()
        .evaluate("['f0', -4] ~> %binary.shift")
        .expect("'0f'");

    // Test zero shift
    quiver()
        .evaluate("['ff', 0] ~> %binary.shift")
        .expect("'ff'");
}

#[test]
fn test_get_byte() {
    quiver()
        .evaluate("['68656c6c6f', 0] ~> %binary.get_byte")
        .expect("104"); // 0x68 = 104
    quiver()
        .evaluate("['68656c6c6f', 1] ~> %binary.get_byte")
        .expect("101"); // 0x65 = 101
    quiver()
        .evaluate("['68656c6c6f', 4] ~> %binary.get_byte")
        .expect("111"); // 0x6f = 111
}

#[test]
fn test_get_bit() {
    quiver()
        .evaluate("['80', 0] ~> %binary.get_bit")
        .expect("1"); // MSB of 0x80 = 10000000
    quiver()
        .evaluate("['80', 7] ~> %binary.get_bit")
        .expect("0"); // LSB of 0x80
    quiver()
        .evaluate("['ff', 3] ~> %binary.get_bit")
        .expect("1"); // Bit 3 of 0xFF

    // Test bit across byte boundary
    quiver()
        .evaluate("['ff00', 8] ~> %binary.get_bit")
        .expect("0"); // First bit of second byte (0x00)
}

#[test]
fn test_set_byte() {
    quiver()
        .evaluate("['00000000', 0, 255] ~> %binary.set_byte")
        .expect("'ff000000'");
    quiver()
        .evaluate("['00000000', 2, 170] ~> %binary.set_byte")
        .expect("'0000aa00'");

    quiver()
        .evaluate(
            r#"
            b = 3 ~> %binary.new,
            [b, 1, 255] ~> %binary.set_byte
            "#,
        )
        .expect("'00ff00'");
}

#[test]
fn test_set_bit() {
    quiver()
        .evaluate("['00', 0, 1] ~> %binary.set_bit")
        .expect("'80'"); // Set MSB
    quiver()
        .evaluate("['00', 7, 1] ~> %binary.set_bit")
        .expect("'01'"); // Set LSB
    quiver()
        .evaluate("['ff', 0, 0] ~> %binary.set_bit")
        .expect("'7f'"); // Clear MSB

    // Test bit across byte boundary
    quiver()
        .evaluate("['0000', 8, 1] ~> %binary.set_bit")
        .expect("'0080'"); // Set first bit of second byte
}

#[test]
fn test_slice() {
    quiver()
        .evaluate("['68656c6c6f', 0, 3] ~> %binary.slice")
        .expect("'68656c'"); // "hel"
    quiver()
        .evaluate("['68656c6c6f', 2, 5] ~> %binary.slice")
        .expect("'6c6c6f'"); // "llo"
    quiver()
        .evaluate("['68656c6c6f', 1, 4] ~> %binary.slice")
        .expect("'656c6c'"); // "ell"

    // Test empty slice
    quiver()
        .evaluate("['68656c6c6f', 2, 2] ~> %binary.slice")
        .expect("''");

    // Test full slice
    quiver()
        .evaluate("['68656c6c6f', 0, 5] ~> %binary.slice")
        .expect("'68656c6c6f'");
}

#[test]
fn test_chained_operations() {
    // Test combining multiple operations
    quiver()
        .evaluate(
            r#"
            a = 2 ~> %binary.new,
            b = [a, 0, 255] ~> %binary.set_byte,
            c = [b, 1, 170] ~> %binary.set_byte,
            ['ff00', c] ~> %binary.concat
            "#,
        )
        .expect("'ff00ffaa'");

    // Test bitwise operations chain
    quiver()
        .evaluate(
            r#"
            a = 'aa',
            b = 'ff',
            [a, b] ~> %binary.and ~> %binary.not
            "#,
        )
        .expect("'55'");
}

#[test]
fn test_bit_manipulation_pattern() {
    // Simulate HAMT-like bit operations
    quiver()
        .evaluate(
            r#"
            bitmap = 1 ~> %binary.new,
            step1 = [bitmap, 0, 1] ~> %binary.set_bit,
            step2 = [step1, 3, 1] ~> %binary.set_bit,
            step3 = [step2, 7, 1] ~> %binary.set_bit,
            step3
            "#,
        )
        .expect("'91'"); // 10010001 in binary
}

#[test]
fn test_append() {
    // Append single byte
    quiver()
        .evaluate("['68656c', 0x6c, 1] ~> %binary.append")
        .expect("'68656c6c'");

    // Append multi-byte value
    quiver()
        .evaluate("['', 0x68656c6c, 4] ~> %binary.append")
        .expect("'68656c6c'");

    // Build string by appending characters (UTF-8)
    quiver()
        .evaluate(
            r#"
            step1 = ['', 0x41, 1] ~> %binary.append,
            step2 = [step1, 0xC3A9, 2] ~> %binary.append,
            step3 = [step2, 0xE282AC, 3] ~> %binary.append,
            step3
            "#,
        )
        .expect("'41c3a9e282ac'"); // "Aé€"
}
