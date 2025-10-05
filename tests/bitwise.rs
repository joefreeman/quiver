mod common;
use common::*;

#[test]
fn test_binary_and() {
    // Test basic AND operation: 0xFF & 0xF0 = 0xF0
    quiver()
        .evaluate("['ff', 'f0'] ~> <binary_and>")
        .expect("'f0'");

    // Test with known values: 0xAA & 0xCC = 0x88
    quiver()
        .evaluate("['aa', 'cc'] ~> <binary_and>")
        .expect("'88'");

    // Test multi-byte AND
    quiver()
        .evaluate("['ff00', '0ff0'] ~> <binary_and>")
        .expect("'0f00'");
}

#[test]
fn test_binary_or() {
    // Test basic OR operation: 0xF0 | 0x0F = 0xFF
    quiver()
        .evaluate("['f0', '0f'] ~> <binary_or>")
        .expect("'ff'");

    // Test with known values: 0xAA | 0x55 = 0xFF
    quiver()
        .evaluate("['aa', '55'] ~> <binary_or>")
        .expect("'ff'");

    // Test multi-byte OR
    quiver()
        .evaluate("['f000', '00f0'] ~> <binary_or>")
        .expect("'f0f0'");
}

#[test]
fn test_binary_xor() {
    // Test basic XOR operation: 0xFF ^ 0xF0 = 0x0F
    quiver()
        .evaluate("['ff', 'f0'] ~> <binary_xor>")
        .expect("'0f'");

    // Test XOR with itself gives zeros
    quiver()
        .evaluate("['aa', 'aa'] ~> <binary_xor>")
        .expect("'00'");

    // Test multi-byte XOR
    quiver()
        .evaluate("['ff00', '00ff'] ~> <binary_xor>")
        .expect("'ffff'");
}

#[test]
fn test_binary_not() {
    // Test NOT operation: ~0x00 = 0xFF
    quiver().evaluate("'00' ~> <binary_not>").expect("'ff'");

    // Test NOT of 0xFF = 0x00
    quiver().evaluate("'ff' ~> <binary_not>").expect("'00'");

    // Test multi-byte NOT
    quiver().evaluate("'f00f' ~> <binary_not>").expect("'0ff0'");
}

#[test]
fn test_binary_shift_left() {
    // Test left shift by 1: 0x01 << 1 = 0x02
    quiver()
        .evaluate("['01', 1] ~> <binary_shift_left>")
        .expect("'02'");

    // Test left shift by 4: 0x0F << 4 = 0xF0
    quiver()
        .evaluate("['0f', 4] ~> <binary_shift_left>")
        .expect("'f0'");

    // Test multi-byte shift: 0x0001 << 8 = 0x0100
    quiver()
        .evaluate("['0001', 8] ~> <binary_shift_left>")
        .expect("'0100'");
}

#[test]
fn test_binary_shift_right() {
    // Test right shift by 1: 0x02 >> 1 = 0x01
    quiver()
        .evaluate("['02', 1] ~> <binary_shift_right>")
        .expect("'01'");

    // Test right shift by 4: 0xF0 >> 4 = 0x0F
    quiver()
        .evaluate("['f0', 4] ~> <binary_shift_right>")
        .expect("'0f'");

    // Test multi-byte shift: 0x0100 >> 8 = 0x0001
    quiver()
        .evaluate("['0100', 8] ~> <binary_shift_right>")
        .expect("'0001'");
}

#[test]
fn test_binary_popcount_critical() {
    // Test popcount - CRITICAL for HAMT operations

    // Empty binary should have 0 bits set
    quiver().evaluate("'' ~> <binary_popcount>").expect("0");

    // Single null byte should have 0 bits set
    quiver().evaluate("'00' ~> <binary_popcount>").expect("0");

    // Test known popcount values
    // 0xFF has 8 bits set
    quiver().evaluate("'ff' ~> <binary_popcount>").expect("8");

    // 0x0F has 4 bits set
    quiver().evaluate("'0f' ~> <binary_popcount>").expect("4");

    // 0x55 = 01010101 has 4 bits set
    quiver().evaluate("'55' ~> <binary_popcount>").expect("4");
}

#[test]
fn test_binary_get_bit_pos() {
    // Test getting specific bits from 0x80 = 10000000
    quiver()
        .evaluate("['80', 0] ~> <binary_get_bit_pos>")
        .expect("1"); // MSB is set
    quiver()
        .evaluate("['80', 7] ~> <binary_get_bit_pos>")
        .expect("0"); // LSB is not set

    // Test with 0xFF = 11111111 (all bits set)
    quiver()
        .evaluate("['ff', 3] ~> <binary_get_bit_pos>")
        .expect("1"); // Bit 3 is set
}

#[test]
fn test_binary_set_bit() {
    // Test setting bit 0 in 0x00 to get 0x80
    quiver()
        .evaluate("['00', 0, 1] ~> <binary_set_bit>")
        .expect("'80'");

    // Test clearing bit 0 in 0xFF to get 0x7F
    quiver()
        .evaluate("['ff', 0, 0] ~> <binary_set_bit>")
        .expect("'7f'");

    // Test setting multiple bits
    quiver()
        .evaluate(
            r#"
            start = '00'
            bit0_set = [start, 0, 1] ~> <binary_set_bit>
            [bit0_set, 7, 1] ~> <binary_set_bit>
            "#,
        )
        .expect("'81'"); // 10000001
}

#[test]
fn test_binary_popcount_hamt_pattern() {
    // Test popcount with typical HAMT patterns

    // Create a binary with specific bit pattern for HAMT testing
    quiver()
        .evaluate(
            r#"
            // Create binary: set some bits to simulate HAMT bitmap
            empty = 4 ~> <binary_new>
            with_bit0 = [empty, 0, 1] ~> <binary_set_bit>
            with_bit5 = [with_bit0, 5, 1] ~> <binary_set_bit>
            with_bit10 = [with_bit5, 10, 1] ~> <binary_set_bit>
            with_bit10 ~> <binary_popcount>
            "#,
        )
        .expect("3");
}

#[test]
fn test_shift_operations_boundary() {
    // Test shift operations with edge cases

    // Shift by 0 should be identity
    quiver()
        .evaluate("['ff', 0] ~> <binary_shift_left>")
        .expect("'ff'");

    // Large shift should result in zeros
    quiver()
        .evaluate("['ff', 100] ~> <binary_shift_left>")
        .expect("'00'");
}

#[test]
fn test_bitwise_chaining() {
    // Test chaining multiple bitwise operations (important for HAMT)
    quiver()
        .evaluate(
            r#"
            a = 'aa',  // 10101010
            b = '55',  // 01010101
            // XOR then AND
            xor_result = [a, b] ~> <binary_xor>,  // Should be 0xFF
            [xor_result, a] ~> <binary_and>  // 0xFF & 0xAA = 0xAA
            "#,
        )
        .expect("'aa'");
}

#[test]
fn test_error_conditions() {
    // Test negative shift
    quiver()
        .evaluate("['ff', -1] ~> <binary_shift_left>")
        .expect_runtime_error(quiver::vm::Error::InvalidArgument(
            "Shift amount cannot be negative".to_string(),
        ));

    // Test bit index out of bounds
    quiver()
        .evaluate("['ff', 100] ~> <binary_get_bit_pos>")
        .expect_runtime_error(quiver::vm::Error::InvalidArgument(
            "Bit index 100 out of bounds for binary with 8 bits".to_string(),
        ));

    // Test invalid bit value for set_bit
    quiver()
        .evaluate("['ff', 0, 2] ~> <binary_set_bit>")
        .expect_runtime_error(quiver::vm::Error::InvalidArgument(
            "Bit value must be 0 or 1".to_string(),
        ));
}

#[test]
fn test_hamt_simulation() {
    // Test a realistic HAMT operation sequence
    quiver()
        .evaluate(
            r#"
            // Simulate HAMT bitmap operations
            bitmap = 8 ~> <binary_new>,  // 8-byte bitmap

            // Set bits at positions that would represent hash collisions
            step1 = [bitmap, 5, 1] ~> <binary_set_bit>,   // Set bit 5
            step2 = [step1, 13, 1] ~> <binary_set_bit>,   // Set bit 13
            step3 = [step2, 21, 1] ~> <binary_set_bit>,   // Set bit 21

            // Count how many slots are occupied
            occupied_count = step3 ~> <binary_popcount>,

            // Extract a 5-bit chunk (like HAMT does for navigation)
            shifted = [step3, 3] ~> <binary_shift_right>,  // Shift right by 3
            mask = 4 ~> <binary_new>,  // Create mask binary
            mask_with_bits = [mask, 0, 1] ~> <binary_set_bit>,  // Set LSB
            chunk = [shifted, mask_with_bits] ~> <binary_and>,

            occupied_count
            "#,
        )
        .expect("3");
}
