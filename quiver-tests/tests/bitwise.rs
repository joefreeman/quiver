mod common;
use common::*;

#[test]
fn test_binary_and() {
    // Test basic AND operation: 0xFF & 0xF0 = 0xF0
    quiver()
        .evaluate("['ff', 'f0'] ~> __binary_and__")
        .expect("'f0'");

    // Test with known values: 0xAA & 0xCC = 0x88
    quiver()
        .evaluate("['aa', 'cc'] ~> __binary_and__")
        .expect("'88'");

    // Test multi-byte AND
    quiver()
        .evaluate("['ff00', '0ff0'] ~> __binary_and__")
        .expect("'0f00'");
}

#[test]
fn test_binary_or() {
    // Test basic OR operation: 0xF0 | 0x0F = 0xFF
    quiver()
        .evaluate("['f0', '0f'] ~> __binary_or__")
        .expect("'ff'");

    // Test with known values: 0xAA | 0x55 = 0xFF
    quiver()
        .evaluate("['aa', '55'] ~> __binary_or__")
        .expect("'ff'");

    // Test multi-byte OR
    quiver()
        .evaluate("['f000', '00f0'] ~> __binary_or__")
        .expect("'f0f0'");
}

#[test]
fn test_binary_xor() {
    // Test basic XOR operation: 0xFF ^ 0xF0 = 0x0F
    quiver()
        .evaluate("['ff', 'f0'] ~> __binary_xor__")
        .expect("'0f'");

    // Test XOR with itself gives zeros
    quiver()
        .evaluate("['aa', 'aa'] ~> __binary_xor__")
        .expect("'00'");

    // Test multi-byte XOR
    quiver()
        .evaluate("['ff00', '00ff'] ~> __binary_xor__")
        .expect("'ffff'");
}

#[test]
fn test_binary_not() {
    // Test NOT operation: ~0x00 = 0xFF
    quiver().evaluate("'00' ~> __binary_not__").expect("'ff'");

    // Test NOT of 0xFF = 0x00
    quiver().evaluate("'ff' ~> __binary_not__").expect("'00'");

    // Test multi-byte NOT
    quiver().evaluate("'f00f' ~> __binary_not__").expect("'0ff0'");
}

#[test]
fn test_binary_shift_left() {
    // Test left shift by 1: 0x01 << 1 = 0x02
    quiver()
        .evaluate("['01', 1] ~> __binary_shift_left__")
        .expect("'02'");

    // Test left shift by 4: 0x0F << 4 = 0xF0
    quiver()
        .evaluate("['0f', 4] ~> __binary_shift_left__")
        .expect("'f0'");

    // Test multi-byte shift: 0x0001 << 8 = 0x0100
    quiver()
        .evaluate("['0001', 8] ~> __binary_shift_left__")
        .expect("'0100'");
}

#[test]
fn test_binary_shift_right() {
    // Test right shift by 1: 0x02 >> 1 = 0x01
    quiver()
        .evaluate("['02', 1] ~> __binary_shift_right__")
        .expect("'01'");

    // Test right shift by 4: 0xF0 >> 4 = 0x0F
    quiver()
        .evaluate("['f0', 4] ~> __binary_shift_right__")
        .expect("'0f'");

    // Test multi-byte shift: 0x0100 >> 8 = 0x0001
    quiver()
        .evaluate("['0100', 8] ~> __binary_shift_right__")
        .expect("'0001'");
}

#[test]
fn test_binary_popcount_critical() {
    // Test popcount - CRITICAL for HAMT operations

    // Empty binary should have 0 bits set
    quiver().evaluate("'' ~> __binary_popcount__").expect("0");

    // Single null byte should have 0 bits set
    quiver().evaluate("'00' ~> __binary_popcount__").expect("0");

    // Test known popcount values
    // 0xFF has 8 bits set
    quiver().evaluate("'ff' ~> __binary_popcount__").expect("8");

    // 0x0F has 4 bits set
    quiver().evaluate("'0f' ~> __binary_popcount__").expect("4");

    // 0x55 = 01010101 has 4 bits set
    quiver().evaluate("'55' ~> __binary_popcount__").expect("4");
}

#[test]
fn test_binary_get_bit_pos() {
    // Test getting specific bits from 0x80 = 10000000
    quiver()
        .evaluate("['80', 0] ~> __binary_get_bit_pos__")
        .expect("1"); // MSB is set
    quiver()
        .evaluate("['80', 7] ~> __binary_get_bit_pos__")
        .expect("0"); // LSB is not set

    // Test with 0xFF = 11111111 (all bits set)
    quiver()
        .evaluate("['ff', 3] ~> __binary_get_bit_pos__")
        .expect("1"); // Bit 3 is set
}

#[test]
fn test_binary_set_bit() {
    // Test setting bit 0 in 0x00 to get 0x80
    quiver()
        .evaluate("['00', 0, 1] ~> __binary_set_bit__")
        .expect("'80'");

    // Test clearing bit 0 in 0xFF to get 0x7F
    quiver()
        .evaluate("['ff', 0, 0] ~> __binary_set_bit__")
        .expect("'7f'");

    // Test setting multiple bits
    quiver()
        .evaluate(
            r#"
            start = '00'
            bit0_set = [start, 0, 1] ~> __binary_set_bit__
            [bit0_set, 7, 1] ~> __binary_set_bit__
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
            empty = 4 ~> __binary_new__
            with_bit0 = [empty, 0, 1] ~> __binary_set_bit__
            with_bit5 = [with_bit0, 5, 1] ~> __binary_set_bit__
            with_bit10 = [with_bit5, 10, 1] ~> __binary_set_bit__
            with_bit10 ~> __binary_popcount__
            "#,
        )
        .expect("3");
}

#[test]
fn test_shift_operations_boundary() {
    // Test shift operations with edge cases

    // Shift by 0 should be identity
    quiver()
        .evaluate("['ff', 0] ~> __binary_shift_left__")
        .expect("'ff'");

    // Large shift should result in zeros
    quiver()
        .evaluate("['ff', 100] ~> __binary_shift_left__")
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
            xor_result = [a, b] ~> __binary_xor__,  // Should be 0xFF
            [xor_result, a] ~> __binary_and__  // 0xFF & 0xAA = 0xAA
            "#,
        )
        .expect("'aa'");
}

#[test]
fn test_error_conditions() {
    // Test negative shift
    quiver()
        .evaluate("['ff', -1] ~> __binary_shift_left__")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Shift amount cannot be negative".to_string(),
        ));

    // Test bit index out of bounds
    quiver()
        .evaluate("['ff', 100] ~> __binary_get_bit_pos__")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Bit index 100 out of bounds for binary with 8 bits".to_string(),
        ));

    // Test invalid bit value for set_bit
    quiver()
        .evaluate("['ff', 0, 2] ~> __binary_set_bit__")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
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
            bitmap = 8 ~> __binary_new__,  // 8-byte bitmap

            // Set bits at positions that would represent hash collisions
            step1 = [bitmap, 5, 1] ~> __binary_set_bit__,   // Set bit 5
            step2 = [step1, 13, 1] ~> __binary_set_bit__,   // Set bit 13
            step3 = [step2, 21, 1] ~> __binary_set_bit__,   // Set bit 21

            // Count how many slots are occupied
            occupied_count = step3 ~> __binary_popcount__,

            // Extract a 5-bit chunk (like HAMT does for navigation)
            shifted = [step3, 3] ~> __binary_shift_right__,  // Shift right by 3
            mask = 4 ~> __binary_new__,  // Create mask binary
            mask_with_bits = [mask, 0, 1] ~> __binary_set_bit__,  // Set LSB
            chunk = [shifted, mask_with_bits] ~> __binary_and__,

            occupied_count
            "#,
        )
        .expect("3");
}
