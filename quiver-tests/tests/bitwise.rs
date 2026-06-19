mod common;
use common::*;

#[test]
fn test_binary_and() {
    // Test basic AND operation: 0xFF & 0xF0 = 0xF0
    quiver()
        .evaluate("[0xff, 0xf0] ~> __binary_and__")
        .expect("0xf0");

    // Test with known values: 0xAA & 0xCC = 0x88
    quiver()
        .evaluate("[0xaa, 0xcc] ~> __binary_and__")
        .expect("0x88");

    // Test multi-byte AND
    quiver()
        .evaluate("[0xff00, 0x0ff0] ~> __binary_and__")
        .expect("0x0f00");
}

#[test]
fn test_binary_or() {
    // Test basic OR operation: 0xF0 | 0x0F = 0xFF
    quiver()
        .evaluate("[0xf0, 0x0f] ~> __binary_or__")
        .expect("0xff");

    // Test with known values: 0xAA | 0x55 = 0xFF
    quiver()
        .evaluate("[0xaa, 0x55] ~> __binary_or__")
        .expect("0xff");

    // Test multi-byte OR
    quiver()
        .evaluate("[0xf000, 0x00f0] ~> __binary_or__")
        .expect("0xf0f0");
}

#[test]
fn test_binary_xor() {
    // Test basic XOR operation: 0xFF ^ 0xF0 = 0x0F
    quiver()
        .evaluate("[0xff, 0xf0] ~> __binary_xor__")
        .expect("0x0f");

    // Test XOR with itself gives zeros
    quiver()
        .evaluate("[0xaa, 0xaa] ~> __binary_xor__")
        .expect("0x00");

    // Test multi-byte XOR
    quiver()
        .evaluate("[0xff00, 0x00ff] ~> __binary_xor__")
        .expect("0xffff");
}

#[test]
fn test_binary_not() {
    // Test NOT operation: ~0x00 = 0xFF
    quiver().evaluate("0x00 ~> __binary_not__").expect("0xff");

    // Test NOT of 0xFF = 0x00
    quiver().evaluate("0xff ~> __binary_not__").expect("0x00");

    // Test multi-byte NOT
    quiver()
        .evaluate("0xf00f ~> __binary_not__")
        .expect("0x0ff0");
}

#[test]
fn test_binary_shift_left() {
    // Test left shift by 1: 0x01 << 1 = 0x02 (positive = left)
    quiver()
        .evaluate("[0x01, 1] ~> __binary_shift__")
        .expect("0x02");

    // Test left shift by 4: 0x0F << 4 = 0xF0
    quiver()
        .evaluate("[0x0f, 4] ~> __binary_shift__")
        .expect("0xf0");

    // Test multi-byte shift: 0x0001 << 8 = 0x0100
    quiver()
        .evaluate("[0x0001, 8] ~> __binary_shift__")
        .expect("0x0100");
}

#[test]
fn test_binary_shift_right() {
    // Test right shift by 1: 0x02 >> 1 = 0x01 (negative = right)
    quiver()
        .evaluate("[0x02, -1] ~> __binary_shift__")
        .expect("0x01");

    // Test right shift by 4: 0xF0 >> 4 = 0x0F
    quiver()
        .evaluate("[0xf0, -4] ~> __binary_shift__")
        .expect("0x0f");

    // Test multi-byte shift: 0x0100 >> 8 = 0x0001
    quiver()
        .evaluate("[0x0100, -8] ~> __binary_shift__")
        .expect("0x0001");
}

#[test]
fn test_binary_popcount_critical() {
    // Test popcount - CRITICAL for HAMT operations

    // Empty binary should have 0 bits set
    quiver().evaluate("0x ~> __binary_popcount__").expect("0");

    // Single null byte should have 0 bits set
    quiver().evaluate("0x00 ~> __binary_popcount__").expect("0");

    // Test known popcount values
    // 0xFF has 8 bits set
    quiver().evaluate("0xff ~> __binary_popcount__").expect("8");

    // 0x0F has 4 bits set
    quiver().evaluate("0x0f ~> __binary_popcount__").expect("4");

    // 0x55 = 01010101 has 4 bits set
    quiver().evaluate("0x55 ~> __binary_popcount__").expect("4");
}

#[test]
fn test_binary_get_bit_pos() {
    // Test getting specific bits from 0x80 = 10000000
    quiver()
        .evaluate("[0x80, 0, 0, 1] ~> __binary_get__")
        .expect("1"); // MSB is set
    quiver()
        .evaluate("[0x80, 0, 7, 1] ~> __binary_get__")
        .expect("0"); // LSB is not set

    // Test with 0xFF = 11111111 (all bits set)
    quiver()
        .evaluate("[0xff, 0, 3, 1] ~> __binary_get__")
        .expect("1"); // Bit 3 is set
}

#[test]
fn test_binary_set_bit() {
    // Test setting bit 0 in 0x00 to get 0x80
    quiver()
        .evaluate("[0x00, 0, 0, 1, 1] ~> __binary_set__")
        .expect("0x80");

    // Test clearing bit 0 in 0xFF to get 0x7F
    quiver()
        .evaluate("[0xff, 0, 0, 0, 1] ~> __binary_set__")
        .expect("0x7f");

    // Test setting multiple bits
    quiver()
        .evaluate(
            r#"
            start = 0x00
            bit0_set = [start, 0, 0, 1, 1] ~> __binary_set__
            [bit0_set, 0, 7, 1, 1] ~> __binary_set__
            "#,
        )
        .expect("0x81"); // 10000001
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
            with_bit0 = [empty, 0, 0, 1, 1] ~> __binary_set__
            with_bit5 = [with_bit0, 0, 5, 1, 1] ~> __binary_set__
            with_bit10 = [with_bit5, 1, 2, 1, 1] ~> __binary_set__
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
        .evaluate("[0xff, 0] ~> __binary_shift__")
        .expect("0xff");

    // Large shift should result in zeros
    quiver()
        .evaluate("[0xff, 100] ~> __binary_shift__")
        .expect("0x00");
}

#[test]
fn test_bitwise_chaining() {
    // Test chaining multiple bitwise operations (important for HAMT)
    quiver()
        .evaluate(
            r#"
            a = 0xaa,  // 10101010
            b = 0x55,  // 01010101
            // XOR then AND
            xor_result = [a, b] ~> __binary_xor__,  // Should be 0xFF
            [xor_result, a] ~> __binary_and__  // 0xFF & 0xAA = 0xAA
            "#,
        )
        .expect("0xaa");
}

#[test]
fn test_error_conditions() {
    // Test bit index out of bounds
    quiver()
        .evaluate("[0xff, 12, 4, 1] ~> __binary_get__")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Not enough bits: need 1 bits starting at byte 12 bit 4".to_string(),
        ));

    // Test invalid bit offset
    quiver()
        .evaluate("[0xff, 0, 8, 1] ~> __binary_get__")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Bit offset must be 0-7".to_string(),
        ));
}

#[test]
fn test_bit_unaligned_access() {
    // Test reading and writing bits that span byte boundaries

    // Create binary: 0xAB = 10101011
    // Read 4 bits starting at bit 2: should get bits 2-5 = 1010 = 10
    quiver()
        .evaluate("[0xab, 0, 2, 4] ~> __binary_get__")
        .expect("10"); // 1010 in binary = 10 in decimal

    // Test writing across byte boundaries
    // Start with 0x0000, write 12 bits starting at byte 0 bit 4
    // Writing 0xABC (101010111100 in binary)
    quiver()
        .evaluate(
            r#"
            start = 0x0000
            result = [start, 0, 4, 2748, 12] ~> __binary_set__
            result
            "#,
        )
        .expect("0x0abc"); // Should be 0x0ABC when aligned to bit 4

    // Test reading multi-byte values starting at odd bit positions
    // 0xFFFF = 1111111111111111
    // Read 8 bits starting at byte 0 bit 4: should get 11111111 = 255
    quiver()
        .evaluate("[0xffff, 0, 4, 8] ~> __binary_get__")
        .expect("255");
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
            step1 = [bitmap, 0, 5, 1, 1] ~> __binary_set__,   // Set bit 5
            step2 = [step1, 1, 5, 1, 1] ~> __binary_set__,   // Set bit 13
            step3 = [step2, 2, 5, 1, 1] ~> __binary_set__,   // Set bit 21

            // Count how many slots are occupied
            occupied_count = step3 ~> __binary_popcount__,

            // Extract a 5-bit chunk (like HAMT does for navigation)
            shifted = [step3, -3] ~> __binary_shift__,  // Shift right by 3 (negative = right)
            mask = 4 ~> __binary_new__,  // Create mask binary
            mask_with_bits = [mask, 0, 0, 1, 1] ~> __binary_set__,  // Set LSB
            chunk = [shifted, mask_with_bits] ~> __binary_and__,

            occupied_count
            "#,
        )
        .expect("3");
}
