use crate::common::*;

#[test]
fn test_binary_and() {
    // Test basic AND operation: <FF> & <F0> = <F0>
    quiver()
        .evaluate("[<ff>, <f0>] ~> __binary_and__ ~")
        .expect("<f0>");

    // Test with known values: <AA> & <CC> = <88>
    quiver()
        .evaluate("[<aa>, <cc>] ~> __binary_and__ ~")
        .expect("<88>");

    // Test multi-byte AND
    quiver()
        .evaluate("[<ff00>, <0ff0>] ~> __binary_and__ ~")
        .expect("<0f00>");
}

#[test]
fn test_binary_or() {
    // Test basic OR operation: <F0> | <0F> = <FF>
    quiver()
        .evaluate("[<f0>, <0f>] ~> __binary_or__ ~")
        .expect("<ff>");

    // Test with known values: <AA> | <55> = <FF>
    quiver()
        .evaluate("[<aa>, <55>] ~> __binary_or__ ~")
        .expect("<ff>");

    // Test multi-byte OR
    quiver()
        .evaluate("[<f000>, <00f0>] ~> __binary_or__ ~")
        .expect("<f0f0>");
}

#[test]
fn test_binary_xor() {
    // Test basic XOR operation: <FF> ^ <F0> = <0F>
    quiver()
        .evaluate("[<ff>, <f0>] ~> __binary_xor__ ~")
        .expect("<0f>");

    // Test XOR with itself gives zeros
    quiver()
        .evaluate("[<aa>, <aa>] ~> __binary_xor__ ~")
        .expect("<00>");

    // Test multi-byte XOR
    quiver()
        .evaluate("[<ff00>, <00ff>] ~> __binary_xor__ ~")
        .expect("<ffff>");
}

#[test]
fn test_binary_not() {
    // Test NOT operation: ~<00> = <FF>
    quiver().evaluate("<00> ~> __binary_not__ ~").expect("<ff>");

    // Test NOT of <FF> = <00>
    quiver().evaluate("<ff> ~> __binary_not__ ~").expect("<00>");

    // Test multi-byte NOT
    quiver()
        .evaluate("<f00f> ~> __binary_not__ ~")
        .expect("<0ff0>");
}

#[test]
fn test_binary_shift_left() {
    // Test left shift by 1: <01> << 1 = <02> (positive = left)
    quiver()
        .evaluate("[<01>, 1] ~> __binary_shift__ ~")
        .expect("<02>");

    // Test left shift by 4: <0F> << 4 = <F0>
    quiver()
        .evaluate("[<0f>, 4] ~> __binary_shift__ ~")
        .expect("<f0>");

    // Test multi-byte shift: <0001> << 8 = <0100>
    quiver()
        .evaluate("[<0001>, 8] ~> __binary_shift__ ~")
        .expect("<0100>");
}

#[test]
fn test_binary_shift_right() {
    // Test right shift by 1: <02> >> 1 = <01> (negative = right)
    quiver()
        .evaluate("[<02>, -1] ~> __binary_shift__ ~")
        .expect("<01>");

    // Test right shift by 4: <F0> >> 4 = <0F>
    quiver()
        .evaluate("[<f0>, -4] ~> __binary_shift__ ~")
        .expect("<0f>");

    // Test multi-byte shift: <0100> >> 8 = <0001>
    quiver()
        .evaluate("[<0100>, -8] ~> __binary_shift__ ~")
        .expect("<0001>");
}

#[test]
fn test_binary_popcount_critical() {
    // Test popcount - CRITICAL for HAMT operations

    // Empty binary should have 0 bits set
    quiver().evaluate("<> ~> __binary_popcount__ ~").expect("0");

    // Single null byte should have 0 bits set
    quiver()
        .evaluate("<00> ~> __binary_popcount__ ~")
        .expect("0");

    // Test known popcount values
    // <FF> has 8 bits set
    quiver()
        .evaluate("<ff> ~> __binary_popcount__ ~")
        .expect("8");

    // <0F> has 4 bits set
    quiver()
        .evaluate("<0f> ~> __binary_popcount__ ~")
        .expect("4");

    // <55> = 01010101 has 4 bits set
    quiver()
        .evaluate("<55> ~> __binary_popcount__ ~")
        .expect("4");
}

#[test]
fn test_binary_get_bit_pos() {
    // Test getting specific bits from <80> = 10000000
    quiver()
        .evaluate("[<80>, 0, 0, 1] ~> __binary_get__ ~")
        .expect("1"); // MSB is set
    quiver()
        .evaluate("[<80>, 0, 7, 1] ~> __binary_get__ ~")
        .expect("0"); // LSB is not set

    // Test with <FF> = 11111111 (all bits set)
    quiver()
        .evaluate("[<ff>, 0, 3, 1] ~> __binary_get__ ~")
        .expect("1"); // Bit 3 is set
}

#[test]
fn test_binary_set_bit() {
    // Test setting bit 0 in <00> to get <80>
    quiver()
        .evaluate("[<00>, 0, 0, 1, 1] ~> __binary_set__ ~")
        .expect("<80>");

    // Test clearing bit 0 in <FF> to get <7F>
    quiver()
        .evaluate("[<ff>, 0, 0, 0, 1] ~> __binary_set__ ~")
        .expect("<7f>");

    // Test setting multiple bits
    quiver()
        .evaluate(
            r#"
            start = <00>
            bit0_set = [start, 0, 0, 1, 1] ~> __binary_set__ ~
            [bit0_set, 0, 7, 1, 1] ~> __binary_set__ ~
            "#,
        )
        .expect("<81>"); // 10000001
}

#[test]
fn test_binary_popcount_hamt_pattern() {
    // Test popcount with typical HAMT patterns

    // Create a binary with specific bit pattern for HAMT testing
    quiver()
        .evaluate(
            r#"
            // Create binary: set some bits to simulate HAMT bitmap
            empty = 4 ~> __binary_new__ ~
            with_bit0 = [empty, 0, 0, 1, 1] ~> __binary_set__ ~
            with_bit5 = [with_bit0, 0, 5, 1, 1] ~> __binary_set__ ~
            with_bit10 = [with_bit5, 1, 2, 1, 1] ~> __binary_set__ ~
            with_bit10 ~> __binary_popcount__ ~
            "#,
        )
        .expect("3");
}

#[test]
fn test_shift_operations_boundary() {
    // Test shift operations with edge cases

    // Shift by 0 should be identity
    quiver()
        .evaluate("[<ff>, 0] ~> __binary_shift__ ~")
        .expect("<ff>");

    // Large shift should result in zeros
    quiver()
        .evaluate("[<ff>, 100] ~> __binary_shift__ ~")
        .expect("<00>");
}

#[test]
fn test_bitwise_chaining() {
    // Test chaining multiple bitwise operations (important for HAMT)
    quiver()
        .evaluate(
            r#"
            a = <aa>;  // 10101010
            b = <55>;  // 01010101
            // XOR then AND
            xor_result = [a, b] ~> __binary_xor__ ~;  // Should be <FF>
            [xor_result, a] ~> __binary_and__ ~  // <FF> & <AA> = <AA>
            "#,
        )
        .expect("<aa>");
}

#[test]
fn test_error_conditions() {
    // Test bit index out of bounds
    quiver()
        .evaluate("[<ff>, 12, 4, 1] ~> __binary_get__ ~")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Not enough bits: need 1 bits starting at byte 12 bit 4".to_string(),
        ));

    // Test invalid bit offset
    quiver()
        .evaluate("[<ff>, 0, 8, 1] ~> __binary_get__ ~")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Bit offset must be 0-7".to_string(),
        ));
}

#[test]
fn test_bit_unaligned_access() {
    // Test reading and writing bits that span byte boundaries

    // Create binary: <AB> = 10101011
    // Read 4 bits starting at bit 2: should get bits 2-5 = 1010 = 10
    quiver()
        .evaluate("[<ab>, 0, 2, 4] ~> __binary_get__ ~")
        .expect("10"); // 1010 in binary = 10 in decimal

    // Test writing across byte boundaries
    // Start with <0000>, write 12 bits starting at byte 0 bit 4
    // Writing <ABC> (101010111100 in binary)
    quiver()
        .evaluate(
            r#"
            start = <0000>
            result = [start, 0, 4, 2748, 12] ~> __binary_set__ ~
            result
            "#,
        )
        .expect("<0abc>"); // Should be <0ABC> when aligned to bit 4

    // Test reading multi-byte values starting at odd bit positions
    // <FFFF> = 1111111111111111
    // Read 8 bits starting at byte 0 bit 4: should get 11111111 = 255
    quiver()
        .evaluate("[<ffff>, 0, 4, 8] ~> __binary_get__ ~")
        .expect("255");
}

#[test]
fn test_hamt_simulation() {
    // Test a realistic HAMT operation sequence
    quiver()
        .evaluate(
            r#"
            // Simulate HAMT bitmap operations
            bitmap = 8 ~> __binary_new__ ~;  // 8-byte bitmap

            // Set bits at positions that would represent hash collisions
            step1 = [bitmap, 0, 5, 1, 1] ~> __binary_set__ ~;   // Set bit 5
            step2 = [step1, 1, 5, 1, 1] ~> __binary_set__ ~;   // Set bit 13
            step3 = [step2, 2, 5, 1, 1] ~> __binary_set__ ~;   // Set bit 21

            // Count how many slots are occupied
            occupied_count = step3 ~> __binary_popcount__ ~;

            // Extract a 5-bit chunk (like HAMT does for navigation)
            shifted = [step3, -3] ~> __binary_shift__ ~;  // Shift right by 3 (negative = right)
            mask = 4 ~> __binary_new__ ~;  // Create mask binary
            mask_with_bits = [mask, 0, 0, 1, 1] ~> __binary_set__ ~;  // Set LSB
            chunk = [shifted, mask_with_bits] ~> __binary_and__ ~;

            occupied_count
            "#,
        )
        .expect("3");
}
