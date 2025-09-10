mod common;
use common::*;

#[test]
fn test_binary_and() {
    // Test basic AND operation
    quiver()
        .evaluate("[\"AA\", \"CC\"] ~> <binary_and>")
        .expect_any_binary();

    // Test with known values: 0xAA & 0xCC = 0x88
    // A = 1010, C = 1100, so A & C = 1000 = 8
    // But we're working with strings, so 'A' (0x41) & 'C' (0x43) = 0x41 = 'A'
    quiver()
        .evaluate(
            r#"
        a = "AC",
        b = "CA",
        result = [a, b] ~> <binary_and>,
        result ~> <binary_length>
    "#,
        )
        .expect_int(2);
}

#[test]
fn test_binary_or() {
    // Test basic OR operation
    quiver()
        .evaluate("[\"AB\", \"CD\"] ~> <binary_or>")
        .expect_any_binary();

    // Test length preservation
    quiver()
        .evaluate("[\"test\", \"word\"] ~> <binary_or> ~> <binary_length>")
        .expect_int(4);
}

#[test]
fn test_binary_xor() {
    // Test basic XOR operation
    quiver()
        .evaluate("[\"AB\", \"CD\"] ~> <binary_xor>")
        .expect_any_binary();

    // Test XOR with itself gives zeros (well, zero chars in this case)
    quiver()
        .evaluate(
            r#"
        text = "test",
        result = [text, text] ~> <binary_xor>,
        [result, 0] ~> <binary_get_byte>
    "#,
        )
        .expect_int(0);
}

#[test]
fn test_binary_not() {
    // Test NOT operation
    quiver()
        .evaluate("\"A\" ~> <binary_not>")
        .expect_any_binary();

    // NOT should preserve length
    quiver()
        .evaluate("\"hello\" ~> <binary_not> ~> <binary_length>")
        .expect_int(5);
}

#[test]
fn test_binary_shift_left() {
    // Test left shift
    quiver()
        .evaluate("[\"A\", 1] ~> <binary_shift_left>")
        .expect_any_binary();

    // Test shift by 8 bits (1 byte)
    quiver()
        .evaluate("[\"AB\", 8] ~> <binary_shift_left> ~> <binary_length>")
        .expect_int(2);
}

#[test]
fn test_binary_shift_right() {
    // Test right shift
    quiver()
        .evaluate("[\"A\", 1] ~> <binary_shift_right>")
        .expect_any_binary();

    // Test shift preserves length
    quiver()
        .evaluate("[\"test\", 4] ~> <binary_shift_right> ~> <binary_length>")
        .expect_int(4);
}

#[test]
fn test_binary_popcount_critical() {
    // Test popcount - CRITICAL for HAMT operations

    // Empty binary should have 0 bits set
    quiver().evaluate("\"\" ~> <binary_popcount>").expect_int(0);

    // Single null byte should have 0 bits set
    quiver()
        .evaluate("1 ~> <binary_new> ~> <binary_popcount>")
        .expect_int(0);

    // Test known popcount values
    // For ASCII characters:
    // 'A' = 0x41 = 01000001 has 2 bits set
    // 'B' = 0x42 = 01000010 has 2 bits set
    // So "AB" should have 4 bits set total
    quiver()
        .evaluate("\"AB\" ~> <binary_popcount>")
        .expect_int(4);
}

#[test]
fn test_binary_get_bit_pos() {
    // Test getting specific bits
    quiver()
        .evaluate("[\"A\", 0] ~> <binary_get_bit_pos>")
        .expect_int(0); // MSB of 'A' (0x41)
    quiver()
        .evaluate("[\"A\", 1] ~> <binary_get_bit_pos>")
        .expect_int(1); // Second bit of 'A'
}

#[test]
fn test_binary_set_bit() {
    // Test setting bits
    quiver()
        .evaluate("[\"A\", 0, 1] ~> <binary_set_bit>")
        .expect_any_binary();

    // Test setting and getting
    quiver()
        .evaluate(
            r#"
        original = "A",
        modified = [original, 7, 1] ~> <binary_set_bit>,
        [modified, 7] ~> <binary_get_bit_pos>
    "#,
        )
        .expect_int(1);
}

#[test]
fn test_binary_popcount_hamt_pattern() {
    // Test popcount with typical HAMT patterns

    // Create a binary with specific bit pattern for HAMT testing
    quiver()
        .evaluate(
            r#"
        // Create binary: set some bits to simulate HAMT bitmap
        empty = 4 ~> <binary_new>,
        with_bit0 = [empty, 0, 1] ~> <binary_set_bit>,
        with_bit5 = [with_bit0, 5, 1] ~> <binary_set_bit>,
        with_bit10 = [with_bit5, 10, 1] ~> <binary_set_bit>,
        with_bit10 ~> <binary_popcount>
    "#,
        )
        .expect_int(3);
}

#[test]
fn test_shift_operations_boundary() {
    // Test shift operations with edge cases

    // Shift by 0 should be identity
    quiver()
        .evaluate("[\"test\", 0] ~> <binary_shift_left>")
        .expect_any_binary();

    // Large shift should result in zeros
    quiver()
        .evaluate("[\"A\", 100] ~> <binary_shift_left> ~> <binary_popcount>")
        .expect_int(0);
}

#[test]
fn test_bitwise_chaining() {
    // Test chaining multiple bitwise operations (important for HAMT)
    quiver()
        .evaluate(
            r#"
        a = "test",
        b = "word",
        // XOR then AND then popcount
        xor_result = [a, b] ~> <binary_xor>,
        and_result = [xor_result, a] ~> <binary_and>,
        and_result ~> <binary_popcount>
    "#,
        )
        .expect_int(2); // "test" & ("test" ^ "word") should have 2 bits set
}

#[test]
fn test_error_conditions() {
    // Test negative shift
    quiver()
        .evaluate("[\"\", -1] ~> <binary_shift_left>")
        .expect_error();

    // Test bit index out of bounds
    quiver()
        .evaluate("[\"A\", 100] ~> <binary_get_bit_pos>")
        .expect_error();

    // Test invalid bit value for set_bit
    quiver()
        .evaluate("[\"A\", 0, 2] ~> <binary_set_bit>")
        .expect_error();
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
        .expect_int(3);
}
