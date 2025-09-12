mod common;
use common::*;

#[test]
fn test_binary_literal_constant() {
    // Test that binary literals create constant references
    quiver().evaluate("'00ff'").expect_binary(&[0x00, 0xff]);
    quiver().evaluate("'abcd'").expect_binary(&[0xab, 0xcd]);
}

#[test]
fn test_string_literal_constant() {
    // Test that string literals create constant references
    quiver().evaluate("\"hello\"").expect_binary(b"hello");
}

#[test]
fn test_binary_equality_content() {
    // Test that binaries with same content are equal even if different references
    quiver()
        .evaluate("[\"hello\", \"hello\"] ~> ==")
        .expect_binary(b"hello");
    quiver()
        .evaluate("['abcd', 'abcd'] ~> ==")
        .expect_binary(&[0xab, 0xcd]);
}

#[test]
fn test_binary_inequality_content() {
    // Test that binaries with different content are not equal
    quiver()
        .evaluate("[\"hello\", \"world\"] ~> ==")
        .expect_nil();
    quiver().evaluate("[\"abcd\", \"efgh\"] ~> ==").expect_nil();
}

#[test]
fn test_binary_new_builtin() {
    // Test creating new zero-filled binaries
    quiver()
        .evaluate("10 ~> <binary_new>")
        .expect_binary(&[0; 10]);
    quiver().evaluate("0 ~> <binary_new>").expect_binary(&[]);
}

#[test]
fn test_binary_new_size_validation() {
    // Test negative size validation
    quiver()
        .evaluate("-1 ~> <binary_new>")
        .expect_runtime_error(quiver::vm::Error::InvalidArgument(
            "Size cannot be negative".to_string(),
        ));
}

#[test]
fn test_binary_length_builtin() {
    // Test getting length of binaries
    quiver()
        .evaluate("\"hello\" ~> <binary_length>")
        .expect_int(5);
    quiver().evaluate("\"AB\" ~> <binary_length>").expect_int(2); // 2 characters = 2 bytes
    quiver()
        .evaluate("10 ~> <binary_new> ~> <binary_length>")
        .expect_int(10);
}

#[test]
fn test_binary_get_byte_builtin() {
    // Test getting bytes from binaries
    quiver()
        .evaluate("[\"hello\", 0] ~> <binary_get_byte>")
        .expect_int(104); // 'h' = 104
    quiver()
        .evaluate("[\"hello\", 1] ~> <binary_get_byte>")
        .expect_int(101); // 'e' = 101
    // Use regular ASCII characters
    quiver()
        .evaluate("[\"AB\", 0] ~> <binary_get_byte>")
        .expect_int(65); // 'A' = 65
    quiver()
        .evaluate("[\"AB\", 1] ~> <binary_get_byte>")
        .expect_int(66); // 'B' = 66
}

#[test]
fn test_binary_get_byte_bounds() {
    // Test bounds checking
    quiver()
        .evaluate("[\"hello\", 5] ~> <binary_get_byte>")
        .expect_runtime_error(quiver::vm::Error::InvalidArgument(
            "Index 5 out of bounds for binary of length 5".to_string(),
        ));
    quiver()
        .evaluate("[\"hello\", -1] ~> <binary_get_byte>")
        .expect_runtime_error(quiver::vm::Error::InvalidArgument(
            "Index cannot be negative".to_string(),
        ));
}

#[test]
fn test_binary_concat_builtin() {
    // Test concatenating binaries
    quiver()
        .evaluate("[\"hello\", \" world\"] ~> <binary_concat>")
        .expect_binary(b"hello world");
    quiver()
        .evaluate("[\"ab\", \"cd\"] ~> <binary_concat>")
        .expect_binary(b"abcd");
}

#[test]
fn test_binary_concat_then_length() {
    // Test that concatenation produces correct length
    quiver()
        .evaluate("[\"hello\", \" world\"] ~> <binary_concat> ~> <binary_length>")
        .expect_int(11);
    quiver()
        .evaluate("[\"a\", \"b\"] ~> <binary_concat> ~> <binary_length>")
        .expect_int(2);
}

#[test]
fn test_binary_operations_chain() {
    // Test chaining multiple binary operations
    quiver()
        .evaluate(
            r#"
            hello = "hello",
            world = " world",
            combined = [hello, world] ~> <binary_concat>,
            combined ~> <binary_length>
            "#,
        )
        .expect_int(11);
}

#[test]
fn test_empty_binary() {
    // Test zero-length binaries
    quiver()
        .evaluate("0 ~> <binary_new> ~> <binary_length>")
        .expect_int(0);
    quiver().evaluate("\"\" ~> <binary_length>").expect_int(0);
}

#[test]
fn test_binary_print() {
    // Test that binaries can be printed (should work with existing IO)
    quiver().evaluate("\"hello\" ~> <print>").expect_ok();
    quiver().evaluate("\"world\" ~> <println>").expect_ok();
}

#[test]
fn test_heap_vs_constant_equality() {
    // Test that heap and constant binaries compare correctly
    quiver()
        .evaluate(
            r#"
            heap_bin = 5 ~> <binary_new>,
            const_bin = 5 ~> <binary_new>,
            [heap_bin, const_bin] ~> ==
            "#,
        )
        .expect_binary(&[0; 5]);
}
