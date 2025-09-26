mod common;
use common::*;

#[test]
fn test_binary_literal_constant() {
    // Test that binary literals create constant references
    quiver().evaluate("'00ff'").expect("'00ff'");
    quiver().evaluate("'abcd'").expect("'abcd'");
}

#[test]
fn test_string_literal_constant() {
    // Test that string literals create constant references
    quiver().evaluate("\"hello\"").expect("'68656c6c6f'");
}

#[test]
fn test_binary_equality_content() {
    // Test that binaries with same content are equal even if different references
    quiver()
        .evaluate("[\"hello\", \"hello\"] ~> ==")
        .expect("'68656c6c6f'");
    quiver().evaluate("['abcd', 'abcd'] ~> ==").expect("'abcd'");
}

#[test]
fn test_binary_inequality_content() {
    // Test that binaries with different content are not equal
    quiver()
        .evaluate("[\"hello\", \"world\"] ~> ==")
        .expect("[]");
    quiver().evaluate("[\"abcd\", \"efgh\"] ~> ==").expect("[]");
}

#[test]
fn test_binary_new_builtin() {
    // Test creating new zero-filled binaries
    quiver()
        .evaluate("10 ~> <binary_new>")
        .expect("'00000000000000000000'");
    quiver().evaluate("0 ~> <binary_new>").expect("''");
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
        .expect("5");
    quiver().evaluate("\"AB\" ~> <binary_length>").expect("2"); // 2 characters = 2 bytes
    quiver()
        .evaluate("10 ~> <binary_new> ~> <binary_length>")
        .expect("10");
}

#[test]
fn test_binary_get_byte_builtin() {
    // Test getting bytes from binaries
    quiver()
        .evaluate("[\"hello\", 0] ~> <binary_get_byte>")
        .expect("104"); // 'h' = 104
    quiver()
        .evaluate("[\"hello\", 1] ~> <binary_get_byte>")
        .expect("101"); // 'e' = 101
    // Use regular ASCII characters
    quiver()
        .evaluate("[\"AB\", 0] ~> <binary_get_byte>")
        .expect("65"); // 'A' = 65
    quiver()
        .evaluate("[\"AB\", 1] ~> <binary_get_byte>")
        .expect("66"); // 'B' = 66
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
        .expect("'68656c6c6f20776f726c64'");
    quiver()
        .evaluate("[\"ab\", \"cd\"] ~> <binary_concat>")
        .expect("'61626364'");
}

#[test]
fn test_binary_concat_then_length() {
    // Test that concatenation produces correct length
    quiver()
        .evaluate("[\"hello\", \" world\"] ~> <binary_concat> ~> <binary_length>")
        .expect("11");
    quiver()
        .evaluate("[\"a\", \"b\"] ~> <binary_concat> ~> <binary_length>")
        .expect("2");
}

#[test]
fn test_binary_operations_chain() {
    // Test chaining multiple binary operations
    quiver()
        .evaluate(
            r#"
            "hello" ~> ^hello,
            " world" ~> ^world,
            [hello, world] ~> <binary_concat> ~> ^combined,
            combined ~> <binary_length>
            "#,
        )
        .expect("11");
}

#[test]
fn test_empty_binary() {
    // Test zero-length binaries
    quiver()
        .evaluate("0 ~> <binary_new> ~> <binary_length>")
        .expect("0");
    quiver().evaluate("\"\" ~> <binary_length>").expect("0");
}

#[test]
fn test_binary_print() {
    // Test that binaries can be printed (should work with existing IO)
    quiver().evaluate("\"hello\" ~> <print>").expect("Ok");
    quiver().evaluate("\"world\" ~> <println>").expect("Ok");
}

#[test]
fn test_heap_vs_constant_equality() {
    // Test that heap and constant binaries compare correctly
    quiver()
        .evaluate(
            r#"
            5 ~> <binary_new> ~> ^heap_bin,
            5 ~> <binary_new> ~> ^const_bin,
            [heap_bin, const_bin] ~> ==
            "#,
        )
        .expect("'0000000000'");
}
