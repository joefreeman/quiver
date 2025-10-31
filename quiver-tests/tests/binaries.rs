mod common;
use common::*;

#[test]
fn test_binary_literal_constant() {
    // Test that binary literals create constant references
    quiver().evaluate("'00ff'").expect("'00ff'");
    quiver().evaluate("'abcd'").expect("'abcd'");
}

#[test]
fn test_binary_equality_content() {
    // Test that binaries with same content are equal even if different references
    quiver()
        .evaluate("['68656c6c6f', '68656c6c6f'] ~> ==")
        .expect("'68656c6c6f'");
    quiver().evaluate("['abcd', 'abcd'] ~> ==").expect("'abcd'");
}

#[test]
fn test_binary_inequality_content() {
    // Test that binaries with different content are not equal
    quiver()
        .evaluate("['68656c6c6f', '776f726c64'] ~> ==")
        .expect("[]");
    quiver()
        .evaluate("['61626364', '65666768'] ~> ==")
        .expect("[]");
}

#[test]
fn test_binary_new_builtin() {
    // Test creating new zero-filled binaries
    quiver()
        .evaluate("8 ~> __binary_new__")
        .expect("'0000000000000000'");
    quiver().evaluate("0 ~> __binary_new__").expect("''");
}

#[test]
fn test_binary_new_size_validation() {
    // Test negative size validation
    quiver()
        .evaluate("-1 ~> __binary_new__")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Size cannot be negative".to_string(),
        ));
}

#[test]
fn test_binary_length_builtin() {
    // Test getting length of binaries
    quiver()
        .evaluate("'68656c6c6f' ~> __binary_length__")
        .expect("5");
    quiver().evaluate("'4142' ~> __binary_length__").expect("2");
    quiver()
        .evaluate("10 ~> __binary_new__ ~> __binary_length__")
        .expect("10");
}

#[test]
fn test_binary_get_byte_builtin() {
    // Test getting bytes from binaries using binary_get with 1 byte (8 bits)
    quiver()
        .evaluate("['68656c6c6f', 0, 0, 8] ~> __binary_get__")
        .expect("104"); // 0x68 = 104
    quiver()
        .evaluate("['68656c6c6f', 1, 0, 8] ~> __binary_get__")
        .expect("101"); // 0x65 = 101
    quiver()
        .evaluate("['4142', 0, 0, 8] ~> __binary_get__")
        .expect("65"); // 0x41 = 65
    quiver()
        .evaluate("['4142', 1, 0, 8] ~> __binary_get__")
        .expect("66"); // 0x42 = 66
}

#[test]
fn test_binary_get_byte_bounds() {
    // Test bounds checking
    quiver()
        .evaluate("['68656c6c6f', 5, 0, 8] ~> __binary_get__")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Not enough bits: need 8 bits starting at byte 5 bit 0".to_string(),
        ));
    quiver()
        .evaluate("['68656c6c6f', -1, 0, 8] ~> __binary_get__")
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Byte offset cannot be negative".to_string(),
        ));
}

#[test]
fn test_binary_concat_builtin() {
    // Test concatenating binaries
    quiver()
        .evaluate("['68656c6c', '20776f'] ~> __binary_concat__")
        .expect("'68656c6c20776f'");
    quiver()
        .evaluate("['6162', '6364'] ~> __binary_concat__")
        .expect("'61626364'");
}

#[test]
fn test_binary_concat_then_length() {
    // Test that concatenation produces correct length
    quiver()
        .evaluate("['68656c6c6f', '20776f726c64'] ~> __binary_concat__ ~> __binary_length__")
        .expect("11");
    quiver()
        .evaluate("['61', '62'] ~> __binary_concat__ ~> __binary_length__")
        .expect("2");
}

#[test]
fn test_binary_operations_chain() {
    // Test chaining multiple binary operations
    quiver()
        .evaluate(
            r#"
            hello = '68656c6c6f'
            world = '20776f726c64'
            combined = [hello, world] ~> __binary_concat__
            combined ~> __binary_length__
            "#,
        )
        .expect("11");
}

#[test]
fn test_empty_binary() {
    // Test zero-length binaries
    quiver()
        .evaluate("0 ~> __binary_new__ ~> __binary_length__")
        .expect("0");
    quiver().evaluate("'' ~> __binary_length__").expect("0");
}

#[test]
fn test_heap_vs_constant_equality() {
    // Test that heap and constant binaries compare correctly
    quiver()
        .evaluate(
            r#"
            heap_bin = 5 ~> __binary_new__
            const_bin = 5 ~> __binary_new__
            [heap_bin, const_bin] ~> ==
            "#,
        )
        .expect("'0000000000'");
}
