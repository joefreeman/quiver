mod common;
use common::*;

#[test]
fn test_file_write_and_read() {
    // Create a temporary file path
    let temp_path = std::env::temp_dir().join(format!("quiver_test_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();

    // Write to file and read it back
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            // O_WRONLY | O_CREAT | O_TRUNC = 1 | 64 | 512 = 577
            // O_RDONLY = 0
            // Mode 0o644 = 420

            write_file = __file_open__["{}" ~> .0, 577, 420],
            __file_write__[write_file, 0, "Hello, World!" ~> .0],
            __file_close__[write_file],

            read_file = __file_open__["{}" ~> .0, 0, 0],
            data = __file_read__[read_file, 0, 4096],
            __file_close__[read_file],

            Str[data]
        "#,
            path_str, path_str
        ))
        .expect("\"Hello, World!\"");

    // Clean up
    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_file_append() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_test_append_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();

    // Write initial content, then append using explicit offset
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            // O_WRONLY | O_CREAT | O_TRUNC = 577
            write_file = __file_open__["{}" ~> .0, 577, 420],
            __file_write__[write_file, 0, "First line\n" ~> .0],
            __file_close__[write_file],

            // Write at offset 11 (length of "First line\n")
            append_file = __file_open__["{}" ~> .0, 1, 420],
            __file_write__[append_file, 11, "Second line\n" ~> .0],
            __file_close__[append_file],

            // Read everything
            read_file = __file_open__["{}" ~> .0, 0, 0],
            data = __file_read__[read_file, 0, 4096],
            __file_close__[read_file],

            Str[data]
        "#,
            path_str, path_str, path_str
        ))
        .expect("\"First line\\nSecond line\\n\"");

    // Clean up
    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_file_type_checking() {
    // Test that file operations have correct type signatures
    quiver()
        .with_io()
        .evaluate(
            r#"
            // Function that takes a file and returns data
            read_from_file = #\File {
                =f,
                data = __file_read__[f, 0, 1024],
                data
            },

            // Should type check
            []
        "#,
        )
        .expect("[]");
}

#[test]
fn test_file_resource_type() {
    quiver()
        .with_io()
        .evaluate(r#"__file_open__["/tmp/foo" ~> .0, 577, 420]"#)
        .expect_type("\\File");
}

#[test]
fn test_file_flush() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_test_flush_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();

    // Write data with explicit flush
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            file = __file_open__["{}" ~> .0, 577, 420],
            __file_write__[file, 0, "Flushed data" ~> .0],
            __file_flush__[file],
            __file_close__[file],
            Ok
        "#,
            path_str
        ))
        .expect("Ok");

    // Verify the data was written
    let content = std::fs::read_to_string(&temp_path).expect("File should exist");
    assert_eq!(content, "Flushed data");

    // Clean up
    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_multiple_writes() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_test_multi_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();

    // Multiple writes to same file at different offsets
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            file = __file_open__["{}" ~> .0, 577, 420],
            __file_write__[file, 0, "Line 1\n" ~> .0],
            __file_write__[file, 7, "Line 2\n" ~> .0],
            __file_write__[file, 14, "Line 3\n" ~> .0],
            __file_close__[file],

            read_file = __file_open__["{}" ~> .0, 0, 0],
            data = __file_read__[read_file, 0, 4096],
            __file_close__[read_file],

            Str[data]
        "#,
            path_str, path_str
        ))
        .expect("\"Line 1\\nLine 2\\nLine 3\\n\"");

    // Clean up
    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_read_from_closed_file() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_test_closed_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();

    // Write a file first
    std::fs::write(&temp_path, "test data").expect("Failed to create test file");

    // Open, close, then try to read - should fail with runtime error
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            file = __file_open__["{}" ~> .0, 0, 0],
            __file_close__[file],
            __file_read__[file, 0, 1024]
        "#,
            path_str
        ))
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Effect operation failed: InvalidArgument(\"Resource 1 not found\")".to_string(),
        ));

    // Clean up
    let _ = std::fs::remove_file(&temp_path);
}
