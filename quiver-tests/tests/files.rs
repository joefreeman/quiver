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

            write_file = __file_open__ ["{}" ~> .0, 577, 420],
            __file_write__ [write_file, 0, "Hello, World!" ~> .0],
            __file_close__ write_file,

            read_file = __file_open__ ["{}" ~> .0, 0, 0],
            data = __file_read__ [read_file, 0, 4096],
            __file_close__ read_file,

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
            write_file = __file_open__ ["{}" ~> .0, 577, 420],
            __file_write__ [write_file, 0, "First line\n" ~> .0],
            __file_close__ write_file,

            // Write at offset 11 (length of "First line\n")
            append_file = __file_open__ ["{}" ~> .0, 1, 420],
            __file_write__ [append_file, 11, "Second line\n" ~> .0],
            __file_close__ append_file,

            // Read everything
            read_file = __file_open__ ["{}" ~> .0, 0, 0],
            data = __file_read__ [read_file, 0, 4096],
            __file_close__ read_file,

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
                data = __file_read__ [f, 0, 1024],
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
        .evaluate(r#"__file_open__ ["/tmp/foo" ~> .0, 577, 420]"#)
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
            file = __file_open__ ["{}" ~> .0, 577, 420],
            __file_write__ [file, 0, "Flushed data" ~> .0],
            __file_flush__ file,
            __file_close__ file,
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
            file = __file_open__ ["{}" ~> .0, 577, 420],
            __file_write__ [file, 0, "Line 1\n" ~> .0],
            __file_write__ [file, 7, "Line 2\n" ~> .0],
            __file_write__ [file, 14, "Line 3\n" ~> .0],
            __file_close__ file,

            read_file = __file_open__ ["{}" ~> .0, 0, 0],
            data = __file_read__ [read_file, 0, 4096],
            __file_close__ read_file,

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
            file = __file_open__ ["{}" ~> .0, 0, 0],
            __file_close__ file,
            __file_read__ [file, 0, 1024]
        "#,
            path_str
        ))
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Effect operation failed: InvalidArgument(\"Resource 1 not found\")".to_string(),
        ));

    // Clean up
    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_resource_ownership_transfers_on_send() {
    let temp_path = std::env::temp_dir().join(format!(
        "quiver_test_owntransfer_{}.txt",
        std::process::id()
    ));
    let path_str = temp_path.to_str().unwrap();
    std::fs::write(&temp_path, "Hello").expect("Failed to create test file");

    // Open in the main process, send the handle to a reader process. Ownership follows the
    // message, so the reader can read it.
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            'reader = Read[\File];
            r = @{{
                !#'reader ~> {{ =Read[f] => __file_read__ [f, 0, 5] ~> Str[~] }}
            }},
            file = __file_open__ ["{}" ~> .0, 0, 0],
            Read[file] ~> r,
            !r
        "#,
            path_str
        ))
        .expect("\"Hello\"");

    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_resource_ownership_enforced_after_transfer() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_test_ownenforce_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();
    std::fs::write(&temp_path, "Hello").expect("Failed to create test file");

    // After sending the handle away (transferring ownership), the original process may no
    // longer use it: the operation fails cleanly with a runtime error rather than hanging.
    // The holder process loops so it stays alive (and keeps ownership) while the main
    // process attempts its read.
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            'holder = Hold[\File];
            h = @{{
                !#'holder ~> {{ =Hold[_] => ^ }}
            }},
            file = __file_open__ ["{}" ~> .0, 0, 0],
            Hold[file] ~> h,
            __file_read__ [file, 0, 5]
        "#,
            path_str
        ))
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Effect operation failed: Process 0 does not own resource 1".to_string(),
        ));

    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_resource_cleanup_on_owner_completion() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_test_owncleanup_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();
    std::fs::write(&temp_path, "Hello").expect("Failed to create test file");

    // Ownership transfers to the reader, which reads then completes. On completion the
    // runtime auto-closes the file it owned, so a later access from the main process finds
    // the resource gone.
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            'reader = Read[\File];
            r = @{{
                !#'reader ~> {{ =Read[f] => __file_read__ [f, 0, 5] ~> Str[~] }}
            }},
            file = __file_open__ ["{}" ~> .0, 0, 0],
            Read[file] ~> r,
            !r,
            __file_read__ [file, 0, 5]
        "#,
            path_str
        ))
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Effect operation failed: InvalidArgument(\"Resource 1 not found\")".to_string(),
        ));

    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_std_file_write_then_read() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_std_file_wr_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();
    let _ = std::fs::remove_file(&temp_path);

    // Exercise the `file` standard library module: open for writing, write, close, then
    // reopen for reading and read the contents back.
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            file = %file,
            p = "{}" ~> %path.parse,
            w = file.open [p, mode: W],
            file.write [w, 0, "Hello, write!" ~> .0],
            file.close w,
            r = file.open [p],
            data = file.read [r, 0, 4096],
            file.close r,
            Str[data]
        "#,
            path_str
        ))
        .expect("\"Hello, write!\"");

    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_std_file_sequential_reads() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_std_file_seq_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();
    std::fs::write(&temp_path, "ABCDEFGHIJ").expect("Failed to create test file");

    // A single handle serves multiple reads at different offsets without reopening the file.
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            file = %file,
            p = "{}" ~> %path.parse,
            r = file.open [p],
            a = file.read [r, 0, 3],
            b = file.read [r, 3, 3],
            c = file.read [r, 6, 4],
            file.close r,
            [Str[a], Str[b], Str[c]]
        "#,
            path_str
        ))
        .expect("[\"ABC\", \"DEF\", \"GHIJ\"]");

    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_std_file_write_returns_byte_count() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_std_file_wc_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();
    let _ = std::fs::remove_file(&temp_path);

    // `write` reports the number of bytes written.
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            file = %file,
            p = "{}" ~> %path.parse,
            w = file.open [p, mode: W],
            n = file.write [w, 0, "Hello, write!" ~> .0],
            file.close w,
            n
        "#,
            path_str
        ))
        .expect("13");

    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_std_file_read_all() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_std_file_all_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();
    // Larger than the 65536-byte read chunk, to exercise the multi-chunk accumulation loop.
    let contents = "x".repeat(200_000);
    std::fs::write(&temp_path, &contents).expect("Failed to create test file");

    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            file = %file,
            p = "{}" ~> %path.parse,
            r = file.open [p],
            all = file.read_all r,
            file.close r,
            all ~> %bin.length
        "#,
            path_str
        ))
        .expect("200000");

    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_std_file_lines() {
    let temp_path =
        std::env::temp_dir().join(format!("quiver_std_file_lines_{}.txt", std::process::id()));
    let path_str = temp_path.to_str().unwrap();
    std::fs::write(&temp_path, "first line\nsecond line\nthird line\n")
        .expect("Failed to create test file");

    // `lines` yields an iterator over newline-separated lines (without the newline).
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            file = %file,
            r = "{}" ~> %path.parse ~> file.open [~],
            ls = file.lines r ~> %list.collect,
            file.close r,
            ls
        "#,
            path_str
        ))
        .expect("Cons[\"first line\", Cons[\"second line\", Cons[\"third line\", Nil]]]");

    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_std_file_lines_no_trailing_newline() {
    let temp_path = std::env::temp_dir().join(format!(
        "quiver_std_file_lines_nonl_{}.txt",
        std::process::id()
    ));
    let path_str = temp_path.to_str().unwrap();
    // No trailing newline, and an embedded empty line.
    std::fs::write(&temp_path, "a\n\nb").expect("Failed to create test file");

    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            file = %file,
            r = "{}" ~> %path.parse ~> file.open [~],
            ls = file.lines r ~> %list.collect,
            file.close r,
            ls
        "#,
            path_str
        ))
        .expect("Cons[\"a\", Cons[\"\", Cons[\"b\", Nil]]]");

    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_std_file_lines_spanning_chunks() {
    let temp_path = std::env::temp_dir().join(format!(
        "quiver_std_file_lines_big_{}.txt",
        std::process::id()
    ));
    let path_str = temp_path.to_str().unwrap();
    // A single line longer than the 65536-byte read chunk, followed by a short line: exercises
    // buffering a partial line across multiple reads before a newline is seen.
    let contents = format!("{}\nshort\n", "A".repeat(100_000));
    std::fs::write(&temp_path, contents).expect("Failed to create test file");

    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            file = %file,
            r = "{}" ~> %path.parse ~> file.open [~],
            lengths = file.lines r
              ~> %iter.map [~, #Str['bin] {{ .0 ~> %bin.length }}]
              ~> %list.collect,
            file.close r,
            lengths
        "#,
            path_str
        ))
        .expect("Cons[100000, Cons[5, Nil]]");

    let _ = std::fs::remove_file(&temp_path);
}

#[test]
fn test_read_dir_single_entry() {
    let dir = std::env::temp_dir().join(format!("quiver_readdir_one_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    std::fs::write(dir.join("only.txt"), b"x").expect("Failed to create test file");
    let dir_str = dir.to_str().unwrap();

    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            "{}" ~> %fs.list ~> %list.collect
        "#,
            dir_str
        ))
        .expect("Cons[Entry[name: \"only.txt\", kind: File], Nil]");

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn test_read_dir_reports_kind() {
    let dir = std::env::temp_dir().join(format!("quiver_readdir_kind_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    std::fs::create_dir(dir.join("d")).expect("Failed to create subdir");
    let dir_str = dir.to_str().unwrap();

    // A single subdirectory must come back with kind Dir.
    quiver()
        .with_io()
        .evaluate(&format!(r#""{}" ~> %fs.list ~> %list.collect"#, dir_str))
        .expect("Cons[Entry[name: \"d\", kind: Dir], Nil]");

    let _ = std::fs::remove_dir_all(&dir);
}

// Regression: matching a kind tag (from the module's `'%fs.kind` union) inside a nested block via
// condition-consequence used to send the type narrower into an infinite loop (stack overflow at
// compile time). It must compile and select the right branch at runtime.
#[test]
fn test_read_dir_kind_match_narrows() {
    let dir = std::env::temp_dir().join(format!("quiver_readdir_narrow_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    std::fs::create_dir(dir.join("d")).expect("Failed to create subdir");
    let dir_str = dir.to_str().unwrap();

    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            "{}"
            ~> %fs.list
            ~> %iter.map [~, #'%fs.entry {{ .kind ~> {{ =Dir => IsDir | NotDir }} }}]
            ~> %list.collect
        "#,
            dir_str
        ))
        .expect("Cons[IsDir, Nil]");

    let _ = std::fs::remove_dir_all(&dir);
}

// Regression: the natural `=(kind: Dir)` partial-pattern filter must narrow by the entry's runtime
// kind. The entries are built with a union-typed `kind`, so this only works if the field match
// checks the field value (not a root type assertion on the whole `Entry`).
#[test]
fn test_read_dir_filter_by_kind() {
    let dir = std::env::temp_dir().join(format!("quiver_readdir_filter_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    std::fs::create_dir(dir.join("sub")).expect("Failed to create subdir");
    std::fs::write(dir.join("f.txt"), b"x").expect("Failed to create file");
    let dir_str = dir.to_str().unwrap();

    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            "{}"
            ~> %fs.list
            ~> %iter.filter [~, #'%fs.entry {{ =(kind: Dir) }}]
            ~> %iter.map [~, #'%fs.entry {{ .name }}]
            ~> %list.collect
        "#,
            dir_str
        ))
        .expect("Cons[\"sub\", Nil]");

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn test_read_dir_empty() {
    let dir = std::env::temp_dir().join(format!("quiver_readdir_empty_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    let dir_str = dir.to_str().unwrap();

    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            "{}" ~> %fs.list ~> %list.collect
        "#,
            dir_str
        ))
        .expect("Nil");

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn test_read_dir_count() {
    let dir = std::env::temp_dir().join(format!("quiver_readdir_count_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    for name in ["a", "b", "c"] {
        std::fs::write(dir.join(name), b"").expect("Failed to create test file");
    }
    let dir_str = dir.to_str().unwrap();

    // The iterator yields entries in filesystem order (unsorted), so assert on the count rather
    // than a specific ordering.
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            "{}" ~> %fs.list ~> %iter.count
        "#,
            dir_str
        ))
        .expect("3");

    let _ = std::fs::remove_dir_all(&dir);
}

// Regression: a module that is value-cached on an earlier REPL line records the return-type
// dispatch tables of its functions (e.g. `num.add`). When a *later* line freshly compiles a
// different module (`str`, via `str.join`) that calls those cached dispatch functions, the tables
// must be restored — otherwise the dispatch result widens to the frozen `num` type (`'opt`) and a
// `'int` argument deep inside `str` fails to type-check. Here line 1 caches `iter`/`num` (through
// `fs`) without `str`; line 2 compiles `str` fresh against them.
#[test]
fn test_repl_cached_dispatch_module_then_fresh_consumer() {
    let dir = std::env::temp_dir().join(format!("quiver_readdir_repl_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    std::fs::write(dir.join("only"), b"").expect("Failed to create test file");
    let dir_str = dir.to_str().unwrap();

    quiver()
        .with_io()
        .evaluate(&format!(r#""{}" ~> %fs.list ~> %list.collect"#, dir_str))
        .expect("Cons[Entry[name: \"only\", kind: File], Nil]")
        .then_evaluate(&format!(
            r#""{}" ~> %fs.list ~> %iter.map [~, #'%fs.entry {{ .name }}] ~> %str.join [~, " | "]"#,
            dir_str
        ))
        .expect("\"only\"");

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn test_stat_file_kind_and_size() {
    let dir = std::env::temp_dir().join(format!("quiver_stat_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    std::fs::write(dir.join("f.txt"), b"hello").expect("Failed to create test file");
    let path = dir.join("f.txt");
    let path = path.to_str().unwrap();

    // Asserts on kind + size (mtime/mode vary by environment). Also exercises a literal field match
    // (`size: 5`) alongside a tag field match (`kind: File`) in one partial pattern.
    quiver()
        .with_io()
        .evaluate(&format!(
            r#""{}" ~> %fs.stat ~> {{ =(kind: File, size: 5) => Good | Bad }}"#,
            path
        ))
        .expect("Good");

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn test_stat_missing_is_nil() {
    let path = std::env::temp_dir().join(format!("quiver_stat_missing_{}", std::process::id()));
    let path = path.to_str().unwrap();

    quiver()
        .with_io()
        .evaluate(&format!(
            r#""{}" ~> %fs.stat ~> {{ <> => Absent | Present }}"#,
            path
        ))
        .expect("Absent");
}

// Regression: the composite tuples returned by the filesystem builtins must carry their *real*
// declared result type — including the nested `File`/`Dir`/… kind tag. The backend has no type
// registry of its own, so the environment pushes it each builtin's result type ids (outer tuple +
// named variants, see `set_type_ids`/`ResultTupleInfo`), and the backend stamps them directly. A
// regular file's stat result must therefore match `[File, 'int, 'int, 'int]` as a type, and *not*
// the old all-int `['int, 'int, 'int, 'int]` shape.
#[test]
fn test_stat_result_has_its_real_declared_type() {
    let dir = std::env::temp_dir().join(format!("quiver_stat_shape_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    std::fs::write(dir.join("f.txt"), b"hello").expect("Failed to create test file");
    let path = dir.join("f.txt");
    let path = path.to_str().unwrap();

    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            p = "{path}" ~> .0,
            s = __filesystem_stat__ p,
            [
              s ~> =[File, 'int, 'int, 'int] ~> {{ <> => No | Yes }},
              s ~> =['int, 'int, 'int, 'int] ~> {{ <> => No | Yes }}
            ]
            "#
        ))
        .expect("[Yes, No]");

    let _ = std::fs::remove_dir_all(&dir);
}

// Companion for the directory-entry tuple `[name, kind]`. Its first field is a heap binary (the
// entry name via the side-channel) and its second is a nested kind tag, so this exercises the real
// outer tuple id, the nested variant id, and heap injection together. The `warm` line registers a
// same-arity `[int, int]` first — the collision that defeated an earlier arity-based guess. With
// the real declared ids stamped, a regular-file entry matches `[bin, File]` and *not* `['int, 'int]`.
#[test]
fn test_directory_entry_has_its_real_declared_type() {
    let dir = std::env::temp_dir().join(format!("quiver_entry_shape_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    std::fs::write(dir.join("only"), b"").expect("Failed to create test file");
    let dir_str = dir.to_str().unwrap();

    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            warm = [1, 2] ~> =['int, 'int],
            d = "{dir_str}" ~> .0 ~> __directory_read__,
            e = __directory_next__ d,
            [
              e ~> =['bin, File] ~> {{ <> => No | Yes }},
              e ~> =['int, 'int] ~> {{ <> => No | Yes }}
            ]
            "#
        ))
        .expect("[Yes, No]");

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn test_fs_predicates() {
    let dir = std::env::temp_dir().join(format!("quiver_pred_{}", std::process::id()));
    std::fs::create_dir_all(dir.join("sub")).expect("Failed to create subdir");
    std::fs::write(dir.join("f.txt"), b"x").expect("Failed to create file");
    let d = dir.to_str().unwrap();

    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            [
              "{d}/f.txt" ~> %fs.exists? ~> {{ <> => No | Yes }},
              "{d}/nope"  ~> %fs.exists? ~> {{ <> => No | Yes }},
              "{d}/sub"   ~> %fs.dir?    ~> {{ <> => No | Yes }},
              "{d}/f.txt" ~> %fs.dir?    ~> {{ <> => No | Yes }},
              "{d}/f.txt" ~> %fs.file?   ~> {{ <> => No | Yes }}
            ]
            "#
        ))
        .expect("[Yes, No, Yes, No, Yes]");

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn test_read_dir_lazy_take() {
    let dir = std::env::temp_dir().join(format!("quiver_readdir_take_{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("Failed to create test directory");
    for name in ["a", "b", "c"] {
        std::fs::write(dir.join(name), b"").expect("Failed to create test file");
    }
    let dir_str = dir.to_str().unwrap();

    // Taking one entry from a three-entry directory must not require enumerating the rest,
    // demonstrating the iterator is lazy over the underlying directory handle.
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            "{}" ~> %fs.list ~> %iter.take [~, 1] ~> %iter.count
        "#,
            dir_str
        ))
        .expect("1");

    let _ = std::fs::remove_dir_all(&dir);
}
