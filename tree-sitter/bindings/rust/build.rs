fn main() {
    let src_dir = std::path::Path::new("src");

    let mut c_config = cc::Build::new();
    c_config.std("c11").include(src_dir);
    c_config
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-unused-but-set-variable")
        .flag_if_supported("-Wno-trigraphs")
        // The generated `ts_lex_keywords` (present because the grammar sets `word`) expands
        // START_LEXER(), which defines an unused `next_state:` label.
        .flag_if_supported("-Wno-unused-label");

    let parser_path = src_dir.join("parser.c");
    c_config.file(&parser_path);
    println!("cargo:rerun-if-changed={}", parser_path.to_str().unwrap());

    // This grammar has no external scanner (scanner.c); the parser is self-contained.

    c_config.compile("tree-sitter-quiver");
}
