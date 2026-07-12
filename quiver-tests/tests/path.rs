mod common;
use common::*;

#[test]
fn test_parse() {
    quiver().evaluate(r#""/" ~> %path.parse"#).expect("Root");

    quiver()
        .evaluate(r#""/foo" ~> %path.parse"#)
        .expect("Path[\"foo\", Root]");

    quiver()
        .evaluate(r#""/foo/bar" ~> %path.parse"#)
        .expect("Path[\"bar\", Path[\"foo\", Root]]");

    quiver()
        .evaluate(r#""foo" ~> %path.parse"#)
        .expect("Path[\"foo\", Relative[0]]");

    quiver()
        .evaluate(r#""foo/bar" ~> %path.parse"#)
        .expect("Path[\"bar\", Path[\"foo\", Relative[0]]]");

    // "." normalization - should be skipped
    quiver()
        .evaluate(r#""foo/./bar" ~> %path.parse"#)
        .expect("Path[\"bar\", Path[\"foo\", Relative[0]]]");

    quiver()
        .evaluate(r#""/foo/./bar" ~> %path.parse"#)
        .expect("Path[\"bar\", Path[\"foo\", Root]]");

    quiver()
        .evaluate(r#""./foo" ~> %path.parse"#)
        .expect("Path[\"foo\", Relative[0]]");

    // ".." normalization - should go up one level
    quiver()
        .evaluate(r#""foo/../bar" ~> %path.parse"#)
        .expect("Path[\"bar\", Relative[0]]");

    quiver()
        .evaluate(r#""/foo/../bar" ~> %path.parse"#)
        .expect("Path[\"bar\", Root]");

    quiver()
        .evaluate(r#""../foo" ~> %path.parse"#)
        .expect("Path[\"foo\", Relative[1]]");

    quiver()
        .evaluate(r#""foo/bar/../baz" ~> %path.parse"#)
        .expect("Path[\"baz\", Path[\"foo\", Relative[0]]]");

    // ".." at root should stay at root
    quiver()
        .evaluate(r#""/../foo" ~> %path.parse"#)
        .expect("Path[\"foo\", Root]");

    // Multiple ".."
    quiver()
        .evaluate(r#""../../foo" ~> %path.parse"#)
        .expect("Path[\"foo\", Relative[2]]");
}

#[test]
fn test_basename() {
    quiver()
        .evaluate(r#""/foo/bar" ~> %path.parse ~> %path.basename"#)
        .expect("\"bar\"");

    quiver()
        .evaluate(r#""/foo" ~> %path.parse ~> %path.basename"#)
        .expect("\"foo\"");

    quiver()
        .evaluate(r#""foo" ~> %path.parse ~> %path.basename"#)
        .expect("\"foo\"");
}

#[test]
fn test_parent() {
    quiver()
        .evaluate(r#""/foo/bar" ~> %path.parse ~> %path.parent"#)
        .expect("Path[\"foo\", Root]");

    quiver()
        .evaluate(r#""/foo" ~> %path.parse ~> %path.parent"#)
        .expect("Root");

    quiver()
        .evaluate(r#""/" ~> %path.parse ~> %path.parent"#)
        .expect("Root");

    quiver()
        .evaluate(r#""foo" ~> %path.parse ~> %path.parent"#)
        .expect("Relative[0]");

    quiver()
        .evaluate(r#"Relative[0] ~> %path.parent"#)
        .expect("Relative[1]");
}

#[test]
fn test_to_string() {
    quiver()
        .evaluate(r#""/" ~> %path.parse ~> %path.to_string"#)
        .expect("\"/\"");

    quiver()
        .evaluate(r#""/foo/bar" ~> %path.parse ~> %path.to_string"#)
        .expect("\"/foo/bar\"");

    quiver()
        .evaluate(r#""foo/bar" ~> %path.parse ~> %path.to_string"#)
        .expect("\"foo/bar\"");

    quiver()
        .evaluate(r#"Relative[0] ~> %path.to_string"#)
        .expect("\".\"");

    quiver()
        .evaluate(r#"Relative[2] ~> %path.to_string"#)
        .expect("\"../..\"");
}

#[test]
fn test_absolute() {
    quiver()
        .evaluate(r#""/" ~> %path.parse ~> %path.absolute?"#)
        .expect("Ok");

    quiver()
        .evaluate(r#""/foo/bar" ~> %path.parse ~> %path.absolute?"#)
        .expect("Ok");

    quiver()
        .evaluate(r#""foo/bar" ~> %path.parse ~> %path.absolute?"#)
        .expect("[]");

    quiver()
        .evaluate(r#"Relative[0] ~> %path.absolute?"#)
        .expect("[]");
}

#[test]
fn test_relative() {
    quiver()
        .evaluate(r#""foo/bar" ~> %path.parse ~> %path.relative?"#)
        .expect("Ok");

    quiver()
        .evaluate(r#"Relative[2] ~> %path.relative?"#)
        .expect("Ok");

    quiver()
        .evaluate(r#""/" ~> %path.parse ~> %path.relative?"#)
        .expect("[]");

    quiver()
        .evaluate(r#""/foo" ~> %path.parse ~> %path.relative?"#)
        .expect("[]");
}

#[test]
fn test_join() {
    // Test Path construction
    quiver()
        .evaluate(r#"Path["test", Root]"#)
        .expect("Path[\"test\", Root]");

    // Test what bar parses to
    quiver()
        .evaluate(r#""bar" ~> %path.parse"#)
        .expect("Path[\"bar\", Relative[0]]");

    // simple: absolute + Relative[0] -> same path
    quiver()
        .evaluate(r#"["/foo" ~> %path.parse, Relative[0]] ~> %path.join ~> %path.to_string"#)
        .expect("\"/foo\"");

    // Debug: should return Path[s, first] = Path["bar", Path["foo", Root]]
    quiver()
        .evaluate(r#"["/foo" ~> %path.parse, "bar" ~> %path.parse] ~> %path.join"#)
        .expect("Path[\"bar\", Path[\"foo\", Root]]");

    // absolute + relative -> appends
    quiver()
        .evaluate(
            r#"["/foo" ~> %path.parse, "bar" ~> %path.parse] ~> %path.join ~> %path.to_string"#,
        )
        .expect("\"/foo/bar\"");

    // joining with nested relative path
    quiver()
        .evaluate(
            r#"["/foo" ~> %path.parse, "bar/baz" ~> %path.parse] ~> %path.join ~> %path.to_string"#,
        )
        .expect("\"/foo/bar/baz\"");

    // absolute + absolute -> nil (invalid)
    quiver()
        .evaluate(r#"["/foo" ~> %path.parse, "/bar" ~> %path.parse] ~> %path.join"#)
        .expect("[]");

    // traversing past root -> nil (invalid)
    quiver()
        .evaluate(r#"["/foo" ~> %path.parse, Relative[2]] ~> %path.join"#)
        .expect("[]");

    // traversing exactly to root -> valid
    quiver()
        .evaluate(r#"["/foo" ~> %path.parse, Relative[1]] ~> %path.join ~> %path.to_string"#)
        .expect("\"/\"");

    // relative + relative
    quiver()
        .evaluate(
            r#"["foo" ~> %path.parse, "bar" ~> %path.parse] ~> %path.join ~> %path.to_string"#,
        )
        .expect("\"foo/bar\"");

    // joining with parent reference
    quiver()
        .evaluate(r#"["/foo/bar" ~> %path.parse, Relative[1]] ~> %path.join ~> %path.to_string"#)
        .expect("\"/foo\"");
}
