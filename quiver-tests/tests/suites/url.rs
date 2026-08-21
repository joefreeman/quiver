use crate::common::*;

// `%url` (std/url.qv): absolute-URL parsing, rendering and reference resolution. All pure —
// no host capability is involved, so these run on a bare registry.

const PARTS: &str = r#"parts = #'%str {
      %url.parse ~ ~> =('%url)u
      [u.scheme, u.host, u.port, u.path, u.query, u.fragment]
    }
    "#;

#[test]
fn test_parse_splits_every_component() {
    quiver()
        .evaluate(&format!(
            r#"{PARTS}parts "http://example.com/a/b?x=1&y=2#frag""#
        ))
        .expect(r#"["http", "example.com", 80, "/a/b", "x=1&y=2", "frag"]"#);
}

#[test]
fn test_parse_fills_the_port_from_the_scheme() {
    // An absent port is filled in, so a client never has to special-case it; a stated one
    // wins, and the scheme is folded to lower case (it is case-insensitive on the wire).
    quiver()
        .evaluate(&format!(r#"{PARTS}parts "https://example.com""#))
        .expect(r#"["https", "example.com", 443, "/", "", ""]"#);
    quiver()
        .evaluate(&format!(r#"{PARTS}parts "ws://h/socket""#))
        .expect(r#"["ws", "h", 80, "/socket", "", ""]"#);
    quiver()
        .evaluate(&format!(r#"{PARTS}parts "HTTP://example.com:8080/p""#))
        .expect(r#"["http", "example.com", 8080, "/p", "", ""]"#);
}

#[test]
fn test_parse_handles_an_ipv6_literal() {
    // The colons inside the brackets are part of the address, not the port separator.
    quiver()
        .evaluate(&format!(r#"{PARTS}parts "https://[::1]:9000/v6""#))
        .expect(r#"["https", "[::1]", 9000, "/v6", "", ""]"#);
    quiver()
        .evaluate(&format!(r#"{PARTS}parts "https://[::1]/v6""#))
        .expect(r#"["https", "[::1]", 443, "/v6", "", ""]"#);
}

#[test]
fn test_parse_rejects_malformed_input() {
    for bad in [
        r#""not a url""#,
        r#""http:///p""#,         // no host
        r#""://example.com""#,    // no scheme
        r#""http:/example.com""#, // one slash
        r#""http://h:notaport/""#,
    ] {
        quiver().evaluate(&format!("%url.parse {bad}")).expect("[]");
    }
}

#[test]
fn test_format_omits_a_default_port_and_round_trips() {
    quiver()
        .evaluate(r#""https://e.com:443/p" ~> %url.parse ~ ~> =('%url)u; %url.format u"#)
        .expect(r#""https://e.com/p""#);
    quiver()
        .evaluate(r#""http://e.com:8080/p?q#f" ~> %url.parse ~ ~> =('%url)u; %url.format u"#)
        .expect(r#""http://e.com:8080/p?q#f""#);
}

#[test]
fn test_target_is_path_and_query_only() {
    // What goes in the request line — never the scheme, host or fragment.
    quiver()
        .evaluate(r#""http://e.com/a?x=1#frag" ~> %url.parse ~ ~> =('%url)u; %url.target u"#)
        .expect(r#""/a?x=1""#);
    quiver()
        .evaluate(r#""http://e.com/a" ~> %url.parse ~ ~> =('%url)u; %url.target u"#)
        .expect(r#""/a""#);
}

#[test]
fn test_resolve_covers_the_reference_forms_a_redirect_uses() {
    let resolve = r#"res = #'%str {
          "http://example.com/a/b?x=1#f" ~> %url.parse ~ ~> =('%url)base
          %url.resolve [base, $] ~> =('%url)u
          %url.format u
        }
        "#;
    for (reference, expected) in [
        ("https://other.org/z", "https://other.org/z"), // absolute
        ("//other.org/z", "http://other.org/z"),        // scheme-relative
        ("/z?q=2", "http://example.com/z?q=2"),         // absolute path
        ("#top", "http://example.com/a/b?x=1#top"),     // fragment only
        ("c/d", "http://example.com/a/c/d"),            // relative to the base directory
    ] {
        quiver()
            .evaluate(&format!(r#"{resolve}res "{reference}""#))
            .expect(&format!(r#""{expected}""#));
    }
}
