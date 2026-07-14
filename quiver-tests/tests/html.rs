mod common;
use common::*;
use quiver_compiler::compiler::Error;

// The `%html{ … }` dialect and renderer (std/html.qv): the dialect
// parses HTML-with-holes into a '%html node tree at compile time, `render` serializes it.
// Holes (`{ … }`) are host blocks; their values normalize through `child`/`attr_value`.
// Static template text is trusted (emitted verbatim); hole values are escaped.

#[test]
fn test_render_static_elements() {
    quiver()
        .evaluate(r#"%html{ <p>hi</p> } ~> %html.render"#)
        .expect(r#""<p>hi</p>""#);
    quiver()
        .evaluate(r#"%html{ <div><span>a</span><span>b</span></div> } ~> %html.render"#)
        .expect(r#""<div><span>a</span><span>b</span></div>""#);
}

#[test]
fn test_attributes() {
    // Static string values, bare boolean attributes, and dashed names.
    quiver()
        .evaluate(r#"%html{ <a href="/x" data-k="v" hidden>go</a> } ~> %html.render"#)
        .expect(r#""<a href=\"/x\" data-k=\"v\" hidden>go</a>""#);
}

#[test]
fn test_void_and_self_closing_elements() {
    // Void elements take no close tag; `/>` self-closes any element (rendered expanded).
    quiver()
        .evaluate(r#"%html{ <img src="a.png"><br><div/> } ~> %html.render"#)
        .expect(r#""<img src=\"a.png\"><br><div></div>""#);
}

#[test]
fn test_multiple_roots_render_as_fragment() {
    quiver()
        .evaluate(r#"%html{ <li>a</li><li>b</li> } ~> %html.render"#)
        .expect(r#""<li>a</li><li>b</li>""#);
}

#[test]
fn test_root_whitespace_is_trimmed_interior_preserved() {
    // Whitespace-only text at the root's edges is trimmed (so a single root splices bare);
    // interior whitespace is verbatim.
    quiver()
        .evaluate("%html{\n  <p>hi</p>\n} ~> %html.render")
        .expect(r#""<p>hi</p>""#);
    quiver()
        .evaluate("%html{ <ul>\n  <li>a</li>\n</ul> } ~> %html.render")
        .expect(r#""<ul>\n  <li>a</li>\n</ul>""#);
}

#[test]
fn test_text_holes_are_escaped() {
    quiver()
        .evaluate(r#"name = "<b>&\"x"; %html{ <p>{name}</p> } ~> %html.render"#)
        .expect(r#""<p>&lt;b&gt;&amp;\"x</p>""#);
}

#[test]
fn test_static_text_is_trusted() {
    // Author-written entities pass through verbatim — only hole values are escaped.
    quiver()
        .evaluate(r#"%html{ <p>&amp; &#123;</p> } ~> %html.render"#)
        .expect(r#""<p>&amp; &#123;</p>""#);
}

#[test]
fn test_raw_opts_out_of_escaping() {
    quiver()
        .evaluate(r#"%html{ <div>{ "<b>x</b>" ~> %html.raw }</div> } ~> %html.render"#)
        .expect(r#""<div><b>x</b></div>""#);
}

#[test]
fn test_int_holes_render_as_digits() {
    quiver()
        .evaluate(r#"%html{ <p>{ 42 }</p> } ~> %html.render"#)
        .expect(r#""<p>42</p>""#);
}

#[test]
fn test_nil_hole_renders_nothing() {
    // A hole is a host block; one that fails (nil) renders nothing — the conditional idiom.
    quiver()
        .evaluate(r#"no = []; %html{ <p>x{ no => "yes" }y</p> } ~> %html.render"#)
        .expect(r#""<p>xy</p>""#);
    quiver()
        .evaluate(r#"ok? = Ok; %html{ <p>{ ok? => "yes" }</p> } ~> %html.render"#)
        .expect(r#""<p>yes</p>""#);
}

#[test]
fn test_flowing_value_in_holes() {
    // `~` inside a hole is the value flowing into the dialect term.
    quiver()
        .evaluate(r#"[name: "Ada"] ~> %html{ <p>{~.name}</p> } ~> %html.render"#)
        .expect(r#""<p>Ada</p>""#);
}

#[test]
fn test_attribute_holes() {
    // A Str renders as the value (escaped); an int as digits; Ok bare; nil omits.
    quiver()
        .evaluate(r#"c = "a\"b"; %html{ <div class={c} data-n={ 7 }>x</div> } ~> %html.render"#)
        .expect(r#""<div class=\"a&quot;b\" data-n=\"7\">x</div>""#);
    quiver()
        .evaluate(r#"%html{ <input disabled={ Ok } value={ [] }> } ~> %html.render"#)
        .expect(r#""<input disabled>""#);
}

#[test]
fn test_node_list_holes_render_as_fragments() {
    quiver()
        .evaluate(
            r#"
            items = %list{ "a", "b" } ~> %list.iter
              ~> [~, #Str['bin] { %html{ <li>{$}</li> } }] ~> %iter.map
              ~> %list.collect;
            %html{ <ul>{items}</ul> } ~> %html.render
            "#,
        )
        .expect(r#""<ul><li>a</li><li>b</li></ul>""#);
}

#[test]
fn test_components_are_functions() {
    // A component is a function 'props -> '%html; children pass as a nested %html{…} value.
    quiver()
        .evaluate(
            r#"
            card = #[title: Str['bin], children: '%html] {
              %html{ <section><h2>{$title}</h2>{$children}</section> }
            };
            %html{ <main>{ [title: "T", children: %html{ <p>b</p> }] ~> card }</main> } ~> %html.render
            "#,
        )
        .expect(r#""<main><section><h2>T</h2><p>b</p></section></main>""#);
}

#[test]
fn test_tree_is_pattern_matchable() {
    // The dialect yields an inspectable tree — structural assertions instead of string goldens.
    quiver()
        .evaluate(r#"%html{ <div id="x">hi</div> } ~> =Element(tag: t); t"#)
        .expect(r#""div""#);
    quiver()
        .evaluate(r#"%html{ <br> } ~> =Element[tag: "br", attrs: Nil, children: Nil]; Ok"#)
        .expect("Ok");
}

#[test]
fn test_comments_are_skipped() {
    quiver()
        .evaluate(r#"%html{ <div><!-- note -->x</div> } ~> %html.render"#)
        .expect(r#""<div>x</div>""#);
}

#[test]
fn test_page_prepends_doctype() {
    quiver()
        .evaluate(r#"%html{ <p>hi</p> } ~> %html.page"#)
        .expect(r#""<!doctype html><p>hi</p>""#);
}

#[test]
fn test_mismatched_close_tag_is_a_positioned_error() {
    // Content starts at 1:8 (after `%html{`); the close-tag name `div` sits at column 21.
    quiver()
        .evaluate(r#"%html{ <div><span></div> }"#)
        .expect_compile_error(Error::DialectFailed {
            module: "%html".to_string(),
            message: "failed: '</span>' (at main:1:21)".to_string(),
        });
}

#[test]
fn test_capitalized_tags_are_rejected() {
    quiver()
        .evaluate(r#"%html{ <Card/> }"#)
        .expect_compile_error(Error::DialectFailed {
            module: "%html".to_string(),
            message:
                "failed: a lowercase tag name (capitalized tags are reserved for components) (at main:1:9)"
                    .to_string(),
        });
}

#[test]
fn test_inline_script_is_rejected() {
    quiver()
        .evaluate(r#"%html{ <script>x</script> }"#)
        .expect_compile_error(Error::DialectFailed {
            module: "%html".to_string(),
            message: "failed: a supported tag (inline script/style is not supported) (at main:1:9)"
                .to_string(),
        });
}

#[test]
fn test_duplicate_attribute_is_rejected() {
    quiver()
        .evaluate(r#"%html{ <div id="a" id="b"></div> }"#)
        .expect_compile_error(Error::DialectFailed {
            module: "%html".to_string(),
            message: "failed: a unique attribute name (at main:1:20)".to_string(),
        });
}

#[test]
fn test_hole_type_errors_cite_the_hole() {
    // A hole value outside the child union ('%html node, Str, int, node list, nil) is a
    // type error at the splice.
    quiver()
        .evaluate(r#"%html{ <p>{ [1, 2] }</p> }"#)
        .expect_type_mismatch();
}
