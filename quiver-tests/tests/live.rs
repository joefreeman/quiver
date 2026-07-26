mod common;
use common::*;

// `%html/live` (std/html/live.qv): frames and patches over the `:template` provenance
// `%html{ … }` expansions attach. `frame` derives a render's diffable shape, `render`
// serializes it to instrumented HTML (child-hole markers, data-q anchors), and `diff`
// answers the patches turning one frame into another — recursing where template statics
// match, one subsuming patch where a slot changed shape.

#[test]
fn test_template_provenance_is_attached() {
    // The dialect attaches [statics, holes]: the attr hole's run cut excludes the whole
    // attribute, its element carries a data-q anchor, and child holes leave markers in
    // the surrounding runs. Hole values are the normalized ones the tree shares.
    quiver()
        .evaluate(
            r#"u = [name: "Ada", on?: Ok]
               t = u ~> %html{ <div class="x" hidden={~.on?}>Hi {~.name}!</div> }
               t:('%html.tmpl<[]>)template"#,
        )
        .expect(
            r#"[statics: Cons["<div data-q=\"0\" class=\"x\"", Cons[">Hi <!--q:1-->", Cons["<!--/q:1-->!</div>", Nil]]], holes: Cons[Attr[elem: 0, name: "hidden", value: Ok], Cons[Child[Text["Ada"]], Nil]]]"#,
        );
}

#[test]
fn test_tree_render_stays_clean_of_instrumentation() {
    // Markers and data-q anchors live in the provenance statics only — the canonical
    // tree, and therefore %html.render, are untouched.
    quiver()
        .evaluate(
            r#"u = [name: "Ada", on?: Ok]
               u ~> %html{ <div class="x" hidden={~.on?}>Hi {~.name}!</div> } ~> %html.render"#,
        )
        .expect(r#""<div class=\"x\" hidden>Hi Ada!</div>""#);
}

#[test]
fn test_frame_render_is_instrumented() {
    quiver()
        .evaluate(
            r#"u = [name: "Ada", on?: Ok]
               u ~> %html{ <div class="x" hidden={~.on?}>Hi {~.name}!</div> }
               ~> %html/live.frame ~> %html/live.render"#,
        )
        .expect(r#""<div data-q=\"0\" class=\"x\" hidden>Hi <!--q:1-->Ada<!--/q:1-->!</div>""#);
}

#[test]
fn test_diff_equal_frames_is_nil() {
    quiver()
        .evaluate(
            r#"view = #[name: Str['bin], on?: (Ok | [])] { %html{ <p hidden={$on?}>{$name}</p> } }
               f1 = view [name: "x", on?: []] ~> %html/live.frame
               f2 = view [name: "x", on?: []] ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect("Nil");
}

#[test]
fn test_diff_leaf_change_emits_set_text() {
    quiver()
        .evaluate(
            r#"view = #Str['bin] { %html{ <p>Hi {$}!</p> } }
               f1 = "Ada" ~> view ~> %html/live.frame
               f2 = "Bob" ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(r#"Cons[SetText[path: Cons[0, Nil], value: "Bob"], Nil]"#);
}

#[test]
fn test_diff_attr_change_emits_set_attr() {
    // A boolean attribute dropping to nil (omitted), and a string value appearing.
    quiver()
        .evaluate(
            r#"view = #[on?: (Ok | []), c: (Str['bin] | [])] { %html{ <p hidden={$on?} class={$c}>x</p> } }
               f1 = view [on?: Ok, c: []] ~> %html/live.frame
               f2 = view [on?: [], c: "big"] ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(
            r#"Cons[SetAttr[path: Cons[0, Nil], elem: 0, name: "hidden", value: []], Cons[SetAttr[path: Cons[1, Nil], elem: 0, name: "class", value: "big"], Nil]]"#,
        );
}

#[test]
fn test_diff_branch_switch_emits_one_subsuming_set_html() {
    // A conditional hole switching templates: the statics differ, so the diff replaces
    // the slot with the new subtree's instrumented HTML — and emits nothing beneath it.
    quiver()
        .evaluate(
            r#"view = #(Ok | []) { on? = $; %html{ <div>{ { | on? ~> =Ok => %html{ <b>yes</b> } | %html{ <i>no</i> } } }</div> } }
               f1 = Ok ~> view ~> %html/live.frame
               f2 = [] ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(r#"Cons[SetHtml[path: Cons[0, Nil], html: "<i>no</i>"], Nil]"#);
}

#[test]
fn test_diff_nested_component_change_is_granular() {
    // A component's expansion carries its own provenance, so a change inside it patches
    // at the nested path — not by replacing the outer slot.
    quiver()
        .evaluate(
            r#"item = #[t: Str['bin]] { %html{ <li>{$t}</li> } }
               view = #Str['bin] { %html{ <ul>{ [t: $] ~> item }</ul> } }
               f1 = "a" ~> view ~> %html/live.frame
               f2 = "b" ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(r#"Cons[SetText[path: Cons[0, Cons[0, Nil]], value: "b"], Nil]"#);
}

#[test]
fn test_list_rows_render_with_markers() {
    // A node-list hole is a Many: rows delimited by anonymous <!--r--> markers inside
    // the slot's own markers.
    quiver()
        .evaluate(
            r#"items = Cons[%html.text "a", Cons[%html.text "b", Nil]]
               %html{ <ul>{items}</ul> } ~> %html/live.frame ~> %html/live.render"#,
        )
        .expect(r#""<ul><!--q:0--><!--r-->a<!--/r--><!--r-->b<!--/r--><!--/q:0--></ul>""#);
}

#[test]
fn test_diff_list_row_changes_in_place() {
    // Rows diff pairwise: a changed row patches at its row index, not by replacing
    // the list.
    quiver()
        .evaluate(
            r#"view = #[Str['bin], Str['bin]] {
                 =[a, b]
                 items = Cons[%html.text a, Cons[%html.text b, Nil]]
                 %html{ <ul>{items}</ul> }
               }
               f1 = view ["a", "b"] ~> %html/live.frame
               f2 = view ["a", "c"] ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(r#"Cons[SetText[path: Cons[0, Cons[1, Nil]], value: "c"], Nil]"#);
}

#[test]
fn test_diff_list_append_emits_one_ins_row() {
    // Appending costs one r+ op carrying the row (markers included) — template items
    // arrive with their own slot markers, so later in-row patches stay granular.
    quiver()
        .evaluate(
            r#"item = #Str['bin] { %html{ <li>{$}</li> } }
               view = #<'e>'%html.nodes<'e> { =items; %html{ <ul>{items}</ul> } }
               f1 = Cons[item "a", Cons[item "b", Nil]] ~> view ~> %html/live.frame
               f2 = Cons[item "a", Cons[item "b", Cons[item "c", Nil]]] ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(
            r#"Cons[InsRow[path: Cons[0, Nil], at: 2, html: "<!--r--><li><!--q:0-->c<!--/q:0--></li><!--/r-->"], Nil]"#,
        );
}

#[test]
fn test_diff_list_truncate_emits_del_rows_at_fixed_index() {
    // Deleting the tail is DelRow at the same index repeatedly: each removal shifts
    // the rest down, so sequential application is correct.
    quiver()
        .evaluate(
            r#"view = #<'e>'%html.nodes<'e> { =items; %html{ <ul>{items}</ul> } }
               f1 = Cons[%html.text "a", Cons[%html.text "b", Cons[%html.text "c", Nil]]] ~> view ~> %html/live.frame
               f2 = Cons[%html.text "a", Nil] ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(
            r#"Cons[DelRow[path: Cons[0, Nil], at: 1], Cons[DelRow[path: Cons[0, Nil], at: 1], Nil]]"#,
        );
}

#[test]
fn test_row_frames_carry_key_annotations() {
    // A row's optional identity is the item node's :key annotation — read into the
    // frame now, used by the keyed diff later.
    quiver()
        .evaluate(
            r#"it = %html{ <li>x</li> } ~> { :key "k1" }
               Cons[it, Nil] ~> %html.child ~> %html/live.frame ~> =Many[Cons[[key: k, row: _], Nil]]
               k"#,
        )
        .expect(r#""k1""#);
}

#[test]
fn test_encode_row_ops() {
    quiver()
        .evaluate(
            r#"view = #<'e>'%html.nodes<'e> { =items; %html{ <ul>{items}</ul> } }
               f1 = Cons[%html.text "a", Nil] ~> view ~> %html/live.frame
               f2 = Cons[%html.text "a", Cons[%html.text "b", Nil]] ~> view ~> %html/live.frame
               f3 = Cons[%html.text "a", Nil] ~> view ~> %html/live.frame
               [%html/live.diff [f1, f2] ~> %html/live.encode ["0", ~], %html/live.diff [f2, f3] ~> %html/live.encode ["0", ~]]"#,
        )
        .expect(
            r#"["[1,\"0\",[[\"r+\",[0],1,\"<!--r-->b<!--/r-->\"]]]", "[1,\"0\",[[\"r-\",[0],1]]]"]"#,
        );
}

#[test]
fn test_diff_root_template_switch_targets_root() {
    // A Nil path addresses the root (the live region itself).
    quiver()
        .evaluate(
            r#"f1 = %html{ <p>x</p> } ~> %html/live.frame
               f2 = %html{ <div>x</div> } ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(r#"Cons[SetHtml[path: Nil, html: "<div>x</div>"], Nil]"#);
}

#[test]
fn test_frame_of_unannotated_tree_is_opaque() {
    // Hand-built trees (raw, text, manual construction) have no provenance: rendered
    // and compared as one blob.
    quiver()
        .evaluate(
            r#"f1 = %html.raw "<b>x</b>" ~> %html/live.frame
               f1b = %html.raw "<b>x</b>" ~> %html/live.frame
               f2 = %html.raw "<b>y</b>" ~> %html/live.frame
               [%html/live.diff [f1, f1b], %html/live.diff [f1, f2]]"#,
        )
        .expect(r#"[Nil, Cons[SetHtml[path: Nil, html: "<b>y</b>"], Nil]]"#);
}

#[test]
fn test_event_attributes_serialize_payloads_as_data() {
    // `on:click={ … }` takes a typed event payload, encoded (%data notation, in the
    // `Ev` envelope) into an emitted `data-q-click` attribute — events are data the
    // client echoes back verbatim, and the component decodes with `%data.decode`.
    quiver()
        .evaluate(r#"%html{ <button on:click={Inc[5]}>+</button> } ~> %html.render"#)
        .expect(r#""<button data-q-click=\"Ev[Inc[5]]\">+</button>""#);
    // A string payload is data too, attr-escaped like any attribute value.
    quiver()
        .evaluate(r#"%html{ <button on:click={"inc"}>+</button> } ~> %html.render"#)
        .expect(r#""<button data-q-click=\"Ev[&quot;inc&quot;]\">+</button>""#);
}

#[test]
fn test_reserved_bare_attributes_emit_as_data_q() {
    // `nav`, `keys`, `debounce` and `throttle` are the live layer's own bare attribute
    // names: they emit under `data-q-` and are read only by the client, so they never
    // reach the server as event metadata or as ordinary attributes.
    quiver()
        .evaluate(
            r#"%html{ <input on:input={F} debounce="300" keys="Enter Escape" throttle="50"> } ~> %html.render"#,
        )
        .expect(
            r#""<input data-q-input=\"Ev[F]\" data-q-debounce=\"300\" data-q-keys=\"Enter Escape\" data-q-throttle=\"50\">""#,
        );
    // An author still cannot write the instrumentation namespace directly.
    quiver()
        .evaluate(r#"%html{ <p data-q-keys="x">hi</p> }"#)
        .expect_error_containing("'data-q' is reserved for template instrumentation");
}

#[test]
fn test_event_payload_changes_patch_like_attributes() {
    // A payload that varies with state diffs as an ordinary attribute slot.
    quiver()
        .evaluate(
            r#"view = #'int { %html{ <button on:click={Del[~]}>x</button> } }
               f1 = 1 ~> view ~> %html/live.frame
               f2 = 2 ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(
            r#"Cons[SetAttr[path: Cons[0, Nil], elem: 0, name: "data-q-click", value: "Ev[Del[2]]"], Nil]"#,
        );
}

#[test]
fn test_encode_frame_message() {
    // One render's patches as the wire frame: [1, vid, [patch, …]] — "t"/"a"/"h"
    // tags, paths as index arrays, attr values as string/true/null.
    quiver()
        .evaluate(
            r#"view = #[name: Str['bin], on?: (Ok | [])] { %html{ <p hidden={$on?}>{$name}</p> } }
               f1 = view [name: "a", on?: Ok] ~> %html/live.frame
               f2 = view [name: "b", on?: []] ~> %html/live.frame
               %html/live.diff [f1, f2] ~> %html/live.encode ["0", ~]"#,
        )
        .expect(r#""[1,\"0\",[[\"a\",[0],0,\"hidden\",null],[\"t\",[1],\"b\"]]]""#);
}

// Boundaries: `child [component, init, key]` places a child view. The frame carries
// a Boundary the differ compares by key and never descends into — the interior is
// the child's own, patched under its own vid.

/// A minimal child component for boundary tests.
const KID: &str = r#"
    'cev = Bump
    cc = [
      mount: #'int,
      update: #[(state): 'int, (event): 'cev] { %num.add [$state, 1] },
      view: #'int { %str.from_int $ ~> %html{ <em>{~}</em> } },
      decode: &%data.decode<Ev['cev]>,
    ]
    ccf = %html/live.component cc
"#;

#[test]
fn test_boundary_same_key_never_descends() {
    // The parent's state changes — including the child's INIT — but the key is
    // stable, so the boundary contributes no patch: the interior is the child's.
    quiver()
        .evaluate(
            &[
                KID,
                r#"view = #'int { n = $; %html{ <div><p>{%str.from_int n}</p>{ ccf [init: n, key: "w"] }</div> } }
                   f1 = 1 ~> view ~> %html/live.frame
                   f2 = 2 ~> view ~> %html/live.frame
                   %html/live.diff [f1, f2]"#,
            ]
            .concat(),
        )
        .expect(r#"Cons[SetText[path: Cons[0, Nil], value: "2"], Nil]"#);
}

#[test]
fn test_boundary_key_change_replaces_slot() {
    // A changed key is a different child: one subsuming replace at the slot. The
    // fresh markers render empty here because a bare diff never reconciles (no vid
    // is assigned) — in the live loop the replacement carries the new child's
    // markers and its first frame fills them.
    quiver()
        .evaluate(
            &[
                KID,
                r#"view = #Str['bin] { k = $; %html{ <div>{ ccf [init: 0, key: k] }</div> } }
                   f1 = "a" ~> view ~> %html/live.frame
                   f2 = "b" ~> view ~> %html/live.frame
                   %html/live.diff [f1, f2]"#,
            ]
            .concat(),
        )
        .expect(r#"Cons[SetHtml[path: Cons[0, Nil], html: ""], Nil]"#);
}

#[test]
fn test_boundary_in_keyed_rows_moves_without_descent() {
    // The rows lesson, applied to views: a boundary in a keyed row MOVES with its
    // row — one r> op, nothing inside either child touched.
    quiver()
        .evaluate(
            &[
                KID,
                r#"row = #[Str['bin], 'int] {
                     =[k, i]
                     %html{ <li>{ ccf [init: i, key: k] }</li> } ~> { :key k }
                   }
                   view = #(Ok | []) {
                     rows = $ ~> {
                       | =Ok => Cons[row ["x", 1], Cons[row ["y", 2], Nil]]
                       | Cons[row ["y", 2], Cons[row ["x", 1], Nil]]
                     }
                     %html{ <ul>{rows}</ul> }
                   }
                   f1 = Ok ~> view ~> %html/live.frame
                   f2 = [] ~> view ~> %html/live.frame
                   %html/live.diff [f1, f2]"#,
            ]
            .concat(),
        )
        .expect(r#"Cons[MovRow[path: Cons[0, Nil], from: 1, to: 0], Nil]"#);
}

#[test]
fn test_keyed_reorder_emits_one_move() {
    // Fully keyed lists diff by identity: a reorder is one r> — the DOM nodes move,
    // their state riding along.
    quiver()
        .evaluate(
            r#"it = #[Str['bin], Str['bin]] { =[k, t]; %html{ <li>{t}</li> } ~> { :key k } }
               view = #<'e>'%html.nodes<'e> { =items; %html{ <ul>{items}</ul> } }
               f1 = Cons[it ["a", "a"], Cons[it ["b", "b"], Cons[it ["c", "c"], Nil]]] ~> view ~> %html/live.frame
               f2 = Cons[it ["c", "c"], Cons[it ["a", "a"], Cons[it ["b", "b"], Nil]]] ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(r#"Cons[MovRow[path: Cons[0, Nil], from: 2, to: 0], Nil]"#);
}

#[test]
fn test_keyed_middle_removal_is_one_del() {
    quiver()
        .evaluate(
            r#"it = #[Str['bin], Str['bin]] { =[k, t]; %html{ <li>{t}</li> } ~> { :key k } }
               view = #<'e>'%html.nodes<'e> { =items; %html{ <ul>{items}</ul> } }
               f1 = Cons[it ["a", "a"], Cons[it ["b", "b"], Cons[it ["c", "c"], Nil]]] ~> view ~> %html/live.frame
               f2 = Cons[it ["a", "a"], Cons[it ["c", "c"], Nil]] ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(r#"Cons[DelRow[path: Cons[0, Nil], at: 1], Nil]"#);
}

#[test]
fn test_keyed_middle_insert_is_one_ins() {
    quiver()
        .evaluate(
            r#"it = #[Str['bin], Str['bin]] { =[k, t]; %html{ <li>{t}</li> } ~> { :key k } }
               view = #<'e>'%html.nodes<'e> { =items; %html{ <ul>{items}</ul> } }
               f1 = Cons[it ["a", "a"], Cons[it ["c", "c"], Nil]] ~> view ~> %html/live.frame
               f2 = Cons[it ["a", "a"], Cons[it ["b", "b"], Cons[it ["c", "c"], Nil]]] ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(
            r#"Cons[InsRow[path: Cons[0, Nil], at: 1, html: "<!--r--><li><!--q:0-->b<!--/q:0--></li><!--/r-->"], Nil]"#,
        );
}

#[test]
fn test_keyed_kept_row_patches_in_place() {
    // A kept key with changed content patches inside the row.
    quiver()
        .evaluate(
            r#"it = #[Str['bin], Str['bin]] { =[k, t]; %html{ <li>{t}</li> } ~> { :key k } }
               view = #<'e>'%html.nodes<'e> { =items; %html{ <ul>{items}</ul> } }
               f1 = Cons[it ["a", "old"], Nil] ~> view ~> %html/live.frame
               f2 = Cons[it ["a", "new"], Nil] ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(r#"Cons[SetText[path: Cons[0, Cons[0, Cons[0, Nil]]], value: "new"], Nil]"#);
}

#[test]
fn test_mixed_keys_fall_back_to_positional() {
    // One unkeyed row drops the whole list to the positional diff: the reorder
    // becomes in-place rewrites instead of a move.
    quiver()
        .evaluate(
            r#"it = #[Str['bin], Str['bin]] { =[k, t]; %html{ <li>{t}</li> } ~> { :key k } }
               un = #Str['bin] { %html{ <li>{$}</li> } }
               view = #<'e>'%html.nodes<'e> { =items; %html{ <ul>{items}</ul> } }
               f1 = Cons[it ["a", "a"], Cons[un "b", Nil]] ~> view ~> %html/live.frame
               f2 = Cons[un "b", Cons[it ["a", "a"], Nil]] ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(
            r#"Cons[SetText[path: Cons[0, Cons[0, Cons[0, Nil]]], value: "b"], Cons[SetText[path: Cons[0, Cons[1, Cons[0, Nil]]], value: "a"], Nil]]"#,
        );
}

// The sim harness: the session seam with no socket and no browser. `sim` mounts a
// component; `sim_text` feeds a wire envelope through the loop's own parse → decode →
// update path; `sim_event` feeds a decoded event; `sim_changed` is the wakeup path.
// Each step answers the patches a connected client would have been sent.

/// A minimal live component (an int counter) plus a bare GET request.
const COUNTER: &str = r#"
    'ev = Inc | Noop
    counter = [
      mount: #'%http { 0 },
      update: #[(state): 'int, (event): 'ev] { $event ~> { | =Inc => %num.add [$state, 1] | $state } },
      view: #'int { %str.from_int $ ~> %html{ <p>Count: {~}</p> } },
      decode: &%data.decode<Ev['ev]>,
    ]
    req = Request[method: GET, target: "/", path: Nil, query: Nil, version: "HTTP/1.1", headers: Nil, body: 0x]
"#;

#[test]
fn test_sim_mount_answers_frame_zero() {
    // `sim` runs mount and holds frame 0; `sim_html` is the instrumented region HTML
    // a joining client's DOM would show.
    quiver()
        .evaluate(
            &[
                COUNTER,
                r#"%html/live.sim [req, counter] ~> %html/live.sim_html"#,
            ]
            .concat(),
        )
        .expect(r#""<p>Count: <!--q:0-->0<!--/q:0--></p>""#);
}

#[test]
fn test_sim_event_steps_update_and_answers_patches() {
    // Decoded events fold through `update`; each step diffs against the frame the
    // client holds, so consecutive steps answer consecutive deltas.
    quiver()
        .evaluate(
            &[
                COUNTER,
                r#"s = %html/live.sim [req, counter]
                   %html/live.sim_event [s, Inc] ~> =[s2, ps1]
                   %html/live.sim_event [s2, Inc] ~> =[s3, ps2]
                   [ps1, ps2]"#,
            ]
            .concat(),
        )
        .expect(
            r#"[Cons[SetText[path: Cons[0, Nil], value: "1"], Nil], Cons[SetText[path: Cons[0, Nil], value: "2"], Nil]]"#,
        );
}

#[test]
fn test_sim_event_unchanged_view_answers_nil() {
    // Empty-frame suppression: an update that leaves the rendered view unchanged
    // produces no patches — the loop would send nothing.
    quiver()
        .evaluate(
            &[
                COUNTER,
                r#"s = %html/live.sim [req, counter]
                   %html/live.sim_event [s, Noop] ~> =[_, ps]
                   ps"#,
            ]
            .concat(),
        )
        .expect("Nil");
}

#[test]
fn test_sim_text_envelope_decodes_to_patch() {
    // A click's wire envelope — `[vid, payload, metadata]` as the socket Text frame
    // carries it — through parse → decode → update → diff.
    quiver()
        .evaluate(
            &[
                COUNTER,
                r#"s = %html/live.sim [req, counter]
                   %html/live.sim_text [s, "[\"0\", \"Ev[Inc]\", [\"click\", 10, 20, 0, [false, false, false, false]]]"] ~> =[_, ps]
                   ps"#,
            ]
            .concat(),
        )
        .expect(r#"Cons[SetText[path: Cons[0, Nil], value: "1"], Nil]"#);
}

#[test]
fn test_sim_text_drops_malformed_and_forged() {
    // The border checkpoint: unparseable text and a payload `decode` answers nil for
    // are both dropped — no patches, and the state never saw them.
    quiver()
        .evaluate(
            &[
                COUNTER,
                r#"s = %html/live.sim [req, counter]
                   %html/live.sim_text [s, "not json"] ~> =[s2, ps1]
                   %html/live.sim_text [s2, "[\"0\", \"zap\", [\"click\", 10, 20, 0, [false, false, false, false]]]"] ~> =[s3, ps2]
                   [ps1, ps2, %html/live.sim_html s3]"#,
            ]
            .concat(),
        )
        .expect(r#"[Nil, Nil, "<p>Count: <!--q:0-->0<!--/q:0--></p>"]"#);
}

#[test]
fn test_submit_fields_reach_update_as_event_metadata() {
    // A submit's form fields are browser metadata: they ride beside the payload and
    // join the decoded event as the `:event` annotation, which `update` reads back with
    // a checked retrieval. The payload itself is just the marker the view rendered.
    quiver()
        .evaluate(
            r#"'ev = Submit
               comp = [
                 mount: #'%http { "" },
                 update: #[(state): Str['bin], (event): 'ev] {
                   $event:('%html/live.event)event ~> =Submit(fields: f)
                   %http.get [f, "v"]
                 },
                 view: #Str['bin] { %html{ <p>{$}</p> } },
                 decode: &%data.decode<Ev['ev]>,
               ]
               req = Request[method: GET, target: "/", path: Nil, query: Nil, version: "HTTP/1.1", headers: Nil, body: 0x]
               s = %html/live.sim [req, comp]
               %html/live.sim_text [s, "[\"0\", \"Ev[Submit]\", [\"submit\", [[\"v\", \"hello\"]]]]"] ~> =[_, ps]
               ps"#,
        )
        .expect(r#"Cons[SetText[path: Cons[0, Nil], value: "hello"], Nil]"#);
}

#[test]
fn test_event_metadata_is_invisible_to_the_data_plane() {
    // The stamp rides the value without changing it: the event still matches and
    // compares as the bare payload it decoded to, and an `update` that never asks for
    // the metadata is untouched by its presence.
    quiver()
        .evaluate(
            r#"'ev = Inc
               comp = [
                 mount: #'%http { "" },
                 update: #[(state): Str['bin], (event): 'ev] { $event ~> { | =Inc => "bare" | "wrapped" } },
                 view: #Str['bin] { %html{ <p>{$}</p> } },
                 decode: &%data.decode<Ev['ev]>,
               ]
               req = Request[method: GET, target: "/", path: Nil, query: Nil, version: "HTTP/1.1", headers: Nil, body: 0x]
               s = %html/live.sim [req, comp]
               %html/live.sim_text [s, "[\"0\", \"Ev[Inc]\", [\"click\", 1, 2, 0, [false, false, false, false]]]"] ~> =[_, ps]
               ps"#,
        )
        .expect(r#"Cons[SetText[path: Cons[0, Nil], value: "bare"], Nil]"#);
}

#[test]
fn test_change_metadata_carries_checked_state() {
    // A checkbox's `value` is its value attribute whether or not it is ticked, so
    // `checked?` is what says the box is on — the reason `'field` carries both.
    quiver()
        .evaluate(
            r#"'ev = Toggle
               comp = [
                 mount: #'%http { "" },
                 update: #[(state): Str['bin], (event): 'ev] {
                   $event:('%html/live.event)event ~> =Change(value: v, checked?: c)
                   c ~> { | =Ok => %str.concat ["on:", v] | %str.concat ["off:", v] }
                 },
                 view: #Str['bin] { %html{ <p>{$}</p> } },
                 decode: &%data.decode<Ev['ev]>,
               ]
               req = Request[method: GET, target: "/", path: Nil, query: Nil, version: "HTTP/1.1", headers: Nil, body: 0x]
               s = %html/live.sim [req, comp]
               %html/live.sim_text [s, "[\"0\", \"Ev[Toggle]\", [\"change\", \"yes\", true]]"] ~> =[s2, _]
               %html/live.sim_text [s2, "[\"0\", \"Ev[Toggle]\", [\"change\", \"yes\", false]]"] ~> =[_, ps]
               ps"#,
        )
        .expect(r#"Cons[SetText[path: Cons[0, Nil], value: "off:yes"], Nil]"#);
}

#[test]
fn test_input_and_change_share_one_shape() {
    // `input` (per keystroke) and `change` (on commit) report the same thing, so they
    // spread one `'field` type and an update can read either the same way.
    quiver()
        .evaluate(
            r#"'ev = Edit
               comp = [
                 mount: #'%http { "" },
                 update: #[(state): Str['bin], (event): 'ev] {
                   $event:('%html/live.event)event ~> =(value: v)
                   v
                 },
                 view: #Str['bin] { %html{ <p>{$}</p> } },
                 decode: &%data.decode<Ev['ev]>,
               ]
               req = Request[method: GET, target: "/", path: Nil, query: Nil, version: "HTTP/1.1", headers: Nil, body: 0x]
               s = %html/live.sim [req, comp]
               %html/live.sim_text [s, "[\"0\", \"Ev[Edit]\", [\"input\", \"typed\", false]]"] ~> =[s2, _]
               %html/live.sim_text [s2, "[\"0\", \"Ev[Edit]\", [\"change\", \"committed\", false]]"] ~> =[_, ps]
               ps"#,
        )
        .expect(r#"Cons[SetText[path: Cons[0, Nil], value: "committed"], Nil]"#);
}

#[test]
fn test_keydown_metadata_carries_key_and_modifiers() {
    // Which keys reach the server is the client's `keys` filter; what arrives is the
    // key name and the modifiers held with it.
    quiver()
        .evaluate(
            r#"'ev = Commit
               comp = [
                 mount: #'%http { "" },
                 update: #[(state): Str['bin], (event): 'ev] {
                   $event:('%html/live.event)event ~> =KeyDown(key: k, mods: m)
                   m ~> =(ctrl?: Ok)
                   k
                 },
                 view: #Str['bin] { %html{ <p>{$}</p> } },
                 decode: &%data.decode<Ev['ev]>,
               ]
               req = Request[method: GET, target: "/", path: Nil, query: Nil, version: "HTTP/1.1", headers: Nil, body: 0x]
               s = %html/live.sim [req, comp]
               %html/live.sim_text [s, "[\"0\", \"Ev[Commit]\", [\"keydown\", \"Enter\", [false, true, false, false]]]"] ~> =[_, ps]
               ps"#,
        )
        .expect(r#"Cons[SetText[path: Cons[0, Nil], value: "Enter"], Nil]"#);
}

#[test]
fn test_click_metadata_carries_coordinates_and_modifiers() {
    // Every event name has its own metadata shape; a click's is its coordinates,
    // button, and modifier keys.
    quiver()
        .evaluate(
            r#"'ev = Mark
               comp = [
                 mount: #'%http { "" },
                 update: #[(state): Str['bin], (event): 'ev] {
                   $event:('%html/live.event)event ~> =Click(x: x, y: y, mods: m)
                   m ~> =(shift?: Ok)
                   %str.concat [%str.from_int x, %str.concat [",", %str.from_int y]]
                 },
                 view: #Str['bin] { %html{ <p>{$}</p> } },
                 decode: &%data.decode<Ev['ev]>,
               ]
               req = Request[method: GET, target: "/", path: Nil, query: Nil, version: "HTTP/1.1", headers: Nil, body: 0x]
               s = %html/live.sim [req, comp]
               %html/live.sim_text [s, "[\"0\", \"Ev[Mark]\", [\"click\", 30, 40, 0, [false, false, true, false]]]"] ~> =[_, ps]
               ps"#,
        )
        .expect(r#"Cons[SetText[path: Cons[0, Nil], value: "30,40"], Nil]"#);
}

// The socket round-trip test lives in tests/live_socket.rs — its own test binary, so
// it runs without sibling-test thread contention (the harness's free-running virtual
// clock turns the stack's virtual-time windows into real-time races under load).

#[test]
fn test_redirect_marks_state_without_changing_it() {
    // Server-initiated navigation asks for a URL change by marking the state `update`
    // returns. The mark rides the value invisibly — the state matches and prints exactly
    // as before — and the view loop reads the target with a checked retrieval.
    quiver()
        .evaluate(
            r#"s = [page: "x", clicks: 3] ~> %html/live.redirect [~, "/posts/7"]
               [s, s:('%str)redirect, s ~> =(page: "x")]"#,
        )
        .expect(r#"[[page: "x", clicks: 3], "/posts/7", Ok]"#);
}

#[test]
fn test_redirect_composes_inside_generic_app_code() {
    // The mark is attached through a type variable, so an app's own generic helper can
    // set it — not only code that knows the state's concrete type.
    quiver()
        .evaluate(
            r#"to_home = #<'s>'s { %html/live.redirect [$, "/"] }
               s = to_home [page: "x"]
               s:('%str)redirect"#,
        )
        .expect(r#""/""#);
}

#[test]
fn test_sim_changed_rerenders_after_store_step() {
    // The reactive path, socketless: the sim's tracked render subscribes the calling
    // process to the sampled store, so the test receives the `Changed` wakeup itself
    // and then applies `sim_changed` — exactly the loop's select-then-rerender.
    quiver()
        .evaluate(
            r#"'state = [store: (@'int ?'int)]
               mkcomp = #(@'int ?'int) {
                 =store
                 [
                   mount: #'%http { [store: &store] },
                   update: #[(state): 'state, (event): Ok] { $state },
                   view: #'state { =(store: st); ?st ~> %str.from_int ~> %html{ <p>{~}</p> } },
                   decode: #'%str { [] },
                 ]
               }
               st = 0 ~> @'int { !'int ~> { =n => ^ n } }
               req = Request[method: GET, target: "/", path: Nil, query: Nil, version: "HTTP/1.1", headers: Nil, body: 0x]
               s = %html/live.sim [req, mkcomp &st]
               7 ~> st
               w = ![#'%proc.changed, 2000] ~> { ='%proc.changed => Woke | TimedOut }
               %html/live.sim_changed s ~> =[_, ps]
               [w, ps]"#,
        )
        .expect(r#"[Woke, Cons[SetText[path: Cons[0, Nil], value: "7"], Nil]]"#);
}

#[test]
fn test_pinned_component_rejects_stray_event_payload() {
    // `component<'ev>` pins the wire-event type: a view emitting a payload outside the
    // pinned union is a compile error naming the offending event (unpinned, the
    // inferred union would silently widen and the payload only be dropped at decode).
    quiver()
        .evaluate(
            r#"'ev = Inc | Dec
               bad = %html/live.component<'ev> [
                 mount: #[] { 0 },
                 update: #[(state): 'int, (event): 'ev] { $state },
                 view: #'int { %html{ <button on:click={Bogus}>+</button> } },
                 decode: &%data.decode<Ev['ev]>,
               ]
               Ok"#,
        )
        .expect_error_containing("Bogus does not fit Dec | Inc");
}

#[test]
fn test_pinned_component_accepts_matching_events() {
    // The positive half: a pinned component whose payloads all fit compiles, and the
    // sim round-trips an event through the same seams.
    quiver()
        .evaluate(
            r#"'ev = Inc | Dec
               c = %html/live.component<'ev> [
                 mount: #[] { 0 },
                 update: #[(state): 'int, (event): 'ev] {
                   $event ~> { | =Inc => %num.add [$state, 1] | %num.sub [$state, 1] }
                 },
                 view: #'int { %html{ <button on:click={Inc}>{$}</button> } },
                 decode: &%data.decode<Ev['ev]>,
               ]
               Ok"#,
        )
        .expect("Ok");
}

#[test]
fn test_one_event_union_folds_markers_and_payloads_together() {
    // One event type spans both kinds of event: a marker whose data the browser
    // supplies (`Submit`) and a payload the view rendered in full (`Del[id]`). `update`
    // folds them together, reaching for the metadata only for the marker.
    quiver()
        .evaluate(
            r#"'ev = Submit | Del['int]
               comp = [
                 mount: #[] { "start" },
                 update: #[(state): Str['bin], (event): 'ev] {
                   $event ~> {
                     | =Del[_] => "deleted"
                     | =Submit => {
                       $event:('%html/live.event)event ~> =Submit(fields: f)
                       %http.get [f, "t"]
                     }
                   }
                 },
                 view: #Str['bin] { %html{ <form on:submit={Submit}><b>{$}</b></form> } },
                 decode: &%data.decode<Ev['ev]>,
               ]
               s = %html/live.sim [[], comp]
               %html/live.sim_text [s, "[\"0\", \"Ev[Submit]\", [\"submit\", [[\"t\", \"typed\"]]]]"] ~> =[_, ps]
               ps"#,
        )
        .expect(r#"Cons[SetText[path: Cons[1, Nil], value: "typed"], Nil]"#);
}

#[test]
fn test_event_attribute_frames_encode_to_data_notation() {
    // Frames are event-type-erased: an `on:*` hole's `Ev` value is encoded to its
    // %data notation at frame derivation, so an event change patches as a plain
    // attribute string.
    quiver()
        .evaluate(
            r#"view = #'int { %html{ <button on:click={Del[$]}>x</button> } }
               f1 = 1 ~> view ~> %html/live.frame
               f2 = 2 ~> view ~> %html/live.frame
               %html/live.diff [f1, f2]"#,
        )
        .expect(
            r#"Cons[SetAttr[path: Cons[0, Nil], elem: 0, name: "data-q-click", value: "Ev[Del[2]]"], Nil]"#,
        );
}
