mod common;
use common::*;

// Build a dict from a list of [key, value] pairs (helper prelude for the tests below).
const FROM: &str = r#"
    Cons[["alpha", 1], Cons[["bravo", 2], Cons[["charlie", 3], Cons[["delta", 4],
    Cons[["echo", 5], Cons[["foxtrot", 6], Cons[["golf", 7], Cons[["hotel", 8],
    Cons[["india", 9], Cons[["juliet", 10], Cons[["kilo", 11], Cons[["lima", 12],
    Nil]]]]]]]]]]]] ~> %dict.from
"#;

// Two distinct binary keys with the same 32-bit FNV-1a hash (3521592947), found by search.
// FNV-1a is injective over short inputs, so a colliding pair is needed to exercise the
// `Collision`-node / bucket code paths that ordinary keys never reach. `DA` builds a dict
// holding both (which forms a single `Collision` node).
const KA: &str = "0x0a59538016";
const KB: &str = "0xfd3eb827ca";
const DA: &str =
    "%dict.new ~> [~, 0x0a59538016, 1] ~> %dict.put ~> [~, 0xfd3eb827ca, 2] ~> %dict.put";

#[test]
fn test_collision_forms_collision_node() {
    // Documents that the two keys really do collide: both land in one Collision bucket. If FNV
    // ever changes, this fails loudly (the other collision tests would otherwise silently stop
    // testing collisions).
    quiver()
        .evaluate(DA)
        .expect("Collision[3521592947, Cons[[0x0a59538016, 1], Cons[[0xfd3eb827ca, 2], Nil]]]");
}

#[test]
fn test_collision_get_both() {
    quiver()
        .evaluate(&format!("{DA} ~> [~, {KA}] ~> %dict.get"))
        .expect("1");
    quiver()
        .evaluate(&format!("{DA} ~> [~, {KB}] ~> %dict.get"))
        .expect("2");
}

#[test]
fn test_collision_count() {
    quiver()
        .evaluate(&format!("{DA} ~> %dict.count"))
        .expect("2");
}

#[test]
fn test_collision_overwrite() {
    // overwriting one colliding key updates only its bucket entry, leaving the other intact
    quiver()
        .evaluate(&format!(
            "{DA} ~> [~, {KA}, 9] ~> %dict.put ~> [~, {KA}] ~> %dict.get"
        ))
        .expect("9");
    quiver()
        .evaluate(&format!(
            "{DA} ~> [~, {KA}, 9] ~> %dict.put ~> [~, {KB}] ~> %dict.get"
        ))
        .expect("2");
    quiver()
        .evaluate(&format!("{DA} ~> [~, {KA}, 9] ~> %dict.put ~> %dict.count"))
        .expect("2");
}

#[test]
fn test_collision_remove_one() {
    // removing one colliding key leaves the other retrievable; the removed key is gone
    quiver()
        .evaluate(&format!(
            "{DA} ~> [~, {KA}] ~> %dict.remove ~> [~, {KB}] ~> %dict.get"
        ))
        .expect("2");
    quiver()
        .evaluate(&format!(
            "{DA} ~> [~, {KA}] ~> %dict.remove ~> [~, {KA}] ~> %dict.get"
        ))
        .expect("[]");
    quiver()
        .evaluate(&format!("{DA} ~> [~, {KA}] ~> %dict.remove ~> %dict.count"))
        .expect("1");
}

#[test]
fn test_collision_remove_absent() {
    // removing a non-colliding, absent key leaves the bucket intact
    quiver()
        .evaluate(&format!(
            r#"{DA} ~> [~, "zz"] ~> %dict.remove ~> %dict.count"#
        ))
        .expect("2");
}

#[test]
fn test_collision_remove_all() {
    // emptying a collision bucket collapses to Empty (no nil corruption) and the dict is reusable
    quiver()
        .evaluate(&format!(
            "{DA} ~> [~, {KA}] ~> %dict.remove ~> [~, {KB}] ~> %dict.remove ~> %dict.count"
        ))
        .expect("0");
    quiver()
        .evaluate(&format!(r#"{DA} ~> [~, {KA}] ~> %dict.remove ~> [~, {KB}] ~> %dict.remove ~> [~, "c", 5] ~> %dict.put ~> [~, "c"] ~> %dict.get"#))
        .expect("5");
}

// Canonical form: a dict's structure depends only on its contents, so dicts with equal contents
// are `==` regardless of how they were built — insertion order, and insert-then-remove vs. a
// direct build. (Relies on structural `==`; see test_structural_equality_across_construction.)

#[test]
fn test_canonical_insertion_order_independent() {
    quiver()
        .evaluate(
            r#"
            d1 = %dict.new ~> [~, "alpha", 1] ~> %dict.put ~> [~, "bravo", 2] ~> %dict.put ~> [~, "charlie", 3] ~> %dict.put,
            d2 = %dict.new ~> [~, "charlie", 3] ~> %dict.put ~> [~, "bravo", 2] ~> %dict.put ~> [~, "alpha", 1] ~> %dict.put,
            [d1, d2] ~> == ~> <> ~> <>
        "#,
        )
        .expect("Ok");
}

#[test]
fn test_canonical_remove_matches_direct_build() {
    // building {a,b,c,d} then removing c equals directly building {a,b,d}
    quiver()
        .evaluate(
            r#"
            d1 = %dict.new ~> [~, "alpha", 1] ~> %dict.put ~> [~, "bravo", 2] ~> %dict.put ~> [~, "charlie", 3] ~> %dict.put ~> [~, "delta", 4] ~> %dict.put ~> [~, "charlie"] ~> %dict.remove,
            d2 = %dict.new ~> [~, "alpha", 1] ~> %dict.put ~> [~, "bravo", 2] ~> %dict.put ~> [~, "delta", 4] ~> %dict.put,
            [d1, d2] ~> == ~> <> ~> <>
        "#,
        )
        .expect("Ok");
}

#[test]
fn test_canonical_collision_remove_matches_direct_build() {
    // a collision that loses one entry equals directly building the single-key dict
    quiver()
        .evaluate(&format!(
            "[{DA} ~> [~, {KA}] ~> %dict.remove, %dict.new ~> [~, {KB}, 2] ~> %dict.put] ~> == ~> <> ~> <>"
        ))
        .expect("Ok");
}

#[test]
fn test_remove_collapses_node_to_leaf() {
    // removing one of two keys under a Node hoists the lone surviving Leaf up to where the Node
    // was — the same shape as a one-key dict — rather than leaving a degenerate single-child Node.
    quiver()
        .evaluate(r#"%dict.new ~> [~, "a", 1] ~> %dict.put ~> [~, "b", 2] ~> %dict.put ~> [~, "b"] ~> %dict.remove"#)
        .expect(r#"Leaf[3826002220, "a", 1]"#);
}

#[test]
fn test_remove_collapses_collision_to_leaf() {
    // removing one of two colliding keys collapses the Collision bucket back to a Leaf
    quiver()
        .evaluate(&format!("{DA} ~> [~, {KA}] ~> %dict.remove"))
        .expect(r#"Leaf[3521592947, 0xfd3eb827ca, 2]"#);
}

#[test]
fn test_new_get_empty() {
    quiver()
        .evaluate(r#"%dict.new ~> [~, "a"] ~> %dict.get"#)
        .expect("[]");
}

#[test]
fn test_put_get() {
    quiver()
        .evaluate(
            r#"%dict.new ~> [~, "a", 1] ~> %dict.put ~> [~, "b", 2] ~> %dict.put ~> [~, "b"] ~> %dict.get"#,
        )
        .expect("2");
}

#[test]
fn test_get_missing() {
    quiver()
        .evaluate(r#"%dict.new ~> [~, "a", 1] ~> %dict.put ~> [~, "z"] ~> %dict.get"#)
        .expect("[]");
}

#[test]
fn test_overwrite() {
    quiver()
        .evaluate(
            r#"%dict.new ~> [~, "a", 1] ~> %dict.put ~> [~, "a", 99] ~> %dict.put ~> [~, "a"] ~> %dict.get"#,
        )
        .expect("99");
}

#[test]
fn test_overwrite_keeps_count() {
    quiver()
        .evaluate(
            r#"%dict.new ~> [~, "a", 1] ~> %dict.put ~> [~, "a", 9] ~> %dict.put ~> %dict.count"#,
        )
        .expect("1");
}

#[test]
fn test_binary_keys() {
    quiver()
        .evaluate("%dict.new ~> [~, 0x01, 7] ~> %dict.put ~> [~, 0x01] ~> %dict.get")
        .expect("7");
}

#[test]
fn test_has() {
    quiver()
        .evaluate(r#"%dict.new ~> [~, "a", 1] ~> %dict.put ~> [~, "a"] ~> %dict.has?"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"%dict.new ~> [~, "a", 1] ~> %dict.put ~> [~, "q"] ~> %dict.has?"#)
        .expect("[]");
}

#[test]
fn test_remove() {
    quiver()
        .evaluate(r#"%dict.new ~> [~, "a", 1] ~> %dict.put ~> [~, "b", 2] ~> %dict.put ~> [~, "b"] ~> %dict.remove ~> [~, "b"] ~> %dict.get"#)
        .expect("[]");
}

#[test]
fn test_remove_keeps_others() {
    quiver()
        .evaluate(r#"%dict.new ~> [~, "a", 1] ~> %dict.put ~> [~, "b", 2] ~> %dict.put ~> [~, "b"] ~> %dict.remove ~> [~, "a"] ~> %dict.get"#)
        .expect("1");
}

#[test]
fn test_remove_absent_noop() {
    quiver()
        .evaluate(
            r#"%dict.new ~> [~, "a", 1] ~> %dict.put ~> [~, "z"] ~> %dict.remove ~> %dict.count"#,
        )
        .expect("1");
}

#[test]
fn test_immutability() {
    // Updating a binding doesn't mutate the original dict it was derived from.
    quiver()
        .evaluate(
            r#"
            d = %dict.new ~> [~, "a", 1] ~> %dict.put,
            d ~> [~, "a", 99] ~> %dict.put,
            d ~> [~, "a"] ~> %dict.get
        "#,
        )
        .expect("1");
}

#[test]
fn test_count_many() {
    quiver()
        .evaluate(&format!("{FROM} ~> %dict.count"))
        .expect("12");
}

#[test]
fn test_get_many() {
    quiver()
        .evaluate(&format!(r#"{FROM} ~> [~, "golf"] ~> %dict.get"#))
        .expect("7");
    quiver()
        .evaluate(&format!(r#"{FROM} ~> [~, "lima"] ~> %dict.get"#))
        .expect("12");
    quiver()
        .evaluate(&format!(r#"{FROM} ~> [~, "alpha"] ~> %dict.get"#))
        .expect("1");
}

#[test]
fn test_remove_many_then_count() {
    quiver()
        .evaluate(&format!(
            r#"{FROM} ~> [~, "echo"] ~> %dict.remove ~> %dict.count"#
        ))
        .expect("11");
}

#[test]
fn test_remove_many_then_get() {
    quiver()
        .evaluate(&format!(
            r#"{FROM} ~> [~, "echo"] ~> %dict.remove ~> [~, "echo"] ~> %dict.get"#
        ))
        .expect("[]");
    // an unrelated key survives the removal
    quiver()
        .evaluate(&format!(
            r#"{FROM} ~> [~, "echo"] ~> %dict.remove ~> [~, "india"] ~> %dict.get"#
        ))
        .expect("9");
}

#[test]
fn test_from_later_wins() {
    quiver()
        .evaluate(r#"Cons[["k", 1], Cons[["k", 2], Nil]] ~> %dict.from ~> [~, "k"] ~> %dict.get"#)
        .expect("2");
}

#[test]
fn test_merge_b_wins() {
    quiver()
        .evaluate(
            r#"
            a = %dict.new ~> [~, "k", 1] ~> %dict.put,
            b = %dict.new ~> [~, "k", 9] ~> %dict.put,
            [a, b] ~> %dict.merge ~> [~, "k"] ~> %dict.get
        "#,
        )
        .expect("9");
}

#[test]
fn test_merge_keeps_disjoint() {
    quiver()
        .evaluate(
            r#"
            a = %dict.new ~> [~, "x", 1] ~> %dict.put,
            b = %dict.new ~> [~, "y", 2] ~> %dict.put,
            [a, b] ~> %dict.merge ~> [~, "x"] ~> %dict.get
        "#,
        )
        .expect("1");
}

#[test]
fn test_iter_count() {
    quiver()
        .evaluate(&format!("{FROM} ~> %dict.iter ~> %iter.count"))
        .expect("12");
}

#[test]
fn test_iter_sum_values() {
    // sum every value via the entry iterator: 1 + 2 + ... + 12 = 78
    quiver()
        .evaluate(&format!(
            "{FROM} ~> %dict.iter ~> [~, #['bin, 'int] {{ $1 }}] ~> %iter.map ~> [~, 0, #['int, 'int] {{ %num.add }}] ~> %iter.fold"
        ))
        .expect("78");
}

#[test]
fn test_keys_present() {
    // every original key is found via has?
    quiver()
        .evaluate(&format!(r#"{FROM} ~> =d, ["alpha", "delta", "kilo"] ~> {{ =[a, b, c], [d ~> [~, a] ~> %dict.has?, d ~> [~, b] ~> %dict.has?, d ~> [~, c] ~> %dict.has?] }}"#))
        .expect("[Ok, Ok, Ok]");
}

#[test]
fn test_repl_continuation_across_lines() {
    // Each REPL line stores its result type and feeds it (as `~`) into the next. The tree ops are
    // annotated `-> '<'v>` so that result type stays the compact node type instead of a recursive
    // type that balloons across lines — which previously overflowed the stack on the third line.
    quiver()
        .evaluate("%dict.new")
        .then_evaluate(r#"[~, "a", 1] ~> %dict.put"#)
        .then_evaluate(r#"[~, "b", 2] ~> %dict.put"#)
        .then_evaluate(r#"[~, "c", 3] ~> %dict.put"#)
        .then_evaluate(r#"[~, "a"] ~> %dict.remove"#)
        .then_evaluate(r#"[~, "c"] ~> %dict.get"#)
        .expect("3");
}

#[test]
fn test_string_and_binary_keys_distinct() {
    // The string "A" (Str[0x41]) and the raw binary 0x41 are distinct keys.
    quiver()
        .evaluate(
            r#"%dict.new ~> [~, "A", 1] ~> %dict.put ~> [~, 0x41, 2] ~> %dict.put ~> [~, "A"] ~> %dict.get"#,
        )
        .expect("1");
    quiver()
        .evaluate(r#"%dict.new ~> [~, "A", 1] ~> %dict.put ~> [~, 0x41, 2] ~> %dict.put ~> [~, 0x41] ~> %dict.get"#)
        .expect("2");
}
