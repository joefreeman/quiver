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
const DA: &str = "%dict.new ~> %dict.put [~, 0x0a59538016, 1] ~> %dict.put [~, 0xfd3eb827ca, 2]";

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
        .evaluate(&format!("{DA} ~> %dict.get [~, {KA}]"))
        .expect("1");
    quiver()
        .evaluate(&format!("{DA} ~> %dict.get [~, {KB}]"))
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
            "{DA} ~> %dict.put [~, {KA}, 9] ~> %dict.get [~, {KA}]"
        ))
        .expect("9");
    quiver()
        .evaluate(&format!(
            "{DA} ~> %dict.put [~, {KA}, 9] ~> %dict.get [~, {KB}]"
        ))
        .expect("2");
    quiver()
        .evaluate(&format!("{DA} ~> %dict.put [~, {KA}, 9] ~> %dict.count"))
        .expect("2");
}

#[test]
fn test_collision_remove_one() {
    // removing one colliding key leaves the other retrievable; the removed key is gone
    quiver()
        .evaluate(&format!(
            "{DA} ~> %dict.remove [~, {KA}] ~> %dict.get [~, {KB}]"
        ))
        .expect("2");
    quiver()
        .evaluate(&format!(
            "{DA} ~> %dict.remove [~, {KA}] ~> %dict.get [~, {KA}]"
        ))
        .expect("[]");
    quiver()
        .evaluate(&format!("{DA} ~> %dict.remove [~, {KA}] ~> %dict.count"))
        .expect("1");
}

#[test]
fn test_collision_remove_absent() {
    // removing a non-colliding, absent key leaves the bucket intact
    quiver()
        .evaluate(&format!(r#"{DA} ~> %dict.remove [~, "zz"] ~> %dict.count"#))
        .expect("2");
}

#[test]
fn test_collision_remove_all() {
    // emptying a collision bucket collapses to Empty (no nil corruption) and the dict is reusable
    quiver()
        .evaluate(&format!(
            "{DA} ~> %dict.remove [~, {KA}] ~> %dict.remove [~, {KB}] ~> %dict.count"
        ))
        .expect("0");
    quiver()
        .evaluate(&format!(r#"{DA} ~> %dict.remove [~, {KA}] ~> %dict.remove [~, {KB}] ~> %dict.put [~, "c", 5] ~> %dict.get [~, "c"]"#))
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
            d1 = %dict.new ~> %dict.put [~, "alpha", 1] ~> %dict.put [~, "bravo", 2] ~> %dict.put [~, "charlie", 3],
            d2 = %dict.new ~> %dict.put [~, "charlie", 3] ~> %dict.put [~, "bravo", 2] ~> %dict.put [~, "alpha", 1],
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
            d1 = %dict.new ~> %dict.put [~, "alpha", 1] ~> %dict.put [~, "bravo", 2] ~> %dict.put [~, "charlie", 3] ~> %dict.put [~, "delta", 4] ~> %dict.remove [~, "charlie"],
            d2 = %dict.new ~> %dict.put [~, "alpha", 1] ~> %dict.put [~, "bravo", 2] ~> %dict.put [~, "delta", 4],
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
            "[{DA} ~> %dict.remove [~, {KA}], %dict.new ~> %dict.put [~, {KB}, 2]] ~> == ~> <> ~> <>"
        ))
        .expect("Ok");
}

#[test]
fn test_remove_collapses_node_to_leaf() {
    // removing one of two keys under a Node hoists the lone surviving Leaf up to where the Node
    // was — the same shape as a one-key dict — rather than leaving a degenerate single-child Node.
    quiver()
        .evaluate(r#"%dict.new ~> %dict.put [~, "a", 1] ~> %dict.put [~, "b", 2] ~> %dict.remove [~, "b"]"#)
        .expect(r#"Leaf[3826002220, "a", 1]"#);
}

#[test]
fn test_remove_collapses_collision_to_leaf() {
    // removing one of two colliding keys collapses the Collision bucket back to a Leaf
    quiver()
        .evaluate(&format!("{DA} ~> %dict.remove [~, {KA}]"))
        .expect(r#"Leaf[3521592947, 0xfd3eb827ca, 2]"#);
}

#[test]
fn test_new_get_empty() {
    quiver()
        .evaluate(r#"%dict.new ~> %dict.get [~, "a"]"#)
        .expect("[]");
}

#[test]
fn test_put_get() {
    quiver()
        .evaluate(
            r#"%dict.new ~> %dict.put [~, "a", 1] ~> %dict.put [~, "b", 2] ~> %dict.get [~, "b"]"#,
        )
        .expect("2");
}

#[test]
fn test_get_missing() {
    quiver()
        .evaluate(r#"%dict.new ~> %dict.put [~, "a", 1] ~> %dict.get [~, "z"]"#)
        .expect("[]");
}

#[test]
fn test_overwrite() {
    quiver()
        .evaluate(
            r#"%dict.new ~> %dict.put [~, "a", 1] ~> %dict.put [~, "a", 99] ~> %dict.get [~, "a"]"#,
        )
        .expect("99");
}

#[test]
fn test_overwrite_keeps_count() {
    quiver()
        .evaluate(r#"%dict.new ~> %dict.put [~, "a", 1] ~> %dict.put [~, "a", 9] ~> %dict.count"#)
        .expect("1");
}

#[test]
fn test_binary_keys() {
    quiver()
        .evaluate("%dict.new ~> %dict.put [~, 0x01, 7] ~> %dict.get [~, 0x01]")
        .expect("7");
}

#[test]
fn test_has() {
    quiver()
        .evaluate(r#"%dict.new ~> %dict.put [~, "a", 1] ~> %dict.has? [~, "a"]"#)
        .expect("Ok");
    quiver()
        .evaluate(r#"%dict.new ~> %dict.put [~, "a", 1] ~> %dict.has? [~, "q"]"#)
        .expect("[]");
}

#[test]
fn test_remove() {
    quiver()
        .evaluate(r#"%dict.new ~> %dict.put [~, "a", 1] ~> %dict.put [~, "b", 2] ~> %dict.remove [~, "b"] ~> %dict.get [~, "b"]"#)
        .expect("[]");
}

#[test]
fn test_remove_keeps_others() {
    quiver()
        .evaluate(r#"%dict.new ~> %dict.put [~, "a", 1] ~> %dict.put [~, "b", 2] ~> %dict.remove [~, "b"] ~> %dict.get [~, "a"]"#)
        .expect("1");
}

#[test]
fn test_remove_absent_noop() {
    quiver()
        .evaluate(r#"%dict.new ~> %dict.put [~, "a", 1] ~> %dict.remove [~, "z"] ~> %dict.count"#)
        .expect("1");
}

#[test]
fn test_immutability() {
    // Updating a binding doesn't mutate the original dict it was derived from.
    quiver()
        .evaluate(
            r#"
            d = %dict.new ~> %dict.put [~, "a", 1],
            d ~> %dict.put [~, "a", 99],
            d ~> %dict.get [~, "a"]
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
        .evaluate(&format!(r#"{FROM} ~> %dict.get [~, "golf"]"#))
        .expect("7");
    quiver()
        .evaluate(&format!(r#"{FROM} ~> %dict.get [~, "lima"]"#))
        .expect("12");
    quiver()
        .evaluate(&format!(r#"{FROM} ~> %dict.get [~, "alpha"]"#))
        .expect("1");
}

#[test]
fn test_remove_many_then_count() {
    quiver()
        .evaluate(&format!(
            r#"{FROM} ~> %dict.remove [~, "echo"] ~> %dict.count"#
        ))
        .expect("11");
}

#[test]
fn test_remove_many_then_get() {
    quiver()
        .evaluate(&format!(
            r#"{FROM} ~> %dict.remove [~, "echo"] ~> %dict.get [~, "echo"]"#
        ))
        .expect("[]");
    // an unrelated key survives the removal
    quiver()
        .evaluate(&format!(
            r#"{FROM} ~> %dict.remove [~, "echo"] ~> %dict.get [~, "india"]"#
        ))
        .expect("9");
}

#[test]
fn test_from_later_wins() {
    quiver()
        .evaluate(r#"Cons[["k", 1], Cons[["k", 2], Nil]] ~> %dict.from ~> %dict.get [~, "k"]"#)
        .expect("2");
}

#[test]
fn test_merge_b_wins() {
    quiver()
        .evaluate(
            r#"
            a = %dict.new ~> %dict.put [~, "k", 1],
            b = %dict.new ~> %dict.put [~, "k", 9],
            %dict.merge [a, b] ~> %dict.get [~, "k"]
        "#,
        )
        .expect("9");
}

#[test]
fn test_merge_keeps_disjoint() {
    quiver()
        .evaluate(
            r#"
            a = %dict.new ~> %dict.put [~, "x", 1],
            b = %dict.new ~> %dict.put [~, "y", 2],
            %dict.merge [a, b] ~> %dict.get [~, "x"]
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
            "{FROM} ~> %dict.iter ~> %iter.map [~, #['bin, 'int] {{ $1 }}] ~> %iter.fold [~, 0, #['int, 'int] {{ %num.add }}]"
        ))
        .expect("78");
}

#[test]
fn test_keys_present() {
    // every original key is found via has?
    quiver()
        .evaluate(&format!(r#"{FROM} ~> =d, ["alpha", "delta", "kilo"] ~> {{ =[a, b, c], [d ~> %dict.has? [~, a], d ~> %dict.has? [~, b], d ~> %dict.has? [~, c]] }}"#))
        .expect("[Ok, Ok, Ok]");
}

#[test]
fn test_repl_continuation_across_lines() {
    // Each REPL line stores its result type and feeds it (as `~`) into the next. The tree ops are
    // annotated `-> '<'v>` so that result type stays the compact node type instead of a recursive
    // type that balloons across lines — which previously overflowed the stack on the third line.
    quiver()
        .evaluate("%dict.new")
        .then_evaluate(r#"%dict.put [~, "a", 1]"#)
        .then_evaluate(r#"%dict.put [~, "b", 2]"#)
        .then_evaluate(r#"%dict.put [~, "c", 3]"#)
        .then_evaluate(r#"%dict.remove [~, "a"]"#)
        .then_evaluate(r#"%dict.get [~, "c"]"#)
        .expect("3");
}

#[test]
fn test_string_and_binary_keys_distinct() {
    // The string "A" (Str[0x41]) and the raw binary 0x41 are distinct keys.
    quiver()
        .evaluate(
            r#"%dict.new ~> %dict.put [~, "A", 1] ~> %dict.put [~, 0x41, 2] ~> %dict.get [~, "A"]"#,
        )
        .expect("1");
    quiver()
        .evaluate(r#"%dict.new ~> %dict.put [~, "A", 1] ~> %dict.put [~, 0x41, 2] ~> %dict.get [~, 0x41]"#)
        .expect("2");
}
