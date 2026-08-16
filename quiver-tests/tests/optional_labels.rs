// Optional field labels: a tuple type may mark a field's label omittable with
// `[(foo): 'int]`. A tuple literal checked against such a type (a call argument)
// may omit the marked labels; the constructed value adopts them positionally, so it
// is fully labeled regardless of spelling — matching, equality, and partial access
// see one shape. The marker is spelling metadata, never part of type identity.

mod common;
use common::*;

#[test]
fn test_positional_call_adopts_labels() {
    quiver()
        .evaluate("f = #[(foo): 'int, (bar): 'int] { $foo }; f [1, 2]")
        .expect("1");
}

#[test]
fn test_named_call_still_works() {
    quiver()
        .evaluate("f = #[(foo): 'int, (bar): 'int] { $bar }; f [foo: 1, bar: 2]")
        .expect("2");
}

#[test]
fn test_mixed_positional_and_named() {
    quiver()
        .evaluate("f = #[(foo): 'int, (bar): 'int] { $foo }; f [1, bar: 2]")
        .expect("1");
}

#[test]
fn test_wrong_stated_label_rejected() {
    quiver()
        .evaluate("f = #[(foo): 'int, (bar): 'int] { $foo }; f [oof: 1, 2]")
        .expect_type_mismatch();
}

#[test]
fn test_reordered_labels_resolve() {
    // A labeled entry names its slot, so labels may be written in any order — the value is
    // built in the expected type's canonical order either way.
    quiver()
        .evaluate("f = #[(foo): 'int, (bar): 'int] { $foo }; f [bar: 2, foo: 1]")
        .expect("1");
    // Ordering is independent of the marker: it needs only a known expected tuple type.
    quiver()
        .evaluate("f = #[foo: 'int, bar: 'int] { $foo }; f [bar: 2, foo: 1]")
        .expect("1");
}

#[test]
fn test_positional_entry_after_label_rejected() {
    // Adoption is positional, so a trailing positional entry has no well-defined slot
    // once labels have claimed some out of order.
    quiver()
        .evaluate("f = #[(foo): 'int, (bar): 'int] { $bar }; f [bar: 2, 1]")
        .expect_type_mismatch();
}

#[test]
fn test_bare_literal_keeps_written_order() {
    // With no expected type there is no canonical order to reorder to.
    quiver()
        .evaluate("x = [bar: 2, foo: 1]; x")
        .expect("[bar: 2, foo: 1]");
}

#[test]
fn test_unmarked_labels_still_required() {
    quiver()
        .evaluate("f = #[foo: 'int, bar: 'int] { $foo }; f [1, 2]")
        .expect_type_mismatch();
}

#[test]
fn test_marker_is_per_field() {
    // Only `foo` is marked, so `bar` must still be labeled.
    quiver()
        .evaluate("f = #[(foo): 'int, bar: 'int] { $foo }; f [1, 2]")
        .expect_type_mismatch();
}

#[test]
fn test_adopted_value_is_fully_labeled() {
    // The positionally-written argument matches a named partial inside the callee.
    quiver()
        .evaluate("f = #[(foo): 'int, (bar): 'int] { $ ~> =(foo: x); x }; f [7, 8]")
        .expect("7");
}

#[test]
fn test_equality_across_spellings() {
    quiver()
        .evaluate(
            r#"
            f = #[(foo): 'int] { $ }
            a = f [1]
            b = f [foo: 1]
            { a ~> =&b => Same | Different }
            "#,
        )
        .expect("Same");
}

#[test]
fn test_named_tuple_labels_pun() {
    quiver()
        .evaluate("g = #Point[(x): 'int, (y): 'int] { $x }; g Point[3, 4]")
        .expect("3");
}

#[test]
fn test_tuple_name_is_not_adopted() {
    // Field labels are adoptable; the tuple's own name is not.
    quiver()
        .evaluate("g = #Point[(x): 'int, (y): 'int] { $x }; g [3, 4]")
        .expect_type_mismatch();
}

#[test]
fn test_flowing_value_fills_positional_argument() {
    quiver()
        .evaluate("f = #[(a): 'int, (b): 'int] { %num.add [$a, $b] }; 5 ~> f [~, 10]")
        .expect("15");
}

#[test]
fn test_generic_alias_keeps_markers() {
    quiver()
        .evaluate(
            r#"
            'pair<'t> = [(first): 't, (second): 't]
            f = #'pair<'int> { $first }
            f [42, 7]
            "#,
        )
        .expect("42");
}

#[test]
fn test_explicit_instantiation_keeps_markers() {
    quiver()
        .evaluate("f = #<'t>[(a): 't, (b): 't] { $a }; f<'int> [10, 20]")
        .expect("10");
}

#[test]
fn test_nested_literal_adopts_labels() {
    quiver()
        .evaluate("f = #[(p): [(x): 'int, (y): 'int], (z): 'int] { $p.y }; f [[1, 2], 3]")
        .expect("2");
}

#[test]
fn test_type_spread_propagates_markers() {
    quiver()
        .evaluate(
            r#"
            'base = [(x): 'int]
            'ext = [...'base, (y): 'int]
            f = #'ext { %num.add [$x, $y] }
            f [1, 2]
            "#,
        )
        .expect("3");
}

#[test]
fn test_marked_and_unmarked_spellings_are_one_type() {
    // The marker never distinguishes types: a value built against the unmarked
    // spelling flows into a parameter declared with the marked one.
    quiver()
        .evaluate(
            r#"
            'marked = [(foo): 'int]
            'plain = [foo: 'int]
            make = #'plain { $ }
            take = #'marked { $foo }
            make [foo: 5] ~> take ~
            "#,
        )
        .expect("5");
}

#[test]
fn test_adoption_composes_with_literal_inference() {
    // Label adoption and `#{ … }` parameter inference ride the same per-field
    // expected types, in the same argument tuple.
    quiver()
        .evaluate("f = #[(n): 'int, (g): #'int -> 'int] { $g $n }; f [5, #{ %num.add [$, 1] }]")
        .expect("6");
}

#[test]
fn test_marker_rejected_in_partial_type() {
    quiver()
        .evaluate("f = #((foo): 'int) { $foo }; f [foo: 1]")
        .expect_error_containing("not partial types");
}

#[test]
fn test_apply_argument_callable_adoption() {
    // `x ~> f g`: the flow becomes `g`'s argument when `g` is a bare callable, so the
    // literal adopts g's labels before f consumes g's result — the shape the standard
    // library's percent-decoding and byte-scanning loops use.
    quiver()
        .evaluate(
            "f = #[(a): 'int, (b): 'int] { %num.sub [$a, $b] }; wrap = #'int { [$, $] }; \
             [10, 3] ~> f ~ ~> wrap ~",
        )
        .expect("[7, 7]");
}

#[test]
fn test_bare_tail_call_adopts_labels() {
    // Bare `^` recurses into the current function, whose parameter is known, so a
    // positional `^ [args]` literal adopts its labels — what lets labeled internal
    // loop helpers keep positional recursion sites.
    quiver()
        .evaluate(
            "f = #[(n): 'int, (acc): 'int] { { $n ~> =0 => $acc | ^ [%num.sub [$n, 1], \
             %num.add [$acc, $n]] } }; f [4, 0]",
        )
        .expect("10");
}

#[test]
fn test_union_parameter_labeled_call() {
    // Adoption is only ever a fallback for omitted labels; a labeled literal
    // checks against a union parameter as usual.
    quiver()
        .evaluate(
            "f = #([(foo): 'int] | [(bar): 'int]) { | =(foo: x) => x | =(bar: y) => y }; f [foo: 7]",
        )
        .expect("7");
}

#[test]
fn test_union_parameter_never_adopts_when_ambiguous() {
    // Both members could claim the positional spelling; a union never adopts, so
    // the call is an ordinary mismatch asking for labels — no silent pick.
    quiver()
        .evaluate(
            "f = #([(foo): 'int] | [(bar): 'int]) { | =(foo: x) => x | =(bar: y) => y }; f [7]",
        )
        .expect_type_mismatch();
}

#[test]
fn test_union_parameter_never_adopts_even_when_unique() {
    // Only one member could fit the positional spelling, but the rule is
    // "unions never adopt", not "unique fit" — adoption requires the expected
    // type to be a single tuple.
    quiver()
        .evaluate(
            "f = #([(foo): 'int] | Wrapped['int]) { | =(foo: x) => x | =Wrapped[y] => y }; f [7]",
        )
        .expect_type_mismatch();
}

#[test]
fn test_union_with_unnamed_member_accepts_positional() {
    // No adoption involved: the positional literal fits the unnamed member as-is,
    // so unions of labeled and unlabeled shapes still work normally.
    quiver()
        .evaluate("f = #(['int, 'int] | [foo: 'int]) { | =[a, _] => a | =(foo: x) => x }; f [1, 2]")
        .expect("1");
}

// --- Pattern-side adoption -------------------------------------------------------
// The dual of literal adoption: an unlabeled tuple-pattern field adopts a marked
// label from the scrutinee's type, positionally. Stated labels must always match;
// unmarked labels are never adopted.

#[test]
fn test_pattern_adopts_labels() {
    quiver()
        .evaluate(
            r#"
            'point = [(x): 'int, (y): 'int]
            f = #'point { $ }
            p = f [1, 2]
            p ~> =[a, b]
            %num.add [a, b]
            "#,
        )
        .expect("3");
}

#[test]
fn test_pattern_adopts_labels_in_binding_statement() {
    quiver()
        .evaluate(
            r#"
            'point = [(x): 'int, (y): 'int]
            f = #'point { $ }
            [a, b] = f [4, 5]
            b
            "#,
        )
        .expect("5");
}

#[test]
fn test_pattern_positional_literal_test() {
    // A literal in an adopted position tests the field, so `=[0, b]` guards on x.
    quiver()
        .evaluate(
            r#"
            'point = [(x): 'int, (y): 'int]
            f = #'point { { =[0, b] => b | Other } }
            [f [0, 6], f [5, 6]]
            "#,
        )
        .expect("[6, Other]");
}

#[test]
fn test_pattern_stated_label_must_match() {
    quiver()
        .evaluate(
            r#"
            'point = [(x): 'int, (y): 'int]
            f = #'point { { =[oof: a, _] => a | NoMatch } }
            f [1, 2]
            "#,
        )
        .expect("NoMatch");
}

#[test]
fn test_pattern_does_not_adopt_unmarked_labels() {
    quiver()
        .evaluate("q = [foo: 1]; { q ~> =[a] => a | NoMatch }")
        .expect("NoMatch");
}

#[test]
fn test_pattern_adopts_labels_for_named_tuple() {
    quiver()
        .evaluate(
            r#"
            g = #Point[(x): 'int, (y): 'int] { =Point[a, b]; %num.sub [a, b] }
            g Point[10, 4]
            "#,
        )
        .expect("6");
}

#[test]
fn test_pattern_adopts_labels_in_nested_pattern() {
    quiver()
        .evaluate(
            r#"
            'line = [(from): [(x): 'int, (y): 'int], (to): [(x): 'int, (y): 'int]]
            f = #'line { =[[a, _], [_, b]]; %num.add [a, b] }
            f [[1, 2], [3, 4]]
            "#,
        )
        .expect("5");
}

#[test]
fn test_pattern_adopts_labels_in_or_pattern() {
    // Each alternative adopts independently; both bind `b`, as or-patterns require.
    quiver()
        .evaluate(
            r#"
            'point = [(x): 'int, (y): 'int]
            f = #'point { { =([0, b] | [b, 0]) => b | Neither } }
            [f [0, 7], f [3, 0], f [1, 1]]
            "#,
        )
        .expect("[7, 3, Neither]");
}

#[test]
fn test_pattern_adopts_through_union_member() {
    // A pattern matches whichever members it can — adoption applies per candidate
    // member, so a union scrutinee is fine on the pattern side (nothing is picked;
    // the value decides at runtime).
    quiver()
        .evaluate(
            r#"
            'msg = [(a): 'int] | Wrapped['int]
            f = #'msg { | =Wrapped[y] => %num.mul [y, 10] | =[x] => x }
            [f [a: 7], f Wrapped[3]]
            "#,
        )
        .expect("[7, 30]");
}
