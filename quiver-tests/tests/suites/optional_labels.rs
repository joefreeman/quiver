// Optional field labels: a function's parameter tuple may mark a field's label omittable
// with `#[(foo): 'int]`. A call's argument literal may then omit the marked labels; the
// constructed value adopts them positionally, so it is fully labeled regardless of
// spelling — matching, equality, and partial access see one shape. The mark is a calling
// convention, so it rides on the function type and is never part of the parameter tuple's
// identity: it is legal only in a parameter position, and never reaches patterns.

use crate::common::*;

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
fn test_explicit_instantiation_keeps_markers() {
    quiver()
        .evaluate("f = #<'t>[(a): 't, (b): 't] { $a }; f<'int> [10, 20]")
        .expect("10");
}

#[test]
fn test_marked_and_unmarked_spellings_are_one_type() {
    // The marker never distinguishes types: a value built against the unmarked
    // spelling flows into a parameter declared with the marked one.
    quiver()
        .evaluate(
            r#"
            make = #[foo: 'int] { $ }
            take = #[(foo): 'int] { $foo }
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

// --- Spread decorators -----------------------------------------------------------
// An entry in a parameter tuple that states no type *decorates* a field an earlier
// spread brought in: it adjusts only that field's label and default, leaving its type
// and its position alone. An entry that does state a type defines or replaces one, as
// it always did.

#[test]
fn test_decorator_marks_label_omittable() {
    quiver()
        .evaluate("'base = [x: 'int]; f = #[...'base, (x)] { $x }; f [5]")
        .expect("5");
}

#[test]
fn test_decorator_gives_a_default() {
    quiver()
        .evaluate("'base = [x: 'int]; f = #[...'base, x = 7] { $x }; f []")
        .expect("7");
    // `name = value` adjusts only the default, so the label stays required.
    quiver()
        .evaluate("'base = [x: 'int]; f = #[...'base, x = 7] { $x }; f [7]")
        .expect_type_mismatch();
}

#[test]
fn test_decorator_marks_and_defaults() {
    quiver()
        .evaluate("'base = [x: 'int]; f = #[...'base, (x) = 7] { $x }; f []")
        .expect("7");
    quiver()
        .evaluate("'base = [x: 'int]; f = #[...'base, (x) = 7] { $x }; f [3]")
        .expect("3");
}

#[test]
fn test_bare_decorator_is_a_checked_restatement() {
    // A bare `x` changes nothing — it names the spread's field so `$x` in the body reads
    // against something written locally.
    quiver()
        .evaluate("'base = [x: 'int]; f = #[...'base, x] { $x }; f [x: 5]")
        .expect("5");
}

#[test]
fn test_decorator_keeps_field_position() {
    // Decorating out of order must not reorder the fields: positional calls are exactly
    // what a mark enables, so a moved field would silently change what one means.
    quiver()
        .evaluate("'base = [x: 'int, y: 'int]; f = #[...'base, (y), (x)] { [$x, $y] }; f [10, 20]")
        .expect("[10, 20]");
}

#[test]
fn test_decorated_spread_takes_a_new_marked_field() {
    // The spread's field is marked by a decorator; a fresh field states its type and is
    // marked in place — what the old `'base`/`'ext` pair of marked aliases used to spell.
    quiver()
        .evaluate(
            r#"
            'base = [x: 'int]
            f = #[...'base, (x), (y): 'int] { %num.add [$x, $y] }
            f [1, 2]
            "#,
        )
        .expect("3");
}

#[test]
fn test_decorated_generic_alias_instantiation() {
    // A spread may instantiate a parameterized alias, and its fields decorate as usual.
    quiver()
        .evaluate(
            r#"
            'pair<'t> = [first: 't, second: 't]
            f = #[...'pair<'int>, (first), (second)] { $first }
            f [42, 7]
            "#,
        )
        .expect("42");
}

#[test]
fn test_decorator_does_not_mark_the_alias() {
    // Decorating a spread of `'p` marks `marked`'s parameter only: a second function
    // declared with the bare alias still requires the labels.
    quiver()
        .evaluate(
            r#"
            'p = [x: 'int, y: 'int]
            marked = #[...'p, (x), (y)] { $x }
            plain = #'p { $y }
            plain [1, 2]
            "#,
        )
        .expect_type_mismatch();
}

#[test]
fn test_decorator_must_name_a_field_the_spread_has() {
    quiver()
        .evaluate("'base = [x: 'int]; f = #[...'base, (z)] { $x }; f [5]")
        .expect_error_containing("nothing here has one by that name");
}

#[test]
fn test_decorator_without_a_spread_rejected() {
    quiver()
        .evaluate("f = #[(x)] { $x }; f [5]")
        .expect_error_containing("this tuple has no spread");
}

// --- Where a mark may appear -----------------------------------------------------
// A mark is a calling convention, so it belongs to a function type's parameter tuple —
// written at a literal or inside an alias — and nowhere else. Elsewhere it could never
// grant anything, so it is rejected rather than quietly ignored.

#[test]
fn test_marker_rejected_in_partial_type() {
    quiver()
        .evaluate("f = #((foo): 'int) { $foo }; f [foo: 1]")
        .expect_error_containing("not partial types");
}

#[test]
fn test_marker_rejected_in_tuple_alias() {
    // A bare tuple type has no callers to grant anything to — and a mark stored against
    // an interned tuple would leak to every structurally identical spelling.
    quiver()
        .evaluate("'point = [(x): 'int, (y): 'int]; f = #'point { $x }; f [1, 2]")
        .expect_error_containing("only allowed in a function's parameter type");
}

#[test]
fn test_marker_rejected_in_nested_parameter_field() {
    quiver()
        .evaluate("f = #[(p): [(x): 'int, (y): 'int], (z): 'int] { $p.y }; f [[1, 2], 3]")
        .expect_error_containing("only allowed in a function's parameter type");
}

#[test]
fn test_adoption_is_top_level_only() {
    // Marking the field that *holds* a tuple says nothing about that tuple's own labels,
    // so the nested literal must still be spelled with them.
    quiver()
        .evaluate("f = #[(p): [x: 'int, y: 'int], (z): 'int] { $p.y }; f [[1, 2], 3]")
        .expect_type_mismatch();
    quiver()
        .evaluate("f = #[(p): [x: 'int, y: 'int], (z): 'int] { $p.y }; f [[x: 1, y: 2], 3]")
        .expect("2");
}

#[test]
fn test_marker_in_function_type_inside_alias_is_honoured() {
    // Modelled on `%html/live`'s `'component`: a record field declares
    // `update: #[(state): 's, (event): 'e] -> 's`, and a caller holding only the declared
    // type calls it positionally.
    quiver()
        .evaluate(
            r#"
            'component<'s, 'e> = [update: #[(state): 's, (event): 'e] -> 's]
            counter = [update: #[(state): 'int, (event): 'int] { %num.add [$state, $event] }]
            step = #<'s, 'e>[c: 'component<'s, 'e>, state: 's, event: 'e] {
              $c.update [$state, $event]
            }
            step [c: counter, state: 1, event: 2]
            "#,
        )
        .expect("3");
}

#[test]
fn test_marker_does_not_leak_to_a_same_shaped_parameter() {
    // The regression the move to function types fixes: an unmarked parameter stays
    // unmarked even when a marked function of the same tuple shape is in scope.
    quiver()
        .evaluate(
            r#"
            marked = #[(x): 'int, (y): 'int] { $x }
            plain = #[x: 'int, y: 'int] { $y }
            plain [1, 2]
            "#,
        )
        .expect_type_mismatch();
    // Both spellings coexist: the marked one still adopts.
    quiver()
        .evaluate(
            r#"
            marked = #[(x): 'int, (y): 'int] { $x }
            plain = #[x: 'int, y: 'int] { $y }
            [marked [1, 2], plain [x: 1, y: 2]]
            "#,
        )
        .expect("[1, 2]");
}

// --- Union parameters ------------------------------------------------------------
// A mark names a field position, so it needs a single tuple: a union parameter has no
// positions of its own, and a member of one is not the parameter tuple.

#[test]
fn test_union_parameter_labeled_call() {
    quiver()
        .evaluate(
            "f = #([foo: 'int] | [bar: 'int]) { | =(foo: x) => x | =(bar: y) => y }; f [foo: 7]",
        )
        .expect("7");
}

#[test]
fn test_union_parameter_never_adopts() {
    // Nothing is marked and nothing could be, so a positional call is an ordinary
    // mismatch asking for labels — no silent pick between the members.
    quiver()
        .evaluate("f = #([foo: 'int] | [bar: 'int]) { | =(foo: x) => x | =(bar: y) => y }; f [7]")
        .expect_type_mismatch();
}

#[test]
fn test_marker_rejected_in_union_member() {
    quiver()
        .evaluate(
            "f = #([(foo): 'int] | [bar: 'int]) { | =(foo: x) => x | =(bar: y) => y }; f [foo: 7]",
        )
        .expect_error_containing("only allowed in a function's parameter type");
}

#[test]
fn test_union_with_unnamed_member_accepts_positional() {
    // No adoption involved: the positional literal fits the unnamed member as-is,
    // so unions of labeled and unlabeled shapes still work normally.
    quiver()
        .evaluate("f = #(['int, 'int] | [foo: 'int]) { | =[a, _] => a | =(foo: x) => x }; f [1, 2]")
        .expect("1");
}

// --- Patterns never adopt --------------------------------------------------------
// A mark is spelling for the call site alone. An unlabeled tuple-pattern field never
// matches a labeled value field, whatever the value's type was built against.

#[test]
fn test_unlabeled_pattern_does_not_match_labeled_value() {
    quiver()
        .evaluate("q = [foo: 1, bar: 2]; { q ~> =[a, b] => a | NoMatch }")
        .expect("NoMatch");
}

#[test]
fn test_pattern_does_not_adopt_a_marked_parameters_labels() {
    // The value was written positionally against a marked parameter, but adoption
    // happened at the call: what arrives is fully labeled, and the pattern is not.
    quiver()
        .evaluate("f = #[(x): 'int, (y): 'int] { { $ ~> =[a, b] => a | NoMatch } }; f [1, 2]")
        .expect("NoMatch");
}

#[test]
fn test_pattern_reads_the_adopted_labels() {
    // Stating the labels is how the adopted value is destructured.
    quiver()
        .evaluate("f = #[(x): 'int, (y): 'int] { $ ~> =[x: a, y: b]; %num.add [a, b] }; f [1, 2]")
        .expect("3");
}

#[test]
fn test_spawn_elaborates_its_init_as_a_call_does() {
    // A spawn's init is the root function's argument, so it is spelled as one: omittable
    // labels adopted, defaulted fields filled, an inferred literal typed from the parameter.
    quiver()
        .evaluate("f = #[(x): 'int, (y): 'int] { %num.add [$x, $y] }; p = @f [5, 6]; !p")
        .expect("11");
    quiver()
        .evaluate("g = #[(path): '%str, n: 'int = 3] { $n }; q = @g [\"a\"]; !q")
        .expect("3");
    quiver()
        .evaluate("h = #['int, #'int -> 'int] { $1 $0 }; r = @h [4, #{ %num.mul [$, 2] }]; !r")
        .expect("8");
}
