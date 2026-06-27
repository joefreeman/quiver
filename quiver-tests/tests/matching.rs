mod common;
use common::*;

#[test]
fn test_pin_simple() {
    quiver().evaluate("y = 2, 2 ~> =&y").expect("Ok");
    quiver().evaluate("y = 2, 3 ~> =&y").expect("[]");
}

#[test]
fn test_pin_in_tuple() {
    quiver()
        .evaluate("y = 2, Point[x, &y] = Point[1, 2], x")
        .expect("1");
    quiver()
        .evaluate("y = 2, Point[x, &y] = Point[1, 3]")
        .expect("[]");
}

#[test]
fn test_pin_with_term_syntax() {
    quiver()
        .evaluate("y = 2, Point[1, 2] ~> =Point[x, &y], x")
        .expect("1");
    quiver()
        .evaluate("y = 2, Point[1, 3] ~> =Point[x, &y]")
        .expect("[]");
}

#[test]
fn test_mixed_pin_and_bind() {
    quiver()
        .evaluate("y = 2, Point[1, 2] ~> =Point[x, &y], x")
        .expect("1");
    quiver()
        .evaluate("y = 2, Point[x, &y] = Point[1, 2], x")
        .expect("1");
}

#[test]
fn test_nested_pin_and_bind() {
    quiver()
        .evaluate("y = 2, A[1, B[2, C[3]]] ~> =A[x, B[&y, C[z]]], [x, z]")
        .expect("[1, 3]");
}

#[test]
fn test_pin_multiple_variables() {
    quiver()
        .evaluate("x = 1, y = 2, [1, 2] ~> =[&x, &y]")
        .expect("Ok");
    quiver()
        .evaluate("x = 1, y = 2, [1, 3] ~> =[&x, &y]")
        .expect("[]");
}

#[test]
fn test_repeated_identifier_pin() {
    // Repeated identifier in pin mode - checks equality
    quiver().evaluate("[5, 5] ~> =[x, x]").expect("Ok");
    quiver().evaluate("[5, 6] ~> =[x, x]").expect("[]");
}

#[test]
fn test_pin_against_partial() {
    quiver().evaluate("A[x: 1, y: 2] ~> =(x), x").expect("1");

    // Binding: (field_name: identifier) binds the field to the identifier
    quiver().evaluate("A[x: 1, y: 2] ~> =(x: b), b").expect("1");

    // Pinning: (field_name: &var) checks field against variable value
    quiver().evaluate("x = 1, A[x: 1] ~> =(x: &x)").expect("Ok");
    quiver().evaluate("x = 2, A[x: 1] ~> =(x: &x)").expect("[]");

    // Without variable for pin, should error: `&x` names an undefined binding.
    quiver()
        .evaluate("A[x: 1] ~> =(x: &x)")
        .expect_compile_error(quiver_compiler::compiler::Error::VariableUndefined(
            "x".to_string(),
        ));
}

#[test]
fn test_pin_with_variable_and_repetition() {
    // When variable exists and identifier is repeated, check both Variable and FieldEquality
    quiver().evaluate("x = 5, [5, 5] ~> =[&x, &x]").expect("Ok");
    quiver().evaluate("x = 5, [5, 6] ~> =[&x, &x]").expect("[]"); // Fails field equality
    quiver().evaluate("x = 5, [4, 4] ~> =[&x, &x]").expect("[]"); // Fails variable check
}

#[test]
fn test_pin_without_variable_single_occurrence() {
    // Reference with no matching binding should error as an undefined variable.
    quiver().evaluate("5 ~> =&x").expect_compile_error(
        quiver_compiler::compiler::Error::VariableUndefined("x".to_string()),
    );
}

#[test]
fn test_pin_from_outer_scope() {
    // Pin pattern should be able to reference variables from outer scopes
    quiver()
        .evaluate("x = 5, f = #{ A[5] ~> =A[&x] }, f []")
        .expect("Ok");
    quiver()
        .evaluate("x = 5, f = #{ A[6] ~> =A[&x] }, f []")
        .expect("[]");
}

#[test]
fn test_pin_mixed_repeated_and_single() {
    // References with no matching bindings should error as undefined variables.
    quiver()
        .evaluate("[1, 1, 2] ~> =[&x, &x, &y]")
        .expect_compile_error(quiver_compiler::compiler::Error::VariableUndefined(
            "x".to_string(),
        ));
}

// Type narrowing tests

#[test]
fn test_pin_int_type() {
    quiver().evaluate("42 ~> ='int").expect("Ok");
    quiver().evaluate("0xff ~> ='int").expect("[]");
}

#[test]
fn test_pin_bin_type() {
    quiver().evaluate("0xabcd ~> ='bin").expect("Ok");
    quiver().evaluate("42 ~> ='bin").expect("[]");
}

#[test]
fn test_pin_default_type() {
    // `='` references the enclosing module's default type, like `'int` references a named one.
    quiver().evaluate("' = A | B;\nA ~> ='").expect("Ok");
    quiver().evaluate("' = A | B;\nC ~> ='").expect("[]");
}

#[test]
fn test_or_pattern_no_bindings() {
    // `(p | q)` matches if either alternative matches; here neither binds anything.
    let src = r#"[[], 5] ~> { =([[], _] | [_, []]) => "nil" | "ok" }"#;
    quiver().evaluate(src).expect("\"nil\"");
    let src = r#"[5, 6] ~> { =([[], _] | [_, []]) => "nil" | "ok" }"#;
    quiver().evaluate(src).expect("\"ok\"");
}

#[test]
fn test_or_pattern_shared_binding() {
    // Both alternatives bind `x`, so the body sees it whichever matched.
    let src = "'ab = A['int] | B['int];\nf = #'ab { =(A[x] | B[x]) => x },\nB[7] ~> f";
    quiver().evaluate(src).expect("7");
}

#[test]
fn test_or_pattern_inconsistent_bindings_is_error() {
    // Alternatives that bind different variables are rejected at compile time.
    let src = "'ab = A['int] | B['int];\nf = #'ab { =(A[x] | B[y]) => 9 },\nA[1] ~> f";
    quiver().evaluate(src).expect_compile_error(
        quiver_compiler::compiler::Error::OrPatternBindingMismatch {
            expected: vec!["x".to_string()],
            found: vec!["y".to_string()],
        },
    );
}

#[test]
fn test_type_narrowing_in_blocks() {
    // Test type narrowing for int
    quiver()
        .evaluate("value = 42, value ~> { ='int => \"is_int\" | \"is_bin\" }")
        .expect("\"is_int\"");

    // Test type narrowing for bin
    quiver()
        .evaluate("value = 0xabcd, value ~> { ='bin => \"is_bin\" | \"is_int\" }")
        .expect("\"is_bin\"");

    // Test type narrowing failure falls through
    quiver()
        .evaluate("value = 42, value ~> { ='bin => \"is_bin\" | \"not_bin\" }")
        .expect("\"not_bin\"");
}

#[test]
fn test_type_narrowing_in_function() {
    quiver()
        .evaluate("f = #('int | 'bin) { ='int => $ }, 1 ~> f")
        .expect_type("'int");
}

#[test]
fn test_narrowing_multiple_matching_variants() {
    // Multiple variants match the pattern structurally: `=A[x]` covers both `A['int]` and
    // `A['bin]`, so `x` is `'int | 'bin`. The argument `A[1]` is an `A`, so it always matches —
    // the unhandled `B` is unreachable here, so the result carries no `| []`.
    quiver()
        .evaluate("f = #(A['int] | A['bin] | B['int]) { =A[x] => x }, A[1] ~> f")
        .expect_type("'bin | 'int");
}

#[test]
fn test_narrowing_no_matching_variants() {
    // No variants match - should return just []
    quiver()
        .evaluate("f = #(A['int] | B['int]) { =C[x] }, A[1] ~> f")
        .expect_type("[]");
}

#[test]
fn test_narrowing_all_variants_match() {
    // All variants match structurally - exhaustive matching removes [] from result type
    quiver()
        .evaluate("f = #(A['int] | A['bin]) { =A[x] => x }, A[1] ~> f")
        .expect_type("'bin | 'int");
}

#[test]
fn test_narrowing_nested_field() {
    // Pattern narrows based on nested tuple types
    quiver()
        .evaluate("f = #(X[A['int]] | X[B['int]]) { =X[A['int]] => $ }, X[A[1]] ~> f")
        .expect_type("X[A['int]]");
}

#[test]
fn test_narrowing_with_wildcard() {
    // Wildcards match anything - only outer structure matters
    quiver()
        .evaluate("f = #(A['int, 'bin] | B['int, 'bin]) { =A[_, _] => $ }, A[1, 0xff] ~> f")
        .expect_type("A['int, 'bin]");
}

#[test]
fn test_narrowing_with_literal() {
    // Literal pattern should narrow to only matching variant
    quiver()
        .evaluate("f = #(A['int] | B['int]) { =A[1] => $ }, A[1] ~> f")
        .expect_type("A['int] | []");
}

#[test]
fn test_narrowing_repeated_identifiers() {
    // Repeated identifier requires both structural match and runtime equality
    quiver()
        .evaluate(
            r#"
            f = #(A['int, 'int] | A['int, 'bin] | B['int, 'int]) {
              =A[x, x] => x
            },
            A[1, 1] ~> f
            "#,
        )
        .expect_type("'int | []");
}

#[test]
fn test_narrowing_partial_types() {
    // Should work with partial types
    quiver()
        .evaluate("f = #(A[x: 'int] | B[x: 'int]) { =A[x: 'int] => $ }, A[x: 1] ~> f")
        .expect_type("A[x: 'int]");
}

#[test]
fn test_narrowing_type_and_variable_pin() {
    // Combines structural narrowing with runtime variable check
    quiver()
        .evaluate("y = 2, f = #(A['int] | B['int]) { =A[&y] => $ }, A[2] ~> f")
        .expect_type("A['int] | []");
}

#[test]
fn test_narrowing_nested_union_in_field() {
    quiver()
        .evaluate("f = #(A['int | 'bin] | B['int]) { =A[('int | 'bin)] => $ }, A[1] ~> f")
        .expect_type("A[('bin | 'int)]");
}

#[test]
fn test_partial_pattern_field_narrows_union_by_value() {
    // A partial-pattern field naming a nullary tag (`=(tag: Cat)`) must match the field's *runtime
    // value*, narrowing a union-typed field. `mk` builds Rec with a union-typed tag, so the value's
    // concrete type carries the whole union — a root type assertion couldn't distinguish Cat from
    // Dog, but a field-value check can.
    quiver()
        .evaluate(
            r#"
            'k = Cat | Dog
            'rec = Rec[tag: 'k]
            mk = #'k { Rec[tag: ~] },
            f = #'rec { =(tag: Cat) => IsCat | No },
            [Cat ~> mk ~> f, Dog ~> mk ~> f]
            "#,
        )
        .expect("[IsCat, No]");
}

#[test]
fn test_partial_pattern_field_tag_with_binding() {
    // The same field-value match, mixed with a sibling binding via the `(…) = …` form.
    quiver()
        .evaluate(
            r#"
            'k = Cat | Dog
            'rec = Rec[tag: 'k, label: 'k]
            mk = #'k { Rec[tag: ~, label: Dog] },
            f = #'rec { (tag: Cat, label: l) = ~ => Got[l] | No },
            [Cat ~> mk ~> f, Dog ~> mk ~> f]
            "#,
        )
        .expect("[Got[Dog], No]");
}

#[test]
fn test_partial_pattern_nullary_tag_field_value_is_checked() {
    // Regression: a partial-pattern field naming a nullary tag must check the field's *value*, not
    // merely that the field is present. `=(mode: W)` against a `mode: A` field can never match, so
    // it must fall through to the `A` branch — before the fix the unsatisfiable field check was
    // dropped and the partial matched unconditionally (yielding 1 here).
    quiver()
        .evaluate("[mode: A] ~> { =(mode: W) => 1 | =(mode: A) => 2 | 0 }")
        .expect("2");

    // A partial still requires the field's presence: no `mode` field means no match.
    quiver()
        .evaluate("[other: 1] ~> { =(mode: W) => 1 | 0 }")
        .expect("0");
}

#[test]
fn test_partial_pattern_sequential_branches_narrow_union_field() {
    // Regression: once `=(mode: W)` fails, the complement must narrow only the field (dropping W),
    // not remove the whole `mode`-bearing variant — so the later `=(mode: A)` / `=(mode: R)`
    // branches still match. This mirrors `std/file.qv`'s open-mode flag dispatch.
    quiver()
        .evaluate(
            r#"
            f = #([a: 'int] | [a: 'int, mode: R | W | A]) {
              | =(mode: W) => 1
              | =(mode: A) => 2
              | =(mode: R) => 3
              | 0
            },
            [[a: 1, mode: A] ~> f, [a: 1, mode: W] ~> f, [a: 1, mode: R] ~> f, [a: 1] ~> f]
            "#,
        )
        .expect("[2, 1, 3, 0]");
}

#[test]
fn test_narrowing_with_branches() {
    quiver()
        .evaluate("f = #(A | B | C) { =(A | B) => =A }, A ~> f")
        .expect_type("Ok | []");

    quiver()
        .evaluate("f = #(A | B | C) { =(A | B) => =A | X }, A ~> f")
        .expect_type("Ok | X | []");

    quiver()
        .evaluate("f = #(A | B | C) { =(A | B) => 1 | X }, A ~> f")
        .expect_type("'int | X");
}

#[test]
fn test_narrowing_star_pattern() {
    // Star matches everything - no narrowing, no failure possible
    quiver()
        .evaluate("f = #(A['int] | B['int]) { =_ => 1 }, A[1] ~> f")
        .expect_type("'int");
}

#[test]
fn test_narrowing_with_type_alias() {
    // Type alias should work for narrowing
    quiver()
        .evaluate(
            r#"
            'a = A['int, 'int];
            f = #(A['int, 'int] | B['int, 'int]) { ='a => $ },
            A[1, 2] ~> f
            "#,
        )
        .expect_type("A['int, 'int]");
}

#[test]
fn test_narrowing_generic_type() {
    // Parameterized types should narrow correctly
    quiver()
        .evaluate(
            r#"
            'box<'t> = Box['t];
            f = #(Box[A['int]] | Box[B['int]]) { ='box<A['int]> => $ },
            Box[A[1]] ~> f
            "#,
        )
        .expect_type("Box[A['int]]");
}

#[test]
fn test_narrowing_preserves_field_types() {
    // Narrowing should preserve the exact field types from matching variants
    quiver()
        .evaluate("f = #(A['int] | B['int]) { =A[1] => 1 }, A[1] ~> f")
        .expect_type("'int | []");
}

#[test]
fn test_narrowing_complex_nested_pattern() {
    // Complex nested pattern with multiple levels
    quiver()
        .evaluate(
            r#"
            f = #(X[Y[A['int]]] | X[Y[B['int]]] | X[Z[A['int]]]) {
                =X[Y[A['int]]] => $
            },
            X[Y[A[1]]] ~> f
            "#,
        )
        .expect_type("X[Y[A['int]]]");
}

#[test]
fn test_narrowing_in_block_branches() {
    // Type narrowing in block branches
    quiver()
        .evaluate(
            r#"
            f = #(A['int] | B['int]) {
              | =A[x] => x
              | =B[x] => x
              | 0
            },
            value = A[1],
            value ~> f
            "#,
        )
        .expect_type("'int");
}

#[test]
fn test_narrowing_with_fallback_branch() {
    quiver()
        .evaluate("f = #('int | 'bin) { ='bin | 0xff }, &f")
        .expect_type("#('bin | 'int) -> ('bin | Ok)");

    quiver()
        .evaluate("f = #('int | 'bin) { ='bin | 0xff }, 0x0a ~> f")
        .expect("Ok");
    quiver()
        .evaluate("f = #('int | 'bin) { ='bin | 0xff }, 42 ~> f")
        .expect("0xff");
}

#[test]
fn test_nil_condition_with_fallback() {
    quiver()
        .evaluate("#(A['int] | B['int]) { =C['int] }")
        .expect_type("#(A['int] | B['int]) -> []");
    quiver()
        .evaluate("#(A['int] | B['int]) { =C['int] | 42 }")
        .expect_type("#(A['int] | B['int]) -> 'int");
}

#[test]
fn test_not_operator_type_narrowing() {
    // When ~> <> succeeds (returns Ok), it proves input was nil
    // Subsequent branches receive narrowed type with nil subtracted
    quiver()
        .evaluate("#(A | []) { <> | =A }")
        .expect_type("#(A | []) -> Ok");
}

#[test]
fn test_variable_pattern_matching_in_branches() {
    // Regression test for variable-based branch matching bug.
    // When pattern matching on a variable (not ~>) in multi-branch blocks,
    // the second branch should work correctly after the first branch fails.
    // Previously this caused VariableUndefined("local [7]") because the
    // cleanup code assumed pattern bindings were stored when they weren't.
    quiver()
        .evaluate(
            r#"
            'list = Nil | Cons['int, ^];

            f = #['list, 'list] -> 'list {
              =[xs, ys],
              t = [xs, ys],
              {
                | t ~> =[Nil, zs] => zs
                | t ~> =[Cons[head, tail], zs] => zs
              }
            },

            [f [Nil, Cons[1, Nil]], f [Cons[1, Nil], Cons[2, Nil]]]
            "#,
        )
        .expect("[Cons[1, Nil], Cons[2, Nil]]");
}

#[test]
fn test_as_pattern_binds_and_asserts_type() {
    // `('int)x` matches the value against `'int` AND binds the whole value to `x`.
    quiver().evaluate("42 ~> =('int)x, x").expect("42");
    // The binding is at the narrowed type, so `x` is usable as an int.
    quiver()
        .evaluate("42 ~> =('int)x, __integer_add__ [x, 1]")
        .expect("43");
    // A type mismatch fails the match (yields nil), like any failed match.
    quiver().evaluate("0x0a ~> =('int)x").expect("[]");
}

#[test]
fn test_as_pattern_propagates_nil() {
    // The key use: assert-and-bind that fails (propagates) on nil, replacing the `=x, x` re-emit.
    // A non-nil value binds and continues; a nil value fails the assertion and short-circuits.
    quiver()
        .evaluate(
            "opt = #'int { | =0 => [] | $ }, 5 ~> opt ~> =('int)x, x ~> __integer_add__ [~, 1]",
        )
        .expect("6");
    quiver()
        .evaluate("opt = #'int { | =0 => [] | $ }, 0 ~> opt ~> =('int)x, x")
        .expect("[]");
}

#[test]
fn test_as_pattern_narrows_union_in_field() {
    // Nested in a field, the as-binder narrows a union variant by field type and binds the field.
    quiver()
        .evaluate("f = #(A[a: 'int] | A[a: 'bin]) { =A[a: ('int)x] => x | NoMatch }, A[a: 5] ~> f")
        .expect("5");
    // The other variant fails the field-type assertion.
    quiver()
        .evaluate(
            "f = #(A[a: 'int] | A[a: 'bin]) { =A[a: ('int)x] => x | NoMatch }, A[a: 0x0a] ~> f",
        )
        .expect("NoMatch");
    // The partial-pattern spelling works identically.
    quiver()
        .evaluate("A[a: 5] ~> =A(a: ('int)x), x")
        .expect("5");
}

#[test]
fn test_as_pattern_captures_whole_value() {
    // At the top level the binder captures the whole value, ascribed the parenthesised type.
    quiver()
        .evaluate("A[a: 5] ~> =(A[a: 'int])whole, whole")
        .expect("A[a: 5]");
}

#[test]
fn test_as_pattern_with_union_type() {
    quiver()
        .evaluate("0x0a ~> =('int | 'bin)x, x")
        .expect("0x0a");
    quiver().evaluate("42 ~> =('int | 'bin)x, x").expect("42");
}
