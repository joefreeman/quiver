use crate::common::*;

#[test]
fn test_pin_simple() {
    quiver().evaluate("y = 2; 2 ~> =^y").expect("2");
    quiver().evaluate("y = 2; 3 ~> =^y").expect("[]");
}

#[test]
fn test_pin_in_tuple() {
    quiver()
        .evaluate("y = 2; Point[x, ^y] = Point[1, 2]; x")
        .expect("1");
    quiver()
        .evaluate("y = 2; Point[x, ^y] = Point[1, 3]")
        .expect("[]");
}

#[test]
fn test_pin_with_term_syntax() {
    quiver()
        .evaluate("y = 2; Point[1, 2] ~> =Point[x, ^y]; x")
        .expect("1");
    quiver()
        .evaluate("y = 2; Point[1, 3] ~> =Point[x, ^y]")
        .expect("[]");
}

#[test]
fn test_mixed_pin_and_bind() {
    quiver()
        .evaluate("y = 2; Point[1, 2] ~> =Point[x, ^y]; x")
        .expect("1");
    quiver()
        .evaluate("y = 2; Point[x, ^y] = Point[1, 2]; x")
        .expect("1");
}

#[test]
fn test_nested_pin_and_bind() {
    quiver()
        .evaluate("y = 2; A[1, B[2, C[3]]] ~> =A[x, B[^y, C[z]]]; [x, z]")
        .expect("[1, 3]");
}

#[test]
fn test_pin_multiple_variables() {
    quiver()
        .evaluate("x = 1; y = 2; [1, 2] ~> =[^x, ^y]")
        .expect("[1, 2]");
    quiver()
        .evaluate("x = 1; y = 2; [1, 3] ~> =[^x, ^y]")
        .expect("[]");
}

#[test]
fn test_repeated_identifier_pin() {
    // Repeated identifier in pin mode - checks equality
    quiver().evaluate("[5, 5] ~> =[x, x]").expect("[5, 5]");
    quiver().evaluate("[5, 6] ~> =[x, x]").expect("[]");
}

#[test]
fn test_pin_against_partial() {
    quiver().evaluate("A[x: 1, y: 2] ~> =(x); x").expect("1");

    // Binding: (field_name: identifier) binds the field to the identifier
    quiver().evaluate("A[x: 1, y: 2] ~> =(x: b); b").expect("1");

    // Pinning: (field_name: &var) checks field against variable value
    quiver()
        .evaluate("x = 1; A[x: 1] ~> =(x: ^x)")
        .expect("A[x: 1]");
    quiver().evaluate("x = 2; A[x: 1] ~> =(x: ^x)").expect("[]");

    // Without variable for pin, should error: `&x` names an undefined binding.
    quiver()
        .evaluate("A[x: 1] ~> =(x: ^x)")
        .expect_compile_error(quiver_compiler::compiler::Error::VariableUndefined(
            "x".to_string(),
        ));
}

#[test]
fn test_pin_with_variable_and_repetition() {
    // When variable exists and identifier is repeated, check both Variable and FieldEquality
    quiver()
        .evaluate("x = 5; [5, 5] ~> =[^x, ^x]")
        .expect("[5, 5]");
    quiver().evaluate("x = 5; [5, 6] ~> =[^x, ^x]").expect("[]"); // Fails field equality
    quiver().evaluate("x = 5; [4, 4] ~> =[^x, ^x]").expect("[]"); // Fails variable check
}

#[test]
fn test_pin_without_variable_single_occurrence() {
    // Reference with no matching binding should error as an undefined variable.
    quiver().evaluate("5 ~> =^x").expect_compile_error(
        quiver_compiler::compiler::Error::VariableUndefined("x".to_string()),
    );
}

#[test]
fn test_pin_from_outer_scope() {
    // Pin pattern should be able to reference variables from outer scopes
    quiver()
        .evaluate("x = 5; f = #[] { A[5] ~> =A[^x] }; [] ~> f ~")
        .expect("A[5]");
    quiver()
        .evaluate("x = 5; f = #[] { A[6] ~> =A[^x] }; [] ~> f ~")
        .expect("[]");
}

#[test]
fn test_pin_mixed_repeated_and_single() {
    // References with no matching bindings should error as undefined variables.
    quiver()
        .evaluate("[1, 1, 2] ~> =[^x, ^x, ^y]")
        .expect_compile_error(quiver_compiler::compiler::Error::VariableUndefined(
            "x".to_string(),
        ));
}

// Type narrowing tests

#[test]
fn test_pin_int_type() {
    quiver().evaluate("42 ~> ='int").expect("42");
    quiver().evaluate("<ff> ~> ='int").expect("[]");
}

#[test]
fn test_pin_bin_type() {
    quiver().evaluate("<abcd> ~> ='bin").expect("<abcd>");
    quiver().evaluate("42 ~> ='bin").expect("[]");
}

#[test]
fn test_pin_default_type() {
    // `='` references the enclosing module's default type, like `'int` references a named one.
    quiver().evaluate("' = A | B;\nA ~> ='").expect("A");
    quiver().evaluate("' = A | B;\nC ~> ='").expect("[]");
}

#[test]
fn test_pin_variable_field_path() {
    // A pin target may walk fields of an existing variable.
    quiver()
        .evaluate("p = Point[x: 1, y: 2]; 1 ~> =^p.x")
        .expect("1");
    quiver()
        .evaluate("p = Point[x: 1, y: 2]; 2 ~> =^p.x")
        .expect("[]");
    quiver()
        .evaluate("p = [a: [b: 7]]; 7 ~> =^p.a.b")
        .expect("7");
    quiver().evaluate("p = [3, 4]; 4 ~> =^p.1").expect("4");
}

#[test]
fn test_pin_parameter() {
    // `&$` pins the whole parameter; `&$x` / `&$0` pin its fields (glued sugar, as in `$x`).
    quiver().evaluate("f = #'int { 5 ~> =^$ }; f 5").expect("5");
    quiver()
        .evaluate("f = #'int { 5 ~> =^$ }; f 6")
        .expect("[]");
    quiver()
        .evaluate("f = #[x: 'int, y: 'int] { $y ~> =^$x }; f [x: 3, y: 3]")
        .expect("3");
    quiver()
        .evaluate("f = #[x: 'int, y: 'int] { $y ~> =^$x }; f [x: 3, y: 4]")
        .expect("[]");
    quiver()
        .evaluate("f = #['int, 'int] { $1 ~> =^$0 }; f [3, 3]")
        .expect("3");
    quiver()
        .evaluate("f = #[p: [q: 'int]] { 9 ~> =^$p.q }; f [p: [q: 9]]")
        .expect("9");
}

#[test]
fn test_pin_path_in_tuple_pattern() {
    // Path pins compose inside tuple patterns like plain pins.
    quiver()
        .evaluate(
            "f = #[x: 'int, pair: ['int, 'int]] { $pair ~> =[^$x, b]; b }; f [x: 5, pair: [5, 9]]",
        )
        .expect("9");
    quiver()
        .evaluate(
            "f = #[x: 'int, pair: ['int, 'int]] { $pair ~> =[^$x, b]; b }; f [x: 5, pair: [6, 9]]",
        )
        .expect("[]");
    quiver()
        .evaluate("p = [limit: 10]; [v: 10] ~> =(v: ^p.limit)")
        .expect("[v: 10]");
}

#[test]
fn test_pin_path_captured_in_closure() {
    // A pin path rooted at an outer variable is captured like the equivalent expression access.
    quiver()
        .evaluate("p = [x: 42]; f = #'int { $ ~> =^p.x }; f 42")
        .expect("42");
    quiver()
        .evaluate("p = [x: 42]; f = #'int { $ ~> =^p.x }; f 41")
        .expect("[]");
}

#[test]
fn test_pin_path_through_union_root() {
    // The field sits at different positions across the union's members, so the pin's
    // access step resolves by name at runtime.
    let src = "'u = A[pad: 'int, x: 'int] | B[x: 'int];\n\
               f = #'u { { 5 ~> =^$x => Yes | No } };\n\
               [f A[pad: 0, x: 5], f B[x: 5], f B[x: 6]]";
    quiver().evaluate(src).expect("[Yes, Yes, No]");
}

#[test]
fn test_pin_path_in_alternation() {
    // Path pins bind nothing, so alternatives stay balanced.
    let src = "f = #[x: 'int, y: 'int] { { 3 ~> =(^$x | ^$y) => Yes | No } };\n\
               [f [x: 3, y: 9], f [x: 9, y: 3], f [x: 9, y: 9]]";
    quiver().evaluate(src).expect("[Yes, Yes, No]");
}

#[test]
fn test_pin_path_unknown_field() {
    // A pin path is resolved against the root's static type, so a missing field is a
    // compile error, not a failed match.
    quiver()
        .evaluate("f = #[x: 'int] { 1 ~> =^$z }; f [x: 1]")
        .expect_compile_error(quiver_compiler::compiler::Error::MemberFieldNotFound {
            field_name: "z".to_string(),
            target: "$".to_string(),
        });
    quiver()
        .evaluate("p = [x: 1]; 1 ~> =^p.z")
        .expect_compile_error(quiver_compiler::compiler::Error::MemberFieldNotFound {
            field_name: "z".to_string(),
            target: "p".to_string(),
        });
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
    let src = "'ab = A['int] | B['int];\nf = #'ab { =(A[x] | B[x]) => x };\nB[7] ~> f ~";
    quiver().evaluate(src).expect("7");
}

#[test]
fn test_or_pattern_inconsistent_bindings_is_error() {
    // Alternatives that bind different variables are rejected at compile time.
    let src = "'ab = A['int] | B['int];\nf = #'ab { =(A[x] | B[y]) => 9 };\nA[1] ~> f ~";
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
        .evaluate("value = 42; value ~> { ='int => \"is_int\" | \"is_bin\" }")
        .expect("\"is_int\"");

    // Test type narrowing for bin
    quiver()
        .evaluate("value = <abcd>; value ~> { ='bin => \"is_bin\" | \"is_int\" }")
        .expect("\"is_bin\"");

    // Test type narrowing failure falls through
    quiver()
        .evaluate("value = 42; value ~> { ='bin => \"is_bin\" | \"not_bin\" }")
        .expect("\"not_bin\"");
}

#[test]
fn test_type_narrowing_in_function() {
    quiver()
        .evaluate("f = #('int | 'bin) { ='int => $ }; 1 ~> f ~")
        .expect_type("'int");
}

#[test]
fn test_narrowing_multiple_matching_variants() {
    // Multiple variants match the pattern structurally: `=A[x]` covers both `A['int]` and
    // `A['bin]`, so `x` is `'int | 'bin`. The argument `A[1]` is an `A`, so it always matches —
    // the unhandled `B` is unreachable here, so the result carries no `| []`.
    quiver()
        .evaluate("f = #(A['int] | A['bin] | B['int]) { =A[x] => x }; A[1] ~> f ~")
        .expect_type("'bin | 'int");
}

#[test]
fn test_narrowing_no_matching_variants() {
    // No variants match - should return just []
    quiver()
        .evaluate("f = #(A['int] | B['int]) { =C[x] }; A[1] ~> f ~")
        .expect_type("[]");
}

#[test]
fn test_narrowing_all_variants_match() {
    // All variants match structurally - exhaustive matching removes [] from result type
    quiver()
        .evaluate("f = #(A['int] | A['bin]) { =A[x] => x }; A[1] ~> f ~")
        .expect_type("'bin | 'int");
}

#[test]
fn test_narrowing_nested_field() {
    // Pattern narrows based on nested tuple types
    quiver()
        .evaluate("f = #(X[A['int]] | X[B['int]]) { =X[A['int]] => $ }; X[A[1]] ~> f ~")
        .expect_type("X[A['int]]");
}

#[test]
fn test_narrowing_with_wildcard() {
    // Wildcards match anything - only outer structure matters
    quiver()
        .evaluate("f = #(A['int, 'bin] | B['int, 'bin]) { =A[_, _] => $ }; A[1, <ff>] ~> f ~")
        .expect_type("A['int, 'bin]");
}

#[test]
fn test_narrowing_with_literal() {
    // Literal pattern should narrow to only matching variant
    quiver()
        .evaluate("f = #(A['int] | B['int]) { =A[1] => $ }; A[1] ~> f ~")
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
            };
            A[1, 1] ~> f ~
            "#,
        )
        .expect_type("'int | []");
}

#[test]
fn test_narrowing_partial_types() {
    // Should work with partial types
    quiver()
        .evaluate("f = #(A[x: 'int] | B[x: 'int]) { =A[x: 'int] => $ }; A[x: 1] ~> f ~")
        .expect_type("A[x: 'int]");
}

#[test]
fn test_narrowing_type_and_variable_pin() {
    // Combines structural narrowing with runtime variable check
    quiver()
        .evaluate("y = 2; f = #(A['int] | B['int]) { =A[^y] => $ }; A[2] ~> f ~")
        .expect_type("A['int] | []");
}

#[test]
fn test_narrowing_nested_union_in_field() {
    quiver()
        .evaluate("f = #(A['int | 'bin] | B['int]) { =A[('int | 'bin)] => $ }; A[1] ~> f ~")
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
            mk = #'k { Rec[tag: ~] };
            f = #'rec { =(tag: Cat) => IsCat | No };
            [Cat ~> mk ~ ~> f ~, Dog ~> mk ~ ~> f ~]
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
            mk = #'k { Rec[tag: ~, label: Dog] };
            f = #'rec { (tag: Cat, label: l) = ~ => Got[l] | No };
            [Cat ~> mk ~ ~> f ~, Dog ~> mk ~ ~> f ~]
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
            };
            [[a: 1, mode: A] ~> f ~, [a: 1, mode: W] ~> f ~, [a: 1, mode: R] ~> f ~, [a: 1] ~> f ~]
            "#,
        )
        .expect("[2, 1, 3, 0]");
}

#[test]
fn test_narrowing_with_branches() {
    quiver()
        .evaluate("f = #(A | B | C) { =(A | B) => =A }; A ~> f ~")
        .expect_type("A | []");

    quiver()
        .evaluate("f = #(A | B | C) { =(A | B) => =A | X }; A ~> f ~")
        .expect_type("A | X | []");

    quiver()
        .evaluate("f = #(A | B | C) { =(A | B) => 1 | X }; A ~> f ~")
        .expect_type("'int | X");
}

#[test]
fn test_narrowing_star_pattern() {
    // Star matches everything - no narrowing, no failure possible
    quiver()
        .evaluate("f = #(A['int] | B['int]) { =_ => 1 }; A[1] ~> f ~")
        .expect_type("'int");
}

#[test]
fn test_narrowing_with_type_alias() {
    // Type alias should work for narrowing
    quiver()
        .evaluate(
            r#"
            'a = A['int, 'int];
            f = #(A['int, 'int] | B['int, 'int]) { ='a => $ };
            A[1, 2] ~> f ~
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
            f = #(Box[A['int]] | Box[B['int]]) { ='box<A['int]> => $ };
            Box[A[1]] ~> f ~
            "#,
        )
        .expect_type("Box[A['int]]");
}

#[test]
fn test_narrowing_preserves_field_types() {
    // Narrowing should preserve the exact field types from matching variants
    quiver()
        .evaluate("f = #(A['int] | B['int]) { =A[1] => 1 }; A[1] ~> f ~")
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
            };
            X[Y[A[1]]] ~> f ~
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
            };
            value = A[1];
            value ~> f ~
            "#,
        )
        .expect_type("'int");
}

#[test]
fn test_narrowing_with_fallback_branch() {
    quiver()
        .evaluate("f = #('int | 'bin) { ='bin | <ff> }; f")
        .expect_type("#('bin | 'int) -> 'bin");

    quiver()
        .evaluate("f = #('int | 'bin) { ='bin | <ff> }; <0a> ~> f ~")
        .expect("<0a>");
    quiver()
        .evaluate("f = #('int | 'bin) { ='bin | <ff> }; 42 ~> f ~")
        .expect("<ff>");
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
fn test_nil_match_type_narrowing() {
    // When `=[]` succeeds, it proves input was nil
    // Subsequent branches receive narrowed type with nil subtracted, so `=A` cannot fail
    quiver()
        .evaluate("#(A | []) { =[] => Nil | =A }")
        .expect_type("#(A | []) -> (A | Nil)");
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
              =[xs, ys];
              t = [xs, ys];
              {
                | t ~> =[Nil, zs] => zs
                | t ~> =[Cons[head, tail], zs] => zs
              }
            };

            [[Nil, Cons[1, Nil]] ~> f ~, [Cons[1, Nil], Cons[2, Nil]] ~> f ~]
            "#,
        )
        .expect("[Cons[1, Nil], Cons[2, Nil]]");
}

#[test]
fn test_conjunction_binds_and_asserts_type() {
    // `('int & x)` matches the value against `'int` AND binds the whole value to `x`.
    quiver().evaluate("42 ~> =('int & x); x").expect("42");
    // The binding is at the narrowed type, so `x` is usable as an int.
    quiver()
        .evaluate("42 ~> =('int & x); [x, 1] ~> __integer_add__ ~")
        .expect("43");
    // A type mismatch fails the match (yields nil), like any failed match.
    quiver().evaluate("<0a> ~> =('int & x)").expect("[]");
}

#[test]
fn test_conjunction_propagates_nil() {
    // The key use: assert-and-bind that fails (propagates) on nil, replacing the `=x, x` re-emit.
    // A non-nil value binds and continues; a nil value fails the assertion and short-circuits.
    quiver()
        .evaluate(
            "opt = #'int { | =0 => [] | $ }; 5 ~> opt ~ ~> =('int & x); x ~> [~, 1] ~> __integer_add__ ~",
        )
        .expect("6");
    quiver()
        .evaluate("opt = #'int { | =0 => [] | $ }; 0 ~> opt ~ ~> =('int & x); x")
        .expect("[]");
}

#[test]
fn test_conjunction_narrows_union_in_field() {
    // Nested in a field, the as-binder narrows a union variant by field type and binds the field.
    quiver()
        .evaluate(
            "f = #(A[a: 'int] | A[a: 'bin]) { =A[a: ('int & x)] => x | NoMatch }; A[a: 5] ~> f ~",
        )
        .expect("5");
    // The other variant fails the field-type assertion.
    quiver()
        .evaluate(
            "f = #(A[a: 'int] | A[a: 'bin]) { =A[a: ('int & x)] => x | NoMatch }; A[a: <0a>] ~> f ~",
        )
        .expect("NoMatch");
    // The partial-pattern spelling works identically.
    quiver()
        .evaluate("A[a: 5] ~> =A(a: ('int & x)); x")
        .expect("5");
}

#[test]
fn test_conjunction_captures_whole_value() {
    // At the top level the binder captures the whole value, ascribed the parenthesised type.
    quiver()
        .evaluate("A[a: 5] ~> =(A[a: 'int] & whole); whole")
        .expect("A[a: 5]");
}

#[test]
fn test_conjunction_with_union_type() {
    quiver()
        .evaluate("<0a> ~> =(('int | 'bin) & x); x")
        .expect("<0a>");
    quiver()
        .evaluate("42 ~> =(('int | 'bin) & x); x")
        .expect("42");
}

#[test]
fn test_conjunction_over_literal_alternation() {
    // The ascribed head is a pattern, so it reaches alternations the type grammar cannot
    // spell — a set of integer literals has no type to state.
    quiver()
        .evaluate("9 ~> =((32 | 9 | 10 | 13) & b); b")
        .expect("9");
    quiver()
        .evaluate("7 ~> =((32 | 9 | 10 | 13) & b); b")
        .expect("[]");
}

#[test]
fn test_conjunction_over_literal_alternation_narrows() {
    // The binder takes the alternatives' union, so an int-only operation accepts it.
    quiver()
        .evaluate("1 ~> =((0 | 1) & n); %num.add [n, 1]")
        .expect("2");
    // ... and a nil alternative widens it back, so the narrowed type is not just `'int`.
    quiver()
        .evaluate("[] ~> =(([] | 1) & n); { n ~> ='int => Int | Other }")
        .expect("Other");
}

#[test]
fn test_conjunction_over_binding_alternation() {
    // Alternatives may bind, and the ascription's own binder captures the whole value
    // alongside them — one binding set per alternative, each carrying both.
    let src = "[1, []] ~> =(([x, []] | [[], x]) & whole); [x, whole]";
    quiver().evaluate(src).expect("[1, [1, []]]");
    let src = "[[], 2] ~> =(([x, []] | [[], x]) & whole); [x, whole]";
    quiver().evaluate(src).expect("[2, [[], 2]]");
}

#[test]
fn test_conjunction_over_alternation_of_pins() {
    // A pin inside an ascribed alternation still resolves against the enclosing scope.
    let src = "f = #[x: 'int, y: 'int] { { 3 ~> =((^$x | ^$y) & v) => v | No } };\n\
               [f [x: 3, y: 9], f [x: 9, y: 3], f [x: 9, y: 9]]";
    quiver().evaluate(src).expect("[3, 3, No]");
}

#[test]
fn test_conjunction_over_alternation_repeated_binder() {
    // A repeated binder is a runtime equality check, as it is for a plain ascription.
    quiver()
        .evaluate("[0, 0] ~> =[((0 | 1) & n), n]; n")
        .expect("0");
    quiver()
        .evaluate("[0, 1] ~> =[((0 | 1) & n), n]; n")
        .expect("[]");
}

#[test]
fn test_conjunction_binder_order_is_irrelevant() {
    // A binder takes the value at the meet of the other conjuncts wherever it is written.
    quiver()
        .evaluate("f = #('int | 'bin) { =(x & 'int) => %num.add [x, 1] | 0 }; [f 4, f <01>]")
        .expect("[5, 0]");
}

#[test]
fn test_conjunction_binds_several_names() {
    quiver()
        .evaluate("5 ~> =(a & b & 'int); [a, b]")
        .expect("[5, 5]");
    // Nested conjunctions flatten into one.
    quiver()
        .evaluate("5 ~> =(('int & a) & b); %num.add [a, b]")
        .expect("10");
}

#[test]
fn test_conjunction_of_structural_patterns() {
    // A destructuring conjunct binds alongside the whole-value binder.
    quiver()
        .evaluate("Point[x: 1, y: 2] ~> =(Point[x: a, y: _] & p); [a, p]")
        .expect("[1, Point[x: 1, y: 2]]");
    // Each conjunct must match.
    quiver()
        .evaluate("Point[x: 1, y: 2] ~> { =(Point[x: 1, y: _] & (y: 3)) => Hit | Miss }")
        .expect("Miss");
}

#[test]
fn test_conjunction_binds_tighter_than_alternation() {
    quiver()
        .evaluate("f = #(A['int] | B['int]) { =(A[n] & v | B[n] & v) => [n, v] }; [f A[1], f B[2]]")
        .expect("[[1, A[1]], [2, B[2]]]");
}

#[test]
fn test_conjunction_with_a_resource_type_member() {
    // A type the pattern grammar cannot spell bare is still a conjunct.
    quiver()
        .evaluate("f = #(@'int | 'int) { | =(@'int & p) => Proc | Int }; f 3")
        .expect("Int");
}

#[test]
fn test_old_ascription_syntax_is_rejected() {
    quiver()
        .evaluate("42 ~> =('int)x; x")
        .expect_parse_failure();
}

#[test]
fn test_conjunction_over_alternation_inconsistent_bindings_is_error() {
    // The head is analysed as an ordinary alternation, so its balance rule still applies.
    let src = "'ab = A['int] | B['int];\nf = #'ab { =((A[x] | B[y]) & v) => 9 };\nA[1] ~> f ~";
    quiver().evaluate(src).expect_compile_error(
        quiver_compiler::compiler::Error::OrPatternBindingMismatch {
            expected: vec!["x".to_string()],
            found: vec!["y".to_string()],
        },
    );
}

// --- Unnamed tuple patterns destructure any tuple name ---------------------------
// An unnamed tuple pattern with fields doesn't constrain the value's name (state the
// name to require it); the empty unnamed pattern `[]` is the nil literal, an exact
// test. This matches the spec's destructuring examples.

#[test]
fn test_unnamed_pattern_rejects_named_value() {
    quiver()
        .evaluate("[x: a, y: b] = Point[x: 10, y: 20]; a")
        .expect("[]");
    // A partial pattern destructures regardless of the name.
    quiver()
        .evaluate("(x, y) = Point[x: 10, y: 20]; x")
        .expect("10");
}

#[test]
fn test_unnamed_positional_pattern_rejects_named_value() {
    quiver().evaluate("[x, _] = Point[10, 20]; x").expect("[]");
    quiver().evaluate("[x, _] = [10, 20]; x").expect("10");
}

#[test]
fn test_named_pattern_still_requires_name() {
    quiver()
        .evaluate("{ Size[10, 20] ~> =Point[a, _] => a | NoMatch }")
        .expect("NoMatch");
}

#[test]
fn test_unnamed_pattern_rejects_named_union_members() {
    // No member is unnamed, so the pattern matches neither; an alternation
    // states the names and destructures both.
    quiver()
        .evaluate(
            r#"
            'shape = Point['int, 'int] | Size['int, 'int]
            f = #'shape { | =[a, _]; a | NoMatch }
            [f Point[1, 2], f Size[3, 4]]
            "#,
        )
        .expect("[NoMatch, NoMatch]");
    quiver()
        .evaluate(
            r#"
            'shape = Point['int, 'int] | Size['int, 'int]
            f = #'shape { =(Point[a, _] | Size[a, _]); a }
            [f Point[1, 2], f Size[3, 4]]
            "#,
        )
        .expect("[1, 3]");
}

#[test]
fn test_empty_pattern_is_exact_nil_test() {
    // `=[]` is the nil literal: it never matches a named empty tuple.
    quiver()
        .evaluate("{ Blue ~> =[] => Matched | NotNil }")
        .expect("NotNil");
    quiver()
        .evaluate("{ [] ~> =[] => Matched | NotNil }")
        .expect("Matched");
}

#[test]
fn test_ascription_stays_name_strict() {
    // The exact-shape test is a type: ascription requires the unnamed tuple type,
    // where the bracket pattern would destructure any name.
    quiver()
        .evaluate("{ A[1] ~> =(['int] & v) => v | No }")
        .expect("No");
    quiver()
        .evaluate("{ [1] ~> =(['int] & v) => v | No }")
        .expect("[1]");
}

#[test]
fn test_destructure_covering_a_union_is_total() {
    // Both members are pairs, so `[x, y]` matches either: the step can't fail, and a match that
    // can't fail may continue its chain.
    quiver()
        .evaluate(
            "f = #'int { | =0 => [A, B] | [A, C] }
             g = #'int { [x, y] = f $; y }
             [g 0, g 1]",
        )
        .expect("[B, C]");
    quiver()
        .evaluate(
            "f = #'int { | =0 => [A, B] | [A, C] }
             g = #'int { [x, y] = f $; y }
             g",
        )
        .expect_type("#'int -> (B | C)");
    quiver()
        .evaluate(
            "f = #'int { | =0 => [A, B] | [A, C] }
             g = #'int { f $ ~> =[x, y] ~> [~, x] }
             g 0",
        )
        .expect("[[A, B], A]");
}

#[test]
fn test_destructure_covering_a_union_with_a_recursive_field_is_total() {
    // The fresh `[A, $1]` literal carries a row the matched member does not; rows are metadata,
    // so the members still cancel.
    quiver()
        .evaluate(
            "'l = Nil | Cons['int, ^]
             f = #['int, 'l] { | $0 ~> =0 => [A, $1] | [A, Nil] }
             g = #['int, 'l] { [x, y] = f $; y }
             g",
        )
        .expect_type("#['int, (Cons['int, μ1] | Nil)] -> (Cons['int, μ1] | Nil)");
}

#[test]
fn test_destructure_testing_a_member_stays_fallible() {
    quiver()
        .evaluate(
            "f = #'int { | =0 => [A, B] | [A, C] }
             g = #'int { f $ ~> =[x, C] }
             g",
        )
        .expect_type("#'int -> ([A, C] | [])");
    // A recursive field constrained more deeply than the narrowed type records is not covered.
    quiver()
        .evaluate("'l = Nil | Cons['int, ^]; f = #'l { $ ~> =Cons[x, Cons[y, z]] }; f")
        .expect_type("#(Cons['int, μ1] | Nil) -> (Cons['int, (Cons['int, μ1] | Nil)] | [])");
}
