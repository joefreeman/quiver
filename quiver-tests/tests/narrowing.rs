mod common;
use common::*;

// =============================================================================
// Forward Narrowing
// =============================================================================

#[test]
fn test_basic_variable_narrowing_in_chain() {
    // After type check, x is narrowed to A, so x.a is valid
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            x ~> ^A[a: int], x.a
            "#,
        )
        .expect("1");
}

#[test]
fn test_field_narrowing_propagates_to_parent() {
    // Narrowing y.a to int narrows y to A (which has field b)
    quiver()
        .evaluate(
            r#"
            y = 0 ~> #int { ~> =0 => A[a: 1, b: 2] | B[a: '00', c: 3] },
            y.a ~> ^int, y.b
            "#,
        )
        .expect("2");
}

#[test]
fn test_parameter_provenance_narrowing() {
    // Narrowing the parameter also narrows the original variable x
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            x ~> { ~> ^A[a: int] => x.a }
            "#,
        )
        .expect("1");
}

#[test]
fn test_type_intersection_from_multiple_checks() {
    // x ~> ^t ~> ^u narrows to intersection B | C
    quiver()
        .evaluate(
            r#"
            t : A | B | C;
            u : B | C | D;
            x = B,
            x ~> ^t ~> ^u, x
            "#,
        )
        .expect_type("B");
}

#[test]
fn test_inner_scope_inherits_narrowing() {
    // Inner block sees the narrowed type of x
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            x ~> ^A[a: int], { x.a }
            "#,
        )
        .expect("1");
}

// =============================================================================
// Complement Narrowing
// =============================================================================

#[test]
fn test_basic_complement_narrowing() {
    // First branch: y.a is int -> y is A (has b)
    // Second branch: y.a is NOT int -> y is B (has c)
    quiver()
        .evaluate(
            r#"
            y = 0 ~> #int { ~> =0 => A[a: 1, b: 2] | B[a: '00', c: 3] },
            { y.a ~> ^int => y.b | y.c }
            "#,
        )
        .expect("2");

    // Test second branch explicitly
    quiver()
        .evaluate(
            r#"
            y = 1 ~> #int { ~> =0 => A[a: 1, b: 2] | B[a: '00', c: 3] },
            { y.a ~> ^int => y.b | y.c }
            "#,
        )
        .expect("3");
}

#[test]
fn test_complement_from_condition_failure() {
    // Second branch: x is B (has b) because first branch checked for A
    quiver()
        .evaluate(
            r#"
            x = 1 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            { x ~> ^A[a: int] => 0 | x.b }
            "#,
        )
        .expect("2");
}

#[test]
fn test_multiple_type_checks_same_provenance_complement() {
    // Complement of intersection (B|C from A|B|C) is A
    quiver()
        .evaluate(
            r#"
            t : A | B | C;
            u : B | C;
            x = A,
            { x ~> ^t ~> ^u => 1 | x }
            "#,
        )
        .expect_type("A | int");
}

#[test]
fn test_non_type_failable_disables_complement() {
    // Function call after type check disables complement narrowing
    quiver()
        .evaluate(
            r#"
            some_func = #(A[a: int]) -> A[a: int] { ~> },
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            { x ~> ^A[a: int] ~> some_func => 1 | x.a }
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::MemberFieldNotFound {
            field_name: "a".to_string(),
            target: "x".to_string(),
        });
}

#[test]
fn test_multiple_provenances_disables_complement() {
    // Multiple provenances narrowed disables complement
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            y = 0 ~> #int { ~> =0 => C[c: 3] | D[d: 4] },
            { x ~> ^A[a: int], y ~> ^C[c: int] => 1 | x.b }
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::MemberFieldNotFound {
            field_name: "b".to_string(),
            target: "x".to_string(),
        });
}

#[test]
fn test_literal_match_disables_complement() {
    // Literal match is non-type failable, disables complement narrowing
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[val: 1, a: 10] | B[val: 2, b: 20] },
            { x.val ~> =1 => 2 | x.b }
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::MemberFieldNotFound {
            field_name: "b".to_string(),
            target: "x".to_string(),
        });
}

// =============================================================================
// Edge Cases
// =============================================================================

#[test]
fn test_shadowing_does_not_affect_outer_narrowing() {
    // Inner x shadows outer x, but outer narrowing is preserved
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            x ~> ^A[a: int],
            {
                x = B[b: 99],
                x.b
            },
            x.a
            "#,
        )
        .expect("1");
}

#[test]
fn test_complement_with_three_branches() {
    // Each branch narrows the type further
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | ~> =1 => B[b: 2] | C[c: 3] },
            { x ~> ^A[a: int] => 10 | x ~> ^B[b: int] => 20 | x.c }
            "#,
        )
        .expect("10");
}

#[test]
fn test_complement_union_with_common_field() {
    // Narrowing on common field type
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[val: 1, a: 10] | B[val: '00', b: 20] },
            { x.val ~> ^int => x.a | x.b }
            "#,
        )
        .expect("10");
}

// =============================================================================
// Error Cases
// =============================================================================

#[test]
fn test_field_access_on_union_without_narrowing_fails() {
    // Accessing .a on A | B fails when B doesn't have field a
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            x.a
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::MemberFieldNotFound {
            field_name: "a".to_string(),
            target: "x".to_string(),
        });
}

#[test]
fn test_field_access_on_wrong_branch_fails() {
    // After narrowing to A, accessing b (on B) fails
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            x ~> ^A[a: int], x.b
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::MemberFieldNotFound {
            field_name: "b".to_string(),
            target: "x".to_string(),
        });
}

#[test]
fn test_fallback_branch_does_not_require_nil() {
    // A fallback branch that doesn't examine the input should work as exhaustive.
    // This ensures the return type is just int, not int | nil.
    quiver()
        .evaluate(
            r#"
            f = #(A[int] | B[int]) -> int {
              | ~> ^A[int] => 1
              | 2
            },
            A[1] ~> f
            "#,
        )
        .expect("1");
}

// =============================================================================
// Tuple Provenance Narrowing
// =============================================================================

#[test]
fn test_tuple_field_narrowing() {
    // After narrowing t.0 to A, x should be narrowed to A
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            y = C[c: 3],
            t = [x, y],
            t.0 ~> ^A[a: int], x.a
            "#,
        )
        .expect("1");
}

#[test]
fn test_tuple_ripple_preserves_provenance() {
    // Ripple in tuple preserves provenance - narrowing through .0 narrows x
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            t = x ~> [~, 1],
            t.0 ~> ^A[a: int], x.a
            "#,
        )
        .expect("1");
}

#[test]
fn test_nested_tuple_field_access() {
    // Nested tuple field access preserves provenance chain
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            inner = [x],
            outer = [inner],
            outer.0.0 ~> ^A[a: int], x.a
            "#,
        )
        .expect("1");
}

#[test]
fn test_named_tuple_field_narrowing() {
    // Named field access on tuple with provenance works
    quiver()
        .evaluate(
            r#"
            x = 0 ~> #int { ~> =0 => A[a: 1] | B[b: 2] },
            t = [first: x, second: 0],
            t.first ~> ^A[a: int], x.a
            "#,
        )
        .expect("1");
}

// =============================================================================
// Tuple Pattern Complement Narrowing
// =============================================================================

#[test]
fn test_tuple_pattern_complement_first_field() {
    // After matching =[Nil, ys], the first field is known to be Nil.
    // In the second branch, the first field must be Cons[...].
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            f = #<t>[list<t>, list<t>] -> list<t> {
              | ~> =[Nil, ys] => ys
              | ~> =[Cons[head, tail], ys] => ys
            },
            f[Nil, Cons[1, Nil]]
        "#,
        )
        .expect("Cons[1, Nil]");
}

#[test]
fn test_tuple_pattern_complement_second_field() {
    // Same as above but constraining on second field
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            f = #<t>[int, list<t>] -> int {
              | ~> =[n, Nil] => n
              | ~> =[n, Cons[_, _]] => n
            },
            f[42, Cons[1, Nil]]
        "#,
        )
        .expect("42");
}

#[test]
fn test_tuple_pattern_complement_three_variants() {
    // Test with three-variant union
    quiver()
        .evaluate(
            r#"
            tri<t> : A[t] | B[t] | C[t];
            f = #<t>[tri<t>, int] -> int {
              | ~> =[A[_], n] => n
              | ~> =[B[_], n] => n
              | ~> =[C[_], n] => n
            },
            f[B[1], 42]
        "#,
        )
        .expect("42");
}

#[test]
fn test_tuple_pattern_complement_exhaustive() {
    // The reverse' function from the spec should compile without error
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            reverse' = #<t>[list<t>, list<t>] -> list<t> {
              | ~> =[Nil, ys] => ys
              | ~> =[Cons[head, tail], ys] => Cons[head, ys] ~> &[tail, ~]
            },
            reverse'[Cons[1, Cons[2, Nil]], Nil]
        "#,
        )
        .expect("Cons[2, Cons[1, Nil]]");
}

#[test]
fn test_tuple_pattern_multiple_constraining_fields_not_exhaustive() {
    // Multiple constraining fields should NOT enable exhaustiveness
    // This test verifies that [Nil, Nil] and [Cons, Cons] patterns don't
    // incorrectly claim exhaustiveness (missing [Nil, Cons] and [Cons, Nil])
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            f = #<t>[list<t>, list<t>] {
              | ~> =[Nil, Nil] => 0
              | ~> =[Cons[_, _], Cons[_, _]] => 1
            },
            f[Nil, Nil]
        "#,
        )
        .expect_type("[] | int");
}

#[test]
fn test_tuple_pattern_nested_pattern_not_complement() {
    // Nested patterns (like Cons[Cons[x, _], _]) should disable complement
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            f = #<t>[list<list<t>>, int] {
              | ~> =[Nil, n] => n
              | ~> =[Cons[Nil, _], n] => n
            },
            f[Nil, 42]
        "#,
        )
        .expect_type("[] | int");
}
