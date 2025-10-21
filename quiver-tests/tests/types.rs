mod common;
use common::*;

#[test]
fn test_simple_type_definition() {
    quiver().evaluate("circle :: Circle[r: int]").expect("[]");
}

#[test]
fn test_union_type_definition() {
    quiver()
        .evaluate(
            r#"
            shape ::
              | Circle[r: int]
              | Rectangle[w: int, h: int]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_function_with_type_pattern() {
    quiver()
        .evaluate(
            r#"
            shape ::
              | Circle[r: int]
              | Rectangle[w: int, h: int];

            area = #shape {
              | ~> =Circle[r: r] => [r, r] ~> <multiply>
              | ~> =Rectangle[w: w, h: h] => [w, h] ~> <multiply>
            },

            a1 = Circle[r: 5] ~> area,
            a2 = Rectangle[w: 4, h: 3] ~> area,
            [a1, a2] ~> <add>
            "#,
        )
        .expect("37");
}

#[test]
fn test_recursive_list_type() {
    quiver()
        .evaluate(
            r#"
            list :: Nil | Cons[int, &];
            xs = Cons[1, Cons[2, Cons[3, Nil]]],
            [xs.1.0, xs.1.1.0] ~> <add>
            "#,
        )
        .expect("5")
}

#[test]
fn test_cycle_ref_with_pattern_matching() {
    quiver()
        .evaluate(
            r#"
            list :: Nil | Cons[int, &];
            get_head = #list {
              | ~> =Cons[h, _] => h
              | ~> =Nil => 0
            },
            Cons[1, Cons[2, Cons[3, Nil]]] ~> get_head
            "#,
        )
        .expect("1")
}

#[test]
fn test_cycle_ref_nested_depth() {
    quiver()
        .evaluate(
            r#"
            json :: True | False | Array[(Nil | Cons[&0, &1])];
            f = #json { ~> =Array[Cons[a, Cons[b, Nil]]] => [a, b] },
            Array[Cons[False, Cons[True, Nil]]] ~> f
            "#,
        )
        .expect("[False, True]")
}

#[test]
fn test_cycle_ref_error_no_union() {
    // Should fail: cycle without enclosing union
    let result = std::panic::catch_unwind(|| {
        quiver().evaluate("bad :: Bad[&]").expect("should error");
    });
    assert!(result.is_err(), "Expected error for cycle without union");
}

#[test]
fn test_cycle_ref_error_no_base_case() {
    // Should fail: union without base case
    let result = std::panic::catch_unwind(|| {
        quiver()
            .evaluate("bad :: A[&] | B[&]")
            .expect("should error");
    });
    assert!(
        result.is_err(),
        "Expected error for union without base case"
    );
}

#[test]
fn test_cycle_ref_error_no_base_case_nested() {
    // Should fail: cycle nested in tuple field, but no base case in union
    let result = std::panic::catch_unwind(|| {
        quiver()
            .evaluate("bad :: A[x: int, next: &] | B[y: int, next: &]")
            .expect("should error");
    });
    assert!(
        result.is_err(),
        "Expected error for union without base case even with nested cycles"
    );
}

#[test]
fn test_nested_union_pattern_matching_in_block() {
    quiver()
        .evaluate(
            r#"
            list :: Nil | Cons[int, &];

            Cons[10, Cons[20, Cons[30, Nil]]] ~> {
              | ~> =Cons[_, Cons[h, _]] => h
              | 999
            }
            "#,
        )
        .expect("20");
}

#[test]
fn test_nested_union_pattern_matching_in_function() {
    quiver()
        .evaluate(
            r#"
            list :: Nil | Cons[int, &];

            // Test extracting second element with nested pattern
            get_second = #list {
              | ~> =Cons[_, Cons[h, _]] => h
              | 999
            },

            Cons[10, Cons[20, Cons[30, Nil]]] ~> get_second
            "#,
        )
        .expect("20");

    quiver()
        .evaluate(
            r#"
            list :: Nil | Cons[int, &];

            get_first_two = #list {
              | ~> =Cons[first, Cons[second, _]] => [first, second]
              | [0, 0]
            },

            Cons[10, Cons[20, Cons[30, Nil]]] ~> get_first_two
            "#,
        )
        .expect("[10, 20]");

    quiver()
        .evaluate(
            r#"
            list :: Nil | Cons[int, &];

            get_third = #list {
              | ~> =Cons[_, Cons[_, Cons[h, _]]] => h
              | 999
            },

            Cons[10, Cons[20, Cons[30, Cons[40, Nil]]]] ~> get_third
            "#,
        )
        .expect("30");
}

#[test]
fn test_multiple_runtime_type_checks_with_nested_patterns() {
    // Regression test for stack corruption issue with runtime type checks
    // This test ensures that multiple runtime type checks don't leave extra values on the stack
    quiver()
        .evaluate(
            r#"
            tree :: Leaf[int] | Node[&, &];

            // Function with multiple nested patterns requiring runtime checks
            extract_left_leaf = #tree {
              | ~> =Node[Node[Leaf[x], _], _] => x
              | ~> =Node[Leaf[x], _] => x
              | ~> =Leaf[x] => x
            },

            t1 = Node[Node[Leaf[42], Leaf[99]], Leaf[7]],
            t2 = Node[Leaf[15], Leaf[25]],
            t3 = Leaf[3],

            r1 = t1 ~> extract_left_leaf,
            r2 = t2 ~> extract_left_leaf,
            r3 = t3 ~> extract_left_leaf,

            [r1, r2, r3]
            "#,
        )
        .expect("[42, 15, 3]");
}

#[test]
fn test_recursive_type_as_function_parameter() {
    quiver()
        .evaluate(
            r#"
            list :: Nil | Cons[int, &];
            get_head = #list {
              | ~> =Cons[h, _] => h
              | ~> =Nil => 0
            },
            Cons[1, Cons[2, Cons[3, Nil]]] ~> get_head
            "#,
        )
        .expect("1")
}

#[test]
fn test_recursive_tree_type() {
    quiver()
        .evaluate(
            r#"
            tree :: Node[left: &, right: &] | Leaf[int];
            t = Node[
              left: Node[
                left: Leaf[1],
                right: Leaf[2]
              ],
              right: Node[
                left: Node[
                  left: Leaf[3],
                  right: Node[
                    left: Leaf[4],
                    right: Leaf[5]
                  ]
                ],
                right: Leaf[6]
              ]
            ],
            t.right.left.left ~> =Leaf[value],
            value
            "#,
        )
        .expect("3")
}

#[test]
fn test_recursive_type_with_cycle() {
    quiver()
        .evaluate(
            r#"
            list :: Nil | Cons[int, &];
            prepend = #list { ~> =x => Cons[10, x] },
            Cons[20, Cons[30, Nil]] ~> prepend ~> .0
            "#,
        )
        .expect("10");
}

#[test]
fn test_recursive_type_pattern_matching_bug() {
    // Regression test for the bug where pattern matching on recursive types
    // would generate field access instructions before type checks
    // This caused FieldAccessInvalid errors when trying to access fields
    // of the wrong variant

    quiver()
        .evaluate(
            r#"
            t :: Empty | Full[&];

            // This function matches on a tuple where the first element is a recursive type
            // The bug would occur when the pattern compiler tried to access field 0 of Empty
            // (which has no fields) when matching the pattern [Full[rest], n]
            match_recursive = #[t, int] {
              | ~> =[Empty, n] => n
              | ~> =[Full[rest], n] => [n, 100] ~> <add>
            },

            // Test with Empty - should return n
            r1 = [Empty, 42] ~> match_recursive,

            // Test with Full[Empty] - should return n + 100
            r2 = [Full[Empty], 42] ~> match_recursive,

            // Test with Full[Full[Empty]] - should return n + 100
            r3 = [Full[Full[Empty]], 42] ~> match_recursive,

            [r1, r2, r3]
            "#,
        )
        .expect("[42, 142, 142]");

    // Test with a more complex recursive type
    quiver()
        .evaluate(
            r#"
            tree :: Leaf[int] | Node[&, &];

            // Function that matches on first element of tuple
            match_first = #[tree, int] {
              | ~> =[Leaf[x], n] => [x, n] ~> <add>
              | ~> =[Node[l, r], n] => n
            },

            t1 = [Leaf[42], 10] ~> match_first,
            t2 = [Node[Leaf[1], Leaf[2]], 20] ~> match_first,

            [t1, t2]
            "#,
        )
        .expect("[52, 20]");

    // Test the exact original bug case scenario
    quiver()
        .evaluate(
            r#"
            list :: Nil | Cons[int, &];

            // Pattern matching that would trigger the bug
            process_list = #[list, int] {
              | ~> =[Nil, x] => x
              | ~> =[Cons[head, tail], x] => [head, x] ~> <add>
            },

            // These should all work without FieldAccessInvalid errors
            r1 = process_list[Nil, 10],
            r2 = process_list[Cons[5, Nil], 10],
            r3 = process_list[Cons[5, Cons[3, Nil]], 10],

            [r1, r2, r3]
            "#,
        )
        .expect("[10, 15, 15]");
}

#[test]
fn test_union_pattern() {
    quiver()
        .evaluate(
            r#"
            t :: Empty | Full[&];
            f = #[t, int] {
              | ~> =[Empty, _] => 100
              | ~> =[Full[rest], n] => 200
            },
            f[Empty, 1]
            "#,
        )
        .expect("100");

    quiver()
        .evaluate(
            r#"
            t :: Empty | Full[&];
            f = #[t, int] {
              | ~> =[Empty, _] => 100
              | ~> =[Full[rest], n] => 200
            },
            f[Full[Empty], 1]
            "#,
        )
        .expect("200");
}

#[test]
fn test_recursive_union_pattern() {
    quiver()
        .evaluate(
            r#"
            t :: Empty | Full[&];
            f = #[t, int] {
              | ~> =[Empty, _] => 100
              | ~> =[Full[rest], n] => &[rest, 0]
            },
            f[Full[Empty], 1]
            "#,
        )
        .expect("100");
}

#[test]
fn test_unnamed_partial_type() {
    quiver()
        .evaluate(
            r#"
            f = #(x: int, y: int) { ~> =(x, y) => [x, y] },
            a = [x: 1, y: 2] ~> f,
            b = [x: 3, y: 4, z: 5] ~> f,
            c = Point[x: 6, y: 7] ~> f,
            d = Point[x: 8, y: 9, z: 10] ~> f,
            [a, b, c, d]
            "#,
        )
        .expect("[[1, 2], [3, 4], [6, 7], [8, 9]]");

    quiver()
        .evaluate(
            r#"
            f = #(x: int, y: int) { ~> =(x, y) => [x, y] },
            [x: 1, z: 3] ~> f
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with (x: int, y: int)".to_string(),
            found: "[x: int, z: int]".to_string(),
        });
}

#[test]
fn test_named_partial_type() {
    quiver()
        .evaluate("f = #Point(x: int) { ~> .x }, Point[x: 1] ~> f")
        .expect("1");

    quiver()
        .evaluate("f = #Point(x: int) { ~> .x }, [x: 1] ~> f")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with Point(x: int)".to_string(),
            found: "[x: int]".to_string(),
        });

    quiver()
        .evaluate("f = #Point(x: int) { ~> .x }, Other[x: 1] ~> f")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with Point(x: int)".to_string(),
            found: "Other[x: int]".to_string(),
        });
}

#[test]
fn test_empty_partial_type() {
    quiver()
        .evaluate(
            r#"
            f = #() { ~> },
            a = [1, 2, 3] ~> f,
            b = [x: 4, y: 5] ~> f,
            c = Point[x: 6, y: 7] ~> f,
            d = Point ~> f,
            [a, b, c, d]
            "#,
        )
        .expect("[[1, 2, 3], [x: 4, y: 5], Point[x: 6, y: 7], Point]");
}

#[test]
fn test_nested_partial_type() {
    quiver()
        .evaluate(
            r#"
            container :: (value: (x: int, y: int));
            f = #container { ~> =c => [c.value.x, c.value.y] },
            f[value: [x: 1, y: 2, z: 3], extra: 42]
            "#,
        )
        .expect("[1, 2]");
}

#[test]
fn test_union_partial_type() {
    quiver()
        .evaluate(
            r#"
            f = #(A(x: int) | B(x: int)) { ~> .x },
            a = A[x: 10, y: 20] ~> f,
            b = B[x: 42, z: 99] ~> f,
            [a, b]
            "#,
        )
        .expect("[10, 42]");

    quiver()
        .evaluate(
            r#"
            f = #(A(x: int) | B(x: int)) { ~> .x },
            C[x: 10] ~> f
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with (A(x: int) | B(x: int))".to_string(),
            found: "C[x: int]".to_string(),
        });

    quiver()
        .evaluate(
            r#"
            f = #(A(x: int) | B(x: int)) { ~> .x },
            B[y: 10] ~> f
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with (A(x: int) | B(x: int))".to_string(),
            found: "B[y: int]".to_string(),
        });
}

#[test]
fn test_invalid_partial_type() {
    quiver()
        .evaluate("bad :: (int, y: int)")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "All fields in a partial type must be named".to_string(),
        ));
}
