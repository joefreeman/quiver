mod common;
use common::*;

#[test]
fn test_simple_type_definition() {
    quiver()
        .evaluate("type circle = Circle[r: int]")
        .expect("[]");
}

#[test]
fn test_union_type_definition() {
    quiver()
        .evaluate("type shape = Circle[r: int] | Rectangle[w: int, h: int]")
        .expect("[]");
}

#[test]
fn test_function_with_type_pattern() {
    quiver()
        .evaluate(
            r#"
            type shape = Circle[r: int] | Rectangle[w: int, h: int];
            area = #shape {
              | Circle[r: r] = $ => [r, r] ~> <multiply>
              | Rectangle[w: w, h: h] = $ => [w, h] ~> <multiply>
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
            type list = Nil | Cons[int, list];
            xs = Cons[1, Cons[2, Cons[3, Nil]]],
            [xs.1.0, xs.1.1.0] ~> <add>;
            "#,
        )
        .expect("5")
}

#[test]
fn test_nested_variant_pattern_matching_in_untyped_function() {
    quiver()
        .evaluate(
            r#"
            type list = Nil | Cons[int, list];

            xs = Cons[10, Cons[20, Cons[30, Nil]]];
            result = xs ~> {
                | Cons[_, Cons[h, _]] = $ => h
                | _ = $ => 999
            };
            result
            "#,
        )
        .expect("20");
}

#[test]
fn test_nested_variant_pattern_matching_in_typed_function() {
    quiver()
        .evaluate(
            r#"
            type list = Nil | Cons[int, list];

            // Test extracting second element with nested pattern
            get_second = #list {
                | Cons[_, Cons[h, _]] = $ => h
                | _ = $ => 999
            };

            xs = Cons[10, Cons[20, Cons[30, Nil]]];
            xs ~> get_second
            "#,
        )
        .expect("20");

    quiver()
        .evaluate(
            r#"
            type list = Nil | Cons[int, list];

            get_first_two = #list {
                | Cons[first, Cons[second, _]] = $ => [first, second]
                | _ = $ => [0, 0]
            };

            xs = Cons[10, Cons[20, Cons[30, Nil]]];
            xs ~> get_first_two
            "#,
        )
        .expect("[10, 20]");

    quiver()
        .evaluate(
            r#"
            type list = Nil | Cons[int, list];

            get_third = #list {
                | Cons[_, Cons[_, Cons[h, _]]] = $ => h
                | _ = $ => 999
            };

            xs = Cons[10, Cons[20, Cons[30, Cons[40, Nil]]]];
            xs ~> get_third
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
            type tree = Leaf[int] | Node[tree, tree];

            // Function with multiple nested patterns requiring runtime checks
            extract_left_leaf = #tree {
                | Node[Node[Leaf[x], _], _] = $ => x
                | Node[Leaf[x], _] = $ => x
                | Leaf[x] = $ => x
            };

            t1 = Node[Node[Leaf[42], Leaf[99]], Leaf[7]];
            t2 = Node[Leaf[15], Leaf[25]];
            t3 = Leaf[3];

            r1 = t1 ~> extract_left_leaf;
            r2 = t2 ~> extract_left_leaf;
            r3 = t3 ~> extract_left_leaf;

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
            type list = Nil | Cons[int, list];
            get_head = #list {
              | Cons[h, _] = $ => h
              | Nil = $ => 0
            };
            xs = Cons[1, Cons[2, Cons[3, Nil]]],
            xs ~> get_head;
            "#,
        )
        .expect("1")
}

#[test]
fn test_recursive_tree_type() {
    quiver()
        .evaluate(
            r#"
            type tree = Node[left: tree, right: tree] | Leaf[int];
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
            Leaf[value] = t.right.left.left,
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
            type list = Nil | Cons[int, list];
            prepend = #list { Cons[10, $] },
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
            type t = Empty | Full[t];

            // This function matches on a tuple where the first element is a recursive type
            // The bug would occur when the pattern compiler tried to access field 0 of Empty
            // (which has no fields) when matching the pattern [Full[rest], n]
            match_recursive = #[t, int] {
                [Empty, n] = $ => n |
                [Full[rest], n] = $ => [n, 100] ~> <add>
            };

            // Test with Empty - should return n
            r1 = [Empty, 42] ~> match_recursive;

            // Test with Full[Empty] - should return n + 100
            r2 = [Full[Empty], 42] ~> match_recursive;

            // Test with Full[Full[Empty]] - should return n + 100
            r3 = [Full[Full[Empty]], 42] ~> match_recursive;

            [r1, r2, r3]
            "#,
        )
        .expect("[42, 142, 142]");

    // Test with a more complex recursive type
    quiver()
        .evaluate(
            r#"
            type tree = Leaf[int] | Node[tree, tree];

            // Function that matches on first element of tuple
            match_first = #[tree, int] {
                [Leaf[x], n] = $ => [x, n] ~> <add> |
                [Node[l, r], n] = $ => n
            };

            t1 = [Leaf[42], 10] ~> match_first;
            t2 = [Node[Leaf[1], Leaf[2]], 20] ~> match_first;

            [t1, t2]
            "#,
        )
        .expect("[52, 20]");

    // Test the exact original bug case scenario
    quiver()
        .evaluate(
            r#"
            type list = Nil | Cons[int, list];

            // Pattern matching that would trigger the bug
            process_list = #[list, int] {
                [Nil, x] = $ => x |
                [Cons[head, tail], x] = $ => [head, x] ~> <add>
            };

            // These should all work without FieldAccessInvalid errors
            r1 = [Nil, 10] ~> process_list;
            r2 = [Cons[5, Nil], 10] ~> process_list;
            r3 = [Cons[5, Cons[3, Nil]], 10] ~> process_list;

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
            type t = Empty | Full[t];
            f = #[t, int] {
              | [Empty, _] = $ => 100
              | [Full[rest], n] = $ => 200
            };
            [Empty, 1] ~> f
            "#,
        )
        .expect("100");

    quiver()
        .evaluate(
            r#"
            type t = Empty | Full[t];
            f = #[t, int] {
              | [Empty, _] = $ => 100
              | [Full[rest], n] = $ => 200
            };
            [Full[Empty], 1] ~> f
            "#,
        )
        .expect("200");
}

#[test]
fn test_recursive_union_pattern() {
    quiver()
        .evaluate(
            r#"
            type t = Empty | Full[t];
            f = #[t, int] {
              | [Empty, _] = $ => 100
              | [Full[rest], n] = $ => [rest, 0] ~> &
            };
            [Full[Empty], 1] ~> f
            "#,
        )
        .expect("100");
}
