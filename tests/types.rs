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
            #shape {
              | Circle[r: r] => [r, r] ~> <multiply>
              | Rectangle[w: w, h: h] => [w, h] ~> <multiply>
            } ~> area,
            Circle[r: 5] ~> area! ~> a1,
            Rectangle[w: 4, h: 3] ~> area! ~> a2,
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
            Cons[1, Cons[2, Cons[3, Nil]]] ~> xs,
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

            Cons[10, Cons[20, Cons[30, Nil]]] ~> {
              | Cons[_, Cons[h, _]] => h
              | 999
            }
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
            #list {
              | Cons[_, Cons[h, _]] => h
              | 999
            } ~> get_second,

            Cons[10, Cons[20, Cons[30, Nil]]] ~> get_second!
            "#,
        )
        .expect("20");

    quiver()
        .evaluate(
            r#"
            type list = Nil | Cons[int, list];

            #list {
              | Cons[first, Cons[second, _]] => [first, second]
              | [0, 0]
            } ~> get_first_two,

            Cons[10, Cons[20, Cons[30, Nil]]] ~> get_first_two!
            "#,
        )
        .expect("[10, 20]");

    quiver()
        .evaluate(
            r#"
            type list = Nil | Cons[int, list];

            #list {
              | Cons[_, Cons[_, Cons[h, _]]] => h
              | 999
            } ~> get_third,

            Cons[10, Cons[20, Cons[30, Cons[40, Nil]]]] ~> get_third!
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
            #tree {
              | Node[Node[Leaf[x], _], _] => x
              | Node[Leaf[x], _] => x
              | Leaf[x] => x
            } ~> extract_left_leaf,

            Node[Node[Leaf[42], Leaf[99]], Leaf[7]] ~> t1,
            Node[Leaf[15], Leaf[25]] ~> t2,
            Leaf[3] ~> t3,

            t1 ~> extract_left_leaf! ~> r1,
            t2 ~> extract_left_leaf! ~> r2,
            t3 ~> extract_left_leaf! ~> r3,

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
            #list {
              | Cons[h, _] => h
              | Nil => 0
            } ~> get_head,
            Cons[1, Cons[2, Cons[3, Nil]]] ~> get_head!
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
            Node[
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
            ] ~> t,
            t.right.left.left ~> Leaf[value],
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
            #list { x => Cons[10, x] } ~> prepend,
            Cons[20, Cons[30, Nil]] ~> prepend! ~> .0
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
            #[t, int] {
              | [Empty, n] => n
              | [Full[rest], n] => [n, 100] ~> <add>
            } ~> match_recursive,

            // Test with Empty - should return n
            [Empty, 42] ~> match_recursive! ~> r1,

            // Test with Full[Empty] - should return n + 100
            [Full[Empty], 42] ~> match_recursive! ~> r2,

            // Test with Full[Full[Empty]] - should return n + 100
            [Full[Full[Empty]], 42] ~> match_recursive! ~> r3,

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
            #[tree, int] {
              | [Leaf[x], n] => [x, n] ~> <add>
              | [Node[l, r], n] => n
            } ~> match_first,

            [Leaf[42], 10] ~> match_first! ~> t1,
            [Node[Leaf[1], Leaf[2]], 20] ~> match_first! ~> t2,

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
            #[list, int] {
              | [Nil, x] => x
              | [Cons[head, tail], x] => [head, x] ~> <add>
            } ~> process_list,

            // These should all work without FieldAccessInvalid errors
            [Nil, 10] ~> process_list! ~> r1,
            [Cons[5, Nil], 10] ~> process_list! ~> r2,
            [Cons[5, Cons[3, Nil]], 10] ~> process_list! ~> r3,

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
            #[t, int] {
              | [Empty, _] => 100
              | [Full[rest], n] => 200
            } ~> f,
            [Empty, 1] ~> f!
            "#,
        )
        .expect("100");

    quiver()
        .evaluate(
            r#"
            type t = Empty | Full[t];
            #[t, int] {
              | [Empty, _] => 100
              | [Full[rest], n] => 200
            } ~> f,
            [Full[Empty], 1] ~> f!
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
            #[t, int] {
              | [Empty, _] => 100
              | [Full[rest], n] => [rest, 0] ~> &
            } ~> f,
            [Full[Empty], 1] ~> f!
            "#,
        )
        .expect("100");
}
