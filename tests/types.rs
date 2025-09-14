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
