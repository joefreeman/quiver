mod common;
use common::*;

#[test]
fn test_basic_generic_type() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            xs = Cons[42, Cons[99, Nil]],
            xs.0
            "#,
        )
        .expect("42");
}

#[test]
fn test_generic_type_with_different_instantiations() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            Cons[1, Cons[2, Nil]]
            "#,
        )
        .expect_type("Cons[int, Cons[int, Nil]]");

    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            Cons['aa', Cons['bb', Nil]],
            "#,
        )
        .expect_type("Cons[bin, Cons[bin, Nil]]");
}

#[test]
fn test_generic_function_single_type_param() {
    quiver()
        .evaluate("#<t>t { ~> =x => x }")
        .expect_type("#t -> t");

    quiver()
        .evaluate("42 ~> #<t>t { ~> =x => x }")
        .expect_type("int");

    quiver()
        .evaluate("'00' ~> #<t>t { ~> =x => x }")
        .expect_type("bin");
}

#[test]
fn test_generic_function_multiple_type_params() {
    quiver()
        .evaluate(
            r#"
            pair = #<a, b>[a, b] { ~> =[x, y] => [y, x] },
            pair[1, '00']
            "#,
        )
        .expect("['00', 1]");
}

#[test]
fn test_generic_function_with_same_type_param_widening() {
    quiver()
        .evaluate("[1, '00'] ~> #<t>[t, t] { ~> =[a, _] => a }")
        .expect_type("bin | int");
}

#[test]
fn test_generic_function_with_recursive_type() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            head = #<t>list<t> {
              | ~> =Cons[h, _] => h
              | ~> =Nil => 0
            },
            Cons[42, Cons[99, Nil]] ~> head
            "#,
        )
        .expect("42");
}

#[test]
fn test_generic_type_structural_equivalence() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            f = #list<int> { ~> =xs => xs },
            g = #list<int> { ~> =xs => xs },
            xs = Cons[42, Nil],
            xs ~> f ~> g
            "#,
        )
        .expect("Cons[42, Nil]");
}

#[test]
fn test_heterogeneous_list_via_widening() {
    quiver()
        .evaluate(
            r#"
            list = %"list",
            list.new[]
              ~> list.append[~, 5]
              ~> list.append[~, "a"],
            "#,
        )
        .expect_type("[] | Cons[(int | Str[bin]), μ1] | Nil");
}

#[test]
fn test_nested_generic_types() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            pair<a, b> : Pair[fst: a, snd: b];

            xs = Cons[Pair[fst: 1, snd: 2], Cons[Pair[fst: 3, snd: 4], Nil]],
            xs.0.fst
            "#,
        )
        .expect("1");
}

#[test]
fn test_generic_function_return_type_inference() {
    quiver()
        .evaluate("#<t>t { ~> =x => A[x] }")
        .expect_type("#t -> A[t]");
}

#[test]
fn test_generic_function_with_tuple_fields() {
    quiver()
        .evaluate(
            r#"
            pair<a, b> : Pair[fst: a, snd: b];
            swap = #<a, b>pair<a, b> {
              ~> =Pair[fst: x, snd: y] => Pair[fst: y, snd: x]
            },
            Pair[fst: 1, snd: "a"] ~> swap
            "#,
        )
        .expect("Pair[fst: \"a\", snd: 1]");
}

#[test]
fn test_multiple_generic_instantiations_same_structure() {
    quiver()
        .evaluate(
            r#"
            pair<t> : Pair[fst: t, snd: t];
            p1 = Pair[fst: 1, snd: 2],
            p2 = Pair[fst: 3, snd: 4],
            [p1.fst, p2.fst]
            "#,
        )
        .expect("[1, 3]");
}

#[test]
fn test_generic_type_with_partial_type() {
    quiver()
        .evaluate(
            r#"
            pair<t> : Pair[fst: t, snd: t];
            get_fst = #<t>pair<t> { ~> .fst },
            Pair[fst: 42, snd: 99] ~> get_fst
            "#,
        )
        .expect("42");
}

#[test]
fn test_generic_function_pattern_matching() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            sum_first_two = #list<int> {
              | ~> =Cons[a, Cons[b, _]] => [a, b] ~> __add__
              | 0
            },
            Cons[10, Cons[20, Cons[30, Nil]]] ~> sum_first_two
            "#,
        )
        .expect("30");
}

#[test]
fn test_wrong_number_of_type_arguments() {
    quiver()
        .evaluate(
            r#"
            pair<a, b> : Pair[a, b];
            f = #pair<int> { ~> =x => x }
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Generic type 'pair' expects 2 type argument(s), got 1".to_string(),
        ));
}

#[test]
fn test_generic_type_cycle_reference() {
    quiver()
        .evaluate(
            r#"
            tree<t> : Leaf[t] | Node[&, &];
            t = Node[Node[Leaf[1], Leaf[2]], Leaf[3]],
            t.0.0.0
            "#,
        )
        .expect("1");
}

#[test]
fn test_generic_function_with_union_result() {
    quiver()
        .evaluate(
            r#"
            maybe<t> : None | Some[t];
            wrap_some = #<t>t { ~> =x => Some[x] },
            42 ~> wrap_some
            "#,
        )
        .expect("Some[42]");
}

#[test]
fn test_generic_nested_function_calls() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            singleton = #<t>t { ~> =x => Cons[x, Nil] },
            head = #<t>list<t> { ~> =Cons[h, _] => h },
            42 ~> singleton ~> head
            "#,
        )
        .expect("42");
}

#[test]
fn test_generic_type_with_multiple_fields() {
    quiver()
        .evaluate(
            r#"
            triple<a, b, c> : Triple[a, b, c];
            t = Triple[1, "a", '00'],
            [t.0, t.1, t.2]
            "#,
        )
        .expect("[1, \"a\", '00']");
}
