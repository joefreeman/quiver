mod common;
use common::*;

#[test]
fn test_simple_type_definition() {
    quiver()
        .evaluate("'circle = Circle[r: 'int]")
        .expect_alias("circle", "Circle[r: 'int]");
}

#[test]
fn test_union_type_definition() {
    quiver()
        .evaluate(
            r#"
            'shape =
              | Circle[r: 'int]
              | Rectangle[w: 'int, h: 'int]
            "#,
        )
        .expect_alias("shape", "Circle[r: 'int] | Rectangle[w: 'int, h: 'int]");
}

#[test]
fn test_function_with_type_pattern() {
    quiver()
        .evaluate(
            r#"
            'shape =
              | Circle[r: 'int]
              | Rectangle[w: 'int, h: 'int];

            area = #'shape {
              | =Circle[r: r] => [r, r] ~> __integer_multiply__ ~
              | =Rectangle[w: w, h: h] => [w, h] ~> __integer_multiply__ ~
            };

            a1 = Circle[r: 5] ~> area ~;
            a2 = Rectangle[w: 4, h: 3] ~> area ~;
            [a1, a2] ~> __integer_add__ ~
            "#,
        )
        .expect("37")
        .expect_variable(
            "area",
            "#(Circle[r: 'int] | Rectangle[w: 'int, h: 'int]) -> 'int",
        );
}

#[test]
fn test_exhaustive_union_matching() {
    // Exhaustive matching should not include [] in return type
    quiver()
        .evaluate(
            r#"
            f = #(A['int] | B['int]) {
              | =A['int] => 10
              | =B['int] => 20
            };
            A[5] ~> f ~
            "#,
        )
        .expect("10")
        .expect_variable("f", "#(A['int] | B['int]) -> 'int");
}

#[test]
fn test_non_exhaustive_union_matching() {
    // Non-exhaustive matching should include [] in return type
    quiver()
        .evaluate(
            r#"
            f = #(A['int] | B['int] | C['int]) {
              | =A['int] => 10
              | =B['int] => 20
            };
            C[99] ~> f ~
            "#,
        )
        .expect("[]")
        .expect_variable("f", "#(A['int] | B['int] | C['int]) -> ('int | [])");
}

#[test]
fn test_value_guard_with_full_coverage() {
    // Even with value guards, full coverage means exhaustive
    quiver()
        .evaluate(
            r#"
            f = #A['int] {
              | =A[10] => 100
              | =A['int] => 999
            };
            A[10] ~> f ~
            "#,
        )
        .expect("100")
        .expect_variable("f", "#A['int] -> 'int");
}

#[test]
fn test_recursive_list_type() {
    quiver()
        .evaluate(
            r#"
            'list = Nil | Cons['int, ^];
            xs = Cons[1, Cons[2, Cons[3, Nil]]];
            [xs.1.0, xs.1.1.0] ~> __integer_add__ ~
            "#,
        )
        .expect("5")
        .expect_variable("xs", "Cons['int, Cons['int, Cons['int, Nil]]]");
}

#[test]
fn test_cycle_ref_with_pattern_matching() {
    quiver()
        .evaluate(
            r#"
            'list = Nil | Cons['int, ^];
            get_head = #'list {
              | =Cons[h, _] => h
              | =Nil => 0
            };
            Cons[1, Cons[2, Cons[3, Nil]]] ~> get_head ~
            "#,
        )
        .expect("1")
        .expect_variable("get_head", "#(Cons['int, μ1] | Nil) -> 'int");
}

#[test]
fn test_cycle_ref_nested_depth() {
    quiver()
        .evaluate(
            r#"
            'json = True | False | Array[(Nil | Cons[^0, ^1])];
            f = #'json { =Array[Cons[a, Cons[b, Nil]]] => [a, b] };
            Array[Cons[False, Cons[True, Nil]]] ~> f ~
            "#,
        )
        .expect("[False, True]");
}

#[test]
fn test_cycle_ref_across_members_of_differing_depth() {
    // `^` counts binders as the type was *written*, so it must resolve the same however the
    // value alternates between members whose `^` sits at different depths — here `Array`'s
    // directly in `Cons`, `Object`'s one tuple deeper. Following a `^` re-enters the binder
    // it names rather than nesting past it, so an array inside an object (and vice versa)
    // fits, and a longer list doesn't shift the outer `^` per element.
    let json = "'json = 'int | Array[(Nil | Cons[^, ^1])] | Object[(Nil | Cons[['%str, ^], ^1])];\n\
                f = #'json { Ok };\n";
    quiver()
        .evaluate(&format!(
            "{json}Object[Cons[[\"a\", Array[Cons[1, Nil]]], Nil]] ~> f ~"
        ))
        .expect("Ok");
    quiver()
        .evaluate(&format!(
            "{json}Array[Cons[Object[Cons[[\"a\", 1], Nil]], Nil]] ~> f ~"
        ))
        .expect("Ok");
    quiver()
        .evaluate(&format!(
            "{json}Object[Cons[[\"a\", Array[Cons[1, Cons[2, Nil]]]], Nil]] ~> f ~"
        ))
        .expect("Ok");
}

#[test]
fn test_cycle_ref_error_no_union() {
    // Should fail: cycle without enclosing union
    let result = std::panic::catch_unwind(|| {
        quiver().evaluate("'bad = Bad[^]").expect("should error");
    });
    assert!(result.is_err(), "Expected error for cycle without union");
}

#[test]
fn test_cycle_ref_error_no_base_case() {
    // Should fail: union without base case
    let result = std::panic::catch_unwind(|| {
        quiver()
            .evaluate("'bad = A[^] | B[^]")
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
            .evaluate("'bad = A[x: 'int, next: ^] | B[y: 'int, next: ^]")
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
            'list = Nil | Cons['int, ^];

            Cons[10, Cons[20, Cons[30, Nil]]] ~> {
              | =Cons[_, Cons[h, _]] => h
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
            'list = Nil | Cons['int, ^];

            // Test extracting second element with nested pattern
            get_second = #'list {
              | =Cons[_, Cons[h, _]] => h
              | 999
            };

            Cons[10, Cons[20, Cons[30, Nil]]] ~> get_second ~
            "#,
        )
        .expect("20");

    quiver()
        .evaluate(
            r#"
            'list = Nil | Cons['int, ^];

            get_first_two = #'list {
              | =Cons[first, Cons[second, _]] => [first, second]
              | [0, 0]
            };

            Cons[10, Cons[20, Cons[30, Nil]]] ~> get_first_two ~
            "#,
        )
        .expect("[10, 20]");

    quiver()
        .evaluate(
            r#"
            'list = Nil | Cons['int, ^];

            get_third = #'list {
              | =Cons[_, Cons[_, Cons[h, _]]] => h
              | 999
            };

            Cons[10, Cons[20, Cons[30, Cons[40, Nil]]]] ~> get_third ~
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
            'tree = Leaf['int] | Node[^, ^];

            // Function with multiple nested patterns requiring runtime checks
            extract_left_leaf = #'tree {
              | =Node[Node[Leaf[x], _], _] => x
              | =Node[Leaf[x], _] => x
              | =Leaf[x] => x
            };

            t1 = Node[Node[Leaf[42], Leaf[99]], Leaf[7]];
            t2 = Node[Leaf[15], Leaf[25]];
            t3 = Leaf[3];

            r1 = t1 ~> extract_left_leaf ~;
            r2 = t2 ~> extract_left_leaf ~;
            r3 = t3 ~> extract_left_leaf ~;

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
            'list = Nil | Cons['int, ^];
            get_head = #'list {
              | =Cons[h, _] => h
              | =Nil => 0
            };
            Cons[1, Cons[2, Cons[3, Nil]]] ~> get_head ~
            "#,
        )
        .expect("1");
}

#[test]
fn test_recursive_tree_type() {
    quiver()
        .evaluate(
            r#"
            'tree = Node[left: ^, right: ^] | Leaf['int];
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
            ];
            t.right.left.left ~> =Leaf[value];
            value
            "#,
        )
        .expect("3");
}

#[test]
fn test_recursive_type_with_cycle() {
    quiver()
        .evaluate(
            r#"
            'list = Nil | Cons['int, ^];
            prepend = #'list { =x => Cons[10, x] };
            Cons[20, Cons[30, Nil]] ~> prepend ~ ~> .0
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
            't = Empty | Full[^];

            // This function matches on a tuple where the first element is a recursive type
            // The bug would occur when the pattern compiler tried to access field 0 of Empty
            // (which has no fields) when matching the pattern [Full[rest], n]
            match_recursive = #['t, 'int] {
              | =[Empty, n] => n
              | =[Full[rest], n] => [n, 100] ~> __integer_add__ ~
            };

            // Test with Empty - should return n
            r1 = [Empty, 42] ~> match_recursive ~;

            // Test with Full[Empty] - should return n + 100
            r2 = [Full[Empty], 42] ~> match_recursive ~;

            // Test with Full[Full[Empty]] - should return n + 100
            r3 = [Full[Full[Empty]], 42] ~> match_recursive ~;

            [r1, r2, r3]
            "#,
        )
        .expect("[42, 142, 142]");

    // Test with a more complex recursive type
    quiver()
        .evaluate(
            r#"
            'tree = Leaf['int] | Node[^, ^];

            // Function that matches on first element of tuple
            match_first = #['tree, 'int] {
              | =[Leaf[x], n] => [x, n] ~> __integer_add__ ~
              | =[Node[l, r], n] => n
            };

            t1 = [Leaf[42], 10] ~> match_first ~;
            t2 = [Node[Leaf[1], Leaf[2]], 20] ~> match_first ~;

            [t1, t2]
            "#,
        )
        .expect("[52, 20]");

    // Test the exact original bug case scenario
    quiver()
        .evaluate(
            r#"
            'list = Nil | Cons['int, ^];

            // Pattern matching that would trigger the bug
            process_list = #['list, 'int] {
              | =[Nil, x] => x
              | =[Cons[head, tail], x] => [head, x] ~> __integer_add__ ~
            };

            // These should all work without FieldAccessInvalid errors
            r1 = [Nil, 10] ~> process_list ~;
            r2 = [Cons[5, Nil], 10] ~> process_list ~;
            r3 = [Cons[5, Cons[3, Nil]], 10] ~> process_list ~;

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
            't = Empty | Full[^];
            f = #['t, 'int] {
              | =[Empty, _] => 100
              | =[Full[rest], n] => 200
            };
            [Empty, 1] ~> f ~
            "#,
        )
        .expect("100");

    quiver()
        .evaluate(
            r#"
            't = Empty | Full[^];
            f = #['t, 'int] {
              | =[Empty, _] => 100
              | =[Full[rest], n] => 200
            };
            [Full[Empty], 1] ~> f ~
            "#,
        )
        .expect("200");
}

#[test]
fn test_recursive_union_pattern() {
    quiver()
        .evaluate(
            r#"
            't = Empty | Full[^];
            f = #['t, 'int] {
              | =[Empty, _] => 100
              | =[Full[rest], n] => [rest, 0] ~> ^ ~
            };
            [Full[Empty], 1] ~> f ~
            "#,
        )
        .expect("100");
}

#[test]
fn test_unnamed_partial_type() {
    quiver()
        .evaluate(
            r#"
            f = #(x: 'int, y: 'int) { =(x, y) => [x, y] };
            a = [x: 1, y: 2] ~> f ~;
            b = [x: 3, y: 4, z: 5] ~> f ~;
            c = Point[x: 6, y: 7] ~> f ~;
            d = Point[x: 8, y: 9, z: 10] ~> f ~;
            [a, b, c, d]
            "#,
        )
        .expect("[[1, 2], [3, 4], [6, 7], [8, 9]]")
        .expect_variable("f", "#(x: 'int, y: 'int) -> ['int, 'int]");

    quiver()
        .evaluate(
            r#"
            f = #(x: 'int, y: 'int) { =(x, y) => [x, y] };
            [x: 1, z: 3] ~> f ~
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with (x: 'int, y: 'int)".to_string(),
            found: "[x: 'int, z: 'int]".to_string(),
        });
}

#[test]
fn test_named_partial_type() {
    quiver()
        .evaluate("f = #Point(x: 'int) { .x }; Point[x: 1] ~> f ~")
        .expect("1")
        .expect_variable("f", "#Point(x: 'int) -> 'int");

    quiver()
        .evaluate("f = #Point(x: 'int) { .x }; [x: 1] ~> f ~")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with Point(x: 'int)".to_string(),
            found: "[x: 'int]".to_string(),
        });

    quiver()
        .evaluate("f = #Point(x: 'int) { .x }; Other[x: 1] ~> f ~")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with Point(x: 'int)".to_string(),
            found: "Other[x: 'int]".to_string(),
        });
}

#[test]
fn test_empty_partial_type() {
    quiver()
        .evaluate(
            r#"
            f = #() { $ };
            a = [1, 2, 3] ~> f ~;
            b = [x: 4, y: 5] ~> f ~;
            c = Point[x: 6, y: 7] ~> f ~;
            d = Point ~> f ~;
            [a, b, c, d]
            "#,
        )
        .expect("[[1, 2, 3], [x: 4, y: 5], Point[x: 6, y: 7], Point]")
        .expect_variable("f", "#() -> ()");
}

#[test]
fn test_nested_partial_type() {
    quiver()
        .evaluate(
            r#"
            'container = (value: (x: 'int, y: 'int));
            f = #'container { =c => [c.value.x, c.value.y] };
            [value: [x: 1, y: 2, z: 3], extra: 42] ~> f ~
            "#,
        )
        .expect("[1, 2]")
        .expect_alias("container", "(value: (x: 'int, y: 'int))");
}

#[test]
fn test_union_partial_type() {
    quiver()
        .evaluate(
            r#"
            f = #(A(x: 'int) | B(x: 'int)) { .x };
            a = A[x: 10, y: 20] ~> f ~;
            b = B[x: 42, z: 99] ~> f ~;
            [a, b]
            "#,
        )
        .expect("[10, 42]");

    quiver()
        .evaluate(
            r#"
            f = #(A(x: 'int) | B(x: 'int)) { .x };
            C[x: 10] ~> f ~
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with A(x: 'int) | B(x: 'int)".to_string(),
            found: "C[x: 'int]".to_string(),
        });

    quiver()
        .evaluate(
            r#"
            f = #(A(x: 'int) | B(x: 'int)) { .x };
            B[y: 10] ~> f ~
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with A(x: 'int) | B(x: 'int)".to_string(),
            found: "B[y: 'int]".to_string(),
        });
}

#[test]
fn test_invalid_partial_type() {
    quiver()
        .evaluate("'bad = ('int, y: 'int)")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "All fields in a partial type must be named".to_string(),
        ));
}

#[test]
fn test_type_spread_basic() {
    quiver()
        .evaluate(
            r#"
            'base = Base[x: 'int];
            'extended = Extended[...'base, y: 'int]
            "#,
        )
        .expect_alias("extended", "Extended[x: 'int, y: 'int]");
}

#[test]
fn test_type_spread_field_override() {
    quiver()
        .evaluate(
            r#"
            'base = Base[x: 'int, y: 'int];
            'modified = Modified[...'base, y: 'bin]
            "#,
        )
        .expect_alias("modified", "Modified[x: 'int, y: 'bin]");
}

#[test]
fn test_type_spread_union_distribution() {
    quiver()
        .evaluate(
            r#"
            'shape = Circle[r: 'int] | Square[s: 'int];
            'colored = Colored[...'shape, color: 'bin]
            "#,
        )
        .expect_alias(
            "colored",
            "Colored[r: 'int, color: 'bin] | Colored[s: 'int, color: 'bin]",
        );
}

#[test]
fn test_type_spread_unnamed() {
    quiver()
        .evaluate(
            r#"
            'base = Base[x: 'int];
            'extended = [...'base, y: 'int]
            "#,
        )
        .expect_alias("extended", "[x: 'int, y: 'int]");
}

#[test]
fn test_type_spread_name_modes() {
    quiver()
        .evaluate(
            r#"
            'base = Base[x: 'int];
            'unnamed = [...'base, y: 'int];
            'renamed = Renamed[...'base, y: 'int]
            "#,
        )
        .expect_alias("unnamed", "[x: 'int, y: 'int]")
        .expect_alias("renamed", "Renamed[x: 'int, y: 'int]");
}

#[test]
fn test_type_spread_multiple_fields() {
    quiver()
        .evaluate(
            r#"
            'point2d = Point2D[x: 'int, y: 'int];
            'point3d = Point3D[...'point2d, z: 'int, color: 'bin]
            "#,
        )
        .expect_alias("point3d", "Point3D[x: 'int, y: 'int, z: 'int, color: 'bin]");
}

#[test]
fn test_type_spread_union_with_override() {
    quiver()
        .evaluate(
            r#"
            'base = A[x: 'int, y: 'int] | B[x: 'int, z: 'int];
            'modified = Modified[...'base, y: 'bin]
            "#,
        )
        .expect_alias(
            "modified",
            "Modified[x: 'int, y: 'bin] | Modified[x: 'int, z: 'int, y: 'bin]",
        );
}

#[test]
fn test_type_spread_empty_base() {
    quiver()
        .evaluate(
            r#"
            'empty = Empty[];
            'extended = Extended[...'empty, x: 'int]
            "#,
        )
        .expect_alias("extended", "Extended[x: 'int]");
}

#[test]
fn test_type_spread_identifier_preserves_name() {
    quiver()
        .evaluate(
            r#"
            'base = Point[x: 'int];
            'extended = 'base[...'base, y: 'int]
            "#,
        )
        .expect_alias("extended", "Point[x: 'int, y: 'int]");
}

#[test]
fn test_type_spread_in_generic_definition() {
    quiver()
        .evaluate(
            r#"
            'base = Base[x: 'int, y: 'int];
            'extended<'t> = Extended[...'base, z: 't]
            "#,
        )
        .expect_alias("extended", "Extended[x: 'int, y: 'int, z: 't]");
}

#[test]
fn test_type_spread_with_generic_fields() {
    quiver()
        .evaluate(
            r#"
            'base = Base[x: 'int];
            'extended<'t, 'u> = Extended[...'base, y: 't, z: 'u]
            "#,
        )
        .expect_alias("extended", "Extended[x: 'int, y: 't, z: 'u]");
}

#[test]
fn test_type_spread_in_generic_union() {
    quiver()
        .evaluate(
            r#"
            'shape = Circle[r: 'int] | Square[s: 'int];
            'colored<'t> = Colored[...'shape, color: 't]
            "#,
        )
        .expect_alias(
            "colored",
            "Colored[r: 'int, color: 't] | Colored[s: 'int, color: 't]",
        );
}

#[test]
fn test_type_spread_with_parameterized_type() {
    quiver()
        .evaluate(
            r#"
            'point<'t> = Point[x: 't, y: 't];
            'point3d<'t> = Point3D[...'point<'t>, z: 't]
            "#,
        )
        .expect_alias("point3d", "Point3D[x: 't, y: 't, z: 't]");
}

#[test]
fn test_type_spread_with_mixed_parameters() {
    quiver()
        .evaluate(
            r#"
            'base<'t> = Base[value: 't];
            'extended<'t, 'u> = Extended[...'base<'t>, extra: 'u]
            "#,
        )
        .expect_alias("extended", "Extended[value: 't, extra: 'u]");
}

#[test]
fn test_type_spread_parameterized_union() {
    quiver()
        .evaluate(
            r#"
            'result<'t, 'e> = Ok[value: 't] | Err[error: 'e];
            'tagged<'t, 'e> = Tagged[...'result<'t, 'e>, tag: 'bin]
            "#,
        )
        .expect_alias(
            "tagged",
            "Tagged[error: 'e, tag: 'bin] | Tagged[value: 't, tag: 'bin]",
        );
}

#[test]
fn test_expect_alias_simple() {
    quiver()
        .evaluate(
            r#"
            'point = Point[x: 'int, y: 'int]
            "#,
        )
        .expect_alias("point", "Point[x: 'int, y: 'int]");
}

#[test]
fn test_expect_alias_with_parameters() {
    quiver()
        .evaluate(
            r#"
            'point<'t> = Point[x: 't, y: 't]
            "#,
        )
        .expect_alias("point", "Point[x: 't, y: 't]");
}

#[test]
fn test_expect_alias_union() {
    quiver()
        .evaluate(
            r#"
            'shape = Circle[r: 'int] | Square[s: 'int]
            "#,
        )
        .expect_alias("shape", "Circle[r: 'int] | Square[s: 'int]");
}

#[test]
fn test_expect_alias_with_spread() {
    quiver()
        .evaluate(
            r#"
            'base = Base[x: 'int];
            'extended = Extended[...'base, y: 'int]
            "#,
        )
        .expect_alias("extended", "Extended[x: 'int, y: 'int]");
}

#[test]
fn test_expect_alias_parameterized_with_spread() {
    quiver()
        .evaluate(
            r#"
            'base<'t> = Base[x: 't];
            'extended<'t> = Extended[...'base<'t>, y: 't]
            "#,
        )
        .expect_alias("extended", "Extended[x: 't, y: 't]");
}

#[test]
fn test_spread_partial_type_basic() {
    quiver()
        .evaluate(
            r#"
            'entity = (id: 'int);
            'user = User[...'entity, name: 'bin]
            "#,
        )
        .expect_alias("user", "User[id: 'int, name: 'bin]");
}

#[test]
fn test_spread_multiple_partials() {
    quiver()
        .evaluate(
            r#"
            'entity = (id: 'int);
            'metadata = (updated_at: 'int, created_at: 'int);
            'user = User[...'entity, name: 'bin, ...'metadata]
            "#,
        )
        .expect_alias(
            "user",
            "User[id: 'int, name: 'bin, updated_at: 'int, created_at: 'int]",
        );
}

#[test]
fn test_spread_partial_with_override() {
    quiver()
        .evaluate(
            r#"
            'base = (x: 'int, y: 'int);
            'extended = Extended[...'base, y: 'bin, z: 'int]
            "#,
        )
        .expect_alias("extended", "Extended[x: 'int, y: 'bin, z: 'int]");
}

#[test]
fn test_spread_partial_and_tuple() {
    quiver()
        .evaluate(
            r#"
            'partial = (x: 'int);
            'tuple = Tuple[y: 'int];
            'combined = Combined[...'partial, ...'tuple, z: 'int]
            "#,
        )
        .expect_alias("combined", "Combined[x: 'int, y: 'int, z: 'int]");
}

#[test]
fn test_identifier_spread_syntax_basic() {
    quiver()
        .evaluate(
            r#"
            'base = Base[x: 'int];
            'extended = 'base[..., y: 'int]
            "#,
        )
        .expect_alias("extended", "Base[x: 'int, y: 'int]");
}

#[test]
fn test_identifier_spread_syntax_union() {
    quiver()
        .evaluate(
            r#"
            'event = Created[id: 'int] | Updated | Deleted;
            'logged = 'event[..., timestamp: 'int]
            "#,
        )
        .expect_alias(
            "logged",
            "Created[id: 'int, timestamp: 'int] | Deleted[timestamp: 'int] | Updated[timestamp: 'int]",
        );
}

#[test]
fn test_identifier_spread_syntax_partial() {
    quiver()
        .evaluate(
            r#"
            'entity = (id: 'int);
            'timestamped = 'entity[..., created_at: 'int]
            "#,
        )
        .expect_alias("timestamped", "[id: 'int, created_at: 'int]");
}

#[test]
fn test_primitive_type_alias_int() {
    quiver()
        .evaluate("'int = Int[x: 'int]")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot redefine primitive type 'int'".to_string(),
        ));
}

#[test]
fn test_primitive_type_alias_bin() {
    quiver()
        .evaluate("'bin = Bin[x: 'int]")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot redefine primitive type 'bin'".to_string(),
        ));
}

// The primitive type names are free as variable names — a type always carries its leading
// apostrophe (`'int`), so a bare `int` is unambiguously a value.
#[test]
fn test_int_allowed_as_variable() {
    quiver().evaluate("42 ~> =int; int").expect("42");
}

#[test]
fn test_bin_allowed_as_variable() {
    quiver().evaluate("<0a> ~> =bin; bin").expect("<0a>");
}

#[test]
fn test_int_allowed_in_destructuring() {
    quiver().evaluate("[1, 2] ~> =[int, y]; int").expect("1");
}

#[test]
fn test_bin_allowed_in_destructuring() {
    quiver()
        .evaluate("[<0a>, <0b>] ~> =[bin, y]; bin")
        .expect("<0a>");
}

#[test]
fn test_int_allowed_in_nested_pattern() {
    quiver()
        .evaluate("[1, [2, 3]] ~> =[x, [y, int]]; int")
        .expect("3");
}

#[test]
fn test_reserved_names_allowed_as_field_names() {
    quiver()
        .evaluate("Config[int: 42, bin: 99]")
        .expect("Config[int: 42, bin: 99]");
}

#[test]
fn test_reserved_names_allowed_in_type_definitions() {
    quiver()
        .evaluate("'data = Data[int: 'int, bin: 'int]; Data[int: 42, bin: 99]")
        .expect("Data[int: 42, bin: 99]");
}

#[test]
fn test_pin_with_type_alias_primitive() {
    quiver()
        .evaluate(
            r#"
            'number_or_bytes = 'int | 'bin;
            42 ~> ='number_or_bytes
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            'number_or_bytes = 'int | 'bin;
            <0a> ~> ='number_or_bytes
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_pin_with_type_alias_union() {
    quiver()
        .evaluate(
            r#"
            'list = Nil | Cons['int, ^];
            Nil ~> ='list
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            'list = Nil | Cons['int, ^];
            Cons[1, Cons[2, Nil]] ~> ='list
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_pin_with_type_alias_mismatch() {
    quiver()
        .evaluate(
            r#"
            'number = 'int;
            <0a> ~> ='number
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            'point = Point[x: 'int, y: 'int];
            42 ~> ='point
            "#,
        )
        .expect("[]");
}

#[test]
fn test_pin_with_type_alias_in_pattern() {
    quiver()
        .evaluate(
            r#"
            'shape = Circle[r: 'int] | Square[s: 'int];
            Circle[r: 5] ~> ='shape
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_pin_with_type_alias_nested() {
    quiver()
        .evaluate(
            r#"
            'inner = 'int | 'bin;
            Wrapper[value: 42] ~> =Wrapper[value: 'inner]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_pin_with_inline_type_primitive() {
    quiver()
        .evaluate(
            r#"
            42 ~> =('int | 'bin)
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            <0a> ~> =('int | 'bin)
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_pin_with_inline_type_mismatch() {
    quiver()
        .evaluate(
            r#"
            A ~> =('int | 'bin)
            "#,
        )
        .expect("[]");
}

#[test]
fn test_pin_with_inline_type_nested() {
    quiver()
        .evaluate(
            r#"
            A[value: 42] ~> =A[value: ('int | 'bin)]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_pin_with_inline_type_complex() {
    quiver()
        .evaluate(
            r#"
            Rectangle[w: 5, h: 10] ~> =(Rectangle[w: 'int, h: 'int] | Circle[r: 'int])
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_inline_type_without_ampersand() {
    // Type expressions work with just = (no & needed for inline types)
    quiver()
        .evaluate(
            r#"
            42 ~> =('int | 'bin)
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_generic_type_explicit_instantiation() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            Cons[42, Nil] ~> ='list<'int>
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_generic_type_explicit_instantiation_mismatch() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            Cons[<aa>, Nil] ~> ='list<'int>
            "#,
        )
        .expect("[]");
}

#[test]
fn test_generic_type_explicit_instantiation_in_pattern() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            Cons[42, Cons[99, Nil]] ~> =Cons[x, 'list<'int>]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_generic_type_without_instantiation_error() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            Cons[42, Nil] ~> ='list
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Generic type 'list' expects 1 type argument(s), got 0".to_string(),
        ));
}

#[test]
fn test_generic_type_short_syntax() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            Cons[42, Nil] ~> ='list<'int>
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_generic_type_short_syntax_mismatch() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            Cons[<aa>, Nil] ~> ='list<'int>
            "#,
        )
        .expect("[]");
}

#[test]
fn test_named_partial_type_without_parens() {
    // Named partial type without extra parentheses: =A(x: 'int)
    quiver()
        .evaluate("A[x: 1, y: 2] ~> =A(x: 'int)")
        .expect("Ok");

    // Type mismatch should fail
    quiver()
        .evaluate("A[x: <ff>, y: 2] ~> =A(x: 'int)")
        .expect("[]");

    // Missing field should fail
    quiver()
        .evaluate("A[y: 2, z: 3] ~> =A(x: 'int)")
        .expect("[]");
}

#[test]
fn test_unnamed_partial_type_without_parens() {
    // Unnamed partial type without extra parentheses: =(x: 'int)
    quiver().evaluate("A[x: 1] ~> =(x: 'int)").expect("Ok");

    quiver().evaluate("[x: 1, y: 2] ~> =(x: 'int)").expect("Ok");

    // Type mismatch should fail
    quiver().evaluate("A[x: <ff>] ~> =(x: 'int)").expect("[]");
}

#[test]
fn test_empty_partial_type_without_parens() {
    // Empty partial type matches any tuple: =()
    quiver().evaluate("A[1] ~> =()").expect("Ok");
    quiver().evaluate("[1, 2, 3] ~> =()").expect("Ok");
    quiver().evaluate("42 ~> =()").expect("[]");
}

#[test]
fn test_type_alias_after_expression_statement() {
    // A type alias may be interspersed after an expression statement; the statement
    // parser must leave the separator for the program parser to consume.
    quiver()
        .evaluate("x = 1; 'n = 'int; 42 ~> ='n; Ok")
        .expect("Ok");
}

#[test]
fn test_generic_type_parameters_do_not_collide_across_definitions() {
    // `pick`'s 't/'u share source names with `apply`'s; each definition's parameters
    // must be distinct variables or the inferred `#{ $0 }` silently fails to project.
    quiver()
        .evaluate(
            r#"
            apply = #<'t, 'u>['t, #'t -> 'u] { =[v, f]; v ~> f ~ }
            pick = #<'t, 'u>['t, 'u] { =[a, b]; apply [[a, b], #{ $0 }] }
            [1, "x"] ~> pick ~
            "#,
        )
        .expect("1");
}

#[test]
fn test_enclosing_generic_variable_in_callee_argument() {
    // The closure captures a `#^`-typed self whose type mentions the enclosing
    // function's 't; applying it must treat that variable as rigid, not unbound.
    quiver()
        .evaluate(
            r#"
            f = #<'t>[#^ -> ('t | []), 't] {
              =[self, n]
              me = #'int { [self, $] ~> self ~ }
              n
            }
            [f, 3] ~> f ~
            "#,
        )
        .expect("3");
}

#[test]
fn test_bare_binder_match_is_irrefutable_on_nil_input() {
    // A bare binder matches anything, including nil: the block's type must not widen
    // with `[]` just because the bound value can be nil.
    quiver().evaluate("{ =x; 42 }").expect_type("'int");
}

#[test]
fn test_function_type_alias_after_expression_statement() {
    // An alias whose RHS is a function type also parses as a chain (an identity literal
    // with a declared return type), so the statement sequence must yield to the alias
    // rather than greedily consuming the line.
    quiver()
        .evaluate("x = 1; 'q<'t> = #['t, 't] -> ('t | []); 42")
        .expect("42");
}

#[test]
fn test_union_folds_members_differing_only_by_annotation_row() {
    // A freshly built literal `Nil` (exact-empty annotation row) widened into a union
    // with an alias-typed `Nil` (open row) must fold to one member, not display as
    // `Nil | Nil` (likewise the bare `[]` next to a rowed one).
    quiver()
        .evaluate(
            r#"
            comma = [44, "','"] ~> %parse.byte ~
            elems = [%parse.int, comma] ~> %parse.sep_by ~
            a1 = [[91, "'['"] ~> %parse.byte ~, elems] ~> %parse.right ~
            a3 = [a1, [93, "']'"] ~> %parse.byte ~] ~> %parse.left ~
            %parse.map [a3, #{ $ }]
            "#,
        )
        .expect_type(
            "#P[data: 'bin, pos: 'int, len: 'int, err: (Expected[offset: 'int, message: Str['bin]] | [])] -> ([(Cons['int, μ1] | Nil), P[data: 'bin, pos: 'int, len: 'int, err: (Expected[offset: 'int, message: Str['bin]] | [])]] | [])",
        );
}

#[test]
fn test_type_alias_then_same_named_binding() {
    // Types and values are separate namespaces: a value binding must not clobber a
    // same-named type alias defined earlier in the same (top-level) scope.
    quiver()
        .evaluate(
            r#"
            'room = Str['bin]
            room = #'int { $ }
            f = #'room { Ok }
            ["hi" ~> f ~, 7 ~> room ~]
            "#,
        )
        .expect("[Ok, 7]");
}

#[test]
fn test_binding_then_same_named_type_alias() {
    // ... and in the other order: a type alias must not clobber a same-named
    // value binding defined earlier in the same scope.
    quiver()
        .evaluate(
            r#"
            room = 42
            'room = Str['bin]
            f = #'room { Ok }
            ["hi" ~> f ~, room]
            "#,
        )
        .expect("[Ok, 42]");
}

#[test]
fn test_same_named_alias_and_binding_in_function_body() {
    // A binding inside a function body must not shadow out an outer type alias of
    // the same name: the alias stays usable after the binding.
    quiver()
        .evaluate(
            r#"
            'room = Str['bin]
            main = #{
              room = #'int { $ }
              g = #'room { Ok }
              ["hi" ~> g ~, 7 ~> room ~]
            }
            main []
            "#,
        )
        .expect("[Ok, 7]");
}

#[test]
fn test_module_alias_and_binding_share_name() {
    // Module top level: a module may export a function under the same name as one of
    // its type aliases, with the alias still usable after the binding.
    let mut modules = std::collections::HashMap::new();
    modules.insert(
        vec!["frames".to_string()],
        r#"
        'frame = Frame['int]
        frame = #'int { Frame[$] }
        unwrap = #'frame { =Frame[n] => n }
        [frame: frame, unwrap: unwrap]
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("5 ~> %frames.frame ~ ~> %frames.unwrap ~")
        .expect("5");
}

#[test]
fn test_alias_and_binding_share_name_across_repl_lines() {
    // The REPL persists bindings between lines: a variable defined on a later line
    // must not clobber the persisted same-named type alias, or vice versa.
    quiver()
        .evaluate("'room = Str['bin]")
        .then_evaluate("room = 42")
        .then_evaluate("f = #'room { Ok }")
        .then_evaluate(r#"["hi" ~> f ~, room]"#)
        .expect("[Ok, 42]");
}

// --- hygienic alias instantiation: `^` as a type argument ------------------------------
// A recursive back-reference passed as a type argument must keep pointing at the
// caller's binder once spliced under the alias body's own binders — previously the two
// knots collapsed (`Cons[μ1, μ1]`), rejecting valid values.

#[test]
fn test_recursive_argument_to_module_alias() {
    quiver()
        .evaluate(
            r#"
            'tree = Leaf['int] | Node['%list<^>]
            f = #'tree { =Node[kids] => %list.count kids }
            Node[Cons[Leaf[1], Cons[Leaf[2], Nil]]] ~> f ~
            "#,
        )
        .expect("2");
}

#[test]
fn test_recursive_argument_to_local_alias() {
    quiver()
        .evaluate(
            r#"
            'mylist<'t> = Nil | Cons['t, ^]
            'tree = Leaf['int] | Node['mylist<^>]
            f = #'tree { =Node[kids] => %list.count kids }
            Node[Cons[Leaf[1], Cons[Leaf[2], Nil]]] ~> f ~
            "#,
        )
        .expect("2");
}

#[test]
fn test_recursive_argument_deep_fold() {
    // Both knots survive in depth: the element cycle reaches the tree, the tail cycle
    // stays the list, through multiple levels of nesting.
    quiver()
        .evaluate(
            r#"
            'tree = Leaf['int] | Node['%list<^>]
            sum = #[#^ -> 'int, 'tree, 'int] {
              | =[_, Leaf[n], acc] => __integer_add__ [n, acc]
              | =[self, Node[kids], acc] => %list.fold [kids, acc, #{ self [self, $1, $0] }]
            }
            sum [sum, Node[Cons[Leaf[1], Cons[Node[Cons[Leaf[2], Cons[Leaf[3], Nil]]], Nil]]], 0]
            "#,
        )
        .expect("6");
}

#[test]
fn test_recursive_argument_in_variant_position() {
    // An 'opt-style alias splices the argument as a union member; flattening strips a
    // binder, which the substitution compensates for. Both member shapes construct.
    quiver()
        .evaluate(
            r#"
            'opt<'t> = 't | []
            'tree = Leaf['int] | Node['opt<^>]
            [Node[Leaf[7]] ~> ='tree, Node[[]] ~> ='tree]
            "#,
        )
        .expect("[Ok, Ok]");
}

#[test]
fn test_recursive_argument_two_occurrences() {
    // The same parameter substituted at two different binder depths: each occurrence
    // shifts by its own depth (the tuple field by one binder, the list element by two).
    quiver()
        .evaluate(
            r#"
            'both<'t> = ['t, (Nil | Cons['t, ^])]
            'tree = A | B['both<^>]
            f = #'tree { =B[[first, list]] => %list.count Cons[first, list] }
            f B[[A, Nil]]
            "#,
        )
        .expect("1");
}

#[test]
fn test_nested_union_member_cycle_flattening() {
    // A parenthesised union member is flattened into its parent; a cycle inside it that
    // counted both binders is shortened to match. The value must still satisfy the type.
    quiver()
        .evaluate(
            r#"
            'x = A | (B[^] | C)
            f = #'x { | =B[inner] => inner | =A => A | =C => C }
            f B[A] ~> %data.encode ~
            "#,
        )
        .expect("\"A\"");
}

// --- scoped type aliases -------------------------------------------------------------
// A type alias is a step of any sequence, not just the program's, and is scoped to the
// sequence's enclosing scope.

#[test]
fn test_type_alias_in_function_body() {
    quiver()
        .evaluate(
            r#"
            main = #{
              'p = [x: 'int]
              g = #'p { $x }
              g [x: 5]
            }
            main []
            "#,
        )
        .expect("5");
}

#[test]
fn test_type_alias_in_block() {
    quiver()
        .evaluate(
            r#"
            main = #{
              {
                'p = [x: 'int]
                g = #'p { $x }
                g [x: 7]
              }
            }
            main []
            "#,
        )
        .expect("7");
}

#[test]
fn test_type_alias_scoped_to_its_block() {
    // The alias does not escape the block that declares it.
    quiver()
        .evaluate(
            r#"
            main = #{
              { 'p = [x: 'int]; 1 }
              g = #'p { $x }
              g [x: 5]
            }
            main []
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeAliasMissing(
            "p".to_string(),
        ));
}

#[test]
fn test_type_alias_shadows_outer() {
    // An inner alias shadows an outer one of the same name, and the outer is unaffected
    // after the block ends.
    quiver()
        .evaluate(
            r#"
            'p = 'int
            outer = #'p { $ }
            main = #{
              inner = { 'p = 'bin; f = #'p { $ }; f <0a> }
              [inner, 3 ~> outer ~]
            }
            main []
            "#,
        )
        .expect("[<0a>, 3]");
}

#[test]
fn test_type_alias_is_branch_local() {
    // Bindings are cleared between a block's branches, and an alias is a binding like any
    // other: one declared in an earlier branch is not visible in a later one.
    quiver()
        .evaluate(
            r#"
            main = #{
              { | [] => { 'p = 'int; 1 } | g = #'p { $ }; g 2 }
            }
            main []
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeAliasMissing(
            "p".to_string(),
        ));
}

#[test]
fn test_type_alias_over_function_type_parameter() {
    // A body-scoped alias may name a type in terms of the enclosing function's type
    // parameters — which a top-level alias cannot see.
    quiver()
        .evaluate(
            r#"
            h = #<'t>'t {
              'pair = ['t, 't]
              [$, $] ~> =('pair)p
              p
            }
            h 1
            "#,
        )
        .expect("[1, 1]");
}

#[test]
fn test_type_alias_step_is_transparent_to_the_flow() {
    // An alias step neither consumes nor produces a value: it never becomes the sequence's
    // result, and the chain after it is still the last one.
    quiver()
        .evaluate(
            r#"
            main = #{
              5 ~> __integer_add__ [~, 1]
              'p = 'int
              __integer_add__ [7, 2]
            }
            main []
            "#,
        )
        .expect("9");
    // Nor does it interrupt short-circuiting: a nil step before an alias still ends the
    // sequence.
    quiver()
        .evaluate(
            r#"
            main = #{
              []
              'p = 'int
              7
            }
            main []
            "#,
        )
        .expect("[]");
}

#[test]
fn test_type_alias_step_does_not_stop_nil_short_circuit() {
    quiver()
        .evaluate(
            r#"
            main = #{
              []
              'p = 'int
              7
            }
            main []
            "#,
        )
        .expect("[]");
}

#[test]
fn test_alias_only_block_is_nil() {
    // No step produces a value, so the block is nil — the answer a sequence gives when it
    // has nothing to evaluate.
    quiver()
        .evaluate(
            r#"
            main = #{ [{ 'p = 'int }, 5] }
            main []
            "#,
        )
        .expect("[[], 5]");
}

#[test]
fn test_type_alias_forward_reference_in_block_fails() {
    // Aliases are positional inside a block exactly as they are at the top level.
    quiver()
        .evaluate(
            r#"
            main = #{
              g = #'p { $ }
              'p = 'int
              g 2
            }
            main []
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeAliasMissing(
            "p".to_string(),
        ));
}

#[test]
fn test_nested_type_alias_is_module_private() {
    // Only top-level aliases form a module's type namespace, so an alias declared inside a
    // function body is not reachable as `'%mod.name`.
    let mut modules = std::collections::HashMap::new();
    modules.insert(
        vec!["helper".to_string()],
        r#"
        make = #'int {
          'private = Secret['int]
          Secret[$] ~> =('private)s
          s
        }
        [make: make]
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("f = #'%helper.private { Ok }; 1 ~> %helper.make ~ ~> f ~")
        .expect_error_containing("private");
}

#[test]
fn test_nested_type_alias_may_name_a_module_type() {
    // A module referenced only from a nested alias is still linked.
    quiver()
        .evaluate(
            r#"
            main = #{
              'ints = '%list<'int>
              %list.new [] ~> %list.prepend [~, 4] ~> =('ints)xs
              xs
            }
            main []
            "#,
        )
        .expect("Cons[4, Nil]");
}

#[test]
fn test_process_type_spells_grants_as_operations() {
    // A process type lists what its holder may do, each clause written as the
    // operation: a glued head (send, what application takes), `!'r` (await, what
    // selecting on the pid yields), `?'s` (sample, what `?` reads) — in that order,
    // with no parentheses required.
    quiver()
        .evaluate("'h = @'int !'bin ?'int")
        .expect_alias("h", "@'int !'bin ?'int");
}

#[test]
fn test_process_type_sigil_glues_to_bare_at() {
    quiver().evaluate("'a = @!'int").expect_alias("a", "@!'int");
    quiver().evaluate("'w = @?'int").expect_alias("w", "@?'int");
}

#[test]
fn test_process_type_clauses_bind_before_a_union() {
    quiver()
        .evaluate("'u = 'int | @'int ?'int")
        .expect_alias("u", "'int | (@'int ?'int)");
}

#[test]
fn test_function_output_trailing_clause_is_the_functions() {
    // In `#'a -> @ !'c` the clause is the function's receive; granting await on the
    // returned pid takes a parenthesized output, `#'a -> (@!'r)`.
    quiver()
        .evaluate("'f = #'int -> @ !'int")
        .expect_alias("f", "#'int -> @ !'int");
    quiver()
        .evaluate("'g = #'int -> (@!'int)")
        .expect_alias("g", "#'int -> (@!'int)");
}

#[test]
fn test_process_type_clause_stops_at_a_newline() {
    // A clause sigil needs horizontal whitespace, so the `!'int` on its own line is a
    // receive step, not an await clause reaching across the step boundary.
    quiver()
        .evaluate("f = #{\n  'p = @'int\n  !'int\n}\nq = @f []\n7 ~> q ~\n!q")
        .expect("7");
}

#[test]
fn test_process_type_arrow_form_is_gone() {
    // The pre-clause spelling `@'m -> 'r` no longer parses: sending a message does
    // not yield the result — awaiting does, and that grant is spelled `!'r`.
    quiver()
        .evaluate("'bad = @'int -> 'int")
        .expect_parse_failure();
}
