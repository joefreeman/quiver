mod common;
use common::*;

#[test]
fn test_simple_type_definition() {
    quiver()
        .evaluate("circle : Circle[r: int]")
        .expect_alias("circle", "Circle[r: int]");
}

#[test]
fn test_union_type_definition() {
    quiver()
        .evaluate(
            r#"
            shape :
              | Circle[r: int]
              | Rectangle[w: int, h: int]
            "#,
        )
        .expect_alias("shape", "Circle[r: int] | Rectangle[w: int, h: int]");
}

#[test]
fn test_function_with_type_pattern() {
    quiver()
        .evaluate(
            r#"
            shape :
              | Circle[r: int]
              | Rectangle[w: int, h: int];

            area = #shape {
              | ~> =Circle[r: r] => [r, r] ~> __multiply__
              | ~> =Rectangle[w: w, h: h] => [w, h] ~> __multiply__
            },

            a1 = Circle[r: 5] ~> area,
            a2 = Rectangle[w: 4, h: 3] ~> area,
            [a1, a2] ~> __add__
            "#,
        )
        .expect("37")
        .expect_variable(
            "area",
            "#(Circle[r: int] | Rectangle[w: int, h: int]) -> ([] | int)",
        );
}

#[test]
fn test_recursive_list_type() {
    quiver()
        .evaluate(
            r#"
            list : Nil | Cons[int, &];
            xs = Cons[1, Cons[2, Cons[3, Nil]]],
            [xs.1.0, xs.1.1.0] ~> __add__
            "#,
        )
        .expect("5")
        .expect_variable("xs", "Cons[int, Cons[int, Cons[int, Nil]]]");
}

#[test]
fn test_cycle_ref_with_pattern_matching() {
    quiver()
        .evaluate(
            r#"
            list : Nil | Cons[int, &];
            get_head = #list {
              | ~> =Cons[h, _] => h
              | ~> =Nil => 0
            },
            Cons[1, Cons[2, Cons[3, Nil]]] ~> get_head
            "#,
        )
        .expect("1")
        .expect_variable("get_head", "#(Cons[int, μ1] | Nil) -> ([] | int)");
}

#[test]
fn test_cycle_ref_nested_depth() {
    quiver()
        .evaluate(
            r#"
            json : True | False | Array[(Nil | Cons[&0, &1])];
            f = #json { ~> =Array[Cons[a, Cons[b, Nil]]] => [a, b] },
            Array[Cons[False, Cons[True, Nil]]] ~> f
            "#,
        )
        .expect("[False, True]");
}

#[test]
fn test_cycle_ref_error_no_union() {
    // Should fail: cycle without enclosing union
    let result = std::panic::catch_unwind(|| {
        quiver().evaluate("bad : Bad[&]").expect("should error");
    });
    assert!(result.is_err(), "Expected error for cycle without union");
}

#[test]
fn test_cycle_ref_error_no_base_case() {
    // Should fail: union without base case
    let result = std::panic::catch_unwind(|| {
        quiver()
            .evaluate("bad : A[&] | B[&]")
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
            .evaluate("bad : A[x: int, next: &] | B[y: int, next: &]")
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
            list : Nil | Cons[int, &];

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
            list : Nil | Cons[int, &];

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
            list : Nil | Cons[int, &];

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
            list : Nil | Cons[int, &];

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
            tree : Leaf[int] | Node[&, &];

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
            list : Nil | Cons[int, &];
            get_head = #list {
              | ~> =Cons[h, _] => h
              | ~> =Nil => 0
            },
            Cons[1, Cons[2, Cons[3, Nil]]] ~> get_head
            "#,
        )
        .expect("1");
}

#[test]
fn test_recursive_tree_type() {
    quiver()
        .evaluate(
            r#"
            tree : Node[left: &, right: &] | Leaf[int];
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
        .expect("3");
}

#[test]
fn test_recursive_type_with_cycle() {
    quiver()
        .evaluate(
            r#"
            list : Nil | Cons[int, &];
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
            t : Empty | Full[&];

            // This function matches on a tuple where the first element is a recursive type
            // The bug would occur when the pattern compiler tried to access field 0 of Empty
            // (which has no fields) when matching the pattern [Full[rest], n]
            match_recursive = #[t, int] {
              | ~> =[Empty, n] => n
              | ~> =[Full[rest], n] => [n, 100] ~> __add__
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
            tree : Leaf[int] | Node[&, &];

            // Function that matches on first element of tuple
            match_first = #[tree, int] {
              | ~> =[Leaf[x], n] => [x, n] ~> __add__
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
            list : Nil | Cons[int, &];

            // Pattern matching that would trigger the bug
            process_list = #[list, int] {
              | ~> =[Nil, x] => x
              | ~> =[Cons[head, tail], x] => [head, x] ~> __add__
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
            t : Empty | Full[&];
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
            t : Empty | Full[&];
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
            t : Empty | Full[&];
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
        .expect("[[1, 2], [3, 4], [6, 7], [8, 9]]")
        .expect_variable("f", "#(x: int, y: int) -> [int, int]");

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
        .expect("1")
        .expect_variable("f", "#Point(x: int) -> int");

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
        .expect("[[1, 2, 3], [x: 4, y: 5], Point[x: 6, y: 7], Point]")
        .expect_variable("f", "#() -> ()");
}

#[test]
fn test_nested_partial_type() {
    quiver()
        .evaluate(
            r#"
            container : (value: (x: int, y: int));
            f = #container { ~> =c => [c.value.x, c.value.y] },
            f[value: [x: 1, y: 2, z: 3], extra: 42]
            "#,
        )
        .expect("[1, 2]")
        .expect_alias("container", "(value: (x: int, y: int))");
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
            expected: "function parameter compatible with A(x: int) | B(x: int)".to_string(),
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
            expected: "function parameter compatible with A(x: int) | B(x: int)".to_string(),
            found: "B[y: int]".to_string(),
        });
}

#[test]
fn test_invalid_partial_type() {
    quiver()
        .evaluate("bad : (int, y: int)")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "All fields in a partial type must be named".to_string(),
        ));
}

#[test]
fn test_type_spread_basic() {
    quiver()
        .evaluate(
            r#"
            base : Base[x: int];
            extended : Extended[...base, y: int]
            "#,
        )
        .expect_alias("extended", "Extended[x: int, y: int]");
}

#[test]
fn test_type_spread_field_override() {
    quiver()
        .evaluate(
            r#"
            base : Base[x: int, y: int];
            modified : Modified[...base, y: bin]
            "#,
        )
        .expect_alias("modified", "Modified[x: int, y: bin]");
}

#[test]
fn test_type_spread_union_distribution() {
    quiver()
        .evaluate(
            r#"
            shape : Circle[r: int] | Square[s: int];
            colored : Colored[...shape, color: bin]
            "#,
        )
        .expect_alias(
            "colored",
            "Colored[r: int, color: bin] | Colored[s: int, color: bin]",
        );
}

#[test]
fn test_type_spread_unnamed() {
    quiver()
        .evaluate(
            r#"
            base : Base[x: int];
            extended : [...base, y: int]
            "#,
        )
        .expect_alias("extended", "[x: int, y: int]");
}

#[test]
fn test_type_spread_name_modes() {
    quiver()
        .evaluate(
            r#"
            base : Base[x: int];
            unnamed : [...base, y: int];
            renamed : Renamed[...base, y: int]
            "#,
        )
        .expect_alias("unnamed", "[x: int, y: int]")
        .expect_alias("renamed", "Renamed[x: int, y: int]");
}

#[test]
fn test_type_spread_multiple_fields() {
    quiver()
        .evaluate(
            r#"
            point2d : Point2D[x: int, y: int];
            point3d : Point3D[...point2d, z: int, color: bin]
            "#,
        )
        .expect_alias("point3d", "Point3D[x: int, y: int, z: int, color: bin]");
}

#[test]
fn test_type_spread_union_with_override() {
    quiver()
        .evaluate(
            r#"
            base : A[x: int, y: int] | B[x: int, z: int];
            modified : Modified[...base, y: bin]
            "#,
        )
        .expect_alias(
            "modified",
            "Modified[x: int, y: bin] | Modified[x: int, z: int, y: bin]",
        );
}

#[test]
fn test_type_spread_empty_base() {
    quiver()
        .evaluate(
            r#"
            empty : Empty[];
            extended : Extended[...empty, x: int]
            "#,
        )
        .expect_alias("extended", "Extended[x: int]");
}

#[test]
fn test_type_spread_identifier_preserves_name() {
    quiver()
        .evaluate(
            r#"
            base : Point[x: int];
            extended : base[...base, y: int]
            "#,
        )
        .expect_alias("extended", "Point[x: int, y: int]");
}

#[test]
fn test_type_spread_in_generic_definition() {
    quiver()
        .evaluate(
            r#"
            base : Base[x: int, y: int];
            extended<t> : Extended[...base, z: t]
            "#,
        )
        .expect_alias("extended", "Extended[x: int, y: int, z: t]");
}

#[test]
fn test_type_spread_with_generic_fields() {
    quiver()
        .evaluate(
            r#"
            base : Base[x: int];
            extended<t, u> : Extended[...base, y: t, z: u]
            "#,
        )
        .expect_alias("extended", "Extended[x: int, y: t, z: u]");
}

#[test]
fn test_type_spread_in_generic_union() {
    quiver()
        .evaluate(
            r#"
            shape : Circle[r: int] | Square[s: int];
            colored<t> : Colored[...shape, color: t]
            "#,
        )
        .expect_alias(
            "colored",
            "Colored[r: int, color: t] | Colored[s: int, color: t]",
        );
}

#[test]
fn test_type_spread_with_parameterized_type() {
    quiver()
        .evaluate(
            r#"
            point<t> : Point[x: t, y: t];
            point3d<t> : Point3D[...point<t>, z: t]
            "#,
        )
        .expect_alias("point3d", "Point3D[x: t, y: t, z: t]");
}

#[test]
fn test_type_spread_with_mixed_parameters() {
    quiver()
        .evaluate(
            r#"
            base<t> : Base[value: t];
            extended<t, u> : Extended[...base<t>, extra: u]
            "#,
        )
        .expect_alias("extended", "Extended[value: t, extra: u]");
}

#[test]
fn test_type_spread_parameterized_union() {
    quiver()
        .evaluate(
            r#"
            result<t, e> : Ok[value: t] | Err[error: e];
            tagged<t, e> : Tagged[...result<t, e>, tag: bin]
            "#,
        )
        .expect_alias(
            "tagged",
            "Tagged[error: e, tag: bin] | Tagged[value: t, tag: bin]",
        );
}

#[test]
fn test_expect_alias_simple() {
    quiver()
        .evaluate(
            r#"
            point : Point[x: int, y: int]
            "#,
        )
        .expect_alias("point", "Point[x: int, y: int]");
}

#[test]
fn test_expect_alias_with_parameters() {
    quiver()
        .evaluate(
            r#"
            point<t> : Point[x: t, y: t]
            "#,
        )
        .expect_alias("point", "Point[x: t, y: t]");
}

#[test]
fn test_expect_alias_union() {
    quiver()
        .evaluate(
            r#"
            shape : Circle[r: int] | Square[s: int]
            "#,
        )
        .expect_alias("shape", "Circle[r: int] | Square[s: int]");
}

#[test]
fn test_expect_alias_with_spread() {
    quiver()
        .evaluate(
            r#"
            base : Base[x: int];
            extended : Extended[...base, y: int]
            "#,
        )
        .expect_alias("extended", "Extended[x: int, y: int]");
}

#[test]
fn test_expect_alias_parameterized_with_spread() {
    quiver()
        .evaluate(
            r#"
            base<t> : Base[x: t];
            extended<t> : Extended[...base<t>, y: t]
            "#,
        )
        .expect_alias("extended", "Extended[x: t, y: t]");
}

#[test]
fn test_spread_partial_type_basic() {
    quiver()
        .evaluate(
            r#"
            entity : (id: int);
            user : User[...entity, name: bin]
            "#,
        )
        .expect_alias("user", "User[id: int, name: bin]");
}

#[test]
fn test_spread_multiple_partials() {
    quiver()
        .evaluate(
            r#"
            entity : (id: int);
            metadata : (updated_at: int, created_at: int);
            user : User[...entity, name: bin, ...metadata]
            "#,
        )
        .expect_alias(
            "user",
            "User[id: int, name: bin, updated_at: int, created_at: int]",
        );
}

#[test]
fn test_spread_partial_with_override() {
    quiver()
        .evaluate(
            r#"
            base : (x: int, y: int);
            extended : Extended[...base, y: bin, z: int]
            "#,
        )
        .expect_alias("extended", "Extended[x: int, y: bin, z: int]");
}

#[test]
fn test_spread_partial_and_tuple() {
    quiver()
        .evaluate(
            r#"
            partial : (x: int);
            tuple : Tuple[y: int];
            combined : Combined[...partial, ...tuple, z: int]
            "#,
        )
        .expect_alias("combined", "Combined[x: int, y: int, z: int]");
}

#[test]
fn test_identifier_spread_syntax_basic() {
    quiver()
        .evaluate(
            r#"
            base : Base[x: int];
            extended : base[..., y: int]
            "#,
        )
        .expect_alias("extended", "Base[x: int, y: int]");
}

#[test]
fn test_identifier_spread_syntax_union() {
    quiver()
        .evaluate(
            r#"
            event : Created[id: int] | Updated | Deleted;
            logged : event[..., timestamp: int]
            "#,
        )
        .expect_alias(
            "logged",
            "Created[id: int, timestamp: int] | Deleted[timestamp: int] | Updated[timestamp: int]",
        );
}

#[test]
fn test_identifier_spread_syntax_partial() {
    quiver()
        .evaluate(
            r#"
            entity : (id: int);
            timestamped : entity[..., created_at: int]
            "#,
        )
        .expect_alias("timestamped", "[id: int, created_at: int]");
}

#[test]
fn test_primitive_type_alias_int() {
    quiver().evaluate("int : Int[x: int]").expect_compile_error(
        quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot redefine primitive type 'int'".to_string(),
        ),
    );
}

#[test]
fn test_primitive_type_alias_bin() {
    quiver().evaluate("bin : Bin[x: int]").expect_compile_error(
        quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot redefine primitive type 'bin'".to_string(),
        ),
    );
}

#[test]
fn test_reserved_name_int_as_variable() {
    quiver().evaluate("#{ 42 ~> =int }").expect_compile_error(
        quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot use reserved primitive type 'int' as a variable name".to_string(),
        ),
    );
}

#[test]
fn test_reserved_name_bin_as_variable() {
    quiver().evaluate("#{ '0a' ~> =bin }").expect_compile_error(
        quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot use reserved primitive type 'bin' as a variable name".to_string(),
        ),
    );
}

#[test]
fn test_reserved_name_int_in_destructuring() {
    quiver()
        .evaluate("#{ [1, 2] ~> =[int, y] }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot use reserved primitive type 'int' as a variable name".to_string(),
        ));
}

#[test]
fn test_reserved_name_bin_in_destructuring() {
    quiver()
        .evaluate("#{ ['0a', '0b'] ~> =[bin, y] }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot use reserved primitive type 'bin' as a variable name".to_string(),
        ));
}

#[test]
fn test_reserved_name_int_in_nested_pattern() {
    quiver()
        .evaluate("#{ [1, [2, 3]] ~> =[x, [y, int]] }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot use reserved primitive type 'int' as a variable name".to_string(),
        ));
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
        .evaluate("data : Data[int: int, bin: int]; Data[int: 42, bin: 99]")
        .expect("Data[int: 42, bin: 99]");
}

#[test]
fn test_pin_with_type_alias_primitive() {
    quiver()
        .evaluate(
            r#"
            int_or_bin : int | bin;
            42 ~> ^int_or_bin
            "#,
        )
        .expect("42");

    quiver()
        .evaluate(
            r#"
            int_or_bin : int | bin;
            '0a' ~> ^int_or_bin
            "#,
        )
        .expect("'0a'");
}

#[test]
fn test_pin_with_type_alias_union() {
    quiver()
        .evaluate(
            r#"
            list : Nil | Cons[int, &];
            Nil ~> ^list
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            list : Nil | Cons[int, &];
            Cons[1, Cons[2, Nil]] ~> ^list
            "#,
        )
        .expect("Cons[1, Cons[2, Nil]]");
}

#[test]
fn test_pin_with_type_alias_mismatch() {
    quiver()
        .evaluate(
            r#"
            int_only : int;
            '0a' ~> ^int_only
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            point : Point[x: int, y: int];
            42 ~> ^point
            "#,
        )
        .expect("[]");
}

#[test]
fn test_pin_with_type_alias_in_pattern() {
    quiver()
        .evaluate(
            r#"
            shape : Circle[r: int] | Square[s: int];
            Circle[r: 5] ~> =^shape
            "#,
        )
        .expect("Circle[r: 5]");
}

#[test]
fn test_pin_with_type_alias_nested() {
    quiver()
        .evaluate(
            r#"
            inner : int | bin;
            Wrapper[value: 42] ~> =Wrapper[value: ^inner]
            "#,
        )
        .expect("Wrapper[value: 42]");
}

#[test]
fn test_pin_with_inline_type_primitive() {
    quiver()
        .evaluate(
            r#"
            42 ~> ^(int | bin)
            "#,
        )
        .expect("42");

    quiver()
        .evaluate(
            r#"
            '0a' ~> ^(int | bin)
            "#,
        )
        .expect("'0a'");
}

#[test]
fn test_pin_with_inline_type_mismatch() {
    quiver()
        .evaluate(
            r#"
            A ~> ^(int | bin)
            "#,
        )
        .expect("[]");
}

#[test]
fn test_pin_with_inline_type_nested() {
    quiver()
        .evaluate(
            r#"
            A[value: 42] ~> =A[value: ^(int | bin)]
            "#,
        )
        .expect("A[value: 42]");
}

#[test]
fn test_pin_with_inline_type_complex() {
    quiver()
        .evaluate(
            r#"
            Rectangle[w: 5, h: 10] ~> ^(Rectangle[w: int, h: int] | Circle[r: int])
            "#,
        )
        .expect("Rectangle[w: 5, h: 10]");
}

#[test]
fn test_inline_type_in_bind_mode_error() {
    quiver()
        .evaluate(
            r#"
            42 ~> =(int | bin)
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::FeatureUnsupported(
            "Type expressions can only be used in pin mode (^), not bind mode (=)".to_string(),
        ));
}

#[test]
fn test_generic_type_explicit_instantiation() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            Cons[42, Nil] ~> ^(list<int>)
            "#,
        )
        .expect("Cons[42, Nil]");
}

#[test]
fn test_generic_type_explicit_instantiation_mismatch() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            Cons['aa', Nil] ~> ^(list<int>)
            "#,
        )
        .expect("[]");
}

#[test]
fn test_generic_type_explicit_instantiation_in_pattern() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            Cons[42, Cons[99, Nil]] ~> =Cons[x, ^(list<int>)]
            "#,
        )
        .expect("Cons[42, Cons[99, Nil]]");
}

#[test]
fn test_generic_type_without_instantiation_error() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            Cons[42, Nil] ~> ^list
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::FeatureUnsupported(
            "Parameterized type alias 'list' requires explicit type arguments. Use ^(list<...>) with type arguments specified".to_string(),
        ));
}

#[test]
fn test_generic_type_short_syntax() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            Cons[42, Nil] ~> ^list<int>
            "#,
        )
        .expect("Cons[42, Nil]");
}

#[test]
fn test_generic_type_short_syntax_mismatch() {
    quiver()
        .evaluate(
            r#"
            list<t> : Nil | Cons[t, &];
            Cons['aa', Nil] ~> ^list<int>
            "#,
        )
        .expect("[]");
}

#[test]
fn test_named_partial_type_without_parens() {
    // Named partial type without extra parentheses: ^A(x: int)
    quiver()
        .evaluate("A[x: 1, y: 2] ~> ^A(x: int)")
        .expect("A[x: 1, y: 2]");

    // Type mismatch should fail
    quiver()
        .evaluate("A[x: 'ff', y: 2] ~> ^A(x: int)")
        .expect("[]");

    // Missing field should fail
    quiver()
        .evaluate("A[y: 2, z: 3] ~> ^A(x: int)")
        .expect("[]");
}

#[test]
fn test_unnamed_partial_type_without_parens() {
    // Unnamed partial type without extra parentheses: ^(x: int)
    quiver().evaluate("A[x: 1] ~> ^(x: int)").expect("A[x: 1]");

    quiver()
        .evaluate("[x: 1, y: 2] ~> ^(x: int)")
        .expect("[x: 1, y: 2]");

    // Type mismatch should fail
    quiver().evaluate("A[x: 'ff'] ~> ^(x: int)").expect("[]");
}

#[test]
fn test_empty_partial_type_without_parens() {
    // Empty partial type matches any tuple: ^()
    quiver().evaluate("A[1] ~> ^()").expect("A[1]");
    quiver().evaluate("[1, 2, 3] ~> ^()").expect("[1, 2, 3]");
    quiver().evaluate("42 ~> ^()").expect("[]");
}
