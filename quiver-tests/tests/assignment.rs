mod common;
use common::*;

#[test]
fn test_simple_assignment() {
    quiver()
        .evaluate("1 ~> =x, 2 ~> =y, [x, y] ~> __add__")
        .expect("3");

    quiver().evaluate("x = 42; x").expect("42");
}

#[test]
fn test_tuple_destructuring() {
    quiver()
        .evaluate("[1, 2] ~> =[a, b], [a, b] ~> __add__")
        .expect("3");

    quiver().evaluate("[x, y] = [1, 2]; y").expect("2");
}

#[test]
fn test_named_field_assignment() {
    quiver().evaluate("A[a: 1] ~> =A[a: a], a").expect("1");
    quiver().evaluate("A[a: 1] ~> =[a: a], a").expect("[]");
    quiver()
        .evaluate("A[a: 1] ~> =A[a: a, b: b], a")
        .expect("[]");
    quiver().evaluate("A[a: 1] ~> =A[x: a], a").expect("[]");
    quiver().evaluate("A[a: 1] ~> =A[a], a").expect("[]");
    quiver().evaluate("A[a: a] = A[a: 5]; a").expect("5");
}

#[test]
fn test_nested_field_assignment() {
    quiver()
        .evaluate("A[a: B[b: 2]] ~> =A[a: B[b: b]], b")
        .expect("2");
}

#[test]
fn test_partial_tuple_assignment() {
    quiver()
        .evaluate("[x: 1, y: 2, z: 3] ~> =(x, y), [x, y] ~> __add__")
        .expect("3");
}

#[test]
fn test_named_partial_pattern() {
    quiver()
        .evaluate("A[x: 1, y: 2, z: 3] ~> =A(x, z), [x, z]")
        .expect("[1, 3]");
    quiver().evaluate("A[x: 1, y: 2] ~> =B(x, y)").expect("[]");
}

#[test]
fn test_named_partial_pattern_with_union() {
    quiver()
        .evaluate(
            r#"
            union :: A[x: int, y: int] | B[x: int, z: int];
            #union { ~> =A(x) => x } ~> =f,
            A[x: 1, y: 2] ~> f ~> =a,
            B[x: 3, z: 4] ~> f ~> =b,
            [a, b]
            "#,
        )
        .expect("[1, []]");
}

#[test]
fn test_named_partial_pattern_in_block() {
    quiver()
        .evaluate(
            r#"
            A[x: 5, y: 10] ~> {
              | ~> =B(x, y) => 0
              | ~> =A(x, y) => [x, y] ~> __add__
            }
            "#,
        )
        .expect("15");
}

#[test]
fn test_star_assignment() {
    quiver()
        .evaluate("[a: 1, b: 2] ~> =*, [a, b] ~> __add__")
        .expect("3");

    quiver()
        .evaluate("* = [a: 1, b: 2]; [a, b] ~> __add__")
        .expect("3");
}

#[test]
fn test_ignore_placeholder() {
    quiver().evaluate("[1, 2] ~> =[a, _], a").expect("1");
    quiver().evaluate("[1, 2] ~> =_").expect("Ok");
    quiver().evaluate("_ = [1, 2]").expect("Ok");
}

#[test]
fn test_failed_assignment_length() {
    quiver().evaluate("[1, 2] ~> =[a]").expect("[]");
    quiver().evaluate("[a] = [1, 2]").expect("[]");
}

#[test]
fn test_failed_assignment_on_mismatched_literal() {
    quiver().evaluate("4 ~> =a, a ~> =3").expect("[]");
}

#[test]
fn test_failed_assignment_on_mismatched_literal_in_tuple() {
    quiver().evaluate("[1, 2] ~> =[a, 3]").expect("[]");
}

#[test]
fn test_failed_assignment_with_unrecognised_partial_field() {
    // TODO: specify error
    quiver().evaluate("[a: 1, b: 2] ~> =(c)").expect("[]");
}

#[test]
fn test_failed_assignment_type() {
    // TODO: specify error
    quiver().evaluate("B[1] ~> =A[a]").expect("[]");
}

#[test]
fn test_union_match_with_literals() {
    quiver()
        .evaluate(
            r#"
            2 ~> {
              | ~> =1 => 100
              | ~> =2 => 200
              | ~> =3 => 300
            }
            "#,
        )
        .expect("200");
}
#[test]
fn test_union_match_with_tuples() {
    quiver()
        .evaluate(
            r#"
            A[3] ~> {
              | ~> =A[x] => [x, 1] ~> __add__
              | ~> =B[x] => [x, 2] ~> __add__
            }
            "#,
        )
        .expect("4");
}

#[test]
fn test_union_type_partial_destructuring() {
    quiver()
        .evaluate(
            r#"
            #[a: int, b: int] { ~> =(a, b) => [a, b] } ~> =f,
            [a: 1, b: 2] ~> f
            "#,
        )
        .expect("[1, 2]");

    quiver()
        .evaluate(
            r#"
            union :: [a: int, b: int] | [x: int];
            #union { ~> =(a, b) => [a, b] } ~> =f,
            [a: 1, b: 2] ~> f ~> =b1,
            [x: 3] ~> f ~> =b2,
            [b1, b2]
            "#,
        )
        .expect("[[1, 2], []]");

    quiver()
        .evaluate(
            r#"
            union :: [a: int, b: int] | [b: int, c: int];
            #union { ~> =(b) => b } ~> =f,
            [a: 1, b: 2] ~> f ~> =b1,
            [b: 3, c: 4] ~> f ~> =b2,
            [b1, b2]
            "#,
        )
        .expect("[2, 3]");

    quiver()
        .evaluate(
            r#"
            union :: [a: int, b: int] | [b: int, c: int];
            #union { ~> =(a, b) => [a, b] } ~> =f,
            [a: 1, b: 2] ~> f ~> =b1,
            [b: 3, c: 4] ~> f ~> =b2,
            [b1, b2]
            "#,
        )
        .expect("[[1, 2], []]");
}

#[test]
fn test_match_union_in_nested_tuple() {
    quiver()
        .evaluate(
            r#"
            option :: Some[int] | None;
            #[option, int] {
              | ~> =[None, z] => 0
              | ~> =[Some[x], z] => [x, z] ~> __add__
            } ~> =f,
            [Some[5], 2] ~> f
            "#,
        )
        .expect("7");
}

#[test]
fn test_multiple_placeholders() {
    quiver()
        .evaluate("[1, 2, 3, 4, 5] ~> =[_, x, _, y, _], [x, y] ~> __add__")
        .expect("6");
}

#[test]
fn test_mixed_pattern_literal_and_binding() {
    quiver().evaluate("[1, 2, 3] ~> =[1, x, 3], x").expect("2");
    quiver().evaluate("[1, 2, 4] ~> =[1, x, 3]").expect("[]"); // Literal 4 doesn't match 3
}

#[test]
fn test_deeply_nested_tuple_pattern() {
    quiver()
        .evaluate("A[B[C[42]]] ~> =A[B[C[x]]], x")
        .expect("42");
}

#[test]
fn test_empty_tuple_pattern() {
    quiver().evaluate("[] ~> =[]").expect("Ok");
    quiver().evaluate("[1] ~> =[]").expect("[]");
}

#[test]
fn test_string_literal_pattern() {
    // String literal matching with integer extraction
    quiver()
        .evaluate(r#"["hello", 123] ~> =["hello", x], x"#)
        .expect("123");

    quiver()
        .evaluate(r#"["goodbye", 123] ~> =["hello", x]"#)
        .expect("[]"); // String literal doesn't match
}

#[test]
fn test_complex_union_pattern_matching() {
    quiver()
        .evaluate(
            r#"
            result :: Ok[int] | Err[int];
            option :: Some[result] | None;

            Some[Ok[42]] ~> {
              | ~> =None => 0
              | ~> =Some[Err[_]] => -1
              | ~> =Some[Ok[x]] => x
            }
            "#,
        )
        .expect("42");
}

#[test]
fn test_simple_partial_pattern() {
    // Test a simple partial pattern without unions first
    quiver().evaluate("[x: 1, y: 2] ~> =(x, y), x").expect("1");
}

#[test]
fn test_partial_pattern_order_for_union() {
    quiver()
        .evaluate(
            r#"
            union :: A[x: int, y: int] | B[y: int, x: int]
            #Wrapper[union] { ~> =Wrapper[(x, y)] => [x, y] } ~> =f
            Wrapper[B[y: 1, x: 2]] ~> f
            "#,
        )
        .expect("[2, 1]");
}

#[test]
fn test_star_pattern_order_for_union() {
    quiver()
        .evaluate(
            r#"
            union :: A[x: int, y: int] | B[y: int, x: int]
            #union { ~> =* => [x, y] } ~> =f
            B[y: 1, x: 2] ~> f
            "#,
        )
        .expect("[2, 1]");
}

#[test]
fn test_recursive_destructuring() {
    quiver()
        .evaluate("A[B[i: 1], C[j: 2, k: 3], D[l: 4, m: 5]] ~> =A[B[i: i], C(j), *], [i, j, l, m]")
        .expect("[1, 2, 4, 5]");
}

#[test]
fn test_comparison_with_literal() {
    quiver().evaluate("10 ~> =x, x ~> =10").expect("Ok");
    quiver().evaluate("10 ~> =x, x ~> =5").expect("[]");
}

#[test]
fn test_wildcard() {
    quiver().evaluate("42 ~> =_").expect("Ok");
}

#[test]
fn test_pin_simple() {
    quiver().evaluate("y = 2, 2 ~> ^y").expect("Ok");
    quiver().evaluate("y = 2, 3 ~> ^y").expect("[]");
}

#[test]
fn test_pin_in_tuple() {
    quiver()
        .evaluate("y = 2, Point[x, ^y] = Point[1, 2], x")
        .expect("1");
    quiver()
        .evaluate("y = 2, Point[x, ^y] = Point[1, 3]")
        .expect("[]");
}

#[test]
fn test_pin_with_term_syntax() {
    quiver()
        .evaluate("y = 2, Point[1, 2] ~> ^Point[=x, y], x")
        .expect("1");
    quiver()
        .evaluate("y = 2, Point[1, 3] ~> ^Point[=x, y]")
        .expect("[]");
}

#[test]
fn test_mixed_pin_and_bind() {
    quiver()
        .evaluate("y = 2, Point[1, 2] ~> ^Point[=x, y], x")
        .expect("1");
    quiver()
        .evaluate("y = 2, Point[x, ^y] = Point[1, 2], x")
        .expect("1");
}

#[test]
fn test_nested_pin_and_bind() {
    quiver()
        .evaluate("y = 2, A[1, B[2, C[3]]] ~> =A[x, ^B[y, =C[z]]], [x, z]")
        .expect("[1, 3]");
}

#[test]
fn test_pin_multiple_variables() {
    quiver()
        .evaluate("x = 1, y = 2, [1, 2] ~> ^[x, y]")
        .expect("Ok");
    quiver()
        .evaluate("x = 1, y = 2, [1, 3] ~> ^[x, y]")
        .expect("[]");
}

#[test]
fn test_repeated_identifier_bind() {
    // Repeated identifier in bind mode - checks equality
    quiver().evaluate("[1, 1] ~> =[x, x], x").expect("1");
    quiver().evaluate("[1, 2] ~> =[x, x]").expect("[]");
}

#[test]
fn test_repeated_identifier_pin() {
    // Repeated identifier in pin mode - checks equality
    quiver().evaluate("[5, 5] ~> ^[x, x]").expect("Ok");
    quiver().evaluate("[5, 6] ~> ^[x, x]").expect("[]");
}

#[test]
fn test_repeated_identifier_nested() {
    quiver()
        .evaluate("[Point[1, 2], 1] ~> =[Point[x, _], x], x")
        .expect("1");
    quiver()
        .evaluate("[Point[1, 2], 2] ~> =[Point[x, _], x]")
        .expect("[]");
}

#[test]
fn test_repeated_identifier_multiple_times() {
    quiver().evaluate("[1, 1, 1] ~> =[x, x, x], x").expect("1");
    quiver().evaluate("[1, 1, 2] ~> =[x, x, x]").expect("[]");
}

#[test]
fn test_pin_with_variable_and_repetition() {
    // When variable exists and identifier is repeated, check both Variable and FieldEquality
    quiver().evaluate("x = 5, [5, 5] ~> ^[x, x]").expect("Ok");
    quiver().evaluate("x = 5, [5, 6] ~> ^[x, x]").expect("[]"); // Fails field equality
    quiver().evaluate("x = 5, [4, 4] ~> ^[x, x]").expect("[]"); // Fails variable check
}

#[test]
fn test_pin_without_variable_single_occurrence() {
    // Pin with single occurrence and no variable should error
    quiver().evaluate("5 ~> ^x").expect_compile_error(
        quiver_compiler::compiler::Error::InternalError {
            message: "Pin variable 'x' not found in scope".to_string(),
        },
    );
}

#[test]
fn test_pin_mixed_repeated_and_single() {
    quiver()
        .evaluate("[1, 1, 2] ~> ^[x, x, y]")
        .expect_compile_error(quiver_compiler::compiler::Error::InternalError {
            message: "Pin variable 'y' not found in scope".to_string(),
        });
}
