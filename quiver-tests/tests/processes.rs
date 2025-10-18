mod common;
use common::quiver;

#[test]
fn test_self_reference() {
    quiver().evaluate(".").expect("@0");
}

#[test]
fn test_spawn_simple_function() {
    quiver().evaluate("f = #{ [] }, @f").expect("@1");
}

#[test]
fn test_send_to_process() {
    quiver()
        .evaluate(
            r#"
            f = #{ $int { ~> =x => x } },
            p = @f,
            42 ~> p
        "#,
        )
        .expect("Ok");
}

#[test]
fn test_process_with_receive_accepts_correct_type() {
    quiver()
        .evaluate(
            r#"
            f = #{ $int { ~> =x => x } },
            p = @f,
            42 ~> p
        "#,
        )
        .expect("Ok");
}

#[test]
fn test_process_with_receive_rejects_wrong_type() {
    quiver()
        .evaluate(
            r#"
            f = #{ $int { ~> =x => x } },
            p = @f,
            '00' ~> p
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "int".to_string(),
            found: "bin".to_string(),
        });
}

#[test]
fn test_process_without_receive_rejects_send() {
    quiver()
        .evaluate(
            r#"
            f = #{ [] },
            p = @f,
            42 ~> p
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "process with receive type".to_string(),
            found: "process without receive type (cannot send messages)".to_string(),
        });
}

#[test]
fn test_multiple_receives_same_type() {
    quiver()
        .evaluate(
            r#"
            f = #{
                $int { ~> =x => x },
                $int { ~> =y => y }
            },
            @f
        "#,
        )
        .expect("@1");
}

#[test]
fn test_multiple_receives_different_types_error() {
    quiver()
        .evaluate(
            r#"
            f = #{
                $int { ~> =x => x },
                $bin { ~> =y => y }
            }
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::ReceiveTypeMismatch {
            first: "Integer".to_string(),
            second: "Binary".to_string(),
        });
}

#[test]
fn test_call_function_with_matching_receive_type() {
    quiver()
        .evaluate(
            r#"
            helper = #{ $int { ~> =x => x } },
            f = #{ $int { ~> =y => y }, [] ~> helper },
            @f
        "#,
        )
        .expect("@1");
}

#[test]
fn test_call_function_with_mismatched_receive_type() {
    quiver()
        .evaluate(
            r#"
            helper = #{ $bin { ~> =x => x } },
            f = #{ $int { ~> =y => y }, [] ~> helper }
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function with receive type compatible with int".to_string(),
            found: "function with receive type bin".to_string(),
        });
}

#[test]
fn test_call_function_with_receive_from_non_receive_context() {
    quiver()
        .evaluate(
            r#"
            helper = #{ $int { ~> =x => x } },
            f = #{ [] ~> helper }
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function without receive type".to_string(),
            found: "function with receive type int".to_string(),
        });
}

#[test]
fn test_call_function_without_receive_from_any_context() {
    quiver()
        .evaluate(
            r#"
            helper = #{ 42 },
            f = #{ $int { ~> =y => y }, [] ~> helper },
            @f
        "#,
        )
        .expect("@1");
}

#[test]
fn test_process_spawns_process_and_receives_reply() {
    quiver()
        .evaluate(
            r#"
            child = #{ $(@int) { ~> =parent => 42 ~> parent } },
            parent = #{ c = @child, . ~> c, $int { ~> =result => result } },
            @parent
        "#,
        )
        .expect("@1");
}

#[test]
fn test_receive_skips_non_matching_messages() {
    quiver()
        .evaluate(
            r#"
            f = #{
                $int { ~> =100 => Ok },
                $int { ~> =_ => Ok }
            },
            p = @f,
            42 ~> p,
            100 ~> p
        "#,
        )
        .expect("Ok");
}
