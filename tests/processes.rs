mod common;
use common::quiver;

#[test]
fn test_self_reference() {
    quiver().evaluate(".").expect("@0");
}

#[test]
fn test_spawn_simple_function() {
    quiver().evaluate("#{ [] } ~> =f, @f").expect("@1");
}

#[test]
fn test_send_to_process() {
    quiver()
        .evaluate(
            r#"
            #{ $int { ~> =x => x } } ~> =f,
            @f ~> =p,
            42 ~> p$
        "#,
        )
        .expect("Ok");
}

#[test]
fn test_process_with_receive_accepts_correct_type() {
    quiver()
        .evaluate(
            r#"
            #{ $int { ~> =x => x } } ~> =f,
            @f ~> =p,
            42 ~> p$
        "#,
        )
        .expect("Ok");
}

#[test]
fn test_process_with_receive_rejects_wrong_type() {
    quiver()
        .evaluate(
            r#"
            #{ $int { ~> =x => x } } ~> =f,
            @f ~> =p,
            '00' ~> p$
        "#,
        )
        .expect_compile_error(quiver::compiler::Error::TypeMismatch {
            expected: "int".to_string(),
            found: "bin".to_string(),
        });
}

#[test]
fn test_process_without_receive_rejects_send() {
    quiver()
        .evaluate(
            r#"
            #{ [] } ~> =f,
            @f ~> =p,
            42 ~> p$
        "#,
        )
        .expect_compile_error(quiver::compiler::Error::TypeMismatch {
            expected: "process with receive type".to_string(),
            found: "process without receive type (cannot send messages)".to_string(),
        });
}

#[test]
fn test_multiple_receives_same_type() {
    quiver()
        .evaluate(
            r#"
            #{
                $int { ~> =x => x },
                $int { ~> =y => y }
            } ~> =f,
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
            #{
                $int { ~> =x => x },
                $bin { ~> =y => y }
            } ~> =f
        "#,
        )
        .expect_compile_error(quiver::compiler::Error::ReceiveTypeMismatch {
            first: "Integer".to_string(),
            second: "Binary".to_string(),
        });
}

#[test]
fn test_call_function_with_matching_receive_type() {
    quiver()
        .evaluate(
            r#"
            #{ $int { ~> =x => x } } ~> =helper,
            #{ $int { ~> =y => y }, helper! } ~> =f,
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
            #{ $bin { ~> =x => x } } ~> =helper,
            #{ $int { ~> =y => y }, helper! } ~> =f
        "#,
        )
        .expect_compile_error(quiver::compiler::Error::TypeMismatch {
            expected: "function with receive type compatible with int".to_string(),
            found: "function with receive type bin".to_string(),
        });
}

#[test]
fn test_call_function_with_receive_from_non_receive_context() {
    quiver()
        .evaluate(
            r#"
            #{ $int { ~> =x => x } } ~> =helper,
            #{ helper! } ~> =f
        "#,
        )
        .expect_compile_error(quiver::compiler::Error::TypeMismatch {
            expected: "function without receive type".to_string(),
            found: "function with receive type int".to_string(),
        });
}

#[test]
fn test_call_function_without_receive_from_any_context() {
    quiver()
        .evaluate(
            r#"
            #{ 42 } ~> =helper,
            #{ $int { ~> =y => y }, helper! } ~> =f,
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
            #{ $(@int) { ~> =parent => 42 ~> parent$ } } ~> =child,
            #{ @child ~> =c, . ~> c$, $int { ~> =result => result } } ~> =parent,
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
            #{
                $int { ~> =100 => Ok },
                $int { ~> =_ => Ok }
            } ~> =f,
            @f ~> =p,
            42 ~> p$,
            100 ~> p$
        "#,
        )
        .expect("Ok");
}
