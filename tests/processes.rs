mod common;
use common::quiver;

#[test]
fn test_self_reference() {
    quiver().evaluate(".").expect("@0");
}

#[test]
fn test_spawn_simple_function() {
    quiver().evaluate("#{ [] } ~> f, @f").expect("@1");
}

#[test]
fn test_send_to_process() {
    quiver()
        .evaluate(
            r#"
            #{ $int { ~> x => x } } ~> f,
            @f ~> p,
            42 ~> p$
        "#,
        )
        .expect("[]");
}

#[test]
fn test_process_with_receive_accepts_correct_type() {
    quiver()
        .evaluate(
            r#"
            #{ $int { ~> x => x } } ~> f,
            @f ~> p,
            42 ~> p$
        "#,
        )
        .expect("[]");
}

#[test]
fn test_process_with_receive_rejects_wrong_type() {
    quiver()
        .evaluate(
            r#"
            #{ $int { ~> x => x } } ~> f,
            @f ~> p,
            '00' ~> p$
        "#,
        )
        .expect_compile_error(quiver::compiler::Error::TypeMismatch {
            expected: "Integer".to_string(),
            found: "Binary".to_string(),
        });
}

#[test]
fn test_process_without_receive_rejects_send() {
    quiver()
        .evaluate(
            r#"
            #{ [] } ~> f,
            @f ~> p,
            42 ~> p$
        "#,
        )
        .expect_compile_error(quiver::compiler::Error::TypeMismatch {
            expected: "Union([])".to_string(),
            found: "Integer".to_string(),
        });
}

#[test]
fn test_multiple_receives_same_type() {
    quiver()
        .evaluate(
            r#"
            #{
                $int { ~> x => x },
                $int { ~> y => y }
            } ~> f,
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
                $int { ~> x => x },
                $bin { ~> y => y }
            } ~> f
        "#,
        )
        .expect_compile_error(quiver::compiler::Error::ReceiveTypeMismatch {
            first: "Integer".to_string(),
            second: "Binary".to_string(),
        });
}
