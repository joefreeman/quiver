mod common;
use common::quiver;

#[test]
fn test_await_simple_process() {
    quiver().evaluate("#{ 42 } ~> f, @f ~> p, p!").expect("42");
}

#[test]
fn test_await_returns_process_result() {
    quiver()
        .evaluate("#{ [1, 2] ~> <add>! } ~> f, @f ~> p, p!")
        .expect("3");
}

#[test]
fn test_explicit_process_type_return_only() {
    // Function that takes a process with return type and awaits it
    quiver()
        .evaluate(
            r#"
            #(@-> int) { ~> p => p! } ~> await_fn,
            #{ 42 } ~> f,
            @f ~> await_fn!
            "#,
        )
        .expect("42");
}

#[test]
fn test_explicit_process_type_receive_and_return() {
    // Function that takes a process with both receive and return types
    quiver()
        .evaluate(
            r#"
            #(@int -> bin) { ~> p => 42 ~> p$, p! } ~> send_and_await,
            #{ $int { ~> x => '00' } } ~> f,
            @f ~> send_and_await!
            "#,
        )
        .expect("'00'");
}

#[test]
fn test_self_reference_cannot_be_awaited() {
    // Self-reference returns a process without return type, so it can't be awaited
    quiver()
        .evaluate(
            r#"
            #{ $int { ~> x => . ~> self_pid, self_pid! } } ~> f,
            @f ~> p,
            42 ~> p$
            "#,
        )
        .expect_compile_error(quiver::compiler::Error::TypeMismatch {
            expected: "process with return type (awaitable)".to_string(),
            found: "process without return type (cannot await)".to_string(),
        });
}
