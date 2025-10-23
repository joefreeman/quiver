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
            f = #{ !(#int) },
            p = @f,
            42 ~> p
        "#,
        )
        .expect("Ok");
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
fn test_process_type_checking_send() {
    quiver()
        .evaluate(
            r#"
            f = #{ !(#int) },
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
fn test_spawn_with_argument() {
    quiver()
        .evaluate(
            r#"
            f = #int { ~> },
            p = 42 ~> @f,
            p ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_spawn_with_argument_type_mismatch() {
    quiver()
        .evaluate(
            r#"
            f = #int { ~> },
            p = '00' ~> @f
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "value compatible with int".to_string(),
            found: "bin".to_string(),
        });
}

#[test]
fn test_spawn_without_argument_requires_nil_parameter() {
    quiver()
        .evaluate(
            r#"
            f = #int { ~> },
            @f
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function with nil parameter (use 'value ~> @function' to pass an argument)"
                .to_string(),
            found: "function with parameter int".to_string(),
        });
}

#[test]
fn test_receive_simple() {
    quiver()
        .evaluate(
            r#"
            f = #{ !(#int) },
            p = @f,
            42 ~> p,
            p ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_receive_waits_until_match() {
    quiver()
        .evaluate(
            r#"
            f = #{ !#int { ~> =42 => Ok } },
            p = @f,
            10 ~> p,
            20 ~> p,
            42 ~> p,
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_receive_function_must_return_ok_or_nil() {
    quiver()
        .evaluate(
            r#"
            f = #{ !#int { ~> =x => 99 } },
            p = @f,
            42 ~> p,
            !p
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "receive function with body must return [] or Ok".to_string(),
            found: "int".to_string(),
        });
}

#[test]
fn test_receive_filter_returns_original_message() {
    quiver()
        .evaluate(
            r#"
            f = #{ !(#int { ~> =x => Ok }) ~> =result => result },
            p = @f,
            42 ~> p,
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_receive_function_cannot_spawn() {
    quiver()
        .evaluate(
            r#"
            p = @#{ !#int { @#{ 42 }, Ok } },
            10 ~> p,
            !p
            "#,
        )
        .expect_runtime_error(quiver_core::error::Error::OperationNotAllowed {
            operation: "spawn".to_string(),
            context: "receive function".to_string(),
        });
}

#[test]
fn test_receive_function_cannot_send() {
    quiver()
        .evaluate(
            r#"
            p1 = @#{ !#int },
            p2 = @#{ !#int { 42 ~> p1, Ok } },
            10 ~> p2,
            !p2
            "#,
        )
        .expect_runtime_error(quiver_core::error::Error::OperationNotAllowed {
            operation: "send".to_string(),
            context: "receive function".to_string(),
        });
}

#[test]
fn test_multiple_receives_same_type() {
    quiver()
        .evaluate(
            r#"
            f = #{
                !(#int),
                !(#int)
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
                !(#int),
                !(#bin)
            }
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::ReceiveTypeMismatch {
            first: "Integer".to_string(),
            second: "Binary".to_string(),
        });
}

#[test]
fn test_await_simple() {
    quiver()
        .evaluate("f = #{ 42 }, p = @f, p ~> !")
        .expect("42");
}

#[test]
fn test_await_returns_process_result() {
    quiver()
        .evaluate("f = #{ [1, 2] ~> __add__ }, p = @f, p ~> !")
        .expect("3");
}

#[test]
fn test_await_with_captures() {
    quiver()
        .evaluate(
            r#"
            x = 10,
            f = #{ [x, 32] ~> __add__ },
            p = @f,
            p ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_await_process_type_checking() {
    quiver()
        .evaluate(
            r#"
            await_fn = #(@-> int) { ~> =p => p ~> ! },
            f = #{ 42 },
            @f ~> await_fn
            "#,
        )
        .expect("42");
}

#[test]
fn test_self_reference_cannot_be_awaited() {
    quiver()
        .evaluate(
            r#"
            f = #{ !(#int) ~> =x => . ~> =self_pid, self_pid ~> ! },
            p = @f,
            42 ~> p
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "process with return type (awaitable)".to_string(),
            found: "process without return type (cannot await)".to_string(),
        });
}

#[test]
fn test_select_single_process() {
    quiver()
        .evaluate(
            r#"
            f = #{ 42 },
            p = @f,
            !(p)
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_multiple_processes_first_ready() {
    quiver()
        .evaluate(
            r#"
            fast = #{ 42 },
            slow = #{ !(#int) },
            p1 = @fast,
            p2 = @slow,
            !(p1 | p2)
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_priority_left_to_right() {
    quiver()
        .evaluate(
            r#"
            f1 = #{ 42 },
            f2 = #{ 100 },
            p1 = @f1,
            p2 = @f2,
            !p1, !p2,
            !(p1 | p2)
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_single_receive() {
    quiver()
        .evaluate(
            r#"
            f = #{ !(#int) },
            p = @f,
            42 ~> p,
            p ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_multiple_receive_patterns() {
    quiver()
        .evaluate(
            r#"
            f = #{ !(#int | #bin) },
            p = @f,
            42 ~> p,
            p ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_receive_pattern_priority() {
    quiver()
        .evaluate(
            r#"
            f = #{
                . ~> =self_pid,
                42 ~> self_pid,
                '00' ~> self_pid,
                !(#int | #bin)
            },
            @f ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_receive_waits_for_match() {
    quiver()
        .evaluate(
            r#"
            f = #{
                . ~> =self_pid,
                10 ~> self_pid,
                42 ~> self_pid,
                99 ~> self_pid,
                !(#int)
            },
            @f ~> !
            "#,
        )
        .expect("10");
}

#[test]
fn test_timeout_fires() {
    quiver()
        .evaluate(
            r#"
            slow = #{ !(#int) },
            p = @slow,
            !(p | 1000)
            "#,
        )
        .expect("[]");
}

#[test]
fn test_timeout_process_completes_first() {
    quiver()
        .evaluate(
            r#"
            fast = #{ 42 },
            p = @fast,
            !(p | 1000)
            "#,
        )
        .expect("42");
}

#[test]
fn test_timeout_zero() {
    quiver()
        .evaluate(
            r#"
            slow = #{ !(#int) },
            p = @slow,
            !(p | 0)
            "#,
        )
        .expect("[]");
}

#[test]
fn test_timeout_only() {
    quiver().evaluate("!(100)").expect("[]");
}

#[test]
fn test_multiple_timeouts_uses_minimum() {
    quiver()
        .evaluate("!(2000 | 100 | 500)")
        .expect("[]")
        .expect_duration(100, 500);
}

#[test]
fn test_mixed_process_and_receive() {
    quiver()
        .evaluate(
            r#"
            make_receiver = #{
                !(#int | @#{ 99 })
            },
            receiver = @make_receiver,
            42 ~> receiver,
            receiver ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_mixed_all_three_types_receive_wins() {
    quiver()
        .evaluate(
            r#"
            receiver = @#{
                slow = @#{ !(#bin) },
                !(#int | slow | 1000)
            },
            42 ~> receiver,
            receiver ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_mixed_all_three_types_process_wins() {
    quiver()
        .evaluate(
            r#"
            f = #{
                fast = @#{ 99 },
                !(#int | fast | 1000)
            },
            @f ~> !
            "#,
        )
        .expect("99");
}

#[test]
fn test_mixed_all_three_types_timeout_wins() {
    quiver()
        .evaluate(
            r#"
            f = #{
                slow = @#{ !(#bin) },
                !(#int | slow | 500)
            },
            @f ~> !
            "#,
        )
        .expect("[]");
}

#[test]
fn test_select_with_ripple() {
    quiver()
        .evaluate(
            r#"
            fast = #{ 42 },
            p = @fast,
            p ~> !(~ | 1000)
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_ripple_timeout_wins() {
    quiver()
        .evaluate(
            r#"
            slow = #{ !(#int) },
            p = @slow,
            p ~> !(~ | 100)
            "#,
        )
        .expect("[]");
}

#[test]
fn test_select_unused_chained_value_error() {
    quiver()
        .evaluate(
            r#"
            fast = #{ 42 },
            p = @fast,
            p ~> !(1000)
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::FeatureUnsupported(
            "Chained value must be used in select sources (use ripple operator ~)".to_string(),
        ));
}

#[test]
fn test_select_nested_chain_outer_used() {
    quiver()
        .evaluate(
            r#"
            fast = #{ 42 },
            p = @fast,
            p ~> !(~ | 3 ~> ~)
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_receive_with_ripple() {
    quiver()
        .evaluate("p1 = @#{ !#int ~> [~] }, 0 ~> p1, !p1")
        .expect("[0]");
}

#[test]
fn test_receive_type_from_variable() {
    quiver()
        .evaluate(
            r#"
            receiver_func = #int,
            p = @#{ !(receiver_func) ~> [~, 100] ~> __add__ },
            42 ~> p,
            p ~> !
            "#,
        )
        .expect("142");
}

#[test]
fn test_receive_type_from_module_builtin() {
    quiver()
        .evaluate(
            r#"
            math = %"math",
            p1 = @#{ !math.add },
            [10, 32] ~> p1,
            p1 ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_postfix_select_with_function() {
    quiver()
        .evaluate(
            r#"
            receiver = #int,
            p = @#{
              receiver ~> !
            },
            42 ~> p,
            p ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_postfix_select_with_timeout() {
    quiver()
        .evaluate(
            r#"
            p = @#{
              1 ~> !
            },
            p ~> !
            "#,
        )
        .expect("[]");
}

#[test]
fn test_postfix_select_equivalence_timeout() {
    quiver()
        .evaluate(
            r#"
            p1 = @#{ 1 ~> ! },
            p2 = @#{ !(1) },
            result1 = p1 ~> !,
            result2 = p2 ~> !,
            [result1, result2]
            "#,
        )
        .expect("[[], []]");
}

#[test]
fn test_nested_select() {
    quiver()
        .evaluate(
            r#"
            inner = #{
                !(#int | 500)
            },
            p = @inner,
            42 ~> p,
            !(p | 1000)
            "#,
        )
        .expect("42");
}

#[test]
fn test_continuation_after_timeout() {
    quiver()
        .evaluate(
            r#"
            f = #{
                slow = @#{ !(#int) },
                result = !(slow | 100),
                [result, 42]
            },
            @f ~> !
            "#,
        )
        .expect("[[], 42]");
}

#[test]
fn test_process_spawns_and_receives_reply() {
    quiver()
        .evaluate(
            r#"
            child = #{ !(#(@int) { ~> =parent => 42 ~> parent, Ok }) },
            parent = #{ c = @child, . ~> c, !(#int) },
            @parent
            "#,
        )
        .expect("@1");
}
