mod common;
use common::quiver;

#[test]
fn test_self_reference() {
    quiver().evaluate("&.").expect("@0");
}

#[test]
fn test_spawn_simple_function() {
    quiver().evaluate("f = #{ [] }, @f").expect("@1");
}

#[test]
fn test_send_to_process() {
    quiver().evaluate("p = @#{ !#'int }, 42 ~> p").expect("@1");
}

#[test]
fn test_process_without_receive_rejects_send() {
    quiver()
        .evaluate("p = @#{ [] }, 42 ~> p")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "process with send type".to_string(),
            found: "process without send type (cannot send to it)".to_string(),
        });
}

#[test]
fn test_process_type_checking_send() {
    quiver()
        .evaluate(
            r#"
            p = @#{ !#'int },
            0x00 ~> p
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "'int".to_string(),
            found: "'bin".to_string(),
        });
}

#[test]
fn test_spawn_with_argument() {
    quiver().evaluate("p = 42 ~> @#'int { $ }, !p").expect("42");
}

#[test]
fn test_spawn_juxtaposed_argument() {
    // `@f x` supplies the spawned function's init argument by juxtaposition.
    quiver()
        .evaluate("f = #'int { $ }, p = @f 42, !p")
        .expect("42");
}

#[test]
fn test_spawn_juxtaposed_argument_equals_piped() {
    // `@f x` is equivalent to `x ~> @f`.
    quiver()
        .evaluate("f = #'int { $ }, p = 42 ~> @f, !p")
        .expect("42");
}

#[test]
fn test_spawn_juxtaposed_argument_flows_chained_value() {
    // The chained value flows into the juxtaposed argument (via `~`), like a call argument.
    quiver()
        .evaluate("f = #['int, 'int] { __integer_add__ }, p = 10 ~> @f [~, 5], !p")
        .expect("15");
}

#[test]
fn test_spawn_ripple_with_juxtaposed_argument() {
    // `@~ x`: the chained value is the function to spawn; `x` is its init argument.
    quiver()
        .evaluate("f = #'int { $ }, p = &f ~> @~ 42, !p")
        .expect("42");
}

#[test]
fn test_spawn_with_argument_type_mismatch() {
    quiver()
        .evaluate("0x00 ~> @#'int { $ }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "'int".to_string(),
            found: "'bin".to_string(),
        });
}

#[test]
fn test_spawn_without_argument_requires_nil_parameter() {
    // With implicit continuation, spawning @#int { $ } without an explicit argument
    // passes [] (nil) from the implicit continuation, which doesn't match int
    quiver().evaluate("@#'int { $ }").expect_compile_error(
        quiver_compiler::compiler::Error::TypeMismatch {
            expected: "'int".to_string(),
            found: "[]".to_string(),
        },
    );
}

#[test]
fn test_spawn_postfix_syntax() {
    quiver().evaluate("p = #{ 42 } ~> @, !p").expect("42");
}

#[test]
fn test_spawn_sugar_parameterless() {
    quiver().evaluate("p = @{ 42 }, !p").expect("42");
}

#[test]
fn test_spawn_sugar_primitive_type() {
    quiver().evaluate("p = 42 ~> @'int { $ }, !p").expect("42");
}

#[test]
fn test_spawn_sugar_type_alias() {
    quiver()
        .evaluate(
            r#"
            'my_type = 'int | 'bin;
            p = 42 ~> @'my_type { $ },
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_spawn_sugar_tuple_type() {
    quiver()
        .evaluate(
            r#"
            p = [42, 100] ~> @['int, 'int] { $ },
            !p
            "#,
        )
        .expect("[42, 100]");
}

#[test]
fn test_receive_simple() {
    quiver()
        .evaluate("p = @#{ !#'int }, 42 ~> p, !p")
        .expect("42");
}

#[test]
fn test_receive_waits_until_match() {
    quiver()
        .evaluate(
            r#"
            p = @#{ !#'int { =42 => Ok } },
            10 ~> p, 20 ~> p, 42 ~> p,
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_receive_filter_accepts_on_any_non_nil() {
    // A filter accepts a message on any non-nil result (matching Quiver's truthiness
    // convention), not only on `Ok`. The select still yields the received message, never the
    // filter's own result — here the filter returns 99 but the receive evaluates to 42.
    quiver()
        .evaluate(
            r#"
            p = @#{ !#'int { =x => 99 } },
            42 ~> p,
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_receive_filter_returns_original_message() {
    quiver()
        .evaluate(
            r#"
            p = @#{ ! [#'int { =x => Ok }] ~> =result => result },
            42 ~> p,
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_receive_filter_type_is_parameter_not_result() {
    // The type of a receive with a filter should be the parameter type (the message type),
    // not the filter's result type (Ok or [])
    quiver()
        .evaluate("#{ !'int { Ok } }")
        .expect_type("#[] -> 'int");
}

#[test]
fn test_receive_function_cannot_spawn() {
    quiver()
        .evaluate(
            r#"
            p = @#{ !#'int { @#{ 42 }, Ok } },
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
            p1 = @#{ !#'int },
            p2 = @#{ !#'int { 42 ~> p1, Ok } },
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
    quiver().evaluate("@#{ !#'int, !#'int }").expect("@1");
}

#[test]
fn test_multiple_receives_different_types_widens() {
    // Multiple receives with different types should widen to a union
    // The function receives int | bin, and the receives return those types
    // Since we receive int first, then bin, the result is bin (last value)
    quiver().evaluate("@#{ !#'int, !#'bin }").expect("@1");
}

#[test]
fn test_await_simple() {
    quiver().evaluate("!@#{ 42 }").expect("42");
}

#[test]
fn test_await_returns_process_result() {
    quiver().evaluate("!@#{ [1, 2] ~> __integer_add__ }").expect("3");
}

#[test]
fn test_await_with_captures() {
    quiver()
        .evaluate("x = 10, !@#{ [x, 32] ~> __integer_add__ }")
        .expect("42");
}

#[test]
fn test_await_process_type_checking() {
    quiver()
        .evaluate(
            r#"
            await_fn = #(@-> 'int) { =p => !p },
            f = #{ 42 },
            @f ~> await_fn
            "#,
        )
        .expect("42");
}

#[test]
fn test_self_reference_cannot_be_awaited() {
    quiver()
        .evaluate("42 ~> @#{ !#'int ~> =x => &. ~> =self_pid, !self_pid }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "process with receive type (awaitable/readable)".to_string(),
            found: "process without receive type (cannot select)".to_string(),
        });
}

#[test]
fn test_select_single_process() {
    quiver().evaluate("!@#{ 42 }").expect("42");
}

#[test]
fn test_select_multiple_processes_first_ready() {
    quiver()
        .evaluate(
            r#"
            fast = #{ 42 },
            slow = #{ !#'int },
            p1 = @fast,
            p2 = @slow,
            ! [p1, p2]
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
            ! [p1, p2]
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_single_receive() {
    quiver()
        .evaluate(
            r#"
            p = @#{ !#'int },
            42 ~> p, !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_multiple_receive_patterns() {
    quiver()
        .evaluate(
            r#"
            p = @#{ ! [#'int, #'bin] },
            42 ~> p, !p
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
                &. ~> =self_pid,
                42 ~> self_pid,
                0x00 ~> self_pid,
                ! [#'int, #'bin]
            },
            !@f
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
                &. ~> =self_pid,
                10 ~> self_pid,
                42 ~> self_pid,
                99 ~> self_pid,
                !#'int
            },
            !@f
            "#,
        )
        .expect("10");
}

#[test]
fn test_timeout_fires() {
    quiver()
        .evaluate(
            r#"
            slow = #{ !#'int },
            ! [@slow, 1000]
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
            ! [@fast, 1000]
            "#,
        )
        .expect("42");
}

#[test]
fn test_timeout_zero() {
    quiver()
        .evaluate(
            r#"
            slow = #{ !#'int },
            ! [@slow, 0]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_empty_select_returns_nil() {
    quiver().evaluate("! []").expect("[]");
}

#[test]
fn test_timeout_only() {
    quiver().evaluate("! [100]").expect("[]");
}

#[test]
fn test_multiple_timeouts_uses_minimum() {
    quiver()
        .evaluate("! [2000, 100, 500]")
        .expect("[]")
        .expect_duration(100, 500);
}

#[test]
fn test_mixed_process_and_receive() {
    quiver()
        .evaluate(
            r#"
            make_receiver = #{
                fast = @#{ 99 },
                !fast,
                ! [#'int, fast]
            },
            receiver = @make_receiver,
            42 ~> receiver, !receiver
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
                slow = @#{ !#'bin },
                ! [#'int, slow, 1000]
            },
            42 ~> receiver, !receiver
            "#,
        )
        .expect("42");
}

#[test]
fn test_mixed_all_three_types_process_wins() {
    quiver()
        .evaluate(
            r#"
            !@#{
                fast = @#{ 99 },
                ! [#'int, fast, 1000]
            }
            "#,
        )
        .expect("99");
}

#[test]
fn test_mixed_all_three_types_timeout_wins() {
    quiver()
        .evaluate(
            r#"
            !@#{
                slow = @#{ !#'bin },
                ! [#'int, slow, 500]
            }
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
            @fast ~> ! [~, 1000]
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_ripple_timeout_wins() {
    quiver()
        .evaluate(
            r#"
            slow = #{ !#'int },
            @slow ~> ! [~, 100]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_select_nested_chain_outer_used() {
    quiver()
        .evaluate(
            r#"
            fast = #{ 42 },
            @fast ~> ! [~, 3 ~> ~]
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_receive_with_ripple() {
    quiver()
        .evaluate("p1 = @#{ !#'int ~> [~] }, 0 ~> p1, !p1")
        .expect("[0]");
}

#[test]
fn test_receive_type_from_variable() {
    quiver()
        .evaluate(
            r#"
            receiver_func = #'int,
            p = @#{ ! [&receiver_func] ~> [~, 100] ~> __integer_add__ },
            42 ~> p, !p
            "#,
        )
        .expect("142");
}

#[test]
fn test_receive_type_from_module_member() {
    // A module member used as a receiver supplies only the message type; it is body-less, so
    // (like an identity function) it is NOT applied to the message — the message passes through
    // unchanged. The module member is used inline, with no intermediate binding.
    quiver()
        .evaluate("p = @#{ !%int.and }, [255, 240] ~> p, !p")
        .expect("[255, 240]");
}

#[test]
fn test_body_less_receiver_builtin_matches_identity_function() {
    // A builtin and the equivalent body-less (identity) function behave identically as
    // receivers: both name the message type and return the received message, neither applies.
    quiver()
        .evaluate("p = @#{ !%int.and }, [255, 240] ~> p, !p")
        .expect("[255, 240]");
    quiver()
        .evaluate("p = @#{ !#['int, 'int] }, [255, 240] ~> p, !p")
        .expect("[255, 240]");
}

#[test]
fn test_postfix_select_with_function() {
    // Postfix form with a single function source
    // This is equivalent to ! [receiver]
    quiver()
        .evaluate(
            r#"
            receiver = #'int,
            p = @#{ &. ~> =self_pid, 42 ~> self_pid, !receiver },
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_postfix_select_with_timeout() {
    quiver().evaluate("!@#{ ! [1] }").expect("[]");
}

#[test]
fn test_postfix_select_equivalence_timeout() {
    // Test that ! [1] works for timeout
    quiver()
        .evaluate(
            r#"
            p = @#{ ! [1] };
            !p
            "#,
        )
        .expect("[]");
}

#[test]
fn test_nested_select() {
    quiver()
        .evaluate(
            r#"
            inner = #{ ! [#'int, 500] },
            p = @inner,
            42 ~> p,
            ! [p, 1000]
            "#,
        )
        .expect("42");
}

#[test]
fn test_continuation_after_timeout() {
    quiver()
        .evaluate(
            r#"
            !@#{
                slow = @#{ !#'int },
                result = ! [slow, 100] ~> <>,
                [result, 42]
            }
            "#,
        )
        .expect("[Ok, 42]");
}

#[test]
fn test_process_spawns_and_receives_reply() {
    quiver()
        .evaluate(
            r#"
            child = #{ ! [#(@'int) { =parent => { 42 ~> parent, Ok } }] },
            parent = #{ c = @child, &. ~> c, !#'int },
            @parent
            "#,
        )
        .expect("@1");
}

#[test]
fn test_send_to_self() {
    quiver().evaluate("!@#{ 10 ~> ., !#'int }").expect("10");
}

#[test]
fn test_send_to_self_with_receive_type_check() {
    quiver()
        .evaluate("#{ 0x00 ~> ., !#'int }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "'int".to_string(),
            found: "'bin".to_string(),
        });
}

#[test]
fn test_receive_type_in_function_argument() {
    // Test that receive type is correctly inferred when used in function call arguments
    quiver()
        .evaluate(
            r#"
            p = 20 ~> @#'int { %int.div [~, !#'int] },
            2 ~> p, !p
            "#,
        )
        .expect("10");
}

#[test]
fn test_receive_type_in_tuple_argument() {
    // Test that receive type is correctly inferred when used in tuple constructor arguments
    quiver()
        .evaluate(
            r#"
            p = 10 ~> @#'int { [~, !#'int] },
            32 ~> p, !p
            "#,
        )
        .expect("[10, 32]");
}

#[test]
fn test_receive_type_in_builtin_argument() {
    // Test that receive type is correctly inferred when used in builtin call arguments
    quiver()
        .evaluate(
            r#"
            p = 10 ~> @#'int { __integer_add__ [~, !#'int] },
            32 ~> p, !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_receive_type_in_tail_call_argument() {
    // Test that receive type is correctly inferred when used in tail call arguments
    quiver()
        .evaluate(
            r#"
            f = #['int, 'int] { __integer_add__ },
            p = 10 ~> @#'int { ^f [~, !#'int] },
            32 ~> p, !p
            "#,
        )
        .expect("42");
}

// Tests for new syntactic sugar (without # prefix)

#[test]
fn test_sugar_bare_primitive_type() {
    // Test !int instead of !#int
    quiver()
        .evaluate("p = @#{ !'int }, 42 ~> p, !p")
        .expect("42");
}

#[test]
fn test_sugar_type_alias() {
    // Test !(type_alias) where type_alias is defined
    // Note: lowercase type aliases need explicit parentheses since parser can't
    // distinguish them from variables
    quiver()
        .evaluate(
            r#"
            'my_type = 'int;
            p = @#{ !('my_type) },
            42 ~> p, !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_sugar_union_type() {
    // Test !(int | bin) instead of !#(int | bin)
    quiver()
        .evaluate(
            r#"
            p = @#{ !('int | 'bin) },
            42 ~> p, !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_sugar_receive_function_with_identifier_type() {
    // Test !int { ... } instead of !#int { ... }
    quiver()
        .evaluate(
            r#"
            p = @#{ !'int { =42 => Ok } },
            10 ~> p, 20 ~> p, 42 ~> p,
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_sugar_receive_function_with_union_type() {
    // Test !(int | bin) { ... } instead of !#(int | bin) { ... }
    quiver()
        .evaluate(
            r#"
            p = @#{ !('int | 'bin) { =42 => Ok } },
            0x00 ~> p, 42 ~> p,
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_sugar_parenthesized_identifier() {
    // Test !(type_alias) resolves correctly
    quiver()
        .evaluate(
            r#"
            'receiver_type = 'int;
            p = @#{ !('receiver_type) },
            42 ~> p, !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_sugar_mixed_with_comma_separation() {
    // Test bracket-separated sources with new syntax
    quiver()
        .evaluate(
            r#"
            make_receiver = #{
                fast = @#{ 99 },
                !fast,
                ! [#'int, fast]
            },
            receiver = @make_receiver,
            42 ~> receiver, !receiver
            "#,
        )
        .expect("42");
}

#[test]
fn test_sugar_tuple_type() {
    // Test !#[int, int] for identity receive on tuple type
    // Note: ! [...] is now sources syntax, so we need explicit #
    quiver()
        .evaluate(
            r#"
            p = @#{ !#['int, 'int] },
            [42, 100] ~> p, !p
            "#,
        )
        .expect("[42, 100]");
}

#[test]
fn test_process_reference_after_completion() {
    // Test that @N syntax works even after the process has completed
    quiver()
        .evaluate("p = @{ 42 }")
        .expect("Ok")
        .then_evaluate("!@1")
        .expect("42");
}

// A nilary process function ignores an implicitly-flowing init value (like a nilary call), but an
// explicit juxtaposed argument is still type-checked.

#[test]
fn test_nilary_spawn_ignores_chained_value() {
    quiver().evaluate("p = 5 ~> @{ 99 }, !p").expect("99");
}

#[test]
fn test_nilary_spawn_rejects_juxtaposed_argument() {
    quiver().evaluate("p = @{ 99 } 5, !p").expect_compile_error(
        quiver_compiler::compiler::Error::TypeMismatch {
            expected: "[]".to_string(),
            found: "'int".to_string(),
        },
    );
}
