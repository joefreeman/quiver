use crate::common::quiver;

#[test]
fn test_self_reference() {
    quiver().evaluate("@").expect("@0");
}

#[test]
fn test_spawn_simple_function() {
    quiver().evaluate("f = #[] { [] }; @f []").expect("@1");
}

#[test]
fn test_send_to_process() {
    quiver()
        .evaluate("p = @#[] { !#'int } []; %proc.send [p, 42]")
        .expect("Ok");
}

#[test]
fn test_process_without_receive_rejects_send() {
    quiver()
        .evaluate("p = @#[] { [] } []; %proc.send [p, 42]")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "in `1`: 'int does not fit never".to_string(),
        ));
}

#[test]
fn test_process_type_checking_send() {
    quiver()
        .evaluate(
            r#"
            p = @#[] { !#'int } [];
            %proc.send [p, <00>]
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "in `1`: 'bin does not fit 'int".to_string(),
        ));
}

#[test]
fn test_send_through_all_process_union() {
    // Two process types that differ (here in result type) widen to a union rather than
    // folding by subsumption. A pipe into the union must still compile as a send — it
    // used to fall through to the non-applicable path and silently replace, discarding
    // the message.
    quiver()
        .evaluate(
            r#"
            f = #'int { !'int };
            g = #'int { !'int ~> [~] };
            a = 0 ~> @f ~;
            p = Ok ~> { | =Ok => a | 0 ~> @g ~ };
            %proc.send [p, 42];
            !a
        "#,
        )
        .expect("42");
}

#[test]
fn test_union_send_checks_every_member() {
    // The message must fit every member's send type: whichever member the handle
    // turns out to be at runtime has to accept it.
    quiver()
        .evaluate(
            r#"
            f = #'int { !'int };
            h = #'int { !Str['bin]; $ };
            p = Ok ~> { | =Ok => 0 ~> @f ~ | 0 ~> @h ~ };
            %proc.send [p, 42]
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "in `0`: Str['bin] does not fit 'int".to_string(),
        ));
}

#[test]
fn test_send_to_mixed_union_is_error() {
    // A union mixing a process with plain values is not a send target: the value may
    // turn out to be the plain member, which accepts nothing.
    quiver()
        .evaluate(
            r#"
            f = #'int { !'int };
            p = Ok ~> { | =Ok => 0 ~> @f ~ | 5 };
            %proc.send [p, 42]
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "in `0`: 'int is not @'m".to_string(),
        ));
}

#[test]
fn test_pipe_into_function_union_is_error() {
    // A union of function types cannot be applied (the members are separate
    // functions); rejected rather than silently replacing.
    quiver()
        .evaluate(
            r#"
            f = #'int { $ };
            g = #'int { "hi" };
            u = Ok ~> { | =Ok => f | g };
            42 ~> u ~
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::UnionApplication {
            union: "(#'int -> 'int) | (#'int -> Str['bin])".to_string(),
            all_functions: true,
        });
}

#[test]
fn test_spawn_with_argument() {
    quiver()
        .evaluate("p = 42 ~> @#'int { $ } ~; !p")
        .expect("42");
}

#[test]
fn test_spawn_juxtaposed_argument() {
    // `x @f` supplies the spawned function's init argument from the chained value.
    quiver()
        .evaluate("f = #'int { $ }; p = 42 ~> @f ~; !p")
        .expect("42");
}

#[test]
fn test_spawn_juxtaposed_argument_equals_piped() {
    // `.f x` is equivalent to `x @f`.
    quiver()
        .evaluate("f = #'int { $ }; p = 42 ~> @f ~; !p")
        .expect("42");
}

#[test]
fn test_spawn_juxtaposed_argument_flows_chained_value() {
    // The chained value flows into the argument tuple (via `~`), like a call argument.
    quiver()
        .evaluate("f = #['int, 'int] { __integer_add__ ~ }; p = 10 ~> [~, 5] ~> @f ~; !p")
        .expect("15");
}

#[test]
fn spawn_with_init_argument_is_argument_first() {
    // There is no form that spawns the flowing function *with* an init argument; spawn a named
    // function argument-first instead (`x @f`). Bare `.~` survives only for a nilary target.
    quiver()
        .evaluate("f = #'int { $ }; p = 42 ~> @f ~; !p")
        .expect("42");
}

#[test]
fn test_spawn_with_argument_type_mismatch() {
    quiver()
        .evaluate("<00> ~> @#'int { $ } ~")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "'int".to_string(),
            found: "'bin".to_string(),
        });
}

#[test]
fn test_spawn_without_argument_is_rejected() {
    // A spawn's init is written, like a call's argument — including when it is nil.
    quiver().evaluate("@#'int { $ }").expect_compile_error(
        quiver_compiler::compiler::Error::MissingCallArgument {
            form: "@f".to_string(),
        },
    );
    quiver().evaluate("p = @#'int { $ } 5; !p").expect("5");
}

#[test]
fn test_spawn_postfix_syntax() {
    quiver()
        .evaluate("p = #[] { 42 } ~> @~ []; !p")
        .expect("42");
}

#[test]
fn test_spawn_sugar_parameterless() {
    quiver().evaluate("p = @[] { 42 } []; !p").expect("42");
}

#[test]
fn test_spawn_sugar_primitive_type() {
    quiver()
        .evaluate("p = 42 ~> @'int { $ } ~; !p")
        .expect("42");
}

#[test]
fn test_spawn_sugar_type_alias() {
    quiver()
        .evaluate(
            r#"
            'my_type = 'int | 'bin;
            p = 42 ~> @'my_type { $ } ~;
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
            p = [42, 100] ~> @['int, 'int] { $ } ~;
            !p
            "#,
        )
        .expect("[42, 100]");
}

#[test]
fn test_receive_simple() {
    quiver()
        .evaluate("p = @#[] { !#'int } []; %proc.send [p, 42]; !p")
        .expect("42");
}

#[test]
fn test_receive_waits_until_match() {
    quiver()
        .evaluate(
            r#"
            p = @#[] { ![#'int { =42 => Ok }] } [];
            %proc.send [p, 10]; %proc.send [p, 20]; %proc.send [p, 42];
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
            p = @#[] { ![#'int { =x => 99 }] } [];
            %proc.send [p, 42];
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
            p = @#[] { ![#'int { =x => Ok }] ~> =result => result } [];
            %proc.send [p, 42];
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_receive_filter_type_is_parameter_not_result() {
    // The type of a receive with a filter should be the parameter type (the message type),
    // not the filter's result type (Ok or []). The `!'int` clause is the function's own
    // receive type, rendered in the written clause syntax.
    quiver()
        .evaluate("#[] { ![#'int { Ok }] }")
        .expect_type("#[] -> 'int !'int");
}

// A restricted-context violation crashes the *filtering* process; `!` is never lethal,
// so the awaiter observes the crash as a `:crash`-stamped nil (an `Error[pid, message]`
// payload) rather than dying with it. These tests read the
// stamp with a checked retrieval; one also pins the crashed pid.

#[test]
fn test_receive_function_cannot_spawn() {
    quiver()
        .evaluate(
            r#"
            p = @#[] { ![#'int { @#[] { 42 } []; Ok }] } [];
            %proc.send [p, 10];
            r = !p;
            r:((message: Str['bin]))crash ~> =(message: m);
            m
            "#,
        )
        .expect("\"spawn is not allowed in receive function\"");
}

#[test]
fn test_receive_function_cannot_send() {
    quiver()
        .evaluate(
            r#"
            p1 = @#[] { !#'int } [];
            p2 = @#[] { ![#'int { %proc.send [p1, 42]; Ok }] } [];
            %proc.send [p2, 10];
            r = !p2;
            r:((message: Str['bin]))crash ~> =(message: m);
            m
            "#,
        )
        .expect("\"send is not allowed in receive function\"");
}

#[test]
fn test_receive_function_cannot_select() {
    quiver()
        .evaluate(
            r#"
            p = @#[] { ![#'int { !#'int; Ok }] } [];
            %proc.send [p, 10];
            r = !p;
            r:((message: Str['bin]))crash ~> =(message: m);
            m
            "#,
        )
        .expect("\"select is not allowed in receive function\"");
}

#[test]
fn test_receive_function_cannot_await() {
    // Also pins the crash payload's pid: it must compare equal (`=&p`) to the pid the
    // spawner holds, and the kind must be `Error` (a runtime error, not a panic).
    quiver()
        .evaluate(
            r#"
            q = @#[] { 42 } [];
            p = @#[] { ![#'int { !q; Ok }] } [];
            %proc.send [p, 10];
            r = !p;
            r:(Error(pid: (@)))crash ~> =Error(pid: &p)
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_receive_function_cannot_perform_effect() {
    quiver()
        .with_io()
        .evaluate(
            r#"
            p = @#[] { ![#'int { ["/dev/null" ~> .0, 0, 0] ~> __file_open__ ~; Ok }] } [];
            %proc.send [p, 10];
            r = !p;
            r:((message: Str['bin]))crash ~> =(message: m);
            m
            "#,
        )
        .expect("\"effect is not allowed in receive function\"");
}

// Containment-by-default ownership: `@f` spawns an owned
// child, torn down when its parent terminates — any reason, like resource auto-close.
// `%proc.detach` (parent-only) opts a child out. Note the reference idiom: a bare pid
// receiving a flowing value is a *send*, so arguments pass pids as `&p`.

#[test]
fn test_parent_termination_tears_down_children() {
    // The parent completes normally; both children (blocked receiving) are killed, and
    // awaiting them answers the `Killed` crash kind. Round-robin placement makes the
    // cross-worker kill path the common case here.
    quiver()
        .evaluate(
            r#"
            a = @#[] { [@#[] { !'int } [], @#[] { !'int } []] } [];
            !a ~> =[b1, b2];
            r1 = !b1;
            r2 = !b2;
            [r1:(Killed)crash, r2:(Killed)crash]
            "#,
        )
        .expect("[Killed, Killed]");
}

#[test]
fn test_detached_child_survives_parent() {
    quiver()
        .evaluate(
            r#"
            a = @#[] { p = @#[] { !'int } []; %proc.detach p; [p] } [];
            !a ~> =[b];
%proc.send [b, 42];
            !b ~> =('int)v;
            v
            "#,
        )
        .expect("42");
}

#[test]
fn test_detach_requires_ownership() {
    // Detaching is the owner's prerogative; a second detach (the entry is gone) fails
    // exactly like any non-owner's attempt.
    quiver()
        .evaluate(
            r#"
            p = @#[] { !'int } [];
            %proc.detach p;
            %proc.detach p
            "#,
        )
        .expect_runtime_error(quiver_core::error::Error::NotAnOwnedChild);
}

#[test]
fn test_kill_terminates_a_running_process() {
    // Retrieves with the `'%proc.crash` module alias (the documented general form):
    // the union admits any crash kind, and the expectation pins it to `Killed`. The
    // neighbouring tests use narrow shapes (`(Killed)`, `(Panic(message: …))`), where
    // the retrieval gate itself asserts the kind.
    quiver()
        .evaluate(
            r#"
            p = @#[] { !'int } [];
            %proc.kill p;
            r = !p;
            r:('%proc.crash)crash
            "#,
        )
        .expect("Killed");
}

#[test]
fn test_kill_of_completed_process_is_noop() {
    quiver()
        .evaluate(
            r#"
            p = @#[] { 42 } [];
            !p ~> =('int)v;
            %proc.kill p;
            v
            "#,
        )
        .expect("42");
}

#[test]
fn test_link_fires_on_abnormal_termination() {
    // Symmetric fate-sharing: c links v, then panics — v is killed with it.
    quiver()
        .evaluate(
            r#"
            v = @#[] { !'int } [];
            c = @#[] { %proc.link v; "die" ~> __panic__ ~ } [];
            rc = !c;
            rv = !v;
            rv:(Killed)crash
            "#,
        )
        .expect("Killed");
}

#[test]
fn test_link_is_silent_on_normal_completion() {
    // A finished peer doesn't take its gang with it: c completes normally, v lives on.
    quiver()
        .evaluate(
            r#"
            v = @#[] { !'int } [];
            c = @#[] { %proc.link v; 1 } [];
            !c ~> =('int)one;
            %proc.send [v, 42];
            !v ~> =('int)out;
            out
            "#,
        )
        .expect("42");
}

#[test]
fn test_link_to_crashed_process_kills_immediately() {
    // Tombstones keep the error, so there is no establishment race: linking a
    // dead-by-crash process kills the caller on the spot.
    quiver()
        .evaluate(
            r#"
            dead = @#[] { "x" ~> __panic__ ~ } [];
            { ![50] | Ok };
            c = @#[] { %proc.link dead; !'int } [];
            r = !c;
            r:(Killed)crash
            "#,
        )
        .expect("Killed");
}

#[test]
fn test_panic_is_catchable_at_await() {
    // A child's `__panic__` arrives at the await as a catchable `:crash` value of kind
    // `Panic` carrying the panic message — a process boundary is where "unrecoverable"
    // ends.
    quiver()
        .evaluate(
            r#"
            p = @#[] { "boom" ~> __panic__ ~ } [];
            r = !p;
            r:(Panic(message: Str['bin]))crash ~> =(message: m);
            m
            "#,
        )
        .expect("\"boom\"");
}

#[test]
fn test_timeout_stamp_carries_the_ms() {
    // A timed-out select answers nil stamped `:timeout` with the ms that fired,
    // discriminating a timeout from a crash or a legit nil result.
    quiver()
        .evaluate(
            r#"
            p = @#[] { !'int } [];
            r = ![p, 30];
            r:('int)timeout
            "#,
        )
        .expect("30");
}

#[test]
fn test_late_await_of_crashed_process_yields_same_crash() {
    // The crash persists on the tombstone: awaiting after the death answers the same
    // stamped nil as awaiting before it — observation has no deadline.
    quiver()
        .evaluate(
            r#"
            p = @#[] { "gone" ~> __panic__ ~ } [];
            { ![20] | Ok };
            r = !p;
            r:(Panic(pid: (@), message: Str['bin]))crash ~> =(pid: &p, message: m);
            m
            "#,
        )
        .expect("\"gone\"");
}

#[test]
fn test_await_same_process_twice() {
    // Re-awaiting a process re-registers the await, displacing the previously stored
    // result in the awaiting map — which must be released, not leaked (regression:
    // the insert used to clobber the retained value; caught by the debug refcount
    // invariant at completion).
    quiver()
        .evaluate(
            r#"
            p = @#[] { "hello" } [];
            !p;
            !p
            "#,
        )
        .expect("\"hello\"");
}

#[test]
fn test_send_to_completed_process_is_dropped() {
    // Messages to a dead process are dropped (not queued on the tombstone); the stored
    // result stays awaitable afterwards, and heap-carrying messages must not leak
    // (validated by the debug refcount invariant at completion).
    quiver()
        .evaluate(
            r#"
            p = @#[] { !#Str['bin] } [];
            %proc.send [p, "first"];
            !p;
            %proc.send [p, "second"];
            !p
            "#,
        )
        .expect("\"first\"");
}

#[test]
fn test_multiple_receives_same_type() {
    quiver().evaluate("@#[] { !#'int; !#'int } []").expect("@1");
}

#[test]
fn test_multiple_receives_different_types_widens() {
    // Multiple receives with different types should widen to a union
    // The function receives int | bin, and the receives return those types
    // Since we receive int first, then bin, the result is bin (last value)
    quiver().evaluate("@#[] { !#'int; !#'bin } []").expect("@1");
}

#[test]
fn test_await_simple() {
    quiver().evaluate("@#[] { 42 } [] ~> !").expect("42");
}

#[test]
fn test_await_returns_process_result() {
    quiver()
        .evaluate("@#[] { [1, 2] ~> __integer_add__ ~ } [] ~> !")
        .expect("3");
}

#[test]
fn test_await_with_captures() {
    quiver()
        .evaluate("x = 10; @#[] { [x, 32] ~> __integer_add__ ~ } [] ~> !")
        .expect("42");
}

#[test]
fn test_await_process_type_checking() {
    quiver()
        .evaluate(
            r#"
            await_fn = #(@!'int) { =p => !p };
            f = #[] { 42 };
            @f [] ~> await_fn ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_self_reference_cannot_be_awaited() {
    quiver()
        .evaluate("@#'int { !#'int ~> =x => @ ~> =self_pid; !self_pid } 42")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "process with receive type (awaitable/readable)".to_string(),
            found: "process without receive type (cannot select)".to_string(),
        });
}

#[test]
fn test_select_single_process() {
    quiver().evaluate("@#[] { 42 } [] ~> !").expect("42");
}

#[test]
fn test_select_multiple_processes_first_ready() {
    quiver()
        .evaluate(
            r#"
            fast = #[] { 42 };
            slow = #[] { !#'int };
            p1 = @fast [];
            p2 = @slow [];
            ![p1, p2]
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_priority_left_to_right() {
    quiver()
        .evaluate(
            r#"
            f1 = #[] { 42 };
            f2 = #[] { 100 };
            p1 = @f1 [];
            p2 = @f2 [];
            !p1; !p2;
            ![p1, p2]
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_single_receive() {
    quiver()
        .evaluate(
            r#"
            p = @#[] { !#'int } [];
            %proc.send [p, 42]; !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_multiple_receive_patterns() {
    quiver()
        .evaluate(
            r#"
            p = @#[] { ![#'int, #'bin] } [];
            %proc.send [p, 42]; !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_receive_pattern_priority() {
    quiver()
        .evaluate(
            r#"
            f = #[] {
                @ ~> =self_pid;
%proc.send [self_pid, 42];
%proc.send [self_pid, <00>];
                ![#'int, #'bin]
            };
            @f [] ~> !
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_receive_waits_for_match() {
    quiver()
        .evaluate(
            r#"
            f = #[] {
                @ ~> =self_pid;
%proc.send [self_pid, 10];
%proc.send [self_pid, 42];
%proc.send [self_pid, 99];
                !#'int
            };
            @f [] ~> !
            "#,
        )
        .expect("10");
}

#[test]
fn test_timeout_fires() {
    quiver()
        .evaluate(
            r#"
            slow = #[] { !#'int };
            ![@slow [], 1000]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_timeout_process_completes_first() {
    quiver()
        .evaluate(
            r#"
            fast = #[] { 42 };
            ![@fast [], 1000]
            "#,
        )
        .expect("42");
}

#[test]
fn test_timeout_zero() {
    quiver()
        .evaluate(
            r#"
            slow = #[] { !#'int };
            ![@slow [], 0]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_empty_select_returns_nil() {
    quiver().evaluate("![]").expect("[]");
}

#[test]
fn test_timeout_only() {
    quiver().evaluate("![100]").expect("[]");
}

#[test]
fn test_multiple_timeouts_uses_minimum() {
    // The elapsed assertion reads the environment's clock, so this one runs alone.
    quiver()
        .isolated()
        .evaluate("![2000, 100, 500]")
        .expect("[]")
        .expect_duration(100, 500);
}

#[test]
fn test_mixed_process_and_receive() {
    quiver()
        .evaluate(
            r#"
            make_receiver = #[] {
                fast = @#[] { 99 } [];
                !fast;
                ![#'int, fast]
            };
            receiver = @make_receiver [];
            %proc.send [receiver, 42]; !receiver
            "#,
        )
        .expect("42");
}

#[test]
fn test_mixed_all_three_types_receive_wins() {
    quiver()
        .evaluate(
            r#"
            receiver = @#[] {
                slow = @#[] { !#'bin } [];
                ![#'int, slow, 1000]
            } [];
            %proc.send [receiver, 42]; !receiver
            "#,
        )
        .expect("42");
}

#[test]
fn test_mixed_all_three_types_process_wins() {
    quiver()
        .evaluate(
            r#"
            @#[] {
                fast = @#[] { 99 } [];
                ![#'int, fast, 1000]
            } [] ~> !
            "#,
        )
        .expect("99");
}

#[test]
fn test_mixed_all_three_types_timeout_wins() {
    quiver()
        .evaluate(
            r#"
            @#[] {
                slow = @#[] { !#'bin } [];
                ![#'int, slow, 500]
            } [] ~> !
            "#,
        )
        .expect("[]");
}

#[test]
fn test_select_with_ripple() {
    quiver()
        .evaluate(
            r#"
            fast = #[] { 42 };
            @fast [] ~> ![~, 1000]
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_ripple_timeout_wins() {
    quiver()
        .evaluate(
            r#"
            slow = #[] { !#'int };
            @slow [] ~> ![~, 100]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_select_nested_chain_outer_used() {
    quiver()
        .evaluate(
            r#"
            fast = #[] { 42 };
            @fast [] ~> ![~, 3 ~> ~]
            "#,
        )
        .expect("42");
}

#[test]
fn test_select_receive_with_ripple() {
    quiver()
        .evaluate("p1 = @#[] { !#'int ~> [~] } []; %proc.send [p1, 0]; !p1")
        .expect("[0]");
}

#[test]
fn test_receive_type_from_variable() {
    quiver()
        .evaluate(
            r#"
            receiver_func = #'int;
            p = @#[] { ![receiver_func] ~> [~, 100] ~> __integer_add__ ~ } [];
            %proc.send [p, 42]; !p
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
        .evaluate("p = @#[] { !%int.and } []; %proc.send [p, [255, 240]]; !p")
        .expect("[255, 240]");
}

#[test]
fn test_body_less_receiver_builtin_matches_identity_function() {
    // A builtin and the equivalent body-less (identity) function behave identically as
    // receivers: both name the message type and return the received message, neither applies.
    quiver()
        .evaluate("p = @#[] { !%int.and } []; %proc.send [p, [255, 240]]; !p")
        .expect("[255, 240]");
    quiver()
        .evaluate("p = @#[] { !#['int, 'int] } []; %proc.send [p, [255, 240]]; !p")
        .expect("[255, 240]");
}

#[test]
fn test_postfix_select_with_function() {
    // Postfix form with a single function source
    // This is equivalent to ![receiver]
    quiver()
        .evaluate(
            r#"
            receiver = #'int;
            p = @#[] { @ ~> =self_pid;%proc.send [self_pid, 42]; !receiver } [];
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_postfix_select_with_timeout() {
    quiver().evaluate("@#[] { ![1] } [] ~> !").expect("[]");
}

#[test]
fn test_postfix_select_equivalence_timeout() {
    // Test that ![1] works for timeout
    quiver()
        .evaluate(
            r#"
            p = @#[] { ![1] } [];
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
            inner = #[] { ![#'int, 500] };
            p = @inner [];
            %proc.send [p, 42];
            ![p, 1000]
            "#,
        )
        .expect("42");
}

#[test]
fn test_continuation_after_timeout() {
    quiver()
        .evaluate(
            r#"
            @#[] {
                slow = @#[] { !#'int } [];
                result = ![slow, 100] ~> =[];
                [result, 42]
            } [] ~> !
            "#,
        )
        .expect("[Ok, 42]");
}

#[test]
fn test_process_spawns_and_receives_reply() {
    quiver()
        .evaluate(
            r#"
            child = #[] { ![#(@'int) { =parent => {%proc.send [parent, 42]; Ok } }] };
            parent = #[] { c = @child []; %proc.send [c, @]; !#'int };
            @parent []
            "#,
        )
        .expect("@1");
}

#[test]
fn test_send_to_self() {
    quiver()
        .evaluate("@#[] { me = @; %proc.send [me, 10]; !#'int } [] ~> !")
        .expect("10");
}

#[test]
fn test_send_to_self_with_receive_type_check() {
    quiver()
        .evaluate("#[] { me = @; %proc.send [me, <00>]; !#'int }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "in `1`: 'bin does not fit 'int".to_string(),
        ));
}

#[test]
fn test_receive_type_in_function_argument() {
    // Test that receive type is correctly inferred when used in function call arguments
    quiver()
        .evaluate(
            r#"
            p = 20 ~> @#'int { [~, !#'int] ~> %int.div ~ } ~;
            %proc.send [p, 2]; !p
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
            p = 10 ~> @#'int { [~, !#'int] } ~;
            %proc.send [p, 32]; !p
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
            p = 10 ~> @#'int { [~, !#'int] ~> __integer_add__ ~ } ~;
            %proc.send [p, 32]; !p
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
            f = #['int, 'int] { __integer_add__ ~ };
            p = 10 ~> @#'int { [~, !#'int] ~> ^f ~ } ~;
            %proc.send [p, 32]; !p
            "#,
        )
        .expect("42");
}

// Tests for new syntactic sugar (without # prefix)

#[test]
fn test_sugar_bare_primitive_type() {
    // Test !'int instead of !#'int
    quiver()
        .evaluate("p = @#[] { !'int } []; %proc.send [p, 42]; !p")
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
            p = @#[] { !('my_type) } [];
            %proc.send [p, 42]; !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_sugar_union_type() {
    // Test !('int | 'bin) instead of !#('int | 'bin)
    quiver()
        .evaluate(
            r#"
            p = @#[] { !('int | 'bin) } [];
            %proc.send [p, 42]; !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_sugar_receive_function_with_identifier_type() {
    // Test !'int { ... } instead of !#'int { ... }
    quiver()
        .evaluate(
            r#"
            p = @#[] { ![#'int { =42 => Ok }] } [];
            %proc.send [p, 10]; %proc.send [p, 20]; %proc.send [p, 42];
            !p
            "#,
        )
        .expect("42");
}

#[test]
fn test_sugar_receive_function_with_union_type() {
    // Test !('int | 'bin) { ... } instead of !#('int | 'bin) { ... }
    quiver()
        .evaluate(
            r#"
            p = @#[] { ![#('int | 'bin) { =42 => Ok }] } [];
            %proc.send [p, <00>]; %proc.send [p, 42];
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
            p = @#[] { !('receiver_type) } [];
            %proc.send [p, 42]; !p
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
            make_receiver = #[] {
                fast = @#[] { 99 } [];
                !fast;
                ![#'int, fast]
            };
            receiver = @make_receiver [];
            %proc.send [receiver, 42]; !receiver
            "#,
        )
        .expect("42");
}

#[test]
fn test_sugar_tuple_type() {
    // Test !#['int, 'int] for identity receive on tuple type
    // Note: ![...] is now sources syntax, so we need explicit #
    quiver()
        .evaluate(
            r#"
            p = @#[] { !#['int, 'int] } [];
            %proc.send [p, [42, 100]]; !p
            "#,
        )
        .expect("[42, 100]");
}

#[test]
fn test_process_reference_after_completion() {
    // Test that @N syntax works even after the process has completed
    quiver()
        .evaluate("p = @[] { 42 } []")
        .expect("Ok")
        .then_evaluate("!@1")
        .expect("42");
}

// A nilary process function ignores an implicitly-flowing init value (like a nilary call). The
// juxtaposed-argument spawn form (`@f x`) no longer exists, so supplying one is rejected at parse
// time.

#[test]
fn test_nilary_spawn_takes_no_init() {
    quiver().evaluate("p = @[] { 99 } []; !p").expect("99");
}

#[test]
fn test_filter_shorthand_skips_messages() {
    // A type shorthand with a body is a receive *filter* (the body-presence rule): a nil
    // verdict leaves the message in the mailbox. The filter receives the 2 (skipping the
    // queued 1), and the following body-less receive gets the 1 that stayed behind.
    quiver()
        .evaluate(
            r#"
            p = @#[] {
                me = @; %proc.send [me, 1];
                me = @; %proc.send [me, 2];
                !'int { =2 => Ok | [] };
                !'int
            } [];
            !p
            "#,
        )
        .expect("1");
}

#[test]
fn test_partial_type_receive_shorthand() {
    // `!(...)` accepts a partial type — the parens are part of the type syntax.
    quiver()
        .evaluate("p = @#[] { !(x: 'int) ~> .x } []; %proc.send [p, [x: 7, y: 8]]; !p")
        .expect("7");
}

#[test]
fn test_general_select_is_glued() {
    // The general form is glued like every other select form: `![sources]`.
    quiver()
        .evaluate("p = @#[] { 42 } []; ![p, 1000]")
        .expect("42");
    // A space between `!` and the tuple is a parse error (bare `!` then a stray term).
    quiver()
        .evaluate("p = @#[] { 42 } []; ! [p, 1000]")
        .expect_parse_failure();
}

#[test]
fn test_receive_type_propagates_through_call() {
    // Calling a receiver executes its receives in this process, so the spawned
    // function's receive type includes the callee's (regression: the widening was
    // computed but discarded when the callable's type was registered).
    quiver()
        .evaluate(
            r#"
            f = #[] { !#'int };
            g = #[] { f [] };
            p = @g [];
            %proc.send [p, 5];
            !p
            "#,
        )
        .expect("5");
}

#[test]
fn test_receive_type_propagates_through_tail_call() {
    // Same as above through `^f` (regression: tail calls never widened at all).
    quiver()
        .evaluate(
            r#"
            f = #[] { !#'int };
            g = #[] { ^f [] };
            p = @g [];
            %proc.send [p, 5];
            !p
            "#,
        )
        .expect("5");
}

#[test]
fn test_spawn_captures_and_heap_argument_share_one_index_space() {
    // Regression: a spawn's captures and argument ship with ONE heap side-channel.
    // Extracting them separately (each 0-based) and concatenating the vecs left the
    // argument's binaries pointing at the wrong entries whenever a capture carried
    // heap data — a captured string emptied the argument's segments.
    quiver()
        .evaluate(
            r#"
            s = %bin.concat ["bo" ~> .0, "o!" ~> .0]
            h = #[Str['bin], Str['bin]] {
              cap = s
              [$0, $1, Str[cap]]
            }
            arg = [%bin.concat ["w" ~> .0, "s" ~> .0] ~> Str[~], "plain"]
            hp = arg ~> @h ~
            !hp
            "#,
        )
        .expect(r#"["ws", "plain", "boo!"]"#);
}
