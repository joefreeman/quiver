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
        .evaluate("p = @#[] { !#'int } []; p 42")
        .expect("Ok");
}

#[test]
fn test_process_without_receive_rejects_send() {
    quiver()
        .evaluate("p = @#[] { [] } []; p 42")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "a process that receives messages".to_string(),
            found: "@never ![] ?[]".to_string(),
        });
}

#[test]
fn test_process_type_checking_send() {
    quiver()
        .evaluate(
            r#"
            p = @#[] { !#'int } [];
            p <00>
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "message compatible with 'int".to_string(),
            found: "'bin".to_string(),
        });
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
            p 42;
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
            p 42
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "message compatible with Str['bin]".to_string(),
            found: "'int".to_string(),
        });
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
            p 42
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::UnionApplication {
            union: "'int | (@'int !'int ?'int)".to_string(),
        });
}

#[test]
fn test_pipe_into_function_union_calls_it() {
    // A union of function types is one function at runtime: the argument must suit every
    // member, and the result is whichever member's.
    quiver()
        .evaluate(
            r#"
            f = #'int { $ };
            g = #'int { "hi" };
            u = Ok ~> { | =Ok => f | g };
            42 ~> u ~
        "#,
        )
        .expect("42")
        .expect_type("'int | Str['bin]");
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
fn test_spawn_sugar_generic() {
    quiver()
        .evaluate("p = @<'t>['t, 't] { $0 } [42, 7]; !p")
        .expect("42");
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
        .evaluate("p = @#[] { !#'int } []; p 42; !p")
        .expect("42");
}

#[test]
fn test_receive_waits_until_match() {
    quiver()
        .evaluate(
            r#"
            p = @#[] { ![#'int { =42 => Ok }] } [];
            p 10; p 20; p 42;
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
            p 42;
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
            p 42;
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
            p 10;
            r = !p;
            r:crash<(message: Str['bin])> ~> =(message: m);
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
            p2 = @#[] { ![#'int { p1 42; Ok }] } [];
            p2 10;
            r = !p2;
            r:crash<(message: Str['bin])> ~> =(message: m);
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
            p 10;
            r = !p;
            r:crash<(message: Str['bin])> ~> =(message: m);
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
            p 10;
            r = !p;
            r:crash<Error(pid: (@))> ~> { =Error(pid: ^p) => Matched | Missed }
            "#,
        )
        .expect("Matched");
}

#[test]
fn test_receive_function_cannot_perform_effect() {
    quiver()
        .with_io()
        .evaluate(
            r#"
            p = @#[] { ![#'int { ["/dev/null" ~> .0, 0, 0] ~> __file_open__ ~; Ok }] } [];
            p 10;
            r = !p;
            r:crash<(message: Str['bin])> ~> =(message: m);
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
            [r1:crash<Killed()>, r2:crash<Killed()>]
            "#,
        )
        .expect("[Killed[reason: []], Killed[reason: []]]");
}

#[test]
fn test_detached_child_survives_parent() {
    quiver()
        .evaluate(
            r#"
            a = @#[] { p = @#[] { !'int } []; %proc.detach p; [p] } [];
            !a ~> =[b];
b 42;
            !b ~> =('int & v);
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
    // neighbouring tests use narrow shapes (`Killed()`, `(Panic(message: …))`), where
    // the retrieval gate itself asserts the kind.
    quiver()
        .evaluate(
            r#"
            p = @#[] { !'int } [];
            %proc.kill [p];
            r = !p;
            r:crash<'%proc.crash>
            "#,
        )
        .expect("Killed[reason: []]");
}

#[test]
fn test_kill_reason_reaches_awaiters() {
    // The reason is delivered in the `Killed` payload. It is built at `reason: _`, so a
    // field pattern (not a narrower checked retrieval) reads it.
    quiver()
        .evaluate(
            r#"
            p = @#[] { !'int } [];
            %proc.kill [p, reason: Timeout[5000]];
            r = !p;
            r:crash<'%proc.crash> ~> =Killed[reason: Timeout[~]]
            "#,
        )
        .expect("5000");
}

#[test]
fn test_kill_reason_must_be_data() {
    // The reason outlives the killer in the target's tombstone, so identity is refused —
    // statically, as the reason is typed `'%data`.
    quiver()
        .evaluate(
            r#"
            p = @#[] { !'int } [];
            %proc.kill [p, reason: [by: @]]
            "#,
        )
        .expect_error_containing("reason: [by: @");
}

#[test]
fn test_sleep_leaves_the_mailbox_untouched() {
    quiver()
        .evaluate(
            r#"
            @ 7;
            %proc.sleep %time{ 10ms };
            !'int
            "#,
        )
        .expect("7");
}

#[test]
fn test_send_after_delivers_unless_cancelled() {
    // The earlier timer is killed, so the later message is the first to arrive. Were the
    // cancel to fail, 2 would arrive first — no window to race.
    quiver()
        .evaluate(
            r#"
            %proc.send_after [@, 1, after: %time{ 300ms }];
            t = %proc.send_after [@, 2, after: %time{ 200ms }];
            %proc.kill [t];
            !'int
            "#,
        )
        .expect("1");
}

#[test]
fn test_send_after_is_cancelled_with_its_sender() {
    // The timer is owned by the process that set it, so it dies with it.
    quiver()
        .evaluate(
            r#"
            me = @;
            s = @[] { %proc.send_after [me, 1, after: %time{ 200ms }]; Ok } [];
            !s;
            { ![#'int, 300] }
            "#,
        )
        .expect("[]");
}

#[test]
fn test_expire_kills_with_a_timeout_reason() {
    quiver()
        .evaluate(
            r#"
            p = @[] { !'int } [];
            %proc.expire [p, %time{ 10ms }];
            r = !p;
            r:crash<'%proc.crash>
            "#,
        )
        .expect("Killed[reason: Timeout[Duration[10000000]]]");
}

#[test]
fn test_expire_watchdog_ends_with_its_process() {
    // A process that finishes first is untouched, and the watchdog does not linger until
    // the deadline.
    quiver()
        .evaluate(
            r#"
            p = @[] { 5 } [];
            w = %proc.expire [p, %time{ 100s }];
            [!p, ![w, 1000]]
            "#,
        )
        .expect("[5, Ok]");
}

#[test]
fn test_race_answers_the_first_success() {
    // Nil and crashed racers count as failures, not wins.
    quiver()
        .evaluate(
            r#"
            %proc.race [%list{
              #[] { [] },
              #[] { __panic__ "x" },
              #[] { %proc.sleep %time{ 20ms }; 2 },
              #[] { %proc.sleep %time{ 500ms }; 3 },
            }]
            "#,
        )
        .expect("2");
}

#[test]
fn test_race_fails_when_every_racer_fails() {
    quiver()
        .evaluate(
            r#"
            [
              { %proc.race [%list{ #[] { [] }, #[] { __panic__ "x" } }] },
              { %proc.race [Nil] },
            ]
            "#,
        )
        .expect("[[], []]");
}

#[test]
fn test_race_kills_the_losers() {
    // The loser would send after the race is decided; it is torn down first.
    quiver()
        .evaluate(
            r#"
            me = @;
            w = %proc.race [%list{ #[] { 1 }, #[] { %proc.sleep %time{ 200ms }; me 9; 2 } }];
            [w, { ![#'int, 300] }]
            "#,
        )
        .expect("[1, []]");
}

#[test]
fn test_race_timeout_abandons_an_undecided_race() {
    quiver()
        .evaluate(
            r#"
            r = %proc.race [%list{ #[] { %proc.sleep %time{ 1000ms }; 1 } }, timeout: %time{ 10ms }];
            r:crash<'%proc.crash>
            "#,
        )
        .expect("Killed[reason: Timeout[Duration[10000000]]]");
}

#[test]
fn test_kill_of_completed_process_is_noop() {
    quiver()
        .evaluate(
            r#"
            p = @#[] { 42 } [];
            !p ~> =('int & v);
            %proc.kill [p];
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
            rv:crash<Killed()>
            "#,
        )
        .expect("Killed[reason: []]");
}

#[test]
fn test_link_is_silent_on_normal_completion() {
    // A finished peer doesn't take its gang with it: c completes normally, v lives on.
    quiver()
        .evaluate(
            r#"
            v = @#[] { !'int } [];
            c = @#[] { %proc.link v; 1 } [];
            !c ~> =('int & one);
            v 42;
            !v ~> =('int & out);
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
            r:crash<Killed()>
            "#,
        )
        .expect("Killed[reason: []]");
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
            r:crash<Panic(message: Str['bin])> ~> =(message: m);
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
            r:timeout<'int>
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
            r:crash<Panic(pid: (@), message: Str['bin])> ~> =(pid: ^p, message: m);
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
            p "first";
            !p;
            p "second";
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
            p 42; !p
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
            p 42; !p
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
self_pid 42;
self_pid <00>;
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
self_pid 10;
self_pid 42;
self_pid 99;
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
            receiver 42; !receiver
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
            receiver 42; !receiver
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
        .evaluate("p1 = @#[] { !#'int ~> [~] } []; p1 0; !p1")
        .expect("[0]");
}

#[test]
fn test_receive_type_from_variable() {
    quiver()
        .evaluate(
            r#"
            receiver_func = #'int;
            p = @#[] { ![receiver_func] ~> [~, 100] ~> __integer_add__ ~ } [];
            p 42; !p
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
        .evaluate("p = @#[] { !%int.and } []; p [255, 240]; !p")
        .expect("[255, 240]");
}

#[test]
fn test_body_less_receiver_builtin_matches_identity_function() {
    // A builtin and the equivalent body-less (identity) function behave identically as
    // receivers: both name the message type and return the received message, neither applies.
    quiver()
        .evaluate("p = @#[] { !%int.and } []; p [255, 240]; !p")
        .expect("[255, 240]");
    quiver()
        .evaluate("p = @#[] { !#['int, 'int] } []; p [255, 240]; !p")
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
            p = @#[] { @ ~> =self_pid;self_pid 42; !receiver } [];
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
            p 42;
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
                result = ![slow, 100] ~> { =[] => TimedOut | Arrived };
                [result, 42]
            } [] ~> !
            "#,
        )
        .expect("[TimedOut, 42]");
}

#[test]
fn test_process_spawns_and_receives_reply() {
    quiver()
        .evaluate(
            r#"
            child = #[] { ![#(@'int) { =parent => {parent 42; Ok } }] };
            parent = #[] { c = @child []; c @; !#'int };
            @parent []
            "#,
        )
        .expect("@1");
}

#[test]
fn test_send_to_self() {
    quiver()
        .evaluate("@#[] { me = @; me 10; !#'int } [] ~> !")
        .expect("10");
}

#[test]
fn test_send_to_self_with_receive_type_check() {
    quiver()
        .evaluate("#[] { @ <00>; !#'int }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "message compatible with 'int".to_string(),
            found: "'bin".to_string(),
        });
}

#[test]
fn test_send_to_self_applied() {
    // A bare `@` applies like any other name: `@ x` sends to the current process.
    quiver()
        .evaluate("@#[] { @ 10; 20 ~> @ ~; [!#'int, !#'int] } [] ~> !")
        .expect("[10, 20]");
}

#[test]
fn test_send_answers_ok() {
    quiver()
        .evaluate("p = @#[] { !#'int } []; p 42")
        .expect("Ok");
}

#[test]
fn test_send_to_ungranted_process_is_error() {
    // A pid whose type states no send grant accepts nothing, whatever the process
    // behind it receives.
    quiver()
        .evaluate(
            r#"
            f = #(@!'int) { $ 1 };
            p = @#[] { !#'int } [];
            f p
        "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::SendNotGranted {
            process: "@!'int".to_string(),
        });
}

#[test]
fn test_union_send_accepts_common_message() {
    // A message every member accepts is sendable, even where the members' grants
    // differ.
    quiver()
        .evaluate(
            r#"
            f = #'int { !'int };
            g = #'int { !('int | 'bin) };
            p = Ok ~> { | =Ok => 0 ~> @f ~ | 0 ~> @g ~ };
            p 42
        "#,
        )
        .expect("Ok");
}

#[test]
fn test_receive_type_in_function_argument() {
    // Test that receive type is correctly inferred when used in function call arguments
    quiver()
        .evaluate(
            r#"
            p = 20 ~> @#'int { [~, !#'int] ~> %int.div ~ } ~;
            p 2; !p
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
            p 32; !p
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
            p 32; !p
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
            p 32; !p
            "#,
        )
        .expect("42");
}

// Tests for new syntactic sugar (without # prefix)

#[test]
fn test_sugar_bare_primitive_type() {
    // Test !'int instead of !#'int
    quiver()
        .evaluate("p = @#[] { !'int } []; p 42; !p")
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
            p 42; !p
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
            p 42; !p
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
            p 10; p 20; p 42;
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
            p <00>; p 42;
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
            p 42; !p
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
            receiver 42; !receiver
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
            p [42, 100]; !p
            "#,
        )
        .expect("[42, 100]");
}

#[test]
fn test_process_reference_after_completion() {
    // Test that @N syntax works even after the process has completed
    quiver()
        .evaluate("p = @[] { 42 } []")
        .expect("@1")
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
                me = @; me 1;
                me = @; me 2;
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
        .evaluate("p = @#[] { !(x: 'int) ~> .x } []; p [x: 7, y: 8]; !p")
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
            p 5;
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
            p 5;
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

// A binary whose bytes live in the constants table crosses to another process as that
// *index* rather than as a byte copy — workers share the table — and is rebuilt on the
// far side still naming it. The receiving process must be able to use what it got, so
// the builtins that read bytes have to resolve the index. A debug build's failure-origin
// module name is such a binary (the site table names the constant instead of allocating
// bytes), which is what these two carry across.

#[test]
fn test_constant_binary_crosses_as_a_spawn_init() {
    quiver()
        .debug()
        .evaluate(
            r#"
            r = { 1 ~> =2 };
            r:origin<(module: '%str, line: 'int)> ~> =(module: m);
            p = m ~> @'%str { %str.length $ } ~;
            !p
            "#,
        )
        .expect("4");
}

#[test]
fn test_constant_binary_crosses_in_a_message() {
    quiver()
        .debug()
        .evaluate(
            r#"
            p = @#[] { !'%str ~> { =Str[b] => %bin.length b } } [];
            r = { 1 ~> =2 };
            r:origin<(module: '%str, line: 'int)> ~> =(module: m);
            p m;
            !p
            "#,
        )
        .expect("4");
}

#[test]
fn test_a_receive_cannot_take_a_type_variable_it_cannot_fix() {
    // Messages are tested against the receive type at runtime, where no instantiation is
    // known. A function's own type parameters are instantiated afresh by each call...
    quiver()
        .evaluate("f = #<'t>'t -> 't { !'t }; f")
        .expect_error_containing("A receive can't take 't");
    quiver()
        .evaluate("f = #<'t>'t -> 't { g = #[] -> 't { !'t }; g [] }; f")
        .expect_error_containing("A receive can't take 't");
    // ... and a generic function value's are never instantiated at all: a receive through
    // one would type any message as its `'q`.
    quiver()
        .evaluate(
            "id = #<'q>'q { $ }
             p = @[] { !id ~> __integer_add__ [~, 1] } []
             p <01>",
        )
        .expect_error_containing("A receive can't take 'q");
}

#[test]
fn test_a_process_may_receive_its_enclosing_type_parameter() {
    // Fixed for as long as the process lives, and every send to it is checked against the
    // same instantiation — how `%proc.race` collects its racers' results.
    quiver()
        .evaluate(
            "g = #<'t>'t { v = $; c = @[] { !Box['t] ~> =Box[x]; x } []; c Box[v]; !c }
             g 5 ~> { | =('int & n) => __integer_add__ [n, 1] | 0 }",
        )
        .expect("6");
}

#[test]
fn test_a_receive_may_name_an_alias_from_its_body() {
    quiver()
        .evaluate("p = @[] { 'm = Ping | Pong; !'m } []; p Ping; !p")
        .expect("Ping");
}
