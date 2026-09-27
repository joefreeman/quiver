use crate::common::quiver;

// %sup (std/sup.qv): supervision as library code on the step-2 process primitives.
// Each test's start closure spawns a worker that announces
// its pid to the test process (so restarts are observable as fresh announcements) and
// then waits: 0 makes it crash, any other int completes it normally.
//
// The shared shape:
//   worker:  @[] { %proc.send [me, @]; !'int ~> { =0 => panic | ... } } []
//   start:   spawns the worker, wires its watcher (%sup.watch), answers the pid

#[test]
fn test_supervisor_restarts_crashed_child() {
    quiver()
        .with_io()
        .evaluate(
            r#"
            me = @
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @[] {
                %proc.send [me, @]
                !'int ~> { =0 => "boom" ~> __panic__ ~ | [] ~> ^ ~ }
              } []
              %sup.watch [id, w, sup]
              w
            }
            spec = [id: "w", restart: Permanent, start: mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start ~
            p1 = !#(@'int)
%proc.send [p1, 0]
            p2 = !#(@'int)
            [p2] ~> { =[^p1] => "same pid" | "restarted" }
            "#,
        )
        .expect("\"restarted\"");
}

#[test]
fn test_restart_intensity_limit_escalates() {
    // Three crashes against max_restarts 2: the supervisor gives up and crashes
    // itself — escalation is just crashing, catchable at the awaiter like any panic.
    quiver()
        .with_io()
        .evaluate(
            r#"
            me = @
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @[] {
                %proc.send [me, @]
                !'int ~> { =0 => "boom" ~> __panic__ ~ | [] ~> ^ ~ }
              } []
              %sup.watch [id, w, sup]
              w
            }
            spec = [id: "w", restart: Permanent, start: mk]
            sup = [children: Cons[spec, Nil], max_restarts: 2, window: 5000] ~> %sup.start ~
            p1 = !#(@'int);%proc.send [p1, 0]
            p2 = !#(@'int);%proc.send [p2, 0]
            p3 = !#(@'int);%proc.send [p3, 0]
            r = !sup
            r:crash<Panic(message: Str['bin])> ~> =(message: msg)
            msg
            "#,
        )
        .expect("\"%sup: restart intensity exceeded\"");
}

#[test]
fn test_temporary_child_is_not_restarted() {
    quiver()
        .with_io()
        .evaluate(
            r#"
            me = @
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @[] {
                %proc.send [me, @]
                !'int ~> { =0 => "boom" ~> __panic__ ~ | Ok }
              } []
              %sup.watch [id, w, sup]
              w
            }
            spec = [id: "w", restart: Temporary, start: mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start ~
            p1 = !#(@'int)
%proc.send [p1, 0]
            { | ![#(@'int), 200] ~> =(@'int & p2) => "restarted" | "no restart" }
            "#,
        )
        .expect("\"no restart\"");
}

#[test]
fn test_transient_child_restarts_on_crash() {
    quiver()
        .with_io()
        .evaluate(
            r#"
            me = @
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @[] {
                %proc.send [me, @]
                !'int ~> { =0 => "boom" ~> __panic__ ~ | Ok }
              } []
              %sup.watch [id, w, sup]
              w
            }
            spec = [id: "w", restart: Transient, start: mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start ~
            p1 = !#(@'int)
%proc.send [p1, 0]
            p2 = !#(@'int)
            [p2] ~> { =[^p1] => "same pid" | "restarted" }
            "#,
        )
        .expect("\"restarted\"");
}

#[test]
fn test_transient_child_not_restarted_after_normal_completion() {
    quiver()
        .with_io()
        .evaluate(
            r#"
            me = @
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @[] {
                %proc.send [me, @]
                !'int ~> { =0 => "boom" ~> __panic__ ~ | Ok }
              } []
              %sup.watch [id, w, sup]
              w
            }
            spec = [id: "w", restart: Transient, start: mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start ~
            p1 = !#(@'int)
%proc.send [p1, 1]
            { | ![#(@'int), 200] ~> =(@'int & p2) => "restarted" | "no restart" }
            "#,
        )
        .expect("\"no restart\"");
}

#[test]
fn test_killing_the_supervisor_tears_down_its_children() {
    // Containment: children (and watchers) are owned by the supervisor process, so
    // killing it takes the subtree — the worker answers the Killed crash kind.
    quiver()
        .with_io()
        .evaluate(
            r#"
            me = @
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @[] {
                %proc.send [me, @]
                !'int ~> { =0 => "boom" ~> __panic__ ~ | [] ~> ^ ~ }
              } []
              %sup.watch [id, w, sup]
              w
            }
            spec = [id: "w", restart: Permanent, start: mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start ~
            // The declared message type spells the await grant (`-> []`): a declared
            // type grants only what it spells, and this test awaits the child.
            p1 = !#(@'int ![])
            %proc.kill [sup]
            r = !p1
            r:crash<Killed()>
            "#,
        )
        .expect("Killed[reason: []]");
}

#[test]
fn test_add_supervises_child_dynamically() {
    // A child added to a RUNNING supervisor is supervised like any startup spec:
    // it announces, crashes, and a fresh incarnation announces.
    quiver()
        .with_io()
        .evaluate(
            r#"
            me = @
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @[] {
                %proc.send [me, @]
                !'int ~> { =0 => "boom" ~> __panic__ ~ | [] ~> ^ ~ }
              } []
              %sup.watch [id, w, sup]
              w
            }
            sup = [children: Nil, max_restarts: 3, window: 5000] ~> %sup.start ~
            v = %sup.add [sup, [id: "w", restart: Permanent, start: mk]]
            started? = v ~> { =Started[_] => Ok | [] }
            p1 = !#(@'int)
%proc.send [p1, 0]
            p2 = !#(@'int)
            [started?, [p2] ~> { =[^p1] => "same pid" | "restarted" }]
            "#,
        )
        .expect(r#"[Ok, "restarted"]"#);
}

#[test]
fn test_add_duplicate_id_rejected() {
    quiver()
        .with_io()
        .evaluate(
            r#"
            me = @
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @[] { %proc.send [me, @]; !'int ~> { =0 => "boom" ~> __panic__ ~ | [] ~> ^ ~ } } []
              %sup.watch [id, w, sup]
              w
            }
            spec = [id: "w", restart: Permanent, start: mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start ~
            !#(@'int)
            %sup.add [sup, spec]
            "#,
        )
        .expect("Duplicate");
}

#[test]
fn test_drop_forgets_and_id_is_reusable() {
    // Terminating kills the child and drops its row; its watcher's report is
    // pin-ignored (the pid no longer matches — id even REUSED here by a fresh
    // start_child, which must not be disturbed by the late report). The killed
    // incarnation's death restarts nothing: only the second incarnation's own
    // crash produces a new announcement.
    quiver()
        .with_io()
        .evaluate(
            r#"
            me = @
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @[] { %proc.send [me, @]; !'int ~> { =0 => "boom" ~> __panic__ ~ | [] ~> ^ ~ } } []
              %sup.watch [id, w, sup]
              w
            }
            spec = [id: "w", restart: Permanent, start: mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start ~
            p1 = !#(@'int)
            %sup.drop [sup, "w"]
            v = %sup.add [sup, spec]
            p2 = !#(@'int)
            // The dead first incarnation must not spawn a third announcement: give
            // any spurious restart a moment, then crash p2 and expect exactly one.
            spurious = ![#(@'int), 100] ~> { =(@'int) => Spurious | Quiet }
%proc.send [p2, 0]
            p3 = !#(@'int)
            [v ~> { =Started[_] => Ok | [] }, spurious, [p3] ~> { =[^p2] => "same" | "fresh" }]
            "#,
        )
        .expect(r#"[Ok, Quiet, "fresh"]"#);
}
