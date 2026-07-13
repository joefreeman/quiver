mod common;
use common::quiver;

// %sup (std/sup.qv): supervision as library code on the step-2 process primitives
// (docs/process-state.md). Each test's start closure spawns a worker that announces
// its pid to the test process (so restarts are observable as fresh announcements) and
// then waits: 0 makes it crash, any other int completes it normally.
//
// The shared shape:
//   worker:  @{ &. ~> me; !'int ~> { =0 => panic | ... } }
//   start:   spawns the worker, wires its watcher (%sup.watch), answers the pid

#[test]
fn test_supervisor_restarts_crashed_child() {
    quiver()
        .with_io()
        .evaluate(
            r#"
            me = &.
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @{
                &. ~> me
                !'int ~> { =0 => "boom" ~> __panic__ | [] ~> ^ }
              }
              %sup.watch [id, &w, &sup]
              &w
            }
            spec = [id: "w", restart: Permanent, start: &mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start
            p1 = !#(@'int)
            0 ~> p1
            p2 = !#(@'int)
            [&p2] ~> { =[&p1] => "same pid" | "restarted" }
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
            me = &.
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @{
                &. ~> me
                !'int ~> { =0 => "boom" ~> __panic__ | [] ~> ^ }
              }
              %sup.watch [id, &w, &sup]
              &w
            }
            spec = [id: "w", restart: Permanent, start: &mk]
            sup = [children: Cons[spec, Nil], max_restarts: 2, window: 5000] ~> %sup.start
            p1 = !#(@'int); 0 ~> p1
            p2 = !#(@'int); 0 ~> p2
            p3 = !#(@'int); 0 ~> p3
            r = !sup
            r:(Panic(message: Str['bin]))crash ~> =(message: msg)
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
            me = &.
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @{
                &. ~> me
                !'int ~> { =0 => "boom" ~> __panic__ | Ok }
              }
              %sup.watch [id, &w, &sup]
              &w
            }
            spec = [id: "w", restart: Temporary, start: &mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start
            p1 = !#(@'int)
            0 ~> p1
            { | ![#(@'int), 200] ~> =(@'int)p2 => "restarted" | "no restart" }
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
            me = &.
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @{
                &. ~> me
                !'int ~> { =0 => "boom" ~> __panic__ | Ok }
              }
              %sup.watch [id, &w, &sup]
              &w
            }
            spec = [id: "w", restart: Transient, start: &mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start
            p1 = !#(@'int)
            0 ~> p1
            p2 = !#(@'int)
            [&p2] ~> { =[&p1] => "same pid" | "restarted" }
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
            me = &.
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @{
                &. ~> me
                !'int ~> { =0 => "boom" ~> __panic__ | Ok }
              }
              %sup.watch [id, &w, &sup]
              &w
            }
            spec = [id: "w", restart: Transient, start: &mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start
            p1 = !#(@'int)
            1 ~> p1
            { | ![#(@'int), 200] ~> =(@'int)p2 => "restarted" | "no restart" }
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
            me = &.
            mk = #[Str['bin], (@'%sup.down)] {
              =[id, sup]
              w = @{
                &. ~> me
                !'int ~> { =0 => "boom" ~> __panic__ | [] ~> ^ }
              }
              %sup.watch [id, &w, &sup]
              &w
            }
            spec = [id: "w", restart: Permanent, start: &mk]
            sup = [children: Cons[spec, Nil], max_restarts: 3, window: 5000] ~> %sup.start
            // The declared message type spells the await grant (`-> []`): a declared
            // type grants only what it spells, and this test awaits the child.
            p1 = !#(@'int -> [])
            %proc.kill &sup
            r = !p1
            r:(Killed)crash
            "#,
        )
        .expect("Killed");
}
