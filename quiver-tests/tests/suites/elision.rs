use crate::common::*;
use std::collections::HashMap;

// Wrapper elision: an applied module member whose body is a trivial forwarder — one
// builtin over reshaped parameter fields — compiles as the builtin call itself. These
// tests pin the *semantics* (results, contracts, references are unchanged); that the
// elision actually fires is pinned structurally in units.rs (a key-only import entry)
// and by the zzjsonbench micro numbers.

fn forwarders() -> HashMap<Vec<String>, String> {
    HashMap::from([(
        vec!["m".to_string()],
        r#"[
  add1: #[(n): 'int] { [$n, 1] ~> __integer_add__ ~ },
  byte: #[(bin): 'bin, (index): 'int] { __binary_get__ [$bin, $index, 0, 8] },
  pair_sum: #['int, 'int] { __integer_add__ $ },
  wrapped: #[(n): 'int] { [$n, 1] ~> __integer_add__ ~ ~> W[~] },
  checked: #[(n): 'int] {
    :pre #{ [$n, 0] ~> __integer_compare__ ~ ~> =1 }
    [$n, 1] ~> __integer_add__ ~
  },
]"#
        .to_string(),
    )])
}

#[test]
fn test_forwarder_calls_by_every_form() {
    // Juxtaposition with a literal argument, a piped whole tuple, and a flowing value
    // into an argument field all agree with the un-elided semantics.
    quiver()
        .with_modules(forwarders())
        .evaluate("%m.add1 [41]")
        .expect("42");
    quiver()
        .with_modules(forwarders())
        .evaluate("[<6162>, 1] ~> %m.byte ~")
        .expect("98");
    quiver()
        .with_modules(forwarders())
        .evaluate("41 ~> %m.add1 [~]")
        .expect("42");
}

#[test]
fn test_forwarder_shapes() {
    // The whole parameter forwarded unwrapped, and a result wrapped in a tuple.
    quiver()
        .with_modules(forwarders())
        .evaluate("%m.pair_sum [20, 22]")
        .expect("42");
    quiver()
        .with_modules(forwarders())
        .evaluate("%m.wrapped [41]")
        .expect("W[42]");
}

#[test]
fn test_forwarder_reference_is_a_real_function() {
    // Naming a member does not call it, so the member must still exist as a callable
    // value for a later application to reach.
    quiver()
        .with_modules(forwarders())
        .evaluate("f = %m.add1; f [41]")
        .expect("42");
    quiver()
        .with_modules(forwarders())
        .evaluate("[a: %m.add1] ~> .a ~> ~ [n: 41]")
        .expect("42");
}

#[test]
fn test_contract_carrying_forwarder_is_not_elided() {
    // A forwarder-shaped body carrying `:pre` must keep the normal call path — an
    // elided call would silently skip debug-mode contract enforcement.
    quiver()
        .debug()
        .with_modules(forwarders())
        .evaluate("%m.checked [5]")
        .expect("6");
    quiver()
        .debug()
        .with_modules(forwarders())
        .evaluate("%m.checked [-5]")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Precondition violated at test:1:1".to_string(),
        ));
}

#[test]
fn test_forwarder_argument_evaluation_still_happens() {
    // Argument field expressions run exactly once, in order, elided or not.
    quiver()
        .with_modules(forwarders())
        .evaluate("x = 40; %m.add1 [[x, 1] ~> __integer_add__ ~]")
        .expect("42");
}
