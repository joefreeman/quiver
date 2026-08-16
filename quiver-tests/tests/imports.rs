mod common;
use common::*;
use std::collections::HashMap;

#[test]
fn test_module_import() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["mymath".to_string()],
        "[add: __integer_add__]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("[1, 2] ~> %mymath.add ~")
        .expect("3");
}

#[test]
fn test_destructured_import() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["mymath".to_string()],
        r#"[add: __integer_add__, sub: __integer_subtract__]"#.to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            (add, sub) = %mymath;
            [3, 4] ~> add ~ ~> [~, 2] ~> sub ~
            "#,
        )
        .expect("5");
}

#[test]
fn test_star_import() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["mymath".to_string()],
        "[add: __integer_add__]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("* = %mymath; [3, 4] ~> add ~")
        .expect("7");
}

#[test]
fn test_import_function_with_capture() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["capture".to_string()],
        "x = 42; #{ [x, 2] ~> __integer_multiply__ ~ }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("[] ~> %capture ~")
        .expect("84");
}

#[test]
fn test_import_nested_function_captures() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["nested".to_string()],
        r#"
        x = 10;
        inner = #{ [x, 1] ~> __integer_add__ ~ };
        #{ [] ~> inner ~ ~> [~, 2] ~> __integer_multiply__ ~ }
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("[] ~> %nested ~")
        .expect("22");
}

#[test]
fn test_import_tuple_with_captured_function() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["tuple_capture".to_string()],
        "x = 5; y = 3; [x, #{ [x, y] ~> __integer_add__ ~ }, y]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("t = %tuple_capture; f = t.1; [] ~> f ~")
        .expect("8");
}

#[test]
fn test_multi_level_import_with_captures() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["level1".to_string()],
        "base = 100; #{ [base, 1] ~> __integer_add__ ~ }".to_string(),
    );
    modules.insert(
        vec!["level2".to_string()],
        r#"
        x = 3;
        #{ [] ~> %level1 ~ ~> [~, x] ~> __integer_multiply__ ~ }
        "#
        .to_string(),
    );
    modules.insert(
        vec!["level3".to_string()],
        r#"
        x = 5;
        [#{ [] ~> %level2 ~ }, #{ [] ~> %level2 ~ ~> [~, x] ~> __integer_add__ ~ }]
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules.clone())
        .evaluate("funcs = %level3; f1 = funcs.0; [] ~> f1 ~")
        .expect("303"); // (100 + 1) * 3 = 303

    quiver()
        .with_modules(modules)
        .evaluate("funcs = %level3; f2 = funcs.1; [] ~> f2 ~")
        .expect("308"); // ((100 + 1) * 3) + 5 = 308
}

#[test]
fn test_named_module_type_alias() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["types".to_string()],
        "'ok = Ok['int]; 'err = Err['int]; []".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            'ok = '%types.ok;
            double = #'ok { =Ok[x] => [x, 2] ~> __integer_multiply__ ~ };
            Ok[21] ~> double ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_named_module_type_inline() {
    // A module type used inline in a function signature, without a local alias.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["types".to_string()],
        "'result = Ok['int] | Err['int]; []".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            unwrap = #'%types.result { =Ok[x] => x | =Err[x] => 0 };
            Ok[42] ~> unwrap ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_default_module_type() {
    // The module's nameless default type, referenced as `'%mod`.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["result".to_string()],
        "' = Ok['int] | Err['int]; []".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            unwrap = #'%result { =Ok[x] => x | =Err[x] => 0 };
            Ok[42] ~> unwrap ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_generic_module_type() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["types".to_string()],
        "'list<'t> = Nil | Cons['t, ^];".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(r#"'list<'t> = '%types.list<'t>"#)
        .expect_alias("list", "Cons['t, μ1] | Nil");
}

#[test]
fn test_generic_default_module_type_applied() {
    // A generic default type instantiated with a concrete argument.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["list".to_string()],
        "'<'t> = Nil | Cons['t, ^];".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(r#"'ints = '%list<'int>"#)
        .expect_alias("ints", "Cons['int, μ1] | Nil");
}

#[test]
fn test_module_type_missing() {
    let mut modules = HashMap::new();
    modules.insert(vec!["types".to_string()], "'ok = Ok['int]; []".to_string());
    quiver()
        .with_modules(modules)
        .evaluate(r#"'nope = '%types.missing"#)
        .expect_compile_error(quiver_compiler::compiler::Error::ModuleTypeMissing {
            type_name: "missing".to_string(),
            module: "types".to_string(),
        });
}

#[test]
fn test_default_aliasing_named_in_same_module() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["range".to_string()],
        "'range = Range['int, 'int]; ' = 'range; []".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(r#"'r = '%range"#)
        .expect_alias("r", "Range['int, 'int]");
}

#[test]
fn test_self_default_type_reference() {
    // Within a module, a bare `'` refers to the module's own default type.
    quiver()
        .evaluate(
            r#"
            ' = Ok['int] | Err['int]
            unwrap = #' { =Ok[x] => x | =Err[_] => 0 }
            Ok[42] ~> unwrap ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_self_default_type_parameterised() {
    quiver()
        .evaluate(
            r#"
            '<'t> = Nil | Cons['t, ^]
            len? = #'<'int> { =Nil => 0 | =Cons[_, _] => 1 }
            Cons[7, Nil] ~> len? ~
            "#,
        )
        .expect("1");
}

#[test]
fn test_module_ref_minting_rejected() {
    // A module's value must be identity-free (deterministic, shareable across the
    // sessions that import it), so minting a ref during compile-time module
    // evaluation is rejected — like host-state reads.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["tagged".to_string()],
        r#"tag = %ref []; [tag: tag]"#.to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%tagged.tag")
        .expect_error_containing("creating a ref is not supported in compile-time execution");
}

#[test]
fn test_module_host_read_rejected() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["stamped".to_string()],
        "[at: %time.now []]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%stamped.at")
        .expect_error_containing("reading host state is not supported in compile-time execution");
}

#[test]
fn test_module_may_reference_ref_minting_function() {
    // Referencing the minting function is fine — only evaluation-time minting is
    // banned; the ref is minted at runtime, by the importer.
    let mut modules = HashMap::new();
    modules.insert(vec!["util".to_string()], "[mk: %ref]".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("%util.mk [] ~> ='ref")
        .expect("Ok");
}

#[test]
fn test_reexported_closure_with_foreign_capture() {
    // Transitive value embedding: %c re-exports %b's closure, whose captures embed
    // a function of %a — a module %c's own source never references. %a sits in
    // %c's transitive value-import closure (through %b), so its Merkle key covers
    // %a and %c's artifact records an ordinary import of it. Two sessions, so the
    // second links %c from the artifact the first extracted.
    let modules = || {
        let mut modules = HashMap::new();
        modules.insert(
            vec!["a".to_string()],
            "[inc: #'int { [~, 1] ~> __integer_add__ ~ }]".to_string(),
        );
        modules.insert(
            vec!["b".to_string()],
            "inc = %a.inc\n[wrapped: #'int { $ ~> inc ~ }]".to_string(),
        );
        modules.insert(vec!["c".to_string()], "[go: %b.wrapped]".to_string());
        modules
    };
    quiver()
        .with_modules(modules())
        .evaluate("5 ~> %c.go ~")
        .expect("6");
    quiver()
        .with_modules(modules())
        .evaluate("7 ~> %c.go ~")
        .expect("8");
}

// ---------------------------------------------------------------------------------------
// Constant-build hoisting + reconstruction CSE.
//
// A module reference in a function body becomes a synthetic capture (built where the
// closure is built, loaded in the body), and repeated emission of the same cached value
// shares one CSE slot per scope. The slots are ordinary locals, so their lifetime must
// track the compiler's Reset choreography exactly; each test below pins one edge where a
// stale slot would load a local the runtime has discarded (observed as an
// undefined-variable runtime error, or a wrong value).

#[test]
fn hoisted_import_rebuilds_in_next_branch() {
    // Branch isolation clears the shared block scope (no per-branch push): a slot
    // registered by the failed first branch must not survive into the second, whose
    // path never allocated the local.
    quiver()
        .evaluate("x = 5 ~> { | =4 => %num.add [1, 2] | %num.add [10, 20] }; x")
        .expect("30");
}

#[test]
fn hoisted_import_shared_from_condition_to_consequence() {
    // Same branch: the condition's slot is live for the consequence — the sharing case.
    quiver()
        .evaluate("x = { %num.add [1, 2] ~> =3 => %num.add [4, 5] | 0 }; x")
        .expect("9");
}

#[test]
fn hoisted_import_rebuilds_after_block_scope_exit() {
    // A slot registered inside a block dies with the block's scope (and its Reset); the
    // sequence continuing outside must rebuild, not load the discarded local.
    quiver()
        .evaluate("x = 1 ~> { t = %num.add [~, 1]; t }; y = %num.add [x, 10]; y")
        .expect("12");
}

#[test]
fn cse_slots_cleared_before_convergence_attach() {
    // A block's annotation-attach chains compile at the convergence, after the runtime
    // Reset(locals_before) but with the block scope still current — every slot a branch
    // registered is dead there and the attach value must rebuild. (This edge was latent
    // in the first CSE implementation.)
    quiver()
        .evaluate("x = 0 ~> {\n  :note %num.add [1, 2]\n  | A[%num.add [10, 20]]\n}\nx ~> :note")
        .expect("3");
}

#[test]
fn hoisting_through_module_binding() {
    // A member reached through a whole-module binding captures by path; the closure
    // loads its slot rather than re-emitting the member's construction per call.
    quiver()
        .evaluate("num = %num; f = #'int { num.sub [~, 1] }; 8 ~> f ~")
        .expect("7");
}

// ---------------------------------------------------------------------------------------
// Module bodies are compile-time evaluated and reject process/effect work — the program's
// top level no longer is (it runs at boot), so modules are where these rejections live.

#[test]
fn module_spawn_rejected_at_compile_time() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["spawner".to_string()],
        "p = @#{ 42 } []; [x: 1]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%spawner.x")
        .expect_error_containing("spawning a process is not supported in compile-time execution");
}

#[test]
fn module_send_rejected_statically() {
    // A module body can never obtain a sendable pid: spawning is rejected, and the
    // module top level has no receive type, so `.` types as a process that cannot be
    // sent to. The rejection is therefore static — the runtime `Operation::Send` arm
    // remains only as a backstop for other compile-time execution (dialects).
    let mut modules = HashMap::new();
    modules.insert(
        vec!["sender".to_string()],
        "me = .; 42 ~> me ~; !'int; [x: 1]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%sender.x")
        .expect_error_containing("found process without send type");
}

#[test]
fn module_receive_stalls_precisely_at_compile_time() {
    // A module-body receive can never be satisfied (no sender can exist at compile
    // time); the sync executor's backstop reports it rather than spinning.
    let mut modules = HashMap::new();
    modules.insert(vec!["receiver".to_string()], "!'int; [x: 1]".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("%receiver.x")
        .expect_error_containing("waiting to receive a message that can never arrive");
}

#[test]
fn instantiated_members_never_share_a_cse_slot() {
    // An instantiated builtin member's payload is synthesized fresh per use site, so two
    // instantiations must never collapse into one slot — `CseKey` owns the payload `Rc`
    // precisely so a freed payload's address can't alias a later one. Each decode below
    // must keep its own type argument.
    quiver()
        .evaluate(
            r#"a = %data.decode<'int> "42"; b = %data.decode<'bin> "0x0a"; c = %data.decode<'int> "7"; [a, b, c]"#,
        )
        .expect("[42, 0x0a, 7]");
}

#[test]
fn test_the_std_listing_is_stable_and_unique() {
    // `std_module_names` gives whole-std artifacts a canonical compilation sequence, so
    // it must be sorted and free of duplicates.
    let names = quiver_compiler::resolver::std_module_names();
    assert!(names.contains(&"http/client".to_string()));
    assert!(names.contains(&"http/tcp".to_string()));
    let mut deduped = names.clone();
    deduped.dedup();
    assert_eq!(names, deduped, "std module names must be unique");
}
