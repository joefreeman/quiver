use crate::common::*;
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
        "x = 42; #[] { [x, 2] ~> __integer_multiply__ ~ }".to_string(),
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
        inner = #[] { [x, 1] ~> __integer_add__ ~ };
        #[] { [] ~> inner ~ ~> [~, 2] ~> __integer_multiply__ ~ }
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
        "x = 5; y = 3; [x, #[] { [x, y] ~> __integer_add__ ~ }, y]".to_string(),
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
        "base = 100; #[] { [base, 1] ~> __integer_add__ ~ }".to_string(),
    );
    modules.insert(
        vec!["level2".to_string()],
        r#"
        x = 3;
        #[] { [] ~> %level1 ~ ~> [~, x] ~> __integer_multiply__ ~ }
        "#
        .to_string(),
    );
    modules.insert(
        vec!["level3".to_string()],
        r#"
        x = 5;
        [#[] { [] ~> %level2 ~ }, #[] { [] ~> %level2 ~ ~> [~, x] ~> __integer_add__ ~ }]
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
        .expect_error_containing(
            "creating a ref is not supported; move this work into a function the module exports",
        );
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
        .expect_error_containing("reading host state is not supported");
}

#[test]
fn test_module_may_reference_ref_minting_function() {
    // Referencing the minting function is fine — only evaluation-time minting is
    // banned; the ref is minted at runtime, by the importer.
    let mut modules = HashMap::new();
    modules.insert(vec!["util".to_string()], "[mk: %ref]".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("%util.mk [] ~> { ='ref => IsRef | NotRef }")
        .expect("IsRef");
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
// A module reference is a compile-time-known value, so it compiles to the single constant
// that names it, wherever it appears. These pin the positions that used to need care, when
// the value was rebuilt inline and shared through per-scope slots whose lifetime had to
// track the compiler's Reset choreography exactly: each was once an edge where a stale slot
// loaded a local the runtime had discarded.

#[test]
fn import_emits_in_each_branch() {
    // Branches share one block scope, and the first one here fails.
    quiver()
        .evaluate("x = 5 ~> { | =4 => %num.add [1, 2] | %num.add [10, 20] }; x")
        .expect("30");
}

#[test]
fn import_emits_across_condition_and_consequence() {
    // The same member in a branch's condition and in its consequence.
    quiver()
        .evaluate("x = { %num.add [1, 2] ~> =3 => %num.add [4, 5] | 0 }; x")
        .expect("9");
}

#[test]
fn import_emits_after_block_scope_exit() {
    // Inside a block, then again in the sequence that continues after it.
    quiver()
        .evaluate("x = 1 ~> { t = %num.add [~, 1]; t }; y = %num.add [x, 10]; y")
        .expect("12");
}

#[test]
fn import_emits_in_a_convergence_attach() {
    // A block's annotation-attach chains compile at the convergence — after the runtime
    // `Reset(locals_before)`, with the block scope still current.
    quiver()
        .evaluate("x = 0 ~> {\n  :note %num.add [1, 2]\n  | A[%num.add [10, 20]]\n}\nx ~> :note")
        .expect("3");
}

#[test]
fn import_through_a_module_binding() {
    // A member reached through a whole-module binding rather than named directly.
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
        "p = @#[] { 42 } []; [x: 1]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%spawner.x")
        .expect_error_containing("spawning a process is not supported");
}

#[test]
fn module_send_rejected_statically() {
    // A module body can never obtain a sendable pid: spawning is rejected, and the
    // module top level has no receive type, so `@` types as a process whose send grant
    // is `never` — nothing fits it. The rejection is therefore static.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["sender".to_string()],
        "me = @; me 42; !'int; [x: 1]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%sender.x")
        .expect_error_containing("a process that receives messages");
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
fn instantiated_members_never_share_a_constant() {
    // Constants intern by content, so anything the content does not distinguish collapses.
    // A type-consuming builtin's instantiation must therefore be part of what its constant
    // says — it is, because `register_builtin_instantiated` gives each instantiation its own
    // builtin id, and that id is what `Constant::Builtin` carries. Each decode below must
    // keep its own type argument.
    quiver()
        .evaluate(
            r#"a = %data.decode<'int> "42"; b = %data.decode<'bin> "<0a>"; c = %data.decode<'int> "7"; [a, b, c]"#,
        )
        .expect("[42, <0a>, 7]");
}

/// The manifest routing tests below address a module by *file path* and reach it through a
/// `path` provider, which is what makes them distinct from the `with_modules` tests above:
/// a provider's directory appears in the module's id (its path relative to the package
/// root) but not in the import path that reached it. Anything that resolves an id as
/// though it were an import path prepends that directory twice, so these projects only
/// compile while the two stay distinguished. The test harness always attaches an artifact
/// store, which is what drives the type namespace to be built for every compiled module.
#[test]
fn a_module_resolves_through_a_path_provider() {
    quiver()
        .with_files(&[
            (
                "quiver.toml",
                r#"modules = [{ std = true }, { path = "./src" }]"#,
            ),
            ("src/util.qv", "[double: #'int { %num.mul [$, 2] }]"),
        ])
        .evaluate("%util.double 21")
        .expect("42");
}

#[test]
fn a_module_resolves_through_a_named_path_provider() {
    quiver()
        .with_files(&[
            (
                "quiver.toml",
                r#"modules = [{ std = true }, { name = "mathx", path = "./vendor/mathx/src" }]"#,
            ),
            (
                "vendor/mathx/src/calc.qv",
                "[triple: #'int { %num.mul [$, 3] }]",
            ),
        ])
        .evaluate("%mathx/calc.triple 14")
        .expect("42");
}

#[test]
fn a_module_under_a_path_provider_imports_its_neighbour() {
    // The importing module is itself resolved through the provider, so its own imports
    // are the recursive case: each level must keep resolving by path, not by id.
    quiver()
        .with_files(&[
            (
                "quiver.toml",
                r#"modules = [{ std = true }, { path = "./src" }]"#,
            ),
            ("src/inner.qv", "[triple: #'int { %num.mul [$, 3] }]"),
            (
                "src/outer.qv",
                "[sextuple: #'int { %inner.triple $ ~> %num.mul [~, 2] }]",
            ),
        ])
        .evaluate("%outer.sextuple 7")
        .expect("42");
}

#[test]
fn a_module_type_resolves_through_a_path_provider() {
    // `'%util` builds the module's type namespace by name, the same machinery the
    // compiler drives eagerly for every module once a store is attached.
    quiver()
        .with_files(&[
            (
                "quiver.toml",
                r#"modules = [{ std = true }, { path = "./src" }]"#,
            ),
            (
                "src/util.qv",
                "' = Point[x: 'int, y: 'int]\n[origin: Point[x: 0, y: 0]]",
            ),
        ])
        .evaluate("x = #'%util { $x }; x %util.origin")
        .expect("0");
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

/// Where a compile error in an imported module is, as `(name, line, column)` per module on
/// the import path, outermost first.
fn module_path(
    error: &quiver_compiler::compiler::LocatedError,
) -> Vec<(String, Option<(usize, usize)>)> {
    error
        .modules
        .iter()
        .map(|site| {
            (
                site.name.clone(),
                site.span.map(|span| (span.line, span.column)),
            )
        })
        .collect()
}

fn line_column(span: Option<quiver_compiler::parser::SourceSpan>) -> Option<(usize, usize)> {
    span.map(|span| (span.line, span.column))
}

#[test]
fn an_error_in_a_module_is_located_in_that_module() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["bad".to_string()],
        "x = 1\n[f: #[] { __integer_add__ [x, <01>] }]".to_string(),
    );
    let error = quiver()
        .with_modules(modules)
        .evaluate("y = 2\ny ~> %bad.f ~")
        .expect_located_compile_error();
    assert!(matches!(
        error.error,
        quiver_compiler::compiler::Error::TypeMismatch { .. }
    ));
    // The position in the compiled source is the import; the module holds the error.
    assert_eq!(line_column(error.span), Some((2, 6)));
    assert_eq!(
        module_path(&error),
        vec![("%bad".to_string(), Some((2, 11)))]
    );
}

#[test]
fn an_error_in_a_nested_module_carries_its_import_path() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["inner".to_string()],
        "[f: #[] { __integer_add__ [1, <01>] }]".to_string(),
    );
    modules.insert(
        vec!["outer".to_string()],
        "\ni = %inner\n[g: #[] { 1 }]".to_string(),
    );
    let error = quiver()
        .with_modules(modules)
        .evaluate("%outer.g []")
        .expect_located_compile_error();
    assert_eq!(line_column(error.span), Some((1, 1)));
    assert_eq!(
        module_path(&error),
        vec![
            ("%outer".to_string(), Some((2, 5))),
            ("%inner".to_string(), Some((1, 11))),
        ]
    );
}

#[test]
fn a_module_parse_error_is_located_in_that_module() {
    let mut modules = HashMap::new();
    modules.insert(vec!["broken".to_string()], "x = 1\ny = [".to_string());
    let error = quiver()
        .with_modules(modules)
        .evaluate("%broken")
        .expect_located_compile_error();
    assert!(matches!(
        error.error,
        quiver_compiler::compiler::Error::ModuleParse { .. }
    ));
    assert_eq!(error.modules.len(), 1);
    assert_eq!(error.modules[0].span.map(|span| span.line), Some(2));
}

#[test]
fn a_module_evaluation_error_has_no_position_in_the_module() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["failing".to_string()],
        "x = 1\n__panic__ \"boom\"".to_string(),
    );
    let error = quiver()
        .with_modules(modules)
        .evaluate("[] ~> { %failing }")
        .expect_located_compile_error();
    assert!(
        error
            .to_string()
            .starts_with("Evaluating module %failing at compile time failed: boom"),
        "{error}"
    );
    // Evaluation has no source position, so the import is all there is to point at.
    assert_eq!(line_column(error.span), Some((1, 9)));
    assert_eq!(module_path(&error), vec![("%failing".to_string(), None)]);
}

#[test]
fn an_error_after_an_import_is_not_located_in_the_module() {
    let mut modules = HashMap::new();
    modules.insert(vec!["fine".to_string()], "\n\n\n[f: #[] { 1 }]".to_string());
    let error = quiver()
        .with_modules(modules)
        .evaluate("x = %fine\nnope")
        .expect_located_compile_error();
    assert!(error.modules.is_empty());
    assert_eq!(error.span.map(|span| span.line), Some(2));
}

#[test]
fn the_standard_library_compiles_without_warnings() {
    // Importers aren't shown std's warnings, so this is where they surface: compile every
    // std module from source (no artifact store) and read what each recorded.
    let source: String = quiver_compiler::resolver::std_module_names()
        .iter()
        .enumerate()
        .map(|(index, name)| format!("m{index} = %{name}\n"))
        .collect();
    let builtins = quiver_core::builtins::BuiltinRegistry::<quiver_io::NativeEffect>::with_modules(
        &quiver_core::builtins::universal_modules(),
    );
    let mut program = quiver_core::program::Program::new();
    let mut module_cache = quiver_compiler::compiler::ModuleCache::new();
    let nil_type_id = program.register_type(quiver_core::types::Type::nil());
    quiver_compiler::Compiler::compile(
        quiver_compiler::parse(&source).unwrap(),
        &quiver_compiler::compiler::Bindings::default(),
        Default::default(),
        &mut module_cache,
        &quiver_compiler::PackageResolver::memory(HashMap::new()),
        &mut program,
        nil_type_id,
        &HashMap::new(),
        &builtins,
        None,
        Default::default(),
    )
    .expect("the standard library compiles");
    let warnings: Vec<String> = module_cache
        .value_cache
        .iter()
        .flat_map(|(id, cached)| {
            cached.warnings.iter().map(move |(warning, span)| {
                format!("%{}:{}:{}: {warning}", id.display(), span.line, span.column)
            })
        })
        .collect();
    assert!(warnings.is_empty(), "{}", warnings.join("\n"));
}

#[test]
fn an_unreachable_branch_is_warned_about() {
    let result = quiver().evaluate("x = 5\nx ~> { | =y => y | 2 }");
    let warnings = result.warnings();
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    let span = warnings[0].span.expect("in the compiled source");
    assert_eq!((span.line, span.column), (2, 20));
    let quiver_compiler::compiler::Warning::UnreachableBranch { cause } = warnings[0].warning
    else {
        panic!(
            "expected an unreachable branch, got {:?}",
            warnings[0].warning
        );
    };
    assert_eq!((cause.line, cause.column), (2, 10));
}

#[test]
fn a_branch_that_can_fail_is_not_warned_about() {
    let result = quiver().evaluate("x = 5\nx ~> { | =5 => 1 | =y => y | 3 }");
    // Only the last fallback is unreachable: `=5` can fail.
    let lines: Vec<usize> = result
        .warnings()
        .iter()
        .map(|w| w.span.unwrap().column)
        .collect();
    assert_eq!(lines, vec![30]);
}

#[test]
fn a_module_warning_is_reported_in_the_module_once() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["m".to_string()],
        "\n[f: #'int { | =x => x | 2 }]".to_string(),
    );
    let result = quiver().with_modules(modules).evaluate("%m.f 1");
    let warnings = result.warnings();
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    let site = warnings[0].module.as_ref().expect("in the module");
    assert_eq!(site.name, "%m");
    assert_eq!(site.span.map(|span| span.line), Some(2));
    // The session has reported it: a later line using the module doesn't repeat it.
    let result = result.then_evaluate("%m.f 2");
    assert!(result.warnings().is_empty(), "{:?}", result.warnings());
}

/// The warnings an evaluation raised, as `(kind, line, column)` in the compiled source.
fn warning_sites(result: &crate::common::TestResult) -> Vec<(String, usize, usize)> {
    result
        .warnings()
        .iter()
        .map(|warning| {
            let span = warning.span.expect("in the compiled source");
            let kind = format!("{:?}", warning.warning);
            let kind = kind.split([' ', '{']).next().unwrap().to_string();
            (kind, span.line, span.column)
        })
        .collect()
}

#[test]
fn a_match_that_can_never_succeed_is_warned_about() {
    let result = quiver().evaluate("x = 5\nx ~> { | =<01> => 1 | ='bin => 2 | =A => 3 | 4 }");
    assert_eq!(
        warning_sites(&result),
        vec![
            ("ImpossibleMatch".to_string(), 2, 10),
            ("ImpossibleMatch".to_string(), 2, 23),
            ("ImpossibleMatch".to_string(), 2, 36),
        ]
    );
}

#[test]
fn a_step_that_is_always_nil_is_warned_about() {
    let result = quiver().evaluate("5 ~> { []; 2 }");
    assert_eq!(
        warning_sites(&result),
        vec![("AlwaysNil".to_string(), 1, 8)]
    );
    // Not the last step of a branch, though: `| []` is how a block says "otherwise, nil".
    let result = result.then_evaluate("5 ~> { | =4 => 1 | [] }");
    assert!(result.warnings().is_empty(), "{:?}", result.warnings());
}

#[test]
fn an_unused_binding_is_warned_about() {
    let result = quiver().evaluate("{ x = 1; y = 2; x }");
    assert_eq!(
        warning_sites(&result),
        vec![("UnusedBinding".to_string(), 1, 10)]
    );
}

#[test]
fn bindings_that_do_their_job_unread_are_not_warned_about() {
    // A repeated binder tests its occurrences equal; a star binds every field by design; a
    // pin, a capture and an alternation's binders all read or bind as written.
    quiver()
        .evaluate(
            r#"{
              [1, 1] ~> =[x, x]
              * = [a: 1, b: 2]
              y = 2; 2 ~> =^y
              z = 3; f = #[] { z }; f []
              [[], 5] ~> =([w, []] | [[], w]); w
            }"#,
        )
        .expect_no_warnings();
}

#[test]
fn a_session_top_level_binding_is_not_warned_about() {
    // Later lines may read it.
    let result = quiver().evaluate("x = 1");
    assert!(result.warnings().is_empty(), "{:?}", result.warnings());
}
