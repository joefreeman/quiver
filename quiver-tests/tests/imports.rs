mod common;
use common::*;
use std::collections::HashMap;

#[test]
fn test_module_import() {
    let mut modules = HashMap::new();
    modules.insert(vec!["mymath".to_string()], "[add: &__add__]".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("%mymath.add [1, 2]")
        .expect("3");
}

#[test]
fn test_destructured_import() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["mymath".to_string()],
        r#"[add: &__add__, sub: &__subtract__]"#.to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            (add, sub) = %mymath,
            add [3, 4] ~> sub [~, 2]
            "#,
        )
        .expect("5");
}

#[test]
fn test_star_import() {
    let mut modules = HashMap::new();
    modules.insert(vec!["mymath".to_string()], "[add: &__add__]".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("* = %mymath, add [3, 4]")
        .expect("7");
}

#[test]
fn test_import_function_with_capture() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["capture".to_string()],
        "x = 42, #{ [x, 2] ~> __multiply__ }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%capture []")
        .expect("84");
}

#[test]
fn test_import_nested_function_captures() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["nested".to_string()],
        r#"
        x = 10,
        inner = #{ [x, 1] ~> __add__ },
        #{ inner [] ~> [~, 2] ~> __multiply__ }
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%nested []")
        .expect("22");
}

#[test]
fn test_import_tuple_with_captured_function() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["tuple_capture".to_string()],
        "x = 5, y = 3, [x, #{ [x, y] ~> __add__ }, y]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("t = %tuple_capture, f = &t.1, f []")
        .expect("8");
}

#[test]
fn test_multi_level_import_with_captures() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["level1".to_string()],
        "base = 100, #{ [base, 1] ~> __add__ }".to_string(),
    );
    modules.insert(
        vec!["level2".to_string()],
        r#"
        x = 3,
        #{ %level1 [] ~> [~, x] ~> __multiply__ }
        "#
        .to_string(),
    );
    modules.insert(
        vec!["level3".to_string()],
        r#"
        x = 5,
        [#{ %level2 [] }, #{ %level2 [] ~> [~, x] ~> __add__ }]
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules.clone())
        .evaluate("funcs = %level3, f1 = &funcs.0, f1 []")
        .expect("303"); // (100 + 1) * 3 = 303

    quiver()
        .with_modules(modules)
        .evaluate("funcs = %level3, f2 = &funcs.1, f2 []")
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
            double = #'ok { =Ok[x] => [x, 2] ~> __multiply__ },
            Ok[21] ~> double
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
            unwrap = #'%types.result { =Ok[x] => x | =Err[x] => 0 },
            Ok[42] ~> unwrap
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
            unwrap = #'%result { =Ok[x] => x | =Err[x] => 0 },
            Ok[42] ~> unwrap
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
            Ok[42] ~> unwrap
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
            Cons[7, Nil] ~> len?
            "#,
        )
        .expect("1");
}
