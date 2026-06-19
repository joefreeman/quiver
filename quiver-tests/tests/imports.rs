mod common;
use common::*;
use std::collections::HashMap;

#[test]
fn test_module_import() {
    let mut modules = HashMap::new();
    modules.insert(vec!["mymath".to_string()], "[add: &__add__]".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("%mymath.add[1, 2]")
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
            add[3, 4] ~> sub[~, 2]
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
        .evaluate("* = %mymath, add[3, 4]")
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
        .evaluate("%capture[]")
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
        #{ inner[] ~> [~, 2] ~> __multiply__ }
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%nested[]")
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
        .evaluate("t = %tuple_capture, f = &t.1, f[]")
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
        #{ %level1[] ~> [~, x] ~> __multiply__ }
        "#
        .to_string(),
    );
    modules.insert(
        vec!["level3".to_string()],
        r#"
        x = 5,
        [#{ %level2[] }, #{ %level2[] ~> [~, x] ~> __add__ }]
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules.clone())
        .evaluate("funcs = %level3, f1 = &funcs.0, f1[]")
        .expect("303"); // (100 + 1) * 3 = 303

    quiver()
        .with_modules(modules)
        .evaluate("funcs = %level3, f2 = &funcs.1, f2[]")
        .expect("308"); // ((100 + 1) * 3) + 5 = 308
}

#[test]
fn test_partial_type_import() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["types".to_string()],
        "ok : Ok[int]; err : Err[int]; []".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            (ok, err) : %types;
            double = #ok { =Ok[x] => [x, 2] ~> __multiply__ },
            Ok[21] ~> double
            "#,
        )
        .expect("42");
}

#[test]
fn test_star_type_import() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["types".to_string()],
        "result : Ok[int] | Err[int]; []".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            * : %types;
            unwrap = #result { =Ok[x] => x | =Err[x] => 0 },
            Ok[42] ~> unwrap
            "#,
        )
        .expect("42");
}

#[test]
fn test_import_generic() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["types".to_string()],
        "list<t> : Nil | Cons[t, ^];".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(r#"(list) : %types"#)
        .expect_alias("list", "Cons[t, μ1] | Nil");
}
