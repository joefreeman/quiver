mod common;
use common::*;
use std::collections::HashMap;

#[test]
fn test_module_import() {
    let mut modules = HashMap::new();
    modules.insert("./math.qv".to_string(), "[add: <add>]".to_string());

    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            math = %"./math.qv",
            math.add[1, 2]
            "#,
        )
        .expect("3");
}

#[test]
fn test_destructured_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "./math.qv".to_string(),
        r#"[add: <add>, sub: <subtract>]"#.to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            (add, sub) = %"./math.qv",
            add[3, 4] ~> sub[~, 2]
            "#,
        )
        .expect("5");
}

#[test]
fn test_star_import() {
    let mut modules = HashMap::new();
    modules.insert("./math.qv".to_string(), "[add: <add>]".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("* = %\"./math.qv\", add[3, 4]")
        .expect("7");
}

#[test]
fn test_import_function_with_capture() {
    let mut modules = HashMap::new();
    modules.insert(
        "./capture.qv".to_string(),
        "x = 42, #{ [x, 2] ~> <multiply> }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("f = %\"./capture.qv\", f[]")
        .expect("84");
}

#[test]
fn test_import_nested_function_captures() {
    let mut modules = HashMap::new();
    modules.insert(
        "./nested.qv".to_string(),
        r#"
        x = 10,
        inner = #{ [x, 1] ~> <add> },
        #{ inner[] ~> [~, 2] ~> <multiply> }
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("f = %\"./nested.qv\", f[]")
        .expect("22");
}

#[test]
fn test_import_tuple_with_captured_function() {
    let mut modules = HashMap::new();
    modules.insert(
        "./tuple_capture.qv".to_string(),
        "x = 5, y = 3, [x, #{ [x, y] ~> <add> }, y]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("t = %\"./tuple_capture.qv\", f = t.1, f[]")
        .expect("8");
}

#[test]
fn test_multi_level_import_with_captures() {
    let mut modules = HashMap::new();
    modules.insert(
        "./level1.qv".to_string(),
        "base = 100, #{ [base, 1] ~> <add> }".to_string(),
    );
    modules.insert(
        "./level2.qv".to_string(),
        r#"
        f = %"./level1.qv",
        x = 3,
        #{ f[] ~> [~, x] ~> <multiply> }
        "#
        .to_string(),
    );
    modules.insert(
        "./level3.qv".to_string(),
        r#"
        g = %"./level2.qv",
        x = 5,
        [g, #{ g[] ~> [~, x] ~> <add> }]
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules.clone())
        .evaluate("funcs = %\"./level3.qv\", f1 = funcs.0, f1[]")
        .expect("303"); // (100 + 1) * 3 = 303

    quiver()
        .with_modules(modules)
        .evaluate("funcs = %\"./level3.qv\", f2 = funcs.1, f2[]")
        .expect("308"); // ((100 + 1) * 3) + 5 = 308
}

#[test]
fn test_partial_type_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "./types.qv".to_string(),
        "ok :: Ok[int]; err :: Err[int]; []".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            (ok, err) :: %"./types.qv";
            double = #ok { ~> =Ok[x] => [x, 2] ~> <multiply> },
            Ok[21] ~> double
            "#,
        )
        .expect("42");
}

#[test]
fn test_star_type_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "./types.qv".to_string(),
        "result :: Ok[int] | Err[int]; []".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(
            r#"
            * :: %"./types.qv";
            unwrap = #result { ~> =Ok[x] => x | ~> =Err[x] => 0 },
            Ok[42] ~> unwrap
            "#,
        )
        .expect("42");
}
