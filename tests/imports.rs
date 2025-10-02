mod common;
use common::*;
use std::collections::HashMap;

#[test]
fn test_module_import() {
    let mut modules = HashMap::new();
    modules.insert("./math.qv".to_string(), "[add: <add>]".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("%\"./math.qv\" ~> math, [1, 2] ~> math.add!")
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
        .evaluate("%\"./math.qv\" ~> (add, sub), [3, 4] ~> add! ~> [~, 2] ~> sub!")
        .expect("5");
}

#[test]
fn test_star_import() {
    let mut modules = HashMap::new();
    modules.insert("./math.qv".to_string(), "[add: <add>]".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("%\"./math.qv\" ~> *, [3, 4] ~> add!")
        .expect("7");
}

#[test]
fn test_import_function_with_capture() {
    let mut modules = HashMap::new();
    modules.insert(
        "./capture.qv".to_string(),
        "42 ~> x, #{ [x, 2] ~> <multiply>! }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%\"./capture.qv\" ~> f, f!")
        .expect("84");
}

#[test]
fn test_import_nested_function_captures() {
    let mut modules = HashMap::new();
    modules.insert(
        "./nested.qv".to_string(),
        r#"
        10 ~> x,
        #{ [x, 1] ~> <add>! } ~> inner,
        #{ inner! ~> [~, 2] ~> <multiply>! }
        "#
        .to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%\"./nested.qv\" ~> f, f!")
        .expect("22");
}

#[test]
fn test_import_tuple_with_captured_function() {
    let mut modules = HashMap::new();
    modules.insert(
        "./tuple_capture.qv".to_string(),
        "5 ~> x, 3 ~> y, [x, #{ [x, y] ~> <add>! }, y]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("%\"./tuple_capture.qv\" ~> t, t.1 ~> f, f!")
        .expect("8");
}

#[test]
fn test_multi_level_import_with_captures() {
    let mut modules = HashMap::new();
    modules.insert(
        "./level1.qv".to_string(),
        "100 ~> base, #{ [base, 1] ~> <add>! }".to_string(),
    );
    modules.insert(
        "./level2.qv".to_string(),
        "%\"./level1.qv\" ~> f, 3 ~> x, #{ f! ~> [~, x] ~> <multiply>! }".to_string(),
    );
    modules.insert(
        "./level3.qv".to_string(),
        "%\"./level2.qv\" ~> g, 5 ~> x, [g, #{ g! ~> [~, x] ~> <add>! }]".to_string(),
    );

    quiver()
        .with_modules(modules.clone())
        .evaluate("%\"./level3.qv\" ~> funcs, funcs.0 ~> f1, f1!")
        .expect("303"); // (100 + 1) * 3 = 303

    quiver()
        .with_modules(modules)
        .evaluate("%\"./level3.qv\" ~> funcs, funcs.1 ~> f2, f2!")
        .expect("308"); // ((100 + 1) * 3) + 5 = 308
}

#[test]
fn test_partial_type_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "./types.qv".to_string(),
        "type ok = Ok[int]; type err = Err[int]; []".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(
            r#"type (ok, err) = %"./types.qv";
            #ok { ~> Ok[x] => [x, 2] ~> <multiply>! } ~> double,
            Ok[21] ~> double!"#,
        )
        .expect("42");
}

#[test]
fn test_star_type_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "./types.qv".to_string(),
        "type result = Ok[int] | Err[int]; []".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(
            r#"type * = %"./types.qv";
            #result { ~> Ok[x] => x | ~> Err[x] => 0 } ~> unwrap,
            Ok[42] ~> unwrap!"#,
        )
        .expect("42");
}
