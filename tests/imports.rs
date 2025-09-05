mod common;
use common::*;
use std::collections::HashMap;

#[test]
fn test_module_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "./math.qv".to_string(),
        "add = #[int, int] { [$0, $1] ~> + }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("math = %\"./math.qv\", [1, 2] ~> math.add")
        .expect_int(3);
}

#[test]
fn test_destructured_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "./math.qv".to_string(),
        "add = #[int, int] { [$0, $1] ~> + }, sub = #[int, int] { [$0, $1] ~> - }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("(add, sub) = %\"./math.qv\", [3, 4] ~> add ~> [~, 2] ~> sub")
        .expect_int(5);
}

#[test]
fn test_star_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "./math.qv".to_string(),
        "add = #[int, int] { [$0, $1] ~> + }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("* = %\"./math.qv\", [3, 4] ~> add")
        .expect_int(7);
}
