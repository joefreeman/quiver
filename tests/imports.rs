mod common;
use common::*;
use quiver::vm::Value;
use std::collections::HashMap;

#[test]
fn test_simple_module_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "math".to_string(),
        "add = #[int, int] { [$0, $1] ~> + }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import math; [3, 4] ~> math.add")
        .expect_value(Value::Integer(7));
}

#[test]
fn test_specific_function_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "utils".to_string(),
        "double = #int { [$, 2] ~> * }; triple = #int { [$, 3] ~> * }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("from utils import double; 5 ~> double")
        .expect_value(Value::Integer(10));
}

#[test]
fn test_multiple_specific_imports() {
    let mut modules = HashMap::new();
    modules.insert(
        "ops".to_string(),
        "add1 = #int { [$, 1] ~> + }; sub1 = #int { [$, 1] ~> - }; mul2 = #int { [$, 2] ~> * }"
            .to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("from ops import add1, mul2; 3 ~> add1 ~> mul2")
        .expect_value(Value::Integer(8));
}

#[test]
fn test_aliased_module_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "mathematics".to_string(),
        "PI = 3; square = #int { [$, $] ~> * }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import mathematics as math; 4 ~> math.square")
        .expect_value(Value::Integer(16));
}

#[test]
fn test_aliased_function_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "geometry".to_string(),
        "area_of_square = #int { [$, $] ~> * }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("from geometry import area_of_square as square_area; 5 ~> square_area")
        .expect_value(Value::Integer(25));
}

#[test]
fn test_nested_module_access() {
    let mut modules = HashMap::new();
    modules.insert(
        "math".to_string(),
        "ops = [add: #[int, int] { [$0, $1] ~> + }, multiply: #[int, int] { [$0, $1] ~> * }]"
            .to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import math; [6, 7] ~> math.ops.add")
        .expect_value(Value::Integer(13));
}

#[test]
fn test_module_constants() {
    let mut modules = HashMap::new();
    modules.insert(
        "constants".to_string(),
        "MAX_SIZE = 100; MIN_SIZE = 1; DEFAULT_VALUE = 42".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import constants; constants.DEFAULT_VALUE")
        .expect_value(Value::Integer(42));
}

#[test]
fn test_module_with_types() {
    let mut modules = HashMap::new();
    modules.insert(
        "shapes".to_string(),
        "make_point = #[int, int] { Point[x: $0, y: $1] }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import shapes; [10, 20] ~> shapes.make_point")
        .expect_value(Value::Tuple(
            quiver::bytecode::TypeId::NIL,
            vec![Value::Integer(10), Value::Integer(20)],
        ));
}

#[test]
fn test_recursive_module_functions() {
    let mut modules = HashMap::new();
    modules.insert(
        "recursion".to_string(),
        "factorial = #int { [$, 1] ~> <= => 1 | [$, [$, 1] ~> - ~> &] ~> * }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import recursion; 5 ~> recursion.factorial")
        .expect_value(Value::Integer(120));
}

#[test]
fn test_module_with_conditional_functions() {
    let mut modules = HashMap::new();
    modules.insert("logic".to_string(), "max = #[int, int] { [$0, $1] ~> >= => $0 | $1 }; min = #[int, int] { [$0, $1] ~> <= => $0 | $1 }".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("import logic; [[5, 8] ~> logic.max, [3, 7] ~> logic.min] ~> +")
        .expect_value(Value::Integer(11));
}

#[test]
fn test_multiple_module_imports() {
    let mut modules = HashMap::new();
    modules.insert(
        "math".to_string(),
        "add = #[int, int] { [$0, $1] ~> + }".to_string(),
    );
    modules.insert(
        "string_ops".to_string(),
        "length = #string { 5 }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import math; import string_ops; [10, 15] ~> math.add")
        .expect_value(Value::Integer(25));
}

#[test]
fn test_mixed_import_styles() {
    let mut modules = HashMap::new();
    modules.insert("toolkit".to_string(), "double = #int { [$, 2] ~> * }; triple = #int { [$, 3] ~> * }; quadruple = #int { [$, 4] ~> * }".to_string());

    quiver()
        .with_modules(modules)
        .evaluate(
            "import toolkit; from toolkit import double; [5 ~> double, 5 ~> toolkit.triple] ~> +",
        )
        .expect_value(Value::Integer(25));
}

#[test]
fn test_import_with_local_definitions() {
    let mut modules = HashMap::new();
    modules.insert(
        "external".to_string(),
        "external_func = #int { [$, 10] ~> + }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import external; local_func = #int { [$, 5] ~> * }; 3 ~> external.external_func ~> local_func")
        .expect_value(Value::Integer(65));
}

#[test]
fn test_import_shadowing() {
    let mut modules = HashMap::new();
    modules.insert("shadow_test".to_string(), "value = 100".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("value = 50; import shadow_test; value")
        .expect_value(Value::Integer(50));
}

#[test]
fn test_imported_function_in_higher_order() {
    let mut modules = HashMap::new();
    modules.insert(
        "functors".to_string(),
        "increment = #int { [$, 1] ~> + }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import functors; apply_twice = #[fn, int] { $1 ~> $0 ~> $0 }; [f: functors.increment, x: 5] ~> apply_twice")
        .expect_value(Value::Integer(7));
}

#[test]
fn test_circular_import_prevention() {
    let mut modules = HashMap::new();
    modules.insert(
        "circular_a".to_string(),
        "import circular_b; func_a = #int { $ ~> circular_b.func_b }".to_string(),
    );
    modules.insert(
        "circular_b".to_string(),
        "import circular_a; func_b = #int { $ }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import circular_a; 5 ~> circular_a.func_a")
        .expect_error();
}

#[test]
fn test_nonexistent_module_import() {
    quiver()
        .evaluate("import nonexistent_module; 42")
        .expect_error();
}

#[test]
fn test_nonexistent_function_import() {
    let mut modules = HashMap::new();
    modules.insert(
        "real_module".to_string(),
        "real_function = #int { $ }".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("from real_module import fake_function; 42")
        .expect_error();
}

#[test]
fn test_import_from_nonexistent_module() {
    quiver()
        .evaluate("from fake_module import some_function; 42")
        .expect_error();
}

#[test]
fn test_module_with_complex_data_structures() {
    let mut modules = HashMap::new();
    modules.insert(
        "data".to_string(),
        "complex_data = [users: [[name: \"Alice\", id: 1], [name: \"Bob\", id: 2]], count: 2]"
            .to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import data; data.complex_data.count")
        .expect_value(Value::Integer(2));
}

#[test]
fn test_module_export_patterns() {
    let mut modules = HashMap::new();
    modules.insert("patterns".to_string(), "Point[x: default_x, y: default_y] = Point[x: 0, y: 0]; get_origin = #{ [default_x, default_y] }".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("import patterns; [] ~> patterns.get_origin")
        .expect_value(Value::Tuple(
            quiver::bytecode::TypeId::NIL,
            vec![Value::Integer(0), Value::Integer(0)],
        ));
}

#[test]
fn test_deeply_nested_module_access() {
    let mut modules = HashMap::new();
    modules.insert(
        "deep".to_string(),
        "level1 = [level2: [level3: [value: 42]]]".to_string(),
    );

    quiver()
        .with_modules(modules)
        .evaluate("import deep; deep.level1.level2.level3.value")
        .expect_value(Value::Integer(42));
}

#[test]
fn test_module_with_conditional_exports() {
    let mut modules = HashMap::new();
    modules.insert("conditional".to_string(), "is_positive = #int { [$, 0] ~> > }; safe_divide = #[int, int] { [$1, 0] ~> != => [$0, $1] ~> / | 0 }".to_string());

    quiver()
        .with_modules(modules)
        .evaluate("import conditional; [[10, 2] ~> conditional.safe_divide, [-5] ~> conditional.is_positive] ~> +")
        .expect_value(Value::Integer(5));
}
