#[test]
fn test_repl_variable_persistence() {
    let mut repl = quiver::repl::ReplCli::with_modules(None).expect("Failed to create REPL");

    // First evaluation: define a variable
    let result1 = repl.evaluate_for_test("42 ~> =x");
    assert!(result1.is_ok());
    let value1 = result1.unwrap();
    assert!(value1.is_some());

    // Second evaluation: use the variable
    let result2 = repl.evaluate_for_test("x");
    assert!(result2.is_ok());
    let value2 = result2.unwrap();
    assert!(value2.is_some());
}

#[test]
fn test_repl_multiple_variables() {
    let mut repl = quiver::repl::ReplCli::with_modules(None).expect("Failed to create REPL");

    // Define first variable
    let result1 = repl.evaluate_for_test("10 ~> =a");
    assert!(result1.is_ok());

    // Define second variable
    let result2 = repl.evaluate_for_test("20 ~> =b");
    assert!(result2.is_ok());

    // Use both variables
    let result3 = repl.evaluate_for_test("[a, b]");
    assert!(result3.is_ok());
    let value3 = result3.unwrap();
    assert!(value3.is_some());
}

#[test]
fn test_repl_process_persistence() {
    let mut repl = quiver::repl::ReplCli::with_modules(None).expect("Failed to create REPL");

    // Spawn a process and save the pid
    let result1 = repl.evaluate_for_test("#{ [] } ~> =f, @f ~> =p");
    assert!(result1.is_ok());

    // Verify we can reference the pid variable in a later evaluation
    // (even though the process itself may have terminated)
    let result2 = repl.evaluate_for_test("p");
    assert!(result2.is_ok());
    let value2 = result2.unwrap();
    assert!(value2.is_some());
}
