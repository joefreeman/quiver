use quiver::Quiver;

#[test]
fn test_repl_variable_persistence() {
    let mut q = Quiver::new(None);

    // First evaluation: define a variable
    let result1 = q.evaluate("42 ~> =x", None, None, None);
    assert!(result1.is_ok());
    let (value1, _, vars1, _) = result1.unwrap();
    assert!(value1.is_some());

    // Second evaluation: use the variable
    let result2 = q.evaluate("x", None, Some(&vars1), None);
    assert!(result2.is_ok());
    let (value2, _, _, _) = result2.unwrap();
    assert!(value2.is_some());
}

#[test]
fn test_repl_multiple_variables() {
    let mut q = Quiver::new(None);

    // Define first variable
    let result1 = q.evaluate("10 ~> =a", None, None, None);
    assert!(result1.is_ok());
    let (_, _, vars1, _) = result1.unwrap();

    // Define second variable
    let result2 = q.evaluate("20 ~> =b", None, Some(&vars1), None);
    assert!(result2.is_ok());
    let (_, _, vars2, _) = result2.unwrap();

    // Use both variables
    let result3 = q.evaluate("[a, b]", None, Some(&vars2), None);
    assert!(result3.is_ok());
    let (value3, _, _, _) = result3.unwrap();
    assert!(value3.is_some());
}

#[test]
fn test_repl_process_persistence() {
    let mut q = Quiver::new(None);

    // Spawn a process and save the pid
    let result1 = q.evaluate("#{ [] } ~> =f, @f ~> =p", None, None, None);
    assert!(result1.is_ok());
    let (_, _, vars1, _) = result1.unwrap();

    // Verify we can reference the pid variable in a later evaluation
    // (even though the process itself may have terminated)
    let result2 = q.evaluate("p", None, Some(&vars1), None);
    assert!(result2.is_ok());
    let (value2, _, _, _) = result2.unwrap();
    assert!(value2.is_some());
}
