use quiver::Quiver;

#[test]
fn test_self_reference() {
    let mut q = Quiver::new(None);

    // Test that . (self reference) parses and compiles
    let result = q.evaluate(".", None, None, None);

    // Should succeed in evaluation (returns a Pid)
    assert!(result.is_ok());
    let (value, _) = result.unwrap();
    assert!(value.is_some());
}

#[test]
fn test_spawn_simple_function() {
    let mut q = Quiver::new(None);

    // Define a simple function and spawn it
    let code = "#{ [] } ~> f, @f";
    let result = q.evaluate(code, None, None, None);

    // Should succeed
    assert!(result.is_ok());
    let (value, _) = result.unwrap();
    // Should return a Pid
    assert!(value.is_some());
}

#[test]
fn test_send_to_process() {
    let mut q = Quiver::new(None);

    // Spawn a process and send it a message
    let code = r#"
        #{ [] } ~> f,
        @f ~> p,
        42 ~> p$
    "#;

    let result = q.evaluate(code, None, None, None);

    // Should succeed (send returns nil)
    if let Err(e) = &result {
        eprintln!("Error: {:?}", e);
    }
    assert!(result.is_ok());
}
