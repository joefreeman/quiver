use quiver::Quiver;
use quiver::vm::ProcessStatus;

#[test]
fn test_spawned_process_runs() {
    let mut q = Quiver::new(None);

    // Spawn a simple process that completes immediately
    let code = r#"
        #{ [] } ~> f,
        @f
    "#;

    let result = q.evaluate(code, None, None, None);
    if let Err(e) = &result {
        eprintln!("Error: {:?}", e);
    }
    assert!(result.is_ok());

    // Check processes - spawned process should have run and terminated
    let statuses = q.get_process_statuses();
    eprintln!("Process statuses after spawn: {:?}", statuses);

    // Should only have the REPL process (spawned process completed and terminated)
    assert_eq!(statuses.len(), 1);
    // REPL process (ID 0) should be sleeping (not running, queued, or waiting)
    let repl_status = statuses.values().next().unwrap();
    assert_eq!(*repl_status, ProcessStatus::Sleeping);
}
