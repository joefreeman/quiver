mod common;
use common::*;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::Duration;

#[test]
fn test_tcp_client_connect_and_write() {
    // Start a simple echo server
    let listener = TcpListener::bind("127.0.0.1:0").expect("Failed to bind");
    let addr = listener.local_addr().unwrap();
    let port = addr.port();

    let running = Arc::new(AtomicBool::new(true));
    let running_clone = running.clone();

    thread::spawn(move || {
        while running_clone.load(Ordering::Relaxed) {
            if let Ok((mut stream, _)) = listener.accept() {
                thread::spawn(move || {
                    let mut buf = [0u8; 1024];
                    if let Ok(n) = stream.read(&mut buf) {
                        let _ = stream.write_all(&buf[..n]);
                    }
                });
            }
        }
    });

    // Give server time to start
    thread::sleep(Duration::from_millis(50));

    // Connect from Quiver and send data
    // 0x7f000001 is 127.0.0.1 as raw bytes
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            socket = [0x7f000001, {}] __tcp_connect__,
            [socket, "Hello from Quiver!" .0] __tcp_socket_write__,
            response = [socket, 4096] __tcp_socket_read__,
            socket __tcp_socket_close__,
            Str[response]
        "#,
            port
        ))
        .expect("\"Hello from Quiver!\"");

    running.store(false, Ordering::Relaxed);
    thread::sleep(Duration::from_millis(50));
}

#[test]
fn test_tcp_server_accept_and_respond() {
    let listener = TcpListener::bind("127.0.0.1:0").expect("Failed to bind");
    let addr = listener.local_addr().unwrap();
    let test_port = addr.port();
    drop(listener); // Release the port for Quiver to use

    // Start Quiver server in a separate thread
    let server_handle = thread::spawn(move || {
        quiver()
            .with_io()
            .evaluate(&format!(
                r#"
                listener = [{}, 10] __tcp_listen__,

                // Accept one connection and echo
                client = listener __tcp_listener_accept__,
                request = [client, 4096] __tcp_socket_read__,
                [client, request] __tcp_socket_write__,
                client __tcp_socket_close__,
                listener __tcp_listener_close__,

                Ok
                "#,
                test_port
            ))
            .expect("Ok");
    });

    // Give server time to start
    thread::sleep(Duration::from_millis(100));

    // Connect from Rust
    let mut stream = TcpStream::connect(format!("127.0.0.1:{}", test_port))
        .expect("Failed to connect to Quiver server");
    stream
        .write_all(b"Hello from Rust!")
        .expect("Failed to write");

    let mut buf = [0u8; 1024];
    let n = stream.read(&mut buf).expect("Failed to read");
    let response = String::from_utf8_lossy(&buf[..n]);

    assert_eq!(response, "Hello from Rust!");

    // Wait for server to finish
    server_handle.join().unwrap();
}

#[test]
fn test_tcp_multiple_connections() {
    // Start echo server
    let listener = TcpListener::bind("127.0.0.1:0").expect("Failed to bind");
    let addr = listener.local_addr().unwrap();
    let port = addr.port();

    let running = Arc::new(AtomicBool::new(true));
    let running_clone = running.clone();

    thread::spawn(move || {
        while running_clone.load(Ordering::Relaxed) {
            if let Ok((mut stream, _)) = listener.accept() {
                thread::spawn(move || {
                    let mut buf = [0u8; 1024];
                    if let Ok(n) = stream.read(&mut buf) {
                        let _ = stream.write_all(&buf[..n]);
                    }
                });
            }
        }
    });

    thread::sleep(Duration::from_millis(50));

    // Make multiple connections from Quiver
    // 0x7f000001 is 127.0.0.1 as raw bytes
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            // First connection
            socket1 = [0x7f000001, {}] __tcp_connect__,
            [socket1, "First" .0] __tcp_socket_write__,
            response1 = [socket1, 4096] __tcp_socket_read__,
            socket1 __tcp_socket_close__,

            // Second connection
            socket2 = [0x7f000001, {}] __tcp_connect__,
            [socket2, "Second" .0] __tcp_socket_write__,
            response2 = [socket2, 4096] __tcp_socket_read__,
            socket2 __tcp_socket_close__,

            [Str[response1], Str[response2]]
        "#,
            port, port
        ))
        .expect("[\"First\", \"Second\"]");

    running.store(false, Ordering::Relaxed);
    thread::sleep(Duration::from_millis(50));
}

#[test]
fn test_socket_type_checking() {
    // Test that socket operations have correct type signatures
    quiver()
        .with_io()
        .evaluate(
            r#"
            // Function that handles a socket
            handle_socket = #\TcpSocket {
                =s,
                data = [s, 4096] __tcp_socket_read__,
                [s, data] __tcp_socket_write__,
                s __tcp_socket_close__
            },

            // Function that handles a listener
            handle_listener = #\TcpListener {
                =l,
                client = l __tcp_listener_accept__,
                client __tcp_socket_close__,
                l __tcp_listener_close__
            },

            // Should type check
            []
        "#,
        )
        .expect("[]");
}

#[test]
fn test_binary_data_over_socket() {
    // Start echo server
    let listener = TcpListener::bind("127.0.0.1:0").expect("Failed to bind");
    let addr = listener.local_addr().unwrap();
    let port = addr.port();

    let running = Arc::new(AtomicBool::new(true));
    let running_clone = running.clone();

    thread::spawn(move || {
        while running_clone.load(Ordering::Relaxed) {
            if let Ok((mut stream, _)) = listener.accept() {
                thread::spawn(move || {
                    let mut buf = [0u8; 1024];
                    if let Ok(n) = stream.read(&mut buf) {
                        let _ = stream.write_all(&buf[..n]);
                    }
                });
            }
        }
    });

    thread::sleep(Duration::from_millis(50));

    // Send binary data
    // 0x7f000001 is 127.0.0.1 as raw bytes
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
            socket = [0x7f000001, {}] __tcp_connect__,
            [socket, 0xdeadbeef] __tcp_socket_write__,
            response = [socket, 4096] __tcp_socket_read__,
            socket __tcp_socket_close__,
            response
        "#,
            port
        ))
        .expect("0xdeadbeef");

    running.store(false, Ordering::Relaxed);
    thread::sleep(Duration::from_millis(50));
}

#[test]
fn test_write_to_closed_socket() {
    // Start a simple echo server
    let listener = TcpListener::bind("127.0.0.1:0").expect("Failed to bind");
    let addr = listener.local_addr().unwrap();
    let port = addr.port();

    thread::spawn(move || {
        if let Ok((mut stream, _)) = listener.accept() {
            let mut buf = [0u8; 1024];
            let _ = stream.read(&mut buf);
            // Don't respond, just close
        }
    });

    thread::sleep(Duration::from_millis(50));

    // Connect, close, then try to write - should fail with runtime error
    // 0x7f000001 is 127.0.0.1 as raw bytes
    quiver()
        .with_io()
        .evaluate(&format!(
            r#"
        socket = [0x7f000001, {}] __tcp_connect__,
        socket __tcp_socket_close__,
        [socket, "test" .0] __tcp_socket_write__
    "#,
            port
        ))
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Effect operation failed: InvalidArgument(\"Resource 1 not found\")".to_string(),
        ));
}
