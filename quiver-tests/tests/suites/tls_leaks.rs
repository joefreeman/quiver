use crate::common::tls_server::{Behaviour, TlsServer};
use crate::common::*;
use std::time::Duration;

// The regression this guards: a TLS-upgraded socket whose close — or whose failed
// handshake — leaves it in the backend's resource table holds its file descriptor until
// process teardown, which no correctness test can see. `/proc/self/fd` can. The test lives
// in a file of its own so no parallel test churns descriptors while it counts.

#[test]
fn test_repeated_tls_sessions_hold_no_descriptors() {
    let server = TlsServer::start(Behaviour::Echo);
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(Duration::from_secs(30))
        .evaluate(&server.program(
            r#"
            count_fds = #[] { %fs.list [%path.parse "/proc/self/fd"] ~> %iter.count ~ }

            cycle = #[] {
              %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
              %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__]
              %tcp.write [s, "ping" ~> .0]
              %tcp.read [s, 4]
              %tcp.close s
            }

            // A failed handshake must free the socket too: it consumed the handle, so
            // nothing else could ever close it. The refused attach answers nil, which the
            // block converts to Ok; an attach that unexpectedly *succeeds* fails the test.
            failed_cycle = #[] {
              %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
              { %tls.attach [socket: s, hostname: "localhost", roots: __DECOY__] => [] | Ok }
            }

            repeat = #[(n): 'int, (ok?): (Ok | No)] {
              | $n ~> =0 => Ok
              | { $ok? ~> =Ok => cycle | failed_cycle }; [%num.sub [$n, 1], $ok?] ~> ^ ~
            }

            // Warm up anything allocated lazily on first use — sockets, timers, the
            // directory walk itself — so the counted window holds steady state only.
            repeat [1, Ok]; repeat [1, No]; { ![10] | Ok }; count_fds

            before = count_fds
            repeat [10, Ok]
            repeat [10, No]
            { ![100] | Ok }        // let the server thread finish its last teardown
            after = count_fds
            { after ~> =^before => Ok | [before, after] }
            "#,
        ))
        .expect("Ok");
}
