use crate::common::*;

// Stream resources as select sources: a socket or listener in `![...]` yields its
// next event (`'%tcp.event` / `'%tcp.listener_event`), racing it against messages
// and timeouts in one wait point. Backpressure is pull: a read is armed only while
// a select waits on the resource, and at most one event is buffered per resource.

#[test]
fn test_listener_and_socket_as_select_sources() {
    // Accept via `![l]`, then read the client's bytes via `![conn]` — both events
    // pattern-match the `%tcp` aliases (content-addressed runtime tuples).
    quiver()
        .with_io()
        .evaluate(
            r#"
            %tcp.listen [4293, 8] ~> =(+TcpListener & l)
            c = @[] {
              %tcp.connect [<7f000001>, 4293] ~> =(+TcpSocket & s)
              %tcp.write [s, "ping" ~> .0]
              !'int
            } []
            ![l] ~> =('%tcp.listener_event & ev)
            ev ~> =Accepted[listener: _, sock: conn]
            ![conn] ~> {
              | =Data[sock: _, data: d] => Str[d]
              | =Closed[sock: _] => WasClosed
            }
            "#,
        )
        .expect(r#""ping""#);
}

#[test]
fn test_stream_select_is_fallible() {
    // A stream source's select type carries nil beside its events: a failed read answers
    // the same `:error`-stamped nil every I/O operation does, so only a clean `Closed`
    // means the stream was seen whole.
    quiver()
        .with_io()
        .evaluate(r#"#+TcpSocket { ![$] }"#)
        .expect_type(
            "#+TcpSocket -> (Closed[sock: +TcpSocket] | Data[sock: +TcpSocket, data: 'bin] | [])",
        );
}

#[test]
fn test_socket_select_races_timeout() {
    // A silent peer: the timeout source wins and answers the `:timeout` nil.
    quiver()
        .with_io()
        .evaluate(
            r#"
            %tcp.listen [4294, 8] ~> =(+TcpListener & l)
            c = @[] { %tcp.connect [<7f000001>, 4294] ~> =(+TcpSocket & s); !'int } []
            ![l] ~> =Accepted[listener: _, sock: conn]
            ![conn, 50] ~> {
              | =Data[sock: _, data: _] => GotData
              | =Closed[sock: _] => GotClosed
              | TimedOut
            }
            "#,
        )
        .expect("TimedOut");
}

#[test]
fn test_socket_select_races_mailbox() {
    // A message already queued wins over a silent socket; after the peer sends, the
    // same socket's next select yields the bytes (via the armed read's stash).
    quiver()
        .with_io()
        .evaluate(
            r#"
            %tcp.listen [4295, 8] ~> =(+TcpListener & l)
            c = @[] {
              %tcp.connect [<7f000001>, 4295] ~> =(+TcpSocket & s)
              !'int ~> =1
              %tcp.write [s, "later" ~> .0]
              Ok
            } []
            ![l] ~> =Accepted[listener: _, sock: conn]
            me = @
            %proc.send [me, Hello]
            first = ![conn, #Hello] ~> {
              | =Hello => MailboxFirst
              | =Data[sock: _, data: _] => SocketFirst
              | Neither
            }
            %proc.send [c, 1]
            second = ![conn, 2000] ~> {
              | =Data[sock: _, data: d] => Str[d]
              | Other
            }
            [first, second]
            "#,
        )
        .expect(r#"[MailboxFirst, "later"]"#);
}

#[test]
fn test_socket_closed_event() {
    // The peer disconnecting yields `Closed` (EOF as an event, not an error).
    quiver()
        .with_io()
        .evaluate(
            r#"
            %tcp.listen [4296, 8] ~> =(+TcpListener & l)
            c = @[] {
              %tcp.connect [<7f000001>, 4296] ~> =(+TcpSocket & s)
              %tcp.close s
              Done
            } []
            ![l] ~> =Accepted[listener: _, sock: conn]
            !c
            ![conn, 2000] ~> {
              | =Data[sock: _, data: _] => GotData
              | =Closed[sock: _] => GotClosed
              | TimedOut
            }
            "#,
        )
        .expect("GotClosed");
}

#[test]
fn test_plain_read_consumes_stashed_event() {
    // Read/select interop: an event that arrived after its select was won by a
    // message is consumed by a subsequent plain read, in order.
    quiver()
        .with_io()
        .evaluate(
            r#"
            %tcp.listen [4297, 8] ~> =(+TcpListener & l)
            c = @[] {
              %tcp.connect [<7f000001>, 4297] ~> =(+TcpSocket & s)
              !'int ~> =1
              %tcp.write [s, "stash me" ~> .0]
              !'int
            } []
            ![l] ~> =Accepted[listener: _, sock: conn]
            // Arm the socket (no bytes exist yet), and let a queued message win.
            me = @
            %proc.send [me, Hi]
            ![conn, #Hi] ~> =Hi
            // Release the write; the armed read completes into the stash.
            %proc.send [c, 1]
            { ![150] | Ok }
            // The pull-read consumes the stashed event, in order.
            %tcp.read [conn, 8192] ~> Str[~]
            "#,
        )
        .expect(r#""stash me""#);
}

#[test]
fn test_non_stream_resource_is_rejected() {
    // File is random-access — no next event — so selecting on it is a compile error.
    quiver()
        .with_io()
        .evaluate(r#"__file_open__ ["/dev/null" ~> .0, 0, 0] ~> =(+File & f); ![f]"#)
        .expect_error_containing("not a stream");
}

#[test]
fn test_scoped_host_rejects_io_builtins_at_call_time() {
    // Signatures are universal, so io-referencing code compiles on any host — naming a
    // builtin, or merely holding a reference to one, costs nothing. The capability check
    // is at the call: a host that never attached the implementation errors there.
    quiver()
        .scoped_no_io()
        .evaluate(r#"f = __tcp_connect__; Ok"#)
        .expect("Ok");
    quiver()
        .scoped_no_io()
        .evaluate(r#"%tcp.connect [<7f000001>, 80]"#)
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "builtin __tcp_connect__ is not available on this host".to_string(),
        ));
}

#[test]
fn test_scoped_host_still_compiles_pure_code() {
    // The always-set (and pure std modules) work identically on a scoped host.
    quiver()
        .scoped_no_io()
        .evaluate(r#"[1, 2] ~> %num.add ~ ~> %str.from_int ~"#)
        .expect(r#""3""#);
    // Naming a resource TYPE needs no capability — only the builtins do.
    quiver()
        .scoped_no_io()
        .evaluate(r#"'h = +TcpSocket; Ok"#)
        .expect("Ok");
}

#[test]
fn test_system_only_host_runs_clocks_and_entropy() {
    // The web host's capability set. The system builtins are `Purity::HostRead` — read
    // synchronously in the worker — so attaching implementations is the whole of the host's
    // job: no effect backend, nothing parks, and `%time`/`%random` run unchanged.
    quiver()
        .scoped_system_only()
        .evaluate(r#"%time.now [] ~> %num.gt? [~, 1700000000000]"#)
        .expect("Ok");
    quiver()
        .scoped_system_only()
        .evaluate(r#"%random.hex 8 ~> =Str[b]; %bin.length b"#)
        .expect("16");
    // The pure half of the module composes over the host reading, as it does natively.
    quiver()
        .scoped_system_only()
        .evaluate(r#"0 ~> %time.iso8601 ~"#)
        .expect(r#""1970-01-01T00:00:00.000Z""#);
}

#[test]
fn test_system_only_host_still_rejects_file_and_network() {
    // The groups are separable: granting clocks and entropy grants nothing else, so a
    // socket or file call on this host still errors — at the call, naming the builtin.
    quiver()
        .scoped_system_only()
        .evaluate(r#"%tcp.connect [<7f000001>, 80]"#)
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "builtin __tcp_connect__ is not available on this host".to_string(),
        ));
    quiver()
        .scoped_system_only()
        .evaluate(r#"%fs.stat "/tmp""#)
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "builtin __filesystem_stat__ is not available on this host".to_string(),
        ));
}
