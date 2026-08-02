mod common;
use common::tls_server::{Behaviour, TlsServer};
use common::*;
use std::time::Duration;

// `%tls` against a real peer, without the internet: each test starts a rustls server on a
// loopback port (tests/common/tls_server.rs) with a certificate minted for the run, and hands
// the client its issuing CA through `roots` — the parameter that exists exactly so TLS is
// testable against a locally-issued certificate. What a live remote server cannot do
// reliably — close without a close_notify, dribble a record across many socket reads — the
// local one does deterministically.

fn tls(server: &TlsServer, source: &str) -> TestResult {
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(Duration::from_secs(10))
        .evaluate(&server.program(source))
}

#[test]
fn test_a_locally_issued_certificate_is_trusted_via_roots() {
    // The positive counterpart of the refusal tests: anchors passed as DER bytes are
    // sufficient for a full handshake, echo, and close against a certificate no default
    // root set has ever seen.
    let server = TlsServer::start(Behaviour::Echo);
    tls(
        &server,
        r#"
        %tcp.connect [0x7f000001, __PORT__] ~> =(\TcpSocket)s
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__] ~> =(\TlsSocket)t
        %tls.write [t, "ping" ~> .0]
        %tls.read [t, 4] ~> =('bin)reply
        %tls.close t
        Str[reply]
        "#,
    )
    .expect(r#""ping""#);
}

#[test]
fn test_a_close_notify_reads_as_a_clean_end_of_stream() {
    let server = TlsServer::start(Behaviour::Echo);
    tls(
        &server,
        r#"
        %tcp.connect [0x7f000001, __PORT__] ~> =(\TcpSocket)s
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__] ~> =(\TlsSocket)t
        %tls.write [t, "ping" ~> .0]
        %tls.read [t, 4] ~> =('bin)reply
        after = %tls.read [t, 4]
        [Str[reply], after]
        "#,
    )
    .expect(r#"["ping", 0x]"#);
}

#[test]
fn test_a_bare_tcp_close_is_an_error_not_an_end() {
    // The peer vanishes without a close_notify. For a close-delimited protocol that bare
    // close is indistinguishable from a response cut short — the classic truncation
    // attack — so it must surface as an error, never as a clean end of stream.
    let server = TlsServer::start(Behaviour::EchoThenVanish);
    tls(
        &server,
        r#"
        %tcp.connect [0x7f000001, __PORT__] ~> =(\TcpSocket)s
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__] ~> =(\TlsSocket)t
        %tls.write [t, "ping" ~> .0]
        %tls.read [t, 4] ~> =('bin)reply
        %tls.read [t, 4] ~> :('%io)error ~> =IoError(message: m)
        %str.contains? [m, "close_notify"]
        "#,
    )
    .expect("Ok");
}

#[test]
fn test_a_record_split_across_socket_reads_is_reassembled() {
    // The server dribbles one record's ciphertext a few bytes at a time, so a single
    // `%tls.read` spans several socket reads before a whole record exists to decrypt.
    let server = TlsServer::start(Behaviour::Dribble);
    tls(
        &server,
        r#"
        %tcp.connect [0x7f000001, __PORT__] ~> =(\TcpSocket)s
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__] ~> =(\TlsSocket)t
        %tls.read [t, 16] ~> Str[~]
        "#,
    )
    .expect(r#""drip""#);
}

#[test]
fn test_records_spanning_read_boundaries_arrive_complete() {
    // 100KB spans several TLS records, so 16KB socket reads keep landing mid-record and
    // mid-buffer — the case where dropping a read's remainder would silently lose records.
    let server = TlsServer::start(Behaviour::Send { len: 100_000 });
    tls(
        &server,
        r#"
        read_all = #[(sock): \TlsSocket, (buf): 'bin] {
          %tls.read [$sock, 65536] ~> =('bin)chunk
          {
            | %bin.length chunk ~> =0 => $buf
            | ^ [$sock, %bin.concat [$buf, chunk]]
          }
        }
        %tcp.connect [0x7f000001, __PORT__] ~> =(\TcpSocket)s
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__] ~> =(\TlsSocket)t
        read_all [t, 0x] ~> =('bin)all
        %bin.length all
        "#,
    )
    .expect("100000");
}

#[test]
fn test_a_mismatched_hostname_is_refused() {
    // A certificate for "localhost" must not authenticate a connection asked for under
    // another name — this drives a real mid-handshake verification failure, unlike the
    // unusable-anchors case, which fails before the handshake starts.
    let server = TlsServer::start(Behaviour::Echo);
    tls(
        &server,
        r#"
        %tcp.connect [0x7f000001, __PORT__] ~> =(\TcpSocket)s
        %tls.attach [socket: s, hostname: "wrong.test", roots: __ROOTS__]
        ~> :('%io)error ~> =IoError(message: m)
        %str.contains? [m, "not valid for name"]
        "#,
    )
    .expect("Ok");
}

#[test]
fn test_an_unknown_issuer_is_refused() {
    // The decoy roots are perfectly valid trust anchors — they just never issued the
    // server's certificate.
    let server = TlsServer::start(Behaviour::Echo);
    tls(
        &server,
        r#"
        %tcp.connect [0x7f000001, __PORT__] ~> =(\TcpSocket)s
        %tls.attach [socket: s, hostname: "localhost", roots: __DECOY__]
        ~> :('%io)error ~> =IoError(message: m)
        %str.contains? [m, "UnknownIssuer"]
        "#,
    )
    .expect("Ok");
}

#[test]
fn test_attach_consumes_the_socket() {
    // After a successful attach only the TLS handle may be used; the plain-socket handle is
    // gone, and using it is a fault. The child process is the containment: the fault kills
    // it, and the await answers a `:crash`-stamped nil.
    let server = TlsServer::start(Behaviour::Echo);
    tls(
        &server,
        r#"
        p = @{
          %tcp.connect [0x7f000001, __PORT__] ~> =(\TcpSocket)s
          %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__] ~> =(\TlsSocket)t
          %tcp.read [s, 1]
        }
        !p ~> :((message: Str['bin]))crash ~> =(message: m)
        %str.contains? [m, "not found"]
        "#,
    )
    .expect("Ok");
}

#[test]
fn test_selecting_on_a_tls_socket_is_a_compile_error() {
    // A TLS socket is not a stream source (yet): an armed read would complete with
    // ciphertext, so until the backend can decrypt on that path, `![t]` must fail to
    // compile rather than fault at runtime.
    quiver()
        .with_io()
        .evaluate(r#"f = #\TlsSocket { t = $; ![t] }; Ok"#)
        .expect_error_containing("not a stream");
}
