use crate::common::tls_server::{Behaviour, TestPki, TlsServer};
use crate::common::*;
use std::time::Duration;

// `%tls` against real peers, without the internet. TLS upgrades a socket *in place*, so
// these exercise the ordinary `%tcp` operations — and selects — speaking plaintext through
// the encryption. Client-side tests talk to a rustls server on a loopback port
// (tests/common/tls_server.rs); server-side tests run both ends in Quiver. Certificate
// material arrives as spliced DER hex, trusted via `roots` — the parameter that exists
// exactly so TLS is testable against a locally-issued certificate.

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
    // sufficient for a full handshake against a certificate no default root set has ever
    // seen — and the upgraded socket answers to the ordinary %tcp operations.
    let server = TlsServer::start(Behaviour::Echo);
    tls(
        &server,
        r#"
        %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__]
        %io.write [s, "ping" ~> .0]
        ![s] ~> =Data[source: _, data: reply]
        %io.close s
        Str[reply]
        "#,
    )
    .expect(r#""ping""#);
}

#[test]
fn test_a_bare_tcp_close_fails_a_select() {
    // The peer vanishes without a close_notify. For a close-delimited protocol that bare
    // close is indistinguishable from a response cut short — the classic truncation
    // attack — so the read must answer the failure, nil carrying `:error`, rather than a
    // clean `Closed` that would pass off a cut-short stream as complete.
    let server = TlsServer::start(Behaviour::EchoThenVanish);
    tls(
        &server,
        r#"
        %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__]
        %io.write [s, "ping" ~> .0]
        ![s] ~> =Data[source: _, data: _]
        ![s] ~> :error<'%io> ~> =IoError(message: m)
        %str.contains? [m, "close_notify"]
        "#,
    )
    .expect("Ok");
}

#[test]
fn test_a_record_split_across_socket_reads_is_reassembled() {
    // The server dribbles one record's ciphertext a few bytes at a time, so a single
    // select spans several socket reads before a whole record exists to decrypt.
    let server = TlsServer::start(Behaviour::Dribble);
    tls(
        &server,
        r#"
        %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__]
        ![s] ~> =Data[source: _, data: ~] ~> Str[~]
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
        %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__]
        %io.read_all s ~> =('bin & all)
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
        %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
        %tls.attach [socket: s, hostname: "wrong.test", roots: __ROOTS__]
        ~> :error<'%io> ~> =IoError(message: m)
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
        %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
        %tls.attach [socket: s, hostname: "localhost", roots: __DECOY__]
        ~> :error<'%io> ~> =IoError(message: m)
        %str.contains? [m, "UnknownIssuer"]
        "#,
    )
    .expect("Ok")
    // The refused upgrade consumed the socket inside the backend, and ownership follows.
    .expect_open_resources(0);
}

#[test]
fn test_a_failed_handshake_closes_the_socket() {
    // A handshake that fails poisons the byte stream, so the upgrade consumes the socket
    // either way: after a refused attach the handle is dead, and using it is a fault. The
    // child process is the containment — the await answers a `:crash`-stamped nil.
    let server = TlsServer::start(Behaviour::Echo);
    tls(
        &server,
        r#"
        p = @[] {
          %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
          { %tls.attach [socket: s, hostname: "localhost", roots: __DECOY__] => [] | Ok }
          ![s]
        } []
        !p ~> :crash<(message: Str['bin])> ~> =(message: m)
        %str.contains? [m, "not found"]
        "#,
    )
    .expect("Ok");
}

#[test]
fn test_a_select_yields_decrypted_data_then_closed() {
    // The armed read completes with ciphertext; the event must carry plaintext. The clean
    // close that follows arrives as `Closed` — to a selecting server, a disconnect.
    let server = TlsServer::start(Behaviour::Echo);
    tls(
        &server,
        r#"
        %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__]
        %io.write [s, "ping" ~> .0]
        ![s] ~> =Data[source: _, data: d]
        ![s] ~> =Closed[source: _]
        Str[d]
        "#,
    )
    .expect(r#""ping""#);
}

#[test]
fn test_a_timeout_races_an_upgraded_socket() {
    // A silent peer: the select's timeout must win while the armed TLS read stays pending.
    let server = TlsServer::start(Behaviour::Silent);
    tls(
        &server,
        r#"
        %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
        %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__]
        ![s, 100] ~> :timeout<'int>
        "#,
    )
    .expect("100");
}

#[test]
fn test_tls_accept_serves_an_in_language_client() {
    // Both ends in Quiver: a server process listens, accepts and upgrades with
    // `%tls.accept`; the root process connects and upgrades with `%tls.attach`. The
    // handshake, echo and close all cross a real loopback socket.
    let pki = TestPki::new();
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(Duration::from_secs(10))
        .evaluate(&pki.splice(
            r#"
            server = @[] {
              %tcp.listen [4381, 8] ~> =(+TcpListener & l)
              ![l] ~> =Accepted[source: _, sock: c]
              %tls.accept [socket: c, cert: __CERT__, key: __KEY__]
              ![c] ~> =Data[source: _, data: msg]
              %io.write [c, msg]
              %io.close c
              Done
            } []
            // The server races the connect: retry (bounded) until its listener is up.
            connect = #'int {
              | %tcp.connect [<7f000001>, 4381] ~> =(+TcpSocket & c) => c
              | =0 => []
              | { ![50] | Ok }; n = %num.sub [$, 1]; ^ n
            }
            connect 20 ~> =(+TcpSocket & s)
            %tls.attach [socket: s, hostname: "localhost", roots: __ROOTS__]
            %io.write [s, "ping" ~> .0]
            ![s] ~> =Data[source: _, data: reply]
            %io.close s
            !server ~> =Done
            Str[reply]
            "#,
        ))
        .expect(r#""ping""#);
}

#[test]
fn test_https_serves_via_http_server() {
    // The whole point of the in-place upgrade: `%http/server` speaks HTTPS through the
    // `tls:` option with its pump untouched. The client side is a raw `%tls.connect` plus
    // hand-written HTTP/1.1 — the server's `connection: close` answer ends in a
    // close_notify (the pump's ordinary `%io.close`), so the read loop finishes cleanly.
    let pki = TestPki::new();
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(Duration::from_secs(10))
        .evaluate(&pki.splice(
            r#"
            handler = #'%http { %http/server.text "secure hello" }
            @[] {
              [port: 4382, handler: handler, tls: [cert: __CERT__, key: __KEY__]]
              ~> %http/server.serve
            } []
            // The server races the connect: retry (bounded) until it accepts and shakes
            // hands — a refused or half-up connection answers nil, and we go again.
            connect = #'int {
              | %tls.connect [host: "localhost", port: 4382, roots: __ROOTS__] ~> =(+TcpSocket & c) => c
              | =0 => []
              | { ![50] | Ok }; n = %num.sub [$, 1]; ^ n
            }
            connect 20 ~> =(+TcpSocket & s)
            %io.write [s, "GET / HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n" ~> .0]
            %io.read_all s ~> =('bin & resp)
            [
              %str.contains? [Str[resp], "200"],
              %str.contains? [Str[resp], "secure hello"],
            ]
            "#,
        ))
        .expect("[Ok, Ok]");
}

#[test]
fn test_pem_roots_reach_a_tls_server() {
    // The `%pem` → `%tls` seam: the other tests here splice roots in as DER hex, so this is
    // the only one proving that `certificates` output — real armor, decoded at run time — is
    // what `roots` takes. `std/docs/pem.md` checks the decoding itself over stand-in bodies;
    // only a live server can check that the bytes it produces are usable.
    let server = TlsServer::start(Behaviour::Echo);
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(Duration::from_secs(10))
        .evaluate(&server.program(
            r#"
            %pem.certificates "__CA_PEM__" ~> =('bin & roots)
            %tcp.connect [<7f000001>, __PORT__] ~> =(+TcpSocket & s)
            %tls.attach [socket: s, hostname: "localhost", roots: roots]
            %io.write [s, "ping" ~> .0]
            ![s] ~> =Data[source: _, data: ~] ~> Str[~]
            "#,
        ))
        .expect(r#""ping""#);
}
