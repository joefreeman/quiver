mod common;
use common::*;

// `%http/client` and `%http/transport` — the whole client stack.
//
// The transport is picked up by import, so no test names one. Instead the *OS* is faked
// (`with_mock_io`): the client, the transport, the codecs and the effect plumbing all run for
// real, and only the syscalls are canned. That reaches what a live socket cannot do
// reliably — a response split across awkward reads, a peer that never answers, a refused
// connect, an aborting operation — while the live-server test at the bottom keeps the real
// thing honest.

/// A canned HTTP response, delivered as one read.
fn serves(response: &str) -> MockIo {
    MockIo::Serves(vec![response.as_bytes().to_vec()])
}

#[test]
fn test_client_reaches_a_server_and_parses_its_answer() {
    quiver()
        .with_mock_io(serves("HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nhi"))
        .evaluate(
            r#"%http/client.get "http://example.com/a?x=1" ~> =('%http.response)r
               [r.status, Str[r.body]]"#,
        )
        .expect(r#"[200, "hi"]"#);
}

#[test]
fn test_response_is_assembled_across_arbitrary_read_boundaries() {
    // Reads land where the network decides. A response split mid-status-line, mid-header and
    // mid-chunk must still assemble — the case a live server will not reproduce on demand.
    quiver()
        .with_mock_io(MockIo::Serves(vec![
            b"HTTP/1.1 200 ".to_vec(),
            b"OK\r\ntransfer-enc".to_vec(),
            b"oding: chunked\r\n\r\n5\r\nhel".to_vec(),
            b"lo\r\n6\r\n wor".to_vec(),
            b"ld\r\n0\r\n\r\n".to_vec(),
        ]))
        .evaluate(r#"%http/client.get "http://e.com/" ~> =('%http.response)r; Str[r.body]"#)
        .expect(r#""hello world""#);
}

#[test]
fn test_post_carries_its_body_and_method() {
    quiver()
        .with_mock_io(serves("HTTP/1.1 201 Created\r\ncontent-length: 0\r\n\r\n"))
        .evaluate(
            r#"%http/client.post [url: "http://e.com/submit", body: "payload" ~> .0]
               ~> =('%http.response)r
               r.status"#,
        )
        .expect("201");
}

#[test]
fn test_redirect_budget_answers_the_redirect_rather_than_failing() {
    // Every read answers the same 302, so following is capped by the budget. Exhausting it is
    // not an error: the caller gets the redirect and can see where it was being sent, which
    // failing would hide.
    quiver()
        .with_mock_io(serves(
            "HTTP/1.1 302 Found\r\nlocation: http://other.org/z\r\ncontent-length: 0\r\n\r\n",
        ))
        .evaluate(
            r#"%http/client.request [url: "http://example.com/a", redirects: 2]
               ~> =('%http.response)r
               [r.status, %http.header [r.headers, "location"]]"#,
        )
        .expect(r#"[302, "http://other.org/z"]"#);
}

#[test]
fn test_a_refused_connection_is_a_value_the_caller_can_read() {
    quiver()
        .with_mock_io(MockIo::Refuses)
        .evaluate(r#"%http/client.get "http://e.com/" ~> :('%io)error ~> =IoError(kind: k); k"#)
        .expect("ConnectionRefused");
}

#[test]
fn test_connect_falls_through_to_an_address_that_accepts() {
    // The mock's resolver answers `::1` before `127.0.0.1` — an ordering real resolvers do
    // produce for localhost — and nothing is bound on IPv6, so *every* mock test crosses a
    // refused first address. This one names the property: taking the resolver's first
    // answer on faith was the bug, and the transport must try the next address instead.
    quiver()
        .with_mock_io(serves("HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nhi"))
        .evaluate(r#"%http/client.get "http://dual.test/" ~> =('%http.response)r; r.status"#)
        .expect("200");
}

#[test]
fn test_a_silent_peer_times_out_rather_than_hanging() {
    // The read never completes, so the request process stays parked. A parked effect is not a
    // select source, which is exactly why the client wraps each request in a process: the
    // await is the only thing that can be raced against a clock.
    quiver()
        .with_mock_io(MockIo::Stalls)
        .evaluate(r#"%http/client.request [url: "http://e.com/", timeout: 50] ~> :('int)timeout"#)
        .expect("50");
}

#[test]
fn test_an_aborting_operation_is_caught_at_the_request_process() {
    // The process boundary is what makes this survivable: the abort takes down the request's
    // own process, and the await answers a `:crash`-stamped nil instead of killing the caller.
    quiver()
        .with_mock_io(MockIo::Aborts)
        .evaluate(
            r#"%http/client.get "http://e.com/" ~> :((message: Str['bin]))crash ~> =(message: m)
               %str.contains? [m, "the read aborted"]"#,
        )
        .expect("Ok");
}

#[test]
fn test_a_truncated_response_is_reported_not_silently_short() {
    // The peer hangs up mid-body. Answering the partial bytes would be the worst outcome, so
    // end-of-connection with an incomplete parse is an error.
    quiver()
        .with_mock_io(serves(
            "HTTP/1.1 200 OK\r\ncontent-length: 100\r\n\r\nshort",
        ))
        .evaluate(r#"%http/client.get "http://e.com/" ~> :('%io)error ~> =IoError(message: m); m"#)
        .expect(r#""connection closed mid-response""#);
}

#[test]
fn test_a_malformed_url_fails_before_any_transport_call() {
    quiver()
        .with_mock_io(serves("HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n"))
        .evaluate(r#"%http/client.get "not a url""#)
        .expect("[]");
}

#[test]
fn test_https_reaches_a_real_server() {
    // TLS end to end: resolve, connect, upgrade, request, decrypt, parse — the same
    // `%http/client` call as plain HTTP, with only the scheme different.
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(std::time::Duration::from_secs(30))
        .evaluate(
            r#"%http/client.get "https://example.com/" ~> =Response(status: s, body: b)
               [s, %str.contains? [Str[b], "Example Domain"]]"#,
        )
        .expect("[200, Ok]");
}

#[test]
fn test_an_untrusted_anchor_set_is_refused() {
    // The `roots` parameter is what makes verification configurable — and testable. Bytes
    // that are not certificates yield no anchors, and a connection that cannot be verified
    // is refused rather than made.
    quiver()
        .with_io()
        .with_real_time()
        .evaluate(
            r#""example.com" ~> %dns.resolve ~> %iter.nth [~, 0]
               ~> { | =IPv4[b] => b | =IPv6[b] => b } ~> =('bin)ip
               %tcp.connect [ip, 443] ~> =(\TcpSocket)s
               %tls.attach [socket: s, hostname: "example.com", roots: 0xdeadbeef]
               ~> :('%io)error ~> =IoError(message: m); m"#,
        )
        .expect(r#""tls: no usable trust anchors in the supplied roots""#);
}

// --- and the real thing ------------------------------------------------------------------

#[test]
fn test_client_over_a_real_socket_against_a_real_server() {
    // Both halves in Quiver, both ends of an actual socket: `%http/client` resolves, connects,
    // serializes, reads and parses against `%http/server` in the same runtime. The mocked
    // tests above are sharper; this one is the proof they are testing the right thing.
    quiver()
        .with_io()
        .with_real_time()
        .evaluate(
            r#"
            handler = #'%http {
              [$method, $path] ~> {
                | =[GET, Cons["echo", Cons[what, Nil]]] => %http/server.text what
                | =[POST, Cons["upper", Nil]] => Str[$body] ~> %http/server.text
                | =[GET, Cons["moved", Nil]] => %http/server.redirect "/echo/there"
                | %http/server.not_found
              }
            }
            @{ [port: 4291, handler: &handler] ~> %http/server.serve }
            { ![100] | Ok }

            %http/client.get "http://127.0.0.1:4291/echo/hello" ~> =('%http.response)a
            %http/client.post [url: "http://127.0.0.1:4291/upper", body: "sent" ~> .0]
            ~> =('%http.response)b

            %http/client.get "http://127.0.0.1:4291/nope" ~> =('%http.response)c
            // A 303 from the server is followed automatically, ending at the echo route.
            %http/client.get "http://127.0.0.1:4291/moved" ~> =('%http.response)d

            [[a.status, Str[a.body]], [b.status, Str[b.body]], c.status, [d.status, Str[d.body]]]
            "#,
        )
        .expect(r#"[[200, "hello"], [200, "sent"], 404, [200, "there"]]"#);
}

#[test]
fn test_a_real_refused_connection() {
    quiver()
        .with_io()
        .with_real_time()
        .evaluate(
            r#"%http/client.get "http://127.0.0.1:9/x" ~> :('%io)error ~> =IoError(kind: k); k"#,
        )
        .expect("ConnectionRefused");
}

// --- the web variant, type-checked from a native build -----------------------------------
//
// This is the coverage that replaces an interface-enforcement mechanism. Nothing else in the
// repository compiles `transport.web.qv`: a browser would, but no test runs in one. The
// signature/implementation split exists exactly so a host can type-check code it cannot
// execute, so a *native* test registers the browser's capability set — system builtins plus
// `fetch`, signatures only — claims the web tag, and compiles the whole client stack against
// it. Divergence between the two transports fails here rather than in a browser.

#[test]
fn test_the_web_variant_compiles_against_the_browser_capability_set() {
    quiver()
        .scoped_web()
        .evaluate(r#"%http/transport ~> =(request: _); Ok"#)
        .expect("Ok");
}

#[test]
fn test_the_whole_client_stack_compiles_for_the_browser() {
    // `%http/client` binds whichever transport resolves, so this proves the web variant
    // satisfies what the client expects of it — the check the removed interface type would
    // have made, made by compilation instead.
    quiver()
        .scoped_web()
        .evaluate(r#"(get, post, head, request) = %http/client; Ok"#)
        .expect("Ok");
}

#[test]
fn test_the_browser_gets_the_web_variant_not_the_native_one() {
    // The native transport names `__tcp_connect__`, which the browser's registry does not
    // have — so resolving to it would be a compile error, and the test above passing is only
    // meaningful because this one confirms which file was chosen.
    quiver()
        .scoped_web()
        .evaluate(r#"f = &__tcp_connect__; Ok"#)
        .expect_error_containing("not available on this host");
    quiver()
        .scoped_web()
        .evaluate(r#"f = &__fetch__; Ok"#)
        .expect("Ok");
    // ... and the native host is the mirror image.
    quiver()
        .evaluate(r#"f = &__fetch__; Ok"#)
        .expect_error_containing("not available on this host");
}

#[test]
fn test_pure_std_is_shared_by_both_hosts_unchanged() {
    // `%http` and `%url` are the point of the whole arrangement: one vocabulary, compiled
    // identically for a host with sockets and a host with only fetch.
    for source in [
        r#""http://e.com/a?x=1" ~> %url.parse ~> =('%url)u; %url.target u"#,
        r#"%http.request [method: GET, target: "/"] ~> %http.serialize_request ~> Str[~]"#,
    ] {
        let native = quiver().evaluate(source).value_string();
        let web = quiver().scoped_web().evaluate(source).value_string();
        assert_eq!(native, web, "hosts disagree on pure std for: {source}");
    }
}
