mod common;
use common::*;

// The `%http` shared vocabulary (types + codecs, std/http.qv) and the `%http/server`
// pump (std/http/server.qv); design in docs/http.md. The codecs are pure, so most
// coverage needs no sockets; one integration test drives a real served connection.

#[test]
fn test_parse_request_basics() {
    quiver()
        .evaluate(
            r#""GET /posts/7 HTTP/1.1\r\nHost: x\r\n\r\n" ~> .0 ~> %http.parse_request
               =[r, rest]; [r.method, r.path, r.version, rest ~> %bin.length]"#,
        )
        .expect(r#"[GET, Cons["posts", Cons["7", Nil]], "HTTP/1.1", 0]"#);
}

#[test]
fn test_parse_request_decodes_target() {
    // Percent-decoded segments and query pairs; `+` is a space; raw target preserved.
    quiver()
        .evaluate(
            r#""GET /a%20b?x=1&msg=hi+there HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request
               =[r, _]; [r.target, r.path, r.query]"#,
        )
        .expect(r#"["/a%20b?x=1&msg=hi+there", Cons["a b", Nil], Cons[["x", "1"], Cons[["msg", "hi there"], Nil]]]"#);
}

#[test]
fn test_path_normalization() {
    // One trailing empty segment drops (`/posts/` ≡ `/posts`); "/" is Nil.
    quiver()
        .evaluate(
            r#""GET /posts/ HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~> =[r, _]; r.path"#,
        )
        .expect(r#"Cons["posts", Nil]"#);
    quiver()
        .evaluate(r#""GET / HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~> =[r, _]; r.path"#)
        .expect("Nil");
}

#[test]
fn test_parse_request_body_and_leftover() {
    // Content-Length delimits the body; pipelined bytes come back as the leftover.
    quiver()
        .evaluate(
            r#""POST /x HTTP/1.1\r\nContent-Length: 5\r\n\r\nhelloGET /" ~> .0 ~> %http.parse_request
               =[r, rest]; [r.body, rest]"#,
        )
        .expect(r#"[0x68656c6c6f, 0x474554202f]"#);
}

#[test]
fn test_parse_request_incomplete() {
    quiver()
        .evaluate(r#""GET / HTTP/1.1\r\nHost:" ~> .0 ~> %http.parse_request"#)
        .expect("Incomplete");
    // Head complete but body still arriving.
    quiver()
        .evaluate(
            r#""POST /x HTTP/1.1\r\nContent-Length: 5\r\n\r\nhe" ~> .0 ~> %http.parse_request"#,
        )
        .expect("Incomplete");
}

#[test]
fn test_parse_request_rejections() {
    quiver()
        .evaluate(r#""nonsense\r\n\r\n" ~> .0 ~> %http.parse_request ~> =Bad(status: s); s"#)
        .expect("400");
    quiver()
        .evaluate(
            r#""POST /x HTTP/1.1\r\nContent-Length: nope\r\n\r\n" ~> .0 ~> %http.parse_request ~> =Bad(status: s); s"#,
        )
        .expect("400");
    quiver()
        .evaluate(
            r#""POST /x HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n" ~> .0 ~> %http.parse_request ~> =Bad(status: s); s"#,
        )
        .expect("501");
}

#[test]
fn test_parse_request_limits() {
    // Head cap (431) — checked even before the head completes; body cap (413).
    quiver()
        .evaluate(r#"["GET /aaaaaaaaaaaaaaaa HTTP/1.1\r\n\r\n" ~> .0, 8, 100] ~> %http.parse_request_with ~> =Bad(status: s); s"#)
        .expect("431");
    quiver()
        .evaluate(
            r#"["POST /x HTTP/1.1\r\nContent-Length: 200\r\n\r\n" ~> .0, 8192, 100] ~> %http.parse_request_with ~> =Bad(status: s); s"#,
        )
        .expect("413");
}

#[test]
fn test_header_lookup_is_case_insensitive() {
    quiver()
        .evaluate(
            r#""GET / HTTP/1.1\r\nX-Thing: One\r\nx-thing: Two\r\n\r\n" ~> .0 ~> %http.parse_request
               =[r, _]; [[r.headers, "X-THING"] ~> %http.header, [r.headers, "missing"] ~> %http.header]"#,
        )
        .expect(r#"["One", []]"#);
}

#[test]
fn test_form_decoding() {
    quiver()
        .evaluate(
            r#""POST /x HTTP/1.1\r\nContent-Length: 23\r\n\r\na=1&b=hi+x&c=%2Fpath%2F" ~> .0 ~> %http.parse_request
               =[r, _]; r ~> %http.form"#,
        )
        .expect(r#"Cons[["a", "1"], Cons[["b", "hi x"], Cons[["c", "/path/"], Nil]]]"#);
    // Duplicates preserved in order; `get` answers the first.
    quiver()
        .evaluate(r#"f = "k=1&k=2&bare" ~> .0 ~> %http.form_decode; [f, [f, "k"] ~> %http.get]"#)
        .expect(r#"[Cons[["k", "1"], Cons[["k", "2"], Cons[["bare", ""], Nil]]], "1"]"#);
}

#[test]
fn test_serialize_response() {
    // Content-Length added when absent, kept when present.
    quiver()
        .evaluate(
            r#"Response[status: 404, headers: Nil, body: "gone" ~> .0] ~> %http.serialize_response ~> Str[~]"#,
        )
        .expect(r#""HTTP/1.1 404 Not Found\r\ncontent-length: 4\r\n\r\ngone""#);
    quiver()
        .evaluate(
            r#"Response[status: 200, headers: Cons[["content-length", "0"], Nil], body: 0x] ~> %http.serialize_response ~> Str[~]"#,
        )
        .expect(r#""HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n""#);
}

#[test]
fn test_handlers_are_testable_without_a_server() {
    // The handler contract is a pure function: build a request value, call, assert.
    quiver()
        .evaluate(
            r#"
            handler = #'%http {
              [$method, $path] ~> {
                | =[GET, Cons["greet", Cons[name, Nil]]] => Str[name.0] ~> %http/server.text
                | %http/server.not_found
              }
            };
            "GET /greet/ada HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~> =[req, _];
            req ~> handler ~> =Response(status: s, body: b);
            [s, Str[b]]
            "#,
        )
        .expect(r#"[200, "ada"]"#);
}

#[test]
fn test_served_connection_end_to_end() {
    // A real socket round-trip: serve on a port, connect, two keep-alive requests
    // (the second pipelined into the same connection), then a close.
    quiver()
        .with_io()
        .evaluate(
            r#"
            handler = #'%http {
              [$method, $path] ~> {
                | =[GET, Cons["n", Cons[n, Nil]]] => Str[n.0] ~> %http/server.text
                | %http/server.not_found
              }
            };
            @{ [port: 4181, handler: &handler] ~> %http/server.serve };
            { ![50] | Ok };
            sock = [0x7f000001, 4181] ~> __tcp_connect__;
            [sock, "GET /n/one HTTP/1.1\r\n\r\nGET /n/two HTTP/1.1\r\nConnection: close\r\n\r\n" ~> .0] ~> __tcp_socket_write__;
            r1 = [sock, 4096] ~> __tcp_socket_read__;
            r2 = [sock, 4096] ~> __tcp_socket_read__;
            sock ~> __tcp_socket_close__;
            [Str[[r1, r2] ~> %bin.concat]]
            "#,
        )
        .expect(r#"["HTTP/1.1 200 OK\r\ncontent-length: 3\r\ncontent-type: text/plain; charset=utf-8\r\n\r\noneHTTP/1.1 200 OK\r\ncontent-length: 3\r\ncontent-type: text/plain; charset=utf-8\r\n\r\ntwo"]"#);
}

#[test]
fn test_cookie_parsing() {
    quiver()
        .evaluate(
            r#""GET / HTTP/1.1\r\nCookie: a=1; session=abc.def; b=x%20y\r\n\r\n" ~> .0 ~> %http.parse_request
               =[r, _]; r ~> %http.cookies"#,
        )
        .expect(r#"Cons[["a", "1"], Cons[["session", "abc.def"], Cons[["b", "x%20y"], Nil]]]"#);
    // No Cookie header → no cookies; malformed segments are skipped.
    quiver()
        .evaluate(r#""GET / HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~> =[r, _]; r ~> %http.cookies"#)
        .expect("Nil");
}

#[test]
fn test_set_cookie_builder() {
    quiver()
        .evaluate(r#"["s", "v", Cons["Path=/", Cons["HttpOnly", Nil]]] ~> %http.set_cookie"#)
        .expect(r#"["set-cookie", "s=v; Path=/; HttpOnly"]"#);
}

#[test]
fn test_form_encode_round_trip() {
    // Escaping both ways: spaces, separators, and '=' inside values survive.
    quiver()
        .evaluate(r#"Cons[["a b", "c&d=e"], Cons[["k", "v"], Nil]] ~> %http.form_encode ~> Str[~]"#)
        .expect(r#""a+b=c%26d%3De&k=v""#);
    quiver()
        .evaluate(r#"Cons[["a b", "c&d=e"], Nil] ~> %http.form_encode ~> %http.form_decode"#)
        .expect(r#"Cons[["a b", "c&d=e"], Nil]"#);
}

#[test]
fn test_session_round_trip() {
    // put → Set-Cookie → send it back as Cookie → get verifies and decodes.
    quiver()
        .evaluate(
            r#"
            key = 0x000102030405060708090a0b0c0d0e0f;
            resp = Response[status: 200, headers: Nil, body: 0x];
            r2 = [resp, key, Cons[["count", "7"], Cons[["name", "Ada L"], Nil]]] ~> %http/session.put;
            [r2.headers, "set-cookie"] ~> %http.header ~> =Str[scb];
            [scb, 59, 0] ~> %bin.index ~> =('int)semi;
            cookie = [scb, 0, semi] ~> %bin.slice ~> Str[~];
            req = "GET / HTTP/1.1\r\nCookie: {cookie}\r\n\r\n" ~> .0 ~> %http.parse_request ~> =[rq, _];
            [rq, key] ~> %http/session.get
            "#,
        )
        .expect(r#"Cons[["count", "7"], Cons[["name", "Ada L"], Nil]]"#);
}

#[test]
fn test_session_rejects_tampering() {
    // A forged MAC, a wrong key, and a garbled payload all answer nil.
    quiver()
        .evaluate(
            r#"
            key = 0x000102030405060708090a0b0c0d0e0f;
            req = "GET / HTTP/1.1\r\nCookie: session=ff.00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff\r\n\r\n" ~> .0 ~> %http.parse_request ~> =[rq, _];
            r = [rq, key] ~> %http/session.get;
            { | r ~> =('%http.pairs)p => Forged | Rejected }
            "#,
        )
        .expect("Rejected");
    quiver()
        .evaluate(
            r#"
            key = 0x000102030405060708090a0b0c0d0e0f;
            other = 0xff0102030405060708090a0b0c0d0e0f;
            resp = Response[status: 200, headers: Nil, body: 0x];
            r2 = [resp, key, Cons[["a", "1"], Nil]] ~> %http/session.put;
            [r2.headers, "set-cookie"] ~> %http.header ~> =Str[scb];
            [scb, 59, 0] ~> %bin.index ~> =('int)semi;
            cookie = [scb, 0, semi] ~> %bin.slice ~> Str[~];
            req = "GET / HTTP/1.1\r\nCookie: {cookie}\r\n\r\n" ~> .0 ~> %http.parse_request ~> =[rq, _];
            r = [rq, other] ~> %http/session.get;
            { | r ~> =('%http.pairs)p => WrongKeyAccepted | Rejected }
            "#,
        )
        .expect("Rejected");
}

#[test]
fn test_session_clear() {
    quiver()
        .evaluate(
            r#"Response[status: 200, headers: Nil, body: 0x] ~> %http/session.clear
               =Response(headers: hs); [hs, "set-cookie"] ~> %http.header"#,
        )
        .expect(r#""session=; Path=/; Max-Age=0""#);
}
