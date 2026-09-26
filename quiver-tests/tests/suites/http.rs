//! What a `%http` document cannot reach.
//!
//! `%http` itself — the message records and the codecs over them — is specified and checked in
//! `std/docs/http.md`, which `quiv test` runs: request and response parsing, framing, targets,
//! headers, forms, cookies, serialization and the round trip between them. Those are pure, so a
//! `//= P` matching the value is the right test for all of it.
//!
//! Two kinds of coverage stay here. The first needs a *host*: a real listener, a real socket and
//! a served connection, which a document has no way to stand up. The second belongs to modules
//! layered on `%http` — `%http/server`'s response helpers, and `%http/session`'s signed cookies —
//! and will move when those modules get documents of their own.

use crate::common::*;

// --- %http/server: the connection pump, over real sockets ---------------------------------

#[test]
fn test_served_connection_end_to_end() {
    // A real socket round-trip: serve on a port, connect, two keep-alive requests
    // (the second pipelined into the same connection), then a close.
    quiver()
        .with_io()
        .with_real_time()
        .evaluate(
            r#"
            handler = #'%http {
              [$method, $path] ~> {
                | =[GET, Cons["n", Cons[n, Nil]]] => Str[n.0] ~> %http/server.text ~
                | %http/server.not_found []
              }
            };
            @[] { [port: 4181, handler: handler] ~> %http/server.serve ~ } [];
            { ![50] | Ok };
            [<7f000001>, 4181] ~> __tcp_connect__ ~ ~> =(+TcpSocket)sock;
            [sock, "GET /n/one HTTP/1.1\r\n\r\nGET /n/two HTTP/1.1\r\nConnection: close\r\n\r\n" ~> .0] ~> __tcp_socket_write__ ~;
            [sock, 4096] ~> __tcp_socket_read__ ~ ~> =('bin)r1;
            [sock, 4096] ~> __tcp_socket_read__ ~ ~> =('bin)r2;
            sock ~> __tcp_socket_close__ ~;
            [Str[[r1, r2] ~> %bin.concat ~]]
            "#,
        )
        .expect(r#"["HTTP/1.1 200 OK\r\ncontent-length: 3\r\ncontent-type: text/plain; charset=utf-8\r\n\r\noneHTTP/1.1 200 OK\r\ncontent-length: 3\r\ncontent-type: text/plain; charset=utf-8\r\n\r\ntwo"]"#);
}

#[test]
fn test_crashed_handler_answers_500_and_connection_survives() {
    // A handler that *crashes* (a `__panic__` here) runs in its own process and is
    // awaited: the crash arrives as a `:crash`-stamped nil (never-lethal `!`) and
    // degrades to the same plain 500 as a failed handler —
    // and the connection pump survives to answer the next pipelined request normally.
    quiver()
        .with_io()
        .with_real_time()
        .evaluate(
            r#"
            handler = #'%http {
              [$method, $path] ~> {
                | =[GET, Cons["boom", Nil]] => "handler crashed" ~> __panic__ ~
                | %http/server.not_found []
              }
            };
            @[] { [port: 4182, handler: handler] ~> %http/server.serve ~ } [];
            { ![50] | Ok };
            [<7f000001>, 4182] ~> __tcp_connect__ ~ ~> =(+TcpSocket)sock;
            [sock, "GET /boom HTTP/1.1\r\n\r\nGET /ok HTTP/1.1\r\nConnection: close\r\n\r\n" ~> .0] ~> __tcp_socket_write__ ~;
            [sock, 4096] ~> __tcp_socket_read__ ~ ~> =('bin)r1;
            [sock, 4096] ~> __tcp_socket_read__ ~ ~> =('bin)r2;
            sock ~> __tcp_socket_close__ ~;
            [Str[[r1, r2] ~> %bin.concat ~]]
            "#,
        )
        .expect(r#"["HTTP/1.1 500 Internal Server Error\r\ncontent-length: 21\r\ncontent-type: text/plain; charset=utf-8\r\n\r\nInternal Server ErrorHTTP/1.1 404 Not Found\r\ncontent-length: 9\r\ncontent-type: text/plain; charset=utf-8\r\n\r\nNot Found"]"#);
}

#[test]
fn test_server_response_helpers_need_no_server() {
    // `%http/server`'s `text` and `not_found` are pure response builders, so the handler
    // contract they serve is testable without a socket: build a request value, call, assert.
    // (`std/docs/http.md` makes the same point over plain `%http` Response values.)
    quiver()
        .evaluate(
            r#"
            handler = #'%http {
              [$method, $path] ~> {
                | =[GET, Cons["greet", Cons[name, Nil]]] => Str[name.0] ~> %http/server.text ~
                | %http/server.not_found []
              }
            };
            "GET /greet/ada HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[req, _];
            req ~> handler ~ ~> =Response(status: s, body: b);
            [s, Str[b]]
            "#,
        )
        .expect(r#"[200, "ada"]"#);
}

// --- %http/session: signed cookies ---------------------------------------------------------

#[test]
fn test_session_round_trip() {
    // put → Set-Cookie → send it back as Cookie → get verifies and decodes.
    quiver()
        .evaluate(
            r#"
            key = <000102030405060708090a0b0c0d0e0f>;
            resp = Response[status: 200, headers: Nil, body: <>];
            r2 = [resp, key, Cons[["count", "7"], Cons[["name", "Ada L"], Nil]]] ~> %http/session.put ~;
            [r2.headers, "set-cookie"] ~> %http.header ~ ~> =Str[scb];
            [scb, 59, 0] ~> %bin.index ~ ~> =('int)semi;
            cookie = [scb, 0, semi] ~> %bin.slice ~ ~> Str[~];
            "GET / HTTP/1.1\r\nCookie: {cookie}\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[rq, _];
            [rq, key] ~> %http/session.get ~
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
            key = <000102030405060708090a0b0c0d0e0f>;
            "GET / HTTP/1.1\r\nCookie: session=ff.00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[rq, _];
            r = [rq, key] ~> %http/session.get ~;
            { | r ~> =('%http.pairs)p => Forged | Rejected }
            "#,
        )
        .expect("Rejected");
    quiver()
        .evaluate(
            r#"
            key = <000102030405060708090a0b0c0d0e0f>;
            other = <ff0102030405060708090a0b0c0d0e0f>;
            resp = Response[status: 200, headers: Nil, body: <>];
            r2 = [resp, key, Cons[["a", "1"], Nil]] ~> %http/session.put ~;
            [r2.headers, "set-cookie"] ~> %http.header ~ ~> =Str[scb];
            [scb, 59, 0] ~> %bin.index ~ ~> =('int)semi;
            cookie = [scb, 0, semi] ~> %bin.slice ~ ~> Str[~];
            "GET / HTTP/1.1\r\nCookie: {cookie}\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[rq, _];
            r = [rq, other] ~> %http/session.get ~;
            { | r ~> =('%http.pairs)p => WrongKeyAccepted | Rejected }
            "#,
        )
        .expect("Rejected");
}

#[test]
fn test_session_clear() {
    quiver()
        .evaluate(
            r#"Response[status: 200, headers: Nil, body: <>] ~> %http/session.clear ~ ~> =Response(headers: hs); [hs, "set-cookie"] ~> %http.header ~"#,
        )
        .expect(r#""session=; Path=/; Max-Age=0""#);
}
