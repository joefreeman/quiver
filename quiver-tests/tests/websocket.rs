mod common;
use common::*;

// `%http/websocket` (std/http/websocket.qv): the RFC 6455 handshake and frame codec
// (pure — pinned to the RFC's worked examples), and the upgraded-connection API
// through `%http/server`'s `Upgrade` response (a real socket round-trip).

#[test]
fn test_accept_key_rfc_vector() {
    // RFC 6455 §1.3's worked example.
    quiver()
        .evaluate(r#""dGhlIHNhbXBsZSBub25jZQ==" ~> %http/websocket.accept_key"#)
        .expect(r#""s3pPLMBiTxaQ9kYGzzhZRbK+xOo=""#);
}

#[test]
fn test_handshake_response_bytes() {
    quiver()
        .evaluate(r#""dGhlIHNhbXBsZSBub25jZQ==" ~> %http/websocket.handshake_response ~> Str[~]"#)
        .expect(
            r#""HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n\r\n""#,
        );
}

#[test]
fn test_frame_codec_rfc_masked_hello() {
    // RFC 6455 §5.7: a masked single-frame text "Hello" is exactly these 11 bytes,
    // and parses back to the unmasked payload with nothing left over.
    quiver()
        .evaluate(
            r#"m = %http/websocket.encode_masked_frame [1, "Hello" ~> .0, 0x37fa213d]
               [%bin.to_hex m, %http/websocket.parse_frame m]"#,
        )
        .expect(
            r#"["818537fa213d7f9f4d5158", [Frame[fin: 1, opcode: 1, payload: 0x48656c6c6f], 0x]]"#,
        );
    // The unmasked server-side encoding of the same message (§5.7's first example).
    quiver()
        .evaluate(r#"%http/websocket.encode_frame [1, "Hello" ~> .0] ~> %bin.to_hex"#)
        .expect(r#""810548656c6c6f""#);
}

#[test]
fn test_frame_codec_extended_lengths() {
    // A 300-byte payload takes the 126/16-bit length form; round-trips exactly.
    quiver()
        .evaluate(
            r#"m = %http/websocket.encode_masked_frame [2, __binary_new__ 300, 0x01020304]
               %http/websocket.parse_frame m ~> =[Frame[fin: f, opcode: o, payload: p], rest]
               [f, o, %bin.length p, %bin.length rest]"#,
        )
        .expect("[1, 2, 300, 0]");
}

#[test]
fn test_frame_codec_incomplete_and_violations() {
    quiver()
        .evaluate(r#"0x81 ~> %http/websocket.parse_frame"#)
        .expect("Incomplete");
    // A masked frame cut short of its payload is incomplete, not an error.
    quiver()
        .evaluate(
            r#"m = %http/websocket.encode_masked_frame [1, "Hello" ~> .0, 0x37fa213d]
               %bin.slice [m, 0, 8] ~> %http/websocket.parse_frame"#,
        )
        .expect("Incomplete");
    // An unmasked client frame is a protocol violation.
    quiver()
        .evaluate(r#"%http/websocket.encode_frame [1, "Hi" ~> .0] ~> %http/websocket.parse_frame"#)
        .expect(r#"Bad["unmasked client frame"]"#);
    // A fragmented control frame is a protocol violation (RFC 6455 §5.5).
    quiver()
        .evaluate(r#"0x0980ffffffff ~> %http/websocket.parse_frame"#)
        .expect(r#"Bad["malformed control frame"]"#);
}

#[test]
fn test_upgraded_echo_end_to_end() {
    // The full stack over a real socket: HTTP handshake with the RFC's key, a masked
    // text frame echoed back unmasked, then a client Close (1000) echoed and the
    // socket dropped.
    quiver()
        .with_io()
        .evaluate(
            r#"
            ws_echo = #'%http/websocket {
              %http/websocket.recv $ ~> =[m, c]
              m ~> {
                | =Text[s] => { %http/websocket.send_text [c, s]; ^ c }
                | =Binary[b] => { %http/websocket.send_binary [c, b]; ^ c }
                | Ok
              }
            };
            handler = #'%http {
              [$method, $path] ~> {
                | =[GET, Cons["ws", Nil]] => Upgrade[handler: &ws_echo]
                | %http/server.not_found
              }
            };
            @{ [port: 4183, handler: &handler] ~> %http/server.serve };
            { ![50] | Ok };
            sock = [0x7f000001, 4183] ~> __tcp_connect__;
            req = "GET /ws HTTP/1.1\r\nHost: t\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n";
            __tcp_socket_write__ [sock, req ~> .0];
            r1 = __tcp_socket_read__ [sock, 4096];
            __tcp_socket_write__ [sock, %http/websocket.encode_masked_frame [1, "Hello" ~> .0, 0x37fa213d]];
            r2 = __tcp_socket_read__ [sock, 4096];
            __tcp_socket_write__ [sock, %http/websocket.encode_masked_frame [8, 0x03e8, 0x00000000]];
            r3 = __tcp_socket_read__ [sock, 4096];
            sock ~> __tcp_socket_close__;
            [Str[r1], %bin.to_hex r2, %bin.to_hex r3]
            "#,
        )
        .expect(
            r#"["HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n\r\n", "810548656c6c6f", "880203e8"]"#,
        );
}

#[test]
fn test_upgrade_pings_and_fragments_end_to_end() {
    // `recv` answers a ping transparently and assembles a fragmented text message
    // ("Hel" + "lo" across a continuation) before echoing it as one frame.
    quiver()
        .with_io()
        .evaluate(
            r#"
            ws_echo = #'%http/websocket {
              %http/websocket.recv $ ~> =[m, c]
              m ~> {
                | =Text[s] => { %http/websocket.send_text [c, s]; ^ c }
                | Ok
              }
            };
            handler = #'%http {
              Upgrade[handler: &ws_echo]
            };
            @{ [port: 4184, handler: &handler] ~> %http/server.serve };
            { ![50] | Ok };
            sock = [0x7f000001, 4184] ~> __tcp_connect__;
            req = "GET /ws HTTP/1.1\r\nHost: t\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n";
            __tcp_socket_write__ [sock, req ~> .0];
            r1 = __tcp_socket_read__ [sock, 4096];
            // A ping (masked, zero key: payload rides verbatim) → an unmasked pong.
            __tcp_socket_write__ [sock, 0x898400000000abcdef01];
            r2 = __tcp_socket_read__ [sock, 4096];
            // Text "Hel" without FIN, then a continuation "lo" with FIN — echoed whole.
            f1 = 0x018300000000 ~> %bin.concat [~, "Hel" ~> .0];
            f2 = 0x808200000000 ~> %bin.concat [~, "lo" ~> .0];
            __tcp_socket_write__ [sock, %bin.concat [f1, f2]];
            r3 = __tcp_socket_read__ [sock, 4096];
            sock ~> __tcp_socket_close__;
            [%bin.length r1, %bin.to_hex r2, %bin.to_hex r3]
            "#,
        )
        .expect(r#"[129, "8a04abcdef01", "810548656c6c6f"]"#);
}

#[test]
fn test_upgrade_without_key_answers_400() {
    quiver()
        .with_io()
        .evaluate(
            r#"
            handler = #'%http { Upgrade[handler: #'%http/websocket { Ok }] };
            @{ [port: 4185, handler: &handler] ~> %http/server.serve };
            { ![50] | Ok };
            sock = [0x7f000001, 4185] ~> __tcp_connect__;
            __tcp_socket_write__ [sock, "GET /ws HTTP/1.1\r\n\r\n" ~> .0];
            r = __tcp_socket_read__ [sock, 4096];
            sock ~> __tcp_socket_close__;
            [Str[r]]
            "#,
        )
        .expect(r#"["HTTP/1.1 400 Bad Request\r\ncontent-length: 11\r\ncontent-type: text/plain; charset=utf-8\r\n\r\nBad Request"]"#);
}
