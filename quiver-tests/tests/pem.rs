mod common;
use common::tls_server::{Behaviour, TestPki, TlsServer, escape};
use common::*;
use std::time::Duration;

// `%pem` — the pure-Quiver bridge from PEM text to the DER bytes `%tls` takes. The PEM
// material is minted per test (tests/common/tls_server.rs) and spliced in as escaped
// string literals; the DER the decode must equal is spliced beside it as hex.

#[test]
fn test_certificates_decodes_a_fullchain() {
    // A certbot-style fullchain.pem: leaf certificate then issuer, decoded to the
    // leaf-first concatenated DER that %tls.accept's cert takes.
    let pki = TestPki::new();
    quiver()
        .evaluate(&pki.splice(
            r#"
            expected = __CERT__
            %pem.certificates "__CHAIN_PEM__" ~> =&expected
            "#,
        ))
        .expect("Ok");
}

#[test]
fn test_key_decodes_pkcs8() {
    let pki = TestPki::new();
    quiver()
        .evaluate(&pki.splice(
            r#"
            expected = __KEY__
            %pem.key "__KEY_PEM__" ~> =&expected
            "#,
        ))
        .expect("Ok");
}

#[test]
fn test_crlf_and_surrounding_text_are_tolerated() {
    // openssl's `-text` output puts a human-readable dump before the block, and Windows
    // files arrive CRLF; both must decode to the same DER.
    let pki = TestPki::new();
    let messy = format!(
        "subject=CN = localhost\nnot a pem line\n{}\ntrailing note\n",
        pki.chain_pem().replace('\n', "\r\n")
    );
    quiver()
        .evaluate(
            &pki.splice(
                r#"
                    expected = __CERT__
                    %pem.certificates "__MESSY__" ~> =&expected
                    "#,
            )
            .replace("__MESSY__", &escape(&messy)),
        )
        .expect("Ok");
}

#[test]
fn test_malformed_pem_is_nil() {
    // An unterminated block, and a corrupted base64 body: both poison the whole text.
    let pki = TestPki::new();
    let unterminated = pki.chain_pem().lines().next().unwrap().to_string() + "\nAAAA\n";
    let corrupted = pki.chain_pem().replacen('M', "!", 1);
    quiver()
        .evaluate(
            &r#"
            { %pem.blocks "__UNTERMINATED__" => No | Ok } ~> =Ok
            { %pem.blocks "__CORRUPTED__" => No | Ok } ~> =Ok
            Ok
            "#
            .replace("__UNTERMINATED__", &escape(&unterminated))
            .replace("__CORRUPTED__", &escape(&corrupted)),
        )
        .expect("Ok");
}

#[test]
fn test_no_blocks_is_an_empty_list_not_a_failure() {
    // Text without any block decodes to an empty list — "found nothing" — while
    // `certificates` and `key`, which need at least one block, answer nil.
    quiver()
        .evaluate(
            r#"
            %pem.blocks "just some text" ~> =Nil
            { %pem.certificates "just some text" => No | Ok } ~> =Ok
            { %pem.key "just some text" => No | Ok } ~> =Ok
            Ok
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_key_ignores_legacy_key_formats() {
    // A SEC1 "EC PRIVATE KEY" block is not PKCS#8; answering its DER would fail opaquely
    // inside rustls, so `key` deliberately does not answer it (its docstring names the
    // openssl conversion instead).
    quiver()
        .evaluate(
            r#"
            text = "-----BEGIN EC PRIVATE KEY-----\nAAAA\n-----END EC PRIVATE KEY-----"
            { %pem.key text => No | Ok }
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_pem_roots_reach_a_tls_server() {
    // The integration the module exists for: PEM text in, verified TLS connection out.
    let server = TlsServer::start(Behaviour::Echo);
    quiver()
        .with_io()
        .with_real_time()
        .with_timeout(Duration::from_secs(10))
        .evaluate(&server.program(
            r#"
            %pem.certificates "__CA_PEM__" ~> =('bin)roots
            %tcp.connect [0x7f000001, __PORT__] ~> =(\TcpSocket)s
            %tls.attach [socket: s, hostname: "localhost", roots: roots]
            %tcp.write [s, "ping" ~> .0]
            %tcp.read [s, 4] ~> Str[~]
            "#,
        ))
        .expect(r#""ping""#);
}
