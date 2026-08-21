//! What a document cannot assert about `%pem`.
//!
//! The module's semantics are specified and checked in `std/docs/pem.md`, which `quiv test`
//! runs — block structure, wrapping, CRLF, surrounding text, the malformed cases, and the
//! `certificates`/`key` selections, all over short stand-in bodies. What stays here needs
//! material a document cannot hold: PEM minted at test time (`tests/common/tls_server.rs`),
//! whose DER is only known to the harness that made it. These are therefore the only checks
//! that genuine 64-column armor decodes byte-for-byte. (The `%pem` → `%tls` integration lives
//! with the TLS harness it needs, in `tls.rs`.)

use crate::common::tls_server::TestPki;
use crate::common::*;

#[test]
fn test_certificates_decodes_a_fullchain() {
    // A certbot-style fullchain.pem — real armor, wrapped at 64 columns, leaf then issuer —
    // decoded to the concatenated DER that %tls.accept's cert takes. Both the text and the
    // DER it must equal are spliced in from the freshly minted PKI.
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
    // The same, for a real PKCS#8 private key.
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
