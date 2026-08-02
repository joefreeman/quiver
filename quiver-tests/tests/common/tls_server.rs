//! TLS test material: a per-run PKI (CA, "localhost" server certificate, decoy CA) and a
//! rustls server on a loopback port to point `%tls.attach` at. Certificate material crosses
//! into Quiver programs as spliced DER hex literals — `%tls` taking bytes rather than paths
//! is what makes TLS testable without filesystem fixtures or the public internet.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::Arc;
use std::time::Duration;

/// A certificate authority, a "localhost" server certificate it issued, and a second,
/// unrelated CA (valid anchors that must refuse the server). Minted fresh per use, in both
/// DER (what `%tls` takes) and PEM (what `%pem` decodes).
pub struct TestPki {
    ca: Vec<u8>,
    leaf: Vec<u8>,
    key: Vec<u8>,
    decoy: Vec<u8>,
    ca_pem: String,
    leaf_pem: String,
    key_pem: String,
}

impl TestPki {
    pub fn new() -> Self {
        let ca_key = rcgen::KeyPair::generate().unwrap();
        let ca_cert = ca(&ca_key, "quiver test ca");

        let server_key = rcgen::KeyPair::generate().unwrap();
        let server_cert = rcgen::CertificateParams::new(vec!["localhost".to_string()])
            .unwrap()
            .signed_by(&server_key, &ca_cert, &ca_key)
            .unwrap();

        let decoy_key = rcgen::KeyPair::generate().unwrap();
        let decoy_cert = ca(&decoy_key, "quiver decoy ca");

        TestPki {
            ca: ca_cert.der().to_vec(),
            leaf: server_cert.der().to_vec(),
            key: server_key.serialize_der(),
            decoy: decoy_cert.der().to_vec(),
            ca_pem: ca_cert.pem(),
            leaf_pem: server_cert.pem(),
            key_pem: server_key.serialize_pem(),
        }
    }

    /// The chain as certbot's fullchain.pem has it: leaf first, then the issuer.
    pub fn chain_pem(&self) -> String {
        format!("{}{}", self.leaf_pem, self.ca_pem)
    }

    /// The presented chain, leaf first, as `%tls.accept`'s `cert` wants it.
    fn chain(&self) -> Vec<u8> {
        let mut chain = self.leaf.clone();
        chain.extend_from_slice(&self.ca);
        chain
    }

    /// Splice a program's `__ROOTS__`/`__CERT__`/`__KEY__`/`__DECOY__` markers (DER hex
    /// literals) and `__CA_PEM__`/`__CHAIN_PEM__`/`__KEY_PEM__` markers (PEM text, escaped
    /// for a Quiver string literal). Markers rather than `format!` so the Quiver source
    /// keeps its braces unescaped.
    pub fn splice(&self, source: &str) -> String {
        source
            .replace("__ROOTS__", &hex_literal(&self.ca))
            .replace("__CERT__", &hex_literal(&self.chain()))
            .replace("__KEY__", &hex_literal(&self.key))
            .replace("__DECOY__", &hex_literal(&self.decoy))
            .replace("__CA_PEM__", &escape(&self.ca_pem))
            .replace("__CHAIN_PEM__", &escape(&self.chain_pem()))
            .replace("__KEY_PEM__", &escape(&self.key_pem))
    }
}

/// What the Rust-side server does with a connection, after the handshake.
#[derive(Clone, Copy)]
pub enum Behaviour {
    /// Read 4 bytes, echo them back, close with a close_notify.
    Echo,
    /// Read 4 bytes, echo them back, then drop the TCP stream — no close_notify. The bare
    /// close is indistinguishable from a connection cut mid-response, which is the point.
    EchoThenVanish,
    /// Send `len` bytes of a counting pattern, then close cleanly. Big enough, this spans
    /// many TLS records and exercises record boundaries landing mid-read.
    Send { len: usize },
    /// Send one small record, its ciphertext dribbled a few bytes at a time, so the client
    /// sees a record split across several socket reads.
    Dribble,
    /// Complete the handshake, then say nothing — for timeout races.
    Silent,
}

pub struct TlsServer {
    pub port: u16,
    pki: TestPki,
}

impl TlsServer {
    /// Serve `behaviour` on a fresh loopback port until the test process exits.
    pub fn start(behaviour: Behaviour) -> Self {
        let pki = TestPki::new();
        let config = Arc::new(
            rustls::ServerConfig::builder()
                .with_no_client_auth()
                .with_single_cert(
                    vec![pki.leaf.clone().into(), pki.ca.clone().into()],
                    rustls::pki_types::PrivateKeyDer::Pkcs8(pki.key.clone().into()),
                )
                .unwrap(),
        );

        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        std::thread::spawn(move || {
            for stream in listener.incoming() {
                let Ok(mut tcp) = stream else { break };
                // A connection the client aborts (a refused certificate) errors here; that
                // is expected traffic, and the next test connection must still be served.
                let _ = serve(&config, behaviour, &mut tcp);
            }
        });

        TlsServer { port, pki }
    }

    /// Splice a program's `__PORT__` and certificate markers.
    pub fn program(&self, source: &str) -> String {
        self.pki
            .splice(source)
            .replace("__PORT__", &self.port.to_string())
    }
}

/// A CA certificate under a distinct name. Distinct matters: anchors are matched by issuer
/// name first, so two CAs sharing rcgen's default subject would fail as `BadSignature`
/// where an unrelated CA should fail as `UnknownIssuer`.
fn ca(key: &rcgen::KeyPair, name: &str) -> rcgen::Certificate {
    let mut params = rcgen::CertificateParams::new(Vec::new()).unwrap();
    params.is_ca = rcgen::IsCa::Ca(rcgen::BasicConstraints::Unconstrained);
    params
        .distinguished_name
        .push(rcgen::DnType::CommonName, name);
    params.self_signed(key).unwrap()
}

/// PEM text as the body of a double-quoted Quiver string literal.
pub fn escape(text: &str) -> String {
    text.replace('\r', "\\r").replace('\n', "\\n")
}

fn hex_literal(bytes: &[u8]) -> String {
    use std::fmt::Write;
    bytes.iter().fold("0x".to_string(), |mut out, b| {
        write!(out, "{b:02x}").unwrap();
        out
    })
}

fn serve(
    config: &Arc<rustls::ServerConfig>,
    behaviour: Behaviour,
    tcp: &mut TcpStream,
) -> std::io::Result<()> {
    let mut conn = rustls::ServerConnection::new(config.clone())
        .map_err(|e| std::io::Error::other(e.to_string()))?;
    while conn.is_handshaking() {
        conn.complete_io(tcp)?;
    }
    match behaviour {
        Behaviour::Echo | Behaviour::EchoThenVanish => {
            let mut buf = [0u8; 4];
            rustls::Stream::new(&mut conn, tcp).read_exact(&mut buf)?;
            rustls::Stream::new(&mut conn, tcp).write_all(&buf)?;
            if matches!(behaviour, Behaviour::Echo) {
                conn.send_close_notify();
                conn.complete_io(tcp)?;
            }
            // EchoThenVanish just returns: dropping the stream sends a bare FIN.
        }
        Behaviour::Send { len } => {
            let payload: Vec<u8> = (0..len).map(|i| i as u8).collect();
            rustls::Stream::new(&mut conn, tcp).write_all(&payload)?;
            conn.send_close_notify();
            conn.complete_io(tcp)?;
        }
        Behaviour::Dribble => {
            conn.writer().write_all(b"drip")?;
            let mut wire = Vec::new();
            while conn.wants_write() {
                conn.write_tls(&mut wire)?;
            }
            for chunk in wire.chunks(3) {
                tcp.write_all(chunk)?;
                tcp.flush()?;
                std::thread::sleep(Duration::from_millis(10));
            }
            conn.send_close_notify();
            conn.complete_io(tcp)?;
        }
        Behaviour::Silent => {
            // Hold the connection open without a byte until the peer goes away.
            let mut buf = [0u8; 1];
            let _ = rustls::Stream::new(&mut conn, tcp).read_exact(&mut buf);
        }
    }
    Ok(())
}
