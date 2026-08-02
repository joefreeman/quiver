//! A TLS server on a loopback port, with certificates minted for the run — the peer the
//! `%tls` tests talk to. `%tls.attach` taking trust anchors as DER bytes is what makes this
//! possible at all: the client is told to trust exactly the CA that issued the server's
//! certificate, so no filesystem, environment, or internet is involved.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::Arc;
use std::time::Duration;

/// What the server does with a connection, after the handshake.
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
}

pub struct TlsServer {
    pub port: u16,
    /// The CA that issued the server's certificate, as DER.
    ca: Vec<u8>,
    /// A second, unrelated CA — valid trust anchors that must refuse the server.
    decoy: Vec<u8>,
}

impl TlsServer {
    /// Mint a CA and a "localhost" server certificate, and serve `behaviour` on a fresh
    /// loopback port until the test process exits.
    pub fn start(behaviour: Behaviour) -> Self {
        let ca_key = rcgen::KeyPair::generate().unwrap();
        let ca_cert = ca(&ca_key, "quiver test ca");

        let server_key = rcgen::KeyPair::generate().unwrap();
        let server_cert = rcgen::CertificateParams::new(vec!["localhost".to_string()])
            .unwrap()
            .signed_by(&server_key, &ca_cert, &ca_key)
            .unwrap();

        let decoy_key = rcgen::KeyPair::generate().unwrap();
        let decoy_cert = ca(&decoy_key, "quiver decoy ca");

        let config = Arc::new(
            rustls::ServerConfig::builder()
                .with_no_client_auth()
                .with_single_cert(
                    vec![server_cert.der().clone(), ca_cert.der().clone()],
                    rustls::pki_types::PrivateKeyDer::Pkcs8(server_key.serialize_der().into()),
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

        TlsServer {
            port,
            ca: ca_cert.der().to_vec(),
            decoy: decoy_cert.der().to_vec(),
        }
    }

    /// The issuing CA as a Quiver binary literal, for `roots:`.
    pub fn roots(&self) -> String {
        hex_literal(&self.ca)
    }

    /// The unrelated CA as a Quiver binary literal — anchors that must refuse the server.
    pub fn decoy_roots(&self) -> String {
        hex_literal(&self.decoy)
    }

    /// Splice a program's `__PORT__`/`__ROOTS__`/`__DECOY__` markers. Markers rather than
    /// `format!` so the Quiver source keeps its braces unescaped.
    pub fn program(&self, source: &str) -> String {
        source
            .replace("__PORT__", &self.port.to_string())
            .replace("__ROOTS__", &self.roots())
            .replace("__DECOY__", &self.decoy_roots())
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
    }
    Ok(())
}
