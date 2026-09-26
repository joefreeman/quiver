//! A mock effect backend: the OS boundary faked, everything above it real.
//!
//! Swapping the *backend* rather than a Quiver-level seam means `%http/client`,
//! `%http/tcp`, `%http` and the effect plumbing all run exactly as they do in
//! production — only the syscalls are canned. That makes it possible to test the parts a live
//! socket cannot reach reliably: a response arriving in awkward chunks, a peer that never
//! answers, a connection refused, an operation that aborts.
//!
//! The native builtin *implementations* are still attached; they only construct
//! `NativeEffect` values, so nothing about them needs faking. The canned data is raw HTTP
//! either way: the socket effects serve it as reads, and the mocked `http_request` puts it
//! through `quiver_io::http`'s real head parser and framing decoder — so a canned response
//! exercises the same protocol code a live exchange does.
//!
//! The fake host is dual-stack with nothing bound on IPv6: every name resolves to `::1`
//! then `127.0.0.1`, and a connect to `::1` is always refused. So every test that dials a
//! name also exercises the fallback across a resolver's answers — a client that took the
//! first address on faith would fail all of them.

use quiver_core::effects::{EffectBackend, EffectError, EffectResult};
use quiver_core::error::Error;
use quiver_core::process::{ProcessId, StreamEvent};
use quiver_core::value::ResourceId;
use quiver_core::wire::WireValue;
use quiver_io::NativeEffect;

/// How the fake network behaves for the duration of one test.
#[derive(Clone, Debug)]
pub enum MockIo {
    /// Connects succeed; reads answer these chunks in order, then end-of-stream. Several
    /// chunks exercise a parser across arbitrary read boundaries.
    Serves(Vec<Vec<u8>>),
    /// Every connect is refused.
    Refuses,
    /// Connects succeed, but a read never completes — the peer that went away.
    Stalls,
    /// Connects succeed; the first read aborts the calling process.
    Aborts,
}

pub struct MockBackend {
    behaviour: MockIo,
    next_resource: ResourceId,
    /// Read cursor per resource: into the scripted chunks for a socket, and a
    /// yielded-yet flag for a resolver.
    cursors: std::collections::HashMap<ResourceId, usize>,
    /// Queued events per mocked response body, delivered one per arm.
    bodies:
        std::collections::HashMap<ResourceId, std::collections::VecDeque<(StreamEvent, Vec<u8>)>>,
    events: Vec<(ResourceId, usize, StreamEvent, Vec<u8>)>,
    /// Resource type ids, as pushed by the environment. A handle carrying the wrong one fails
    /// the `=(+TcpSocket)` / `=(+DnsResolver)` checks in std and looks like an empty result.
    socket_type_id: usize,
    dns_type_id: usize,
    byte_stream_type_id: usize,
    http_result: Option<quiver_core::effects::ResultTupleInfo>,
}

impl MockBackend {
    pub fn new(behaviour: MockIo) -> Self {
        Self {
            behaviour,
            next_resource: 1,
            cursors: std::collections::HashMap::new(),
            bodies: std::collections::HashMap::new(),
            events: Vec::new(),
            socket_type_id: 0,
            dns_type_id: 0,
            byte_stream_type_id: 0,
            http_result: None,
        }
    }

    fn fresh_socket(&mut self) -> EffectResult {
        let id = self.next_resource;
        self.next_resource += 1;
        self.cursors.insert(id, 0);
        Ok(WireValue::Resource(id, self.socket_type_id))
    }

    /// A mocked `http_request` answer: the canned bytes through the real head parser and
    /// framing decoder, the body queued as stream events behind a `+ByteStream` handle.
    fn http_exchange(&mut self, method: &[u8]) -> EffectResult {
        let MockIo::Serves(chunks) = &self.behaviour else {
            unreachable!("only Serves reaches the exchange");
        };
        let raw: Vec<u8> = chunks.concat();
        let head = quiver_io::http::parse_response_head(&raw)
            .map_err(EffectError::Other)?
            .ok_or_else(|| EffectError::Other("connection closed mid-response".to_string()))?;
        let mut framing =
            quiver_io::http::response_framing(method, &head).map_err(EffectError::Other)?;
        let mut input = raw[head.body_start..].to_vec();
        let data = framing.decode(&mut input).map_err(EffectError::Other)?;
        let mut queue = std::collections::VecDeque::new();
        if !data.is_empty() {
            queue.push_back((StreamEvent::Data, data));
        }
        // The canned bytes are the whole connection, so their end is the connection's.
        match framing.on_eof() {
            Ok(()) => queue.push_back((StreamEvent::End, Vec::new())),
            Err(e) => queue.push_back((
                StreamEvent::Failed {
                    error: EffectError::Other(e),
                },
                Vec::new(),
            )),
        }
        let id = self.next_resource;
        self.next_resource += 1;
        self.bodies.insert(id, queue);
        let info = self
            .http_result
            .as_ref()
            .expect("http_request result ids pushed");
        Ok(WireValue::tuple(
            info.tuple_id,
            vec![
                WireValue::Int(head.status as i64),
                WireValue::Binary(head.headers.into()),
                WireValue::Resource(id, self.byte_stream_type_id),
            ],
        ))
    }
}

impl EffectBackend for MockBackend {
    type E = NativeEffect;

    fn execute(
        &mut self,
        _process_id: ProcessId,
        effect: NativeEffect,
    ) -> Result<Option<EffectResult>, Error> {
        let result = match effect {
            NativeEffect::DnsResolve { .. } => {
                let id = self.next_resource;
                self.next_resource += 1;
                self.cursors.insert(id, 0);
                Ok(WireValue::Resource(id, self.dns_type_id))
            }
            NativeEffect::DnsNext { resource_id } => {
                let addresses: &[&[u8]] = &[
                    &[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
                    &[127, 0, 0, 1],
                ];
                let seen = self.cursors.entry(resource_id).or_insert(0);
                match addresses.get(*seen) {
                    Some(address) => {
                        *seen += 1;
                        Ok(WireValue::Binary(address.to_vec().into()))
                    }
                    None => Ok(WireValue::nil()),
                }
            }
            NativeEffect::DnsClose { .. } => Ok(WireValue::ok()),

            NativeEffect::TcpConnect { ip, .. } => match self.behaviour {
                MockIo::Refuses => Err(EffectError::ConnectionRefused(
                    "mock: connection refused".to_string(),
                )),
                _ if ip.len() == 16 => Err(EffectError::ConnectionRefused(
                    "mock: nothing bound on ::1".to_string(),
                )),
                _ => self.fresh_socket(),
            },
            NativeEffect::TcpSocketWrite { data, .. } => Ok(WireValue::Int(data.len() as i64)),
            NativeEffect::TcpSocketRead { resource_id, .. } => match &self.behaviour {
                // Never completing is what a stalled peer looks like from here: the request
                // process stays parked, and only the client's own timeout ends it.
                MockIo::Stalls => return Ok(None),
                MockIo::Aborts => {
                    return Err(Error::Panic("mock: the read aborted".to_string()));
                }
                MockIo::Serves(chunks) => {
                    let cursor = self.cursors.entry(resource_id).or_insert(0);
                    let chunk = chunks.get(*cursor).cloned().unwrap_or_default();
                    *cursor += 1;
                    Ok(WireValue::Binary(chunk.into()))
                }
                MockIo::Refuses => Ok(WireValue::Binary(Vec::new().into())),
            },
            NativeEffect::TcpSocketClose { resource_id } => {
                self.cursors.remove(&resource_id);
                Ok(WireValue::ok())
            }

            NativeEffect::HttpRequest { method, .. } => match &self.behaviour {
                MockIo::Refuses => Err(EffectError::ConnectionRefused(
                    "mock: connection refused".to_string(),
                )),
                MockIo::Stalls => return Ok(None),
                MockIo::Aborts => {
                    return Err(Error::Panic("mock: the read aborted".to_string()));
                }
                MockIo::Serves(_) => self.http_exchange(&method),
            },

            other => {
                return Err(Error::InvalidArgument(format!(
                    "mock backend has no answer for {other:?}"
                )));
            }
        };
        Ok(Some(result))
    }

    fn process_completions(&mut self) -> Vec<(ProcessId, EffectResult)> {
        Vec::new()
    }

    fn has_operations_in_flight(&self) -> bool {
        false
    }

    fn close_resource(&mut self, resource_id: ResourceId) {
        self.cursors.remove(&resource_id);
        self.bodies.remove(&resource_id);
    }

    fn arm_stream(&mut self, resource_id: ResourceId) -> Result<(), Error> {
        let Some(queue) = self.bodies.get_mut(&resource_id) else {
            return Err(Error::InvalidArgument(format!(
                "mock: resource {resource_id} is not a stream"
            )));
        };
        // One queued event per arm; a drained body keeps answering End.
        let (event, data) = queue.pop_front().unwrap_or((StreamEvent::End, Vec::new()));
        self.events
            .push((resource_id, self.byte_stream_type_id, event, data));
        Ok(())
    }

    fn take_stream_events(&mut self) -> Vec<(ResourceId, usize, StreamEvent, Vec<u8>)> {
        std::mem::take(&mut self.events)
    }

    fn set_type_ids(
        &mut self,
        resources: &[String],
        results: &[(String, quiver_core::effects::ResultTupleInfo)],
    ) {
        if let Some(index) = resources.iter().position(|name| name == "TcpSocket") {
            self.socket_type_id = index;
        }
        if let Some(index) = resources.iter().position(|name| name == "DnsResolver") {
            self.dns_type_id = index;
        }
        if let Some(index) = resources.iter().position(|name| name == "ByteStream") {
            self.byte_stream_type_id = index;
        }
        if let Some((_, info)) = results.iter().find(|(name, _)| name == "http_request") {
            self.http_result = Some(info.clone());
        }
    }
}
