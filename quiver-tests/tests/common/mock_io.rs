//! A mock effect backend: the OS boundary faked, everything above it real.
//!
//! Swapping the *backend* rather than a Quiver-level seam means `%http/client`,
//! `%http/transport`, `%http` and the effect plumbing all run exactly as they do in
//! production — only the syscalls are canned. That makes it possible to test the parts a live
//! socket cannot reach reliably: a response arriving in awkward chunks, a peer that never
//! answers, a connection refused, an operation that aborts.
//!
//! The native builtin *implementations* are still attached; they only construct
//! `NativeEffect` values, so nothing about them needs faking.

use quiver_core::effects::{EffectBackend, EffectError, EffectResult};
use quiver_core::error::Error;
use quiver_core::process::ProcessId;
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
    /// Resource type ids, as pushed by the environment. A handle carrying the wrong one fails
    /// the `=(\TcpSocket)` / `=(\DnsResolver)` checks in std and looks like an empty result.
    socket_type_id: usize,
    dns_type_id: usize,
}

impl MockBackend {
    pub fn new(behaviour: MockIo) -> Self {
        Self {
            behaviour,
            next_resource: 1,
            cursors: std::collections::HashMap::new(),
            socket_type_id: 0,
            dns_type_id: 0,
        }
    }

    fn fresh_socket(&mut self) -> EffectResult {
        let id = self.next_resource;
        self.next_resource += 1;
        self.cursors.insert(id, 0);
        Ok(WireValue::Resource(id, self.socket_type_id))
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
            // A hostname resolves to 127.0.0.1, so the transport's DNS step is exercised
            // without a resolver.
            NativeEffect::DnsResolve { .. } => {
                let id = self.next_resource;
                self.next_resource += 1;
                self.cursors.insert(id, 0);
                Ok(WireValue::Resource(id, self.dns_type_id))
            }
            NativeEffect::DnsNext { resource_id } => {
                let seen = self.cursors.entry(resource_id).or_insert(0);
                if *seen == 0 {
                    *seen = 1;
                    Ok(WireValue::Binary(vec![127, 0, 0, 1].into()))
                } else {
                    Ok(WireValue::nil())
                }
            }
            NativeEffect::DnsClose { .. } => Ok(WireValue::ok()),

            NativeEffect::TcpConnect { .. } => match self.behaviour {
                MockIo::Refuses => Err(EffectError::ConnectionRefused(
                    "mock: connection refused".to_string(),
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
    }

    fn set_type_ids(
        &mut self,
        resources: &[String],
        _results: &[(String, quiver_core::effects::ResultTupleInfo)],
    ) {
        if let Some(index) = resources.iter().position(|name| name == "TcpSocket") {
            self.socket_type_id = index;
        }
        if let Some(index) = resources.iter().position(|name| name == "DnsResolver") {
            self.dns_type_id = index;
        }
    }
}
