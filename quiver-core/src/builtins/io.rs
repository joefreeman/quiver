//! Type contract for the IO builtins (file and network).
//!
//! The signatures live here — in the type-system authority — independently of any
//! implementation: `__file_read__: [\File, 'int, 'int] -> 'bin` is true regardless of which host
//! provides the runtime. A type-checking host (the language server) registers the signatures
//! alone via [`register_io_signatures`]; an executing host registers the same signatures paired
//! with its own implementations (e.g. `quiver-io`'s native io-uring backend, or a web backend).

use super::{BuiltinFn, BuiltinRegistry, BuiltinResult, TypeSpec};
use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::ProcessId;
use crate::value::Value;

/// The file builtins' contract: `(name, parameter, result)` for each.
fn file_signatures() -> Vec<(&'static str, TypeSpec, TypeSpec)> {
    let file = TypeSpec::Resource("File".to_string());
    let bin = TypeSpec::Binary;
    let int = TypeSpec::Integer;
    let ok = TypeSpec::Tuple(Some("Ok"), vec![]);
    vec![
        // file_open([path, flags, mode]) -> File
        (
            "file_open",
            TypeSpec::Tuple(
                None,
                vec![
                    (None, bin.clone()),
                    (None, int.clone()),
                    (None, int.clone()),
                ],
            ),
            file.clone(),
        ),
        // file_read([File, offset, length]) -> bin
        (
            "file_read",
            TypeSpec::Tuple(
                None,
                vec![
                    (None, file.clone()),
                    (None, int.clone()),
                    (None, int.clone()),
                ],
            ),
            bin.clone(),
        ),
        // file_write([File, offset, bin]) -> int
        (
            "file_write",
            TypeSpec::Tuple(
                None,
                vec![(None, file.clone()), (None, int.clone()), (None, bin)],
            ),
            int,
        ),
        // file_flush([File]) -> Ok
        (
            "file_flush",
            TypeSpec::Tuple(None, vec![(None, file.clone())]),
            ok.clone(),
        ),
        // file_close([File]) -> Ok
        ("file_close", TypeSpec::Tuple(None, vec![(None, file)]), ok),
    ]
}

/// The network builtins' contract: `(name, parameter, result)` for each.
fn network_signatures() -> Vec<(&'static str, TypeSpec, TypeSpec)> {
    let dns = TypeSpec::Resource("DnsResolver".to_string());
    let socket = TypeSpec::Resource("TcpSocket".to_string());
    let listener = TypeSpec::Resource("TcpListener".to_string());
    let bin = TypeSpec::Binary;
    let int = TypeSpec::Integer;
    let ok = TypeSpec::Tuple(Some("Ok"), vec![]);
    let nil = TypeSpec::Tuple(None, vec![]);
    vec![
        (
            "dns_resolve",
            TypeSpec::Tuple(None, vec![(None, bin.clone())]),
            dns.clone(),
        ),
        (
            "dns_next",
            TypeSpec::Tuple(None, vec![(None, dns.clone())]),
            TypeSpec::Union(vec![bin.clone(), nil]),
        ),
        (
            "dns_close",
            TypeSpec::Tuple(None, vec![(None, dns)]),
            ok.clone(),
        ),
        (
            "tcp_connect",
            TypeSpec::Tuple(None, vec![(None, bin.clone()), (None, int.clone())]),
            socket.clone(),
        ),
        (
            "tcp_listen",
            TypeSpec::Tuple(None, vec![(None, int.clone()), (None, int.clone())]),
            listener.clone(),
        ),
        (
            "tcp_socket_read",
            TypeSpec::Tuple(None, vec![(None, socket.clone()), (None, int.clone())]),
            bin.clone(),
        ),
        (
            "tcp_socket_write",
            TypeSpec::Tuple(None, vec![(None, socket.clone()), (None, bin)]),
            int,
        ),
        (
            "tcp_socket_close",
            TypeSpec::Tuple(None, vec![(None, socket.clone())]),
            ok.clone(),
        ),
        (
            "tcp_listener_accept",
            TypeSpec::Tuple(None, vec![(None, listener.clone())]),
            socket,
        ),
        (
            "tcp_listener_close",
            TypeSpec::Tuple(None, vec![(None, listener)]),
            ok,
        ),
    ]
}

/// Placeholder implementation for an IO builtin registered for its signature only (e.g. by the
/// language server, which type-checks but never executes). It is never called — executing hosts
/// register real implementations against these signatures instead.
fn unimplemented_builtin<E: Effect>(
    _: ProcessId,
    _: &Value,
    _: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    unreachable!("IO builtin registered for its signature only; no implementation in this host")
}

/// Register the IO builtins' type signatures (no implementations) — so code using `__file_read__`,
/// `%file`, `%dns`, etc. type-checks in a host that doesn't run effects.
pub fn register_io_signatures<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    for (name, param, result) in file_signatures().into_iter().chain(network_signatures()) {
        let placeholder: BuiltinFn<E> = unimplemented_builtin::<E>;
        registry.register(name.to_string(), placeholder, param, result);
    }
}
