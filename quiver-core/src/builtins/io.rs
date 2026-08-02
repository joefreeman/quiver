//! Type contract for the IO builtins (file and network).
//!
//! The signatures live here — in the type-system authority — independently of any
//! implementation: `__file_read__: [\File, 'int, 'int] -> 'bin` is true regardless of which host
//! provides the runtime. A type-checking host (the language server) registers the signatures
//! alone via [`register_io_signatures`]; an executing host registers the same signatures paired
//! with its own implementations (e.g. `quiver-io`'s native io-uring backend, or a web backend).

use super::{BuiltinContext, BuiltinFn, BuiltinRegistry, Completion, Purity, TypeSpec};
use crate::effects::Effect;
use crate::error::Error;
use crate::value::Value;

/// The file builtins' contract: `(name, parameter, result)` for each.
fn file_signatures() -> Vec<(&'static str, TypeSpec, TypeSpec)> {
    let file = TypeSpec::Resource("File".to_string());
    let dir = TypeSpec::Resource("Dir".to_string());
    let bin = TypeSpec::Binary;
    let int = TypeSpec::Integer;
    let ok = TypeSpec::Tuple(Some("Ok"), vec![]);
    let nil = TypeSpec::Tuple(None, vec![]);
    // A path's kind, as a tag (mirrors `std/fs.qv`'s `'kind`). The directory and stat builtins
    // produce this directly, so the stdlib no longer remaps an int code.
    let kind = TypeSpec::Union(vec![
        TypeSpec::Tuple(Some("File"), vec![]),
        TypeSpec::Tuple(Some("Dir"), vec![]),
        TypeSpec::Tuple(Some("Symlink"), vec![]),
        TypeSpec::Tuple(Some("Other"), vec![]),
    ]);
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
                vec![
                    (None, file.clone()),
                    (None, int.clone()),
                    (None, bin.clone()),
                ],
            ),
            int.clone(),
        ),
        // file_flush(File) -> Ok
        ("file_flush", file.clone(), ok.clone()),
        // file_close(File) -> Ok
        ("file_close", file, ok.clone()),
        // directory_read(path) -> Dir
        ("directory_read", bin.clone(), dir.clone()),
        // directory_next(Dir) -> [bin, kind] | nil  (entry name + kind tag)
        (
            "directory_next",
            dir.clone(),
            TypeSpec::Union(vec![
                TypeSpec::Tuple(None, vec![(None, bin.clone()), (None, kind.clone())]),
                nil.clone(),
            ]),
        ),
        // directory_close(Dir) -> Ok
        ("directory_close", dir, ok),
        // filesystem_stat(path) -> [kind, size, modified, mode] | nil
        // A path that is not there answers nil, like any other lookup that finds nothing;
        // `fallible` folds a *failed* lookup onto the same nil, told apart by its `:error`.
        (
            "filesystem_stat",
            bin,
            TypeSpec::Tuple(
                None,
                vec![
                    (None, kind),
                    (None, int.clone()),
                    (None, int.clone()),
                    (None, int),
                ],
            ),
        ),
    ]
}

/// A result widened with the failure nil. Every effect builtin is fallible: its failure
/// depends on the world, so no caller-side guard could avoid it and the only place a value
/// can come from is the builtin (see `EffectFailure`). Flattens, so a result that already
/// admits nil — `directory_next`'s end-of-iteration, say — gains nothing: exhaustion and
/// failure *are* the same nil, told apart by the `:error` stamp.
fn fallible(result: TypeSpec) -> TypeSpec {
    let mut members = match result {
        TypeSpec::Union(members) => members,
        other => vec![other],
    };
    let has_nil = members
        .iter()
        .any(|member| matches!(member, TypeSpec::Tuple(None, fields) if fields.is_empty()));
    if !has_nil {
        members.push(TypeSpec::Tuple(None, vec![]));
    }
    TypeSpec::Union(members)
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
        ("dns_resolve", bin.clone(), dns.clone()),
        (
            "dns_next",
            dns.clone(),
            TypeSpec::Union(vec![bin.clone(), nil]),
        ),
        ("dns_close", dns, ok.clone()),
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
        ("tcp_socket_close", socket.clone(), ok.clone()),
        ("tcp_listener_accept", listener.clone(), socket),
        ("tcp_listener_close", listener, ok),
    ]
}

/// The TLS builtins' contract: a connected socket, upgraded.
///
/// `attach` deliberately does *not* connect. It takes a socket somebody else made, which is
/// what lets `%tls` compose resolve-then-connect-then-attach in Quiver, and what leaves room
/// for STARTTLS and proxy `CONNECT` without a second builtin. The socket is consumed: after a
/// successful attach only the `\TlsSocket` may be used.
///
/// `roots` is the trust anchors as concatenated DER certificates — no filesystem access, no
/// environment variables, nothing library-specific. **Empty means the host's own defaults**,
/// so the ordinary case carries no certificate data at all; supplying a set replaces them
/// outright, which is the only way a locally-issued certificate can be trusted (and so the
/// only way TLS is testable without the public internet).
fn tls_signatures() -> Vec<(&'static str, TypeSpec, TypeSpec)> {
    let socket = TypeSpec::Resource("TcpSocket".to_string());
    let tls = TypeSpec::Resource("TlsSocket".to_string());
    let bin = TypeSpec::Binary;
    let int = TypeSpec::Integer;
    let ok = TypeSpec::Tuple(Some("Ok"), vec![]);
    vec![
        (
            "tls_attach",
            TypeSpec::Tuple(
                None,
                vec![
                    (Some("socket"), socket),
                    (Some("hostname"), bin.clone()),
                    (Some("roots"), bin.clone()),
                ],
            ),
            tls.clone(),
        ),
        (
            "tls_read",
            TypeSpec::Tuple(None, vec![(None, tls.clone()), (None, int.clone())]),
            bin.clone(),
        ),
        (
            "tls_write",
            TypeSpec::Tuple(None, vec![(None, tls.clone()), (None, bin)]),
            int,
        ),
        ("tls_close", tls, ok),
    ]
}

/// Register the TLS builtins' signatures. Its own group: a host may have sockets without TLS,
/// and a transport naming `__tls_attach__` should fail to compile there rather than at runtime.
///
/// Deliberately *no* stream registration: a `\TlsSocket` is not selectable yet. An armed read
/// completes with ciphertext, so serving a select needs a decrypting completion path (plus
/// buffered plaintext answering ahead of any socket read) that the backend does not have —
/// registering the stream spec would compile `![sock]` against a runtime fault. Until it
/// exists, `__tls_read__` blocks.
pub fn register_tls_signatures<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let placeholder: BuiltinFn<E> = unimplemented_builtin::<E>;
    for (name, param, result) in tls_signatures() {
        registry.register(
            name.to_string(),
            placeholder,
            Purity::Effect,
            param,
            fallible(result),
        );
    }
    register_error_vocabulary(registry);
}

/// The fetch builtin's contract: an HTTP exchange as a single primitive.
///
/// This is a *browser's* floor, not a general one. A host with sockets builds HTTP in Quiver
/// over `__tcp_*__`; a browser cannot, so it gets the whole exchange as one effect. The two
/// are deliberately separate capability groups — a host offering both would let a program
/// compile against `fetch` and then run somewhere it means something different.
///
/// Method and headers cross as bytes (a raw CRLF block) rather than as structured values: the
/// backend has no type registry, so a `'%http.pairs` would mean plumbing tuple ids for `Cons`,
/// `Nil` and `Str` through `set_type_ids` — while `%http` already has the header codec both
/// ways. The response body is a `\HttpBody` stream, the same shape as a socket's.
fn fetch_signatures() -> Vec<(&'static str, TypeSpec, TypeSpec)> {
    let bin = TypeSpec::Binary;
    let int = TypeSpec::Integer;
    let body = TypeSpec::Resource("HttpBody".to_string());
    vec![(
        "fetch",
        TypeSpec::Tuple(
            None,
            vec![
                (Some("method"), bin.clone()),
                (Some("url"), bin.clone()),
                (Some("headers"), bin.clone()),
                (Some("body"), bin),
            ],
        ),
        TypeSpec::Tuple(
            None,
            vec![
                (Some("status"), int),
                (Some("headers"), TypeSpec::Binary),
                (Some("body"), body),
            ],
        ),
    )]
}

/// A fetch response body is a stream, exactly as a socket is: bytes arrive when they arrive,
/// so it is selectable and yields the same `Data`/`Closed` shape.
fn register_fetch_streams<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let body = TypeSpec::Resource("HttpBody".to_string());
    registry.register_stream(
        "HttpBody",
        crate::builtins::StreamSpec {
            data: Some(TypeSpec::Tuple(
                Some("Data"),
                vec![
                    (Some("body"), body.clone()),
                    (Some("data"), TypeSpec::Binary),
                ],
            )),
            resource: None,
            end: TypeSpec::Tuple(Some("Closed"), vec![(Some("body"), body)]),
        },
    );
}

/// Register the fetch builtin's signature (no implementation) — the browser's io capability.
pub fn register_fetch_signatures<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let placeholder: BuiltinFn<E> = unimplemented_builtin::<E>;
    for (name, param, result) in fetch_signatures() {
        registry.register(
            name.to_string(),
            placeholder,
            Purity::Effect,
            param,
            fallible(result),
        );
    }
    register_fetch_streams(registry);
    register_error_vocabulary(registry);
}

/// The system builtins' contract: host-provided entropy and clocks.
fn system_signatures() -> Vec<(&'static str, TypeSpec, TypeSpec)> {
    let bin = TypeSpec::Binary;
    let int = TypeSpec::Integer;
    let nil = TypeSpec::Tuple(None, vec![]);
    vec![
        // random_bytes(n) -> bin: n cryptographically secure random bytes
        ("random_bytes", int.clone(), bin),
        // time_now([]) -> int: milliseconds since the Unix epoch (UTC)
        ("time_now", nil.clone(), int.clone()),
        // time_monotonic([]) -> int: monotonic milliseconds from an arbitrary origin —
        // for measuring durations; unrelated to (and steadier than) the wall clock
        ("time_monotonic", nil, int),
    ]
}

/// Placeholder implementation for an IO builtin registered for its signature only (e.g. by the
/// language server, which type-checks but never executes). It is never called — executing hosts
/// register real implementations against these signatures instead.
fn unimplemented_builtin<E: Effect>(
    _: &Value,
    _: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    unreachable!("IO builtin registered for its signature only; no implementation in this host")
}

/// The network resource kinds' stream declarations: what a select on each yields.
/// Part of the capability contract, like the signatures — a host registering the
/// network builtins gets selectable sockets/listeners with it.
fn register_network_streams<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let socket = TypeSpec::Resource("TcpSocket".to_string());
    let listener = TypeSpec::Resource("TcpListener".to_string());
    registry.register_stream(
        "TcpSocket",
        crate::builtins::StreamSpec {
            data: Some(TypeSpec::Tuple(
                Some("Data"),
                vec![
                    (Some("sock"), socket.clone()),
                    (Some("data"), TypeSpec::Binary),
                ],
            )),
            resource: None,
            end: TypeSpec::Tuple(Some("Closed"), vec![(Some("sock"), socket.clone())]),
        },
    );
    registry.register_stream(
        "TcpListener",
        crate::builtins::StreamSpec {
            data: None,
            resource: Some((
                TypeSpec::Tuple(
                    Some("Accepted"),
                    vec![(Some("listener"), listener.clone()), (Some("sock"), socket)],
                ),
                "TcpSocket".to_string(),
            )),
            end: TypeSpec::Tuple(Some("Closed"), vec![(Some("listener"), listener)]),
        },
    );
}

/// Register the file and network builtins' type signatures (no implementations) — so code using
/// `__file_read__`, `%file`, `%dns`, etc. type-checks in a host that doesn't run effects. These
/// are all `Purity::Effect`: they park the calling process while the host's backend works.
pub fn register_io_signatures<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let placeholder: BuiltinFn<E> = unimplemented_builtin::<E>;
    for (name, param, result) in file_signatures().into_iter().chain(network_signatures()) {
        registry.register(
            name.to_string(),
            placeholder,
            Purity::Effect,
            param,
            fallible(result),
        );
    }
    register_network_streams(registry);
    register_error_vocabulary(registry);
}

/// The io-failure vocabulary: what a failed effect's nil carries under `:error`. Declared
/// alongside the builtins that can produce one, so a host without io never resolves it.
pub fn register_error_vocabulary<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let str_spec = TypeSpec::Tuple(Some("Str"), vec![(None, TypeSpec::Binary)]);
    let kinds: Vec<TypeSpec> = crate::effects::EffectError::KINDS
        .iter()
        .map(|name| TypeSpec::Tuple(Some(name), vec![]))
        .collect();
    registry.declare_error(crate::builtins::ErrorDecl {
        io_error: TypeSpec::Tuple(
            Some("IoError"),
            vec![
                (Some("kind"), TypeSpec::Union(kinds.clone())),
                (Some("message"), str_spec.clone()),
            ],
        ),
        kinds,
        str: str_spec,
        error_key: "error".to_string(),
    });
}

/// Register the system builtins' type signatures (no implementations): entropy and clocks. These
/// are `Purity::HostRead` — read synchronously, in the calling worker, with no effect round-trip —
/// so a host provides them by attaching implementations alone, with no effect backend. That makes
/// them the one IO group a capability-poor host (a browser) can serve outright.
pub fn register_system_signatures<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let placeholder: BuiltinFn<E> = unimplemented_builtin::<E>;
    for (name, param, result) in system_signatures() {
        registry.register(
            name.to_string(),
            placeholder,
            Purity::HostRead,
            param,
            result,
        );
    }
}
