//! Type contract for the IO builtins (file and network).
//!
//! The signatures live here — in the type-system authority — independently of any
//! implementation: `__file_read__: [+File, 'int, 'int] -> 'bin` is true regardless of which host
//! provides the runtime. A type-checking host (the language server) registers the signatures
//! alone via [`register_io_signatures`]; an executing host registers the same signatures paired
//! with its own implementations (e.g. `quiver-io`'s native io-uring backend, or a web backend).

use super::{BuiltinRegistry, Purity, TypeSpec};
use crate::effects::Effect;

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

/// The TLS builtins' contract: a connected socket, upgraded **in place**.
///
/// `__tls_attach__` (client side, verifying against `roots`) and `__tls_accept__` (server
/// side, presenting `cert`/`key`) install TLS on the socket they are given and answer that
/// same socket — the kTLS model, where encryption is a property of the socket rather than a
/// resource kind. The ordinary socket operations then speak plaintext through it: reads
/// decrypt, writes encrypt, close sends a close_notify first, and a select yields decrypted
/// `Data` events. Nothing above the socket needs to know, which is what lets the HTTP
/// server, websockets and live views run over TLS unchanged — and since the upgrade leaves
/// no second handle behind, there is no way to reach the ciphertext stream at all.
///
/// Neither builtin connects or accepts. They take a socket somebody else made, which is what
/// lets `%tls` compose resolve-then-connect-then-attach in Quiver, and what leaves room for
/// STARTTLS and proxy `CONNECT` without further builtins.
///
/// All certificate material crosses as DER bytes — no filesystem access, no environment
/// variables, nothing library-specific; PEM is decoded by the caller. `roots` is trust
/// anchors as concatenated DER certificates, and **empty means the host's own defaults**, so
/// the ordinary client carries no certificate data at all; supplying a set replaces them
/// outright, which is the only way a locally-issued certificate can be trusted (and so the
/// only way TLS is testable without the public internet). `cert` is the presented chain,
/// leaf first, and `key` its PKCS#8 private key.
fn tls_signatures() -> Vec<(&'static str, TypeSpec, TypeSpec)> {
    let socket = TypeSpec::Resource("TcpSocket".to_string());
    let bin = TypeSpec::Binary;
    vec![
        (
            "tls_attach",
            TypeSpec::Tuple(
                None,
                vec![
                    (Some("socket"), socket.clone()),
                    (Some("hostname"), bin.clone()),
                    (Some("roots"), bin.clone()),
                ],
            ),
            socket.clone(),
        ),
        (
            "tls_accept",
            TypeSpec::Tuple(
                None,
                vec![
                    (Some("socket"), socket.clone()),
                    (Some("cert"), bin.clone()),
                    (Some("key"), bin),
                ],
            ),
            socket,
        ),
    ]
}

/// Register the TLS builtins' signatures. Its own group so a host with sockets but no TLS
/// backend is representable — there, `__tls_attach__` is a signature never attached.
pub fn register_tls_signatures<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    for (name, param, result) in tls_signatures() {
        registry.register_signature(name.to_string(), Purity::Effect, param, fallible(result));
    }
    register_error_vocabulary(registry);
}

/// The http_request builtin's contract: an HTTP exchange as a single primitive, mediated by the
/// host. A browser backs it with its `fetch` API — its io floor, since it cannot open a
/// socket — and a native host backs it over its own sockets, so the capability is
/// universal while what mediates it (redirect following, which response headers are
/// visible) legitimately differs per host.
///
/// Method and headers cross as bytes (a raw CRLF block) rather than as structured values: the
/// backend has no type registry, so a `'%http.pairs` would mean plumbing tuple ids for `Cons`,
/// `Nil` and `Str` through `set_type_ids` — while `%http` already has the header codec both
/// ways. The response body is a `+ByteStream`.
fn http_request_signatures() -> Vec<(&'static str, TypeSpec, TypeSpec)> {
    let bin = TypeSpec::Binary;
    let int = TypeSpec::Integer;
    let body = TypeSpec::Resource("ByteStream".to_string());
    vec![(
        "http_request",
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

/// Register `+ByteStream`: the general verb-free stream of byte chunks. A socket is a
/// stream *and* a bundle of operations, so it earns its own kind; a source that is
/// nothing but "chunks until a clean end" — an HTTP response body today; a streamed
/// request body or a child process's output tomorrow — is a `+ByteStream`, whatever
/// produced it. One kind is what lets one consumer drain them all. Every capability
/// group whose operations mint one declares it (the registration is keyed by kind, so
/// repeats agree harmlessly); which group *minted* a given handle is the operation's
/// business, not the type's.
pub fn register_byte_stream<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let stream = TypeSpec::Resource("ByteStream".to_string());
    registry.register_stream(
        "ByteStream",
        crate::builtins::StreamSpec {
            data: Some(TypeSpec::Tuple(
                Some("Data"),
                vec![
                    (Some("stream"), stream.clone()),
                    (Some("data"), TypeSpec::Binary),
                ],
            )),
            resource: None,
            end: TypeSpec::Tuple(Some("Closed"), vec![(Some("stream"), stream)]),
        },
    );
}

/// Register the http_request builtin's signature — the mediated-HTTP capability.
pub fn register_http_signatures<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    for (name, param, result) in http_request_signatures() {
        registry.register_signature(name.to_string(), Purity::Effect, param, fallible(result));
    }
    register_byte_stream(registry);
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
        // time_now([]) -> int: nanoseconds since the Unix epoch (UTC)
        ("time_now", nil.clone(), int.clone()),
        // time_monotonic([]) -> int: monotonic nanoseconds from an arbitrary origin —
        // for measuring durations; unrelated to (and steadier than) the wall clock
        ("time_monotonic", nil, int),
    ]
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
    for (name, param, result) in file_signatures().into_iter().chain(network_signatures()) {
        registry.register_signature(name.to_string(), Purity::Effect, param, fallible(result));
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
    for (name, param, result) in system_signatures() {
        registry.register_signature(name.to_string(), Purity::HostRead, param, result);
    }
}
