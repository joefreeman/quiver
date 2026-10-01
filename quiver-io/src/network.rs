use crate::effects::NativeEffect;
use crate::util::{binary_bytes, expect_resource};
use quiver_core::builtins::{BuiltinContext, BuiltinFn, BuiltinRegistry, Completion, value_to_i64};
use quiver_core::error::Error;
use quiver_core::value::Value;

/// dns_resolve(hostname: bin) -> Resource<DnsResolver>
/// Start DNS resolution for a hostname (UTF-8 bytes), returning an iterator resource
pub fn builtin_dns_resolve(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    Ok(Completion::Effect(NativeEffect::DnsResolve {
        hostname: binary_bytes(value, ctx)?,
    }))
}

/// dns_next(resolver: Resource<DnsResolver>) -> bin | Nil
/// Get the next IP address from a DNS resolver, or Nil if exhausted
pub fn builtin_dns_next(
    value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let resource_id = expect_resource(value)?;

    Ok(Completion::Effect(NativeEffect::DnsNext { resource_id }))
}

/// tcp_connect([ip: bin, port: int]) -> Resource<TcpSocket>
/// Connect to a TCP server using a raw IP address (4 bytes for IPv4, 16 bytes for IPv6)
pub fn builtin_tcp_connect(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    // Extract [ip, port] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 2 {
        return Err(Error::ArityMismatch {
            expected: 2,
            found: fields.len(),
        });
    }

    // Get IP bytes
    let ip_bytes = binary_bytes(&fields[0], ctx)?;

    // Get port
    let port = value_to_i64(&fields[1])?;

    if !(0..=65535).contains(&port) {
        return Err(Error::InvalidArgument(format!(
            "Port must be between 0 and 65535, got {}",
            port
        )));
    }

    // Validate IP address length (4 for IPv4, 16 for IPv6)
    if ip_bytes.len() != 4 && ip_bytes.len() != 16 {
        return Err(Error::InvalidArgument(format!(
            "IP address must be 4 bytes (IPv4) or 16 bytes (IPv6), got {} bytes",
            ip_bytes.len()
        )));
    }

    // Return Action to request TCP connect from Environment
    Ok(Completion::Effect(NativeEffect::TcpConnect {
        ip: ip_bytes,
        port: port as u16,
    }))
}

/// tcp_listen([port: int, backlog: int]) -> Resource<TcpListener>
/// Create a TCP listener
pub fn builtin_tcp_listen(
    value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    // Extract [port, backlog] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 2 {
        return Err(Error::ArityMismatch {
            expected: 2,
            found: fields.len(),
        });
    };

    // Get port
    let port = value_to_i64(&fields[0])?;

    // Get backlog
    let backlog = value_to_i64(&fields[1])? as i32;

    if !(0..=65535).contains(&port) {
        return Err(Error::InvalidArgument(format!(
            "Port must be between 0 and 65535, got {}",
            port
        )));
    }

    // Return Action to request TCP listener from Environment
    Ok(Completion::Effect(NativeEffect::TcpListen {
        port: port as u16,
        backlog,
    }))
}

/// io_write([writable, data]) -> Ok
/// Write every byte to a socket or sink (async).
pub fn builtin_io_write(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    // Extract [writable, data] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 2 {
        return Err(Error::ArityMismatch {
            expected: 2,
            found: fields.len(),
        });
    }

    let resource_id = match &fields[0] {
        Value::Resource(id, _) => *id,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "resource".to_string(),
                found: fields[0].type_name().to_string(),
            });
        }
    };

    // Return Action to request write operation from Environment
    Ok(Completion::Effect(NativeEffect::Write {
        resource_id,
        data: binary_bytes(&fields[1], ctx)?,
    }))
}

/// io_close(resource) -> Ok
/// Close any resource (async).
pub fn builtin_io_close(
    value: &Value,
    _ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let resource_id = expect_resource(value)?;

    // Return Action to request close operation from Environment
    Ok(Completion::Effect(NativeEffect::Close { resource_id }))
}

/// Register network builtins (TCP operations)
/// Attach the native (io-uring/socket2) implementations of the network builtins. Their signatures
/// are part of the universal contract (registered everywhere via `core_modules`); this backs them
/// with a real runtime for an executing host.
pub fn attach_network_builtins(registry: &mut BuiltinRegistry<NativeEffect>) {
    let implementations: [(&str, BuiltinFn<NativeEffect>); 6] = [
        ("dns_resolve", builtin_dns_resolve),
        ("dns_next", builtin_dns_next),
        ("tcp_connect", builtin_tcp_connect),
        ("tcp_listen", builtin_tcp_listen),
        ("io_write", builtin_io_write),
        ("io_close", builtin_io_close),
    ];
    for (name, impl_fn) in implementations {
        registry.attach_implementation(name, impl_fn);
    }
}

// --- TLS builtins ---------------------------------------------------------------------------
//
// Thin: each packages its arguments into an effect and parks. All the state-machine work is in
// the backend, because that is what keeps the ciphertext out of Quiver's heap — see
// `native_backend`'s TLS section.

pub fn builtin_tls_attach(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };
    if fields.len() != 3 {
        return Err(Error::ArityMismatch {
            expected: 3,
            found: fields.len(),
        });
    }
    let resource_id = expect_resource(&fields[0])?;
    Ok(Completion::Effect(NativeEffect::TlsAttach {
        resource_id,
        hostname: binary_bytes(&fields[1], ctx)?,
        roots: binary_bytes(&fields[2], ctx)?,
    }))
}

pub fn builtin_tls_accept(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };
    if fields.len() != 3 {
        return Err(Error::ArityMismatch {
            expected: 3,
            found: fields.len(),
        });
    }
    let resource_id = expect_resource(&fields[0])?;
    Ok(Completion::Effect(NativeEffect::TlsAccept {
        resource_id,
        cert: binary_bytes(&fields[1], ctx)?,
        key: binary_bytes(&fields[2], ctx)?,
    }))
}

/// Attach the native TLS builtins. Registered separately from the network group: a host may
/// have sockets without TLS.
pub fn attach_tls_builtins(registry: &mut BuiltinRegistry<NativeEffect>) {
    let implementations: [(&str, BuiltinFn<NativeEffect>); 2] = [
        ("tls_attach", builtin_tls_attach),
        ("tls_accept", builtin_tls_accept),
    ];
    for (name, impl_fn) in implementations {
        registry.attach_implementation(name, impl_fn);
    }
}
