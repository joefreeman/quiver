use crate::effects::NativeEffect;
use quiver_core::builtins::{BuiltinResult, TypeSpec};
use quiver_core::error::Error;
use quiver_core::executor::Executor;
use quiver_core::process::{Action, ProcessId};
use quiver_core::value::{Binary, Value};

/// dns_resolve([hostname: bin]) -> Resource<DnsResolver>
/// Start DNS resolution for a hostname (UTF-8 bytes), returning an iterator resource
pub fn builtin_dns_resolve(
    process_id: ProcessId,
    value: &Value,
    executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [hostname] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 1 {
        return Err(Error::ArityMismatch {
            expected: 1,
            found: fields.len(),
        });
    }

    // Get hostname binary
    let Value::Binary(hostname_binary) = &fields[0] else {
        return Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: fields[0].type_name().to_string(),
        });
    };

    // Get hostname bytes from binary
    let hostname_bytes = match hostname_binary {
        Binary::Constant(idx) => {
            let constant = executor
                .get_constant(*idx)
                .ok_or(Error::ConstantUndefined(*idx))?;
            match constant {
                quiver_core::bytecode::Constant::Binary(bytes) => bytes.clone(),
                _ => {
                    return Err(Error::TypeMismatch {
                        expected: "binary".to_string(),
                        found: "integer".to_string(),
                    });
                }
            }
        }
        Binary::Heap(idx) => executor
            .get_heap_binary(*idx)
            .ok_or_else(|| Error::InvalidArgument(format!("Heap binary index {} not found", idx)))?
            .to_vec(),
    };

    // Return Action to request DNS resolution from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::DnsResolve {
            hostname: hostname_bytes,
        },
    }))
}

/// dns_next([resolver: Resource<DnsResolver>]) -> bin | Nil
/// Get the next IP address from a DNS resolver, or Nil if exhausted
pub fn builtin_dns_next(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [resolver] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 1 {
        return Err(Error::ArityMismatch {
            expected: 1,
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

    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::DnsNext { resource_id },
    }))
}

/// dns_close([resolver: Resource<DnsResolver>]) -> Ok
/// Close a DNS resolver resource
pub fn builtin_dns_close(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [resolver] tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 1 {
        return Err(Error::ArityMismatch {
            expected: 1,
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

    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::DnsClose { resource_id },
    }))
}

/// tcp_connect([ip: bin, port: int]) -> Resource<TcpSocket>
/// Connect to a TCP server using a raw IP address (4 bytes for IPv4, 16 bytes for IPv6)
pub fn builtin_tcp_connect(
    process_id: ProcessId,
    value: &Value,
    executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
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

    // Get IP binary
    let ip_binary = match &fields[0] {
        Value::Binary(binary) => *binary,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "binary".to_string(),
                found: fields[0].type_name().to_string(),
            });
        }
    };

    // Get port
    let Value::Integer(port) = fields[1] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[1].type_name().to_string(),
        });
    };

    if !(0..=65535).contains(&port) {
        return Err(Error::InvalidArgument(format!(
            "Port must be between 0 and 65535, got {}",
            port
        )));
    }

    // Get IP bytes from binary
    let ip_bytes = match &ip_binary {
        Binary::Constant(idx) => {
            let constant = executor
                .get_constant(*idx)
                .ok_or(Error::ConstantUndefined(*idx))?;
            match constant {
                quiver_core::bytecode::Constant::Binary(bytes) => bytes.clone(),
                _ => {
                    return Err(Error::TypeMismatch {
                        expected: "binary".to_string(),
                        found: "integer".to_string(),
                    });
                }
            }
        }
        Binary::Heap(idx) => executor
            .get_heap_binary(*idx)
            .ok_or_else(|| Error::InvalidArgument(format!("Heap binary index {} not found", idx)))?
            .to_vec(),
    };

    // Validate IP address length (4 for IPv4, 16 for IPv6)
    if ip_bytes.len() != 4 && ip_bytes.len() != 16 {
        return Err(Error::InvalidArgument(format!(
            "IP address must be 4 bytes (IPv4) or 16 bytes (IPv6), got {} bytes",
            ip_bytes.len()
        )));
    }

    // Return Action to request TCP connect from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::TcpConnect {
            ip: ip_bytes,
            port: port as u16,
        },
    }))
}

/// tcp_listen([port: int, backlog: int]) -> Resource<TcpListener>
/// Create a TCP listener
pub fn builtin_tcp_listen(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
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
    let Value::Integer(port) = fields[0] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[0].type_name().to_string(),
        });
    };

    // Get backlog
    let Value::Integer(backlog) = fields[1] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[1].type_name().to_string(),
        });
    };

    if !(0..=65535).contains(&port) {
        return Err(Error::InvalidArgument(format!(
            "Port must be between 0 and 65535, got {}",
            port
        )));
    }

    // Return Action to request TCP listener from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::TcpListen {
            port: port as u16,
            backlog: backlog as i32,
        },
    }))
}

/// tcp_socket_read([socket, length]) -> bin
/// Read from a TCP socket (async)
pub fn builtin_tcp_socket_read(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [socket, length] tuple
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

    let Value::Integer(length) = fields[1] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[1].type_name().to_string(),
        });
    };

    if length <= 0 {
        return Err(Error::InvalidArgument(format!(
            "Length must be positive, got {}",
            length
        )));
    }

    // Return Action to request read operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::TcpSocketRead {
            resource_id,
            length: length as usize,
        },
    }))
}

/// tcp_socket_write([socket, data]) -> int
/// Write to a TCP socket (async), returns bytes written
pub fn builtin_tcp_socket_write(
    process_id: ProcessId,
    value: &Value,
    executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [socket, data] tuple
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

    // Get data binary
    let data_binary = match &fields[1] {
        Value::Binary(binary) => *binary,
        _ => {
            return Err(Error::TypeMismatch {
                expected: "binary".to_string(),
                found: fields[1].type_name().to_string(),
            });
        }
    };

    // Get data bytes
    let data = match &data_binary {
        Binary::Constant(idx) => {
            let constant = executor
                .get_constant(*idx)
                .ok_or(Error::ConstantUndefined(*idx))?;
            match constant {
                quiver_core::bytecode::Constant::Binary(bytes) => bytes.clone(),
                _ => {
                    return Err(Error::TypeMismatch {
                        expected: "binary".to_string(),
                        found: "integer".to_string(),
                    });
                }
            }
        }
        Binary::Heap(idx) => executor
            .get_heap_binary(*idx)
            .ok_or_else(|| Error::InvalidArgument(format!("Heap binary index {} not found", idx)))?
            .to_vec(),
    };

    // Return Action to request write operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::TcpSocketWrite { resource_id, data },
    }))
}

/// tcp_socket_close([socket]) -> Ok
/// Close a TCP socket (async)
pub fn builtin_tcp_socket_close(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract socket resource from tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 1 {
        return Err(Error::ArityMismatch {
            expected: 1,
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

    // Return Action to request close operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::TcpSocketClose { resource_id },
    }))
}

/// tcp_listener_accept([listener]) -> TcpSocket
/// Accept a connection on a TCP listener (async)
pub fn builtin_tcp_listener_accept(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract listener resource from tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 1 {
        return Err(Error::ArityMismatch {
            expected: 1,
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

    // Return Action to request accept operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::TcpListenerAccept { resource_id },
    }))
}

/// tcp_listener_close([listener]) -> Ok
/// Close a TCP listener (async)
pub fn builtin_tcp_listener_close(
    process_id: ProcessId,
    value: &Value,
    _executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract listener resource from tuple
    let Value::Tuple(_, fields) = value else {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: value.type_name().to_string(),
        });
    };

    if fields.len() != 1 {
        return Err(Error::ArityMismatch {
            expected: 1,
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

    // Return Action to request close operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::TcpListenerClose { resource_id },
    }))
}

/// Register network builtins (TCP operations)
pub fn register_network_builtins(
    registry: &mut quiver_core::builtins::BuiltinRegistry<NativeEffect>,
) {
    // dns_resolve accepts binary (UTF-8 hostname)
    let hostname_tuple = TypeSpec::Tuple(None, vec![(None, TypeSpec::Binary)]);

    // tcp_connect accepts raw IP binary (4 bytes IPv4, 16 bytes IPv6)
    let ip_port_tuple = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Binary), (None, TypeSpec::Integer)],
    );
    let int_int_tuple = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Integer), (None, TypeSpec::Integer)],
    );

    // Resource types (opaque identifiers)
    let dns_resolver_resource = TypeSpec::Resource("DnsResolver".to_string());
    let tcp_socket_resource = TypeSpec::Resource("TcpSocket".to_string());
    let tcp_listener_resource = TypeSpec::Resource("TcpListener".to_string());
    let bin_type = TypeSpec::Binary;
    let int_type = TypeSpec::Integer;
    let ok_type = TypeSpec::Tuple(Some("Ok"), vec![]);
    let nil_type = TypeSpec::Tuple(None, vec![]);

    // DNS resolution (iterator-based)
    registry.register(
        "dns_resolve".to_string(),
        builtin_dns_resolve,
        hostname_tuple.clone(),
        dns_resolver_resource.clone(),
    );
    registry.register(
        "dns_next".to_string(),
        builtin_dns_next,
        TypeSpec::Tuple(None, vec![(None, dns_resolver_resource.clone())]),
        TypeSpec::Union(vec![bin_type.clone(), nil_type.clone()]),
    );
    registry.register(
        "dns_close".to_string(),
        builtin_dns_close,
        TypeSpec::Tuple(None, vec![(None, dns_resolver_resource)]),
        ok_type.clone(),
    );

    // Connection and listener creation
    registry.register(
        "tcp_connect".to_string(),
        builtin_tcp_connect,
        ip_port_tuple,
        tcp_socket_resource.clone(),
    );
    registry.register(
        "tcp_listen".to_string(),
        builtin_tcp_listen,
        int_int_tuple,
        tcp_listener_resource.clone(),
    );

    // Socket operations
    // tcp_socket_read([socket, length]) -> bin
    registry.register(
        "tcp_socket_read".to_string(),
        builtin_tcp_socket_read,
        TypeSpec::Tuple(
            None,
            vec![
                (None, tcp_socket_resource.clone()),
                (None, int_type.clone()),
            ],
        ),
        bin_type.clone(),
    );
    registry.register(
        "tcp_socket_write".to_string(),
        builtin_tcp_socket_write,
        TypeSpec::Tuple(
            None,
            vec![(None, tcp_socket_resource.clone()), (None, bin_type)],
        ),
        int_type,
    );
    registry.register(
        "tcp_socket_close".to_string(),
        builtin_tcp_socket_close,
        TypeSpec::Tuple(None, vec![(None, tcp_socket_resource.clone())]),
        ok_type.clone(),
    );

    // Listener operations
    registry.register(
        "tcp_listener_accept".to_string(),
        builtin_tcp_listener_accept,
        TypeSpec::Tuple(None, vec![(None, tcp_listener_resource.clone())]),
        tcp_socket_resource,
    );
    registry.register(
        "tcp_listener_close".to_string(),
        builtin_tcp_listener_close,
        TypeSpec::Tuple(None, vec![(None, tcp_listener_resource)]),
        ok_type,
    );
}
