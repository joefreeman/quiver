use crate::effects::NativeEffect;
use quiver_core::builtins::{BuiltinResult, TypeSpec};
use quiver_core::error::Error;
use quiver_core::executor::Executor;
use quiver_core::process::{Action, ProcessId};
use quiver_core::value::{Binary, Value};

/// tcp_connect([host: bin, port: int, buffer_size: int]) -> Resource<TcpSocket>
/// Connect to a TCP server (async via io_uring)
pub fn builtin_tcp_connect(
    process_id: ProcessId,
    value: &Value,
    executor: &mut Executor<NativeEffect>,
) -> Result<BuiltinResult<NativeEffect>, Error> {
    // Extract [host, port, buffer_size] tuple
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

    // Get host binary (can be either binary or string)
    let host_binary = match &fields[0] {
        Value::Binary(binary) => *binary,
        Value::Tuple(_, str_fields) if str_fields.len() == 1 => {
            // It's a Str[bin] tuple
            match &str_fields[0] {
                Value::Binary(binary) => *binary,
                _ => {
                    return Err(Error::TypeMismatch {
                        expected: "binary or string".to_string(),
                        found: fields[0].type_name().to_string(),
                    });
                }
            }
        }
        _ => {
            return Err(Error::TypeMismatch {
                expected: "binary or string".to_string(),
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

    // Get buffer size
    let Value::Integer(buffer_size) = fields[2] else {
        return Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: fields[2].type_name().to_string(),
        });
    };

    if !(0..=65535).contains(&port) {
        return Err(Error::InvalidArgument(format!(
            "Port must be between 0 and 65535, got {}",
            port
        )));
    }

    if buffer_size <= 0 {
        return Err(Error::InvalidArgument(format!(
            "Buffer size must be positive, got {}",
            buffer_size
        )));
    }

    // Get host bytes from binary
    let host_bytes = match &host_binary {
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
            .heap
            .get(*idx)
            .ok_or_else(|| Error::InvalidArgument(format!("Heap binary index {} not found", idx)))?
            .to_vec(),
    };

    // Return Action to request TCP connect from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::TcpConnect {
            host: host_bytes,
            port: port as u16,
            buffer_size: buffer_size as usize,
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

/// tcp_socket_read([socket]) -> bin
/// Read from a TCP socket (async)
pub fn builtin_tcp_socket_read(
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

    // Return Action to request read operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::Read { resource_id },
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
            .heap
            .get(*idx)
            .ok_or_else(|| Error::InvalidArgument(format!("Heap binary index {} not found", idx)))?
            .to_vec(),
    };

    // Return Action to request write operation from Environment
    Ok(BuiltinResult::Action(Action::RequestEffect {
        process_id,
        effect: NativeEffect::Write { resource_id, data },
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
        effect: NativeEffect::Close { resource_id },
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
        effect: NativeEffect::Accept { resource_id },
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
        effect: NativeEffect::Close { resource_id },
    }))
}

/// Register network builtins (TCP operations)
pub fn register_network_builtins(
    registry: &mut quiver_core::builtins::BuiltinRegistry<NativeEffect>,
) {
    // tcp_connect accepts either binary or string for host parameter
    let host_type = TypeSpec::Union(vec![
        TypeSpec::Binary,
        TypeSpec::Tuple(Some("Str"), vec![(None, TypeSpec::Binary)]),
    ]);
    let bin_int_int_network = TypeSpec::Tuple(
        None,
        vec![
            (None, host_type),
            (None, TypeSpec::Integer),
            (None, TypeSpec::Integer),
        ],
    );
    let int_int_network = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Integer), (None, TypeSpec::Integer)],
    );

    // TCP socket and listener resource types (opaque identifiers)
    let tcp_socket_resource = TypeSpec::Resource("TcpSocket".to_string());
    let tcp_listener_resource = TypeSpec::Resource("TcpListener".to_string());
    let bin_type = TypeSpec::Binary;
    let int_type = TypeSpec::Integer;
    let ok_type = TypeSpec::Tuple(Some("Ok"), vec![]);

    // Connection and listener creation
    registry.register(
        "tcp_connect".to_string(),
        builtin_tcp_connect,
        bin_int_int_network,
        tcp_socket_resource.clone(),
    );
    registry.register(
        "tcp_listen".to_string(),
        builtin_tcp_listen,
        int_int_network,
        tcp_listener_resource.clone(),
    );

    // Socket operations
    registry.register(
        "tcp_socket_read".to_string(),
        builtin_tcp_socket_read,
        TypeSpec::Tuple(None, vec![(None, tcp_socket_resource.clone())]),
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
