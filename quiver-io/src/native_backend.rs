use crate::effects::NativeEffect;
use io_uring::{IoUring as IoUringRing, opcode, types};
use quiver_core::ProcessId;
use quiver_core::effects::{EffectBackend, EffectError, EffectResult, ResultTupleInfo};
use quiver_core::error::Error;
use quiver_core::process::StreamEvent;
use quiver_core::value::ResourceId;
use quiver_core::wire::WireValue;
use socket2::Socket;
use std::collections::HashMap;
use std::fs::File;
use std::io::ErrorKind;
use std::net::{SocketAddr, ToSocketAddrs};
use std::os::fd::FromRawFd;
use std::os::unix::ffi::OsStringExt;
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
use std::os::unix::io::{AsRawFd, RawFd};
use std::sync::Arc;

/// Classify an OS error as an effect outcome. The distinction matters now that outcomes are
/// values: an `EffectError` returned *in* a completion resumes the process with a
/// `:error`-stamped nil, whereas an `Error` returned from `execute` is a submit failure and
/// kills it. Anything the world can do to us — a missing path, a refused connection — belongs
/// on this side.
fn effect_error(context: &str, error: &std::io::Error) -> EffectError {
    let message = format!("{context}: {error}");
    match error.kind() {
        ErrorKind::NotFound => EffectError::NotFound(message),
        ErrorKind::PermissionDenied => EffectError::PermissionDenied(message),
        ErrorKind::AlreadyExists => EffectError::AlreadyExists(message),
        ErrorKind::ConnectionRefused => EffectError::ConnectionRefused(message),
        ErrorKind::WouldBlock => EffectError::WouldBlock,
        ErrorKind::Interrupted => EffectError::Interrupted,
        _ => EffectError::IO(message),
    }
}

/// Classify a failed io_uring completion (`result_code` is `-errno`) as an effect outcome —
/// the completion-side twin of [`effect_error`], for the armed (select-serving) paths.
fn completion_error(context: &str, result_code: i32) -> EffectError {
    effect_error(context, &std::io::Error::from_raw_os_error(-result_code))
}

/// Unwrap an OS result, or return early with an effect *outcome* (a value the caller can
/// branch on) rather than a submit failure (which kills the process).
macro_rules! try_io {
    ($expr:expr, $context:expr) => {
        match $expr {
            Ok(value) => value,
            Err(error) => return Ok(Some(Err(effect_error($context, &error)))),
        }
    };
}

/// Resource metadata stored by the io_uring backend
pub enum Resource {
    TcpSocket {
        socket: Socket,
        peer_addr: SocketAddr,
        /// TLS state once the socket has been upgraded in place (`__tls_attach__` /
        /// `__tls_accept__`). Encryption is a *property of the socket* — the kTLS model —
        /// not a second resource kind: the ordinary socket reads, writes, closes and
        /// selects then speak plaintext through the same handle, and there is no separate
        /// handle left on which ciphertext could be reached at all.
        tls: Option<Box<TlsState>>,
    },
    TcpListener {
        socket: Socket,
        local_addr: SocketAddr,
    },
    File {
        file: File,
        path: String,
    },
    Dir {
        /// Lazy iterator over the directory's entries.
        entries: std::fs::ReadDir,
    },
    DnsResolver {
        /// Resolved IP addresses (4 bytes for IPv4, 16 bytes for IPv6)
        addresses: Vec<Vec<u8>>,
        /// Current position in the iterator
        position: usize,
    },
}

impl Resource {
    fn fd(&self) -> RawFd {
        match self {
            Resource::TcpSocket { socket, .. } => socket.as_raw_fd(),
            Resource::TcpListener { socket, .. } => socket.as_raw_fd(),
            Resource::File { file, .. } => file.as_raw_fd(),
            Resource::Dir { .. } => panic!("Dir does not have a file descriptor"),
            Resource::DnsResolver { .. } => {
                panic!("DnsResolver does not have a file descriptor")
            }
        }
    }
}

/// Type of I/O operation pending
#[derive(Debug)]
pub enum IoOpType {
    Read {
        buffer: Vec<u8>,
    },
    Write {
        buffer: Vec<u8>, // Keep buffer alive for the duration of the async operation
    },
    Flush,
    Accept,
    Connect {
        socket: Socket,
        peer_addr: SocketAddr,
    },
    /// A socket operation serving a TLS goal. Unlike every other variant, its completion may
    /// submit more work rather than finishing — see `tls_drive`.
    Tls {
        resource_id: ResourceId,
        goal: TlsGoal,
        io: TlsIo,
    },
}

/// Native effect backend using io_uring for async I/O operations
pub struct NativeEffectBackend {
    ring: IoUringRing,
    pending: HashMap<u64, (ProcessId, IoOpType)>,
    next_completion_id: u64,
    /// Resource registry - maps ResourceId to resource metadata
    resources: HashMap<ResourceId, Resource>,
    /// Counter for allocating resource IDs
    next_resource_id: ResourceId,
    /// Mapping from resource type name to type ID (pushed by the environment via `set_type_ids`)
    resource_type_ids: HashMap<String, usize>,
    /// Mapping from builtin name to the type ids of its composite result, so effect results can be
    /// stamped with real type ids (pushed by the environment via `set_type_ids`).
    result_infos: HashMap<String, ResultTupleInfo>,
    /// Select-armed stream reads in flight, keyed by completion id. Unlike `pending`
    /// ops, a completion here becomes a stream event (drained by
    /// `take_stream_events`), not an effect completion — no process is parked on it.
    armed: HashMap<u64, ArmedOp>,
    /// Stream resources with an armed read (dedup: at most one per resource).
    armed_resources: std::collections::HashSet<ResourceId>,
    /// Completed armed reads awaiting `take_stream_events`.
    stream_events: Vec<(ResourceId, usize, StreamEvent, Vec<u8>)>,
}

/// A select-armed stream read in flight: the next-event read of a socket or listener.
#[derive(Debug)]
enum ArmedOp {
    Read {
        resource_id: ResourceId,
        buffer: Vec<u8>,
    },
    Accept {
        resource_id: ResourceId,
    },
}

impl NativeEffectBackend {
    /// Create a new native effect backend with the specified io_uring queue depth
    pub fn new(queue_depth: u32) -> Result<Self, Error> {
        let ring = IoUringRing::new(queue_depth)
            .map_err(|e| Error::InvalidArgument(format!("Failed to create io_uring: {}", e)))?;

        Ok(Self {
            ring,
            pending: HashMap::new(),
            next_completion_id: 1,
            resources: HashMap::new(),
            next_resource_id: 1,
            resource_type_ids: HashMap::new(),
            result_infos: HashMap::new(),
            armed: HashMap::new(),
            armed_resources: std::collections::HashSet::new(),
            stream_events: Vec::new(),
        })
    }

    /// Get the type ID for a resource type name. Falls back to 0 only if the tables haven't been
    /// pushed yet (which shouldn't happen for an executing host — see `set_type_ids`).
    fn get_resource_type_id(&self, name: &str) -> usize {
        *self.resource_type_ids.get(name).unwrap_or(&0)
    }

    /// Turn a completed select-armed read into a stream event. A read of zero bytes is
    /// the peer's FIN — a clean `Closed`; a negative completion is an errno, and answers
    /// the failure it names rather than masquerading as an end of stream. A successful
    /// accept registers the new socket here; the environment records its ownership as it
    /// routes the event.
    fn handle_armed_completion(&mut self, armed: ArmedOp, result_code: i32) {
        match armed {
            ArmedOp::Read {
                resource_id,
                mut buffer,
            } => {
                if matches!(
                    self.resources.get(&resource_id),
                    Some(Resource::TcpSocket { tls: Some(_), .. })
                ) {
                    self.handle_armed_tls_read(resource_id, buffer, result_code);
                    return;
                }
                self.armed_resources.remove(&resource_id);
                let socket_type = self.get_resource_type_id("TcpSocket");
                if result_code < 0 {
                    self.stream_events.push((
                        resource_id,
                        socket_type,
                        StreamEvent::Failed {
                            error: completion_error("read", result_code),
                        },
                        vec![],
                    ));
                } else if result_code == 0 {
                    self.stream_events
                        .push((resource_id, socket_type, StreamEvent::End, vec![]));
                } else {
                    buffer.truncate(result_code as usize);
                    self.stream_events
                        .push((resource_id, socket_type, StreamEvent::Data, buffer));
                }
            }
            ArmedOp::Accept { resource_id } => {
                self.armed_resources.remove(&resource_id);
                let listener_type = self.get_resource_type_id("TcpListener");
                if result_code < 0 {
                    self.stream_events.push((
                        resource_id,
                        listener_type,
                        StreamEvent::Failed {
                            error: completion_error("accept", result_code),
                        },
                        vec![],
                    ));
                    return;
                }
                let socket = unsafe { Socket::from_raw_fd(result_code) };
                let peer_addr = socket
                    .peer_addr()
                    .ok()
                    .and_then(|addr| addr.as_socket())
                    .unwrap_or_else(|| SocketAddr::from(([0, 0, 0, 0], 0)));
                let new_resource_id = self.next_resource_id;
                self.next_resource_id += 1;
                self.resources.insert(
                    new_resource_id,
                    Resource::TcpSocket {
                        socket,
                        peer_addr,
                        tls: None,
                    },
                );
                self.stream_events.push((
                    resource_id,
                    listener_type,
                    StreamEvent::Resource {
                        resource_id: new_resource_id,
                    },
                    vec![],
                ));
            }
        }
    }

    /// Submit the armed (select-serving) read of a stream socket, marking it armed. Also the
    /// re-arm path for an upgraded socket whose last chunk decrypted to nothing.
    fn arm_submit_read(
        &mut self,
        resource_id: ResourceId,
        fd: RawFd,
        mut buffer: Vec<u8>,
    ) -> Result<(), Error> {
        let completion_id = self.next_completion_id;
        self.next_completion_id += 1;
        let read_op = opcode::Read::new(types::Fd(fd), buffer.as_mut_ptr(), buffer.len() as u32)
            .build()
            .user_data(completion_id);
        unsafe {
            self.ring
                .submission()
                .push(&read_op)
                .map_err(|e| Error::InvalidArgument(format!("Failed to submit read: {}", e)))?;
        }
        self.ring
            .submit()
            .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {}", e)))?;
        self.armed.insert(
            completion_id,
            ArmedOp::Read {
                resource_id,
                buffer,
            },
        );
        self.armed_resources.insert(resource_id);
        Ok(())
    }

    /// A completed armed read on a TLS-upgraded socket: the buffer holds ciphertext, and the
    /// event may carry only what it decrypts to. A chunk that ends mid-record decrypts to
    /// nothing — then the read is re-armed rather than delivering an empty event, invisibly
    /// to the selecting process. A read error or a broken record answers `Failed`; the end
    /// of the stream answers what rustls's reader says it was — `Closed` after a
    /// close_notify, `Failed` after a bare TCP close — the same distinction the explicit
    /// read path draws. Anything rustls queued to send back (a key-update reply) stays in
    /// `outgoing` and rides out with the next write or close — this path never writes.
    fn handle_armed_tls_read(
        &mut self,
        resource_id: ResourceId,
        buffer: Vec<u8>,
        result_code: i32,
    ) {
        let socket_type = self.get_resource_type_id("TcpSocket");
        let (event, fd) = {
            let Some(Resource::TcpSocket {
                socket,
                tls: Some(state),
                ..
            }) = self.resources.get_mut(&resource_id)
            else {
                // Closed while the read was in flight; routing drops the event if the
                // owner is gone too.
                self.armed_resources.remove(&resource_id);
                self.stream_events
                    .push((resource_id, socket_type, StreamEvent::End, vec![]));
                return;
            };
            let ingested = if result_code < 0 {
                Err(completion_error("tls read", result_code))
            } else {
                state.ingest(&buffer[..result_code as usize])
            };
            let event = match ingested {
                Err(error) => Some((StreamEvent::Failed { error }, vec![])),
                Ok(()) if !state.plaintext.is_empty() => {
                    Some((StreamEvent::Data, std::mem::take(&mut state.plaintext)))
                }
                Ok(()) if state.eof => Some((state.eof_event(), vec![])),
                Ok(()) => None,
            };
            (event, socket.as_raw_fd())
        };
        match event {
            Some((event, data)) => {
                self.armed_resources.remove(&resource_id);
                self.stream_events
                    .push((resource_id, socket_type, event, data));
            }
            None => {
                // Mid-record: nothing to deliver yet, so the select stays parked and the
                // read is re-armed. A failed re-submit fails the stream — the alternative
                // is a select that waits forever.
                if self.arm_submit_read(resource_id, fd, buffer).is_err() {
                    self.armed_resources.remove(&resource_id);
                    self.stream_events.push((
                        resource_id,
                        socket_type,
                        StreamEvent::Failed {
                            error: EffectError::IO(
                                "tls read: could not re-arm the socket read".to_string(),
                            ),
                        },
                        vec![],
                    ));
                }
            }
        }
    }

    /// The type ids to stamp on the composite result of the named builtin. Errors if they weren't
    /// pushed (a wiring bug — the environment pushes these for every loaded program).
    fn result_info(&self, builtin: &str) -> Result<&ResultTupleInfo, Error> {
        self.result_infos.get(builtin).ok_or_else(|| {
            Error::InvalidArgument(format!(
                "no result type ids registered for builtin `{builtin}`"
            ))
        })
    }
}

/// Build a `kind` tag value (`File`/`Dir`/`Symlink`/`Other`) from its name, using the real tuple
/// ids carried in `info.variants`.
fn kind_tag(info: &ResultTupleInfo, name: &str) -> Result<WireValue, Error> {
    let id = info.variants.get(name).copied().ok_or_else(|| {
        Error::InvalidArgument(format!("no tuple id registered for kind tag `{name}`"))
    })?;
    Ok(WireValue::tuple(id, vec![]))
}

/// EffectBackend implementation for NativeEffect
impl EffectBackend for NativeEffectBackend {
    type E = NativeEffect;

    fn execute(
        &mut self,
        process_id: ProcessId,
        effect: NativeEffect,
    ) -> Result<Option<EffectResult>, Error> {
        match effect {
            // File operations
            NativeEffect::FileOpen { path, flags, mode } => {
                self.execute_file_open(path, flags, mode)
            }
            NativeEffect::FileRead {
                resource_id,
                offset,
                length,
            } => self.execute_file_read(process_id, resource_id, offset, length),
            NativeEffect::FileWrite {
                resource_id,
                offset,
                data,
            } => self.execute_file_write(process_id, resource_id, offset, data),
            NativeEffect::FileFlush { resource_id } => {
                self.execute_file_flush(process_id, resource_id)
            }
            NativeEffect::FileClose { resource_id } => self.execute_file_close(resource_id),

            // Filesystem metadata
            NativeEffect::Stat { path } => self.execute_stat(path),

            // Directory operations
            NativeEffect::ReadDirOpen { path } => self.execute_read_dir_open(path),
            NativeEffect::ReadDirNext { resource_id } => self.execute_read_dir_next(resource_id),
            NativeEffect::ReadDirClose { resource_id } => self.execute_read_dir_close(resource_id),

            // DNS operations
            NativeEffect::DnsResolve { hostname } => self.execute_dns_resolve(hostname),
            NativeEffect::DnsNext { resource_id } => self.execute_dns_next(resource_id),
            NativeEffect::DnsClose { resource_id } => self.execute_dns_close(resource_id),

            // TCP operations
            NativeEffect::TcpConnect { ip, port } => self.execute_tcp_connect(process_id, ip, port),
            NativeEffect::TcpListen { port, backlog } => self.execute_tcp_listen(port, backlog),
            NativeEffect::TcpListenerAccept { resource_id } => {
                self.execute_tcp_listener_accept(process_id, resource_id)
            }
            NativeEffect::TcpListenerClose { resource_id } => {
                self.execute_tcp_listener_close(resource_id)
            }
            NativeEffect::TcpSocketRead {
                resource_id,
                length,
            } => self.execute_tcp_socket_read(process_id, resource_id, length),
            NativeEffect::TcpSocketWrite { resource_id, data } => {
                self.execute_tcp_socket_write(process_id, resource_id, data)
            }
            NativeEffect::TlsAttach {
                resource_id,
                hostname,
                roots,
            } => self.execute_tls_attach(process_id, resource_id, hostname, roots),
            NativeEffect::TlsAccept {
                resource_id,
                cert,
                key,
            } => self.execute_tls_accept(process_id, resource_id, cert, key),
            NativeEffect::TcpSocketClose { resource_id } => {
                self.execute_tcp_socket_close(process_id, resource_id)
            }
        }
    }

    /// Outstanding submissions: effect operations a process is parked on, plus select-armed
    /// stream reads. Both complete through the io_uring completion queue, which only
    /// `process_completions`/`take_stream_events` drain — so the driver must keep looking while
    /// either is non-empty.
    fn has_operations_in_flight(&self) -> bool {
        !self.pending.is_empty() || !self.armed.is_empty()
    }

    fn process_completions(&mut self) -> Vec<(ProcessId, EffectResult)> {
        let mut completions = Vec::new();

        // Collect all completed operations from the io_uring completion queue
        let mut completion_results = Vec::new();
        while let Some(cqe) = self.ring.completion().next() {
            completion_results.push((cqe.user_data(), cqe.result()));
        }

        // Process collected completion entries
        for (completion_id, result_code) in completion_results {
            if let Some(armed) = self.armed.remove(&completion_id) {
                self.handle_armed_completion(armed, result_code);
                continue;
            }
            if let Some((process_id, op_type)) = self.pending.remove(&completion_id) {
                match op_type {
                    IoOpType::Read { buffer } => {
                        completions
                            .push((process_id, self.handle_read_completion(result_code, buffer)));
                    }

                    IoOpType::Write { buffer } => {
                        completions.push((
                            process_id,
                            self.handle_write_completion(result_code, buffer.len()),
                        ));
                    }

                    IoOpType::Flush => {
                        completions.push((process_id, self.handle_flush_completion(result_code)));
                    }

                    IoOpType::Accept => {
                        completions.push((process_id, self.handle_accept_completion(result_code)));
                    }

                    IoOpType::Connect { socket, peer_addr } => {
                        completions.push((
                            process_id,
                            self.handle_connect_completion(result_code, socket, peer_addr),
                        ));
                    }

                    IoOpType::Tls {
                        resource_id,
                        goal,
                        io,
                    } => {
                        // The one operation that may not be finished by its completion.
                        match self.tls_drive(process_id, resource_id, goal, Some((io, result_code)))
                        {
                            Ok(Some(result)) => completions.push((process_id, result)),
                            Ok(None) => {}
                            // `InvalidArgument`, not `Other`: a driver error is a backend
                            // defect, and must classify as a fault — as it would had
                            // `execute` returned it — not as an outcome a caller handles.
                            Err(e) => completions.push((
                                process_id,
                                Err(EffectError::InvalidArgument(format!("{e:?}"))),
                            )),
                        }
                    }
                }
            }
        }

        completions
    }

    fn arm_stream(&mut self, resource_id: ResourceId) -> Result<(), Error> {
        if self.armed_resources.contains(&resource_id) {
            return Ok(());
        }
        // What arming the resource needs, decided first: delivering an event or submitting
        // io each needs `self` back.
        enum Arm {
            Data(Vec<u8>),
            End(StreamEvent),
            Read(RawFd),
            Accept(RawFd),
        }
        let arm = match self.resources.get_mut(&resource_id) {
            // An upgraded socket may already hold decrypted bytes (a read wanted less than
            // a record carried), or have seen the end of the stream; a select must be
            // answerable from those without touching the socket. Delivering pushes the
            // event directly — nothing is armed, and the next select arms afresh.
            Some(Resource::TcpSocket { socket, tls, .. }) => match tls {
                Some(state) if !state.plaintext.is_empty() => {
                    Arm::Data(std::mem::take(&mut state.plaintext))
                }
                Some(state) if state.eof => Arm::End(state.eof_event()),
                _ => Arm::Read(socket.as_raw_fd()),
            },
            Some(Resource::TcpListener { socket, .. }) => Arm::Accept(socket.as_raw_fd()),
            Some(_) => {
                return Err(Error::InvalidArgument(format!(
                    "Resource {} is not a stream (not selectable)",
                    resource_id
                )));
            }
            None => {
                return Err(Error::InvalidArgument(format!(
                    "Resource {} not found",
                    resource_id
                )));
            }
        };
        match arm {
            Arm::Data(data) => {
                let socket_type = self.get_resource_type_id("TcpSocket");
                self.stream_events
                    .push((resource_id, socket_type, StreamEvent::Data, data));
            }
            Arm::End(event) => {
                let socket_type = self.get_resource_type_id("TcpSocket");
                self.stream_events
                    .push((resource_id, socket_type, event, vec![]));
            }
            Arm::Read(fd) => self.arm_submit_read(resource_id, fd, vec![0u8; 8192])?,
            Arm::Accept(fd) => {
                let completion_id = self.next_completion_id;
                self.next_completion_id += 1;
                let accept_op =
                    opcode::Accept::new(types::Fd(fd), std::ptr::null_mut(), std::ptr::null_mut())
                        .build()
                        .user_data(completion_id);
                unsafe {
                    self.ring.submission().push(&accept_op).map_err(|e| {
                        Error::InvalidArgument(format!("Failed to submit accept: {}", e))
                    })?;
                }
                self.ring
                    .submit()
                    .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {}", e)))?;
                self.armed
                    .insert(completion_id, ArmedOp::Accept { resource_id });
                self.armed_resources.insert(resource_id);
            }
        }
        Ok(())
    }

    fn take_stream_events(&mut self) -> Vec<(ResourceId, usize, StreamEvent, Vec<u8>)> {
        std::mem::take(&mut self.stream_events)
    }

    fn close_resource(&mut self, resource_id: ResourceId) {
        // Remove resource from registry - Drop impl will close the FD. An armed read
        // on the closed fd completes with an error; its event is dropped at routing
        // (the owner is gone too).
        self.resources.remove(&resource_id);
        self.armed_resources.remove(&resource_id);
    }

    fn set_type_ids(&mut self, resources: &[String], results: &[(String, ResultTupleInfo)]) {
        self.resource_type_ids.clear();
        for (type_id, name) in resources.iter().enumerate() {
            self.resource_type_ids.insert(name.clone(), type_id);
        }
        self.result_infos.clear();
        for (name, info) in results {
            self.result_infos.insert(name.clone(), info.clone());
        }
    }
}

impl NativeEffectBackend {
    fn execute_file_open(
        &mut self,
        path: Vec<u8>,
        flags: i32,
        mode: u32,
    ) -> Result<Option<EffectResult>, Error> {
        // Convert path bytes to string
        let path_str = String::from_utf8(path)
            .map_err(|_| Error::InvalidArgument("Path contains invalid UTF-8".to_string()))?;

        // Parse flags and build OpenOptions
        let mut options = std::fs::OpenOptions::new();

        // Access mode (O_RDONLY=0, O_WRONLY=1, O_RDWR=2)
        let access_mode = flags & 0x3;
        match access_mode {
            0 => {
                options.read(true);
            } // O_RDONLY
            1 => {
                options.write(true);
            } // O_WRONLY
            2 => {
                options.read(true).write(true);
            } // O_RDWR
            _ => {}
        }

        // O_CREAT = 64
        if flags & 0o100 != 0 {
            options.create(true);
        }

        // O_TRUNC = 512
        if flags & 0o1000 != 0 {
            options.truncate(true);
        }

        // O_APPEND = 1024
        if flags & 0o2000 != 0 {
            options.append(true);
        }

        options.mode(mode);

        let file = match options.open(&path_str) {
            Ok(file) => file,
            Err(e) => {
                return Ok(Some(Err(effect_error(
                    &format!("cannot open '{path_str}'"),
                    &e,
                ))));
            }
        };

        // Allocate a new resource ID for this file
        let resource_id = self.next_resource_id;
        self.next_resource_id += 1;

        // Register the file resource
        let metadata = Resource::File {
            file,
            path: path_str,
        };
        self.resources.insert(resource_id, metadata);

        // Return immediate completion with the resource
        let type_id = self.get_resource_type_id("File");
        Ok(Some(Ok(WireValue::Resource(resource_id, type_id))))
    }

    fn execute_stat(&mut self, path: Vec<u8>) -> Result<Option<EffectResult>, Error> {
        let path_str = String::from_utf8(path)
            .map_err(|_| Error::InvalidArgument("Invalid UTF-8 in path".to_string()))?;

        // Follows symlinks (like the conventional `stat`). A path that is not there answers
        // nil — the ordinary "found nothing"; a *failed* lookup also answers nil, but carrying
        // an `:error` payload, so a caller that cares can tell them apart.
        let metadata = match std::fs::metadata(&path_str) {
            Ok(md) => md,
            Err(e) if e.kind() == ErrorKind::NotFound => {
                return Ok(Some(Ok(WireValue::nil())));
            }
            Err(e) => {
                return Ok(Some(Err(effect_error(
                    &format!("cannot stat '{path_str}'"),
                    &e,
                ))));
            }
        };

        let info = self.result_info("filesystem_stat")?;
        // The `kind` tag. `metadata` follows symlinks, so the symlink case never actually arises.
        let kind_name = if metadata.is_dir() {
            "Dir"
        } else if metadata.is_symlink() {
            "Symlink"
        } else if metadata.is_file() {
            "File"
        } else {
            "Other"
        };
        let kind = kind_tag(info, kind_name)?;
        let size: u64 = metadata.len();
        // mtime as nanoseconds since the Unix epoch (0 if the platform cannot report it).
        let modified_nanos: u128 = metadata
            .modified()
            .ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let mode: u32 = metadata.permissions().mode() & 0o7777;

        // `[kind, size, modified, mode]` tuple, stamped with `filesystem_stat`'s real result type
        // ids (pushed by the environment — the backend has no type registry of its own).
        Ok(Some(Ok(WireValue::tuple(
            info.tuple_id,
            vec![
                kind,
                WireValue::Int(size as i64),
                WireValue::Int(modified_nanos as i64),
                WireValue::Int(mode as i64),
            ],
        ))))
    }

    fn execute_read_dir_open(&mut self, path: Vec<u8>) -> Result<Option<EffectResult>, Error> {
        let path_str = String::from_utf8(path)
            .map_err(|_| Error::InvalidArgument("Invalid UTF-8 in path".to_string()))?;

        let entries = match std::fs::read_dir(&path_str) {
            Ok(entries) => entries,
            Err(e) => {
                return Ok(Some(Err(effect_error(
                    &format!("cannot read directory '{path_str}'"),
                    &e,
                ))));
            }
        };

        // Allocate a new resource ID for this directory iterator.
        let resource_id = self.next_resource_id;
        self.next_resource_id += 1;
        self.resources
            .insert(resource_id, Resource::Dir { entries });

        let type_id = self.get_resource_type_id("Dir");
        Ok(Some(Ok(WireValue::Resource(resource_id, type_id))))
    }

    fn execute_read_dir_next(
        &mut self,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        // Fetch the result type ids up front (cloned), before the mutable borrow of
        // `self.resources` below.
        let info = self.result_info("directory_next")?.clone();
        let resource = self
            .resources
            .get_mut(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        let Resource::Dir { entries } = resource else {
            return Err(Error::InvalidArgument("Resource is not a Dir".to_string()));
        };

        match entries.next() {
            Some(Ok(entry)) => {
                let name_bytes = entry.file_name().into_vec();
                // The entry's own type, without following symlinks. `d_type` from `getdents` is
                // essentially free; `file_type()` only falls back to an `lstat` on the rare
                // filesystems that report `DT_UNKNOWN`. `Other` covers socket/fifo/device/unknown.
                let kind_name = match entry.file_type() {
                    Ok(ft) if ft.is_dir() => "Dir",
                    Ok(ft) if ft.is_symlink() => "Symlink",
                    Ok(ft) if ft.is_file() => "File",
                    _ => "Other",
                };
                let kind = kind_tag(&info, kind_name)?;
                // `[name, kind]` pair, stamped with `directory_next`'s real result type ids (pushed
                // by the environment). The name bytes travel via the heap side-channel (`Heap(0)`).
                // The name bytes ride in the value itself now, rather than in a
                // side-channel slot the value pointed at by index.
                Ok(Some(Ok(WireValue::tuple(
                    info.tuple_id,
                    vec![WireValue::Binary(name_bytes.into()), kind],
                ))))
            }
            Some(Err(e)) => Ok(Some(Err(effect_error("cannot read directory entry", &e)))),
            // Iterator exhausted - return Nil.
            None => Ok(Some(Ok(WireValue::nil()))),
        }
    }

    fn execute_read_dir_close(
        &mut self,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        self.resources
            .remove(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        Ok(Some(Ok(WireValue::ok())))
    }

    fn execute_dns_resolve(&mut self, hostname: Vec<u8>) -> Result<Option<EffectResult>, Error> {
        // Convert hostname bytes to string
        let hostname_str = String::from_utf8(hostname)
            .map_err(|_| Error::InvalidArgument("Invalid UTF-8 in hostname".to_string()))?;

        // Resolve address using DNS (blocking)
        let addr_str = format!("{}:0", hostname_str);
        let addresses: Vec<Vec<u8>> = match addr_str.to_socket_addrs() {
            Ok(addrs) => addrs
                .map(|addr| match addr.ip() {
                    std::net::IpAddr::V4(ipv4) => ipv4.octets().to_vec(),
                    std::net::IpAddr::V6(ipv6) => ipv6.octets().to_vec(),
                })
                .collect(),
            Err(e) => {
                // A host with no addresses is an empty resolver, not a failure; anything else
                // is an outcome the caller can act on (retry, fall back).
                match e.kind() {
                    ErrorKind::NotFound | ErrorKind::InvalidInput => vec![],
                    _ => {
                        return Ok(Some(Err(effect_error(
                            &format!("cannot resolve '{hostname_str}'"),
                            &e,
                        ))));
                    }
                }
            }
        };

        // Allocate a new resource ID for this resolver
        let resource_id = self.next_resource_id;
        self.next_resource_id += 1;

        // Register the DNS resolver resource
        self.resources.insert(
            resource_id,
            Resource::DnsResolver {
                addresses,
                position: 0,
            },
        );

        let type_id = self.get_resource_type_id("DnsResolver");
        Ok(Some(Ok(WireValue::Resource(resource_id, type_id))))
    }

    fn execute_dns_next(&mut self, resource_id: ResourceId) -> Result<Option<EffectResult>, Error> {
        let resource = self
            .resources
            .get_mut(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        let Resource::DnsResolver {
            addresses,
            position,
        } = resource
        else {
            return Err(Error::InvalidArgument(
                "Resource is not a DnsResolver".to_string(),
            ));
        };

        if *position < addresses.len() {
            let ip_bytes = addresses[*position].clone();
            *position += 1;
            Ok(Some(Ok(WireValue::Binary(ip_bytes.into()))))
        } else {
            // No more addresses - return Nil
            Ok(Some(Ok(WireValue::nil())))
        }
    }

    fn execute_dns_close(
        &mut self,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        self.resources
            .remove(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        Ok(Some(Ok(WireValue::ok())))
    }

    fn execute_tcp_connect(
        &mut self,
        process_id: ProcessId,
        ip: Vec<u8>,
        port: u16,
    ) -> Result<Option<EffectResult>, Error> {
        // Parse raw IP bytes into SocketAddr
        let addr = match ip.len() {
            4 => {
                let octets: [u8; 4] = ip.try_into().unwrap();
                SocketAddr::from((octets, port))
            }
            16 => {
                let octets: [u8; 16] = ip.try_into().unwrap();
                SocketAddr::from((octets, port))
            }
            _ => {
                return Err(Error::InvalidArgument(format!(
                    "IP address must be 4 bytes (IPv4) or 16 bytes (IPv6), got {} bytes",
                    ip.len()
                )));
            }
        };

        // Create socket using socket2 (domain matches address type)
        let domain = if addr.is_ipv4() {
            socket2::Domain::IPV4
        } else {
            socket2::Domain::IPV6
        };
        let socket = try_io!(
            Socket::new(domain, socket2::Type::STREAM, None),
            "cannot create socket"
        );

        // Set socket to non-blocking mode for async connect
        try_io!(socket.set_nonblocking(true), "cannot set non-blocking");

        // Submit async connect operation
        let completion_id = self.next_completion_id;
        self.next_completion_id += 1;

        let socket_addr: socket2::SockAddr = addr.into();
        let connect_op = opcode::Connect::new(
            types::Fd(socket.as_raw_fd()),
            socket_addr.as_ptr(),
            socket_addr.len(),
        )
        .build()
        .user_data(completion_id);

        unsafe {
            self.ring
                .submission()
                .push(&connect_op)
                .map_err(|e| Error::InvalidArgument(format!("Failed to submit connect: {}", e)))?;
        }
        self.ring
            .submit()
            .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {}", e)))?;

        // Track the pending operation with the socket
        self.pending.insert(
            completion_id,
            (
                process_id,
                IoOpType::Connect {
                    socket,
                    peer_addr: addr,
                },
            ),
        );

        // Async operation, no immediate completion
        Ok(None)
    }

    fn execute_tcp_listen(
        &mut self,
        port: u16,
        backlog: i32,
    ) -> Result<Option<EffectResult>, Error> {
        // Every step here can fail on the world's terms — a port already in use, an exhausted
        // descriptor table — so each is an outcome the caller can act on.
        let socket = try_io!(
            Socket::new(socket2::Domain::IPV4, socket2::Type::STREAM, None),
            "cannot create socket"
        );
        try_io!(socket.set_reuse_address(true), "cannot set SO_REUSEADDR");
        let addr = SocketAddr::from(([0, 0, 0, 0], port));
        try_io!(
            socket.bind(&addr.into()),
            &format!("cannot bind port {port}")
        );
        try_io!(socket.listen(backlog), &format!("cannot listen on {port}"));

        // Allocate a new resource ID for this listener
        let resource_id = self.next_resource_id;
        self.next_resource_id += 1;

        // Register the listener resource
        let metadata = Resource::TcpListener {
            socket,
            local_addr: addr,
        };
        self.resources.insert(resource_id, metadata);

        let type_id = self.get_resource_type_id("TcpListener");
        Ok(Some(Ok(WireValue::Resource(resource_id, type_id))))
    }

    fn execute_file_read(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
        offset: u64,
        length: usize,
    ) -> Result<Option<EffectResult>, Error> {
        let resource = self
            .resources
            .get(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        let fd = resource.fd();

        // Allocate buffer for the read
        let mut buffer = vec![0u8; length];

        // Submit async read operation with explicit offset
        let completion_id = self.next_completion_id;
        self.next_completion_id += 1;

        let read_op = opcode::Read::new(types::Fd(fd), buffer.as_mut_ptr(), buffer.len() as u32)
            .offset(offset)
            .build()
            .user_data(completion_id);

        unsafe {
            self.ring
                .submission()
                .push(&read_op)
                .map_err(|e| Error::InvalidArgument(format!("Failed to submit read: {}", e)))?;
        }
        self.ring
            .submit()
            .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {}", e)))?;

        // Track the pending operation
        self.pending
            .insert(completion_id, (process_id, IoOpType::Read { buffer }));

        // Async operation, no immediate completion
        Ok(None)
    }

    fn execute_tcp_socket_read(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
        length: usize,
    ) -> Result<Option<EffectResult>, Error> {
        let resource = self
            .resources
            .get(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        // An upgraded socket answers in plaintext, which may take several socket operations.
        if matches!(resource, Resource::TcpSocket { tls: Some(_), .. }) {
            return self.tls_drive(
                process_id,
                resource_id,
                TlsGoal::Read { want: length },
                None,
            );
        }

        let fd = resource.fd();

        // Allocate buffer for the read
        let mut buffer = vec![0u8; length];

        // Submit async read operation (no offset for sockets)
        let completion_id = self.next_completion_id;
        self.next_completion_id += 1;

        let read_op = opcode::Read::new(types::Fd(fd), buffer.as_mut_ptr(), buffer.len() as u32)
            .build()
            .user_data(completion_id);

        unsafe {
            self.ring
                .submission()
                .push(&read_op)
                .map_err(|e| Error::InvalidArgument(format!("Failed to submit read: {}", e)))?;
        }
        self.ring
            .submit()
            .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {}", e)))?;

        // Track the pending operation
        self.pending
            .insert(completion_id, (process_id, IoOpType::Read { buffer }));

        // Async operation, no immediate completion
        Ok(None)
    }

    fn execute_file_write(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
        offset: u64,
        data: Vec<u8>,
    ) -> Result<Option<EffectResult>, Error> {
        let resource = self
            .resources
            .get(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        let fd = resource.fd();

        // Submit async write operation with explicit offset
        let completion_id = self.next_completion_id;
        self.next_completion_id += 1;

        let write_op = opcode::Write::new(types::Fd(fd), data.as_ptr(), data.len() as u32)
            .offset(offset)
            .build()
            .user_data(completion_id);

        unsafe {
            self.ring
                .submission()
                .push(&write_op)
                .map_err(|e| Error::InvalidArgument(format!("Failed to submit write: {}", e)))?;
        }
        self.ring
            .submit()
            .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {}", e)))?;

        // Track the pending operation (keep buffer alive until completion)
        self.pending.insert(
            completion_id,
            (process_id, IoOpType::Write { buffer: data }),
        );

        Ok(None)
    }

    fn execute_tcp_socket_write(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
        data: Vec<u8>,
    ) -> Result<Option<EffectResult>, Error> {
        let resource = self
            .resources
            .get_mut(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        // An upgraded socket takes the bytes as plaintext: hand them to rustls, then drain
        // the ciphertext it produces.
        if let Resource::TcpSocket {
            tls: Some(state), ..
        } = resource
        {
            let len = data.len();
            if let Err(e) = std::io::Write::write_all(&mut state.connection.writer(), &data) {
                return Ok(Some(Err(tls_error("tls write", e))));
            }
            return self.tls_drive(process_id, resource_id, TlsGoal::Write { len }, None);
        }

        let fd = resource.fd();

        // Submit async write operation (no offset for sockets)
        let completion_id = self.next_completion_id;
        self.next_completion_id += 1;

        let write_op = opcode::Write::new(types::Fd(fd), data.as_ptr(), data.len() as u32)
            .build()
            .user_data(completion_id);

        unsafe {
            self.ring
                .submission()
                .push(&write_op)
                .map_err(|e| Error::InvalidArgument(format!("Failed to submit write: {}", e)))?;
        }
        self.ring
            .submit()
            .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {}", e)))?;

        // Track the pending operation (keep buffer alive until completion)
        self.pending.insert(
            completion_id,
            (process_id, IoOpType::Write { buffer: data }),
        );

        Ok(None)
    }

    fn execute_file_flush(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        let resource = self
            .resources
            .get(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        let fd = resource.fd();

        // Submit fsync operation
        let completion_id = self.next_completion_id;
        self.next_completion_id += 1;

        let fsync_op = opcode::Fsync::new(types::Fd(fd))
            .build()
            .user_data(completion_id);

        unsafe {
            self.ring
                .submission()
                .push(&fsync_op)
                .map_err(|e| Error::InvalidArgument(format!("Failed to submit fsync: {}", e)))?;
        }
        self.ring
            .submit()
            .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {}", e)))?;

        self.pending
            .insert(completion_id, (process_id, IoOpType::Flush));

        Ok(None)
    }

    fn execute_file_close(
        &mut self,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        // Remove the resource - File will be dropped and closed automatically
        self.resources
            .remove(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        // Return immediate completion
        Ok(Some(Ok(WireValue::ok())))
    }

    fn execute_tcp_listener_accept(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        let resource = self
            .resources
            .get(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        let fd = resource.fd();

        // Submit async accept operation
        let completion_id = self.next_completion_id;
        self.next_completion_id += 1;

        let accept_op =
            opcode::Accept::new(types::Fd(fd), std::ptr::null_mut(), std::ptr::null_mut())
                .build()
                .user_data(completion_id);

        unsafe {
            self.ring
                .submission()
                .push(&accept_op)
                .map_err(|e| Error::InvalidArgument(format!("Failed to submit accept: {}", e)))?;
        }
        self.ring
            .submit()
            .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {}", e)))?;

        self.pending
            .insert(completion_id, (process_id, IoOpType::Accept));

        Ok(None)
    }

    fn execute_tcp_socket_close(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        // An upgraded socket first flushes a close_notify — what lets the peer tell a clean
        // close from a truncated one. Process teardown cannot send it (that path just drops
        // the socket), so this is best-effort on the explicit close only; `tls_finish`
        // removes the resource when the goal completes, usually at the flush write's
        // completion.
        if let Some(Resource::TcpSocket {
            tls: Some(state), ..
        }) = self.resources.get_mut(&resource_id)
        {
            state.connection.send_close_notify();
            return self.tls_drive(process_id, resource_id, TlsGoal::Close, None);
        }

        // Remove the resource - Socket will be dropped and closed automatically
        self.resources
            .remove(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        // Return immediate completion
        Ok(Some(Ok(WireValue::ok())))
    }

    fn execute_tcp_listener_close(
        &mut self,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        // Remove the resource - Listener will be dropped and closed automatically
        self.resources
            .remove(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        // Return immediate completion
        Ok(Some(Ok(WireValue::ok())))
    }

    fn handle_read_completion(&self, result_code: i32, mut buffer: Vec<u8>) -> EffectResult {
        if result_code < 0 {
            // Map common errno values to structured errors
            return Err(match -result_code {
                2 => EffectError::NotFound("File not found".to_string()),
                9 => EffectError::InvalidArgument("Bad file descriptor".to_string()),
                11 => EffectError::WouldBlock,
                13 => EffectError::PermissionDenied("Permission denied".to_string()),
                22 => EffectError::InvalidArgument("Invalid argument".to_string()),
                104 => EffectError::ConnectionRefused("Connection reset by peer".to_string()),
                _ => EffectError::IO(format!("Read error: {}", -result_code)),
            });
        }

        // Truncate buffer to actual bytes read
        let bytes_read = result_code as usize;
        buffer.truncate(bytes_read);

        Ok(WireValue::Binary(buffer.into()))
    }

    fn handle_write_completion(&self, result_code: i32, _buffer_len: usize) -> EffectResult {
        if result_code < 0 {
            // Map common errno values to structured errors
            return Err(match -result_code {
                9 => EffectError::InvalidArgument("Bad file descriptor".to_string()),
                11 => EffectError::WouldBlock,
                22 => EffectError::InvalidArgument("Invalid argument".to_string()),
                32 => EffectError::IO("Broken pipe".to_string()),
                104 => EffectError::ConnectionRefused("Connection reset by peer".to_string()),
                _ => EffectError::IO(format!("Write error: {}", -result_code)),
            });
        }

        // Return bytes actually written (may be less than requested)
        let bytes_written = result_code as i64;
        Ok(WireValue::Int(bytes_written))
    }

    fn handle_flush_completion(&self, result_code: i32) -> EffectResult {
        if result_code < 0 {
            Err(match -result_code {
                9 => EffectError::InvalidArgument("Bad file descriptor".to_string()),
                22 => EffectError::InvalidArgument("Invalid argument".to_string()),
                _ => EffectError::IO(format!("Flush error: {}", -result_code)),
            })
        } else {
            Ok(WireValue::ok())
        }
    }

    fn handle_accept_completion(&mut self, result_code: i32) -> EffectResult {
        if result_code < 0 {
            return Err(match -result_code {
                9 => EffectError::InvalidArgument("Bad file descriptor".to_string()),
                22 => EffectError::InvalidArgument("Invalid argument".to_string()),
                _ => EffectError::IO(format!("Accept error: {}", -result_code)),
            });
        }

        // Accepted connection - result_code is the new FD
        let new_fd = result_code;

        // Create a Socket from the raw FD
        let socket = unsafe { Socket::from_raw_fd(new_fd) };

        // Get the actual peer address
        let peer_addr = socket
            .peer_addr()
            .ok()
            .and_then(|addr| addr.as_socket())
            .unwrap_or_else(|| SocketAddr::from(([0, 0, 0, 0], 0)));

        // Allocate a new resource ID for the accepted socket
        let new_resource_id = self.next_resource_id;
        self.next_resource_id += 1;

        // Register the new socket
        self.resources.insert(
            new_resource_id,
            Resource::TcpSocket {
                socket,
                peer_addr,
                tls: None,
            },
        );

        let type_id = self.get_resource_type_id("TcpSocket");
        Ok(WireValue::Resource(new_resource_id, type_id))
    }

    fn handle_connect_completion(
        &mut self,
        result_code: i32,
        socket: Socket,
        peer_addr: SocketAddr,
    ) -> EffectResult {
        if result_code < 0 {
            // Connect failed - socket will be dropped automatically
            return Err(match -result_code {
                2 => EffectError::NotFound("Host not found".to_string()),
                111 => {
                    EffectError::ConnectionRefused(format!("Connection refused to {}", peer_addr))
                }
                113 => EffectError::IO(format!("No route to host: {}", peer_addr)),
                _ => EffectError::IO(format!("Connect error: {}", -result_code)),
            });
        }

        // Connect succeeded - allocate resource ID and register socket
        let new_resource_id = self.next_resource_id;
        self.next_resource_id += 1;

        self.resources.insert(
            new_resource_id,
            Resource::TcpSocket {
                socket,
                peer_addr,
                tls: None,
            },
        );

        let type_id = self.get_resource_type_id("TcpSocket");
        Ok(WireValue::Resource(new_resource_id, type_id))
    }
}

// --- TLS -----------------------------------------------------------------------------------
//
// TLS is a property a socket takes on, not a resource kind of its own. `__tls_attach__`
// (client side) and `__tls_accept__` (server side) upgrade a connected socket *in place* —
// the kTLS model: the same handle then reads, writes, closes and selects in plaintext, with
// the encryption invisible above this layer. Nothing upstream needs to know — the HTTP
// server pump serves HTTPS unchanged, and a select on an upgraded socket yields decrypted
// `Data` events. The upgrade leaves no second handle on which ciphertext could be reached.
//
// rustls is sans-io: a connection is a state machine with four ports — feed it TLS bytes
// (`read_tls`), let it process them, pull plaintext out (`reader`), push plaintext in
// (`writer`) and drain the encrypted result (`write_tls`). It never touches the socket.
//
// That makes an upgraded socket the one place this backend's "one CQE finishes one
// operation" rule does not hold. A single read may need several socket reads (a record can
// arrive split), and it may need a *write* first (handshake continuation, a key update, an
// alert). So each operation is a small state machine of its own: `tls_drive` applies
// whatever completion just arrived, then either submits the next socket op and waits, or
// answers.

/// The TLS half of an upgraded socket. `plaintext` is what rustls has decrypted but the
/// program has not yet asked for; `outgoing` the ciphertext still to be written (with how
/// much of it has gone).
pub struct TlsState {
    connection: rustls::Connection,
    plaintext: Vec<u8>,
    outgoing: Vec<u8>,
    outgoing_sent: usize,
    eof: bool,
}

impl TlsState {
    fn new(connection: rustls::Connection) -> Box<Self> {
        Box::new(TlsState {
            connection,
            plaintext: Vec::new(),
            outgoing: Vec::new(),
            outgoing_sent: 0,
            eof: false,
        })
    }

    fn flushed(&self) -> bool {
        self.outgoing_sent >= self.outgoing.len()
    }

    /// Feed one completed socket read into the connection, growing `plaintext` with
    /// whatever it decrypts. Shared by the goal driver and the armed (select) path. An
    /// empty read is end-of-stream, which rustls must also learn: seeing the EOF is what
    /// lets its reader distinguish a close_notify-terminated stream from a bare TCP close.
    fn ingest(&mut self, mut bytes: &[u8]) -> Result<(), EffectError> {
        if bytes.is_empty() {
            self.eof = true;
            self.connection
                .read_tls(&mut std::io::empty())
                .map_err(|e| tls_error("tls read", e))?;
            return Ok(());
        }
        // `read_tls` takes what fits in rustls's own buffer and no more, so a single call
        // can leave part of the read behind. Dropping that remainder loses TLS records —
        // which, when the lost record is the one carrying the peer's certificate, means
        // the handshake completes without it ever being checked.
        while !bytes.is_empty() {
            let consumed = self
                .connection
                .read_tls(&mut bytes)
                .map_err(|e| tls_error("tls read", e))?;
            if consumed == 0 {
                break;
            }
            self.connection
                .process_new_packets()
                .map_err(|e| tls_error("tls", e))?;
        }
        // Drain whatever plaintext that produced. `WouldBlock` just means "no more yet",
        // which is the ordinary case for a partial record.
        let mut chunk = [0u8; 16384];
        loop {
            match std::io::Read::read(&mut self.connection.reader(), &mut chunk) {
                Ok(0) => break,
                Ok(n) => self.plaintext.extend_from_slice(&chunk[..n]),
                Err(e) if e.kind() == ErrorKind::WouldBlock => break,
                Err(e) => return Err(tls_error("tls read", e)),
            }
        }
        Ok(())
    }

    /// The event an exhausted upgraded socket answers. Rustls's reader knows whether the
    /// EOF it saw was announced (a close_notify — a clean `End`) or bare (a peer that
    /// just vanished, which may be a stream cut short — `Failed`), because `ingest` fed
    /// the EOF through; this is the same distinction the blocking read draws, so a
    /// select and an explicit read tell the same story.
    fn eof_event(&mut self) -> StreamEvent {
        match std::io::Read::read(&mut self.connection.reader(), &mut [0u8; 1]) {
            Ok(_) => StreamEvent::End,
            Err(e) => StreamEvent::Failed {
                error: tls_error("tls read", e),
            },
        }
    }
}

/// What an upgraded socket is trying to do. Held across however many socket operations it
/// takes.
#[derive(Debug, Clone, Copy)]
pub enum TlsGoal {
    /// Handshaking, on the way to answering with the socket itself.
    Attach,
    /// Up to `want` bytes of plaintext.
    Read { want: usize },
    /// `len` bytes of plaintext, already handed to rustls; drain the ciphertext.
    Write { len: usize },
    /// A `close_notify`, then done.
    Close,
}

/// Which socket operation is in flight for a TLS goal.
#[derive(Debug)]
pub enum TlsIo {
    Read { buffer: Vec<u8> },
    Write,
}

/// How far a goal got when the driver last looked at it.
enum Step {
    /// A socket operation was submitted; wait for its completion.
    Waiting,
    /// The goal is satisfied.
    Done(EffectResult),
}

/// A TLS protocol failure — a bad certificate, a broken record, a peer that hung up mid
/// handshake. All of them are outcomes the caller can act on rather than faults.
fn tls_error(context: &str, detail: impl std::fmt::Display) -> EffectError {
    EffectError::Other(format!("{context}: {detail}"))
}

impl NativeEffectBackend {
    /// The TLS state of an upgraded socket, or an argument error for anything else.
    fn tls_state(&mut self, resource_id: ResourceId) -> Result<&mut TlsState, Error> {
        match self.resources.get_mut(&resource_id) {
            Some(Resource::TcpSocket {
                tls: Some(state), ..
            }) => Ok(state),
            Some(_) => Err(Error::InvalidArgument(format!(
                "Resource {resource_id} is not a TLS-upgraded socket"
            ))),
            None => Err(Error::InvalidArgument(format!(
                "Resource {resource_id} not found"
            ))),
        }
    }

    /// A client config trusting exactly the DER certificates handed in. Nothing is read from
    /// the filesystem or the environment: the caller supplies the anchors, which is what makes
    /// a locally-issued certificate testable and keeps the trust decision out of the runtime.
    fn tls_config(roots_der: &[u8]) -> Result<Arc<rustls::ClientConfig>, EffectError> {
        let mut roots = rustls::RootCertStore::empty();
        if roots_der.is_empty() {
            // Empty means the host's own defaults — the Mozilla set compiled in. Passing them
            // through the builtin as bytes would mean a few hundred KB of certificate in a
            // Quiver literal, for the case that wants no customisation at all.
            roots.extend(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());
        } else {
            let mut added = 0usize;
            for certificate in der_certificates(roots_der) {
                if roots.add(certificate.into()).is_ok() {
                    added += 1;
                }
            }
            if added == 0 {
                return Err(tls_error(
                    "tls",
                    "no usable trust anchors in the supplied roots",
                ));
            }
        }
        Ok(Arc::new(
            rustls::ClientConfig::builder()
                .with_root_certificates(roots)
                .with_no_client_auth(),
        ))
    }

    /// Submit a socket read for a TLS resource and record the goal it serves.
    fn tls_submit_read(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
        goal: TlsGoal,
    ) -> Result<(), Error> {
        let fd = self
            .resources
            .get(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {resource_id} not found")))?
            .fd();
        let mut buffer = vec![0u8; 16384];
        let completion_id = self.next_completion_id;
        self.next_completion_id += 1;
        let op = opcode::Read::new(types::Fd(fd), buffer.as_mut_ptr(), buffer.len() as u32)
            .build()
            .user_data(completion_id);
        unsafe {
            self.ring
                .submission()
                .push(&op)
                .map_err(|e| Error::InvalidArgument(format!("Failed to submit TLS read: {e}")))?;
        }
        self.ring
            .submit()
            .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {e}")))?;
        self.pending.insert(
            completion_id,
            (
                process_id,
                IoOpType::Tls {
                    resource_id,
                    goal,
                    io: TlsIo::Read { buffer },
                },
            ),
        );
        Ok(())
    }

    /// Submit a write of the resource's pending ciphertext, from wherever the last write got
    /// to. Partial writes are the reason for the offset: a half-written TLS record would
    /// corrupt the stream, so the remainder must go out before anything else.
    fn tls_submit_write(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
        goal: TlsGoal,
    ) -> Result<(), Error> {
        let (fd, ptr, len) = {
            let resource = self.resources.get_mut(&resource_id).ok_or_else(|| {
                Error::InvalidArgument(format!("Resource {resource_id} not found"))
            })?;
            let fd = resource.fd();
            let Resource::TcpSocket {
                tls: Some(state), ..
            } = resource
            else {
                return Err(Error::InvalidArgument(
                    "Resource is not a TLS-upgraded socket".into(),
                ));
            };
            (
                fd,
                unsafe { state.outgoing.as_ptr().add(state.outgoing_sent) },
                state.outgoing.len() - state.outgoing_sent,
            )
        };
        let completion_id = self.next_completion_id;
        self.next_completion_id += 1;
        let op = opcode::Write::new(types::Fd(fd), ptr, len as u32)
            .build()
            .user_data(completion_id);
        unsafe {
            self.ring
                .submission()
                .push(&op)
                .map_err(|e| Error::InvalidArgument(format!("Failed to submit TLS write: {e}")))?;
        }
        self.ring
            .submit()
            .map_err(|e| Error::InvalidArgument(format!("Failed to submit: {e}")))?;
        self.pending.insert(
            completion_id,
            (
                process_id,
                IoOpType::Tls {
                    resource_id,
                    goal,
                    io: TlsIo::Write,
                },
            ),
        );
        Ok(())
    }

    /// Advance a TLS goal after a socket completion (or, with `io: None`, from a standing
    /// start). Either submits the next socket operation and answers `Waiting`, or finishes.
    fn tls_drive(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
        goal: TlsGoal,
        io: Option<(TlsIo, i32)>,
    ) -> Result<Option<EffectResult>, Error> {
        // 1. Fold in whatever just completed.
        if let Some((io, result_code)) = io
            && let Err(failure) = self.tls_apply(resource_id, io, result_code)
        {
            return Ok(Some(self.tls_finish(resource_id, goal, Err(failure))));
        }

        // 2. Let rustls decide what it wants next, then either satisfy the goal or wait.
        match self.tls_step(resource_id, goal)? {
            Step::Done(result) => Ok(Some(self.tls_finish(resource_id, goal, result))),
            Step::Waiting => {
                let wants_write = !self.tls_state(resource_id)?.flushed();
                if wants_write {
                    self.tls_submit_write(process_id, resource_id, goal)?;
                } else {
                    self.tls_submit_read(process_id, resource_id, goal)?;
                }
                Ok(None)
            }
        }
    }

    /// A goal's final answer, closing the socket when that answer ends its life: a finished
    /// close (however it went), and a failed upgrade — the handshake poisoned the byte
    /// stream, so there is no plain socket left to hand back. Every goal finishes through
    /// here, whether immediately or at a later socket completion; without that, either case
    /// would hold the fd until process teardown.
    fn tls_finish(
        &mut self,
        resource_id: ResourceId,
        goal: TlsGoal,
        result: EffectResult,
    ) -> EffectResult {
        let defunct = match goal {
            TlsGoal::Close => true,
            TlsGoal::Attach => result.is_err(),
            TlsGoal::Read { .. } | TlsGoal::Write { .. } => false,
        };
        if defunct {
            self.resources.remove(&resource_id);
        }
        result
    }

    /// Apply a completed socket operation to the connection.
    fn tls_apply(
        &mut self,
        resource_id: ResourceId,
        io: TlsIo,
        result_code: i32,
    ) -> Result<(), EffectError> {
        let Some(Resource::TcpSocket {
            tls: Some(state), ..
        }) = self.resources.get_mut(&resource_id)
        else {
            return Err(tls_error("tls", "connection is closed"));
        };
        match io {
            TlsIo::Read { buffer } => {
                if result_code < 0 {
                    return Err(tls_error("tls read", format!("errno {}", -result_code)));
                }
                state.ingest(&buffer[..result_code as usize])?;
            }
            TlsIo::Write => {
                if result_code < 0 {
                    return Err(tls_error("tls write", format!("errno {}", -result_code)));
                }
                state.outgoing_sent += result_code as usize;
                if state.flushed() {
                    state.outgoing.clear();
                    state.outgoing_sent = 0;
                }
            }
        }
        Ok(())
    }

    /// Whether the goal can be answered now, having first given rustls the chance to queue
    /// anything it wants to send.
    fn tls_step(&mut self, resource_id: ResourceId, goal: TlsGoal) -> Result<Step, Error> {
        let body_type = self.get_resource_type_id("TcpSocket");
        let state = self.tls_state(resource_id)?;

        // Anything rustls wants to send goes out before we consider ourselves finished.
        if state.flushed() && state.connection.wants_write() {
            state.outgoing.clear();
            state.outgoing_sent = 0;
            state
                .connection
                .write_tls(&mut state.outgoing)
                .map_err(|e| Error::InvalidArgument(format!("tls write: {e}")))?;
        }
        let flushed = state.flushed();
        let TlsState {
            connection,
            plaintext,
            eof,
            ..
        } = state;

        // A goal that still needs bytes from a peer that has hung up can never be met, and
        // waiting would submit read after read against a closed socket forever.
        let truncated =
            |what: &str| Step::Done(Err(tls_error("tls", format!("peer closed {what}"))));

        Ok(match goal {
            TlsGoal::Attach => {
                if connection.is_handshaking() {
                    if *eof {
                        truncated("during the handshake")
                    } else {
                        Step::Waiting
                    }
                } else if flushed {
                    Step::Done(Ok(WireValue::Resource(resource_id, body_type)))
                } else if *eof {
                    truncated("during the handshake")
                } else {
                    Step::Waiting
                }
            }
            TlsGoal::Read { want } => {
                if !plaintext.is_empty() {
                    let n = want.min(plaintext.len());
                    let bytes: Vec<u8> = plaintext.drain(..n).collect();
                    Step::Done(Ok(WireValue::Binary(bytes.into())))
                } else if *eof {
                    // End of stream — if it was a *clean* one. Only a close_notify proves the
                    // peer meant to stop; a bare TCP close may be a response cut short (the
                    // classic truncation attack, when it isn't an accident), which a
                    // close-delimited protocol cannot detect for itself. rustls's reader
                    // knows the difference once it has seen our EOF (fed in `tls_apply`):
                    // `Ok(0)` after a close_notify, `UnexpectedEof` after a bare close.
                    match std::io::Read::read(&mut connection.reader(), &mut [0u8; 1]) {
                        Ok(_) => Step::Done(Ok(WireValue::Binary(Vec::new().into()))),
                        Err(e) => Step::Done(Err(tls_error("tls read", e))),
                    }
                } else {
                    Step::Waiting
                }
            }
            TlsGoal::Write { len } => {
                if flushed {
                    Step::Done(Ok(WireValue::Int(len as i64)))
                } else if *eof {
                    truncated("mid-write")
                } else {
                    Step::Waiting
                }
            }
            // A close that cannot flush its `close_notify` is still a close: the peer has
            // gone, which is what a close_notify would have told it.
            TlsGoal::Close => {
                if flushed || *eof {
                    Step::Done(Ok(WireValue::ok()))
                } else {
                    Step::Waiting
                }
            }
        })
    }

    fn execute_tls_attach(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
        hostname: Vec<u8>,
        roots: Vec<u8>,
    ) -> Result<Option<EffectResult>, Error> {
        let hostname = match String::from_utf8(hostname) {
            Ok(name) => name,
            Err(_) => return Ok(Some(Err(tls_error("tls", "hostname is not UTF-8")))),
        };
        let server_name = match rustls::pki_types::ServerName::try_from(hostname) {
            Ok(name) => name,
            Err(e) => return Ok(Some(Err(tls_error("tls", e)))),
        };
        let config = match Self::tls_config(&roots) {
            Ok(config) => config,
            Err(failure) => return Ok(Some(Err(failure))),
        };
        let connection = match rustls::ClientConnection::new(config, server_name) {
            Ok(connection) => rustls::Connection::Client(connection),
            Err(e) => return Ok(Some(Err(tls_error("tls", e)))),
        };
        self.tls_upgrade(process_id, resource_id, connection)
    }

    fn execute_tls_accept(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
        cert: Vec<u8>,
        key: Vec<u8>,
    ) -> Result<Option<EffectResult>, Error> {
        let config = match Self::tls_server_config(&cert, key) {
            Ok(config) => config,
            Err(failure) => return Ok(Some(Err(failure))),
        };
        let connection = match rustls::ServerConnection::new(config) {
            Ok(connection) => rustls::Connection::Server(connection),
            Err(e) => return Ok(Some(Err(tls_error("tls", e)))),
        };
        self.tls_upgrade(process_id, resource_id, connection)
    }

    /// A server config presenting the given certificate chain (concatenated DER, leaf
    /// first) with its PKCS#8 DER private key. The same convention as `roots`: bytes in,
    /// nothing read from the filesystem or environment — PEM is decoded by the caller.
    fn tls_server_config(
        cert: &[u8],
        key: Vec<u8>,
    ) -> Result<Arc<rustls::ServerConfig>, EffectError> {
        let chain: Vec<rustls::pki_types::CertificateDer> =
            der_certificates(cert).into_iter().map(Into::into).collect();
        if chain.is_empty() {
            return Err(tls_error("tls", "no certificates in the supplied chain"));
        }
        let key = rustls::pki_types::PrivateKeyDer::Pkcs8(key.into());
        rustls::ServerConfig::builder()
            .with_no_client_auth()
            .with_single_cert(chain, key)
            .map(Arc::new)
            .map_err(|e| tls_error("tls", e))
    }

    /// Install the TLS state on the socket — in place, under the same resource id — and
    /// start the handshake. Upgrading is one-way and one-shot: a socket already upgraded is
    /// rejected. A handshake that later fails closes the socket (see `tls_finish`) — its
    /// byte stream is poisoned mid-handshake, so there is nothing left to hand back.
    fn tls_upgrade(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
        connection: rustls::Connection,
    ) -> Result<Option<EffectResult>, Error> {
        match self.resources.get_mut(&resource_id) {
            Some(Resource::TcpSocket { tls, .. }) => {
                if tls.is_some() {
                    return Err(Error::InvalidArgument(format!(
                        "Resource {resource_id} is already TLS-upgraded"
                    )));
                }
                *tls = Some(TlsState::new(connection));
            }
            Some(_) => {
                return Err(Error::InvalidArgument(format!(
                    "Resource {resource_id} is not a TcpSocket"
                )));
            }
            None => {
                return Err(Error::InvalidArgument(format!(
                    "Resource {resource_id} not found"
                )));
            }
        }
        self.tls_drive(process_id, resource_id, TlsGoal::Attach, None)
    }
}

/// Split a concatenation of DER certificates into individual ones by walking their outer
/// SEQUENCE headers. Trust anchors cross the builtin boundary as raw DER precisely so no PEM
/// parser — and no file or environment access — is needed here.
fn der_certificates(bytes: &[u8]) -> Vec<Vec<u8>> {
    let mut out = Vec::new();
    let mut pos = 0usize;
    while pos + 2 <= bytes.len() {
        if bytes[pos] != 0x30 {
            break; // not a SEQUENCE — the input is not a DER certificate chain
        }
        let first = bytes[pos + 1] as usize;
        let (header, length) = if first < 0x80 {
            (2, first)
        } else {
            let count = first & 0x7f;
            if count == 0 || count > 4 || pos + 2 + count > bytes.len() {
                break;
            }
            let mut length = 0usize;
            for byte in &bytes[pos + 2..pos + 2 + count] {
                length = (length << 8) | *byte as usize;
            }
            (2 + count, length)
        };
        let end = match pos.checked_add(header).and_then(|s| s.checked_add(length)) {
            Some(end) if end <= bytes.len() => end,
            _ => break,
        };
        out.push(bytes[pos..end].to_vec());
        pos = end;
    }
    out
}
