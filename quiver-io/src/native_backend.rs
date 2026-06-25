use crate::effects::NativeEffect;
use io_uring::{IoUring as IoUringRing, opcode, types};
use quiver_core::ProcessId;
use quiver_core::effects::{EffectBackend, EffectError, EffectResult, ResultTupleInfo};
use quiver_core::error::Error;
use quiver_core::value::{ResourceId, Value};
use socket2::Socket;
use std::collections::HashMap;
use std::fs::File;
use std::io::ErrorKind;
use std::net::{SocketAddr, ToSocketAddrs};
use std::os::fd::FromRawFd;
use std::os::unix::ffi::OsStringExt;
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
use std::os::unix::io::{AsRawFd, RawFd};

/// Resource metadata stored by the io_uring backend
#[derive(Debug)]
pub enum Resource {
    TcpSocket {
        socket: Socket,
        peer_addr: SocketAddr,
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
        })
    }

    /// Get the type ID for a resource type name. Falls back to 0 only if the tables haven't been
    /// pushed yet (which shouldn't happen for an executing host — see `set_type_ids`).
    fn get_resource_type_id(&self, name: &str) -> usize {
        *self.resource_type_ids.get(name).unwrap_or(&0)
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
fn kind_tag(info: &ResultTupleInfo, name: &str) -> Result<Value, Error> {
    let id = info.variants.get(name).copied().ok_or_else(|| {
        Error::InvalidArgument(format!("no tuple id registered for kind tag `{name}`"))
    })?;
    Ok(Value::tuple(id, vec![]))
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
            NativeEffect::TcpSocketClose { resource_id } => {
                self.execute_tcp_socket_close(resource_id)
            }
        }
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
                }
            }
        }

        completions
    }

    fn close_resource(&mut self, resource_id: ResourceId) {
        // Remove resource from registry - Drop impl will close the FD
        self.resources.remove(&resource_id);
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

        let file = options
            .open(&path_str)
            .map_err(|e| Error::InvalidArgument(format!("Failed to open file: {}", e)))?;

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
        Ok(Some(Ok((Value::Resource(resource_id, type_id), vec![]))))
    }

    fn execute_stat(&mut self, path: Vec<u8>) -> Result<Option<EffectResult>, Error> {
        let path_str = String::from_utf8(path)
            .map_err(|_| Error::InvalidArgument("Invalid UTF-8 in path".to_string()))?;

        // Follows symlinks (like the conventional `stat`); a path that does not exist maps to nil,
        // while other failures (permission denied, I/O error) propagate as runtime errors.
        let metadata = match std::fs::metadata(&path_str) {
            Ok(md) => md,
            Err(e) if e.kind() == ErrorKind::NotFound => {
                return Ok(Some(Ok((Value::nil(), vec![]))));
            }
            Err(e) => {
                return Err(Error::InvalidArgument(format!(
                    "Failed to stat '{}': {}",
                    path_str, e
                )));
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
        Ok(Some(Ok((
            Value::tuple(
                info.tuple_id,
                vec![
                    kind,
                    Value::Integer(size.into()),
                    Value::Integer(modified_nanos.into()),
                    Value::Integer(mode.into()),
                ],
            ),
            vec![],
        ))))
    }

    fn execute_read_dir_open(&mut self, path: Vec<u8>) -> Result<Option<EffectResult>, Error> {
        let path_str = String::from_utf8(path)
            .map_err(|_| Error::InvalidArgument("Invalid UTF-8 in path".to_string()))?;

        let entries = std::fs::read_dir(&path_str).map_err(|e| {
            Error::InvalidArgument(format!("Failed to read directory '{}': {}", path_str, e))
        })?;

        // Allocate a new resource ID for this directory iterator.
        let resource_id = self.next_resource_id;
        self.next_resource_id += 1;
        self.resources
            .insert(resource_id, Resource::Dir { entries });

        let type_id = self.get_resource_type_id("Dir");
        Ok(Some(Ok((Value::Resource(resource_id, type_id), vec![]))))
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
                Ok(Some(Ok((
                    Value::tuple(
                        info.tuple_id,
                        vec![Value::Binary(quiver_core::value::Binary::Heap(0)), kind],
                    ),
                    vec![name_bytes],
                ))))
            }
            Some(Err(e)) => Err(Error::InvalidArgument(format!(
                "Failed to read directory entry: {}",
                e
            ))),
            // Iterator exhausted - return Nil.
            None => Ok(Some(Ok((Value::nil(), vec![])))),
        }
    }

    fn execute_read_dir_close(
        &mut self,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        self.resources
            .remove(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        Ok(Some(Ok((Value::ok(), vec![]))))
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
                // Distinguish between "not found" and other errors
                match e.kind() {
                    // Host not found - create empty resolver
                    ErrorKind::NotFound | ErrorKind::InvalidInput => vec![],
                    // Other errors - return error
                    _ => {
                        return Err(Error::InvalidArgument(format!(
                            "DNS resolution failed: {}",
                            e
                        )));
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
        Ok(Some(Ok((Value::Resource(resource_id, type_id), vec![]))))
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
            Ok(Some(Ok((
                Value::Binary(quiver_core::value::Binary::Heap(0)),
                vec![ip_bytes],
            ))))
        } else {
            // No more addresses - return Nil
            Ok(Some(Ok((Value::nil(), vec![]))))
        }
    }

    fn execute_dns_close(
        &mut self,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        self.resources
            .remove(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        Ok(Some(Ok((Value::ok(), vec![]))))
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
        let socket = Socket::new(domain, socket2::Type::STREAM, None)
            .map_err(|e| Error::InvalidArgument(format!("Failed to create socket: {}", e)))?;

        // Set socket to non-blocking mode for async connect
        socket
            .set_nonblocking(true)
            .map_err(|e| Error::InvalidArgument(format!("Failed to set non-blocking: {}", e)))?;

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
        // Create socket using socket2
        let socket = Socket::new(socket2::Domain::IPV4, socket2::Type::STREAM, None)
            .map_err(|e| Error::InvalidArgument(format!("Failed to create socket: {}", e)))?;

        // Set SO_REUSEADDR
        socket
            .set_reuse_address(true)
            .map_err(|e| Error::InvalidArgument(format!("Failed to set SO_REUSEADDR: {}", e)))?;

        // Bind to address
        let addr = SocketAddr::from(([0, 0, 0, 0], port));
        socket
            .bind(&addr.into())
            .map_err(|e| Error::InvalidArgument(format!("Failed to bind: {}", e)))?;

        // Listen
        socket
            .listen(backlog)
            .map_err(|e| Error::InvalidArgument(format!("Failed to listen: {}", e)))?;

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
        Ok(Some(Ok((Value::Resource(resource_id, type_id), vec![]))))
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
            .get(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

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
        Ok(Some(Ok((Value::ok(), vec![]))))
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
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        // Remove the resource - Socket will be dropped and closed automatically
        self.resources
            .remove(&resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        // Return immediate completion
        Ok(Some(Ok((Value::ok(), vec![]))))
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
        Ok(Some(Ok((Value::ok(), vec![]))))
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

        Ok((
            Value::Binary(quiver_core::value::Binary::Heap(0)),
            vec![buffer],
        ))
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
        Ok((Value::Integer(bytes_written.into()), vec![]))
    }

    fn handle_flush_completion(&self, result_code: i32) -> EffectResult {
        if result_code < 0 {
            Err(match -result_code {
                9 => EffectError::InvalidArgument("Bad file descriptor".to_string()),
                22 => EffectError::InvalidArgument("Invalid argument".to_string()),
                _ => EffectError::IO(format!("Flush error: {}", -result_code)),
            })
        } else {
            Ok((Value::ok(), vec![]))
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
        self.resources
            .insert(new_resource_id, Resource::TcpSocket { socket, peer_addr });

        let type_id = self.get_resource_type_id("TcpSocket");
        Ok((Value::Resource(new_resource_id, type_id), vec![]))
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

        self.resources
            .insert(new_resource_id, Resource::TcpSocket { socket, peer_addr });

        let type_id = self.get_resource_type_id("TcpSocket");
        Ok((Value::Resource(new_resource_id, type_id), vec![]))
    }
}
