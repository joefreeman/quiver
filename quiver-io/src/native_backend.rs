use crate::effects::NativeEffect;
use io_uring::{IoUring as IoUringRing, opcode, types};
use quiver_core::ProcessId;
use quiver_core::effects::{EffectBackend, EffectResult};
use quiver_core::error::Error;
use quiver_core::value::{ResourceId, Value};
use socket2::Socket;
use std::collections::HashMap;
use std::fs::File;
use std::net::{SocketAddr, ToSocketAddrs};
use std::os::fd::FromRawFd;
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
}

impl Resource {
    fn fd(&self) -> RawFd {
        match self {
            Resource::TcpSocket { socket, .. } => socket.as_raw_fd(),
            Resource::TcpListener { socket, .. } => socket.as_raw_fd(),
            Resource::File { file, .. } => file.as_raw_fd(),
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
        })
    }
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

            // TCP operations
            NativeEffect::TcpConnect { host, port } => {
                self.execute_tcp_connect(process_id, host, port)
            }
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
}

impl NativeEffectBackend {
    fn execute_file_open(
        &mut self,
        path: Vec<u8>,
        flags: i32,
        mode: u32,
    ) -> Result<Option<EffectResult>, Error> {
        use std::os::unix::fs::OpenOptionsExt;

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
        Ok(Some(Ok((
            Value::Resource(resource_id, "File".to_string()),
            vec![],
        ))))
    }

    fn execute_tcp_connect(
        &mut self,
        process_id: ProcessId,
        host: Vec<u8>,
        port: u16,
    ) -> Result<Option<EffectResult>, Error> {
        // Convert host bytes to string
        let host_str = String::from_utf8(host)
            .map_err(|_| Error::InvalidArgument("Invalid UTF-8 in host".to_string()))?;

        // Resolve address (prefer IPv4)
        let addr_str = format!("{}:{}", host_str, port);
        let addr = addr_str
            .to_socket_addrs()
            .map_err(|e| Error::InvalidArgument(format!("Failed to resolve address: {}", e)))?
            .find(|addr| addr.is_ipv4())
            .ok_or_else(|| Error::InvalidArgument("No IPv4 address resolved".to_string()))?;

        // Create socket using socket2
        let socket = Socket::new(socket2::Domain::IPV4, socket2::Type::STREAM, None)
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

        Ok(Some(Ok((
            Value::Resource(resource_id, "TcpListener".to_string()),
            vec![],
        ))))
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
        use quiver_core::effects::EffectError;

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
        use quiver_core::effects::EffectError;

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
        Ok((Value::Integer(bytes_written), vec![]))
    }

    fn handle_flush_completion(&self, result_code: i32) -> EffectResult {
        use quiver_core::effects::EffectError;

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
        use quiver_core::effects::EffectError;

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

        Ok((
            Value::Resource(new_resource_id, "TcpSocket".to_string()),
            vec![],
        ))
    }

    fn handle_connect_completion(
        &mut self,
        result_code: i32,
        socket: Socket,
        peer_addr: SocketAddr,
    ) -> EffectResult {
        use quiver_core::effects::EffectError;

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

        Ok((
            Value::Resource(new_resource_id, "TcpSocket".to_string()),
            vec![],
        ))
    }
}
