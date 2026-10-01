//! Exec effects: operating-system programs run as children of the host.
//!
//! A child is spawned into a process group of its own, with all three standard streams piped,
//! and watched through a pidfd: io_uring polls it, so a select on the child waits without a
//! thread or a `SIGCHLD` handler. Its exit is read with `WNOWAIT`, which leaves it a zombie —
//! that keeps its pid, and so its process group's id, reserved for as long as the resource
//! lives, which is what makes signalling the group safe after the child itself has gone. The
//! child is reaped only when the resource is dropped, and dropping it first kills the group,
//! so a child never outlives its owner, and neither does anything it started.

use super::{NativeEffectBackend, Resource, effect_error};
use quiver_core::ProcessId;
use quiver_core::effects::{EffectError, EffectResult};
use quiver_core::error::Error;
use quiver_core::process::StreamEvent;
use quiver_core::value::ResourceId;
use quiver_core::wire::WireValue;
use std::ffi::OsStr;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::PermissionsExt;
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// A running (or exited, unreaped) child.
pub struct ChildProcess {
    child: std::process::Child,
    pidfd: OwnedFd,
    /// How it ended, once known — kept so a later select answers the same end again.
    end: Option<StreamEvent>,
}

impl ChildProcess {
    pub(super) fn pidfd(&self) -> &OwnedFd {
        &self.pidfd
    }

    /// The end event, if the child has exited: peeked with `WNOWAIT`, so the child stays a
    /// zombie until the resource is dropped. `None` while it runs.
    pub(super) fn end(&mut self) -> std::io::Result<Option<StreamEvent>> {
        if let Some(end) = &self.end {
            return Ok(Some(end.clone()));
        }
        let mut info: libc::siginfo_t = unsafe { std::mem::zeroed() };
        let status = unsafe {
            libc::waitid(
                libc::P_PIDFD,
                self.pidfd.as_raw_fd() as libc::id_t,
                &mut info,
                libc::WEXITED | libc::WNOHANG | libc::WNOWAIT,
            )
        };
        if status < 0 {
            return Err(std::io::Error::last_os_error());
        }
        // With `WNOHANG`, a child still running leaves the info zeroed.
        if unsafe { info.si_pid() } == 0 {
            return Ok(None);
        }
        let value = unsafe { info.si_status() } as i64;
        let end = match info.si_code {
            libc::CLD_EXITED => StreamEvent::End {
                variant: 0,
                fields: vec![WireValue::Int(value)],
            },
            // Killed or dumped: the status is the signal.
            _ => StreamEvent::End {
                variant: 1,
                fields: vec![WireValue::Int(value)],
            },
        };
        self.end = Some(end.clone());
        Ok(Some(end))
    }

    /// Signal the child's process group. A group with nobody left in it is not an error:
    /// the signal had nothing to reach.
    pub(super) fn signal(&self, signal: i32) -> std::io::Result<()> {
        let group = self.child.id() as libc::pid_t;
        if unsafe { libc::kill(-group, signal) } < 0 {
            let error = std::io::Error::last_os_error();
            if error.raw_os_error() != Some(libc::ESRCH) {
                return Err(error);
            }
        }
        Ok(())
    }
}

impl Drop for ChildProcess {
    /// Kill the group, then reap. The child's pid is still reserved (it is unreaped), so the
    /// group id names only what it started. Reaping waits for the kill to land, so it runs
    /// off the backend thread.
    fn drop(&mut self) {
        let _ = self.signal(libc::SIGKILL);
        let pid = self.child.id() as libc::pid_t;
        std::thread::spawn(move || unsafe {
            libc::waitpid(pid, std::ptr::null_mut(), 0);
        });
    }
}

/// Find `cmd` the way a shell would, but only through absolute `PATH` entries: an empty or
/// relative entry would resolve against the working directory, running whatever happens to
/// be there under a trusted name. A name with a `/` in it is a path, and used as given.
fn resolve_program(cmd: &[u8], path: Option<&OsStr>) -> std::io::Result<PathBuf> {
    if cmd.contains(&b'/') {
        return Ok(PathBuf::from(OsStr::from_bytes(cmd)));
    }
    let not_found = || {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("no program '{}' on PATH", String::from_utf8_lossy(cmd)),
        )
    };
    let path = path.ok_or_else(not_found)?;
    std::env::split_paths(path)
        .filter(|dir| dir.is_absolute())
        .map(|dir| dir.join(OsStr::from_bytes(cmd)))
        .find(|candidate| is_executable(candidate))
        .ok_or_else(not_found)
}

fn is_executable(path: &Path) -> bool {
    path.metadata()
        .is_ok_and(|metadata| metadata.is_file() && metadata.permissions().mode() & 0o111 != 0)
}

/// What a spawn hands back: the child and its three pipes.
struct Spawned {
    child: ChildProcess,
    stdin: OwnedFd,
    stdout: OwnedFd,
    stderr: OwnedFd,
}

fn spawn(
    cmd: &[u8],
    args: &[Vec<u8>],
    cwd: &[u8],
    env: &[Vec<u8>],
    clear_env: bool,
) -> std::io::Result<Spawned> {
    // Each entry sets `NAME=value` or unsets `NAME`, in order.
    let changes: Vec<(&OsStr, Option<&OsStr>)> = env
        .iter()
        .map(|entry| match entry.iter().position(|&byte| byte == b'=') {
            Some(at) => (
                OsStr::from_bytes(&entry[..at]),
                Some(OsStr::from_bytes(&entry[at + 1..])),
            ),
            None => (OsStr::from_bytes(entry), None),
        })
        .collect();
    // The program is looked up on the PATH the child will see.
    let path = match changes.iter().rev().find(|(name, _)| *name == "PATH") {
        Some((_, value)) => value.map(OsStr::to_owned),
        None if clear_env => None,
        None => std::env::var_os("PATH"),
    };
    let mut command = Command::new(resolve_program(cmd, path.as_deref())?);
    if clear_env {
        command.env_clear();
    }
    for (name, value) in changes {
        match value {
            Some(value) => command.env(name, value),
            None => command.env_remove(name),
        };
    }
    command
        .args(args.iter().map(|arg| OsStr::from_bytes(arg)))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .process_group(0);
    // The backstop for a host that dies without dropping its resources (a crash, a
    // SIGKILL): the kernel kills the child when the thread that forked it ends. That is a
    // blocking-pool thread, which lives as long as the backend, so the child goes when the
    // backend does at the latest. What the child started is reached only by the ordinary
    // path, which kills the whole group.
    let host = std::process::id() as libc::pid_t;
    unsafe {
        command.pre_exec(move || {
            if libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGKILL) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            // The host may have gone before the death signal was armed.
            if libc::getppid() != host {
                libc::_exit(1);
            }
            Ok(())
        });
    }
    if !cwd.is_empty() {
        command.current_dir(OsStr::from_bytes(cwd));
    }
    let mut child = command.spawn()?;
    let pidfd = unsafe { libc::syscall(libc::SYS_pidfd_open, child.id() as libc::pid_t, 0) };
    if pidfd < 0 {
        let error = std::io::Error::last_os_error();
        let _ = child.kill();
        let _ = child.wait();
        return Err(error);
    }
    let pidfd = unsafe { OwnedFd::from_raw_fd(pidfd as i32) };
    let stdin = OwnedFd::from(child.stdin.take().expect("stdin is piped"));
    let stdout = OwnedFd::from(child.stdout.take().expect("stdout is piped"));
    let stderr = OwnedFd::from(child.stderr.take().expect("stderr is piped"));
    Ok(Spawned {
        child: ChildProcess {
            child,
            pidfd,
            end: None,
        },
        stdin,
        stdout,
        stderr,
    })
}

impl NativeEffectBackend {
    /// `__exec_spawn__`: fork and exec on the blocking pool (PATH lookup and the exec itself
    /// block), then register the child and its pipes back on the backend thread.
    pub(super) fn execute_exec_spawn(
        &mut self,
        process_id: ProcessId,
        cmd: Vec<u8>,
        args: Vec<Vec<u8>>,
        cwd: Vec<u8>,
        env: Vec<Vec<u8>>,
        clear_env: bool,
    ) -> Result<Option<EffectResult>, Error> {
        self.run_blocking(process_id, move || {
            let outcome = spawn(&cmd, &args, &cwd, &env, clear_env);
            let context = format!("cannot run '{}'", String::from_utf8_lossy(&cmd));
            Box::new(move |backend: &mut NativeEffectBackend| {
                let spawned = match outcome {
                    Ok(spawned) => spawned,
                    Err(error) => return Ok(Err(effect_error(&context, &error))),
                };
                let tuple_id = backend.result_info("exec_spawn")?.tuple_id;
                let child = backend.register_resource(Resource::Child(spawned.child), "Child");
                let stdin = backend.register_resource(
                    Resource::Pipe {
                        fd: spawned.stdin,
                        kind: "ByteSink",
                    },
                    "ByteSink",
                );
                let stdout = backend.register_resource(
                    Resource::Pipe {
                        fd: spawned.stdout,
                        kind: "ByteSource",
                    },
                    "ByteSource",
                );
                let stderr = backend.register_resource(
                    Resource::Pipe {
                        fd: spawned.stderr,
                        kind: "ByteSource",
                    },
                    "ByteSource",
                );
                Ok(Ok(WireValue::tuple(
                    tuple_id,
                    vec![child, stdin, stdout, stderr],
                )))
            })
        })
    }

    /// `__exec_kill__`: signal the child's process group.
    pub(super) fn execute_exec_kill(
        &mut self,
        resource_id: ResourceId,
        signal: i32,
    ) -> Result<Option<EffectResult>, Error> {
        let Some(Resource::Child(child)) = self.resources.get(&resource_id) else {
            return Err(Error::InvalidArgument(format!(
                "Resource {resource_id} is not a child"
            )));
        };
        Ok(Some(match child.signal(signal) {
            Ok(()) => Ok(WireValue::ok()),
            Err(error) => Err(effect_error("cannot signal child", &error)),
        }))
    }

    /// The end event a child resource answers, or `None` while it runs — the failure of the
    /// peek itself answering `Failed`.
    pub(super) fn child_end(&mut self, resource_id: ResourceId) -> Option<StreamEvent> {
        let Some(Resource::Child(child)) = self.resources.get_mut(&resource_id) else {
            return Some(StreamEvent::Failed {
                error: EffectError::InvalidArgument(format!(
                    "Resource {resource_id} is not a child"
                )),
            });
        };
        match child.end() {
            Ok(end) => end,
            Err(error) => Some(StreamEvent::Failed {
                error: effect_error("cannot wait for child", &error),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::resolve_program;
    use std::ffi::OsStr;

    #[test]
    fn programs_resolve_only_through_absolute_path_entries() {
        let found = resolve_program(b"sh", Some(OsStr::new("/nonexistent:/bin:/usr/bin")))
            .expect("sh is on an absolute entry");
        assert!(found.is_absolute());
        // An empty entry and a relative one both mean "the working directory" to a shell;
        // neither may be searched.
        assert!(resolve_program(b"sh", Some(OsStr::new(":.:bin"))).is_err());
        assert!(resolve_program(b"sh", None).is_err());
        // A path is used as given.
        assert_eq!(
            resolve_program(b"./x", None).unwrap(),
            std::path::PathBuf::from("./x")
        );
    }
}
