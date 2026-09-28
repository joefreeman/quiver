//! Filesystem effects. Each is a blocking call, so it runs on the blocking pool and finishes
//! back on the backend thread, where results are registered and stamped with type ids.

use super::{NativeEffectBackend, Resource, effect_error, kind_tag, os_path};
use quiver_core::ProcessId;
use quiver_core::effects::EffectResult;
use quiver_core::error::Error;
use quiver_core::value::ResourceId;
use quiver_core::wire::WireValue;
use std::fs::{DirBuilder, FileType, OpenOptions, Permissions};
use std::io::ErrorKind;
use std::os::unix::ffi::OsStringExt;
use std::os::unix::fs::{DirBuilderExt, MetadataExt, OpenOptionsExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

/// A `'%fs.kind` tag's name for a file type. `Other` covers sockets, FIFOs and devices.
fn kind_name(file_type: FileType) -> &'static str {
    if file_type.is_dir() {
        "Dir"
    } else if file_type.is_symlink() {
        "Symlink"
    } else if file_type.is_file() {
        "File"
    } else {
        "Other"
    }
}

fn owned_path(bytes: &[u8]) -> PathBuf {
    os_path(bytes).to_path_buf()
}

/// A path answered to Quiver: its raw bytes, which `%fs` parses into a `'%path`.
fn path_value(path: PathBuf) -> WireValue {
    WireValue::Binary(path.into_os_string().into_vec().into())
}

fn ok_value((): ()) -> WireValue {
    WireValue::ok()
}

/// Remove a file, symlink or directory — a directory's contents too when `recursive`. The
/// path itself is described without following it, so a symlink is removed rather than its
/// target, and `remove_dir_all` never descends through one.
fn remove(path: &Path, recursive: bool) -> std::io::Result<()> {
    if std::fs::symlink_metadata(path)?.is_dir() {
        if recursive {
            std::fs::remove_dir_all(path)
        } else {
            std::fs::remove_dir(path)
        }
    } else {
        std::fs::remove_file(path)
    }
}

/// Copy a regular file's contents and permission bits. Without `replace`, the destination is
/// created exclusively, so an existing one fails with `AlreadyExists` rather than being
/// clobbered.
fn copy(from: &Path, to: &Path, replace: bool) -> std::io::Result<()> {
    // Checked before opening: opening a FIFO would block until a writer arrived.
    let metadata = std::fs::metadata(from)?;
    if metadata.is_dir() {
        return Err(ErrorKind::IsADirectory.into());
    }
    if !metadata.is_file() {
        return Err(std::io::Error::other("not a regular file"));
    }
    let mut source = std::fs::File::open(from)?;

    let mut options = OpenOptions::new();
    options.write(true).mode(metadata.permissions().mode());
    if replace {
        // Not truncated on open: the destination may be the source itself.
        options.create(true);
    } else {
        options.create_new(true);
    }
    let mut destination = options.open(to)?;
    let existing = destination.metadata()?;
    if (existing.dev(), existing.ino()) == (metadata.dev(), metadata.ino()) {
        return Err(std::io::Error::new(
            ErrorKind::InvalidInput,
            "source and destination are the same file",
        ));
    }
    destination.set_len(0)?;
    std::io::copy(&mut source, &mut destination)?;
    // An existing destination kept its own bits through the open.
    destination.set_permissions(metadata.permissions())
}

/// Create a fresh directory under the host's temp dir, private to its owner as `mkdtemp`'s
/// are. The name is random; a collision just draws again.
fn temp_dir() -> std::io::Result<PathBuf> {
    let base = std::env::temp_dir();
    loop {
        let suffix = getrandom::u64().map_err(|e| std::io::Error::other(e.to_string()))?;
        let path = base.join(format!("quiver-{suffix:016x}"));
        match DirBuilder::new().mode(0o700).create(&path) {
            Ok(()) => return Ok(path),
            Err(e) if e.kind() == ErrorKind::AlreadyExists => continue,
            Err(e) => return Err(e),
        }
    }
}

impl NativeEffectBackend {
    pub(super) fn execute_file_open(
        &mut self,
        process_id: ProcessId,
        path: Vec<u8>,
        flags: i32,
        mode: u32,
    ) -> Result<Option<EffectResult>, Error> {
        let mut options = std::fs::OpenOptions::new();
        // Access mode (O_RDONLY=0, O_WRONLY=1, O_RDWR=2)
        match flags & 0x3 {
            0 => options.read(true),
            1 => options.write(true),
            2 => options.read(true).write(true),
            access => {
                return Err(Error::InvalidArgument(format!(
                    "invalid access mode {access} in open flags"
                )));
            }
        };
        // O_CREAT, O_EXCL, O_TRUNC, O_APPEND
        options
            .create(flags & 0o100 != 0)
            .create_new(flags & 0o100 != 0 && flags & 0o200 != 0)
            .truncate(flags & 0o1000 != 0)
            .append(flags & 0o2000 != 0)
            .mode(mode);

        let path = owned_path(&path);
        self.run_blocking(process_id, move || {
            let opened = options.open(&path);
            Box::new(move |backend: &mut NativeEffectBackend| {
                let file = match opened {
                    Ok(file) => file,
                    Err(e) => {
                        return Ok(Err(effect_error(
                            &format!("cannot open '{}'", path.display()),
                            &e,
                        )));
                    }
                };
                Ok(Ok(
                    backend.register_resource(Resource::File { file }, "File")
                ))
            })
        })
    }

    pub(super) fn execute_stat(
        &mut self,
        process_id: ProcessId,
        path: Vec<u8>,
        follow: bool,
    ) -> Result<Option<EffectResult>, Error> {
        let path = owned_path(&path);
        self.run_blocking(process_id, move || {
            // `follow` is `stat`, otherwise `lstat`.
            let lookup = if follow {
                std::fs::metadata(&path)
            } else {
                std::fs::symlink_metadata(&path)
            };
            Box::new(move |backend: &mut NativeEffectBackend| {
                // A path that is not there answers nil — the ordinary "found nothing"; a
                // *failed* lookup also answers nil, but carrying an `:error` payload, so a
                // caller that cares can tell them apart.
                let metadata = match lookup {
                    Ok(metadata) => metadata,
                    Err(e) if e.kind() == ErrorKind::NotFound => return Ok(Ok(WireValue::nil())),
                    Err(e) => {
                        return Ok(Err(effect_error(
                            &format!("cannot stat '{}'", path.display()),
                            &e,
                        )));
                    }
                };
                // mtime as nanoseconds since the Unix epoch — negative for times before it.
                let modified = metadata
                    .modified()
                    .map_err(|e| Error::InvalidArgument(format!("host reports no mtime: {e}")))?;
                let modified_nanos = match modified.duration_since(std::time::UNIX_EPOCH) {
                    Ok(after) => after.as_nanos() as i64,
                    Err(before) => -(before.duration().as_nanos() as i64),
                };
                let perm = metadata.permissions().mode() & 0o7777;

                // `[kind, size, modified, perm]`, stamped with `filesystem_stat`'s real result
                // type ids (pushed by the environment — the backend has no type registry).
                let info = backend.result_info("filesystem_stat")?;
                Ok(Ok(WireValue::tuple(
                    info.tuple_id,
                    vec![
                        kind_tag(info, kind_name(metadata.file_type()))?,
                        WireValue::Int(metadata.len() as i64),
                        WireValue::Int(modified_nanos),
                        WireValue::Int(perm as i64),
                    ],
                )))
            })
        })
    }

    /// Run a path operation on the blocking pool, answering `answer` of its result, or its
    /// failure — described by `context` — as an outcome.
    fn run_path_op<T: Send + 'static>(
        &mut self,
        process_id: ProcessId,
        context: String,
        op: impl FnOnce() -> std::io::Result<T> + Send + 'static,
        answer: fn(T) -> WireValue,
    ) -> Result<Option<EffectResult>, Error> {
        self.run_blocking(process_id, move || {
            let outcome = op();
            Box::new(move |_: &mut NativeEffectBackend| {
                Ok(outcome.map(answer).map_err(|e| effect_error(&context, &e)))
            })
        })
    }

    pub(super) fn execute_create_dir(
        &mut self,
        process_id: ProcessId,
        path: Vec<u8>,
        all: bool,
    ) -> Result<Option<EffectResult>, Error> {
        let path = owned_path(&path);
        let context = format!("cannot create directory '{}'", path.display());
        self.run_path_op(
            process_id,
            context,
            move || DirBuilder::new().recursive(all).create(&path),
            ok_value,
        )
    }

    pub(super) fn execute_remove(
        &mut self,
        process_id: ProcessId,
        path: Vec<u8>,
        recursive: bool,
    ) -> Result<Option<EffectResult>, Error> {
        let path = owned_path(&path);
        let context = format!("cannot remove '{}'", path.display());
        self.run_path_op(
            process_id,
            context,
            move || remove(&path, recursive),
            ok_value,
        )
    }

    pub(super) fn execute_rename(
        &mut self,
        process_id: ProcessId,
        from: Vec<u8>,
        to: Vec<u8>,
    ) -> Result<Option<EffectResult>, Error> {
        let (from, to) = (owned_path(&from), owned_path(&to));
        let context = format!("cannot rename '{}' to '{}'", from.display(), to.display());
        self.run_path_op(
            process_id,
            context,
            move || std::fs::rename(&from, &to),
            ok_value,
        )
    }

    pub(super) fn execute_copy(
        &mut self,
        process_id: ProcessId,
        from: Vec<u8>,
        to: Vec<u8>,
        replace: bool,
    ) -> Result<Option<EffectResult>, Error> {
        let (from, to) = (owned_path(&from), owned_path(&to));
        let context = format!("cannot copy '{}' to '{}'", from.display(), to.display());
        self.run_path_op(
            process_id,
            context,
            move || copy(&from, &to, replace),
            ok_value,
        )
    }

    pub(super) fn execute_symlink(
        &mut self,
        process_id: ProcessId,
        link: Vec<u8>,
        target: Vec<u8>,
    ) -> Result<Option<EffectResult>, Error> {
        let (link, target) = (owned_path(&link), owned_path(&target));
        let context = format!(
            "cannot create symlink '{}' to '{}'",
            link.display(),
            target.display()
        );
        self.run_path_op(
            process_id,
            context,
            move || std::os::unix::fs::symlink(&target, &link),
            ok_value,
        )
    }

    pub(super) fn execute_read_link(
        &mut self,
        process_id: ProcessId,
        path: Vec<u8>,
    ) -> Result<Option<EffectResult>, Error> {
        let path = owned_path(&path);
        let context = format!("cannot read link '{}'", path.display());
        self.run_path_op(
            process_id,
            context,
            move || std::fs::read_link(&path),
            path_value,
        )
    }

    pub(super) fn execute_set_perm(
        &mut self,
        process_id: ProcessId,
        path: Vec<u8>,
        perm: u32,
    ) -> Result<Option<EffectResult>, Error> {
        let path = owned_path(&path);
        let context = format!("cannot set permissions of '{}'", path.display());
        self.run_path_op(
            process_id,
            context,
            move || std::fs::set_permissions(&path, Permissions::from_mode(perm)),
            ok_value,
        )
    }

    pub(super) fn execute_canonical(
        &mut self,
        process_id: ProcessId,
        path: Vec<u8>,
    ) -> Result<Option<EffectResult>, Error> {
        let path = owned_path(&path);
        let context = format!("cannot resolve '{}'", path.display());
        self.run_path_op(
            process_id,
            context,
            move || std::fs::canonicalize(&path),
            path_value,
        )
    }

    pub(super) fn execute_cwd(
        &mut self,
        process_id: ProcessId,
    ) -> Result<Option<EffectResult>, Error> {
        let context = "cannot read the working directory".to_string();
        self.run_path_op(process_id, context, std::env::current_dir, path_value)
    }

    pub(super) fn execute_temp(
        &mut self,
        process_id: ProcessId,
    ) -> Result<Option<EffectResult>, Error> {
        let context = "cannot create a temporary directory".to_string();
        self.run_path_op(process_id, context, temp_dir, path_value)
    }

    pub(super) fn execute_read_dir_open(
        &mut self,
        process_id: ProcessId,
        path: Vec<u8>,
    ) -> Result<Option<EffectResult>, Error> {
        let path = owned_path(&path);
        self.run_blocking(process_id, move || {
            let opened = std::fs::read_dir(&path);
            Box::new(move |backend: &mut NativeEffectBackend| {
                let entries = match opened {
                    Ok(entries) => entries,
                    Err(e) => {
                        return Ok(Err(effect_error(
                            &format!("cannot read directory '{}'", path.display()),
                            &e,
                        )));
                    }
                };
                let entries = Arc::new(Mutex::new(entries));
                Ok(Ok(
                    backend.register_resource(Resource::Dir { entries }, "Dir")
                ))
            })
        })
    }

    pub(super) fn execute_read_dir_next(
        &mut self,
        process_id: ProcessId,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        let Some(Resource::Dir { entries }) = self.resources.get(&resource_id) else {
            return Err(Error::InvalidArgument(format!(
                "Resource {resource_id} is not an open Dir"
            )));
        };
        // The pool thread holds its own reference, so a close while the read is in flight
        // drops the resource without pulling the iterator out from under it.
        let entries = Arc::clone(entries);
        self.run_blocking(process_id, move || {
            let next = entries.lock().expect("directory lock").next().map(|entry| {
                entry.map(|entry| {
                    // The entry's own type, without following symlinks. `d_type` from
                    // `getdents` is essentially free; `file_type()` only falls back to an
                    // `lstat` on the rare filesystems that report `DT_UNKNOWN`.
                    let kind = entry.file_type().map_or("Other", kind_name);
                    (entry.file_name().into_vec(), kind)
                })
            });
            Box::new(move |backend: &mut NativeEffectBackend| match next {
                Some(Ok((name, kind))) => {
                    // `[name, kind]`, stamped with `directory_next`'s real result type ids.
                    let info = backend.result_info("directory_next")?;
                    Ok(Ok(WireValue::tuple(
                        info.tuple_id,
                        vec![WireValue::Binary(name.into()), kind_tag(info, kind)?],
                    )))
                }
                Some(Err(e)) => Ok(Err(effect_error("cannot read directory entry", &e))),
                None => Ok(Ok(WireValue::nil())),
            })
        })
    }

    pub(super) fn execute_read_dir_close(
        &mut self,
        resource_id: ResourceId,
    ) -> Result<Option<EffectResult>, Error> {
        self.release(resource_id)
            .ok_or_else(|| Error::InvalidArgument(format!("Resource {} not found", resource_id)))?;

        Ok(Some(Ok(WireValue::ok())))
    }
}
