//! A small thread pool for the effects the kernel offers no asynchronous form of.
//!
//! io_uring covers the data plane (reads, writes, fsync, sockets), but path and metadata
//! operations — listing a directory, resolving a name, following a link — are blocking calls.
//! Running one on the backend thread stalls every in-flight operation behind it, so they run
//! here instead, the tokio/libuv approach. A job's result is handed back tagged with the id
//! it was submitted under, for the backend to finish on its own thread.

use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, Mutex, OnceLock};

/// How many threads serve the pool. Enough that one slow call (a stat on a hung network
/// mount) does not hold up the rest; these threads spend their time parked in the kernel.
const THREADS: usize = 4;

type Job<T> = Box<dyn FnOnce() -> T + Send>;

/// The job queue's receiving end, shared by the pool's threads.
type JobQueue<T> = Arc<Mutex<Receiver<(u64, Job<T>)>>>;

/// Called from a pool thread after each result, so a driver blocked waiting for work can
/// look at once rather than on its next poll.
pub type Notify = Box<dyn Fn() + Send + Sync>;

pub struct BlockingPool<T: Send + 'static> {
    jobs: Sender<(u64, Job<T>)>,
    /// The workers' shared end of `jobs`, held until the first submit starts them — a
    /// backend that never touches the filesystem never spawns a thread.
    idle: Option<JobQueue<T>>,
    results_sender: Sender<(u64, T)>,
    results: Receiver<(u64, T)>,
    notify: Arc<OnceLock<Notify>>,
}

impl<T: Send + 'static> BlockingPool<T> {
    pub fn new() -> Self {
        let (jobs, job_receiver) = mpsc::channel();
        let (results_sender, results) = mpsc::channel();
        BlockingPool {
            jobs,
            idle: Some(Arc::new(Mutex::new(job_receiver))),
            results_sender,
            results,
            notify: Arc::new(OnceLock::new()),
        }
    }

    /// Install the callback that wakes the driver. Set once, before any result is due.
    pub fn set_notify(&self, notify: Notify) {
        assert!(
            self.notify.set(notify).is_ok(),
            "blocking pool notifier already set"
        );
    }

    /// Run `job` on a pool thread; its result surfaces from [`take_results`] under `id`.
    ///
    /// [`take_results`]: Self::take_results
    pub fn submit(&mut self, id: u64, job: impl FnOnce() -> T + Send + 'static) {
        if let Some(receiver) = self.idle.take() {
            self.start(receiver);
        }
        self.jobs
            .send((id, Box::new(job)))
            .expect("blocking pool threads outlive the pool");
    }

    /// Every result completed since the last call.
    pub fn take_results(&self) -> Vec<(u64, T)> {
        self.results.try_iter().collect()
    }

    fn start(&self, receiver: JobQueue<T>) {
        for index in 0..THREADS {
            let receiver = Arc::clone(&receiver);
            let results = self.results_sender.clone();
            let notify = Arc::clone(&self.notify);
            std::thread::Builder::new()
                .name(format!("quiver-blocking-{index}"))
                .spawn(move || {
                    // The lock is held only to take a job, never while running one. A
                    // disconnected channel is the pool being dropped: the thread ends.
                    // Threads are detached rather than joined on drop, since a call that
                    // never returns (a hung mount) must not hang the backend with it.
                    loop {
                        let job = receiver.lock().expect("job queue lock").recv();
                        let Ok((id, job)) = job else { break };
                        if results.send((id, job())).is_err() {
                            break;
                        }
                        if let Some(notify) = notify.get() {
                            notify();
                        }
                    }
                })
                .expect("spawn blocking pool thread");
        }
    }
}

impl<T: Send + 'static> Default for BlockingPool<T> {
    fn default() -> Self {
        Self::new()
    }
}
