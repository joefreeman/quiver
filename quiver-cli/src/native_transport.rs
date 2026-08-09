use quiver_environment::{
    Command, CommandReceiver, EnvironmentError, Event, EventSender, Worker, WorkerHandle,
};
use quiver_io::NativeEffect;
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::thread::{self, JoinHandle};

/// Command receiver using mpsc::Receiver
pub struct NativeCommandReceiver {
    receiver: Receiver<Command<NativeEffect>>,
    /// Taken by [`wait`](CommandReceiver::wait) and handed to the next `try_recv`.
    /// `mpsc::Receiver` has no peek, so blocking for a command necessarily *removes* it;
    /// this is where it waits rather than being lost.
    waited: Option<Command<NativeEffect>>,
}

impl CommandReceiver<NativeEffect> for NativeCommandReceiver {
    fn try_recv(&mut self) -> Result<Option<Command<NativeEffect>>, EnvironmentError> {
        if let Some(command) = self.waited.take() {
            return Ok(Some(command));
        }
        match self.receiver.try_recv() {
            Ok(cmd) => Ok(Some(cmd)),
            Err(TryRecvError::Empty) => Ok(None),
            Err(TryRecvError::Disconnected) => Err(EnvironmentError::ChannelDisconnected),
        }
    }

    fn wait(&mut self, timeout: Option<std::time::Duration>) {
        if self.waited.is_some() {
            return;
        }
        // A disconnected channel returns immediately; the next `try_recv` reports it as the
        // error, so there is no need to distinguish the two outcomes here.
        self.waited = match timeout {
            Some(timeout) => self.receiver.recv_timeout(timeout).ok(),
            None => self.receiver.recv().ok(),
        };
    }
}

/// Event sender using mpsc::Sender
pub struct NativeEventSender {
    sender: Sender<Event<NativeEffect>>,
    /// Poked after each event, so the environment loop can block rather than poll for one.
    waker: Waker,
}

impl EventSender<NativeEffect> for NativeEventSender {
    fn send(&mut self, event: Event<NativeEffect>) -> Result<(), EnvironmentError> {
        let sent = self.sender.send(event).map_err(|e| {
            EnvironmentError::WorkerCommunication(format!("Failed to send event: {}", e))
        });
        // Wake even on a failed send: the failure is a disconnected environment, and the loop
        // should get a chance to notice rather than sleep through it.
        self.waker.wake();
        sent
    }
}

/// Worker handle for native implementation
pub struct NativeWorkerHandle {
    cmd_sender: Sender<Command<NativeEffect>>,
    evt_receiver: Receiver<Event<NativeEffect>>,
    _thread_handle: JoinHandle<()>,
}

impl WorkerHandle<NativeEffect> for NativeWorkerHandle {
    fn send(&mut self, command: Command<NativeEffect>) -> Result<(), EnvironmentError> {
        self.cmd_sender.send(command).map_err(|e| {
            EnvironmentError::WorkerCommunication(format!("Failed to send command: {}", e))
        })
    }

    /// Worker threads, so a table crosses as a pointer.
    fn shares_memory(&self) -> bool {
        true
    }

    fn try_recv(&mut self) -> Result<Option<Event<NativeEffect>>, EnvironmentError> {
        match self.evt_receiver.try_recv() {
            Ok(event) => Ok(Some(event)),
            Err(TryRecvError::Empty) => Ok(None),
            Err(TryRecvError::Disconnected) => Err(EnvironmentError::ChannelDisconnected),
        }
    }
}

/// The clock a worker runs on, together with how it waits in that clock.
///
/// The two belong in one place because a worker's deadlines are in whatever units `now_ms`
/// returns, while the only wait a command can interrupt (`recv_timeout`) is in real ones.
/// Translating between them is something only the clock's owner can do — supplying the clock
/// alone is what once made a worker sleep 100 *real* ms for a 100 *virtual* ms deadline, firing
/// a `![2000, 100, 500]` select at 1507 ms.
pub trait WorkerClock: Send + 'static {
    /// Now, in this clock's units.
    fn now_ms(&self) -> u64;

    /// How long an idle worker should wait for a command, given the earliest pending select
    /// deadline in this clock's units. `None` waits indefinitely — correct precisely when no
    /// deadline is pending, since nothing but a command can then make the worker runnable.
    fn wait_for(&self, deadline_ms: Option<u64>) -> Option<std::time::Duration>;
}

/// Wall-clock time since the Unix epoch — what a real run uses. Deadlines are real durations,
/// so an idle worker waits exactly as long as the nearest one, and a command cuts it short.
pub struct SystemClock;

impl WorkerClock for SystemClock {
    fn now_ms(&self) -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system time before the Unix epoch")
            .as_millis() as u64
    }

    fn wait_for(&self, deadline_ms: Option<u64>) -> Option<std::time::Duration> {
        deadline_ms.map(|at| std::time::Duration::from_millis(at.saturating_sub(self.now_ms())))
    }
}

/// A clock the *driver* advances, for tests: it moves on by one per idle pass of the environment
/// loop rather than with the wall clock, which lets a test exercise long timeouts in no time at
/// all.
///
/// Time here passes only while someone is looking, so a pending deadline cannot be waited *out*
/// — the worker has to come back and re-read the clock, which it does on a short real interval.
/// With no deadline, waiting for a command is still exactly right: nothing else can wake the
/// worker in either clock.
pub struct SteppedClock {
    now_ms: std::sync::Arc<std::sync::atomic::AtomicU64>,
}

impl SteppedClock {
    /// How often an idle worker re-reads a clock it cannot wait on. Short, because the driver
    /// advances this clock as fast as it can spin and a deadline should land promptly; not zero,
    /// because that is a busy-wait. Measured indistinguishable from yielding (`processes`: 0.23 s
    /// vs 0.25 s wall, 0.62 s of CPU either way), so the gentler one wins.
    const RECHECK: std::time::Duration = std::time::Duration::from_micros(100);

    pub fn new(now_ms: std::sync::Arc<std::sync::atomic::AtomicU64>) -> Self {
        SteppedClock { now_ms }
    }
}

impl WorkerClock for SteppedClock {
    fn now_ms(&self) -> u64 {
        self.now_ms.load(std::sync::atomic::Ordering::Relaxed)
    }

    fn wait_for(&self, deadline_ms: Option<u64>) -> Option<std::time::Duration> {
        deadline_ms.map(|_| Self::RECHECK)
    }
}

/// The environment loop's "something happened" signal.
///
/// The environment collects from several places — every worker's event channel, and the io
/// backend's completions — so it has no single handle to wait on and used to poll. This is that
/// handle: every worker pokes it after sending an event, so the driver can block instead, and a
/// routed message crosses the loop as fast as a thread can be woken rather than at the next tick.
///
/// Capacity **1**, and a poke that finds it full is dropped. A wake signal is idempotent — one
/// pending wake and ten mean the same thing, "go and look" — so coalescing is correct rather
/// than lossy, and it is what stops an unread signal (a driver that idles differently, as the
/// REPL's does) from growing without bound.
#[derive(Clone)]
pub struct Waker {
    sender: mpsc::SyncSender<()>,
}

impl Waker {
    /// Signal that there may be work. Never blocks and never fails: a full channel already
    /// carries the same message.
    pub fn wake(&self) {
        let _ = self.sender.try_send(());
    }
}

/// The receiving half of a [`Waker`], held by the driver loop.
pub struct WakeSignal {
    receiver: Receiver<()>,
}

impl WakeSignal {
    /// How long to wait when the io backend has an operation outstanding. A kernel completion
    /// arrives through neither this channel nor a worker's, so it can only be *noticed*, and
    /// this is how often. Registering io_uring's eventfd and poking the waker from it would
    /// retire the last poll in the system; until then this is the pre-existing interval, so a
    /// program doing io behaves exactly as it did.
    const IO_POLL: std::time::Duration = std::time::Duration::from_millis(5);

    /// Block until a worker signals, or — when `io_in_flight` — until it is time to look for a
    /// completion. Consumes every pending signal, since they all mean the same thing.
    ///
    /// Select deadlines need no handling here: a worker owns its own timers (see
    /// [`WorkerClock`]) and wakes this loop by sending the event that results.
    pub fn wait(&self, io_in_flight: bool) {
        if io_in_flight {
            let _ = self.receiver.recv_timeout(Self::IO_POLL);
        } else {
            let _ = self.receiver.recv();
        }
        while self.receiver.try_recv().is_ok() {}
    }
}

/// A [`Waker`] and its [`WakeSignal`]. Hand clones of the waker to every worker and keep the
/// signal in the loop that drives the environment.
pub fn wake_channel() -> (Waker, WakeSignal) {
    let (sender, receiver) = mpsc::sync_channel(1);
    (Waker { sender }, WakeSignal { receiver })
}

/// A progress signal between an environment's stepping thread and threads waiting on
/// requests it will resolve. The stepping thread calls [`Progress::notify`] after every
/// productive step; a waiter snapshots [`Progress::generation`], polls its request, and
/// — if unresolved — sleeps in [`Progress::wait_past`] until the generation moves on or
/// a timeout elapses (the bound that keeps interrupt flags responsive). Snapshotting
/// *before* polling is what makes the missed-wakeup race benign: a resolution landing
/// between the poll and the wait has already advanced the generation, so the wait
/// returns at once.
#[derive(Default)]
pub struct Progress {
    generation: std::sync::Mutex<u64>,
    condvar: std::sync::Condvar,
}

impl Progress {
    pub fn new() -> Self {
        Self::default()
    }

    /// The current generation, to snapshot before polling.
    pub fn generation(&self) -> u64 {
        *self.generation.lock().unwrap()
    }

    /// Record progress and wake every waiter.
    pub fn notify(&self) {
        *self.generation.lock().unwrap() += 1;
        self.condvar.notify_all();
    }

    /// Block until the generation has moved past `seen`, or `timeout` elapses.
    pub fn wait_past(&self, seen: u64, timeout: std::time::Duration) {
        let guard = self.generation.lock().unwrap();
        let _ = self
            .condvar
            .wait_timeout_while(guard, timeout, |generation| *generation == seen)
            .unwrap();
    }
}

/// Spawn a native worker thread on the given clock (see [`WorkerClock`]).
pub fn spawn_worker<C: WorkerClock>(
    clock: C,
    builtins: quiver_core::builtins::BuiltinRegistry<NativeEffect>,
    worker_id: u16,
    waker: Waker,
) -> NativeWorkerHandle {
    let (cmd_tx, cmd_rx) = mpsc::channel();
    let (evt_tx, evt_rx) = mpsc::channel();

    // Workers take the default stack. They used to reserve 256 MiB, because the runtime walked
    // value structure recursively — dropping, converting, comparing or rendering a value cost
    // stack proportional to its *nesting*, and a Cons list nests once per element. Those walks
    // are all iterative now, so depth costs heap and an ordinary stack is enough.
    let thread_handle = thread::Builder::new()
        .name(format!("quiver-worker-{worker_id}"))
        .spawn(move || {
            let cmd_receiver = NativeCommandReceiver {
                receiver: cmd_rx,
                waited: None,
            };

            // Clone the sender so we can use it for error reporting
            let error_sender = evt_tx.clone();
            let error_waker = waker.clone();
            let evt_sender = NativeEventSender {
                sender: evt_tx,
                waker,
            };

            let mut worker =
                Worker::<NativeEffect, _, _>::new(cmd_receiver, evt_sender, builtins, worker_id);

            // Run the worker loop
            loop {
                let current_time_ms = clock.now_ms();

                match worker.step(current_time_ms) {
                    Ok(true) => {
                        // Work was done, continue immediately
                    }
                    Ok(false) => {
                        // Nothing to run: idle however this clock says to. This used to sleep a
                        // flat 5 ms, which cost no CPU but put those 5 ms on the latency of
                        // every message routed here — an idle worker is precisely one about to
                        // be handed work.
                        worker.wait_for_commands(clock.wait_for(worker.next_timeout_ms()));
                    }
                    Err(e) => {
                        // Send error event to environment
                        let _ = error_sender.send(Event::WorkerError { error: e });
                        error_waker.wake();
                        break;
                    }
                }
            }
        })
        .expect("failed to spawn worker thread");

    NativeWorkerHandle {
        cmd_sender: cmd_tx,
        evt_receiver: evt_rx,
        _thread_handle: thread_handle,
    }
}
