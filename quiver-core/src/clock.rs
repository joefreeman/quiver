//! The clock a worker runs on.

/// A monotonic clock: nanoseconds from an arbitrary origin, never stepping backwards.
///
/// One clock serves a worker twice over. Its select deadlines are measured on it, and
/// `__time_monotonic__` reads it, so a program timing itself sees exactly the time its timeouts
/// see — real time in a real run, and whatever time a test harness drives.
pub trait Clock: Send + Sync {
    /// Now, in nanoseconds from the clock's origin.
    fn now_ns(&self) -> u64;

    /// Now, in whole milliseconds from the clock's origin — the scale of select deadlines.
    fn now_ms(&self) -> u64 {
        self.now_ns() / 1_000_000
    }
}
