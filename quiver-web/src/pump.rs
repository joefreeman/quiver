//! Event-driven scheduler for the worker and environment loops.
//!
//! Instead of busy-rescheduling a `setTimeout(0)` tick forever, a [`Pump`] only runs a tick when
//! there is something to do. Each tick returns a [`Tick`] saying what to do next:
//!
//! - [`Tick::Busy`]  — there is more runnable work right now, so run again immediately.
//! - [`Tick::WakeAt`] — nothing runnable now, but a timer (e.g. a `select` timeout) expires at a
//!   known instant, so schedule a single timer for then.
//! - [`Tick::Idle`]  — nothing to do; stop scheduling entirely and sleep until [`Pump::wake`].
//!
//! External wake sources (an incoming `postMessage`, a queued evaluation) call [`Pump::wake`] to
//! wake an idle pump. Because each WASM context (worker or main thread) is single-threaded and a
//! tick callback never re-enters, the only invariant needed for correctness is "if a tick isn't
//! already scheduled, schedule one" — tracked here with a single pending-timer record. A missed
//! `wake` therefore surfaces as a stalled REPL, not as silent busy-looping.
//!
//! Immediate reschedules ([`Tick::Busy`], delay 0) post through a [`web_sys::MessageChannel`]
//! rather than `setTimeout(0)`. A `setTimeout` that reschedules itself hits the HTML spec's
//! nested-timeout clamp (a 4 ms floor after 5 levels), which throttles a long, busy computation to
//! ~250 ticks/second; a `MessageChannel` message is an unclamped 0-delay macrotask, so a busy pump
//! runs at full speed while still yielding to the event loop between ticks. Future deadlines
//! ([`Tick::WakeAt`]) still use `setTimeout` (the clamp is irrelevant to a real delay).

use std::cell::{Cell, RefCell};
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::{MessageChannel, MessageEvent};

/// A stored JS callback, kept alive for the pump's lifetime. `setTimeout` hands back a
/// zero-arg callback; a `MessagePort`'s `onmessage` receives the `MessageEvent`.
type TimerCallback = RefCell<Option<Closure<dyn FnMut()>>>;
type MessageCallback = RefCell<Option<Closure<dyn FnMut(MessageEvent)>>>;

/// What a pump tick wants to happen next.
pub enum Tick {
    /// More runnable work exists; run again as soon as possible.
    Busy,
    /// Nothing runnable now, but wake at this absolute time (ms, `Date.now()` scale).
    WakeAt(f64),
    /// Nothing to do; do not reschedule. The pump sleeps until `wake()`.
    Idle,
}

struct PumpInner {
    /// Global scope (`Window` or `DedicatedWorkerGlobalScope`); the `this` for the timer calls.
    global: JsValue,
    set_timeout: js_sys::Function,
    clear_timeout: js_sys::Function,
    /// Port we `postMessage` on to schedule an immediate (delay-0) tick without the `setTimeout`
    /// nesting clamp. Its peer's `onmessage` runs the tick.
    post_port: web_sys::MessagePort,
    /// The work to run per tick. Borrowed mutably only while a tick executes; never re-entered.
    step: RefCell<Box<dyn FnMut() -> Tick>>,
    /// The tick callback handed to `setTimeout` (delayed ticks). Stored to keep it alive.
    timer_closure: TimerCallback,
    /// The `onmessage` callback for the message-channel port (immediate ticks). Stored to keep it
    /// alive for the pump's lifetime.
    message_closure: MessageCallback,
    /// Id of the currently pending `setTimeout`, if a delayed tick is scheduled.
    timer: Cell<Option<f64>>,
    /// Absolute time (ms) the pending tick will fire. `None` when idle (nothing scheduled). For an
    /// immediate (message-channel) tick this is `now` and there is no `timer` id.
    next_wake: Cell<Option<f64>>,
}

/// A self-scheduling tick loop. Cheap to clone (shares one underlying scheduler).
#[derive(Clone)]
pub struct Pump {
    inner: Rc<PumpInner>,
}

fn now() -> f64 {
    js_sys::Date::now()
}

impl Pump {
    /// Build a pump around a per-tick `step`. The pump starts idle; call [`Pump::wake`] (or
    /// [`Pump::start`]) to run the first tick.
    pub fn new<F: FnMut() -> Tick + 'static>(step: F) -> Pump {
        let global = js_sys::global();
        let get = |name: &str| -> js_sys::Function {
            js_sys::Reflect::get(&global, &JsValue::from_str(name))
                .unwrap_or_else(|_| panic!("{name} not found"))
                .dyn_into()
                .unwrap_or_else(|_| panic!("{name} is not a function"))
        };
        let channel = MessageChannel::new().expect("MessageChannel::new failed");
        let inner = Rc::new(PumpInner {
            set_timeout: get("setTimeout"),
            clear_timeout: get("clearTimeout"),
            post_port: channel.port2(),
            global: global.into(),
            step: RefCell::new(Box::new(step)),
            timer_closure: RefCell::new(None),
            message_closure: RefCell::new(None),
            timer: Cell::new(None),
            next_wake: Cell::new(None),
        });

        // Both callbacks hold a strong ref back to `inner` so a tick can reschedule itself. This is
        // a deliberate reference cycle: a pump lives for the whole session, so it is never freed.
        let inner_for_timer = inner.clone();
        let timer_closure =
            Closure::wrap(Box::new(move || run_tick(&inner_for_timer)) as Box<dyn FnMut()>);
        *inner.timer_closure.borrow_mut() = Some(timer_closure);

        let inner_for_message = inner.clone();
        let message_closure =
            Closure::wrap(
                Box::new(move |_: MessageEvent| run_tick(&inner_for_message))
                    as Box<dyn FnMut(MessageEvent)>,
            );
        // Setting `onmessage` implicitly starts the port, so queued immediate ticks deliver.
        channel
            .port1()
            .set_onmessage(Some(message_closure.as_ref().unchecked_ref()));
        *inner.message_closure.borrow_mut() = Some(message_closure);

        Pump { inner }
    }

    /// Run the first tick (and thereafter whatever the tick loop schedules). Idempotent.
    pub fn start(&self) {
        schedule(&self.inner, now());
    }

    /// Wake an idle pump now. Safe to call at any time; if a tick is already scheduled at or
    /// before now this is a no-op, so it never stacks redundant timers.
    pub fn wake(&self) {
        schedule(&self.inner, now());
    }

    /// Cancel any pending tick. The pump goes idle until the next `wake`/`start`.
    pub fn cancel(&self) {
        if let Some(id) = self.inner.timer.take() {
            let _ = self
                .inner
                .clear_timeout
                .call1(&self.inner.global, &JsValue::from_f64(id));
        }
        self.inner.next_wake.set(None);
    }
}

/// Run one tick: clear the pending record (so the step's own scheduling decision starts from a
/// clean slate), run the step, then reschedule per its verdict. Shared by both the `setTimeout`
/// callback and the message-channel `onmessage` callback.
fn run_tick(inner: &Rc<PumpInner>) {
    inner.timer.set(None);
    inner.next_wake.set(None);

    let tick = (inner.step.borrow_mut())();
    match tick {
        Tick::Busy => schedule(inner, now()),
        Tick::WakeAt(target) => schedule(inner, target),
        Tick::Idle => {}
    }
}

/// Ensure a tick is scheduled to fire no later than `target` (absolute ms). If an earlier-or-equal
/// tick is already pending, does nothing; if a later (`setTimeout`) one is pending, it is cancelled
/// and replaced. A delay of 0 posts an unclamped message-channel tick; a real delay uses
/// `setTimeout`.
fn schedule(inner: &Rc<PumpInner>, target: f64) {
    let now = now();
    let target = target.max(now);

    match inner.next_wake.get() {
        // Already scheduled at or before the requested time — nothing to do. (A pending immediate
        // tick has `next_wake == its now <= target`, so it always wins here and is never cancelled
        // — which is good, since a posted message cannot be unscheduled.)
        Some(pending) if pending <= target => return,
        // A later `setTimeout` tick is pending; cancel it so we can schedule the sooner one.
        Some(_) => {
            if let Some(id) = inner.timer.take() {
                let _ = inner
                    .clear_timeout
                    .call1(&inner.global, &JsValue::from_f64(id));
            }
        }
        None => {}
    }

    let delay = target - now;
    if delay <= 0.0 {
        // Immediate: post a message to ourselves. Unlike `setTimeout(0)`, this is not subject to
        // the nested-timeout 4 ms clamp, so a busy pump runs at full speed.
        inner
            .post_port
            .post_message(&JsValue::NULL)
            .expect("postMessage call failed");
        inner.next_wake.set(Some(target));
    } else {
        let closure = inner.timer_closure.borrow();
        let func = closure
            .as_ref()
            .expect("pump closure is set during construction")
            .as_ref()
            .unchecked_ref::<js_sys::Function>();
        let id = inner
            .set_timeout
            .call2(&inner.global, func, &JsValue::from_f64(delay))
            .expect("setTimeout call failed");
        inner.timer.set(id.as_f64());
        inner.next_wake.set(Some(target));
    }
}
