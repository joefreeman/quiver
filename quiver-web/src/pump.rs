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
//! `setTimeout` callback never re-enters, the only invariant needed for correctness is "if a tick
//! isn't already scheduled, schedule one" — tracked here with a single pending-timer record. A
//! missed `wake` therefore surfaces as a stalled REPL, not as silent busy-looping.

use std::cell::{Cell, RefCell};
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;

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
    /// The work to run per tick. Borrowed mutably only while a tick executes; never re-entered.
    step: RefCell<Box<dyn FnMut() -> Tick>>,
    /// The tick callback handed to `setTimeout`. Stored to keep it alive and to reschedule itself.
    closure: RefCell<Option<Closure<dyn FnMut()>>>,
    /// Id of the currently pending `setTimeout`, if one is scheduled.
    timer: Cell<Option<f64>>,
    /// Absolute time (ms) the pending timer will fire. `None` when idle (no timer scheduled).
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
        let inner = Rc::new(PumpInner {
            set_timeout: get("setTimeout"),
            clear_timeout: get("clearTimeout"),
            global: global.into(),
            step: RefCell::new(Box::new(step)),
            closure: RefCell::new(None),
            timer: Cell::new(None),
            next_wake: Cell::new(None),
        });

        // The tick closure holds a strong ref back to `inner` so it can reschedule itself. This is
        // a deliberate reference cycle: a pump lives for the whole session, so it is never freed.
        let inner_for_closure = inner.clone();
        let closure = Closure::wrap(Box::new(move || {
            // The timer has fired; clear the pending record before running so the step's own
            // scheduling decision (and any wake it triggers) starts from a clean slate.
            inner_for_closure.timer.set(None);
            inner_for_closure.next_wake.set(None);

            let tick = (inner_for_closure.step.borrow_mut())();
            match tick {
                Tick::Busy => schedule(&inner_for_closure, now()),
                Tick::WakeAt(target) => schedule(&inner_for_closure, target),
                Tick::Idle => {}
            }
        }) as Box<dyn FnMut()>);
        *inner.closure.borrow_mut() = Some(closure);

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

/// Ensure a tick is scheduled to fire no later than `target` (absolute ms). If an earlier-or-equal
/// tick is already pending, does nothing; if a later one is pending, it is cancelled and replaced.
fn schedule(inner: &Rc<PumpInner>, target: f64) {
    let now = now();
    let target = target.max(now);

    match inner.next_wake.get() {
        // Already scheduled at or before the requested time — nothing to do.
        Some(pending) if pending <= target => return,
        // A later tick is pending; cancel it so we can schedule the sooner one.
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
    let closure = inner.closure.borrow();
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
