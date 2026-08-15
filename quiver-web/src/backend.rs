//! The browser's effect backend: `fetch`, and the response body as a stream resource.
//!
//! It lives on the main thread, which is where `fetch` lives — the workers ask for effects and
//! the environment (here) performs them. Each request parks its Quiver process until a promise
//! resolves; the completion is queued and the pump woken, because the main loop sleeps as soon
//! as a tick reports no work and would otherwise never drain the queue.
//!
//! The body is a `\ByteStream` resource — the general chunks-until-a-clean-end stream — so
//! `![body]` works on it and `%http/client` sees one vocabulary on both hosts. Only one read
//! is armed at a time, which is what makes backpressure the browser's problem rather than
//! ours.

use quiver_core::effects::{EffectBackend, EffectError, EffectResult, ResultTupleInfo};
use quiver_core::error::Error;
use quiver_core::process::{ProcessId, StreamEvent};
use quiver_core::value::ResourceId;
use quiver_core::wire::WireValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;

use crate::effects::WebEffect;
use crate::repl_web::PumpHandle;

type Completions = Rc<RefCell<Vec<(ProcessId, EffectResult)>>>;
type StreamEvents = Rc<RefCell<Vec<(ResourceId, usize, StreamEvent, Vec<u8>)>>>;

/// An open response body: the reader that yields its chunks, and the controller that cancels
/// the request if the owning process goes away before the body is drained.
struct Body {
    reader: web_sys::ReadableStreamDefaultReader,
    controller: web_sys::AbortController,
}

pub struct WebEffectBackend {
    pump: PumpHandle,
    completions: Completions,
    events: StreamEvents,
    bodies: Rc<RefCell<HashMap<ResourceId, Body>>>,
    next_resource: Rc<RefCell<ResourceId>>,
    /// Type id of `\ByteStream`, and the tuple ids of `http_request`'s composite result. Both
    /// are pushed by the environment — a backend has no type registry of its own.
    body_type_id: Rc<RefCell<usize>>,
    fetch_result: Rc<RefCell<Option<ResultTupleInfo>>>,
    in_flight: Rc<RefCell<usize>>,
}

// The browser is single-threaded and this never leaves the main thread; the `Send` bound on
// `EffectBackend` exists for hosts that route effects across threads. Same reasoning as
// `WebWorkerHandle`.
unsafe impl Send for WebEffectBackend {}

impl WebEffectBackend {
    pub fn new(pump: PumpHandle) -> Self {
        Self {
            pump,
            completions: Rc::new(RefCell::new(Vec::new())),
            events: Rc::new(RefCell::new(Vec::new())),
            bodies: Rc::new(RefCell::new(HashMap::new())),
            next_resource: Rc::new(RefCell::new(1)),
            body_type_id: Rc::new(RefCell::new(0)),
            fetch_result: Rc::new(RefCell::new(None)),
            in_flight: Rc::new(RefCell::new(0)),
        }
    }
}

/// `Name: value` lines into a `Headers`. Anything without a colon is skipped rather than
/// rejected: the block came from `%http`, which already validated it.
fn headers_from_block(block: &[u8]) -> Result<web_sys::Headers, JsValue> {
    let headers = web_sys::Headers::new()?;
    for line in String::from_utf8_lossy(block).split("\r\n") {
        if let Some((name, value)) = line.split_once(':') {
            // A browser silently drops the headers it reserves (Host among them), so a
            // failure here is informational, not fatal.
            let _ = headers.append(name.trim(), value.trim());
        }
    }
    Ok(headers)
}

/// A `Headers` back into the CRLF block `%http` parses.
fn headers_to_block(headers: &web_sys::Headers) -> Vec<u8> {
    let mut out = String::new();
    let iterator = js_sys::try_iter(headers.as_ref()).ok().flatten();
    if let Some(entries) = iterator {
        for entry in entries.flatten() {
            let pair: js_sys::Array = entry.into();
            let name = pair.get(0).as_string().unwrap_or_default();
            let value = pair.get(1).as_string().unwrap_or_default();
            out.push_str(&format!("{name}: {value}\r\n"));
        }
    }
    out.into_bytes()
}

/// Anything the browser reports is undifferentiated — a `TypeError` covers DNS failure, a
/// refused connection and a CORS block alike — so it maps to the one honest kind.
fn fetch_error(error: &JsValue) -> EffectError {
    let described = error
        .as_string()
        .or_else(|| {
            js_sys::Reflect::get(error, &"message".into())
                .ok()?
                .as_string()
        })
        .unwrap_or_else(|| "fetch failed".to_string());
    EffectError::IO(described)
}

impl EffectBackend for WebEffectBackend {
    type E = WebEffect;

    fn execute(
        &mut self,
        process_id: ProcessId,
        effect: WebEffect,
    ) -> Result<Option<EffectResult>, Error> {
        let WebEffect::HttpRequest {
            method,
            url,
            headers,
            body,
        } = effect;

        let info = self.fetch_result.borrow().clone().ok_or_else(|| {
            Error::InvalidArgument("no result type ids registered for `http_request`".to_string())
        })?;
        let body_type_id = *self.body_type_id.borrow();
        let completions = self.completions.clone();
        let bodies = self.bodies.clone();
        let next_resource = self.next_resource.clone();
        let in_flight = self.in_flight.clone();
        let pump = self.pump.clone();

        *in_flight.borrow_mut() += 1;
        wasm_bindgen_futures::spawn_local(async move {
            let outcome = perform_fetch(&method, &url, &headers, &body).await;
            let completion = match outcome {
                Ok((status, header_block, reader, controller)) => {
                    let id = {
                        let mut next = next_resource.borrow_mut();
                        let id = *next;
                        *next += 1;
                        id
                    };
                    bodies.borrow_mut().insert(id, Body { reader, controller });
                    Ok(WireValue::tuple(
                        info.tuple_id,
                        vec![
                            WireValue::Int(status as i64),
                            WireValue::Binary(header_block.into()),
                            WireValue::Resource(id, body_type_id),
                        ],
                    ))
                }
                Err(error) => Err(fetch_error(&error)),
            };
            *in_flight.borrow_mut() -= 1;
            completions.borrow_mut().push((process_id, completion));
            crate::repl_web::wake(&pump);
        });

        Ok(None)
    }

    fn process_completions(&mut self) -> Vec<(ProcessId, EffectResult)> {
        std::mem::take(&mut *self.completions.borrow_mut())
    }

    fn has_operations_in_flight(&self) -> bool {
        *self.in_flight.borrow() > 0
    }

    fn close_resource(&mut self, resource_id: ResourceId) {
        // Aborting matters: a request abandoned by a timeout would otherwise keep running,
        // and the browser would keep streaming a body nobody will read.
        if let Some(body) = self.bodies.borrow_mut().remove(&resource_id) {
            body.controller.abort();
        }
    }

    fn set_type_ids(&mut self, resources: &[String], results: &[(String, ResultTupleInfo)]) {
        if let Some(index) = resources.iter().position(|name| name == "ByteStream") {
            *self.body_type_id.borrow_mut() = index;
        }
        if let Some((_, info)) = results.iter().find(|(name, _)| name == "http_request") {
            *self.fetch_result.borrow_mut() = Some(info.clone());
        }
    }

    fn arm_stream(&mut self, resource_id: ResourceId) -> Result<(), Error> {
        let reader = match self.bodies.borrow().get(&resource_id) {
            Some(body) => body.reader.clone(),
            None => {
                return Err(Error::InvalidArgument(format!(
                    "response body {resource_id} is closed"
                )));
            }
        };
        let events = self.events.clone();
        let body_type_id = *self.body_type_id.borrow();
        let in_flight = self.in_flight.clone();
        let pump = self.pump.clone();

        *in_flight.borrow_mut() += 1;
        wasm_bindgen_futures::spawn_local(async move {
            let event = match JsFuture::from(reader.read()).await {
                Ok(result) => {
                    let done = js_sys::Reflect::get(&result, &"done".into())
                        .ok()
                        .and_then(|v| v.as_bool())
                        .unwrap_or(true);
                    if done {
                        (StreamEvent::End, Vec::new())
                    } else {
                        let chunk = js_sys::Reflect::get(&result, &"value".into())
                            .ok()
                            .map(|v| js_sys::Uint8Array::new(&v).to_vec())
                            .unwrap_or_default();
                        (StreamEvent::Data, chunk)
                    }
                }
                // A failed read fails the stream: an aborted transfer is not a complete
                // body, and only the consumer knows whether the difference matters.
                Err(error) => (
                    StreamEvent::Failed {
                        error: fetch_error(&error),
                    },
                    Vec::new(),
                ),
            };
            *in_flight.borrow_mut() -= 1;
            events
                .borrow_mut()
                .push((resource_id, body_type_id, event.0, event.1));
            crate::repl_web::wake(&pump);
        });

        Ok(())
    }

    fn take_stream_events(&mut self) -> Vec<(ResourceId, usize, StreamEvent, Vec<u8>)> {
        std::mem::take(&mut *self.events.borrow_mut())
    }
}

/// The request itself: everything that can talk to JS, kept out of the trait impl.
async fn perform_fetch(
    method: &[u8],
    url: &[u8],
    headers: &[u8],
    body: &[u8],
) -> Result<
    (
        u16,
        Vec<u8>,
        web_sys::ReadableStreamDefaultReader,
        web_sys::AbortController,
    ),
    JsValue,
> {
    let method = String::from_utf8_lossy(method).into_owned();
    let url = String::from_utf8_lossy(url).into_owned();

    let controller = web_sys::AbortController::new()?;
    let init = web_sys::RequestInit::new();
    init.set_method(&method);
    init.set_headers(&headers_from_block(headers)?.into());
    init.set_signal(Some(&controller.signal()));
    // GET and HEAD may not carry one, and passing an empty body is a TypeError rather than a
    // no-op.
    if !body.is_empty() {
        init.set_body(&js_sys::Uint8Array::from(body).into());
    }

    let request = web_sys::Request::new_with_str_and_init(&url, &init)?;
    let window = web_sys::window().ok_or_else(|| JsValue::from_str("no window"))?;
    let response: web_sys::Response = JsFuture::from(window.fetch_with_request(&request))
        .await?
        .dyn_into()?;

    let status = response.status();
    let header_block = headers_to_block(&response.headers());
    let stream = response
        .body()
        .ok_or_else(|| JsValue::from_str("response has no body stream"))?;
    let reader: web_sys::ReadableStreamDefaultReader = stream.get_reader().dyn_into()?;

    Ok((status, header_block, reader, controller))
}
