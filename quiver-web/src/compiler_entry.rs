//! The compiler worker: a [`LineCompiler`] in its own web worker, so compilation —
//! the dominant cost of a REPL line — never blocks the main thread or the executors.
//!
//! It speaks a JSON request/response protocol over `postMessage` and never touches an
//! environment: an `evaluate` answers the session bookkeeping (a keep-set to compact,
//! locals to keep) plus a [`WirePayload`](quiver_environment::WirePayload) of
//! relocatable code, which the caller hands to whatever host runs it — the in-browser
//! `Environment`'s `resumeProcess`, or a remote one speaking the same payload. That
//! seam is the point: the compiler's output is host-agnostic.
//!
//! The worker posts `{"type":"ready"}` on boot; the caller then sends `init` and one
//! request at a time (each carries an `id`, echoed on its response). Requests are
//! handled synchronously in arrival order — a slow compile delays this worker only.
//!
//! `@N` process references are deliberately unsupported here: their types live in an
//! environment's id space and do not cross this boundary self-contained. The remote
//! CLI driver made the same choice; a typed registry is the designed replacement.

use crate::effects::WebEffect;
use quiver_environment::{CommittedLine, LineCompiler};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::{DedicatedWorkerGlobalScope, MessageEvent};

#[derive(serde::Deserialize)]
#[serde(
    tag = "type",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
enum CompilerRequest {
    /// Construct the session: a virtual filesystem imports resolve against (a
    /// `quiver.toml` entry defines the routing table), and the compile mode — debug
    /// by default, like the native REPL: failure provenance and checked assertions
    /// are what an interactive session is for.
    Init {
        id: u64,
        #[serde(default)]
        files: HashMap<String, String>,
        #[serde(default = "default_debug")]
        debug: bool,
    },
    /// Compile one line and commit it to the session.
    Evaluate { id: u64, source: String },
    /// Replace the virtual filesystem; cached modules recompile on next use, session
    /// bindings survive.
    SetFiles {
        id: u64,
        files: HashMap<String, String>,
    },
    /// The session's variables, as `(name, formatted type)` in definition order.
    Variables { id: u64 },
    /// Start the session over — fresh bindings, current files — keeping the warm
    /// artifact store, so unchanged modules re-link instead of recompiling. The
    /// recovery verb after a runtime error or interrupt kills the session process:
    /// the caller allocates a new process and continues.
    Reset { id: u64 },
}

fn default_debug() -> bool {
    true
}

#[derive(serde::Serialize)]
#[serde(
    tag = "type",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
enum CompilerResponse {
    /// Posted once at boot, before any request.
    Ready,
    /// `init` / `setFiles` acknowledged.
    Ok { id: u64 },
    /// The request failed; `id` is absent only for an unparseable request.
    Error { id: Option<u64>, message: String },
    Evaluated {
        id: u64,
        /// Locals to keep when compacting the session process — apply before resuming.
        compact_keep: Vec<usize>,
        /// Locals to keep when the result is delivered (the line's bindings).
        keep_indices: Vec<usize>,
        /// What to run, as [`WirePayload`](quiver_environment::WirePayload) JSON to
        /// hand a host verbatim (the in-browser `resumeProcess` takes exactly this
        /// string) — or `None` for a line with nothing to execute (type definitions
        /// alone), whose session state is committed all the same. Pre-serialized so
        /// the caller never re-encodes a payload it only forwards.
        payload: Option<String>,
        /// The line's inferred result type, formatted for display.
        result_type: String,
    },
    Variables {
        id: u64,
        variables: Vec<crate::types::Variable>,
    },
}

struct CompilerState {
    compiler: LineCompiler<WebEffect>,
    /// Held across `setFiles` reloads and `reset`s, so unchanged modules re-link from
    /// their artifacts instead of recompiling.
    store: Rc<quiver_compiler::ArtifactStore>,
    /// The current virtual filesystem, kept so a `reset` rebuilds against it.
    files: HashMap<String, String>,
    debug: bool,
}

impl CompilerState {
    fn new(files: HashMap<String, String>, debug: bool) -> Result<Self, String> {
        let mut state = CompilerState {
            compiler: LineCompiler::new(
                crate::repl_web::create_session_resolver(files.clone())?,
                crate::builtins::web_builtins(),
            ),
            store: Rc::new(quiver_compiler::ArtifactStore::in_memory()),
            files,
            debug,
        };
        state.configure();
        Ok(state)
    }

    /// A fresh session over the same files, store and mode: new `LineCompiler`, no
    /// bindings, warm artifacts.
    fn reset(&mut self) -> Result<(), String> {
        self.compiler = LineCompiler::new(
            crate::repl_web::create_session_resolver(self.files.clone())?,
            crate::builtins::web_builtins(),
        );
        self.configure();
        Ok(())
    }

    fn configure(&mut self) {
        self.compiler
            .set_compile_options(quiver_compiler::compiler::CompileOptions {
                debug: self.debug,
                source_name: "repl".to_string(),
                ..Default::default()
            });
        self.compiler.set_artifact_store(Rc::clone(&self.store));
    }

    fn evaluate(&mut self, id: u64, source: &str) -> CompilerResponse {
        let error = |message: String| CompilerResponse::Error {
            id: Some(id),
            message,
        };
        let prepared = match self.compiler.prepare(source) {
            Ok(prepared) => prepared,
            Err(e) => return error(e.to_string()),
        };
        let compact_keep = prepared.compact_keep().to_vec();
        let compiled = match self.compiler.compile(prepared) {
            Ok(compiled) => compiled,
            Err(e) => return error(e.to_string()),
        };
        let (payload, keep_indices) = match self.compiler.commit_line(compiled) {
            Some(CommittedLine {
                payload,
                keep_indices,
            }) => (
                Some(serde_json::to_string(&payload.to_wire()).expect("serialize payload")),
                keep_indices,
            ),
            None => (None, Vec::new()),
        };
        let result_type = self
            .compiler
            .format_type(self.compiler.get_last_result_type());
        CompilerResponse::Evaluated {
            id,
            compact_keep,
            keep_indices,
            payload,
            result_type,
        }
    }
}

fn handle(state: &mut Option<CompilerState>, request: CompilerRequest) -> CompilerResponse {
    let uninitialized = |id: u64| CompilerResponse::Error {
        id: Some(id),
        message: "compiler worker not initialized — send `init` first".to_string(),
    };
    match request {
        CompilerRequest::Init { id, files, debug } => match CompilerState::new(files, debug) {
            Ok(new_state) => {
                *state = Some(new_state);
                CompilerResponse::Ok { id }
            }
            Err(message) => CompilerResponse::Error {
                id: Some(id),
                message,
            },
        },
        CompilerRequest::Evaluate { id, source } => match state {
            Some(state) => state.evaluate(id, &source),
            None => uninitialized(id),
        },
        CompilerRequest::SetFiles { id, files } => match state {
            Some(state) => match crate::repl_web::create_session_resolver(files.clone()) {
                Ok(resolver) => {
                    state.compiler.reload_modules(resolver);
                    // The reload rebuilt the module cache; re-apply the options and the
                    // session's store, so unchanged modules re-link rather than recompile.
                    state.configure();
                    state.files = files;
                    CompilerResponse::Ok { id }
                }
                Err(message) => CompilerResponse::Error {
                    id: Some(id),
                    message,
                },
            },
            None => uninitialized(id),
        },
        CompilerRequest::Reset { id } => match state {
            Some(state) => match state.reset() {
                Ok(()) => CompilerResponse::Ok { id },
                Err(message) => CompilerResponse::Error {
                    id: Some(id),
                    message,
                },
            },
            None => uninitialized(id),
        },
        CompilerRequest::Variables { id } => match state {
            Some(state) => CompilerResponse::Variables {
                id,
                variables: state
                    .compiler
                    .get_variables()
                    .into_iter()
                    .map(|(name, var_type)| crate::types::Variable { name, var_type })
                    .collect(),
            },
            None => uninitialized(id),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn request(state: &mut Option<CompilerState>, json: &str) -> serde_json::Value {
        let request = serde_json::from_str::<CompilerRequest>(json).expect("request parses");
        let response = serde_json::to_string(&handle(state, request)).expect("response serializes");
        serde_json::from_str(&response).expect("response is JSON")
    }

    #[test]
    fn the_protocol_round_trips_a_session() {
        let mut state = None;

        // Uninitialized requests are refused with the request's id.
        let response = request(&mut state, r#"{"type":"evaluate","id":9,"source":"1"}"#);
        assert_eq!(response["type"], "error");
        assert_eq!(response["id"], 9);

        let response = request(&mut state, r#"{"type":"init","id":1}"#);
        assert_eq!(response["type"], "ok", "{response}");

        // A line importing a module: the payload is WirePayload JSON carrying the
        // module units a host links, exactly what `resumeProcess` accepts.
        let response = request(
            &mut state,
            r#"{"type":"evaluate","id":2,"source":"%num.mul [7, 6]"}"#,
        );
        assert_eq!(response["type"], "evaluated", "{response}");
        assert!(response["resultType"].as_str().unwrap().contains("int"));
        let payload: quiver_environment::WirePayload =
            serde_json::from_str(response["payload"].as_str().expect("a payload string"))
                .expect("payload parses as WirePayload");
        assert!(payload.unit.entry.is_some());
        assert!(
            !payload.modules.is_empty(),
            "the line must carry the modules it imports"
        );

        // Session state commits across lines: a binding shows up in `variables`.
        let response = request(&mut state, r#"{"type":"evaluate","id":3,"source":"x = 1"}"#);
        assert_eq!(response["type"], "evaluated", "{response}");
        let response = request(&mut state, r#"{"type":"variables","id":4}"#);
        assert_eq!(response["type"], "variables");
        assert!(
            response["variables"]
                .as_array()
                .unwrap()
                .iter()
                .any(|variable| variable["name"] == "x"),
            "{response}"
        );

        // A compile error answers an error, and the session survives it.
        let response = request(
            &mut state,
            r#"{"type":"evaluate","id":5,"source":"nonsense_name"}"#,
        );
        assert_eq!(response["type"], "error");
        let response = request(&mut state, r#"{"type":"evaluate","id":6,"source":"x"}"#);
        assert_eq!(response["type"], "evaluated", "{response}");

        // Reset drops the bindings but keeps the session compiling.
        let response = request(&mut state, r#"{"type":"reset","id":7}"#);
        assert_eq!(response["type"], "ok", "{response}");
        let response = request(&mut state, r#"{"type":"variables","id":8}"#);
        assert_eq!(response["variables"].as_array().unwrap().len(), 0);
        let response = request(
            &mut state,
            r#"{"type":"evaluate","id":9,"source":"%num.mul [6, 6]"}"#,
        );
        assert_eq!(response["type"], "evaluated", "{response}");
    }
}

/// Main entry point for the compiler worker — call this from the worker's JS context,
/// where an environment (executor) worker calls `environment_worker_main`.
#[wasm_bindgen]
pub fn compiler_worker_main() {
    console_error_panic_hook::set_once();
    let global = js_sys::global()
        .dyn_into::<DedicatedWorkerGlobalScope>()
        .expect("Not in a worker context");

    let post = |global: &DedicatedWorkerGlobalScope, response: &CompilerResponse| {
        let json = serde_json::to_string(response).expect("serialize response");
        let _ = global.post_message(&JsValue::from_str(&json));
    };

    let state: Rc<RefCell<Option<CompilerState>>> = Rc::new(RefCell::new(None));
    let global_for_closure = global.clone();
    let onmessage = Closure::wrap(Box::new(move |event: MessageEvent| {
        if let Some(text) = event.data().as_string() {
            let response = match serde_json::from_str::<CompilerRequest>(&text) {
                Ok(request) => handle(&mut state.borrow_mut(), request),
                Err(e) => CompilerResponse::Error {
                    id: None,
                    message: format!("invalid request: {e}"),
                },
            };
            post(&global_for_closure, &response);
        }
    }) as Box<dyn FnMut(MessageEvent)>);
    global.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
    onmessage.forget(); // Keep closure alive

    post(&global, &CompilerResponse::Ready);
}
