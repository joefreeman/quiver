; Highlight queries for Quiver.
; Capture names follow the tree-sitter highlight conventions used by Neovim, Helix and Zed.

; ------------------------------------------------------------------- comments

(comment) @comment

; ------------------------------------------------------------------- literals

(integer) @number
(index) @number
(binary) @number
(string) @string
(multiline_string) @string
(escape_sequence) @string.escape

; ---------------------------------------------------------------------- types

(type_name) @type
(resource_type) @type
(tuple_name) @constructor
(cycle_type) @type.builtin
; The bare `'` of a module's nameless default-type marker (`' = ...`).
(default_type_name) @type
; A bare `'` referring to the module's own default type, in a type position.
(self_default_type) @type
; The `.name` of a module type reference (`'%mod.name`); the `%mod` part is a module.
(module_type member: (identifier) @type)

; ------------------------------------------------------------------ functions

(builtin) @function.builtin

; Application is argument-first (`[args] f`), so a call head is just an ordinary access
; term and can't be distinguished syntactically from a value reference; the tail-call
; target, however, is unambiguously a call.
(tail_call function: (identifier) @function.call)

; -------------------------------------------------------------------- imports

; Colour the whole `%num` / `%mathx/vec` as a single module reference (the `%` sigil
; included), mirroring how a type name carries its `'` in one `@type` token. The raised
; priority keeps the span intact against the narrower captures nested inside it — the
; `(identifier) @variable` fallback on each path segment and the `/` operator rule —
; which would otherwise reclaim those bytes under shortest-span overlap resolution.
((import) @module (#set! "priority" 110))

; ------------------------------------------------------------------- dialects

; The raw text of a dialect invocation (`%json{ … }`). The module path is an ordinary
; (import) node, captured above; the content is opaque embedded text, coloured as a
; special string. As for imports, the raised priority keeps the span intact against
; narrower captures nested inside it.
((dialect_content) @string.special (#set! "priority" 110))

; Balanced brace groups inside the content are parsed only to find the dialect's
; matching close brace — their `{`/`}` tokens are content, not punctuation, so
; recapture them against the bracket rule below. (The dialect's own outer braces
; keep their @punctuation.bracket.)
(dialect_content
  ["{" "}"] @string.special
  (#set! "priority" 110))

; ------------------------------------------------------------------- bindings

(chain binding: (identifier) @variable)
(named_field name: (identifier) @property)
(access field: (identifier) @property)
(tail_call field: (identifier) @property)

; ---------------------------------------------------------------- annotations

(annotation_name) @attribute    ; :doc, :error — declaration, attach, and retrieval

; The checked retrieval form `x:('t)key`: the key is the attribute; the `:(` opener
; pairs with the closing `)`, which the general bracket rule below already captures.
(checked_annotation ":(" @punctuation.bracket)
(checked_annotation key: (identifier) @attribute)

; ----------------------------------------------------------------- parameters

(parameter) @variable.builtin   ; $
(ripple) @variable.builtin      ; ~
(self) @variable.builtin        ; .

; ----------------------------------------------------------------- operators

[
  "~>"
  "=>"
  "->"
  "="
  "/"
  "&"
  "^"
  "#"
  "..."
] @operator

(equality) @operator
(not) @operator

; The process operators (`@` spawn / process, `!` select) get a distinct, attention-
; drawing highlight so concurrency stands out from ordinary flow operators.
[
  "@"
  "!"
] @punctuation.special

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
  "<"
  ">"
] @punctuation.bracket

[
  ","
  ":"
  "|"
  "."
] @punctuation.delimiter

; A string interpolation hole `{ … }` — a block body embedded in a string or multi-line
; string. Its inner nodes carry their own captures, overriding the enclosing @string;
; the delimiting braces are marked special so they read as code, not text. (Placed after
; the general bracket rule so later-wins engines prefer this capture.)
(interpolation ["{" "}"] @punctuation.special)

(placeholder) @comment.unused

; A bare identifier defaults to a variable reference.
(identifier) @variable
