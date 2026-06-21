; Highlight queries for Quiver.
; Capture names follow the tree-sitter highlight conventions used by Neovim, Helix and Zed.

; ------------------------------------------------------------------- comments

(comment) @comment

; ------------------------------------------------------------------- literals

(integer) @number
(index) @number
(binary) @number
(string) @string
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

; The leading identifier of an application's callable head is highlighted as a call.
(application
  function: (access source: (identifier) @function.call))
(tail_call function: (identifier) @function.call)

; -------------------------------------------------------------------- imports

; Colour the whole `%math` / `%math/trig` as a single module reference (the `%` sigil
; included), mirroring how a type name carries its `'` in one `@type` token.
(import) @module

; ------------------------------------------------------------------- bindings

(chain binding: (identifier) @variable)
(named_field name: (identifier) @property)
(access field: (identifier) @property)
(tail_call field: (identifier) @property)

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
  ";"
] @punctuation.delimiter

(placeholder) @comment.unused

; A bare identifier defaults to a variable reference.
(identifier) @variable
