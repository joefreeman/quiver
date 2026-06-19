; Indent query for Quiver.
;
; Dialect: Helix / Aether (`@indent` / `@outdent` captures). This is NOT the
; nvim-treesitter indent dialect (`@indent.begin` / `@indent.end` / ...), which uses a
; different capture vocabulary; a Neovim consumer would need its own indents.scm.

[
  (block)
  (tuple)
  (tuple_type)
  (partial_type)
  (pattern_tuple)
  (pattern_partial)
] @indent

[
  "}"
  "]"
  ")"
] @outdent

; Handle ERROR nodes for when auto-pairs is disabled: typing an opening delimiter without
; its closing partner produces an ERROR node, but the body should still indent.
(ERROR "{") @indent
(ERROR "[") @indent
(ERROR "(") @indent
