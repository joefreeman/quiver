/**
 * @file Quiver grammar for tree-sitter
 * @author Quiver
 * @license MIT
 *
 * Quiver is a statically-typed functional language. Data flows left→right through
 * transformation pipelines. The grammar mirrors quiver-compiler/src/parser.rs.
 *
 * Surface syntax:
 * - **A chain is terms joined by a mandatory `~>`.** Whitespace does *not* join chain
 *   terms. The value flows left→right through them; nil passes through (no short-circuit).
 *   `~>` doubles as a **line continuation**: a chain ends at a bare newline, but a newline
 *   followed by `~>` continues it.
 * - **Application is by juxtaposition.** An *applicable* head followed by horizontal space
 *   and a single argument primary is one application term: `f x`, `add [3, 4]`, `^f [~, 1]`,
 *   `~ [args]`, `@f x`. Applicable heads are a *sourced* access (a variable, `$`, `~`,
 *   import member, builtin, or tail call) or a spawn (`@f`); literals, tuples, blocks,
 *   function literals, selects and bare `.field` accessors are not. The
 *   argument is a single primary (`f x y` is an error).
 * - **A sequence is chains separated by semicolon or newline** (the two are synonyms). This
 *   is where the value short-circuits on nil and where binding scope advances.
 * - **The program is one sequence** of chains with type-alias declarations interspersed,
 *   all separated by the sequence separator. Commas are *not* step separators — they
 *   appear only inside brackets (tuple fields, type arguments, select sources).
 *
 * Whitespace handling: spaces, tabs and comments are `extras` (ignored everywhere), but
 * newlines are significant. A newline (or semicolon) separates the chains of a sequence;
 * continuation points (after `~>`, `,`, `|`, `=>`, `=`, and inside brackets) explicitly
 * permit newlines via the `_nl` helper. Application's horizontal gap is just the skipped
 * `extras`, so it can't cross the newline that ends a chain.
 */

/* eslint-disable arrow-parens */
/* eslint-disable camelcase */

/// Comma-separated list (one or more) allowing newlines around commas. Used inside
/// brackets/parens/angles where there is no sequence separator to conflict with.
function commaSep1($, rule) {
  return seq(
    rule,
    repeat(seq(optional($._nl), ',', optional($._nl), rule)),
    optional(seq(optional($._nl), ',')),
  );
}

/// A bracketed, comma-separated list with newlines permitted after the opening and
/// before the closing delimiter.
function bracketed($, open, rule, close) {
  return seq(open, optional($._nl), optional(commaSep1($, rule)), optional($._nl), close);
}

/// As `bracketed`, but the opening delimiter must be immediately adjacent to the
/// preceding token (no whitespace). Used for named forms like `Name[...]`, `Name(...)`
/// and `'alias<...>`, distinguishing them from a bare name followed by a separate group.
function immBracketed($, open, rule, close) {
  return seq(token.immediate(open), optional($._nl), optional(commaSep1($, rule)), optional($._nl), close);
}

/// The body of a checked annotation retrieval after its `:(` opener: the expected shape —
/// a type (`('t)`) or a partial-field gate (`(line: 'int)`) — then the key glued to the `)`.
function checkedAnnotationBody($) {
  return seq(
    optional($._nl),
    optional(choice($._type, commaSep1($, $._partial_type_field))),
    optional($._nl),
    ')',
    field('key', alias($._identifier_immediate, $.identifier)),
  );
}

module.exports = grammar({
  name: 'quiver',

  // No `word` rule: the grammar has no alphabetic keywords to extract, and a word rule
  // breaks `_identifier_immediate` — keyword extraction re-lexes any identifier-shaped
  // token via the word token, which skips whitespace first and so discards immediacy.

  extras: $ => [
    /[ \t\r\f]+/,
    $.comment,
  ],

  conflicts: $ => [
    // A leading token in a chain may begin either a binding pattern (`x = ...`,
    // `[a, b] = ...`) or a term; resolved with the `=` lookahead via GLR.
    [$._binding_target, $._access_source],
    [$.tuple, $.pattern_tuple],
    [$._pattern, $._access_source],
    [$._pattern, $._primary],
    // `~.f` / `a.b` / `$x.y` may be an ordinary access or the source path of a
    // spread-update (`~.f[..., y]`); the glued `[` decides via GLR.
    [$._access_source, $.spread_update],
    // A type atom may stand alone or be the first variant of a multi-line union; the
    // newline-then-`|` lookahead chooses (and likewise whether a trailing newline
    // extends the union or terminates the statement).
    [$._type, $.union_type],
    [$.union_type],
    // A function/function-type may have optional trailing parts (return type, body); a
    // newline after a complete one ends the chain rather than extending it.
    [$.function],
    // After a chain, a separator (semicolon/newline) may continue the current sequence
    // (another chain) or end it so the surrounding construct can take a trailing separator.
    [$.sequence],
    // After a term, a newline may continue the chain (next line starts with `~>`) or end
    // the chain.
    [$.chain],
    // After a branch condition, a newline may precede `=>` (its consequence) or the next
    // branch / block close.
    [$.branch],
    // `source.field` — greedily attach trailing `.field` accessors to the access rather
    // than treating `.` as a self-send term. (Both split access forms carry the `repeat`.)
    [$._sourced_access],
    [$._leading_access],
    // A parenthesised pattern beginning with an identifier may be a partial pattern
    // (`(x: …)`, `(x, …)`) or the first alternative of an or-pattern (`(x | …)`); the `:`/`,`
    // versus `|` that follows decides, via GLR.
    [$._pattern, $._partial_field],
    // In a tuple type, `(name` may open an omittable label (`(foo): 'int`, or the bare
    // decorator `(foo)`) or a partial-type field type (`(foo: 'int)`); the `)` versus `:`
    // decides, via GLR.
    [$._partial_field, $._field_type_base],
    // A bare identifier inside `[ … ]` may be a tuple *pattern*'s field (`=[x, y]`) or a
    // tuple *type*'s decorator entry (`#[...'base, x]`); the two bracket forms already
    // overlap (see `pattern_tuple`/`tuple_type` below) and the field itself does not
    // decide, so both readings are carried, via GLR.
    [$._pattern, $._field_type_base],
    // A parenthesised group of `|`-separated atoms can be read as an or-pattern (of tuple/type
    // patterns) or as a parenthesised type union; both are accepted for editor purposes.
    [$.pattern_tuple, $.tuple_type],
    [$._pattern, $._type_atom],
    [$.pattern_partial, $.partial_type],
    // `(a, b)` may be a punned tuple (value position) or a partial pattern (before a `=`,
    // or after one as `=(a, b)`); the surrounding position decides, via GLR.
    [$.pun, $._partial_field],
    [$.pun, $._pattern, $._partial_field],
    // A `:key` token may open a block-prefix annotation (`:key value`) or be an
    // annotation-retrieval accessor on the flowing value; what follows decides, via GLR.
    [$.annotation, $._leading_accessor],
    // After a block's annotation prefix, a separator may lead into the branches or be the
    // trailing separator of an annotation-only block.
    [$._block_branches],
    // A newline after a block's last annotation may be the separator before the branches
    // or the block's closing newline.
    [$._sep, $._nl],
  ],

  rules: {
    // The program is a single `sequence` — the very node a block's branch body is, since a
    // program is one sequence of steps and nothing more. Optional, so a file of only
    // separators (or an empty one) still parses.
    source_file: $ => seq(
      optional($._sep),
      optional(seq(
        $.sequence,
        optional($._sep),
      )),
    ),

    // One step of a sequence: a type-alias declaration, a chain, or a leading `//=>`
    // assertion — the trailing form with a line break, observing the previous step's value
    // (or, at the start of a sequence, the block's input). An alias is scoped to the
    // sequence's enclosing scope, so one written in a block or function body is local to it.
    _step: $ => choice($.type_alias, $.chain, $.assertion),

    // The sequence separator: one or more semicolons/newlines (they are synonyms),
    // collapsing runs. This is where the flow short-circuits on nil and binding scope
    // advances. (Commas are *not* step separators — they appear only inside brackets.)
    _sep: _ => prec.right(repeat1(choice('\n', ';'))),

    // One or more newlines: a continuation point inside an unfinished construct.
    _nl: _ => prec.right(repeat1('\n')),

    // An ordinary `//` comment — any run except the `//=>` that opens a step assertion.
    // (No lookahead in tree-sitter regexes, so the exclusion is spelled out: empty, a
    // first character other than `=`, or `=` followed by anything but `>`.)
    comment: _ => token(seq('//', optional(choice(
      /[^=\n][^\n]*/,
      seq('=', optional(/[^>\n][^\n]*/)),
    )))),

    // A step assertion: `//=> P` matches the step's value against the pattern `P` in
    // debug builds. It reads as a comment but the pattern is real syntax; a run of three
    // or more spaces after the pattern starts a prose note running to the end of the line.
    // Like a comment, an assertion terminates its line — the real parser rejects code
    // after it; this grammar stays permissive there, as it does for binding patterns.
    assertion: $ => seq('//=>', field('pattern', $._pattern), optional($.assertion_note)),

    // Lexical precedence over the whitespace `extras`, which would otherwise skip the
    // note's leading spaces and lex its prose as code.
    assertion_note: _ => token(prec(1, / {3}[^\n]*/)),

    // ----------------------------------------------------------------- type aliases

    // A named alias (`'point = ...`) or the module's nameless default-type marker
    // (`' = ...` / `'<'t> = ...`), where the name is a bare `'`.
    type_alias: $ => seq(
      field('name', choice($.type_name, $.default_type_name)),
      optional($.type_parameters),
      optional($._nl), '=', optional($._nl),
      field('definition', $._type),
    ),

    default_type_name: _ => "'",

    // ------------------------------------------------------------------ annotations

    // The glued `:name` sigil form — used to attach (`:doc "..."` in a block prefix) and
    // retrieve (`x:doc`). A single token, so `a:foo` (glued, retrieval) never splits into
    // the `a: foo` (spaced) field-label form.
    annotation_name: _ => token(seq(':', /[a-z][a-z0-9_]*[?!]?/)),

    // One block-prefix annotation: `:key value`, the value an ordinary chain terminated by
    // semicolon/newline. Dynamic precedence prefers this reading over a source-less
    // annotation-retrieval access at the start of a block.
    annotation: $ => prec.dynamic(1, seq(field('key', $.annotation_name), $.chain)),

    type_parameters: $ => immBracketed($, '<', $.type_name, '>'),

    // ------------------------------------------------------------------- sequences

    // A sequence of steps separated by the sequence separator (semicolon or newline). Used
    // as block/branch bodies; the top level (`source_file`) is one sequence too.
    sequence: $ => seq(
      $._step,
      repeat(seq($._sep, $._step)),
    ),

    // A chain is a sequence of `term`s joined by a **mandatory** `~>`; the value flows
    // left→right through them, with nil passing through (no short-circuit — that is the
    // sequence separator's job). Whitespace does NOT join chain terms (it binds a juxtaposed
    // application argument to its head instead — see `application`). The `~>` doubles as an
    // explicit line continuation: a chain ends at a bare newline, but a newline followed by
    // `~>` continues it.
    chain: $ => seq(
      // `pattern = chain` binding. The real parser distinguishes a binding (`x = e`, `=`
      // space-surrounded) from an in-chain match (`e =x`, `=` glued) by spacing; tree-sitter
      // treats whitespace as `extras`, so when the leading term is a binding target followed
      // by `=`, prefer the binding reading via dynamic precedence.
      optional(prec.dynamic(1, seq(field('binding', $._binding_target), '=', optional($._nl)))),
      $._term,
      // `//=> P` assertions may end any of the chain's lines, so they precede a continuation
      // as well as ending the chain. (Comments are `extras`, so they need no mention.)
      repeat(seq(repeat(seq(optional($._nl), $.assertion)), $._pipe, $._term)),
      // A trailing assertion on the same line (`5 ~> double //=> 10`). The leading form on its
      // own line is a `_step`, so it is not repeated here.
      repeat($.assertion),
    ),

    // The chain separator / line continuation: a mandatory `~>`, with optional newlines
    // around it. (The lexer prefers the longer `~>` over a bare `~` ripple, so `f ~> g`
    // separates rather than applying `f` to a ripple argument.)
    _pipe: $ => seq(optional($._nl), '~>', optional($._nl)),

    // A term is a primary, optionally applied to a single argument by juxtaposition.
    _term: $ => choice($.application, $._primary),

    // Juxtaposed application: an *applicable* head and a single primary argument. In the
    // real parser the two are separated by horizontal whitespace (`f x`, `add [3, 4]`,
    // `^f [~, 1]`, `~ [args]`, `@f x`); here that gap is just skipped `extras`, and because
    // a newline is significant (not `extras`) the argument can't cross the newline that ends
    // a chain. Exactly one argument: `f x y` is a syntax error. Mirrors `term` in
    // quiver-compiler/src/parser.rs.
    application: $ => seq(
      field('function', $._applicable),
      field('argument', $._primary),
    ),

    // The heads that may take a juxtaposed argument: a *sourced* access (a variable, `$`,
    // `~`, an import member, a tail call), a builtin, or a spawn (`@f`). A bare `.field`
    // accessor (a source-less `access`) is deliberately excluded — `.f x` is an error — so
    // the applicable access form is the sourced one only.
    _applicable: $ => choice(
      alias($._sourced_access, $.access),
      $.builtin,
      $.tail_call,
      $.spawn,
    ),

    // The forms valid as the target of a chain binding (`x = ...`, `[a, b] = ...`,
    // `(add, mul) = ...`, `('bin)ip = ...`, `* = ...`). A bare type or literal is never a
    // binding target, which keeps bindings distinct from type aliases. (Ascribed targets
    // once mis-resolved `$N` accesses via GLR — that was the missing dotless-`$`-sugar
    // rule, fixed alongside it.)
    _binding_target: $ => choice(
      $.identifier,
      $.pattern_tuple,
      $.pattern_partial,
      $.pattern_ascription,
      $.star,
      $.placeholder,
    ),

    _primary: $ => choice(
      $.multiline_string,
      $.string,
      $.integer,
      $.binary,
      $.builtin,
      $.process_ref,
      $.spawn,
      $.self,
      $.bind_match,
      $.select,
      $.state,
      $.tail_call,
      $.tuple,
      $.function,
      $.block,
      $.spread_update,
      $.dialect,
      $.access,
    ),

    // Name-preserving spread-update: `a[..., y]` / `~[..., y]` / `$conn[..., y]`. The source
    // is an access path — a variable path, a parameter or its fields (sigil runs included),
    // or a ripple field. The bracket is adjacent (no space) and begins with a spread,
    // distinguishing it from `a [..., y]` (two terms) and from a plain spread tuple
    // `[...a, y]`. The result inherits the source tuple's name.
    spread_update: $ => seq(
      field('source', choice($.ripple, $.identifier, $.parameter)),
      repeat(seq('.', field('field', choice($.identifier, $.index)))),
      token.immediate('['), optional($._nl),
      $.spread,
      repeat(seq(optional($._nl), ',', optional($._nl), $._field)),
      optional(seq(optional($._nl), ',')),
      optional($._nl), ']',
    ),

    // ----------------------------------------------------------------------- access

    // A variable/parameter/ripple/import optionally followed by `.field`/`.0` accessors,
    // or a leading accessor with no source (`.name` as a chain step). Split into a *sourced*
    // and a *leading* form: only the sourced form is an applicable application head (see
    // `_applicable`), matching the real parser's `access.source.is_some()` test.
    access: $ => choice(
      $._sourced_access,
      $._leading_access,
    ),

    _sourced_access: $ => seq(field('source', $._access_source), repeat($._accessor), optional($.type_arguments)),
    _leading_access: $ => seq($._leading_accessor, repeat($._accessor), optional($.type_arguments)),

    // A source-less access reads off the flowing value (`.name`, `:key` as a chain step).
    // The leading annotation sigil is an ordinary token; *trailing* annotation accessors
    // must be glued (token.immediate), matching the real parser: `x:key` retrieves, while
    // `x :key` is two terms.
    _leading_accessor: $ => choice(
      seq('.', field('field', choice($.identifier, $.index))),
      field('annotation', $.annotation_name),
      field('annotation', $.checked_annotation),
    ),

    _access_source: $ => choice(
      $.identifier,
      $.parameter,
      $.ripple,
      $.import,
    ),

    // A glued sigil run — `$` is the function's own parameter, each extra `$` one function
    // further out (`$$`, `$$$`) — with the dotless single-accessor sugar: `$x` ≡ `$.x`,
    // `$0` ≡ `$.0`, `$$x` likewise (further accessors are dotted: `$0.pos`). The sugar is
    // glued — a spaced `$ x` is an application of the parameter to an argument, never the
    // sugar, and a spaced `$ $` is an application, never a run.
    parameter: $ => seq(token(/\$+/), optional(field('field', choice(
      alias($._identifier_immediate, $.identifier),
      alias($._index_immediate, $.index),
    )))),
    _index_immediate: _ => token.immediate(/\d+/),
    ripple: _ => '~',

    _accessor: $ => choice(
      seq('.', field('field', choice($.identifier, $.index))),
      field('annotation', alias($._annotation_name_immediate, $.annotation_name)),
      field('annotation', alias($._checked_annotation_immediate, $.checked_annotation)),
    ),
    _annotation_name_immediate: _ => token.immediate(seq(':', /[a-z][a-z0-9_]*[?!]?/)),

    // The checked retrieval form `x:('t)key` — a parenthesised expected shape between the
    // `:` and the key, the `=('t)v` ascription syntax transplanted to retrieval
    // (`annotation_accessor` in quiver-compiler/src/parser.rs). Everything is glued: `:(`
    // is a single token (a field label's `:` is followed by a space), and the key sits
    // immediately after the `)`.
    checked_annotation: $ => seq(':(', checkedAnnotationBody($)),
    _checked_annotation_immediate: $ => seq(token.immediate(':('), checkedAnnotationBody($)),
    index: _ => /\d+/,

    // `%num`, `%mathx/vec`. The `/` path separator is immediate so a later `mod / x`
    // (with surrounding spaces) is not mistaken for part of the module path.
    import: $ => seq('%', $.identifier, repeat(seq(token.immediate('/'), $.identifier))),

    // -------------------------------------------------------------------- dialects

    // A dialect invocation `%mod{ raw }`: an import path glued (no whitespace) to a
    // braced raw-text region. The `{` is immediate, so the spaced `%mod { ... }` is
    // *not* a dialect (it stays an import followed by a block). The content is not
    // parsed as Quiver — the grammar only finds the matching close brace, mirroring
    // `dialect_term` in quiver-compiler/src/parser.rs: braces must balance, except
    // inside `"…"` string literals (where a `\` escapes the next byte, so `\"`
    // doesn't close the string) or when escaped as `\{`/`\}`; outside strings `\"`
    // is an escaped literal quote (so an unpaired `"` is writable).
    dialect: $ => seq(
      field('module', $.import),
      token.immediate('{'),
      optional(field('content', $.dialect_content)),
      '}',
    ),

    // The raw content: plain text, escaped braces, strings (in which braces don't
    // count) and balanced nested brace groups. The text token's lexical precedence
    // keeps it ahead of the whitespace/comment `extras`, which would otherwise eat
    // content (a comment token could even swallow a closing `}`).
    dialect_content: $ => repeat1($._dialect_chunk),

    _dialect_chunk: $ => choice(
      $._dialect_text,
      $._dialect_escape,
      $._dialect_backslash,
      $._dialect_string,
      $._dialect_braces,
    ),

    _dialect_text: _ => token(prec(1, /[^{}"\\]+/)),
    // `\{` / `\}` / `\"`: escaped literal braces/quotes — no depth change, and an
    // escaped quote does not open string mode (it lets content carry an unpaired `"`).
    _dialect_escape: _ => token(/\\[{}"]/),
    // A lone `\` (not before a brace or quote) is one ordinary character; matching it
    // alone keeps the *next* character in play, so `\\{` reads as a literal `\` followed
    // by the escaped brace `\{` (no depth change) — exactly as the reference scanner does.
    _dialect_backslash: _ => '\\',
    // A `"…"` string: braces inside don't count, `\` escapes the next byte (newlines
    // included, hence the explicit `(.|\n)` — a bare `.` doesn't match newline).
    _dialect_string: _ => token(seq('"', repeat(choice(/\\(.|\n)/, /[^"\\]/)), '"')),
    // A nested balanced brace group, counted toward depth.
    _dialect_braces: $ => seq('{', repeat($._dialect_chunk), '}'),

    // ------------------------------------------------------------------- operations

    builtin: $ => prec.right(seq(token(/__[a-z][a-zA-Z0-9_]*__/), optional($.type_arguments))),

    // Tail calls: `^` (self), `^name`, `^name.field`, `^.field`, and `^~` (tail-call the
    // flowing value). A tail call is an applicable head (see `_applicable`), so it may take a
    // juxtaposed argument — `^f [~, 1]` becomes an `application`.
    tail_call: $ => prec.right(seq(
      '^',
      optional(choice($.ripple, field('function', $.identifier))),
      repeat($._accessor),
    )),

    // `.` referring to the current process (not followed by an identifier/digit, which
    // would make it a field accessor).
    self: _ => prec(-1, '.'),

    // -------------------------------------------------------------------- select / @

    // The select operator, `!`:
    //   - `![a, b]`   general race/await form: a tuple of sources (each a chain)
    //   - `!'int`      body-less identity receive on a named type (module types too:
    //                  `!'%proc.changed`; named tuple types: `!Done`, `!Reply['ref, 'bin]`)
    //   - `!#'int`     body-less identity receive on a `#`-type
    //   - `!(type)`    body-less identity receive on a parenthesised or partial type
    //   - `!p` / `!f`  single source (process to await, or function to receive on)
    //   - `!@N`/`!@f`  process reference / spawn source
    //   - `!1000`      timeout source
    //   - `!`          bare (postfix) form, uses the chained value
    // A type-shorthand may carry a same-line `{ … }` block: the receive function's body — a
    // **filter** (`!'int { =42 => Ok }`), which leaves a non-matching message in the mailbox.
    // The three type forms (`!(type)`, `!'type`, `!#type`) take this optional filter body; the
    // other shorthands stay body-less. A *handler* is instead an arrowed block chain-step
    // (`!'int ~> { … }`), where the block is a separate `_term` after the `~>`.
    select: $ => prec.right(seq(
      '!',
      optional(choice(
        seq('[', optional($._nl), optional(commaSep1($, field('sources', $.chain))), optional($._nl), ']'),
        seq(choice($.partial_type, $._paren_type), optional(field('filter', $.block))),
        seq($.receive_type, optional(field('filter', $.block))),
        seq('#', field('receive', $._type_atom), optional(field('filter', $.block))),
        $.access,
        $.process_ref,
        $.spawn,
        $.integer,
      )),
    )),

    receive_type: $ => choice(
      $.module_type,
      $.type_identifier,
      alias($._named_tuple_type, $.tuple_type),
    ),

    // A *named* tuple type (`Done`, `Reply['ref, 'bin]`), for the receive shorthand —
    // the unnamed `[…]` form would collide with the select's source-list brackets.
    _named_tuple_type: $ => prec.right(choice(
      seq(field('name', $.tuple_name), immBracketed($, '[', $._field_type, ']')),
      field('name', $.tuple_name),
    )),

    // The state-sample operator, `?`: `?p` samples the target process's state (a snapshot,
    // never a wait). A trailing `?` on an identifier (`empty?`) is part of the identifier,
    // never this.
    state: $ => seq(
      '?',
      field('target', $.access),
    ),

    // `@N` process reference.
    process_ref: $ => seq('@', $.index),

    // `@f`/`@~` (spawn a function value), and the spawn shorthands `@{ ... }`,
    // `@'int { ... }` (module types too: `@'%mod.event { ... }`), `@(type) { ... }`,
    // `@[...] { ... }`, `@Name { ... }`. The spawn sugar keeps its body. The operand is
    // restricted (no value tuples/literals) so a `[`/`Name` after `@` is unambiguously a
    // type parameter rather than a value.
    spawn: $ => prec.right(seq(
      '@',
      optional(choice(
        seq(field('parameter', choice($.module_type, $.type_identifier, $.tuple_type, $._paren_type)), $.block),
        $.block,
        $.function,
        $.access,
      )),
    )),

    // -------------------------------------------------------------------- functions

    function: $ => seq(
      '#',
      optional($.type_parameters),
      optional(field('parameter', $._type)),
      optional(seq(optional($._nl), '->', optional($._nl), field('return', $._type))),
      optional($.block),
    ),

    // ----------------------------------------------------------------------- blocks

    // A block may open with an annotation prefix (`:key value` steps, semicolon/newline
    // separated), attaching to the value the braces denote; an annotation-only block
    // (`{ :error X }`) is identity-plus-attach and has no branches.
    block: $ => seq(
      '{', optional($._nl),
      choice(
        seq(
          $.annotation,
          repeat(seq($._sep, $.annotation)),
          optional(seq($._sep, $._block_branches)),
        ),
        $._block_branches,
      ),
      optional($._nl),
      '}',
    ),

    _block_branches: $ => seq(
      optional(seq('|', optional($._nl))),
      $.branch,
      repeat(seq(optional($._nl), '|', optional($._nl), $.branch)),
    ),

    // A branch is either condition-consequence (`… => …`) or a plain sequence; the
    // `condition`/`consequence` fields exist only on the former — a bare branch's body
    // is not a condition.
    branch: $ => choice(
      seq(
        field('condition', $.sequence),
        optional($._nl), '=>', optional($._nl),
        field('consequence', $.sequence),
      ),
      $.sequence,
    ),

    // ----------------------------------------------------------------------- tuples

    // `Name[...]` (immediate bracket) is a named tuple. A bare tuple is not an applicable
    // head, so `Name [...]` (with a space) is not a juxtaposed application — it needs a `~>`
    // between the terms.
    tuple: $ => choice(
      seq(field('name', $.tuple_name), immBracketed($, '[', $._field, ']')),
      bracketed($, '[', $._field, ']'),
      // Punning forms: `(a, b)` / `Foo(a, b)`, one or more puns and nothing else. Written
      // out rather than via `bracketed` because an empty `()` is not a tuple — it has no
      // name to pun, and nil is `[]`.
      seq(
        field('name', $.tuple_name),
        token.immediate('('), optional($._nl), commaSep1($, $.pun), optional($._nl), ')',
      ),
      seq('(', optional($._nl), commaSep1($, $.pun), optional($._nl), ')'),
      field('name', $.tuple_name),
    ),

    // A punned tuple entry: an access path standing for both a field label and its value,
    // so `(a, p.x)` builds `[a: &a, x: &p.x]`. Rooted at a variable, the parameter, or an
    // import; the label is the path's final named segment, so an index or annotation step
    // cannot end one, and a ripple root is not punnable.
    pun: $ => prec.right(seq(
      field('source', choice($.identifier, $.parameter, $.import)),
      repeat(seq('.', field('field', $.identifier))),
    )),

    _field: $ => choice(
      $.named_field,
      $.spread,
      $.chain,
    ),

    named_field: $ => seq(field('name', $.identifier), ':', optional($._nl), $.chain),
    // `...` (the flowing value), or a sourced `...a.b` / `...$conn` / `...~.f` — the same
    // access-path sources as a spread-update's head, glued to the dots.
    spread: $ => seq('...', optional(seq(
      field('source', choice($.identifier, $.parameter, $.ripple)),
      repeat(seq('.', field('field', choice($.identifier, $.index)))),
    ))),

    // --------------------------------------------------------------------- patterns

    bind_match: $ => seq('=', $._pattern),

    _pattern: $ => choice(
      $.pattern_pin,
      $.pattern_ascription,
      $.multiline_string,
      $.string,
      $.pattern_tuple,
      $.pattern_partial,
      $.pattern_or,
      $.module_type,
      $.type_identifier,
      $.self_default_type,
      $._paren_type,
      $.integer,
      $.binary,
      $.star,
      $.placeholder,
      $.identifier,
    ),

    // A pin: `&` + an access path rooted at a variable (`&x`, `&x.y.0`) or the parameter
    // (`&$`, `&$x`, `&$0.y` — the parameter rule carries the glued first-accessor sugar).
    // Field/index steps only: a pin compares by value, so annotation retrieval has no
    // place in its target.
    pattern_pin: $ => prec.right(seq(
      '&',
      choice($.identifier, $.parameter),
      repeat(seq('.', field('field', choice($.identifier, $.index)))),
    )),

    // A type-ascribed binding: a *parenthesised type* immediately followed by a binding
    // identifier — `('int)x`, `('int | 'bin)v`. Asserts the value's type and binds the whole
    // (narrowed) value. The identifier must be glued (token.immediate), matching the real
    // parser: `('int) x` is a type pattern with `x` left for the next term.
    pattern_ascription: $ => seq(
      $._paren_type,
      field('binding', alias($._identifier_immediate, $.identifier)),
    ),
    _identifier_immediate: _ => token.immediate(/[a-z][a-zA-Z0-9_]*\??!?/),
    // `*` binds every named field; `Name*` additionally requires the tuple's name. The `*`
    // is glued to the name, as a tuple pattern's name is glued to its bracket.
    star: $ => seq(optional(field('name', $.tuple_name)), token.immediate('*')),
    placeholder: _ => '_',

    // An alternation of patterns: `(p | q | …)`, two or more `|`-separated patterns. Shares the
    // parenthesised form with partial patterns and parenthesised type unions; the `|` (rather than
    // `:`/`,`) and a non-field leading pattern select this.
    pattern_or: $ => seq(
      '(', optional($._nl),
      $._pattern,
      repeat1(seq(optional($._nl), '|', optional($._nl), $._pattern)),
      optional($._nl), ')',
    ),

    pattern_tuple: $ => choice(
      seq(field('name', $.tuple_name), immBracketed($, '[', $._pattern_field, ']')),
      bracketed($, '[', $._pattern_field, ']'),
      field('name', $.tuple_name),
    ),

    _pattern_field: $ => choice(
      seq(field('name', $.identifier), ':', optional($._nl), $._pattern),
      $._pattern,
    ),

    pattern_partial: $ => choice(
      seq(field('name', $.tuple_name), immBracketed($, '(', $._partial_field, ')')),
      seq('(', optional($._nl), commaSep1($, $._partial_field), optional($._nl), ')'),
    ),

    _partial_field: $ => seq(
      field('name', $.identifier),
      optional(seq(':', optional($._nl), $._pattern)),
    ),

    // ------------------------------------------------------------------------ types

    _type: $ => choice(
      $.function_type,
      $.union_type,
      $.intersection_type,
      $._type_atom,
    ),

    // Callable type, with the optional clauses ` !'c` (receive) and ` ?'d` (states
    // beyond the parameter): `#'a -> 'b !'c ?'d`. Clause sigils are preceded by
    // whitespace (type names may end in `?`/`!`) and glued to their clause type.
    function_type: $ => prec.right(seq(
      '#',
      optional($.type_parameters),
      field('input', $._type_atom),
      optional(seq(optional($._nl), '->', optional($._nl), field('output', $._type_atom))),
      optional(seq('!', field('receive', $._type_atom))),
      optional(seq('?', field('states', $._type_atom))),
    )),

    union_type: $ => seq(
      optional(seq('|', optional($._nl))),
      choice($.intersection_type, $._type_atom),
      repeat1(seq(optional($._nl), '|', optional($._nl),
        choice($.intersection_type, $._type_atom))),
    ),

    // `'t & 'u`: the type of values satisfying every member. Binds tighter than `|`, so
    // `'t & 'u | 'v` is `('t & 'u) | 'v` — hence a union's members, not the other way round.
    intersection_type: $ => seq(
      $._type_atom,
      repeat1(seq('&', optional($._nl), $._type_atom)),
    ),

    _type_atom: $ => choice(
      $.tuple_type,
      $.partial_type,
      $.resource_type,
      $.cycle_type,
      $.process_type,
      $.module_type,
      $.self_default_type,
      $.type_identifier,
      $._paren_type,
    ),

    _paren_type: $ => seq('(', optional($._nl), $._type, optional($._nl), ')'),

    type_identifier: $ => seq(
      field('name', $.type_name),
      optional($.type_arguments),
    ),

    // A type reached through a module's type namespace: `'%mod` (default type) or
    // `'%mod.name` (named type), with optional type arguments (`'%list<'int>`).
    // Higher precedence than `self_default_type` so that after a leading `'`, a following
    // `%` continues into a module type rather than reducing the bare `'`.
    module_type: $ => prec(1, seq(
      "'",
      field('module', $.import),
      optional(seq(token.immediate('.'), field('member', $.identifier))),
      optional($.type_arguments),
    )),

    // The enclosing module's own default type: a bare `'`, optionally applied to type
    // arguments (`'<'int>`).
    self_default_type: $ => seq("'", optional($.type_arguments)),

    type_arguments: $ => immBracketed($, '<', $._type, '>'),

    tuple_type: $ => choice(
      seq(field('name', choice($.tuple_name, $.type_name)), immBracketed($, '[', $._field_type, ']')),
      bracketed($, '[', $._field_type, ']'),
      field('name', $.tuple_name),
    ),

    // Partial types require all fields to be named (or spreads), which keeps the unnamed
    // form `(x: 'int)` distinct from a parenthesised grouping `('int | 'bin)`.
    partial_type: $ => choice(
      seq(field('name', $.tuple_name), immBracketed($, '(', $._partial_type_field, ')')),
      seq('(', optional($._nl), ')'),
      seq('(', optional($._nl), commaSep1($, $._partial_type_field), optional($._nl), ')'),
    ),

    _partial_type_field: $ => choice(
      $.type_spread,
      seq(field('name', $.identifier), ':', optional($._nl), $._type),
    ),

    // A field of a tuple type. Every non-spread form may carry a default (a spread names no
    // field, so it takes none).
    _field_type: $ => choice(
      $.type_spread,
      seq($._field_type_base, optional($.field_default)),
    ),

    _field_type_base: $ => choice(
      // Omittable label `(name): 'type` — the mark is a calling convention, legal only in
      // a function type's parameter tuple: a call argument may then omit the label and
      // give the field positionally.
      seq('(', field('name', $.identifier), ')', ':', optional($._nl), $._type),
      seq(field('name', $.identifier), ':', optional($._nl), $._type),
      // Decorators: a field entry with no type at all, adjusting a field an earlier spread
      // brought in. It touches only the label and — via the `field_default` suffix that
      // `_field_type` adds, so `(name) = v` and `name = v` fall out here for free — the
      // default, leaving the field's type and position alone. `(name)` marks the label
      // omittable; a bare `name` is a checked restatement that changes nothing.
      //
      // The real parser accepts the *bare* form only when the same tuple contains a
      // spread; without that restriction `A[x]` would read as a tuple type and break
      // pattern alternations like `=(A[x] | B[x])`. A context-free grammar cannot state
      // that condition, so the bare form is accepted generally here — as with the
      // field-default placement rule, the condition is left to the compiler, which
      // reports it better than a positional grammar could.
      seq('(', field('name', $.identifier), ')'),
      // The bare form is the one reading that a tuple *pattern*'s field also has, so where
      // both are live — `=(A[x] | B[x])`, an or-pattern the parenthesised-union reading
      // would otherwise claim — a negative dynamic precedence hands the parse to the
      // pattern. Nothing is lost: a type position (a function's parameter, an alias'
      // definition) never offers the pattern reading for the penalty to tip.
      prec.dynamic(-1, field('name', $.identifier)),
      $._type,
    ),

    // A field's default value: `= <chain>`, spaced like a binding. A call argument may omit a
    // field that declares one. Only meaningful in a function literal's parameter spelling;
    // the grammar accepts it on any field type and leaves the placement rule to the compiler,
    // which reports it better than a positional grammar could.
    field_default: $ => seq('=', optional($._nl), $.chain),

    type_spread: $ => seq('...', optional(seq($.type_name, optional($.type_arguments)))),

    resource_type: $ => /\\[A-Z][a-zA-Z0-9_]*/,
    cycle_type: $ => prec.right(seq('^', optional($.index))),

    // Process type: each grant spelled as the operation that exercises it — a glued
    // head (send), `!'r` (await), `?'s` (sample): `@['msg] [!'r] [?'s]`, e.g.
    // `@'evt ?'status`, `@!'r`, `@?'s`. The real parser requires *horizontal*
    // whitespace before a clause sigil after a head, and a function output's trailing
    // clauses are the function's; with whitespace as extras, both are approximate here.
    process_type: $ => prec.right(seq(
      '@',
      optional(field('message', $._type_atom)),
      optional(seq('!', field('result', $._type_atom))),
      optional(seq('?', field('state', $._type_atom))),
    )),

    // ------------------------------------------------------------------- terminals

    identifier: _ => /[a-z][a-zA-Z0-9_]*\??!?/,
    type_name: _ => token(seq("'", /[a-z][a-zA-Z0-9_]*\??!?/)),
    tuple_name: _ => /[A-Z][a-zA-Z0-9_]*/,

    integer: _ => /-?\d+/,
    // Hex digits between angle brackets, with whitespace separating groups and a newline
    // starting a new row (`<6a09e667 bb67ae85>`, or a table across lines). Like the integer
    // rule this is laxer than the real parser, which additionally requires every group to be
    // a whole number of bytes and forbids padding the brackets on a single line.
    //
    // The negative lexical precedence loses to the glued `<` of a `type_arguments` list
    // wherever both are valid, which is what keeps a type argument whose name happens to
    // be hex digits (`f<ABC>`) from lexing as a binary. It costs nothing in term position,
    // where the immediate `<` is not a candidate at all, and a *spaced* `f <0a1b>` still
    // reaches this rule because an immediate token cannot follow skipped whitespace.
    binary: _ => token(prec(-1, /<[0-9a-fA-F \t\r\n]*>/)),

    // A single-line string. Content stops at `{`: an unescaped brace opens an
    // interpolation hole, parsed exactly like a block body (`string_segments` in
    // quiver-compiler/src/parser.rs) — a literal `{` is written `\{`, while a bare `}` is
    // ordinary content. The content token's lexical precedence keeps it ahead of the
    // whitespace/comment `extras` (as for dialect content). Pattern-position strings
    // don't interpolate in the real language (`{` is literal there); the grammar
    // nevertheless shares this rule for both positions — a separate flat-content token
    // would fight this one in the lexer states where the tuple/pattern-tuple GLR overlap
    // makes both readings live, and longest-match would swallow the holes. The cost is
    // that an *unescaped* `{` in a pattern string over-parses as a hole, accepted for
    // editor purposes (the conventional spelling is `\{` in both positions).
    string: $ => seq(
      '"',
      repeat(choice(
        token.immediate(prec(1, /[^"\\{]+/)),
        $.escape_sequence,
        alias($.block, $.interpolation),
      )),
      '"',
    ),
    escape_sequence: _ => token.immediate(/\\["\\nrt{]/),

    // A triple-quoted, multi-line string, with the same interpolation holes. The content
    // between holes is a few immediate tokens whose lexical precedence keeps them ahead
    // of the whitespace/comment `extras`, which would otherwise eat content (a comment
    // token could even swallow text after a hole). The main token is a run of
    // escape-aware units — up to two quotes followed by an escape (`\` + any char,
    // newlines included) or an ordinary non-quote, non-brace character — so `\"""` stays
    // content and newlines live inside the token. A quote run that the main token can't
    // extend (one or two quotes directly before a hole) is picked up by the bare-quotes
    // token, which stays *below* the closing delimiter's length-3 match so three
    // unescaped quotes always close the string.
    multiline_string: $ => seq(
      '"""',
      repeat(choice(
        token.immediate(prec(1, /([^"\\{]|\\(.|\n)|""?[^"\\{]|""?\\(.|\n))+/)),
        token.immediate(/""?/),
        alias($.block, $.interpolation),
      )),
      '"""',
    ),
  },
});
