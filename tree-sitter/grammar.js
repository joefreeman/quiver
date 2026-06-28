/**
 * @file Quiver grammar for tree-sitter
 * @author Quiver
 * @license MIT
 *
 * Quiver is a statically-typed functional language. Data flows left→right through
 * transformation pipelines. The grammar mirrors quiver-compiler/src/parser.rs.
 *
 * Surface syntax (post whitespace-migration):
 * - **Application is argument-first.** A callable term consumes the flowing value; build
 *   the argument tuple first, then name the function: `[3, 4] num.add`. There is no
 *   `f x` / `f [args]` juxtaposition.
 * - **A chain is space-separated terms.** The value flows through them; nil passes through
 *   (no short-circuit). `~>` is an *optional* separator equivalent to a space, and doubles
 *   as a **line continuation**: a chain ends at a bare newline, but a newline followed by
 *   `~>` continues it.
 * - **A sequence is chains separated by comma or newline** (the two are synonyms). This is
 *   where the value short-circuits on nil and where binding scope advances.
 * - **The program is one sequence** of chains with type-alias declarations interspersed,
 *   all separated by the sequence separator. There is no `;`.
 *
 * Whitespace handling: spaces, tabs and comments are `extras` (ignored everywhere), but
 * newlines are significant. A newline (or comma) separates the chains of a sequence;
 * continuation points (after `~>`, `,`, `|`, `=>`, `=`, and inside brackets) explicitly
 * permit newlines via the `_nl` helper.
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

module.exports = grammar({
  name: 'quiver',

  word: $ => $.identifier,

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
    // `&x` inside brackets may be a pinned pattern (`[&y, x]`) or a reference term
    // (`[&inc, 100]`).
    [$._access_source, $.pattern_pin],
    // A type atom may stand alone or be the first variant of a multi-line union; the
    // newline-then-`|` lookahead chooses (and likewise whether a trailing newline
    // extends the union or terminates the statement).
    [$._type, $.union_type],
    [$.union_type],
    // A function/function-type may have optional trailing parts (return type, body); a
    // newline after a complete one ends the chain rather than extending it.
    [$.function],
    [$.function_type],
    // After a chain, a separator (comma/newline) may continue the current sequence
    // (another chain) or end it so the surrounding construct can take a trailing separator.
    [$.expression],
    // After a term, a newline may continue the chain (next line starts with `~>`) or end
    // the chain.
    [$.chain],
    // After a branch condition, a newline may precede `=>` (its consequence) or the next
    // branch / block close.
    [$.branch],
    // `source.field` — greedily attach trailing `.field` accessors to the access rather
    // than treating `.` as a self-send term.
    [$.access],
    // A parenthesised pattern beginning with an identifier may be a partial pattern
    // (`(x: …)`, `(x, …)`) or the first alternative of an or-pattern (`(x | …)`); the `:`/`,`
    // versus `|` that follows decides, via GLR.
    [$._pattern, $._partial_field],
    // A parenthesised group of `|`-separated atoms can be read as an or-pattern (of tuple/type
    // patterns) or as a parenthesised type union; both are accepted for editor purposes.
    [$.pattern_tuple, $.tuple_type],
    [$._pattern, $._type_atom],
    [$.pattern_partial, $.partial_type],
  ],

  rules: {
    // The program is a single sequence of chains with type-alias declarations
    // interspersed, all separated by the sequence separator (comma or newline, which are
    // synonyms). Consecutive chains group into an `expression`; type aliases break a run.
    source_file: $ => seq(
      optional($._sep),
      optional(seq(
        $._top_level_item,
        repeat(seq($._sep, $._top_level_item)),
        optional($._sep),
      )),
    ),

    _top_level_item: $ => choice($.type_alias, $.expression),

    // The sequence separator: one or more commas/newlines (they are synonyms), collapsing
    // runs. This is where the flow short-circuits on nil and binding scope advances.
    _sep: _ => prec.right(repeat1(choice('\n', ','))),

    // One or more newlines: a continuation point inside an unfinished construct.
    _nl: _ => prec.right(repeat1('\n')),

    comment: _ => token(seq('//', /[^\n]*/)),

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

    type_parameters: $ => immBracketed($, '<', $.type_name, '>'),

    // ----------------------------------------------------------------- expressions

    // A sequence of chains separated by the sequence separator (comma or newline). Used at
    // the top level (grouping a run of chains) and as block/branch bodies.
    expression: $ => seq(
      $.chain,
      repeat(seq($._sep, $.chain)),
    ),

    // A chain is a sequence of `primary` terms; the value flows left→right through them,
    // with nil passing through (no short-circuit — that is the sequence separator's job).
    // Terms are joined by horizontal whitespace (`a b c`) or, equivalently, by an optional
    // `~>` — which doubles as an explicit line continuation: a chain ends at a bare
    // newline, but a newline followed by `~>` continues it. Application is argument-first
    // (`[args] f`); there is no juxtaposition.
    chain: $ => seq(
      // `pattern = chain` binding. The real parser distinguishes a binding (`x = e`, `=`
      // space-surrounded) from an in-chain match (`e =x`, `=` glued) by spacing; tree-sitter
      // treats whitespace as `extras`, so when the leading term is a binding target followed
      // by `=`, prefer the binding reading via dynamic precedence.
      optional(prec.dynamic(1, seq(field('binding', $._binding_target), '=', optional($._nl)))),
      $._primary,
      repeat(seq(optional($._pipe), $._primary)),
    ),

    // The optional, explicit chain separator / line continuation.
    _pipe: $ => seq(optional($._nl), '~>', optional($._nl)),

    // The forms valid as the target of a chain binding (`x = ...`, `[a, b] = ...`,
    // `(add, mul) = ...`, `* = ...`). A bare type or literal is never a binding target,
    // which keeps bindings distinct from type aliases.
    _binding_target: $ => choice(
      $.identifier,
      $.pattern_tuple,
      $.pattern_partial,
      $.star,
      $.placeholder,
    ),

    _primary: $ => choice(
      $.string,
      $.integer,
      $.binary,
      $.builtin,
      $.process_ref,
      $.spawn,
      $.self,
      $.bind_match,
      $.reference,
      $.select,
      $.tail_call,
      $.equality,
      $.not,
      $.tuple,
      $.function,
      $.block,
      $.spread_update,
      $.access,
    ),

    // Name-preserving spread-update: `a[..., y]` / `~[..., y]`. The bracket is adjacent (no
    // space) and begins with a spread, distinguishing it from `a [..., y]` (two terms) and
    // from a plain spread tuple `[...a, y]`. The result inherits the source tuple's name.
    spread_update: $ => seq(
      field('source', choice($.ripple, $.identifier)),
      token.immediate('['), optional($._nl),
      $.spread,
      repeat(seq(optional($._nl), ',', optional($._nl), $._field)),
      optional(seq(optional($._nl), ',')),
      optional($._nl), ']',
    ),

    // ----------------------------------------------------------------------- access

    // A variable/parameter/ripple/import optionally followed by `.field`/`.0` accessors,
    // or a leading accessor with no source (`.name` as a chain step).
    access: $ => choice(
      seq(field('source', $._access_source), repeat($._accessor)),
      repeat1($._accessor),
    ),

    _access_source: $ => choice(
      $.identifier,
      $.parameter,
      $.ripple,
      $.import,
    ),

    parameter: _ => '$',
    ripple: _ => '~',

    _accessor: $ => seq('.', field('field', choice($.identifier, $.index))),
    index: _ => /\d+/,

    // `%num`, `%mathx/vec`. The `/` path separator is immediate so a later `mod / x`
    // (with surrounding spaces) is not mistaken for part of the module path.
    import: $ => seq('%', $.identifier, repeat(seq(token.immediate('/'), $.identifier))),

    // ------------------------------------------------------------------- operations

    builtin: _ => token(/__[a-z][a-zA-Z0-9_]*__/),

    equality: _ => '==',
    not: _ => '<>',

    // Tail calls: `^` (self), `^name`, `^name.field`, `^.field`, and `^~` (tail-call the
    // flowing value). Application is argument-first, so a tail call takes no juxtaposed
    // argument — the argument is the preceding term.
    tail_call: $ => prec.right(seq(
      '^',
      optional(choice($.ripple, field('function', $.identifier))),
      repeat($._accessor),
    )),

    // `&name`/`&mod.f` (reference), `&.` (self ref), `&__b__`. References a value without
    // calling it; the target is required (a fresh ref is minted via the `%ref` module).
    reference: $ => prec.right(seq(
      '&',
      choice(
        $.self,
        $.builtin,
        $.access,
      ),
    )),

    // `.` referring to the current process (not followed by an identifier/digit, which
    // would make it a field accessor).
    self: _ => prec(-1, '.'),

    // -------------------------------------------------------------------- select / @

    // The select operator, `!`:
    //   - `! [a, b]`   general race/await form: a tuple of sources (each a chain)
    //   - `!'int`      body-less identity receive on a named type
    //   - `!#'int`     body-less identity receive on a `#`-type
    //   - `!(type)`    body-less identity receive on a parenthesised type
    //   - `!p` / `!f`  single source (process to await, or function to receive on)
    //   - `!@N`/`!@f`  process reference / spawn source
    //   - `!1000`      timeout source
    //   - `!`          bare (postfix) form, uses the chained value
    // The shorthands are body-less: a `{ … }` after a select is a separate chain step that
    // handles the received message. Filters use the general form `! [#T { filter }]`.
    select: $ => prec.right(seq(
      '!',
      optional(choice(
        seq('[', optional($._nl), optional(commaSep1($, field('sources', $.chain))), optional($._nl), ']'),
        seq('(', optional($._nl), $._type, optional($._nl), ')'),
        $.receive_type,
        seq('#', field('receive', $._type_atom)),
        $.access,
        $.process_ref,
        $.spawn,
        $.integer,
      )),
    )),

    receive_type: $ => $.type_identifier,

    // `@N` process reference.
    process_ref: $ => seq('@', $.index),

    // `@f`/`@~` (spawn a function value), and the spawn shorthands `@{ ... }`,
    // `@'int { ... }`, `@(type) { ... }`, `@[...] { ... }`, `@Name { ... }`. The spawn
    // sugar keeps its body. The operand is restricted (no value tuples/literals) so a
    // `[`/`Name` after `@` is unambiguously a type parameter rather than a value.
    spawn: $ => prec.right(seq(
      '@',
      optional(choice(
        seq(field('parameter', choice($.type_identifier, $.tuple_type, $._paren_type)), $.block),
        $.block,
        $.access,
        $.reference,
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

    block: $ => seq(
      '{', optional($._nl),
      optional(seq('|', optional($._nl))),
      $.branch,
      repeat(seq(optional($._nl), '|', optional($._nl), $.branch)),
      optional($._nl),
      '}',
    ),

    branch: $ => seq(
      field('condition', $.expression),
      optional(seq(optional($._nl), '=>', optional($._nl), field('consequence', $.expression))),
    ),

    // ----------------------------------------------------------------------- tuples

    // `Name[...]` (immediate bracket) is a named tuple. Application is argument-first, so a
    // `Name [...]` with a space is two separate terms, not application.
    tuple: $ => choice(
      seq(field('name', $.tuple_name), immBracketed($, '[', $._field, ']')),
      bracketed($, '[', $._field, ']'),
      field('name', $.tuple_name),
    ),

    _field: $ => choice(
      $.named_field,
      $.spread,
      $.chain,
    ),

    named_field: $ => seq(field('name', $.identifier), ':', optional($._nl), $.chain),
    spread: $ => seq('...', optional(field('source', $.identifier))),

    // --------------------------------------------------------------------- patterns

    bind_match: $ => seq('=', $._pattern),

    _pattern: $ => choice(
      $.pattern_pin,
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

    pattern_pin: $ => seq('&', $.identifier),
    star: _ => '*',
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
      $._type_atom,
    ),

    function_type: $ => seq(
      '#',
      optional($.type_parameters),
      field('input', $._type_atom),
      optional(seq(optional($._nl), '->', optional($._nl), field('output', $._type_atom))),
    ),

    union_type: $ => seq(
      optional(seq('|', optional($._nl))),
      $._type_atom,
      repeat1(seq(optional($._nl), '|', optional($._nl), $._type_atom)),
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

    _field_type: $ => choice(
      $.type_spread,
      seq(field('name', $.identifier), ':', optional($._nl), $._type),
      $._type,
    ),

    type_spread: $ => seq('...', optional(seq($.type_name, optional($.type_arguments)))),

    resource_type: $ => /\\[A-Z][a-zA-Z0-9_]*/,
    cycle_type: $ => prec.right(seq('^', optional($.index))),

    process_type: $ => choice(
      seq('(', optional($._nl), '@', optional($._type_atom), optional($._nl), '->', optional($._nl), $._type_atom, optional($._nl), ')'),
      prec.right(seq('@', optional($._type_atom))),
    ),

    // ------------------------------------------------------------------- terminals

    identifier: _ => /[a-z][a-zA-Z0-9_]*\??!?/,
    type_name: _ => token(seq("'", /[a-z][a-zA-Z0-9_]*\??!?/)),
    tuple_name: _ => /[A-Z][a-zA-Z0-9_]*/,

    integer: _ => /-?\d+/,
    binary: _ => /0x[0-9a-fA-F]*/,

    string: $ => seq(
      '"',
      repeat(choice(
        token.immediate(prec(1, /[^"\\]+/)),
        $.escape_sequence,
      )),
      '"',
    ),
    escape_sequence: _ => token.immediate(/\\["\\nrt]/),
  },
});
