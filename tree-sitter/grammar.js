/**
 * @file Quiver grammar for tree-sitter
 * @author Quiver
 * @license MIT
 *
 * Quiver is a statically-typed functional language with postfix flow. The grammar
 * mirrors quiver-compiler/src/parser.rs.
 *
 * Whitespace handling: spaces, tabs and comments are `extras` (ignored everywhere),
 * but newlines are significant. A newline (or `;`) separates top-level statements, and
 * a newline prevents juxtaposition application (`f x`) from crossing lines. Continuation
 * points (after `~>`, `,`, `|`, `=>`, `=`, and inside brackets) explicitly permit
 * newlines via the `_nl` helper.
 */

/* eslint-disable arrow-parens */
/* eslint-disable camelcase */

/// Comma-separated list (one or more) allowing newlines around commas. Used inside
/// brackets/parens/angles where there is no statement terminator to conflict with.
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
    // newline after a complete one ends the statement rather than extending it.
    [$.function],
    [$.function_type],
    // A trailing comma after a chain may end the expression or precede a further chain
    // on the next line.
    [$.expression],
    // After a term, a newline may continue the chain (next line starts with `~>`) or end
    // the statement.
    [$.chain],
    // After a branch condition, a newline may precede `=>` (its consequence) or the next
    // branch / block close.
    [$.branch],
    // `source.field` — greedily attach trailing `.field` accessors to the access rather
    // than treating `.` as a self-send term.
    [$.access],
  ],

  rules: {
    source_file: $ => seq(
      optional($._terminator),
      optional(seq(
        $._statement,
        repeat(seq($._terminator, $._statement)),
        optional($._terminator),
      )),
    ),

    // One or more newlines and/or semicolons: separates top-level statements.
    _terminator: $ => repeat1(choice('\n', ';')),

    // One or more newlines: a continuation point inside an unfinished construct.
    _nl: _ => prec.right(repeat1('\n')),

    comment: _ => token(seq('//', /[^\n]*/)),

    // ------------------------------------------------------------------ statements

    _statement: $ => choice(
      $.type_alias,
      $.expression,
    ),

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

    // A comma-separated sequence of chains. Newlines are allowed only after a comma so
    // that a bare newline ends the statement instead of continuing the expression.
    expression: $ => seq(
      $.chain,
      repeat(seq(',', optional($._nl), $.chain)),
      optional(','),
    ),

    chain: $ => seq(
      optional(seq(field('binding', $._binding_target), '=', optional($._nl))),
      $._term,
      repeat(seq(optional($._nl), '~>', optional($._nl), $._term)),
    ),

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

    _term: $ => choice($.application, $._primary),

    // Juxtaposition application: `f 5`, `double x`, `map [xs, &f]`. The two operands are
    // adjacent on the same line (no `~>` between them; a newline would end the term).
    application: $ => prec.left(seq(
      field('function', $._primary),
      field('argument', $._primary),
    )),

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
      $.access,
    ),

    // ----------------------------------------------------------------------- access

    // A variable/parameter/ripple/import optionally followed by `.field`/`.0` accessors,
    // or a leading accessor with no source (`~> .name`).
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

    // `%math`, `%math/trig`. The `/` path separator is immediate so a later `mod / x`
    // (with surrounding spaces) is not mistaken for part of the module path.
    import: $ => seq('%', $.identifier, repeat(seq(token.immediate('/'), $.identifier))),

    // ------------------------------------------------------------------- operations

    builtin: _ => token(/__[a-z][a-zA-Z0-9_]*__/),

    equality: _ => '==',
    not: _ => '<>',

    // `^`, `^f`, `^math.mul` tail calls. An argument, if any, is applied by juxtaposition.
    tail_call: $ => prec.right(seq(
      '^',
      optional(field('function', $.identifier)),
      repeat($._accessor),
    )),

    // Bare `&` (new ref), `&name`/`&mod.f` (reference), `&.` (self ref), `&__b__`.
    reference: $ => prec.right(seq(
      '&',
      optional(choice(
        $.self,
        $.builtin,
        $.access,
      )),
    )),

    // `.` referring to the current process (not followed by an identifier/digit, which
    // would make it a field accessor).
    self: _ => prec(-1, '.'),

    // -------------------------------------------------------------------- select / @

    // `!`, `![a, b]`, `!p`, `!'int { ... }`, `!(type)`, `!1000`. The non-bracket sources
    // are restricted (no tuples) so that a `[` after `!` is unambiguously a source list.
    select: $ => prec.right(seq(
      '!',
      optional(choice(
        field('sources', bracketed($, '[', $.chain, ']')),
        seq($.receive_type, optional($.block)),
        seq('(', optional($._nl), $._type, optional($._nl), ')', optional($.block)),
        $.access,
        $.function,
        $.process_ref,
        $.spawn,
        $.reference,
        $.integer,
      )),
    )),

    receive_type: $ => $.type_identifier,

    // `@f` spawn, `@{ ... }`/`@'int { ... }` spawn shorthand, `@5` process ref.
    process_ref: $ => seq('@', $.index),

    // `@f`/`@~` (spawn a function value), and the spawn shorthands `@{ ... }`,
    // `@'int { ... }`, `@(type) { ... }`, `@[...] { ... }`, `@Name { ... }`. The operand
    // is restricted (no value tuples/literals) so a `[`/`Name` after `@` is unambiguously
    // a type parameter rather than a value.
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

    // `Name[...]` (immediate bracket) is a named tuple; `Name [...]` (with a space) is
    // instead application of the bare tuple `Name` to a tuple, handled by `application`.
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
      $.type_identifier,
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
