# Quiver language specification

Quiver is a statically-typed functional programming language with structural typing, pattern matching, and a pipeline syntax. Programs are composed of immutable values flowing left-to-right through `~>`-separated transformation pipelines.

## Core concepts

### Values and immutability

All values in Quiver are immutable. The language supports:

- **Integers**: `42`, `-17` (decimal)
- **Binaries**: `0x0a1b2c` (hexadecimal bytes; an even number of hex digits, so `0x` alone is the empty binary)
- **Tuples**: Collections of values with optional names and field labels

### Value flow

A value flows left-to-right through a **pipeline** — a sequence of terms joined by the pipe marker `~>`. The value produced by one term becomes the input to the next:

```quiver
5 ~> double ~> increment
```

A callable term consumes the value flowing into it, so piping a value into a function calls it (`5 ~> double`). Application can also be written **function-first** by juxtaposition — a callable followed by a single argument (`double 5`, `num.add [3, 4]`); the flowing value flows into that argument, so `5 ~> num.add [~, 100]` computes `[5, 100]`'s sum (see [Function application](#function-application)).

### Pattern matching

Pattern matching allows destructuring values and branching based on their structure. Matches can test literal values, extract components, and control program flow.

```quiver
'response = Success['bin] | Error[code: 'int]

handle_response = #'response {
  | =Success[content] => content
  | =Error[code: 404] => default_page
  | error_page
}
```

## Types

Type names are written with a leading `'` (apostrophe): the built-in `'int`, `'bin`, `'ref`, and any type alias such as `'point`. This distinguishes types from variables and field names, which share the same lowercase identifier syntax. Named tuple types (such as `Point`) are already distinguished by their leading uppercase letter, so they take no prefix.

### Basic types

- `'int` - Integer values
- `'bin` - Binary data (bytes)
- `'ref` - Unique opaque identifiers

### Tuple types

Tuples are the primary composite type. They can optionally have a name, and have zero or more fields. Tuple names start with an uppercase letter. Each field has a type, and may be named (as per identifiers; see below).

```quiver
[]                          // Empty tuple ('nil')
Blue                        // Named, empty tuple
['int, 'int]                // Unnamed tuple with unnamed fields
[x: 'int, y: 'int]          // Named fields
Point[x: 'int, y: 'int]     // Named tuple
A[b: B[c: C['bin]]]         // Nested tuples
```

### Partial types

Partial types define structural constraints on tuples without specifying all fields. They use parentheses instead of brackets and require all fields to be named:

```quiver
(x: 'int, y: 'int)       // Unnamed partial - matches any tuple with 'x' and 'y' integer fields
Point(x: 'int)           // Named partial - matches tuples named 'Point' with an 'x' integer field
()                       // Empty unnamed partial - matches any tuple
Point()                  // Empty named partial - matches any tuple named 'Point'
```

### Type aliases

Type aliases are defined with `=`, the same operator used for value bindings — the `'` prefix on the name marks the statement as a type definition:

```quiver
'point = Point[x: 'int, y: 'int]
'adder = #'int -> 'int
'writer = (write: (#'bin -> Ok))
```

A type definition with the name omitted — a bare `'` — declares a module's default type, reached from other modules as `'%mod` (see [Module types](#module-types)). A module has at most one:

```quiver
' = Str['bin]                  // this module's default type
'<'t> = Nil | Cons['t, ^]      // a parameterised default type
```

### Parameterised types

Type aliases and functions can be parameterised with type parameters using angle brackets:

```quiver
'pair<'a, 'b> = Pair[first: 'a, second: 'b]
```

Functions can also declare type parameters:

```quiver
id = #<'t>'t { $ }
map = #<'t, 'u>['list<'t>, #'t -> 'u] { ... }
```

Type parameters are inferred from usage. When a function with type parameters is called with different argument types, the type variables are unified (i.e., widened) to find the most general type that satisfies all constraints.

### Union types

```quiver
'bool = True | False
'shape =
  | Circle[radius: 'int]
  | Rectangle[width: 'int, height: 'int]
```

### Intersection types

`'t & 'u` is the type of values satisfying *every* member. It binds tighter than `|`, so `'t & 'u | 'v` is `('t & 'u) | 'v`. Disjoint members intersect to nothing, so `'int & 'bin` is uninhabited (it matches no value). Intersection is most useful for composing partial-type constraints:

```quiver
'readable = (read: (#'bin -> Ok))
'writable = (write: (#'bin -> Ok))
'rw = 'readable & 'writable          // a tuple with both fields

x =('rw)                             // in matching, each member is checked separately
```

### Recursive types

Use `^` to refer back to the root of the types, or `^1`/`^2`/etc to refer to ancestral type boundaries (i.e., unions) from the root.

```quiver
'list<'t> = Nil | Cons['t, ^]
'tree<'t> = Leaf['t] | Node[^, ^]
'json =
  | Null
  | 'bool
  | 'int
  | Str['bin]
  | Array[(Nil | Cons[^, ^1])]
```

### Type spreads

Type aliases can be extended using the spread operator `...` to compose new types from existing ones:

```quiver
// Compose types from reusable pieces
'entity = [id: 'int, created_at: 'int]
'updateable = (updated_at: 'int)
'post = Post[...'entity, title: Str['bin], ...'updateable]  // Post[id: 'int, created_at: 'int, title: Str['bin], updated_at: 'int]

// Field override - later fields override earlier ones
'v1 = User[id: 'int, name: Str['bin]]
'v2 = 'v1[..., id: 'bin]  // User[id: 'bin, name: Str['bin]]
```

When spreading a union type, the spread is distributed across all variants:

```quiver
'event = Created[id: 'int] | Updated | Deleted
'logged = 'event[..., timestamp: 'int] // Created[id: 'int, timestamp: 'int] | Updated[timestamp: 'int] | Deleted[timestamp: 'int]
```

### Strings

The compiler converts UTF-8 strings, defined with `"..."` into binaries, wrapped in a `Str` tuple (`Str[0x...]`).

The escape sequences `\n`, `\r`, `\t`, `\\`, `\"` and `\{` are recognised.

#### Interpolation

A string literal — single- or multi-line — may embed `{ … }` holes; the value is its literal text
concatenated with the holes' values. Each hole is parsed like a block body and must evaluate to a
`Str` — a non-`Str` hole is a compile-time error. Like a tuple field, a hole receives the flowing
value, so `~` refers to it; it can also draw on variables in scope. A literal brace is written `\{`.
(In a *pattern*, `{` is literal; patterns don't interpolate.)

```quiver
name = "world"
"hello {name}"               // "hello world"
"world" ~> "hello, {~}"      // "hello, world" — the chained value flows into the hole
"sum: {%str.concat [a, b]}"  // any expression that yields a Str
"a \{ b"                     // a literal brace
```

#### Multi-line strings

A string delimited by triple quotes (`"""`) may span multiple lines. The opening `"""` must be followed by a newline and the closing `"""` must sit on its own line; the indentation of that closing line sets a **margin** stripped from every line (a line indented less is an error):

```quiver
msg = """
    hello
      indented
    """          // "hello\n  indented"
```

The newline before the closing `"""` is not included (end with a blank line for a trailing newline); blank lines are emitted empty and trailing whitespace is stripped per line. The same escapes apply, plus `\s` (a space that survives trailing-whitespace stripping) and a trailing `\` (line continuation: drops the newline and the next line's leading whitespace; a space before the `\` is kept). Embed a literal `"""` as `\"""`.

```quiver
"""
    one \
    two
    three
    """          // "one two\nthree"
```

## Expressions

A whole program is a single **sequence**: a series of **steps** separated by a semicolon or a newline (the two are synonyms). Type-alias declarations may be interspersed between steps and are transparent to the flow. Each step is a [chain](#chains).

A sequence **threads** and is **fallible**: each step starts from the previous step's result, and if a step evaluates to nil (`[]`) the rest of the sequence short-circuits and the whole sequence evaluates to nil. Variable bindings persist across steps. (`,` is not a step separator — it appears only inside brackets: tuple fields, type arguments, and select sources.)

### Chains

A chain is a `~>`-separated sequence of terms — the basic unit of left-to-right flow. The first term starts from the chain's input (the previous step's result, or, for the first step of a block, the block's parameter); each subsequent term transforms the flowing value. The `~>` marker between terms is **mandatory** — whitespace alone does not join terms (a missing `~>` is a pointed parse error). A chain is an **infallible pipe**: nil flows through it like any other value (no short-circuit *within* a chain — only a semicolon/newline step boundary short-circuits on nil). So the two-axis model is: *chain = infallible pipe (nil flows), sequence = fallible pipe (nil short-circuits)*, and `~` always names "the value to the left."

A bare newline ends a chain (starting a new sequence step). To continue one chain across several lines, begin the continuation line with `~>` — the ordinary term separator, placed at the start of the line:

```quiver
foo
~> bar
~> baz
```

When a term receives a value:
- **Callable terms** (functions, processes) are called with the value — unless the callable is **nilary** (its parameter is nil), in which case it ignores the flowing value and is called with nil, like a literal
- **Literals and tuples** replace the value (discarding it)
- **Variables** depend on their type: callable variables are called, others replace the value

So a nilary `f` needs no explicit argument: `f` and `5 ~> f` both call it with nil.

A term may also be a **function-first application** — a callable followed by a single
argument (`double 5`, `num.add [3, 4]`); see [Function application](#function-application).

To explicitly control this behavior:
- `&f` references `f` without calling it

The flowing value is also passed into the fields of a tuple that is constructed in the
chain, and into the arguments of a call. Each field/argument receives its own copy, so a
callable there is called with it (and a non-callable value simply replaces it):

```quiver
inc = #'int { num.add [~, 1] }
5 ~> [inc, 100]         // [6, 100] - inc is called with 5
5 ~> [&inc, 100]        // [<function>, 100] - & passes inc by value
```

### Control flow

Steps in a sequence are executed one at a time. If a step evaluates to nil (`[]`), the sequence short-circuits and evaluates to nil. Since semicolon and newline are synonyms, the same holds across lines:

```quiver
[] ~> 5   // one step (a chain) — nil flows through, evaluates to 5
[]; 5     // two steps — the first is nil, so the sequence short-circuits to []
```

See [Blocks](#blocks) below for further control flow (branches and matching).

### Ripple operator

The value flowing in a chain can be 'expanded' using the `~` ('ripple') operator. This allows the value to be wrapped in a tuple:

```quiver
5 ~> [~, 1]              // [5, 1]
0 ~> Point[x: ~, y: ~]   // Point[x: 0, y: 0]
```

### Spread operator

Tuples can be extended by spreading existing tuples using the `...` operator:

```quiver
a = A[x: 1, y: 2]
a[..., y: 3]             // A[x: 1, y: 3] - preserves name, replaces y
a[..., z: 4]             // A[x: 1, y: 2, z: 4] - adds z

// Replace tuple name
[...a, y: 3]             // [x: 1, y: 3] - removes name
B[...a, y: 3]            // B[x: 1, y: 3] - sets name to B

// Multiple spreads and ordering
b = [z: 5]
[...a, ...b]             // [x: 1, y: 2, z: 5]
[w: 0, ...a]             // [w: 0, x: 1, y: 2] - prepends w

// Spread flowing value
A[x: 1] ~> [..., y: 2]   // [x: 1, y: 2] - removes name
A[x: 1] ~> ~[..., y: 2]  // A[x: 1, y: 2] - preserves name
A[x: 1] ~> B[...]        // B[x: 1] - replaces name
```

## Identifiers

Identifiers (for variables and tuple field names) start with a lowercase letter, followed by alphanumeric characters or underscores. Optional suffixes: `?`, `!` (in order).

```quiver
x; a1; first_name
is_empty?      // ? for predicates
validate!      // ! for emphasis
is_valid?!     // Combined
```

## Pattern matching

Pattern matching binds variables and tests values. Patterns can appear before a chain (`x = ...`) or within a chain (`... =x`). A match **evaluates to `Ok` if it succeeds and nil (`[]`) if it fails** — the matched value does not flow onward, but any variables the pattern binds are in scope afterwards. So an in-chain match doubles as a guard, and within a sequence a failing match short-circuits (the basis for nil-propagation). A bare binder (`=x`) always succeeds — it binds any value, including `[]` — whereas a type, literal, or structural pattern fails when it doesn't match. To keep using a matched value, reference the variable it bound: `expr =x; x ...`.

Note the spacing convention that distinguishes the two forms: `x = e` (spaces around `=`) is a binding, whereas `e =x` (`=` glued to the pattern) is an in-chain match.

### Binding

Create variable bindings:

```quiver
x = 42
p = Point[x: 10, y: 20]
p.y ~> =y
```

### Destructuring

Extract values from tuples:

```quiver
Point[x, y] = Point[10, 20]              // Bind both fields
[x: a, y: b] = Point[x: 10, y: 20]       // Rename during binding
(x, y) = Point[x: 10, y: 20, z: 30]      // Partial pattern (for named fields)
Point(x, y) = Point[x: 1, y: 2, z: 3]    // Named partial pattern
* = Config[host: "localhost", port: 80]  // Star (all named fields)
Config* = Config[host: "x", port: 80]    // Named star (all named fields, matches the name)
[x, _] = Point[10, 20]                   // Placeholder (ignore value)
```

### Literal matching

Mix literals with bindings to test and extract:

```quiver
Point[x: 0, y] = Point[0, 10]    // Succeeds if x=0, binds y to 10
Point[x: 0, y] = Point[1, 10]    // Fails (evaluates to [])

5 ~> =5                          // Literal match (Ok)
5 ~> =6                          // Fails ([])
role ~> ="admin"                 // String matching (with ~>, so this tests — bare `role =...` would bind)
[] ~> =[]                        // Nil test (Ok when the value is nil, [] otherwise)
```

### References

Use `&` to check against an existing variable, instead of binding:

```quiver
y = 2
2 ~> =&y                          // Ok (matches)
3 ~> =&y                          // [] (doesn't match)

Point[x, &y] = Point[1, 2]       // Binds x, checks y is 2
Point[1, 2] ~> =Point[x, &y]     // x bound, y pinned
A[x, B[&y, C[z]]]                // Mixed; x and z bound; y pinned
```

Type references need no `&`: because types are never bound, a type name (carrying its `'` prefix) is always a reference:

```quiver
42 ~> ='int                       // Ok
A[2] ~> =A[&y]                    // Ok
P[x: 1, y: 2] ~> =(x: 'int)      // Ok
```

Identifiers in patterns bind by default. Use `&` to reference an existing variable instead of binding; type references (`'int`, `'point`, …) are always references and need no `&`.

### Type-ascribed binding

A parenthesised type immediately followed by an identifier, `(T)x`, asserts the value's type *and* binds the whole value (at the narrowed type) to `x`. The identifier must be adjacent (no space after `)`). It composes anywhere a pattern can, including field values — so it can narrow a union variant by field type and capture the field in one step:

```quiver
42 ~> =('int)x                    // x = 42, asserted 'int
shape ~> =A[a: ('int)n]           // matches A whose field a is an int, binds n to it
[] ~> =('int)x                    // fails ([]) — nil isn't an int, so this propagates
```

This is also the idiom for "bind, but fail (propagate) on the wrong type": `find [...] ~> =('int)i` binds `i` only when the result is a non-nil int.

### Alternation

A parenthesised, `|`-separated list of patterns is an *alternation*: it matches if any alternative matches.

```quiver
[[], 5] ~> =([[], _] | [_, []])   // Ok (first element is nil)
42 ~> =('int | 'bin)              // Ok (type alternatives)
```

Every alternative must bind the same set of variables, so the body sees them whichever one matched:

```quiver
shape ~> { =(Circle[r] | Square[r]) => area_from [r] | ... }   // both bind `r`
```

Binding different variables in different alternatives is a compile error. (A parenthesised group of named fields is a [partial pattern](#destructuring), not an alternation — the two are distinguished by `:`/`,` versus `|`, exactly as for type expressions.)

## Blocks

A block is a braced expression, `{ … }`. It introduces a scope, and it is where branches (`|`) and condition-consequence matching (`=>`) live — these don't appear at the statement level. Like any chain, each branch starts from the block's parameter, so a value piped into a block is shared across all its branches:

```quiver
B[42] ~> { =A[a] => 1 | =B[b] => 2 }   // 2 - both branches test B[42]
```

### Variable scoping

Blocks create new scopes. Variables assigned within a block shadow outer variables but don't affect them; a branch's bindings are likewise local to the block.

```quiver
x = 42; { x = 5 }; x  // 42
```

### Branches

A block may contain multiple branches, separated by `|`. If a branch's sequence evaluates to nil (`[]`), execution jumps to the next branch, or, if there are no more branches, the block evaluates to nil. (This enables concise logic similar to `&&` and `||` operators or ternary expressions in other languages.)

```quiver
// If item is valid, try to process it, otherwise show error
item ~> { is_valid? ~> process | [] ~> show_error }

// Try multiple sources with fallback
value = id ~> {
  | read_cache         // try using the id to read from the cache
  | query_database     // try using the id to query the database
  | default_value      // fall back to using a default value
}
```

### Condition-consequence

A branch can use 'condition-consequence' syntax - `... => ... | ...`. If the 'condition' sequence (on the left of the `=>`) doesn't evaluate to nil (`[]`), then the 'consequence' sequence will be executed, and then execution will jump to the end of the block, taking the value of the consequence. If the condition does evaluate to nil, execution will jump to the next branch, if any; otherwise the block will evaluate to nil. The significance is that if a consequence fails (i.e., evaluates to nil), execution jumps to the end rather than to the next branch.

```quiver
value ~> {
  | =0 => "zero"
  | num.gt? [~, 0] => "positive"
  | "negative"
}
```

This allows 'guard'-style checks to be added to a condition:

```quiver
{ =Square[x] ~> num.gt? [x, 10] => "large" | "small" }
```

## Field access

Access tuple fields using dot notation:

```quiver
point.x              // Named field access
tuple.0              // Positional access
nested.outer.inner   // Chained access
```

Field access can also be used as postfix operations:

```quiver
name = data ~> .name     // Extract field in pipeline
x = coords ~> .0         // Positional access in pipeline
```

## Annotations

Annotations attach typed key/value metadata to **tuple and function values** (including
builtins) — docstrings and contracts on functions, error payloads on nil, arbitrary keys
on tuples. They are data *about* a value, invisible to the data plane: matching and
equality ignore them, and they never survive construction.

Each annotation's value type is inferred at its attach site and tracked in the value's
type as it flows. Three builtin keys are checked at attach: `:doc` (a `Str['bin]`) and
the contract keys `:pre`/`:post`, which for `#P -> R` expect `#P -> ok?` and
`#[in: P, out: R] -> ok?`. The contracts are inert in release builds but enforced in
debug builds (see [Contract enforcement](#contract-enforcement-debug-builds)).

### Attaching

`:key value` steps at the start of a block (before the first branch) attach to **the
value the braces denote** — a function literal's closure, or a chain block's result; an
annotation-only block is identity-plus-attach. Each value is an ordinary chain with nil
input, evaluated once when the closure is built (in the enclosing scope — no `$`) or when
the block completes. Attaching is copy-on-write; re-attaching a key replaces it.

```quiver
div = #['int, 'int] {
  :doc "Integer division. Fails with :error on a zero divisor."
  | =[_, 0] => [] ~> { :error DivisionByZero }
  | __integer_divide__
}
```

### Retrieving

A glued `:key` accessor yields the annotation, or nil when absent (the glue distinguishes
it from a spaced field label). Since a failing sequence short-circuits with the *same*
nil value, an `:error` payload survives to the caller — through calls — while a
recovering branch discards it with the nil it replaces.

```quiver
div [4, 0] ~> :error           // DivisionByZero — likewise via a call: `half_inc 10 ~> :error`
div:doc                        // an access, so div is not called
```

Bare retrieval compiles only where the annotation is **statically visible**. Inferred
paths (bindings, chains, block parameters, function results, generics, awaits, module
members) preserve that knowledge; explicitly *declared* types (function parameters,
receive types, `(T)x` ascription) shed it, so retrieval there — or of a key no member of
the carrier's type can carry — is a compile error rather than an unsound nil.

The **checked form** `x:('t)key` states the entry's expected shape, as `=('t)v` does for
a binding, and is exempt from both visibility rules: total on any carrier, typed
`'t | []`, with a runtime structural test wherever the rows can't vouch — an absent key
or an entry outside the shape answers nil, as a pattern fails to nil. A provably-fitting
entry elides the test, the shape acting as a pure narrowing filter.

```quiver
f:('int)count                              // the entry, if it is an int — else []
report = #[] { $:('division_error)err }    // total through a declared (row-erased) parameter
```

### Failure provenance (debug builds)

A nil **result** (of a step, block, or function) is a failure. Debug builds (`quiv run`'s
default; `--release` opts out) stamp each fresh one with an `origin` annotation — a
`Site[module: Str['bin], line: 'int, column: 'int, kind: ...]` — which propagates with
the short-circuiting nil and is shown wherever it surfaces (`[]  (match failed at
shapes.qv:12:9)`). Stamping is fresh-only (a propagating failure keeps its original site)
and positional (nil as data — an argument, a field — is never stamped). Stamps are
invisible to the type system, so types are identical across build modes: read them with a
checked retrieval (`x:((line: 'int))origin`, nil in a release build) — bare `x:origin` is
always an error. `origin` is otherwise ordinary; stamps never overwrite a program's own.

### Contract enforcement (debug builds)

Debug builds enforce a function's `:pre`/`:post` contracts as assertions. A call to a
function whose **statically visible** type carries a contract is wrapped: `:pre` is applied
to the argument before the call, `:post` to `[in: argument, out: result]` after, and a nil
verdict aborts (via `__panic__`) with an error naming the call site. The contract functions
are read from the callee value itself, so the check follows the value through variables and
module members — but a contract erased by a declared boundary (a function parameter) is no
longer visible and is not enforced, exactly as annotation retrieval is only visible there.
Release builds emit a plain call, so a correct program behaves identically in both modes.

```quiver
half = #'int {
  :pre #{ num.gt? [~, 0] }              // the argument must be positive
  :post #{ $ ~> =[in: i, out: o]; num.gt? [i, o] }   // the result is smaller
  int.div [~, 2]
}
```

## Ref creation

The `%ref` module is a single nilary function that mints a unique, opaque identifier (of type `'ref`). Unlike other standard-library modules — which import a record of functions — `%ref` *is* the function, so each evaluation yields a fresh ref. Refs support equality and pattern matching.

```quiver
tag = %ref                   // mint a ref inline
[tag, 42] ~> =[&tag, x]
x   // 42
```

To name the minting function, bind it by reference (`&`, like any function value) and call it repeatedly:

```quiver
ref = &%ref
a = ref; b = ref
a ~> =&b        // [] — two distinct refs are not equal
```

## Functions

Functions always have a single parameter and a result. The parameter is explicitly typed, and the result type is inferred. Optionally, the return type can be specified (e.g., `#'int -> 'bin { ... }`), and will be validated at compile time.

Functions are defined with `#... { ... }` syntax, where the first `...` is the type definition of the parameter, and the second `...` is the function body (a 'block'; see above).

The parameter type may be omitted, writing just `#{ ... }`. Such a literal **infers its parameter type from context** when it appears directly as a call argument (the whole argument, or a top-level field of the argument's bracket tuple) and the callee's corresponding parameter type is known. Type variables in that expected type are pinned by the sibling arguments, so the inferring literal must come *after* the arguments that determine its type:

```quiver
xs ~> map [~, #{ $0 }, Nil]   // #{ $0 } infers its parameter from map's #'t -> 'u argument
```

When no expected type is available — or it resolves to a bare, unpinned type variable — `#{ ... }` falls back to a **nil parameter**, the shorthand for a nilary function. To force a nil parameter even where a context type is available, write the parameter explicitly as `#[] { ... }`.

The function parameter can be accessed using `$` (e.g., `$.x`, `$.0`). Unlike `~`, which refers to the value flowing in the current chain, `$` always refers to the enclosing function's parameter.

A single field or index may follow `$` directly, with no dot: `$x` and `$0` are sugar for `$.x` and `$.0`.

Identity functions (that simply return their input unchanged) can be defined without a body: `#'int` is equivalent to `#'int { $ }`.

```quiver
// Single parameter function
double = #'int { num.mul [~, 2] }

// Pattern matching on union types
area = #'shape {
  | =Circle[radius: r] => num.mul [r, r]
  | =Rectangle[width: w, height: h] => num.mul [w, h]
}

// Using a tuple for multiple values
swap = #['int, 'int] { =[a, b] => [b, a] }

// Shorthand for nil parameter
#{ 42 }

// Identity function
f = #'int

// Parameter reference with $
sum = #['int, 'int] { num.add [$.0, $.1] }
```

### Function application

Application is written **function-first** by juxtaposition: a callable, a space, and a
single argument primary (a bracketed tuple or a single value):

```quiver
double 5                 // Apply double to 5
num.add [3, 4]           // Apply add to the tuple [3, 4]
num.add [1, 2] ~> num.mul [~, 3]   // Chained calls: (1+2) then (×3) -> 9
```

The head must be **applicable** — a variable, `$`, an import member (`num.add`), a builtin,
a tail call (`^`/`^f`), a ripple (`~`/`~.f`), or a spawn (`@f`). Exactly one argument is
taken: `f x y` is an error; nest through the pipeline instead (`g x ~> f`). Non-applicable
heads — literals, tuples, `&f` references, function literals, selects — cannot take an
argument.

The flowing value flows **into the argument** (each field of a bracket tuple receives its
own copy), so a juxtaposed call combines the incoming flow with an explicit argument:

```quiver
5 ~> num.add [~, 100]    // 105 — the flowing 5 fills ~ in the argument
```

A function can equally be applied by **piping** the whole value into it, with no explicit
argument — the flowing value becomes the argument:

```quiver
[3, 4] ~> num.add        // 7 — same as `num.add [3, 4]`
```

A nilary function (one taking nil) is called with nil automatically, ignoring any flowing
value; giving it an explicit argument is a type error:

```quiver
list.new                 // create a new list (any flowing value is ignored)
5 ~> list.new            // the 5 is ignored; list.new is called with nil
```

To reference a function without calling it, use `&`. Because tuple fields and call
arguments flow the surrounding value into themselves (see above), a callable used there
is *called* unless prefixed with `&`:

```quiver
&double                  // Reference to double (not called)
map [xs, &double]        // Pass double as an argument (without &, double would be called)
[add: &__integer_add__]  // A record of functions; & references a builtin without calling it
```

To apply a function that is itself the **flowing value**, use the ripple head `~`: it
consumes the flowing function and applies it to the given argument (the argument does *not*
receive the flow). `~.field` applies a function drawn from a field of the flowing value:

```quiver
&num.add ~> ~ [1, 2]     // 3 — the flowing function applied to [1, 2]
num ~> ~.add [1, 2]      // 3 — apply the num record's `add` field to [1, 2]
```

### Tail recursion

Use `^` for tail-recursive calls. Like any call, `^` takes its argument by juxtaposition — the target, then the argument:

```quiver
f = #['int, 'int] {
  | =[1, y] => y
  | =[x, y] => ^ [
    num.sub [x, 1],
    num.mul [x, y]
  ]
}
```

Named tail calls to other functions:

```quiver
f = #['int, 'int] { num.mul }
fact = #'int { ^f [~, 1] }
```

Tail calls take their argument the same way — juxtaposed after the target:

```quiver
g = #['int, 'int] { num.mul }
f = #'int { num.add [~, 1] ~> ^g [~, 2] }
10 ~> f   // 22
```

The flowing value itself can be the tail-call target, using the ripple form `^~`. Bare
`^~` hands the flowing value (which must be a nilary function) a nil argument; `^~ arg`
tail-calls the flowing function with an explicit argument:

```quiver
g = #{ 10 }              // a nilary function
f = #'int { &g ~> ^~ }   // tail-call g (the flowing value), with nil
5 ~> f                    // 10

h = #'int { num.add [~, 100] }
k = #'int { &h ~> ^~ 5 }  // tail-call h (the flowing value), with 5
0 ~> k                     // 105
```

## Processes

Quiver supports lightweight concurrent processes inspired by Erlang. Processes communicate through typed message passing.

### Spawning processes

Spawn a process by applying the `@` operator to a function:

```quiver
process = #{ ... }
processor = @process
```

Processes can be initialised with an argument, and a shorthand can be used to define the function:

```quiver
counter = @'int { ... }
```

The init argument is supplied by the value flowing into the spawn:

```quiver
p = 42 ~> @counter   // init argument from the flowing value
```

Like any juxtaposed call, the spawn takes its init argument after `@target`, and the flowing
value flows into it (`10 ~> @adder [~, 5]` spawns `adder` with `[10, 5]`). When the function
to spawn is itself the flowing value, use the ripple form `@~`: bare `@~` spawns it with a nil
init argument (`&f ~> @~`), and `@~ arg` spawns it with an explicit argument.

### Receiving messages

The select operator, `!`, can be used to receive messages by applying it to a function - for example, applying it to an identity function: `!#'int`, which can be shortened to `!'int`.

The function's parameter type defines the message type to be received. And this in turn will define the receive type of the process spawned with the surrounding function:

```quiver
// Spawn a process with an int receive type
p1 = @{
  !'int ~> {
    | =0 => "done"
    | [] ~> ^
  }
}
```

#### Handlers and filters

The select *shorthands* — `!#'int`, `!'int`, `!(...)` — are **body-less identity receives**: they only name the message type and yield the received message.

A `{ … }` written directly after a select shorthand — glued or space-separated, **not** joined with `~>` — is a **filter body** for the receive: it inspects each candidate message and decides whether to accept it. A filter follows Quiver's usual truthiness convention: if it evaluates to nil (`[]`) the message is skipped (it remains in the mailbox, to be received in future); any non-nil result accepts the message. The filter's result is only a verdict — the select always yields the *received message*, never the filter's result. If none of the messages in the mailbox match, the select waits to receive a message that does. The general form `![#T { filter }]` is an equivalent way to write a filter:

```quiver
!'int { =42 => Ok | [] }   // wait specifically for the message 42, leaving others queued
![#'int { =42 => Ok }]    // the general form — equivalent filter
```

To **handle** the received message instead — process it *after* it has been received — write the block as a separate chain step, joined with `~>`. This is an ordinary handler chain step, applied to the message the select yields (receive-then-handle):

```quiver
!#'command ~> {         // receive a command, then dispatch it
  | =Read[...] => ...
  | =Close => ...
}
```

> **The `~>` before a block at a select is load-bearing.** A block glued or spaced directly after a select (`!'int { … }`) is a **filter** — evaluated *during* the receive to accept or skip candidate messages. A block joined with `~>` (`!'int ~> { … }`) is a **handler** — an ordinary chain step that processes the message the select has already yielded.

A builtin (which has no body) is body-less, exactly like an identity function, so it just names the message type and is never applied to the message. So `![&%int.and]` receives an `['int, 'int]` message and yields it unchanged, identically to `!#['int, 'int]`.

It's important to avoid side effects in a filter, since it may be evaluated multiple times. Filters are not permitted to spawn processes, send messages or contain nested selects.

### Sending messages

Send a message to a process by applying a value to the process:

```quiver
42 ~> pid
```

### Awaiting processes

The select operator (`!`) introduced above can also be used to await the result of a process:

```quiver
p = @f
!p
```

If a process has failed with a runtime error, that error will be propagated to the awaiting process.

### Advanced select usage

As well as being used for receiving messages and awaiting the result of a single process, the select operator can specify multiple sources at once to 'race' them. And also for specifying timeouts.

The general form is `![sources]`, which takes a tuple of sources glued to the `!`, like every other select form. The tuple is an ordinary value tuple, so a *function* source must be passed by reference with `&` (a bare callable would be called); processes and timeouts are plain values and need no `&`. Sources can be:

- Processes (to await their result)
- Functions, by reference (for receiving messages) — e.g. `&f`, `&%mod.recv`
- Integers (timeouts in milliseconds)

For example, given two processes, `p1` and `p2`, the following select will wait for whichever finishes first (prioritising `p1` if both are already finished), or time out after 5 seconds:

```quiver
![p1, p2, 5000]
```

A select operator can be used in a chain by including the ripple operator (`~`) to refer to the flowing value. For example, to wait for a process, but timeout after one second:

```quiver
p1 ~> ![~, 1000]
```

Shorthand forms (each selects on a *single* source):

- `!x` is sugar for `![&x]`, for any variable or module member (`!p`, `!f`, `!%mod.recv`) — the `&` is part of the sugar, so it works inline with no binding. The `&` references the value rather than calling it: required for a function receiver, and a harmless no-op for a process (`&p` is just `p`), so the same form covers both awaiting a process and receiving on a function.
- `!'int` (also `!#'int`, `!(...)`) is sugar for `![#'int]` (body-less identity receive for a type)
- `![]` is a no-op (returns nil immediately)

A `{ … }` glued or spaced directly after a select shorthand is a **filter** body for the receive (see [Handlers and filters](#handlers-and-filters)); a block joined with `~>` is a separate [handler](#handlers-and-filters) chain step that processes the received message. The general form `![#T { filter }]` is an equivalent way to write a filter.

### Referring to processes

When spawning, a process identifier is returned. The current process can refer to itself using:
- `.` to send a message to self: `42 ~> .`
- `&.` to get a reference to self without sending: `&. ~> =self_pid`

To specify a type that refers to a process, use `@` followed by a type. For example, `@'int` is a process that receives integers.

### Resource ownership

Some built-in operations produce *resources* — opaque handles to external state such as open
files or sockets. A resource handle has a type written `\Name` (e.g. `\File`), and is an
ordinary value that can be bound, stored in tuples, and passed in messages.

Every resource is **owned by exactly one process** — initially the process that created it.
Ownership is enforced at runtime:

- Only the owning process may operate on a resource. An operation attempted by any other
  process fails with a runtime error.
- Ownership **moves** when the handle is transferred to another process — by sending it in a
  message, or by capturing it (or passing it as the spawn argument) when spawning. After a
  transfer the original owner can no longer use the handle.
- When a process terminates, any resources it still owns are **automatically closed**.

Because a handle can only be used by its owner, sharing a resource between processes is done
by keeping it in one owning process and sending that process messages requesting operations
on it. This is the pattern the standard library's `file` module follows.

## Modules and imports

Import modules using `%name` or `%namespace/name` syntax. Module names are resolved through a manifest.

Modules are evaluated at compile time, and the result (e.g., the final tuple) is the value that's imported.

```quiver
num = %num                   // Import standard library module
(add, mul) = %num             // Import specific functions
* = %num                      // Import all named exports
```

### Module types

A module's types are reached with a type-level form combining the `'` type prefix and the `%` module sigil:

- `'%mod` — the module's **default type** (its nameless definition; see [Type aliases](#type-aliases))
- `'%mod.name` — a **named type** from the module

These are ordinary type expressions, usable wherever a type can appear and taking type arguments as usual:

```quiver
count = #'%list<'int> { ... }        // the list module's default type, applied
area = #'%shapes.circle { ... }      // a named type from the shapes module
```

Inside the module that defines a type, its own default is written as a bare `'` (or `'<args>` when parameterised):

```quiver
// list.qv
'<'t> = Nil | Cons['t, ^]            // the list module's default type
head = #<'t>'<'t> { ... }            // `'<'t>` is this module's own default
```

A local name for a module type is just an ordinary type alias:

```quiver
'circle = '%shapes.circle           // a local name for a named module type
'pair<'t> = '%shapes.pair<'t>       // re-expose a parameterised type, keeping its parameter
```

A parameterised module type needs its type arguments (`'%shapes.pair<'int>`); to re-export it generically, thread the parameter through as above.

## Standard library

The following standard library modules are available:

- `io`
- `num`
- `int`
- `list`
- `ref`

## Built-in functions

Built-in functions can be accessed using double underscores, although access via the standard library should be preferred.

```quiver
sum = __integer_add__ [3, 4]                  // Built-in addition
doubled = __integer_multiply__ [x, 2]         // Built-in multiplication
```

## Examples

### Basic usage

```quiver
// Import num functions
(add, mul, sub) = %num

// Create and manipulate values
x = 10; y = 20
add [x, y] ~> mul [~, 2] ~> sub [~, 1]
```

### Working with tuples

```quiver
'point = Point[x: 'int, y: 'int]

// Define points
p0 = Point[x: 2, y: 3]
p1 = Point[...p0, x: 5]
p2 = Point[...p1, y: 4]

// Function to add points
add_points = #['point, 'point] {
  Point[
    x: %num.add [$.0.x, $.1.x],
    y: %num.add [$.0.y, $.1.y],
  ]
}

add_points [p1, p2]   // Point[x: 10, y: 7]
```

### Pattern matching

```quiver
'list<'t> = Nil | Cons['t, ^]

// Determine whether a list contains an item
contains? = #<'t>['list<'t>, 't] {
  | =[Nil, _] => []
  | =[Cons[value, _], value] => Ok
  | =[Cons[_, tail], value] => ^ [tail, value]
}

xs = Cons[1, Cons[2, Cons[3, Nil]]]
contains? [xs, 3]   // Ok
contains? [xs, 4]    // []
```

### Conditional logic

```quiver
// Clamp value to range [0, 100]
clamp = #'int {
  | %num.gt? [~, 100] => 100
  | %num.lt? [~, 0] => 0
  | $
}

150 ~> clamp   // 100
-10 ~> clamp   // 0
50 ~> clamp    // 50
```

### Module organization

```quiver
// shapes.qv
'shape =
  | Circle[radius: 'int]
  | Rectangle[width: 'int, height: 'int]

[
  bounding_box: #'shape {
    | =Circle[radius: r] => {
      x = %num.mul [r, 2]
      Rectangle[width: x, height: x]
    }
    | =Rectangle[width: w, height: h] => {
      Rectangle[width: w, height: h]
    }
  },

  is_square?: #'shape {
    =Rectangle[width: x, height: x]
  }
]
```

```quiver
// main.qv
(bounding_box, is_square?) = %shapes

circle = Circle[radius: 5]
rectangle = Rectangle[width: 10, height: 10]

circle ~> bounding_box   // Rectangle[width: 10, height: 10]
rectangle ~> is_square?  // Ok
```

### Using built-ins and field access

```quiver
// Extract and process data
person = Person[
  name: "Alan",
  date_of_birth: [
    year: 1912,
    month: June,
    day: 23
  ]
]
person.name                           // Extract name field
person ~> .date_of_birth ~> .month    // Chain field access

// Built-in operations
next_year = person.age ~> %num.add [~, 1]
```

### Concurrent processes

```quiver
// Spawn process that receives strings
pid = @{
  !#Str['bin] ~> {
    | ="" => []              // Stop on empty string
    | =s => {
      s ~> __println__      // (not implemented!)
      [] ~> ^                 // Receive another message
    }
  }
}

// Send messages
"hello" ~> pid
"bye" ~> pid
"" ~> pid                    // (stop the process)
```
