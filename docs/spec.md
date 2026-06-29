# Quiver language specification

Quiver is a statically-typed functional programming language with structural typing, pattern matching, and a whitespace-driven, argument-first syntax. Programs are composed of immutable values flowing left-to-right through transformation pipelines.

## Core concepts

### Values and immutability

All values in Quiver are immutable. The language supports:

- **Integers**: `42`, `-17` (decimal)
- **Binaries**: `0x0a1b2c` (hexadecimal bytes; an even number of hex digits, so `0x` alone is the empty binary)
- **Tuples**: Collections of values with optional names and field labels

### Value flow

A value flows left-to-right through a whitespace-separated sequence of transformations. Terms are written one after another; the value produced by one becomes the input to the next:

```quiver
5 double increment
```

Application is **argument-first**: a callable consumes the value flowing into it from the left, so the argument is written *before* the function (`5 double`, `[3, 4] num.add`). The optional `~>` marker is an explicit synonym for the space — `5 ~> double` is identical to `5 double` — and also serves as a line continuation (see [Chains](#chains)).

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
name = "world",
"hello {name}"               // "hello world"
"world" "hello, {~}"         // "hello, world" — the chained value flows into the hole
"sum: {[a, b] %str.concat}"  // any expression that yields a Str
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

A whole program is a single **sequence**: a series of **steps** separated by a comma or a newline (the two are synonyms). Type-alias declarations may be interspersed between steps and are transparent to the flow. Each step is a [chain](#chains).

A sequence **threads** and is **fallible**: each step starts from the previous step's result, and if a step evaluates to nil (`[]`) the rest of the sequence short-circuits and the whole sequence evaluates to nil. Variable bindings persist across steps. (`;` is not a separator — it does not exist in the syntax.)

### Chains

A chain is a whitespace-separated sequence of terms — the basic unit of left-to-right flow. The first term starts from the chain's input (the previous step's result, or, for the first step of a block, the block's parameter); each subsequent term transforms the flowing value. A chain is an **infallible pipe**: nil flows through it like any other value (no short-circuit *within* a chain — only a comma/newline step boundary short-circuits on nil). So the two-axis model is: *chain = infallible pipe (nil flows), sequence = fallible pipe (nil short-circuits)*, and `~` always names "the value to the left."

A bare newline ends a chain (starting a new sequence step). To continue one chain across several lines, begin the continuation line with `~>`, which is also the explicit, optional synonym for the space between terms:

```quiver
foo
~> bar
~> baz
```

When a term receives a value:
- **Callable terms** (functions, processes) are called with the value — unless the callable is **nilary** (its parameter is nil), in which case it ignores the flowing value and is called with nil, like a literal
- **Literals and tuples** replace the value (discarding it)
- **Variables** depend on their type: callable variables are called, others replace the value

So a nilary `f` needs no explicit argument: `f` and `5 f` both call it with nil.

To explicitly control this behavior:
- `&f` references `f` without calling it

The flowing value is also passed into the fields of a tuple that is constructed in the
chain, and into the arguments of a call. Each field/argument receives its own copy, so a
callable there is called with it (and a non-callable value simply replaces it):

```quiver
inc = #'int { [~, 1] num.add },
5 [inc, 100]            // [6, 100] - inc is called with 5
5 [&inc, 100]           // [<function>, 100] - & passes inc by value
```

### Control flow

Steps in a sequence are executed one at a time. If a step evaluates to nil (`[]`), the sequence short-circuits and evaluates to nil. Since comma and newline are synonyms, the same holds across lines:

```quiver
[] 5      // one step (a chain) — nil flows through, evaluates to 5
[], 5     // two steps — the first is nil, so the sequence short-circuits to []
```

See [Blocks](#blocks) below for further control flow (branches and matching).

### Ripple operator

The value flowing in a chain can be 'expanded' using the `~` ('ripple') operator. This allows the value to be wrapped in a tuple:

```quiver
5 [~, 1]                 // [5, 1]
0 Point[x: ~, y: ~]      // Point[x: 0, y: 0]
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
A[x: 1] [..., y: 2]      // [x: 1, y: 2] - removes name
A[x: 1] ~[..., y: 2]     // A[x: 1, y: 2] - preserves name
A[x: 1] B[...]           // B[x: 1] - replaces name
```

## Identifiers

Identifiers (for variables and tuple field names) start with a lowercase letter, followed by alphanumeric characters or underscores. Optional suffixes: `?`, `!` (in order).

```quiver
x, a1, first_name
is_empty?      // ? for predicates
validate!      // ! for emphasis
is_valid?!     // Combined
```

## Pattern matching

Pattern matching binds variables and tests values. Patterns can appear before a chain (`x = ...`) or within a chain (`... =x`). A match **evaluates to `Ok` if it succeeds and nil (`[]`) if it fails** — the matched value does not flow onward, but any variables the pattern binds are in scope afterwards. So an in-chain match doubles as a guard, and within a sequence a failing match short-circuits (the basis for nil-propagation). A bare binder (`=x`) always succeeds — it binds any value, including `[]` — whereas a type, literal, or structural pattern fails when it doesn't match. To keep using a matched value, reference the variable it bound: `expr =x, x ...`.

Note the spacing convention that distinguishes the two forms: `x = e` (spaces around `=`) is a binding, whereas `e =x` (`=` glued to the pattern) is an in-chain match.

### Binding

Create variable bindings:

```quiver
x = 42
p = Point[x: 10, y: 20]
p.y =y
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

5 =5                             // Literal match (Ok)
5 =6                             // Fails ([])
role ="admin"                    // String matching
[] =[]                           // Nil test (Ok when the value is nil, [] otherwise)
```

### References

Use `&` to check against an existing variable, instead of binding:

```quiver
y = 2
2 =&y                             // Ok (matches)
3 =&y                             // [] (doesn't match)

Point[x, &y] = Point[1, 2]       // Binds x, checks y is 2
Point[1, 2] =Point[x, &y]        // x bound, y pinned
A[x, B[&y, C[z]]]                // Mixed; x and z bound; y pinned
```

Type references need no `&`: because types are never bound, a type name (carrying its `'` prefix) is always a reference:

```quiver
42 ='int                          // Ok
A[2] =A[&y]                       // Ok
P[x: 1, y: 2] =(x: 'int)         // Ok
```

Identifiers in patterns bind by default. Use `&` to reference an existing variable instead of binding; type references (`'int`, `'point`, …) are always references and need no `&`.

### Type-ascribed binding

A parenthesised type immediately followed by an identifier, `(T)x`, asserts the value's type *and* binds the whole value (at the narrowed type) to `x`. The identifier must be adjacent (no space after `)`). It composes anywhere a pattern can, including field values — so it can narrow a union variant by field type and capture the field in one step:

```quiver
42 =('int)x                       // x = 42, asserted 'int
shape =A[a: ('int)n]              // matches A whose field a is an int, binds n to it
[] =('int)x                       // fails ([]) — nil isn't an int, so this propagates
```

This is also the idiom for "bind, but fail (propagate) on the wrong type": `[...] find =('int)i` binds `i` only when the result is a non-nil int.

### Alternation

A parenthesised, `|`-separated list of patterns is an *alternation*: it matches if any alternative matches.

```quiver
[[], 5] =([[], _] | [_, []])      // Ok (first element is nil)
42 =('int | 'bin)                 // Ok (type alternatives)
```

Every alternative must bind the same set of variables, so the body sees them whichever one matched:

```quiver
shape { =(Circle[r] | Square[r]) => [r] area_from | ... }   // both bind `r`
```

Binding different variables in different alternatives is a compile error. (A parenthesised group of named fields is a [partial pattern](#destructuring), not an alternation — the two are distinguished by `:`/`,` versus `|`, exactly as for type expressions.)

## Blocks

A block is a braced expression, `{ … }`. It introduces a scope, and it is where branches (`|`) and condition-consequence matching (`=>`) live — these don't appear at the statement level. Like any chain, each branch starts from the block's parameter, so a value piped into a block is shared across all its branches:

```quiver
B[42] { =A[a] => 1 | =B[b] => 2 }   // 2 - both branches test B[42]
```

### Variable scoping

Blocks create new scopes. Variables assigned within a block shadow outer variables but don't affect them; a branch's bindings are likewise local to the block.

```quiver
x = 42, { x = 5 }, x  // 42
```

### Branches

A block may contain multiple branches, separated by `|`. If a branch's sequence evaluates to nil (`[]`), execution jumps to the next branch, or, if there are no more branches, the block evaluates to nil. (This enables concise logic similar to `&&` and `||` operators or ternary expressions in other languages.)

```quiver
// If item is valid, try to process it, otherwise show error
item { is_valid? process | [] show_error }

// Try multiple sources with fallback
value = id {
  | read_cache         // try using the id to read from the cache
  | query_database     // try using the id to query the database
  | default_value      // fall back to using a default value
}
```

### Condition-consequence

A branch can use 'condition-consequence' syntax - `... => ... | ...`. If the 'condition' sequence (on the left of the `=>`) doesn't evaluate to nil (`[]`), then the 'consequence' sequence will be executed, and then execution will jump to the end of the block, taking the value of the consequence. If the condition does evaluate to nil, execution will jump to the next branch, if any; otherwise the block will evaluate to nil. The significance is that if a consequence fails (i.e., evaluates to nil), execution jumps to the end rather than to the next branch.

```quiver
value {
  | =0 => "zero"
  | [~, 0] num.gt? => "positive"
  | "negative"
}
```

This allows 'guard'-style checks to be added to a condition:

```quiver
{ =Square[x] [x, 10] num.gt? => "large" | "small" }
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
name = data .name        // Extract field in pipeline
x = coords .0            // Positional access in pipeline
```

## Ref creation

The `%ref` module is a single nilary function that mints a unique, opaque identifier (of type `'ref`). Unlike other standard-library modules — which import a record of functions — `%ref` *is* the function, so each evaluation yields a fresh ref. Refs support equality and pattern matching.

```quiver
tag = %ref,                   // mint a ref inline
[tag, 42] =[&tag, x],
x   // 42
```

To name the minting function, bind it by reference (`&`, like any function value) and call it repeatedly:

```quiver
ref = &%ref,
a = ref, b = ref,
a =&b           // [] — two distinct refs are not equal
```

## Functions

Functions always have a single parameter and a result. The parameter is explicitly typed, and the result type is inferred. Optionally, the return type can be specified (e.g., `#'int -> 'bin { ... }`), and will be validated at compile time.

Functions are defined with `#... { ... }` syntax, where the first `...` is the type definition of the parameter, and the second `...` is the function body (a 'block'; see above).

The parameter type may be omitted, writing just `#{ ... }`. Such a literal **infers its parameter type from context** when it appears directly as a call argument (the whole argument, or a top-level field of the argument's bracket tuple) and the callee's corresponding parameter type is known. Type variables in that expected type are pinned by the sibling arguments, so the inferring literal must come *after* the arguments that determine its type:

```quiver
xs [~, #{ $0 }, Nil] map   // #{ $0 } infers its parameter from map's #'t -> 'u argument
```

When no expected type is available — or it resolves to a bare, unpinned type variable — `#{ ... }` falls back to a **nil parameter**, the shorthand for a nilary function. To force a nil parameter even where a context type is available, write the parameter explicitly as `#[] { ... }`.

The function parameter can be accessed using `$` (e.g., `$.x`, `$.0`). Unlike `~`, which refers to the value flowing in the current chain, `$` always refers to the enclosing function's parameter.

A single field or index may follow `$` directly, with no dot: `$x` and `$0` are sugar for `$.x` and `$.0`.

Identity functions (that simply return their input unchanged) can be defined without a body: `#'int` is equivalent to `#'int { $ }`.

```quiver
// Single parameter function
double = #'int { [~, 2] num.mul }

// Pattern matching on union types
area = #'shape {
  | =Circle[radius: r] => [r, r] num.mul
  | =Rectangle[width: w, height: h] => [w, h] num.mul
}

// Using a tuple for multiple values
swap = #['int, 'int] { =[a, b] => [b, a] }

// Shorthand for nil parameter
#{ 42 }

// Identity function
f = #'int

// Parameter reference with $
sum = #['int, 'int] { [$.0, $.1] num.add }
```

### Function application

Application is **argument-first**: a function is applied to the value flowing into it from the left. Build the argument first — a bracketed tuple or a single value — then name the function:

```quiver
5 double                 // Apply double to 5
[3, 4] num.add           // Apply add to the tuple [3, 4]
[1, 2] num.add [~, 3] num.mul   // Chained calls: (1+2) then (×3) -> 9
```

A nilary function (one taking nil) is called with nil automatically, ignoring any flowing value:

```quiver
list.new                  // create a new list (any flowing value is ignored)
5 list.new                // the 5 is ignored; list.new is called with nil
```

To reference a function without calling it, use `&`. Because tuple fields and call
arguments flow the surrounding value into themselves (see above), a callable used there
is *called* unless prefixed with `&`:

```quiver
&double                  // Reference to double (not called)
[xs, &double] map        // Pass double as an argument (without &, double would be called)
[add: &__integer_add__]  // A record of functions; & references a builtin without calling it
```

There is no juxtaposition (`f x` / `f [args]`) and no bare ripple application. To apply a
function that is itself the flowing value, bind it to a name first, then apply argument-first:

```quiver
f = &num.add, [1, 2] f   // 3 — apply the bound function to [1, 2]
```

### Tail recursion

Use `^` for tail-recursive calls. Like any call it is argument-first — build the argument tuple, then `^`:

```quiver
f = #['int, 'int] {
  | =[1, y] => y
  | =[x, y] => [
    [x, 1] num.sub,
    [x, y] num.mul
  ] ^
}
```

Named tail calls to other functions:

```quiver
f = #['int, 'int] { num.mul },
fact = #'int { [~, 1] ^f }
```

Tail calls take their argument the same way — written before the target:

```quiver
g = #['int, 'int] { num.mul },
f = #'int { [~, 1] num.add [~, 2] ^g },
10 f   // 22
```

The flowing value itself can be the tail-call target, using the ripple form `^~`. This form
is **bare** — it hands the flowing value (which must be a nilary function) a nil argument:

```quiver
g = #{ 10 },              // a nilary function
f = #'int { &g ^~ },      // tail-call g (the flowing value), with nil
5 f                       // 10
```

## Processes

Quiver supports lightweight concurrent processes inspired by Erlang. Processes communicate through typed message passing.

### Spawning processes

Spawn a process by applying the `@` operator to a function:

```quiver
process = #{ ... },
processor = @process
```

Processes can be initialised with an argument, and a shorthand can be used to define the function:

```quiver
counter = @'int { ... }
```

The init argument is supplied by the value flowing into the spawn:

```quiver
p = 42 @counter   // init argument from the flowing value
```

Like any argument-first call, build the argument first; the flowing value flows into it
(`10 [~, 5] @adder` spawns `adder` with `[10, 5]`). When the function to spawn is itself the
flowing value, use the **bare** ripple form `@~`, which spawns it with a nil init argument:
`&f @~`.

### Receiving messages

The select operator, `!`, can be used to receive messages by applying it to a function - for example, applying it to an identity function: `!#'int`, which can be shortened to `!'int`.

The function's parameter type defines the message type to be received. And this in turn will define the receive type of the process spawned with the surrounding function:

```quiver
// Spawn a process with an int receive type
p1 = @{
  !'int {
    | =0 => "done"
    | [] ^
  }
}
```

#### Handlers and filters

The select *shorthands* — `!#'int`, `!'int`, `!(...)` — are **body-less identity receives**: they only name the message type and yield the received message. A `{ … }` written *after* a select is therefore an ordinary chain step — a **handler** — that processes the received message (receive-then-handle):

```quiver
!#'command {            // receive a command, then dispatch it
  | =Read[...] => ...
  | =Close => ...
}
```

To **filter** the mailbox instead — leaving non-matching messages in place to be received later — use the **general form** with a filter body, `! [#T { filter }]`. A filter follows Quiver's usual truthiness convention: if it evaluates to nil (`[]`) the message is skipped (it remains in the mailbox, to be received in future); any non-nil result accepts the message. The filter's result is only a verdict — the select always yields the received message, never the filter's result. If none of the messages in the mailbox match, the select waits to receive a message that does:

```quiver
! [#'int { =42 => Ok }]   // wait specifically for the message 42, leaving others queued
```

A builtin (which has no body) is body-less, exactly like an identity function, so it just names the message type and is never applied to the message. So `! [&%int.and]` receives an `['int, 'int]` message and yields it unchanged, identically to `!#['int, 'int]`.

It's important to avoid side effects in a filter, since it may be evaluated multiple times. Filters are not permitted to spawn processes, send messages or contain nested selects.

### Sending messages

Send a message to a process by applying a value to the process:

```quiver
42 pid
```

### Awaiting processes

The select operator (`!`) introduced above can also be used to await the result of a process:

```quiver
p = @f,
!p
```

If a process has failed with a runtime error, that error will be propagated to the awaiting process.

### Advanced select usage

As well as being used for receiving messages and awaiting the result of a single process, the select operator can specify multiple sources at once to 'race' them. And also for specifying timeouts.

The general form is `! [sources]`, which takes a tuple of sources. **A space is required** between `!` and the tuple; this distinguishes the general form from the tight single-source shorthands below. The tuple is an ordinary value tuple, so a *function* source must be passed by reference with `&` (a bare callable would be called); processes and timeouts are plain values and need no `&`. Sources can be:

- Processes (to await their result)
- Functions, by reference (for receiving messages) — e.g. `&f`, `&%mod.recv`
- Integers (timeouts in milliseconds)

For example, given two processes, `p1` and `p2`, the following select will wait for whichever finishes first (prioritising `p1` if both are already finished), or time out after 5 seconds:

```quiver
! [p1, p2, 5000]
```

A select operator can be used in a chain by including the ripple operator (`~`) to refer to the flowing value. For example, to wait for a process, but timeout after one second:

```quiver
p1 ! [~, 1000]
```

Shorthand forms (tight, no space — each selects on a *single* source):

- `!x` is sugar for `! [&x]`, for any variable or module member (`!p`, `!f`, `!%mod.recv`) — the `&` is part of the sugar, so it works inline with no binding. The `&` references the value rather than calling it: required for a function receiver, and a harmless no-op for a process (`&p` is just `p`), so the same form covers both awaiting a process and receiving on a function.
- `!'int` (also `!#'int`, `!(...)`) is sugar for `! [#'int]` (body-less identity receive for a type)
- `! []` is a no-op (returns nil immediately)

A `{ … }` following a select is *not* part of these shorthands — it is a separate chain step (a [handler](#handlers-and-filters)) that processes the received message. To filter the mailbox, use the general form `! [#T { filter }]`.

### Referring to processes

When spawning, a process identifier is returned. The current process can refer to itself using:
- `.` to send a message to self: `42 .`
- `&.` to get a reference to self without sending: `&. =self_pid`

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
sum = [3, 4] __integer_add__                  // Built-in addition
doubled = [x, 2] __integer_multiply__         // Built-in multiplication
```

## Examples

### Basic usage

```quiver
// Import num functions
(add, mul, sub) = %num,

// Create and manipulate values
x = 10, y = 20,
[x, y] add [~, 2] mul [~, 1] sub
```

### Working with tuples

```quiver
'point = Point[x: 'int, y: 'int]

// Define points
p0 = Point[x: 2, y: 3],
p1 = Point[...p0, x: 5],
p2 = Point[...p1, y: 4],

// Function to add points
add_points = #['point, 'point] {
  Point[
    x: [$.0.x, $.1.x] %num.add,
    y: [$.0.y, $.1.y] %num.add,
  ]
},

[p1, p2] add_points   // Point[x: 10, y: 7]
```

### Pattern matching

```quiver
'list<'t> = Nil | Cons['t, ^]

// Determine whether a list contains an item
contains? = #<'t>['list<'t>, 't] {
  | =[Nil, _] => []
  | =[Cons[value, _], value] => Ok
  | =[Cons[_, tail], value] => [tail, value] ^
},

xs = Cons[1, Cons[2, Cons[3, Nil]]],
[xs, 3] contains?,   // Ok
[xs, 4] contains?    // []
```

### Conditional logic

```quiver
// Clamp value to range [0, 100]
clamp = #'int {
  | [~, 100] %num.gt? => 100
  | [~, 0] %num.lt? => 0
  | $
},

150 clamp,   // 100
-10 clamp,   // 0
50 clamp    // 50
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
      x = [r, 2] %num.mul,
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
(bounding_box, is_square?) = %shapes,

circle = Circle[radius: 5],
rectangle = Rectangle[width: 10, height: 10],

circle bounding_box,      // Rectangle[width: 10, height: 10]
rectangle is_square?      // Ok
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
],
person.name,                           // Extract name field
person .date_of_birth .month,          // Chain field access

// Built-in operations
next_year = person.age [~, 1] %num.add,
```

### Concurrent processes

```quiver
// Spawn process that receives strings
pid = @{
  !#Str['bin] {
    | ="" => []              // Stop on empty string
    | =s => {
      s __println__,         // (not implemented!)
      [] ^                    // Receive another message
    }
  }
},

// Send messages
"hello" pid,
"bye" pid,
"" pid                       // (stop the process)
```
