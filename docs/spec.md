# Quiver language specification

Quiver is a statically-typed functional programming language with structural typing, pattern matching, and a postfix-based syntax. Programs are composed of immutable values flowing through transformation pipelines.

## Core concepts

### Values and immutability

All values in Quiver are immutable. The language supports:

- **Integers**: `42`, `-17` (decimal)
- **Binaries**: `0x0a1b2c` (hexadecimal bytes; an even number of hex digits, so `0x` alone is the empty binary)
- **Tuples**: Collections of values with optional names and field labels

### Postfix flow

Quiver uses postfix notation where data flows left-to-right through transformations:

```quiver
5 ~> double ~> increment
```

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

## Expressions

Programs are made up of multiple statements separated by new lines or semicolons. Each statement is either a type definition or an expression. An expression is a comma-separated sequence of chains.

### Chains

A chain is a `~>`-separated sequence of terms. Every chain implicitly starts with the surrounding block's parameter value. This value flows through the chain, being transformed by each term.

When a term receives a value:
- **Callable terms** (functions, processes) are called with the value
- **Literals and tuples** replace the value (discarding it)
- **Variables** depend on their type: callable variables are called, others replace the value

To explicitly control this behavior:
- `f []` calls `f` with nil, discarding any incoming value
- `&f` references `f` without calling it

The flowing value is also passed into the fields of a tuple that is constructed in the
chain, and into the arguments of a call. Each field/argument receives its own copy, so a
callable there is called with it (and a non-callable value simply replaces it):

```quiver
inc = #'int { math.add [~, 1] },
5 ~> [inc, 100]          // [6, 100] - inc is called with 5
5 ~> [&inc, 100]         // [<function>, 100] - & passes inc by value
```

### Control flow

Chains in a sequence are executed one at a time, unless a chain evaluates to nil (`[]`), in which case the expression short-circuits, and evaluates to nil.

```quiver
[], 5     // one statement, evaluates to []
[]; 5     // two statements, evaluates to 5
```

See [Blocks](#blocks) below for further control flow (branches and matching).

### Ripple operator

The value in a chain can be 'expanded' using the `~` ('ripple') operator. This allows the value to be wrapped in a tuple:

```quiver
5 ~> [~, 1]               // [5, 1]
0 ~> Point[x: ~, y: ~]    // Point[x: 0, y: 0]
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

// Spread chained value
A[x: 1] ~> [..., y: 2]   // [x: 1, y: 2] - removes name
A[x: 1] ~> ~[..., y: 2]  // A[x: 1, y: 2] - preserves name
A[x: 1] ~> B[...]        // B[x: 1] - replaces name
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

Pattern matching binds variables and tests values. Patterns can appear before a chain (`x = ...`) or within a chain (`... ~> =x`). The expression evaluates to the value being matched, or nil (`[]`) if the expression doesn't match.

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

5 ~> =x ~> =5                    // Bind then compare
role ~> ="admin"                 // String matching
```

### References

Use `&` to check against an existing variable, instead of binding:

```quiver
y = 2
2 ~> =&y                          // 2 (matches)
3 ~> =&y                          // [] (doesn't match)

Point[x, &y] = Point[1, 2]       // Binds x, checks y is 2
Point[1, 2] ~> =Point[x, &y]     // x bound, y pinned
A[x, B[&y, C[z]]]                // Mixed; x and z bound; y pinned
```

Type references need no `&`: because types are never bound, a type name (carrying its `'` prefix) is always a reference:

```quiver
42 ~> ='int                       // 42
A[2] ~> =A[&y]                   // A[2]
P[x: 1, y: 2] ~> =(x: 'int)      // P[x: 1, y: 2]
```

Identifiers in patterns bind by default. Use `&` to reference an existing variable instead of binding; type references (`'int`, `'point`, …) are always references and need no `&`.

## Blocks

A block is a braced expression, `{ … }`. It introduces a scope, and it is where branches (`|`) and condition-consequence matching (`=>`) live — these don't appear at the statement level. Like any chain, each branch starts from the block's parameter, so a value piped into a block is shared across all its branches:

```quiver
B[42] ~> { =A[a] => 1 | =B[b] => 2 }   // 2 - both branches test B[42]
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
item ~> { is_valid? ~> process | show_error [] }

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
  | [~, 0] ~> math.gt => "positive"
  | "negative"
}
```

This allows 'guard'-style checks to be added to a condition:

```quiver
{ =Square[x] ~> math.gt [x, 10] => "large" | "small" }
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

## Operators

### Equality and negation

```quiver
[5, 5] ~> ==         // Returns 5 (all equal)
[5, 6, 5] ~> ==      // Returns [] (not equal)
[] ~> /              // Returns Ok (negation of nil)
5 ~> /               // Returns [] (negation of non-nil)
```

### Ref creation

Standalone `&` creates a unique, opaque identifier. Refs support equality and pattern matching.

```quiver
tag = &,
[tag, 42] ~> =[&tag, x],
x   // 42
```

## Functions

Functions always have a single parameter and a result. The parameter is explicitly typed, and the result type is inferred. Optionally, the return type can be specified (e.g., `#'int -> 'bin { ... }`), and will be validated at compile time.

Functions are defined with `#... { ... }` syntax, where the first `...` is the type definition of the parameter, and the second `...` is the function body (a 'block'; see above).

Functions taking a nil parameter can be defined with the shorthand, `#{ ... }`.

The function parameter can be accessed using `$` (e.g., `$.x`, `$.0`). Unlike `~>`, which refers to a block's parameter, `$` always refers to the enclosing function's parameter.

A single field or index may follow `$` directly, with no dot: `$x` and `$0` are sugar for `$.x` and `$.0`.

Identity functions (that simply return their input unchanged) can be defined without a body: `#'int` is equivalent to `#'int { $ }`.

```quiver
// Single parameter function
double = #'int { math.mul [~, 2] }

// Pattern matching on union types
area = #'shape {
  | =Circle[radius: r] => math.mul [r, r]
  | =Rectangle[width: w, height: h] => math.mul [w, h]
}

// Using a tuple for multiple values
swap = #['int, 'int] { =[a, b] => [b, a] }

// Shorthand for nil parameter
#{ 42 }

// Identity function
f = #'int

// Parameter reference with $
sum = #['int, 'int] { [$.0, $.1] ~> math.add }
```

### Function application

Functions are called when a value is applied to them in a chain:

```quiver
5 ~> double              // Apply double to 5
[3, 4] ~> math.add       // Apply add to tuple [3, 4]
```

To call a function with nil, or to discard an incoming value, use explicit argument syntax:

```quiver
f []                      // Call f with nil
list.new []               // Call list.new with nil
```

To reference a function without calling it, use `&`. Because tuple fields and call
arguments flow the surrounding value into themselves (see below), a callable used there
is *called* unless prefixed with `&`:

```quiver
&double                  // Reference to double (not called)
map [xs, &double]        // Pass double as an argument (without &, double would be called)
[add: &__add__]          // A record of functions; & references a builtin without calling it
```

A function is applied to an argument by writing the argument after it, separated by a
space — either a bracketed tuple or a single value:

```quiver
math.add [3, 4]                      // Apply add to the tuple [3, 4]
double 5                             // Apply double to the value 5
math.add [1, 2] ~> math.mul [~, 3]   // Chained calls
&math.add ~> ~ [1, 2]                // The flowing value is a function; apply it to [1, 2]
math ~> ~.add [1, 2]                 // Read `.add` off the flowing value, then apply
```

### Tail recursion

Use `^` for tail-recursive calls:

```quiver
f = #['int, 'int] {
  | =[1, y] => y
  | =[x, y] => [
    [~, 1] ~> math.sub,
    math.mul
  ] ~> ^
}
```

Named tail calls to other functions:

```quiver
f = #['int, 'int] { math.mul },
fact = #'int { [~, 1] ~> ^f }
```

Tail calls also take an argument, using the same space-separated syntax:

```quiver
g = #['int, 'int] { math.mul },
f = #'int { math.add [~, 1] ~> ^g [~, 2] },
10 ~> f   // 22
```

The flowing value itself can be the tail-call target, using the ripple form `^~`. Since the
flowing value is then the function, the argument is supplied by juxtaposition:

```quiver
g = #'int { [~, 2] ~> math.mul },
f = #'int { &g ~> ^~ $ },   // tail-call g (the flowing value) with f's parameter
5 ~> f                      // 10
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

The init argument can be supplied either by the chained value (`x ~> @f`) or by juxtaposition, exactly like a function call. These are equivalent:

```quiver
p = 42 ~> @counter   // init argument from the chained value
p = @counter 42      // init argument by juxtaposition
```

As with a call argument, the chained value flows into a juxtaposed argument (`10 ~> @adder [~, 5]` spawns `adder` with `[10, 5]`). When the function is the chained value itself, use the ripple form `@~`, supplying the argument by juxtaposition: `&f ~> @~ 42`.

### Receiving messages

The select operator, `!`, can be used to receive messages by applying it to a function - for example, applying it to an identity function: `!#'int`, which can be shortened to `!'int`.

The function's parameter type defines the message type to be received. And this in turn will define the receive type of the process spawned with the surrounding function:

```quiver
// Spawn a process with an int receive type
p1 = @{
  !'int ~> {
    | =0 => "done"
    | ^ []
  }
}
```

#### Filtering messages

The example above uses an identity function to specify the receive type. Alternatively a body can be specified to filter messages in the process's mailbox. When a body is specified, the function must return either nil (`[]`) or `Ok`. If the function evaluates to nil, the message will be skipped, but remain in the mailbox (to be received in future). If none of the messages in the mailbox match, the select will wait to receive a message that does match.

It's important to avoid side effects in the receive function, since the block may be evaluated multiple times. Receive functions are not permitted to spawn processes, send messages or contain nested selects.

### Sending messages

Send a message to a process by applying a value to the process:

```quiver
42 ~> pid
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

The general form is `! [sources]`, which takes a tuple of sources. **A space is required** between `!` and the tuple (mirroring function application, `f [...]`); this distinguishes the general form from the single-source shorthands below. Sources can be:

- Processes (to await their result)
- Functions (for receiving messages)
- Integers (timeouts in milliseconds)

For example, given two processes, `p1` and `p2`, the following select will wait for whichever finishes first (prioritising `p1` if both are already finished), or time out after 5 seconds:

```quiver
! [p1, p2, 5000]
```

A select operator can be used in a chain by including the ripple operator (`~`) to refer to the chained value. For example, to wait for a process, but timeout after one second:

```quiver
p1 ~> ! [~, 1000]
```

Shorthand forms (tight, no space — each selects on a *single* source):

- `!p` is sugar for `! [p]`
- `!'int` is sugar for `! [#'int]` (identity receive for type)
- `!'int { ... }` is sugar for `! [#'int { ... }]` (filtered receive)
- `! []` is a no-op (returns nil immediately)

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
math = %math                   // Import standard library module
(add, mul) = %math             // Import specific functions
* = %math                      // Import all named exports
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
- `math`
- `list`

## Built-in functions

Built-in functions can be accessed using double underscores, although access via the standard library should be preferred.

```quiver
sum = [3, 4] ~> __add__               // Built-in addition
doubled = [x, 2] ~> __multiply__      // Built-in multiplication
```

## Examples

### Basic usage

```quiver
// Import math functions
(add, mul, sub) = math,

// Create and manipulate values
x = 10, y = 20,
add [x, y] ~> mul [~, 2] ~> sub [~, 1]
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
    x: %math.add [$.0.x, $.1.x],
    y: %math.add [$.0.y, $.1.y],
  ]
},

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
},

xs = Cons[1, Cons[2, Cons[3, Nil]]],
contains? [xs, 3],   // Ok
contains? [xs, 4]    // []
```

### Conditional logic

```quiver
// Clamp value to range [0, 100]
clamp = #'int {
  | %math.gt [~, 100] => 100
  | %math.lt [~, 0] => 0
  | $
},

150 ~> clamp,   // 100
-10 ~> clamp,   // 0
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
      x = %math.mul [r, 2],
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

circle ~> bounding_box,      // Rectangle[width: 10, height: 10]
rectangle ~> is_square?      // Ok
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
person ~> .date_of_birth ~> .month,    // Chain field access

// Built-in operations
next_year = person.age ~> %math.add [~, 1],
```

### Concurrent processes

```quiver
// Spawn process that receives strings
pid = @{
  !Str['bin] ~> {
    | ="" => []              // Stop on empty string
    | =s => {
      s ~> __println__,      // (not implemented!)
      ^ []                    // Receive another message
    }
  }
},

// Send messages
"hello" ~> pid,
"bye" ~> pid,
"" ~> pid                    // (stop the process)
```
