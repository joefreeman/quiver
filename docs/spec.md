# Quiver language specification

Quiver is a statically-typed functional programming language with structural typing, pattern matching, and a postfix-based syntax. Programs are composed of immutable values flowing through transformation pipelines.

## Core concepts

### Values and immutability

All values in Quiver are immutable. The language supports:

- **Integers**: `42`, `-17`, `0xff`, `0b1010`
- **Binaries**: `'0a1b2c'` (hexadecimal bytes)
- **Tuples**: Collections of values with optional names and field labels

### Postfix flow

Quiver uses postfix notation where data flows left-to-right through transformations:

```
5 ~> double ~> increment
```

### Pattern matching

Pattern matching allows destructuring values and branching based on their structure. Matches can test literal values, extract components, and control program flow.

```
type response = Success[bin] | Error[code: int];

handle_response = #response {
  | ~> =Success[content] => content
  | ~> =Error[code: 404] => default_page
  | error_page
}
```

## Types

### Basic types

- `int` - Integer values
- `bin` - Binary data (bytes)

### Tuple types

Tuples are the primary composite type. They can optionally have a name, and have zero or more fields. Tuple names start with an uppercase letter. Each field has a type, and may be named (as per identifiers; see below).

```
[]                       // Empty tuple ('nil')
Blue                     // Named, empty tuple
[int, int]               // Unnamed tuple wiht unnamed fields
[x: int, y: int]         // Named fields
Point[x: int, y: int]    // Named tuple
A[b: B[c: C[bin]]]       // Nested tuples
```

### Partial types

Partial types define structural constraints on tuples without specifying all fields. They use parentheses instead of brackets and require all fields to be named:

```
(x: int, y: int)         // Unnamed partial - matches any tuple with 'x' and 'y' integer fields
Point(x: int)            // Named partial - matches tuples named 'Point' with an 'x' integer field
()                       // Empty unnamed partial - matches any tuple
Point()                  // Empty named partial - matches any tuple named 'Point'
```

### Type aliases

```
type point = Point[x: int, y: int];
type adder = #int -> int;
type writer = (write: (#bin -> Ok));
```

### Union types

```
type shape = Circle[radius: int] | Rectangle[width: int, height: int];
type bool = True | False;
```

### Recursive types

Use `&` to refer back to the root of the types, or `&1`/`&2`/etc to refer to ancestral type boundaries (i.e., unions) from the root.

```
type list = Nil | Cons[int, &];
type tree = Leaf[int] | Node[&, &];
type json = Null | True | False | int | Str[bin] | Array[(Nil | Cons[&, &1])];
```

### Strings

The compiler converts UTF-8 strings, defined with `"..."` into binaries, wrapped in a `Str` tuple (`Str['...']`).

## Expressions

Programs are made up of multiple statements separated by new lines or semicolons. Each statement is either a type definition or an expression. An expression is a comma-separated sequence of chains.

### Chains

A chain is a `~>`-separated sequence of terms. A chain that starts with a `~>` is a 'continuation', which implicitly captures an initial value. This initial value would typically come from the surrounding block's parameter. For a chain without a continuation, the first term takes a special (but generally intuitive) meaning - for example, reading from a variable rather than writing to it.

### Control flow

Chains in a sequence are executed one at a time, unless a chain evaluates to nil (`[]`), in which case the expression short-circuits, and evaluates to nil.

```
[], 5     // single expression, evaluates to []
[]; 5     // multiple expressions, evaluates to 5
```

See 'blocks' below for further details about control flow.

### Value expansion

The value in a chain can be 'expanded' using the `~` ('ripple') operator. This allows the value to be wrapped in a tuple:

```
5 ~> [~, 1]               // [5, 1]
0 ~> Point[x: ~, y: ~]    // Point[x: 0, y: 0]

```

### Tuple updates

Tuples can be merged to update specific named fields while preserving others. When a tuple receives another tuple, the receiving tuple's named fields update the source:

```
Point[x: 1, y: 2] ~> [x: 3]         // Point[x: 3, y: 2]
Point[x: 1, y: 2] ~> [y: 10]        // Point[x: 1, y: 10]
```

Named tuples can be merged with name validation - the merge succeeds only if names match:

```
p = Point[x: 1, y: 2],
p ~> Point[x: 3]         // Point[x: 3, y: 2] (names match)
p ~> Other[x: 3]         // [] (names don't match)
```

Merging works recursively for nested tuples:

```
data = Outer[Inner[x: 1, y: 2], 3],
data ~> [[x: 5]]         // Outer[Inner[x: 5, y: 2], 3]
```

## Identifiers

Identifiers (for variables and tuple field names) start with a lowercase letter, followed by alphanumeric characters or underscores. Optional suffixes: `?`, `!`, `'` (in order).

```
x, a1, first_name
is_empty?      // ? for predicates
validate!      // ! for emphasis
helper'        // ' for variants
is_valid?!'    // Combined
```

## Pattern matching

Pattern matching binds variables and tests values. Patterns can appear before a chain (`x = ...`) or within a chain (`... ~> =x`).

### Binding

Create variable bindings:

```
x = 42
p = Point[x: 10, y: 20]
p.y ~> =y
```

### Destructuring

Extract values from tuples:

```
Point[x, y] = Point[10, 20]              // Bind both fields
[x: a, y: b] = Point[x: 10, y: 20]       // Rename during binding
(x, y) = Point[x: 10, y: 20, z: 30]      // Partial pattern (for named fields)
Point(x, y) = Point[x: 1, y: 2, z: 3]    // Named partial pattern
* = Config[host: "localhost", port: 80]  // Star (all named fields)
[x, _] = Point[10, 20]                   // Placeholder (ignore value)
```

### Literal matching

Mix literals with bindings to test and extract:

```
Point[x: 0, y] = Point[0, 10]    // Succeeds if x=0, binds y to 10
Point[x: 0, y] = Point[1, 10]    // Fails (evaluates to [])

5 ~> =x, x ~> =5                 // Bind then compare
"admin" ~> =role                 // String matching
```

### Pinning

Use `^` to check against existing variables instead of binding:

```
y = 2
2 ~> ^y                          // Ok (matches)
3 ~> ^y                          // [] (doesn't match)

Point[x, ^y] = Point[1, 2]       // Binds x, checks y is 2
Point[1, 2] ~> ^Point[=x, y]     // x bound, y pinned
A[x, ^B[y, =C[z]]]               // Mixed; x and z bound; y pinned
```

Patterns default to bind mode. Use `^` to switch to pin mode, or `=` within `^` to switch back to bind mode.

## Blocks

Blocks are specified in the form `{ ... }`. They provide variable scoping, control flow, pattern matching, and can improve readability.

### Variable scoping

Blocks create new scopes. Variables assigned within a block shadow outer variables but don't affect them.

```
x = 42, { x = 5 }, x  // 42
```

### Branches

A block may contain multiple branches, separated by `|`. If a sequence evaluates to nil (`[]`), execution jumps to the next branch, or, if there are no more branches, to the end of the block. (This enables concise logic similar to `&&` and `||` operators or ternary expressions in other languages.)

```
// If item is valid, try to process it, otherwise show error
item ~> { ~> is_valid? ~> process | [] ~> show_error }

// Try multiple sources with fallback
value = id ~> {
  | ~> read_cache         // try using the id to read from the cache
  | ~> query_database     // try using the id to query the database
  | default_value         // fall back to using a default value
}
```

### Pattern matching

Branches in a block can use 'condition-consequence' syntax - `{ ... => ... | ... }`. If the 'condition' expression (on the left of the `=>`) doesn't evaluate to nil (`[]`), then the 'consequence' expression will be executed, and then execution will jump to the end of the block, taking the value of the consequence. If the condition does evaluate to nil, execution will jump to the next branch within the block, if any; otherwise the block will evaluate to nil. The significance is that if a consequence fails (i.e., evaluateas to nil), execution jumps to the end of the block rather than to the next branch.

```
value ~> {
  | ~> =0 => "zero"
  | ~> [~, 0] ~> math.gt => "positive"
  | "negative"
}
```

This allows 'guard'-style checks to be added to a condition:

```
{ ~> =x, math.gt[x, 10] => "large" | "small" }
```

## Field access

Access tuple fields using dot notation:

```
point.x              // Named field access
tuple.0              // Positional access
nested.outer.inner   // Chained access
```

Field access can also be used as postfix operations:

```
name = data ~> .name     // Extract field in pipeline
x = coords ~> .0         // Positional access in pipeline
```

## Operators

### Equality and negation

```
[5, 5] ~> ==         // Returns 5 (all equal)
[5, 6, 5] ~> ==      // Returns [] (not equal)
[] ~> /              // Returns Ok (negation of nil)
5 ~> /               // Returns [] (negation of non-nil)
```

## Functions

Functions always have a single parameter and a result. The parameter is explicitly typed, and the result type is inferred.

Functions are defined with `#... { ... }` syntax, where the first `...` is the type definition of the parameter, and the second `...` is the function body (a 'block'; see below).

Functions taking a nil parameter can be defined with the shorthand, `#{ ... }`.

```
// Single parameter function
double = #int { ~> math.mul[~, 2] }

// Pattern matching on union types
area = #shape {
  | ~> =Circle[radius: r] => math.mul[r, r]
  | ~> =Rectangle[width: w, height: h] => math.mul[w, h]
},

// Using a tuple for multiple values
#[int, int] { ~> =[a, b] => [b, a] } ~> swap

// Shorthand for nil parameter
#{ 42 }
```

### Function application

Functions are called by applying a value to them in a chain. The type system determines whether to call the function or just reference it.

```
5 ~> double              // Apply double to 5
[3, 4] ~> math.add       // Apply add to tuple [3, 4]
[] ~> list.new           // For parameterless functions, explicitly pass []
```

Alternatively, functions can be called using shorthand syntax `f[...]` where `[...]` is an unnamed tuple. This is equivalent to `[...] ~> f`:

```
math.add[3, 4]                     // Equivalent to [3, 4] ~> math.add
math.add[1, 2] ~> math.mul[~, 3]   // Equivalent to [1, 2] ~> math.add ~> [~, 3] ~> math.mul
f[]                                // Equivalent to [] ~> f
math ~> .add[1, 2]                 // Equivalent to math.add[1, 2]
```

When used with field access (`.field[...]`), ripples are not allowed in the argument.

### Tail recursion

Use `&` for tail-recursive calls:

```
f = #[int, int] {
  | ~> =[1, y] => y
  | ~> =[x, y] => [
    =[x, 1] ~> math.sub,
    =[x, y] ~> math.mul
  ] ~> &
}
```

Named tail calls to other functions:

```
fact = #int { ~> [~, 1] ~> &f }
```

Tail calls also support the shorthand argument syntax:

```
g = #[int, int] { ~> math.mul },
f = #int { ~> math.add[~, 1] ~> &g[~, 2] },
10 ~> f   // 22
```

## Processes

Quiver supports lightweight concurrent processes inspired by Erlang. Processes communicate through typed message passing.

### Spawning processes

Spawn a process by applying the `@` operator to a function:

```
worker = #int { ... },
pid = @worker
```

### Receiving messages

Use `$type` (e.g., `$int`) to receive messages. The type after `$` defines the process's receive type.

Messages can be filtered by specifying a block - `$type { ... }` - the block will sequentially try to match messages in the mailbox, blocking until a message matches:

```
receiver = #{
  $int {
    | ~> =0 => "zero"
    | "non-zero", [] ~> &
  }
},
pid = @receiver
```

(Note that `$type ~> { ... }` would give subtly different behaviour - this would receive a message without filtering, and then evaluate the block to the received value.)

Avoid side effects when testing messages in the block, since the block may be evaluated multiple times.

### Sending messages

Send a message to a process by applying a value to the process:

```
42 ~> pid
```

### Awaiting processes

Await a process result using the `!` operator:

```
p = @f,
p ~> !
```

### Referring to processes

When spawning, a process identifier is returned. And the `.` operator can be used by the current process to refer to itself.

To specify a type that refers to a process, use `@type`. For example, `@int` is a process that receives integers.

## Modules and imports

Import modules using `%"path"` syntax. Relative imports must start with a ".". Standard library modules start with a letter.

Modules are evaluated at compile time, and the result (e.g., the final tuple) is the value that's imported.

```
math = %"math"                   // Import standard library
utils = %"./utils.qv"            // Import local file
(add, mul) = %"math"             // Import specific functions
* = %"./config.qv"               // Import all named exports
```

### Type imports

Import types from modules using patterns:

```
type (circle, rectangle) = %"./shapes.qv";  // Import specific types
type * = %"./geometry.qv";                  // Import all types
```

## Standard library

The following standard library modules are available:

- `io`
- `math`
- `list`

## Built-in functions

Built-in functions can be accessed using angle brackets, although access via the standard library should be preferred.

```
sum = [3, 4] ~> <add>               // Built-in addition
doubled = [x, 2] ~> <multiply>      // Built-in multiplication
```

## Examples

### Basic usage

```
// Import math functions
(add, mul, sub) = math,

// Create and manipulate values
x = 10, y = 20,
add[x, y] ~> mul[~, 2] ~> sub[~, 1]
```

### Working with tuples

```
type point = Point[x: int, y: int];

math = %"math",

// Define points
p0 = Point[x: 2, y: 3]
p1 = p0 ~> [x: 5],
p2 = p1 ~> [y: 4],

// Function to add points
add_points = #[point, point] {
  ~> =[a, b] => Point[
    x: math.add[a.x, b.x],
    y: math.add[a.y, b.y],
  ]
},

add_points[p1, p2]   // Point[x: 10, y: 7]
```

### Pattern matching

```
type list = Nil | Cons[int, &];

// Determine whether a list contains an item
contains? = #[list, int] {
  | ~> =[Nil, _] => []
  | ~> =[Cons[^value, _], ^value] => Ok
  | ~> =[Cons[_, tail], value] => &[tail, value]
},

xs = Cons[1, Cons[2, Cons[3, Nil]]],
contains?[xs, 3],   // Ok
contains?[xs, 4]    // []
```

### Conditional logic

```
// Clamp value to range [0, 100]
clamp = #int {
  | ~> math.gt[~, 100] => 100
  | ~> math.lt[~, 0] => 0
  | ~> =x => x
},

150 ~> clamp,   // 100
-10 ~> clamp,   // 0
50 ~> clamp    // 50
```

### Module organization

```
// shapes.qv
type shape = Circle[radius: int] | Rectangle[width: int, height: int];

math = %"math",

[
  bounding_box: #shape {
    | ~> =Circle[radius: r] => {
      x = [r, 2] ~> math.mul,
      Rectangle[width: x, height: x]
    }
    | ~> =Rectangle[width: w, height: h] => {
      Rectangle[width: w, height: h]
    }
  },

  is_square?: #shape {
    ~> =Rectangle[width: ^x, height: ^x]
  }
]
```

```
// main.qv
(bounding_box, is_square?) = %"./shapes.qv",

circle = Circle[radius: 5],
rectangle = Rectangle[width: 10, height: 10],

circle ~> bounding_box,      // Rectangle[width: 10, height: 10]
rectangle ~> is_square?      // Ok
```

### Using built-ins and field access

```
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
math = %"math",
next_year = person.age ~> math.add[~, 1],
```

### Concurrent processes

```
// Echo process that receives and prints messages
echo = #[] {
  $Str[bin] {
    | ~> ="" => []           // Stop on empty string
    | ~> =s => {
      s ~> <println>,        // (not implemented!)
      [] ~> &                // Continue receiving
    }
  }
},

// Spawn the process
pid = @echo,

// Send messages
"hello" ~> pid,
"bye" ~> pid,
"" ~> pid                    // (stop the process)
```
