# Quiver language specification

Quiver is a statically-typed functional programming language with structural typing, pattern matching, and a postfix-based syntax. Programs are composed of immutable values flowing through transformation pipelines.

## Core concepts

### Values and immutability

All values in Quiver are immutable. The language supports:

- **Integers**: `42`, `-17`
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

### Type aliases

```
type point = Point[x: int, y: int];
type adder = #int -> int;
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

## Assignment

Assignment is used to assign to variables and/or test a value against a pattern.

Assignment can be done by prefixing the chain with an assignmment - `a = ...` - or with the assignment term - `... ~> =a` within a chain.

```
x = 42                     // Assign 42 to new variable x
x = 1 ~> [~, 2]            // Assign x to the result of a chain
p = Point[x: 10, y: 20]    // Assign tuple to variable p
p.y ~> =y                  // Using the assignment term
```

### Identifiers

Identifiers (for variables and tuple field names) start with a lowercase letter, followed by zero or more alphanumeric characters or underscores.

They can also be suffixed by an optional question mark, an optional exclamation mark, and multiple quote characters (in that order).

```
x, a1, first_name
is_empty?      // ? suffix for predicates/boolean functions
validate!      // ! suffix for emphasis
helper'        // ' suffix for variants or helper functions
is_valid?!     // ? and ! can be combined
valid?!''      // Multiple suffixes can be combined
```

### Destructuring

Extract values from tuples during assignment.

```
// Full destructuring
Point[x: a, y: _] = Point[x: 10, y: 20]

// Partial destructuring (extract specific fields)
(g, b) = Color[r: 255, g: 0, b: 255]

// Partial named destructuring
Person(name, age) = Person[name: "Alice", age: 30, city: "NYC"]

// Star destructuring (extract all named fields)
* = Config[host: "localhost", port: 8080, debug: True]
```

This works the similarly for assignment terms:

```
Point[x: 10, y: 20] ~> =Point[x: a, y: _]
// etc
```

### Matching

Match on literal values:

```
// Evaluates to `Ok` and assigns y to 10
Point[x: 0, y: 10] ~> =Point[x: 0, y: y]

// Evaluates to nil (`[]`), doesn't assign to y
Point[x: 10, y: 10] ~> =Point[x: 0, y: y]
```

This also works on literal values:

```
// Comparing to integer
p.x ~> =0,

// Comparing to string
user.role ~> ="admin"
```

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
{ ~> =x, [x, 10] ~> math.gt => "large" | "small" }
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
double = #int { ~> [~, 2] ~> math.mul }

// Pattern matching on union types
area = #shape {
  | ~> =Circle[radius: r] => [r, r] ~> math.mul
  | ~> =Rectangle[width: w, height: h] => [w, h] ~> math.mul
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
"Hello" ~> <println>                // Built-in print with newline
doubled = [x, 2] ~> <multiply>      // Built-in multiplication
```

## Examples

### Basic usage

```
// Import math functions
(add, mul, sub) = math,

// Create and manipulate values
x = 10, y = 20,
[x, y] ~> add ~> [~, 2] ~> mul ~> [~, 1] ~> sub
```

### Working with tuples

```
math = %"math",

// Define a point type
p1 = Point[x: 10, y: 20],
p2 = Point[x: 5, y: 15],

// Function to add points
add_points = #[Point[x: int, y: int], Point[x: int, y: int]] {
  ~> =[a, b] => Point[
    x: [a.x, b.x] ~> math.add,
    y: [a.y, b.y] ~> math.add
  ]
},

result = [p1, p2] ~> add_points
```

### Pattern matching

```
type list = Nil | Cons[int, &];

// Determine whether a list contains an item
contains? = #[list, int] {
  | ~> =[Nil, _] => []
  | ~> =[Cons[head, tail], value], [head, value] ~> == => Ok
  | ~> =[Cons[_, tail], value] => [tail, value] ~> &
},

xs = Cons[1, Cons[2, Cons[3, Nil]]],
[xs, 3] ~> contains?,   // Ok
[xs, 4] ~> contains?    // []
```

### Conditional logic

```
// Clamp value to range [0, 100]
clamp = #int {
  | ~> [~, 100] ~> math.gt => 100
  | ~> [~, 0] ~> math.lt => 0
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
    ~> =Rectangle[width: w, height: h], [w, h] ~> ==
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
next_year = person.age ~> [~, 1] ~> <add>,    // Use built-in add
name ~> <println>                             // Print to console
```

### Concurrent processes

```
// Echo process that receives and prints messages
echo = #[] {
  $Str[bin] {
    | ~> ="" => []           // Stop on empty string
    | ~> =s => {
      s ~> <println>,        // Print received value
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
