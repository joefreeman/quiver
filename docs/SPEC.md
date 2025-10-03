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
5 ~> double! ~> increment!
```

### Pattern matching

Pattern matching allows destructuring values and branching based on their structure. Matches can test literal values, extract components, and control program flow.

```
type response = Success[bin] | Error[code: int];

#response {
  | ~> Success[content] => content
  | ~> Error[code: 404] => default_page
  | error_page
} ~> handle_response
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

Chains of an expression are executed one at a time, unless a chain evaluates to nil (`[]`), in which case the expression terminats.

See 'blocks' below for further details about control flow.

### Value expansion

The value in a chain can be 'expanded' using the `~` ('ripple') operator. This allows the value to be wrapped in a tuple:

```
5 ~> [~, 1]               // [5, 1]
0 ~> Point[x: ~, y: ~]    // Point[x: 0, y: 0]

```

## Variables and assignment

Variables are declared through assignment using the `~>` operator:

```
42 ~> x                    // Assign 42 to new variable x
Point[x: 10, y: 20] ~> p   // Assign tuple to variable p
```

### Identifiers

Identifiers (for variables and tuple field names) start with a lowercase letter, followed by zero or more alphanumeric characters or underscores.

They can also be suffixed by a question mark and multiple quote characters.

```
x, a1, first_name
is_empty?     // ? suffix for predicates/boolean functions
helper'       // ' suffix for variants or helper functions
valid?''      // Multiple suffixes can be combined
```

### Destructuring

Extract values from tuples during assignment.

```
// Full destructuring
Point[x: 10, y: 20] ~> Point[x: a, y: _]

// Partial destructuring (extract specific fields)
Color[r: 255, g: 0, b: 255] ~> (g, b)

// Partial named destructuring
Person[name: "Alice", age: 30, city: "NYC"] ~> Person(name, age)

// Star destructuring (extract all named fields)
Config[host: "localhost", port: 8080, debug: True] ~> *
```

### Matching

Match on literal values:

```
// Evaluates to `Ok` and assigns y to 10
Point[x: 0, y: 10] ~> Point[x: 0, y: y]

// Evaluates to nil (`[]`), doesn't assign to y
Point[x: 10, y: 10] ~> Point[x: 0, y: y]
```

## Blocks

Blocks are specified in the form `{ ... }`. They provide variable scoping, control flow, pattern matching, and can improve readability.

### Variable scoping

Blocks create new scopes. Variables assigned within a block shadow outer variables but don't affect them.

```
42 ~> x, { 5 ~> x }, x  // 42
```

### Conditional logic

Comma-separated chains provide conditional execution. If a chain evaluates to nil, the expression terminates early. This enables concise logic similar to `&&` and `||` operators or ternary expressions in other languages.

```
// If item is valid, try to process it, otherwise show error
item ~> { ~> is_valid?, ~> process! | show_error! }

// Try multiple sources with fallback
id ~> { ~> read_cache! | ~> query_database! | default_value } ~> value
```

### Pattern matching

Blocks can use 'condition-consequence' syntax - `{ ... => ... | ... }` - with multiple branches. If the 'condition' expression (on the left of the `=>`) doesn't evaluate to nil (`[]`), then the 'consequence' expression will be executed, and then execution will jump to the end of the block, taking the value of the consequence. If the condition does evaluate to nil, execution will jump to the next branch within the block, if any; otherwise the block will evaluate to nil.

```
value ~> {
  | ~> 0 => "zero"
  | ~> [~, 0] ~> math.gt! => "positive"
  | "negative"
}
```

Guards can be added with comma-separated conditions:

```
{ ~> x, [x, 10] ~> math.gt! => "large" | "small" }
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
data ~> .name ~> extracted_name     // Extract field in pipeline
coords ~> .0 ~> x_coord             // Positional access in pipeline
```

## Operators

### Equality and negation

```
[5, 5] ~> ==         // Returns 5 (all equal)
[5, 6, 5] ~> ==      // Returns [] (not equal)
[] ~> !              // Returns Ok (negation of nil)
5 ~> !               // Returns [] (negation of non-nil)
```

## Functions

Functions always have a single parameter and a result. The parameter is explicitly typed, and the result type is inferred.

Functions are defined with `#... { ... }` syntax, where the first `...` is the type definition of the parameter, and the second `...` is the function body (a 'block'; see below).

Functions taking a nil parameter can be defined with the shorthand, `#{ ... }`.

```
// Single parameter function
#int { ~> [~, 2] ~> math.mul! } ~> double

// Pattern matching on union types
#shape {
  | ~> Circle[radius: r] => [r, r] ~> math.mul!
  | ~> Rectangle[width: w, height: h] => [w, h] ~> math.mul!
} ~> area,

// Using a tuple for multiple values
#[int, int] { ~> [a, b] => [b, a] } ~> swap

// Shorthand for nil parameter
#{ 42 }
```

### Function application

An exclamation mark is used to call a function. (Note that without the exclamation mark, the value may be assigned to the variable, overwriting the function.)

```
5 ~> double!             // Apply double to 5
[3, 4] ~> math.add!      // Apply add to tuple [3, 4]
```

### Tail recursion

Use `&` for tail-recursive calls:

```
#[int, int] {
  | ~> [1, y] => y
  | ~> [x, y] => [
    [x, 1] ~> math.sub!,
    [x, y] ~> math.mul!
  ] ~> &
} ~> f
```

Named tail calls to other functions:

```
#int { ~> x => [x, 1] ~> &f } ~> fact
```

## Modules and imports

Import modules using `%"path"` syntax. Relative imports must start with a ".". Standard library modules start with a letter.

Modules are evaluated at compile time, and the result (e.g., the final tuple) is the value that's imported.

```
%"math" ~> math                   // Import standard library
%"./utils.qv" ~> utils            // Import local file
%"math" ~> (add, mul)             // Import specific functions
%"./config.qv" ~> *               // Import all named exports
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
[3, 4] ~> <add> ~> sum              // Built-in addition
"Hello" ~> <println>                // Built-in print with newline
[x, 2] ~> <multiply> ~> doubled     // Built-in multiplication
```

## Examples

### Basic usage

```
// Import math functions
%"math" ~> (add, mul, sub),

// Create and manipulate values
10 ~> x, 20 ~> y,
[x, y] ~> add! ~> sum,
[sum, 2] ~> mul! ~> result
```

### Working with tuples

```
%"math" ~> math,

// Define a point type
Point[x: 10, y: 20] ~> p1,
Point[x: 5, y: 15] ~> p2,

// Function to add points
#[Point[x: int, y: int], Point[x: int, y: int]] {
  ~> [a, b] => Point[
    x: [a.x, b.x] ~> math.add!,
    y: [a.y, b.y] ~> math.add!
  ]
} ~> add_points,

[p1, p2] ~> add_points! ~> result
```

### Pattern matching

```
type list = Nil | Cons[int, &];

// Determine whether a list contains an item
#[list, int] {
  | ~> [Nil, _] => []
  | ~> [Cons[head, tail], value], [head, value] ~> == => Ok
  | ~> [Cons[_, tail], value] => [tail, value] ~> &
} ~> contains?,

Cons[1, Cons[2, Cons[3, Nil]]] ~> xs,
[xs, 3] ~> contains?!,   // Ok
[xs, 4] ~> contains?!    // []
```

### Conditional logic

```
// Clamp value to range [0, 100]
#int {
  | ~> [~, 100] ~> math.gt! => 100
  | ~> [~, 0] ~> math.lt! => 0
  | ~> x => x
} ~> clamp,

150 ~> clamp!,   // 100
-10 ~> clamp!,   // 0
50 ~> clamp!    // 50
```

### Module organization

```
// shapes.qv
type shape = Circle[radius: int] | Rectangle[width: int, height: int];

%"math" ~> math,

[
  bounding_box: #shape {
    | ~> Circle[radius: r] => {
      [r, 2] ~> math.mul! ~> x,
      Rectangle[width: x, height: x]
    }
    | ~> Rectangle[width: w, height: h] => {
      Rectangle[width: w, height: h]
    }
  },

  is_square?: #shape {
    ~> Rectangle[width: w, height: h], [w, h] ~> ==
  }
]
```

```
// main.qv
%"./shapes.qv" ~> (bounding_box, is_square?),

Circle[radius: 5] ~> circle,
Rectangle[width: 10, height: 10] ~> rectangle,

circle ~> bounding_box!,      // Rectangle[width: 10, height: 10]
rectangle ~> is_square?!      // Ok
```

### Using built-ins and field access

```
// Extract and process data
Person[
  name: "Alan",
  date_of_birth: [
    year: 1912,
    month: June,
    day: 23
  ]
] ~> person,
person.name,                           // Extract name field
person ~> .date_of_birth ~> .month,    // Chain field access

// Built-in operations
person.age ~> [~, 1] ~> <add> ~> next_year,    // Use built-in add
name ~> <println>                              // Print to console
```
