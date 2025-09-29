# Quiver Language Specification

Quiver is a statically-typed functional programming language with structural typing, pattern matching, and a postfix-based syntax. Programs are composed of immutable values flowing through transformation pipelines.

## Core Concepts

### Values and Immutability
All values in Quiver are immutable. The language supports:
- **Integers**: `42`, `-17`
- **Binaries**: `'deadbeef'` (hexadecimal bytes), `"hello"` (strings as binary)
- **Tuples**: Collections of values with optional names and field labels

### Postfix Flow
Quiver uses postfix notation where data flows left-to-right through transformations:
```
5 ~> double! ~> increment!
```

The `~` operator (ripple) carries results forward in the pipeline.

## Types

### Basic Types
- `int` - Integer values
- `bin` - Binary data (bytes)

### Tuple Types
Tuples are the primary composite type, supporting:
```
[]                       // Empty tuple (nil)
[int, int]               // Unnamed fields
[x: int, y: int]         // Named fields
Point[x: int, y: int]    // Named tuple type
```

### Type Aliases
```
type point = Point[x: int, y: int]
type adder = #int -> int
```

### Type Imports
Import types from modules using patterns:
```
type (circle, rectangle) = %"shapes"  // Import specific types
type * = %"geometry"                  // Import all types
```

### Union Types
```
type shape = Circle[radius: int] | Rectangle[width: int, height: int]
```

## Variables and Assignment

Variables are declared through assignment using the `~>` operator:
```
42 ~> x                    // Assign 42 to new variable x
Point[x: 10, y: 20] ~> p   // Assign tuple to variable p
```

### Naming Conventions
Identifiers support suffix conventions for clarity:
```
is_empty?     // ? suffix for predicates/boolean functions
helper'       // ' suffix for variants or helper functions
valid'?       // Multiple suffixes can be combined
```

### Destructuring
Extract values from tuples during assignment:
```
// Full destructuring
Point[x: 10, y: 20] ~> Point[x: a, y: b]

// Partial destructuring (extract specific fields)
Person[name: "Alice", age: 30, city: "NYC"] ~> (name, age)

// Star destructuring (extract all named fields)
Config[host: "localhost", port: 8080, debug: True] ~> *
```

## Functions

Functions are defined with `#Type { pattern => expression }` syntax:
```
// Single parameter function
#int { ~> [~, 2] ~> math.mul! } ~> double

// Pattern matching function
#int {
  | ~> 0 => "zero"
  | ~> 1 => "one"
  | ~> n => "other"
} ~> classify

// Multiple parameter function (takes tuple)
#[int, int] { ~> [a, b] => [b, a] } ~> swap
```

### Function Application
Functions use postfix application:
```
5 ~> double!             // Apply double to 5
[3, 4] ~> add!           // Apply add to tuple [3, 4]
```

### Tail Recursion
Use `&` for tail-recursive calls:
```
#[int, int] {
  | ~> [1, y] => y
  | ~> [x, y] => [[x, 1] ~> math.sub!, [x, y] ~> math.mul!] ~> &
} ~> f
```

Named tail calls to other functions:
```
#int { ~> x => [x, 1] ~> &f } ~> fact
```

## Pattern Matching

Blocks use `{ condition => consequence }` syntax with multiple branches:
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

## Field Access

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

### Equality and Negation
```
[5, 5] ~> ==         // Returns 5 (all equal)
[5, 6] ~> ==         // Returns [] (not equal)
[] ~> !              // Returns Ok (negation of nil)
5 ~> !               // Returns [] (negation of non-nil)
```

### Ripple Operator
The `~` carries the previous result forward:
```
5 ~> double! ~> Point[x: ~, y: ~]  // Point[x: 10, y: 10]
```

## Modules

Import modules using `%"path"` syntax:
```
%"math" ~> math                   // Import standard library
%"./utils.qv" ~> utils            // Import local file
%"math" ~> (add, mul)             // Import specific functions
%"config" ~> *                    // Import all named exports
```

## Built-in Functions

Access built-in operations using angle brackets:
```
[3, 4] ~> <add> ~> sum              // Built-in addition
"Hello" ~> <println> ~> output      // Built-in print with newline
[x, 2] ~> <mul> ~> doubled         // Built-in multiplication
```

## Control Flow

### Sequences
Comma-separated expressions execute left-to-right. If any expression returns `[]` (nil), the sequence stops:
```
expr1, expr2, expr3
expr1; expr2; expr3    // Semicolons can also separate statements
```

### Truthiness
- `[]` (empty tuple) is falsy
- All other values are truthy

## Examples

### Basic Usage
```
// Import math functions
%"math" ~> (add, mul, sub),

// Create and manipulate values
10 ~> x,
20 ~> y,
[x, y] ~> add! ~> sum,
[sum, 2] ~> mul! ~> result
```

### Working with Tuples
```
// Define a point type
Point[x: 10, y: 20] ~> p1,
Point[x: 5, y: 15] ~> p2,

// Function to add points
#[Point[x: int, y: int], Point[x: int, y: int]] {
  ~> [a, b] => Point[
    x: [a.x, b.x] ~> add!,
    y: [a.y, b.y] ~> add!
  ]
} ~> add_points,

[p1, p2] ~> add_points! ~> result
```

### Pattern Matching
```
// Classification function
#int {
  | ~> 0 => "zero"
  | ~> [~, 0] ~> math.gt! => "positive"
  | "negative"
} ~> classify,

5 ~> classify!,    // "positive"
-3 ~> classify!,   // "negative"
0 ~> classify!     // "zero"
```

### Conditional Logic
```
// Sign function using guards
#int {
  | ~> [~, 0] ~> math.gt! => 1
  | ~> [~, 0] ~> math.lt! => -1
  | 0
} ~> sign,

// Using with pipeline
-42 ~> sign! ~> result  // -1
```

### Module Organization
```
// math.qv
[
  add: #[int, int] { ~> p => [p.0, p.1] ~> <add> },
  mul: #[int, int] { ~> p => [p.0, p.1] ~> <mul> },
  abs: #int {
    | ~> x, [x, 0] ~> <lt> => [0, x] ~> <sub>
    | ~> x => x
  }
]

// main.qv
%"math" ~> (add, abs);
-5 ~> abs! ~> positive;  // 5
[3, 4] ~> add! ~> sum    // 7
```

### Using Built-ins and Field Access
```
// Extract and process data
Person[name: "Alice", details: [age: 30, city: "NYC"]] ~> person,
person ~> .name ~> name,                    // Extract name field
person ~> .details ~> .age ~> age          // Chain field access

// Built-in operations
[age, 1] ~> <add> ~> next_year_age,         // Use built-in add
name ~> <println>                           // Print to console
```

This specification provides a foundation for understanding Quiver's key concepts while maintaining the language's functional, pipeline-oriented philosophy.
