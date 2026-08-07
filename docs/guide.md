# The Quiver Guide

Quiver is a statically-typed functional language. Values are immutable and structurally
typed, they flow left-to-right through pipelines, and control flow is pattern matching.
Concurrency is Erlang-style: lightweight processes exchanging typed messages.

```quiver program
// shapes.qv — a shape is one of two things.
'shape =
  | Square[side: 'int]
  | Rect[width: 'int, height: 'int]

// Its area, by cases.
area = #'shape {
  | =Square[side: s] => %num.mul [s, s]
  | =Rect[width: w, height: h] => %num.mul [w, h]
}

// A program's last step is the function `quiv run` executes.
#{
  %list{ Square[side: 3], Rect[width: 2, height: 4] }
  ~> %list.map [~, &area]
  ~> %list.fold [~, init: 0, f: &%num.add]
  //=> 17
}
```

Run it with `quiv run shapes.qv`. `quiv repl` starts an interactive session, and
[quiver.run](https://quiver.run) has one in the browser. Short programs can go straight on
the command line: `quiv run -e '#{ %num.add [2, 3] }'`.

Throughout, `//=>` marks what an expression evaluates to — real syntax, a debug-checked
[assertion](#assertions) — and plain `//` is a comment. Every
example that can run standalone is checked against the compiler; a few that need the
network, a second file, or an elided body are marked and are not.

## Notation at a glance

Quiver leans on punctuation. This table is the whole of it; everything below is an
elaboration of one of these rows.

| | | |
| --- | --- | --- |
| `~>` | pipe a value into the next term | `5 ~> double` |
| `~` | the value flowing in this chain | `5 ~> [~, 1]` |
| `;` | step separator (a newline is the same) | `a = 1; a` |
| `//` | comment, to end of line | `x = 1  // a note` |
| `//=>` | assert the step's value, in debug builds | `5 ~> double //=> 10` |
| `#` | function literal | `#'int { $ }` |
| `$` | the function's parameter | `#'int { $ }` |
| `$$` | the *enclosing* function's parameter | `#'int { #'int { $$ } }` |
| `&` | reference a value; don't call it | `&double` |
| `=` | bind, or match | `x = 5` / `5 ~> =x` |
| `\|` | separates branches, and union members | `{ =A => 1 \| 2 }` |
| `=>` | condition, then consequence | `{ =0 => "zero" \| "other" }` |
| `'` | names a type | `'int`, `'point` |
| `%` | names a module | `%num.add` |
| `^` | tail call; recursion inside a type | `^ [n, acc]` |
| `@` | spawn a process; a process type | `@worker`, `@'int` |
| `!` | receive a message, or await a process | `!'int`, `!p` |
| `?` | sample a process's state | `?p` |
| `:` | field label; annotation key | `[x: 1]`, `f:doc` |
| `.` | field access | `p.x` |
| `...` | spread | `[...p, y: 3]` |
| `\` | names a resource type | `\File` |
| `_` | ignore, in a pattern | `=[x, _]` |
| `*` | bind every named field | `* = p` |

## Values

There are three kinds of primitive value and one composite.

```quiver
42                       //=> 42
-17                      //=> -17
0x0a1b                   //=> 0x0a1b
[1, 0x0a, [2, 3]]        //=> [1, 0x0a, [2, 3]]
```

Integers are arbitrary precision. Binaries are written as an even number of hex digits, so
`0x` alone is the empty binary. Everything else is a **tuple**.

### Tuples

A tuple has zero or more fields, and may have a name. Names start with an uppercase letter;
field labels are ordinary identifiers.

```quiver
[]                            //=> []
[1, 2]                        //=> [1, 2]
[x: 1, y: 2]                  //=> [x: 1, y: 2]
Point[x: 1, y: 2]             //=> Point[x: 1, y: 2]
Blue                          //=> Blue
A[b: B[c: 0x01]]              //=> A[b: B[c: 0x01]]
```

Two tuples are worth naming now, because the language uses them as answers:

- `[]` is **nil**. It means "nothing", "no", and "failed".
- `Ok` is a named empty tuple, used to mean "yes" when there is nothing else to say.

A tuple with a name but no fields — `Blue`, `Nil`, `Ok` — is how Quiver spells an enum
case. There are no booleans; nil and non-nil do that job.

### Strings

`"..."` is sugar for UTF-8 bytes wrapped in a `Str` tuple, so a string is an ordinary
value with no special support in the language.

```quiver
"hello"                       //=> "hello"
"hello" ~> =Str[b]; b         //=> 0x68656c6c6f
```

The escapes `\n`, `\r`, `\t`, `\\`, `\"` and `\{` are recognised. A `{ … }` hole
interpolates: its contents are parsed like any code, must evaluate to a `Str`, and — like
a tuple field — receive the value flowing in.

```quiver
name = "world"
"hello {name}"                //=> "hello world"
"world" ~> "hello, {~}"       //=> "hello, world"
"1 + 1 = {%str.from_int 2}"   //=> "1 + 1 = 2"
```

A `"""`-delimited string spans lines. The closing delimiter's indentation sets a margin
stripped from every line, and the newline before it is not included.

```quiver
msg = """
    hello
      indented
    """
msg                           //=> "hello\n  indented"
```

Inside one, `\s` is a space that survives trailing-whitespace stripping, a trailing `\`
joins the next line, and `\"""` is a literal triple quote.

### Names

Variables and field labels start with a lowercase letter, then letters, digits and
underscores. They may end in `?`, `!`, or both — by convention `?` for predicates and `!`
for emphasis.

```quiver
x = 1; first_name = "ada"; is_empty? = Ok; validate! = Ok
[x, first_name, is_empty?]    //=> [1, "ada", Ok]
```

Values are immutable. Nothing below ever changes a value in place; it builds a new one.

## Flow

### Chains

A **chain** is a sequence of terms joined by `~>`. The value produced by one term becomes
the input to the next, and `~` always names the value flowing in.

```quiver
5 ~> %num.add [~, 1] ~> %num.mul [~, 10]   //=> 60
```

What a term does with the value it receives depends on what the term is. A callable is
called with it, a literal replaces it, and `~` puts it wherever you want it — including
inside a tuple being built, whose fields each receive a copy.

```quiver
double = #'int { %num.mul [~, 2] }

5 ~> double                   //=> 10
5 ~> 99                       //=> 99
5 ~> [~, 1]                   //=> [5, 1]
5 ~> [double, 1]              //=> [10, 1]
```

`&` is the opt-out: it references a callable instead of calling it.

```quiver
5 ~> [&double, 1] ~> =[f, _]; f 4   //=> 8
```

The `~>` is mandatory: whitespace alone does not join terms. To spread one chain over
several lines, start each continuation with `~>`.

```quiver
5
~> %num.add [~, 1]
~> %num.mul [~, 10]           //=> 60
```

### Steps

A newline or a `;` ends a chain and starts a new **step**. A run of steps is a
**sequence** — a program, a function body, a branch.

Two rules govern a sequence, and together they are most of Quiver's control flow:

1. Every step starts from the same value — the enclosing block's input.
2. If a step evaluates to nil, the rest of the sequence is skipped and the sequence
   evaluates to that nil.

```quiver
f = #'int { 5; ~ }
9 ~> f                        //=> 9     `~` is f's input, not the 5

g = #'int { a = %num.add [~, 20]; %num.add [a, 100] }
3 ~> g                        //=> 123   carried across the boundary by name
```

So a step's result gates the boundary, and — if it is the last step — is the sequence's
value. It is never handed to the next step. To carry a value forward, name it.

Nil short-circuiting is the other half. Note that this is a property of *steps*, not of
chains: within a chain nil flows onward like any other value.

```quiver
[] ~> 5                       //=> 5    one step; nil flows through the chain
{ []; 5 }                   //=> []   two steps; the first is nil, so it stops
```

That is the whole model: **a chain pipes, a sequence restarts and can fail.**

### Building values in a chain

`~` can appear anywhere a value can, including several times:

```quiver
0 ~> Point[x: ~, y: ~]        //=> Point[x: 0, y: 0]
```

`...` spreads an existing tuple into a new one. Later fields win.

```quiver
a = A[x: 1, y: 2]
a[..., y: 3]                  //=> A[x: 1, y: 3]   keeps the name
a[..., z: 4]                  //=> A[x: 1, y: 2, z: 4]
[...a, y: 3]                  //=> [x: 1, y: 3]    drops the name
B[...a]                       //=> B[x: 1, y: 2]   replaces it
[w: 0, ...a]                  //=> [w: 0, x: 1, y: 2]
```

The source of a spread can be any access path — a variable, a field of one, the function
parameter, or the flowing value. In the name-preserving form the bracket is glued to its
source.

```quiver
n = [inner: P[x: 1, y: 2]]
n.inner[..., y: 5]            //=> P[x: 1, y: 5]
B[...n.inner, y: 3]           //=> B[x: 1, y: 3]
A[x: 1] ~> ~[..., y: 2]       //=> A[x: 1, y: 2]   `~` as the source, name kept
A[x: 1] ~> [..., y: 2]        //=> [x: 1, y: 2]    in-tuple, name dropped
```

When a tuple's fields are just variables of the same name, `( … )` **puns** them:

```quiver
x = 1; y = 2
(x, y)                        //=> [x: 1, y: 2]
Point(x, y)                   //=> Point[x: 1, y: 2]
```

An entry is a *name*, not an expression, so nothing flows into it and a callable entry is
referenced rather than called — which is what a record of functions wants. Entries may be
access paths, labelled by the final segment; `(…)` takes puns and nothing else.

```quiver
p = [x: 1, y: 2]
(p.x, p.y)                    //=> [x: 1, y: 2]

5 ~> [f: double]                    //=> [f: 10]   a field is an expression: called
5 ~> (double) ~> .double ~> ~ 4     //=> 8         a pun is a name: referenced
```

### Field access

`.` reads a field, by label or by position, and works as a chain term.

```quiver
p = Point[x: 10, y: 20]
p.x                           //=> 10
p.0                           //=> 10
p ~> .y                       //=> 20
[a: [b: 7]] ~> .a.b           //=> 7
```

## Functions

A function has exactly one parameter, whose type is written, and a result, whose type is
inferred. `$` is the parameter.

```quiver
double = #'int { %num.mul [$, 2] }
sum = #['int, 'int] { %num.add [$.0, $.1] }

5 ~> double                   //=> 10
sum [3, 4]                    //=> 7
```

`$x` and `$0` are sugar for `$.x` and `$.0`. `#'int` with no body is the identity
function on `'int`. `#[] { … }`, or just `#{ … }`, takes nil — such a function ignores
any value flowing into it and is called automatically:

```quiver
answer = #{ 42 }
answer                        //=> 42
5 ~> answer                   //=> 42   the 5 is ignored
```

A result type can be stated and is then checked: `#'int -> 'bin { … }`.

### Reaching an outer parameter

An extra glued sigil reaches one function further out: `$$` is the enclosing function's
parameter, `$$$` the one beyond, with the same accessor sugar (`$$user`). Blocks are
transparent — only function literals count as levels.

```quiver
adder = #[by: 'int] { #'int { %num.add [$, $$by] } }
add3 = adder [by: 3]; add3 4   //=> 7
```

An outer parameter is **captured by value when the closure is built**, per accessed path,
so `$$by` moves exactly the field it names — the same as the binding it replaces
(`by = $by`). It works in pins (`=&$$by`) and puns (`($$by)`) too. Reaching above the
outermost function is a compile error.

### Applying

There are two spellings, and they mean the same thing.

```quiver
[3, 4] ~> %num.add            //=> 7    pipe the whole value in
%num.add [3, 4]               //=> 7    write the argument after the callee
```

The second is **juxtaposition**: a callable, a space, one argument. The flowing value
flows into that argument, which is what lets the two combine:

```quiver
5 ~> %num.add [~, 100]        //=> 105
```

Exactly one argument is taken — `f x y` is an error; pipe instead (`g x ~> f`). Because
arguments and tuple fields receive the flowing value, a callable written in one is
*called*; prefix `&` to pass it along instead.

```quiver
xs = Cons[1, Cons[2, Nil]]
xs ~> %list.map [~, &double]  //=> Cons[2, Cons[4, Nil]]
```

To apply a function that is itself the flowing value, use `~` as the head:

```quiver
&%num.add ~> ~ [1, 2]         //=> 3
%num ~> ~.add [1, 2]          //=> 3
```

### Inferred parameters

Writing `#{ … }` where a parameter type is *expected* — as a call argument, or a field of
one — infers it from the callee, so a short function passed to a combinator needs no
annotation:

```quiver
xs ~> %list.map [~, #{ %num.mul [$, 3] }]   //=> Cons[3, Cons[6, Nil]]
```

Type variables in that expected type are pinned by the other arguments, so an inferring
literal must come after them. With no expected type, `#{ … }` falls back to a nil
parameter.

### Tail calls

`^` calls a function in tail position, reusing the frame. Bare `^` is the enclosing
function; `^f` names another. Recursion is written this way.

```quiver
count_down = #['int, 'int] {
  | =[0, acc] => acc
  | =[n, acc] => ^ [%num.sub [n, 1], %num.add [acc, n]]
}
count_down [4, 0]             //=> 10
```

`^~` tail-calls a function that is itself the flowing value, which is how a loop steps to
a successor it was handed rather than one it names.

```quiver
double = #'int { %num.mul [$, 2] }
apply = #'int { &double ~> ^~ $ }
apply 5                       //=> 10
```

### Optional labels and defaults

A parameter field whose label is written in parentheses is **omittable**: the argument may
state it or leave the field positional, and the value adopts the label either way. The
label is inferred from the entry's position — a bare entry adopts the marked label of the
field at its index — so it may be left off exactly where the positions line up, and an
unmarked field's label must always be written. Labelled entries may be given in any order;
positional ones come first.

```quiver
at = #[(x): 'int, (y): 'int] { [$x, $y] }
at [x: 1, y: 2]               //=> [1, 2]
at [1, 2]                     //=> [1, 2]
at [1, y: 2]                  //=> [1, 2]
at [y: 2, x: 1]               //=> [1, 2]
```

Patterns adopt the same way, so a marked value destructures as it may be built:

```quiver
pt = #[(x): 'int, (y): 'int] { $ }
pt [1, 2] ~> =[a, b]; [a, b]  //=> [1, 2]
```

A parameter field may also declare a **default**, and the argument may then omit it
entirely. A field may be omitted exactly when it has one.

```quiver
open = #[(path): '%str, mode: (R | W | A) = R, buffer: 'int = 1024] { $ }
open ["f.txt"]                //=> [path: "f.txt", mode: R, buffer: 1024]
open ["f.txt", mode: A]       //=> [path: "f.txt", mode: A, buffer: 1024]
```

The default belongs to the function, not to its parameter type — `open`'s parameter is
exactly `[path: '%str, mode: R | W | A, buffer: 'int]` — so it neither widens the type nor
distinguishes two functions with the same signature. It is read from the callee value, so
like a [contract](#contracts) it is **shed at a declared boundary**: reached through a
written function type, every field is mandatory again. The general form, for a parameter
written as an alias, is a `:defaults` annotation.

## Matching

A **match** tests a value and binds parts of it. It evaluates to `Ok` on success and nil
on failure. The matched value does not flow onward; the bindings it makes do.

The two spellings differ only in spacing, and the spacing is the rule: `x = e` binds,
`e =x` matches within a chain.

```quiver
x = 42                        // bind
42 ~> =x                      //=> Ok   match, in a chain
42 ~> =41                     //=> []   and it can fail
```

Because a failed match is nil, and a nil step ends its sequence, a match doubles as a
guard. This is the basis of nil-propagation throughout the language.

### Destructuring

```quiver
p = Point[x: 10, y: 20]

Point[x: a, y: b] = p; [a, b]   //=> [10, 20]   bind both fields
(x, y) = p; [x, y]              //=> [10, 20]   partial: named fields only
Point(x) = p; x                 //=> 10         named partial
* = p; [x, y]                   //=> [10, 20]   every named field
Point* = p; [x, y]              //=> [10, 20]   ... and require the name
```

A pattern field matches by label when it has one and by position when it does not, so a
positional pattern reads a positionally-built tuple:

```quiver
q = Point[10, 20]
Point[a, b] = q; [a, b]       //=> [10, 20]
Point[m, _] = q; m            //=> 10          `_` ignores a field
```

A tuple pattern's name must correspond to the value's: a stated name requires that name,
and a pattern without one matches only an unnamed tuple — which makes `=[]`, the nil
literal, simply the rule's empty case: it never matches a named empty tuple. To
destructure a named tuple without stating its name, use a partial or star pattern.

```quiver
{ [x: a, y: b] = Point[x: 1, y: 2]; a }   //=> []       the name must be stated...
(x, y) = Point[x: 1, y: 2]; [x, y]          //=> [1, 2]   ...or left to a partial
```

Literals inside a pattern test rather than bind:

```quiver
Point[x: 0, y: n] = Point[x: 0, y: 10]         //=> Ok
{ Point[x: 0, y: n] = Point[x: 1, y: 10] }   //=> []
```

### Testing types and pinning

A type name is always a reference — types are never bound — so `='int` tests. To test
against an existing *value*, prefix it with `&`.

```quiver
42 ~> ='int                   //=> Ok
0x01 ~> ='int                 //=> []

y = 2
2 ~> =&y                      //=> Ok
3 ~> =&y                      //=> []
Point[1, 2] ~> =Point[x, &y]  //=> Ok   binds x, checks y is 2
```

A pin's target may be any access path — a field of a variable, or of the enclosing
function's parameter via `$`.

```quiver
p = Point[x: 1, y: 2]
1 ~> =&p.x                    //=> Ok
same? = #[x: 'int, y: 'int] { $y ~> =&$x }
same? [x: 3, y: 3]            //=> Ok
```

`(T)x` asserts a type *and* binds the value at that narrowed type. The identifier must be
adjacent to the `)`.

```quiver
42 ~> =('int)n; n             //=> 42
{ [] ~> =('int)n }          //=> []   nil is not an int, so this fails
```

This is the idiom for "bind, but fail on the wrong type", which combines with
short-circuiting to propagate.

### Alternation

A parenthesised, `|`-separated list matches if any alternative does. Every alternative
must bind the same variables.

```quiver
[[], 5] ~> =([[], _] | [_, []])   //=> Ok
42 ~> =('int | 'bin)              //=> Ok
```

### Where a match may appear

A match that can fail must be the **last term of its chain**.

```quiver ignore
=Square[side: s] ~> %num.gt? [s, 10]   // error: FallibleMatchNotChainFinal
=Square[side: s]; %num.gt? [s, 10]     // two steps — the boundary does the gating
```

Nothing short-circuits inside a chain, so a term after the match would run whether or not
it matched, reading bindings that only hold on success. Chain-final, its verdict gates the
step boundary — which is what makes it a guard. An irrefutable match, such as a bare
binder, may continue its chain.

In a **value position** — a tuple field, a call argument, an annotation value — nothing
gates on the verdict at all, so a match there is just data: it may not bind, and it does
not narrow the matched value for the surrounding code.

```quiver
x = 5
[int?: x ~> ='int, bin?: x ~> ='bin]   //=> [int?: Ok, bin?: []]
```

## Branching

A block, `{ … }`, introduces a scope and holds branches. Branches are separated by `|`,
and **every branch starts from the block's input** — a value piped into a block is shared
by all of them.

```quiver
B[42] ~> { =A[a] => 1 | =B[b] => 2 }   //=> 2
```

If a branch's sequence evaluates to nil, control moves to the next branch; if there are
none left, the block is nil. That gives fallback chains directly:

```quiver
first_hit = #'int {
  | { =0 => [] | ~ }        // fails on 0
  | -1                         // ... and then this
}
7 ~> first_hit                //=> 7
0 ~> first_hit                //=> -1
```

### Condition and consequence

`cond => body` is a branch that commits. If `cond` is non-nil, `body` runs and the block
is done — even if `body` itself fails. If `cond` is nil, control moves on as usual.

```quiver
sign = #'int {
  | =0 => "zero"
  | %num.gt? [~, 0] => "positive"
  | "negative"
}
0 ~> sign                     //=> "zero"
5 ~> sign                     //=> "positive"
-5 ~> sign                    //=> "negative"
```

A condition is a *sequence*, so a step separator adds a guard. The match is one step, the
guard the next, and the boundary short-circuits — so the guard sees the pattern's
bindings only when it matched.

```quiver
size = #(Square[side: 'int]) {
  | =Square[side: s]; %num.gt? [s, 10] => "large"
  | "small"
}
Square[side: 20] ~> size      //=> "large"
Square[side: 2] ~> size       //=> "small"
```

Joining those with `~>` instead would not be a guard, and is rejected: a fallible match
must end its chain. To test that a pattern does *not* match, use a block:

```quiver
not_square? = #(Square[side: 'int] | Circle[radius: 'int]) {
  | =Square() => []
  | Ok
}
Circle[radius: 1] ~> not_square?   //=> Ok
Square[side: 1] ~> not_square?     //=> []
```

### Scope

Bindings made in a block are local to it, and are cleared between branches.

```quiver
x = 42
{ x = 5 }
x                             //=> 42
```

## Types

Most types are inferred. What you write are function parameters, type aliases, and the
occasional assertion; results, locals and intermediate values are worked out.

A type name carries a leading `'`, which is what distinguishes it from a variable —
`'int` the type, `int` a variable. Tuple names need no prefix; their capital letter
already sets them apart.

There are three primitive types — `'int`, `'bin`, and `'ref` (unique opaque identifiers,
covered under [Refs](#refs)) — and everything else is built from tuples.

### Tuple and function types

A tuple type mirrors the value.

```quiver ignore
[]                            // nil
Blue                          // a named empty tuple
['int, 'int]                  // unnamed fields
[x: 'int, y: 'int]            // named fields
Point[x: 'int, y: 'int]       // a named tuple
#'int -> 'bin                 // a function
```

### Type aliases

An alias is defined with `=`, like a binding; the `'` on the name is what makes it a type
definition.

```quiver
'point = Point[x: 'int, y: 'int]
'adder = #'int -> 'int

origin = #'point { [$x, $y] }
origin Point[x: 0, y: 0]      //=> [0, 0]
```

An alias is a step of a sequence like any other, so it can be declared inside a block or
a function body, where it is scoped to that body and can name the enclosing function's
type parameters. Aliases are positional: a definition must precede its uses.

```quiver
h = #<'t>'t {
  'pair = ['t, 't]
  [$, $] ~> =('pair)p
  p
}
h 3                           //=> [3, 3]
```

### Unions

`|` joins alternatives. Matching is how you take one apart.

```quiver
'bool = True | False
'shape = Circle[radius: 'int] | Rectangle[width: 'int, height: 'int]
```

Nil is an ordinary member, and `'t | []` is how "maybe a `'t`" is spelled.

### Partial types

A partial type constrains some fields and says nothing about the rest. It uses
parentheses, and every field must be named.

```quiver
'positioned = (x: 'int, y: 'int)

nudge = #'positioned { %num.add [$x, 1] }
nudge Point[x: 1, y: 2, z: 3]   //=> 2
```

`()` matches any tuple, and `Point()` any tuple named `Point`.

### Recursive types

`^` refers back to the type's **outermost boundary** — a union or a function type; a
tuple is not one. `^1`, `^2` and so on name successively *inner* boundaries, counting in
from that root.

```quiver
'list<'t> = Nil | Cons['t, ^]
'tree<'t> = Leaf['t] | Node[^, ^]
```

So in a nested type, `^` reaches the whole thing and `^1` the union it is written inside:

```quiver
'json = Null | 'int | Array[(Nil | Cons[^, ^1])]
render = #'json { Ok }
Array[Cons[1, Cons[Null, Nil]]] ~> render   //=> Ok
```

Here `^` is `'json` — so a list element may be any JSON value — and `^1` is the list's own
`Nil | Cons[…]`, which is what makes the tail a list. Naming a boundary the type does not
have is a compile error.

### Generics

Type parameters are declared in angle brackets, on an alias or a function, and are
inferred from usage.

```quiver
'pair<'a, 'b> = Pair[first: 'a, second: 'b]

id = #<'t>'t { $ }
id 42                         //=> 42
id "hi"                       //=> "hi"
```

They can also be pinned explicitly, with the argument list glued to the name. This is a
checked assertion rather than a hint: it narrows what the use site sees, and a mismatch is
a compile error.

```quiver
id<'int> 42                   //=> 42
f = &id<'int>; f 7            //=> 7
```

A prefix may be given and the rest left inferred. A builtin can be *type-consuming* —
its behaviour depends on the type argument itself — and must then always be instantiated
where it is applied: `%data.decode<'t>` is one.

### Spreads

Types compose with `...` the way values do, and later fields override earlier ones.

```quiver
'entity = [id: 'int, created_at: 'int]
'post = Post[...'entity, title: '%str]

make = #'post { $title }
make Post[id: 1, created_at: 0, title: "hi"]   //=> "hi"
```

Spreading a union distributes over its members:
`'event[..., at: 'int]` adds `at` to every variant.

### Intersections

`'t & 'u` is the type of values satisfying every member. It binds tighter than `|`, so
`'t & 'u | 'v` is `('t & 'u) | 'v`. It is most useful for composing partial types.

```quiver
'readable = (read: (#'bin -> Ok))
'writable = (write: (#'bin -> Ok))
'rw = 'readable & 'writable

echo = #'rw { $read 0x01 }
echo [read: #'bin { Ok }, write: #'bin { Ok }]   //=> Ok
```

Two partials meet in a partial constraining both field sets; a field both constrain takes
the intersection of the two constraints. A partial meeting a concrete tuple keeps the
tuple, with the constrained fields narrowed. Disjoint members intersect to nothing, so
`'int & 'bin` is uninhabited — it matches no value.

In matching, each member is checked separately:

```quiver
[x: 1, y: 2] ~> =((x: 'int) & (y: 'int))v; v.x   //=> 1
```

### Data notation

**Quiver data notation** is the literal syntax restricted to data: integers, binaries and
tuples, plus the `"…"` sugar. The `%data` module is its codec.

```quiver
%data.encode Point[x: 1, y: "hi"]              //=> "Point[x: 1, y: \"hi\"]"
```

```quiver
'msg = Ping | Pong['int]
%data.decode<'msg> "Pong[3]"                   //=> Pong[3]
%data.decode<'msg> "Nope"                      //=> []
```

`decode` is driven by the expected type: names in the text resolve only against that
type's members, so decoding can never produce a shape the program does not already
contain. Anything malformed or mismatched answers nil, like a failed match. Strings do not
interpolate in data — a `{` is literal, and `encode` escapes it so encoded text also reads
as literal — and values with identity (refs, pids, functions, resources) have no notation,
so encoding one is a runtime error.

## Modules

A module is a `.qv` file whose value — usually a tuple of functions — is what importing it
gives you. `%name` imports it.

```quiver
num = %num
num.add [1, 2]                //=> 3
```

```quiver
(add, mul) = %num             // just these two
add [1, 2]                    //=> 3
```

```quiver
* = %num                      // every named export
mul [3, 4]                    //=> 12
```

A member can also be reached inline, without binding anything, which is the style used
throughout this guide:

```quiver
%num.add [1, 2]               //=> 3
```

Modules are evaluated at compile time, and the result is what gets imported. That
evaluation must be deterministic and produce identity-free values, so minting refs and
reading the clock are rejected there — they belong in the module's functions, which run
when the caller calls them. A program's own top level has no such restriction: it runs at
startup with the full runtime.

### Types from modules

`'%mod` is a module's **default type** — its nameless definition — and `'%mod.name` a
named one. A module's type namespace is its top-level aliases; one declared inside a
function body is private to the module.

```quiver
'strings = '%list<'%str>
count = #'strings { %list.count $ }
count Cons["a", Cons["b", Nil]]   //=> 2
```

Inside the module that defines it, a module's own default type is a bare `'` — and
`'<'t>` when it takes type parameters, which is how `%list` and `%dict` refer to
themselves (`' = Nil | Cons['t, ^]` is declared `'<'t> = …`).

### Writing one

```quiver ignore
// shapes.qv
' = Circle[radius: 'int] | Rect[width: 'int, height: 'int]   // the default type

'boxed = Boxed[']                                            // a named type, over `'`

[
  area: #' {
    :doc "The shape's area."
    | =Circle[radius: r] => %num.mul [r, r]
    | =Rect[width: w, height: h] => %num.mul [w, h]
  },
]
```

The exported tuple is the module. From elsewhere, that is `%shapes.area`, `'%shapes` and
`'%shapes.boxed`.

### The manifest

`quiver.toml` marks a project root and routes imports to providers. Later rules win, and a
rule whose provider has no such module falls through to an earlier one.

```toml
modules = [
  { std = true },                                  # the standard library
  { path = "./src" },                              # this project's modules
  { name = "mathx", path = "./vendor/mathx/src" }, # a dependency, under %mathx/...
]
```

With no manifest, the standard library alone is available.

## Processes

A process is a lightweight thread with a mailbox. `@` spawns one from a function and
answers a pid.

```quiver
worker = #'int { %num.mul [$, 2] }
a = @worker 21                // the init argument, written explicitly
b = 21 ~> @worker             // or taken from the flowing value
[!a, !b]                      //=> [42, 42]
```

`@'int { … }` is shorthand for spawning a function literal, and `@~` spawns a function
that is itself the flowing value.

### Sending, receiving, awaiting

Send by applying a value to the pid. Receive with `!`, whose parameter type names the
message type. Await a process's result with `!` on the pid.

```quiver
p = @#{ 42 }
!p                            //=> 42
```

```quiver
adder = #{ !#['int, 'int] ~> %num.add }
q = @adder
[3, 4] ~> q; !q               //=> 7
```

A receive shapes the process's message type, and sending is checked against it. `.` is the
current process — `42 ~> .` sends to self, `&.` is a reference to it.

Awaiting is never lethal, so its result is fallible: `'r | []` for a process returning
`'r`. A crashed process answers nil carrying a `:crash` annotation rather than propagating
the error, so an ordinary branch recovers from it.

```quiver ignore
r = !p
r ~> {
  | =('res)v => v                                          // completed
  | r:(Error(message: '%str))crash ~> =(message: m) => m   // crashed
  | fallback                                               // legitimately nil
}
```

### Sampling state

Every process has an observable **state**: the argument its root function was most
recently entered with — the spawn init, then each tail call in the root frame. `?` samples
it, and never waits.

```quiver
'status = Loading | Done['int]
step = #'status { =Loading => 7 ~> ^ Done[~] | =Done[x] => x }
p = Loading ~> @step
?p ~> ='status                //=> Ok   Loading, then Done[7]
```

The sample's type is inferred at the spawn site: the root function's own parameter type
(the spawn init) united with the parameter types of its tail calls. There is no runtime
test — every state write is compile-checked.

### Capabilities in written types

A pid's capabilities are inferred, but a *declared* type grants only what it spells. A
process type takes an optional `-> 'r` result and a `?` state clause, whitespace before
the sigil:

```quiver
'status = Loading | Done['int]
step = #'status { =Loading => 7 ~> ^ Done[~] | =Done[x] => x }
p = Loading ~> @step

watch = #(@?'status) { ?$ }         // sample-only
await = #(@ -> 'int) { !$ }         // awaitable
watch &p ~> ='status                //=> Ok
await &p                            //=> 7
```

A function type takes `!` and `?` clauses in that order — `!'c` is what running it
receives, `?'d` the states beyond its parameter — which matters when the function is to be
spawned: `#'config -> 'r !'cmd ?'connected`.

An omitted clause is **not granted**: `?` on a plain `@'msg` is a compile error. Sends are
contravariant (a process that receives more fits a narrower promise), the await result is
covariant, and state is covariant and strict — a state-less process never satisfies a
stated `?`, which is what keeps the test-free `?p` sound. So a boundary may deliberately
state a coarser state union than the implementation's, keeping internal phases private.

### Ownership

`@f` creates a child **owned** by the spawning process. When a process ends, normally or
not, its owned children are torn down with it, cascading down the subtree. Teardown never
travels upward: a child's death is only ever observed by its parent.

```quiver ignore
%proc.detach &p    // relinquish ownership; p outlives this process
%proc.kill &p      // terminate p and its subtree
%proc.link &p      // fate-sharing: either dying abnormally kills the other
```

### Select

`!` generalises to a **select** over several sources, racing them. The general form is
`![sources]` — an ordinary value tuple, so a function source needs `&`.

```quiver ignore
![p1, p2, 5000]    // whichever of two processes finishes first, or a 5s timeout
![sock, &control]  // socket bytes, or a mailbox message
&p1 ~> ![~, 1000]  // in a chain, via `~`
```

Sources are processes (await), functions by reference (receive), integers (a timeout in
milliseconds), and stream resources. Order is the tie-break: when several are ready at
once, the earliest wins. A timed-out select answers nil carrying `:timeout`, so a fallback
can tell a timeout from a crash from a legitimate nil.

The shorthands each select on one source:

| form | means |
| --- | --- |
| `!p`, `!f`, `!%mod.recv` | `![&p]` — await, or receive |
| `!'int`, `!Done`, `!(…)` | `![#'int]` — receive a message of that type |
| `!#['int, 'int]` | an unnamed tuple type needs the `#` |
| `![]` | a no-op, returning nil |

### Filters and handlers

A block written **directly after** a select is a *filter*: it inspects each candidate
message and decides whether to accept it. Nil skips the message, leaving it in the mailbox.
A block joined with `~>` is an ordinary chain step — a *handler* applied to the message
already received.

```quiver ignore
!'int { =42 => Ok | [] }      // filter: wait specifically for 42
!'int ~> { =42 => Ok | [] }   // handler: take any int, then test it
```

A filter may be re-evaluated, so its verdict must be stable: spawning, sending, nested
selects, host-state reads and ref minting are all rejected inside one.

### Refs

`%ref` is not a record but a single nilary function, so every evaluation of it mints a
fresh unique value of type `'ref`.

```quiver
tag = %ref
[tag, 42] ~> =[&tag, x]; x    //=> 42
```

```quiver
ref = &%ref                   // bind the function to call it repeatedly
a = ref; b = ref
a ~> =&b                      //=> []   distinct refs are not equal
```

## Resources and failure

Some operations produce **resources** — opaque handles to external state. A resource type
is written `\Name`: `\File`, `\Dir`, `\TcpSocket`, `\TcpListener`, `\DnsResolver`,
`\ByteStream`.

Every resource is owned by exactly one process, and only its owner may operate on it.
Ownership **moves** when the handle is sent in a message or captured by a spawn; after the
move the original owner can no longer use it. When a process ends, the resources it still
owns are closed. Sharing therefore means keeping the handle in one process and sending
that process requests — which is what `%file` does.

### Failure as a value

Whether an I/O operation succeeds depends on the world, not on its arguments: no guard can
tell you in advance that a connect will be refused. So failure is a **value** — the result
type includes nil, and a failure is nil carrying an `IoError[kind, message]` under
`:error`. It short-circuits and propagates like any other nil.

```quiver ignore
sock = %tcp.connect [ip, port]                          // a failure ends the sequence
{ %tcp.connect [ip, port]; Connected | Unreachable }    // ... or a branch catches it
result ~> :('%io)error ~> =IoError(kind: ConnectionRefused) => retry
```

Failures that are not the world's doing stay runtime errors, because no caller could act
on them: an argument the operation rejects, an operation on a resource the process does
not own, a host with no backend for the effect.

"Found nothing" is a plain nil, as everywhere else — `%fs.stat` on a missing path answers
nil exactly as `%list.find` does. A *failed* lookup answers nil too; the `:error` payload
is the difference, and a caller that does not care may treat them alike.

Iteration is where the two nils meet: a step that ends the sequence means *exhausted*, and
a step over an I/O-backed source may instead have *failed*. Combinators pass the payload
through untouched, but a consumer that recovers in order to produce a value must check
first — recovering is exactly what discards the difference. `%iter.fold` does not check;
`%iter.try_fold` and `%list.try_collect` do.

### Streams

Some resources are **streams**: their next event arrives when it arrives, so they can be
select sources. A `\TcpSocket` yields `Data[sock, bytes] | Closed[sock]`, and a
`\TcpListener` yields `Accepted[listener, sock] | Closed[listener]`. A source that is
nothing but bytes-until-a-clean-end — an HTTP response body, say — is a `\ByteStream`,
yielding `Data[stream, bytes] | Closed[stream]` whatever produced it.

```quiver ignore
![sock]                    // the next event: a plain blocking read
![sock, 5000]              // ... with a timeout
![listener, &control]      // an accept loop that can also be told to stop
```

Reading a stream is fallible like any other I/O operation: a *failed* read — a reset, a
truncation — answers nil carrying `:error` in place of an event, so a select's type is
`'event | []` and `Closed` alone means the stream arrived whole. An unhandled failure
simply ends the sequence; a consumer for whom a dead stream is a dead stream catches the
nil and moves on.

Selecting is *pull*: a read is armed only while a select waits, so a busy process lets the
kernel buffer and TCP flow control throttle the sender. Events are chunks of bytes, not
protocol messages; framing belongs to the layer above.

## Metadata

**Annotations** attach typed key/value data *about* a tuple or function value. They are
invisible to the data plane: matching and equality ignore them, and they never survive
construction.

A block may open with `:key value` entries, before its first branch. They attach to what
the braces denote — a function literal's closure, or a chain block's result.

```quiver
div = #['int, 'int] {
  :doc "Integer division. Fails with :error on a zero divisor."
  | =[_, 0] => [] ~> { :error DivisionByZero }
  | %int.div
}

div [7, 2]                    //=> 3
div [4, 0]                    //=> []
div [4, 0] ~> :error          //=> DivisionByZero
div:doc                       //=> "Integer division. Fails with :error on a zero divisor."
```

A glued `:key` retrieves the annotation, or nil when absent. Since a failing sequence
short-circuits with the *same* nil, an `:error` payload survives out through calls, while
a recovering branch discards it along with the nil it replaces.

Bare retrieval compiles only where the annotation is statically visible. Inferred paths
keep that knowledge; explicitly declared types — function parameters, `(T)x` ascription —
shed it. The **checked form** `x:('t)key` states the expected shape and is total on any
carrier, answering nil when the key is absent or outside the shape.

```quiver
[x: 1] ~> :('int)nope         //=> []
```

### Contracts

`:pre` and `:post` are checked keys. For a function `#P -> R`, `:pre` is a `#P -> ok?` run
on the argument, and `:post` a `#[in: P, out: R] -> ok?` run afterwards. They are inert in
release builds and enforced in debug builds, where a nil verdict aborts.

```quiver
half = #'int {
  :pre #{ %num.gt? [~, 0] }
  :post #{ $ ~> =[in: i, out: o]; %num.gt? [i, o] }
  %int.div [~, 2]
}
half 10                       //=> 5
```

Contracts are read from the callee value, so they follow it through variables and module
members — but one shed at a declared boundary is no longer visible, and is not enforced.

### Assertions

`//=> P` at the end of a step asserts that the step's value matches the pattern `P`. Like
a contract, it is enforced in debug builds — a mismatch aborts, naming the site — and
skipped in release builds. It reads as a comment, and that is deliberate: this guide's own
result markers are assertions, so a document whose examples carry them is checked by
running it.

```quiver
double = #'int { %num.mul [$, 2] }
5 ~> double //=> 10
x = double 3 //=> Ok   a binding step's value is its verdict
x                      //=> 6
```

The value flows on unchanged — an asserted nil still ends its sequence — and a run of
three or more spaces after the pattern starts a prose note, ignored to the end of the
line. An assertion terminates its line, as the comment it resembles would: code may not
follow it. The pattern may not bind, since an assertion only observes; pins and type tests
cover most of what a binder would. A pattern that could never match the step's type is a
compile error, so a stale expectation fails the build even in release mode, where the
check itself costs nothing.

A `//=>` may also open its own line. A leading `//=>` continues the step above — it is
the trailing form with a line break, asserting that step's value — and several stack,
each observing the same value. At the start of a block there is no step to continue: the
assertion then observes the block's input — the function's parameter, the piped value —
exactly as a bare `~` step would. And in the REPL, where each entry starts from the
previous result, a `//=>` entry asserts the value just computed.

```quiver
double = #'int {
  //=> ('int)   observed on entry: the parameter
  %num.mul [~, 2]
}
double 5
//=> 10
double 5 ~> %num.add [~, 3] //=> 13
```

### Failure provenance

Debug builds — `quiv run`'s default; `--release` opts out — stamp each freshly-created nil
result with an `origin` annotation naming the site, which travels with the short-circuiting
nil and is shown wherever it surfaces:

```
[]  (match failed at shapes.qv:12:9)
```

Stamping is fresh-only, so a propagating failure keeps its original site, and positional,
so nil used as data is never stamped. Stamps are invisible to the type system — types are
identical across build modes — so they are read with a checked retrieval
(`x:((line: 'int))origin`), nil in a release build.

## Dialects

A module can define a **dialect**: `%mod{ … }` hands the braced text to that module at
compile time, and the module returns Quiver code. This is how markup and literal-heavy
notations get first-class syntax without the language growing it.

```quiver
%list{ 1, 2, 3 }                                    //=> Cons[1, Cons[2, Cons[3, Nil]]]
%dict{ "a" => 1, "b" => 2 } ~> %dict.get [~, "a"]   //=> 1
%json{ {"a": [1, 2]} } ~> %json.stringify          //=> "{\"a\":[1,2]}"
%html{ <p class="greeting">hi</p> } ~> %html.render //=> "<p class=\"greeting\">hi</p>"
```

The content is arbitrary — it is the dialect's grammar, not Quiver's — but holes are
ordinary Quiver: they are parsed by the host and evaluated in the caller's scope, with the
flowing value as input.

```quiver
name = "world"
%html{ <p>hello {name}</p> } ~> %html.render   //=> "<p>hello world</p>"
```

The standard library ships `%list{ … }`, `%dict{ … }`, `%json{ … }`, `%html{ … }` and
`%html/live{ … }`. A dialect is an ordinary exported function built from `%parse`
combinators, returning the code IR that `%meta` defines; `%html` exports its grammar seam
so other modules can layer their own attribute policies over it.

## Built-ins

Built-in functions are named with double underscores. The standard library wraps them, and
that is what programs should use — `%num.add` over `__integer_add__` — but they are
reachable directly.

```quiver
__integer_add__ [3, 4]              //=> 7
[add: &__integer_add__] ~> .add ~> ~ [3, 4]   //=> 7
```

`__panic__` aborts the process with a message.

## Standard library

| module | |
| --- | --- |
| `%num` | arithmetic over integers, exact rationals and single-radical surds |
| `%int` | integer-only operations: quotient, modulo, isqrt, bitwise |
| `%bin` | binary data |
| `%str` | strings: the `Str` type, splitting, joining, slicing, parsing |
| `%list` | singly-linked lists, and the eager combinators over them |
| `%iter` | lazy sequences, and the combinators over them |
| `%dict` | a persistent hash map keyed by data values, plus `%dict{ … }` |
| `%range` | integer ranges |
| `%vec` | packed fixed-point numeric vectors |
| `%ref` | mints unique opaque identifiers |
| `%data` | Quiver data notation: `encode` and `decode<'t>` |
| `%json` | JSON parsing and rendering, plus `%json{ … }` |
| `%parse` | parser combinators over binary input |
| `%meta` | the expression IR a dialect returns |
| `%proc` | process management: `detach`, `kill`, `link`, `track` |
| `%sup` | supervision: restart strategies over `%proc` |
| `%io` | the failure vocabulary every I/O operation shares |
| `%file` | files, via an owning process |
| `%fs` | the file system: stat, list, create, remove |
| `%path` | path values and manipulation |
| `%tcp` | TCP sockets and listeners, and their stream events |
| `%tls` | TLS as an in-place upgrade of a connected socket |
| `%pem` | PEM text to the DER bytes `%tls` takes |
| `%dns` | name resolution |
| `%url` | absolute URLs: parsing, rendering, resolution |
| `%http` | the shared HTTP vocabulary: messages and codecs, no I/O |
| `%http/client` | an HTTP client over `http://` and `https://` |
| `%http/server` | an HTTP server, with optional TLS |
| `%http/session` | signed-cookie sessions |
| `%http/websocket` | WebSocket server support |
| `%html` | an HTML grammar, node tree and renderer, plus `%html{ … }` |
| `%html/live` | live views: `%html/live{ … }`, frames and patches |
| `%hash` | cryptographic hashing |
| `%time` | clocks and calendar arithmetic |
| `%random` | randomness from the host's entropy source |

Per-function documentation lives in each module's `:doc` annotations, and is reachable in
the REPL.
