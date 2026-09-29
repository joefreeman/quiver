# The Quiver Guide

Quiver is a statically-typed functional language. Values are immutable and structurally
typed. Programs execute in a shared environment as lightweight processes with typed message receive.

```quiver program
// shapes.qv

// Define a shape type alias - a union of type tuples
'shape =
  | Square[side: 'int]
  | Rect[width: 'int, height: 'int]

// Define a function for calculating the area of a shape
area = #'shape {
  | =Square[side: s] => %num.mul [s, s]
  | =Rect[width: w, height: h] => %num.mul [w, h]
}

// Define the entry point of the program
#[] {
  %list{ Square[side: 3], Rect[width: 2, height: 4] }
  ~> %list.map [~, area]
  ~> %list.fold [~, init: 0, f: %num.add]
  //= 17
}
```

Run a program, like the one above, with `quiv run shapes.qv`. Use `quiv repl` to start an interactive session, or use the online REPL at [quiver.run](https://quiver.run). Short programs can be evaluated with: `quiv run -e '#[] { %num.add [2, 3] }'`.

Throughout, `//=` marks what an expression evaluates to — real syntax, a debug-checked
[assertion](#assertions) — and `//!` marks a step that is expected to *fail*, quoting part
of the error it must produce. Plain `//` is a comment. Every example that can run
standalone is checked against the compiler; a few that need the network, a second file, or
an elided body are marked and are not.

## Notation at a glance

Quiver uses a keyword-less syntax. The table below gives an overview of the symbols used - some are overloaded in different contexts:

| Symbol | Description | Example |
| --- | --- | --- |
| `~>` | pipe a value into the next term | `5 ~> double` |
| `~` | ripple: the value flowing in this chain; in a pattern, the part that flows on | `5 ~> [~, 1]` / `[5] ~> =[~]` |
| `;` | step separator (a newline is the same) | `a = 1; a` |
| `//` | comment, to end of line | `x = 1  // a note` |
| `//=` | assert the value flowing here, in debug builds | `5 ~> double //= 10` |
| `//!` | assert this step fails (documents only) | `5 ~> 99 //! must use the value` |
| `#` | function literal | `#'int { $ }` |
| `$` | the function's parameter | `#'int { $ }` |
| `$$` | the *enclosing* function's parameter | `#'int { #'int { $$ } }` |
| `&` | type intersection; pattern conjunction | `'t & 'u` / `=('int & n)` |
| `\` | negate a pattern | `5 ~> =\[]` |
| `=` | bind, or match | `x = 5` / `5 ~> =x` |
| `\|` | separates branches; union members; pattern alternatives | `{ =A => 1 \| 2 }` |
| `=>` | separates condition from consequence | `{ =0 => "zero" \| "other" }` |
| `'` | identifies a type | `'int`, `'point` |
| `%` | names a module | `%num.add` |
| `^` | tail call; recursion inside a type; pin: match against an existing value | `^ [n, acc]` / `5 ~> =^x` |
| `@` | spawn a process; the current process; a process type | `@worker`, `@`, `@'int` |
| `!` | select operator (receive a message, awaits a process, etc) | `!'int`, `!p` |
| `?` | sample a process's state | `?p` |
| `:` | field label; annotation key; type parameter bound | `[x: 1]`, `f:doc`, `#<'t: 'int>` |
| `.` | field access | `p.x` |
| `...` | spread operator | `[...p, y: 3]` |
| `+` | identifies a resource type | `+File` |
| `_` | ignore value in a pattern; the top type | `=[x, _]`, `#'int -> _` |
| `*` | bind every named field | `* = p` |

## Values

Two primitive value types can be written literally - integers and binaries (raw bytes) - and tuples build from them. All are immutable.

Integers are arbitrary precision, and never wrap:

```quiver
42                                  //= ('int)
-17                                 //= ('int)
%num.add [9223372036854775807, 1]   //= 9223372036854775808 // past a 64-bit word
```

Binaries are specified as an even number of hex digits between angle
brackets (`<>` is the empty binary). Spaces may divide the digits into groups:

```quiver
<0a1b>                   //= ('bin)
<>                       //= ('bin)
<
  428a2f98 71374491 b5c0fbcf e9b5dba5
  d807aa98 12835b01 243185be 550c7dc3
>                        //= ('bin)
```

### Tuples

A tuple can optionally have a name, and has zero or more fields, each of which may have a name. Tuple names start with an uppercase letter;
field labels are ordinary identifiers.

```quiver
[1, 2]                        //= [1, 2]
[x: 1, y: 2]                  //= [x: 1, y: 2]
[]                            //= []
Point[x: 1, y: 2]             //= Point[x: 1, y: 2]
Blue                          //= Blue
A[b: B[c: <01>]]              //= A[b: B[c: <01>]]
```

The nil tuple (`[]`) is used to represent falsey and failure. The `Ok` tuple is used by convention to represent truthy or success.

Named tuples without fields (e.g., `Blue`, `Ok`) can be used as symbols.

### Strings

`"..."` is sugar for UTF-8 bytes wrapped in a `Str` tuple, so a string is an ordinary
value with no special support in the language.

```quiver
"hello"                       //= Str[<68656c6c6f>]
Str[<68656c6c6f>]             //= "hello"
```

The escapes `\n`, `\r`, `\t`, `\\`, `\"` and `\{` are recognised. A `{ … }` hole
interpolates, and the value must evaluate to a string.

```quiver
name = "world"
"hello {name}"                //= "hello world"
"world" ~> "hello, {~}"       //= "hello, world"
"1 + 1 = {%str.from_int 2}"   //= "1 + 1 = 2"
```

A `"""`-delimited string spans lines. The closing delimiter's indentation sets a margin
stripped from every line, and the newline before it is not included:

```quiver
msg = """
    hello
      indented
    """
msg   //= "hello\n  indented"
```

Inside one, `\s` is a space that survives trailing-whitespace stripping, a trailing `\`
joins the next line, and `\"""` is a literal triple quote:

```quiver
msg = """
    foo\s
      bar\
      baz
    \"""
    """
msg   //= "foo \n  barbaz\n\"\"\""
```

### Names

Variables and field labels start with a lowercase letter, can be followed by letters, digits and
underscores, and may end in `?`, `!`, or both (by convention `?` for predicates and `!`
for emphasis).

## Flow

### Chains

A **chain** is a sequence of terms joined by `~>`. The value produced by one term becomes
the input to the next, and `~` — the **ripple** — refers to the value flowing in.

```quiver
3 ~> %num.add [~, 2] ~> %num.mul [~, ~]   //= 25
```

Every term after the first must use the value flowing into it. A name — of a function, or
of a [process](#processes) — does so by being applied to it: `5 ~> double` is `double 5`.

```quiver
double = #'int { %num.mul [$, 2] }

5 ~> double           //= 10 // a name is applied to the value
5 ~> 99               //! must use the value flowing into it // the 99 ignores the 5
5 ~> [1, 2]           //! must use the value flowing into it // so do both fields
```

Only a name after a `~>` is applied. One that starts a sequence — a step, a branch, a
consequence — is just its value, so `=> double` answers the function rather than calling it.

A chain can be spread over multiple lines by starting each line with a continuation (`~>`).
A comment or an [assertion](#assertions) may end any of those lines, the latter observing the
value flowing into the continuation below it.

```quiver
1                         //= 1
~> %num.add [~, 2]        //= 3
~> %num.mul [~, 3]        //= 9
```

### Steps

A newline (not followed by a continuation) or a `;` ends a chain and starts a new **step**. A run of steps is a
**sequence** - a program, a function body, a branch.

Two rules govern a sequence, and together they are most of Quiver's control flow:

1. Every step starts from the same value - the sequence's input. That is the enclosing
   block's input, except in a [consequence](#condition-and-consequence), which starts from
   its condition's value.
2. If a step fails, the rest of the sequence is skipped and the sequence evaluates to the
   nil that failed it. A step fails when a [match](#matching) in it fails, or when it
   doesn't end in a match and evaluates to nil.

```quiver
{ 1 }                     //= 1
{ 1; 2 }                  //= 2
3 ~> { 1; 2; ~ }          //= 3
```

Nil flows through a chain, but short-circuits a sequence:

```quiver
[] ~> [~, 5]              //= [[], 5] // one step; nil flows through the chain
{ []; 2 }                 //= [] // two steps; the first is nil, so it stops
```

### Building values in a chain

`~` can appear anywhere a value can, and can be repeated:

```quiver
0 ~> Point[x: ~, y: ~]        //= Point[x: 0, y: 0]
```

`...` spreads an existing tuple into a new one, with fields being merged:

```quiver
a = A[x: 1, y: 2]
a[..., y: 3]                  //= A[x: 1, y: 3] // keeps the name
a[..., z: 4]                  //= A[x: 1, y: 2, z: 4]
[...a, y: 3]                  //= [x: 1, y: 3] // drops the name
B[...a]                       //= B[x: 1, y: 2] // replaces it
[w: 0, ...a]                  //= [w: 0, x: 1, y: 2]
```

The source of a spread can be any access path — a variable, a field of one, the function
parameter, or the flowing value. In the name-preserving form the bracket is glued to its
source.

```quiver
n = [inner: P[x: 1, y: 2]]
n.inner[..., y: 5]            //= P[x: 1, y: 5]
B[...n.inner, y: 3]           //= B[x: 1, y: 3]
A[x: 1] ~> ~[..., y: 2]       //= A[x: 1, y: 2] // `~` as the source, name kept
A[x: 1] ~> [..., y: 2]        //= [x: 1, y: 2] // in-tuple, name dropped
```

When a tuple's fields are just variables of the same name, `( … )` **puns** them:

```quiver
x = 1; y = 2
(x, y)                        //= [x: 1, y: 2]
Point(x, y)                   //= Point[x: 1, y: 2]
```

An entry is a *name*, not an expression, so nothing flows into it. Entries may be access
paths, labelled by the final segment; `(…)` takes puns and nothing else.

```quiver
p = [x: 1, y: 2]
(p.x, p.y)                    //= [x: 1, y: 2]
```

### Field access

Use `.` to access a tuple's field, by name or by position:

```quiver
p = Point[x: 10, y: 20]
p.x                           //= 10
p.0                           //= 10
p ~> .y                       //= 20
[a: [b: 7]] ~> .a.b           //= 7
```

## Functions

A function has exactly one parameter, whose type is specified, and a result, whose type can optionally be specified, but otherwise is inferred.

The parameter can be referred to with `$`, and `$x` and `$0` are sugar for `$.x` and `$.0`.

```quiver
double = #'int { %num.mul [$, 2] }
sum = #['int, 'int] { %num.add [$0, $1] }

double 2                      //= 4
3 ~> double ~                 //= 6
sum [3, 5]                    //= 8
```

A function that takes a nil parameter (`#[] { … }`) is called explicitly `f []`:

```quiver
answer = #[] { 42 }
answer []                     //= 42
```

A function without a body is an identity function:

```quiver
#'int                         //= (#'int -> 'int)
```

A result type can be stated and is then checked: `#'int -> 'bin { … }`. It is also what a
call is typed as, even where the body's own type is narrower, so the body can change without
changing what its callers see:

```quiver
f = #'int -> ('int | 'bin) { 5 }
f 1 ~> %num.add [~, 1]        //! Type mismatch // f's callers see 'int | 'bin
```

### Reaching outer parameters

Additional `$`s can be used to reach outer parameters - for example `$$` or `$$user` to use the outer function's parameter.

```quiver
adder = #[by: 'int] { #'int { %num.add [$, $$by] } }
add3 = adder [by: 3]
add3 4   //= 7
```

### Applying

A call is a **juxtaposition**: a callable and the argument, separated by a space.

```quiver
%num.add [3, 4]               //= 7 // write the argument after the callee
[3, 4] ~> %num.add            //= 7 // ... or pipe it in
[3, 4] ~> %num.add ~          //= 7 // ... which is short for passing it as `~`
```

The flowing value can be used in either part:

```quiver
1 ~> %num.add [~, 2]          //= 3
%num.add ~> ~ [1, 2]          //= 3
%num ~> ~.add [1, 2]          //= 3
```

### Inferred parameters

A function parameter can be omitted when it can be inferred:

```quiver
%list{ 1, 2 } ~> %list.map [~, #{ %num.mul [$, 3] }]   //= Cons[3, Cons[6, Nil]]
```

### Tail calls

Tail calls (i.e., for recursion, reusing the stack frame) are made explicitly, using `^` - a bare `^` refers to the enclosing
function; `^f` names another.

```quiver
count_down = #['int, 'int] {
  | =[0, acc] => acc
  | =[n, acc] => ^ [%num.sub [n, 1], %num.add [acc, n]]
}
count_down [4, 0]             //= 10
```

Like any name after a `~>`, a tail call is applied to the flowing value, so
`… ~> ^` is `^ …`:

```quiver
power = #['int, 'int] {
  | =[0, acc] => acc
  | =[n, acc] => [%num.sub [n, 1], %num.mul [acc, 2]] ~> ^
}
power [3, 1]                  //= 8
```

A referenced function can be tail-called with `f ~> ^~`:

```quiver
double = #'int { %num.mul [$, 2] }
apply = #'int { double ~> ^~ $ }
apply 5                       //= 10
```

### Optional field labels

A parameter can mark any of its field labels **optional**, by wrapping the label in
parentheses. The caller may then leave them out, and the compiler fills them in — the
value is built fully labelled either way. Labelled fields may also be given out of order,
and the compiler puts them right.

```quiver
at = #[(x): 'int, (y): 'int] { [$x, $y] }
at [x: 1, y: 2]               //= [1, 2]
at [1, 2]                     //= [1, 2]
at [1, y: 2]                  //= [1, 2]
at [y: 2, x: 1]               //= [1, 2]
```

Fully labelled is also what the *body* sees, so a pattern over an optionally-labelled
parameter must read those fields by label. A positional pattern looks for fields the built
value does not have, and quietly matches nothing:

```quiver
at = #[(x): 'int, (y): 'int] { $ ~> { =[x: a, y: b] => [a, b] | Missed } }
at [1, 2]                     //= [1, 2]

positional = #[(x): 'int, (y): 'int] { $ ~> { =[a, b] => [a, b] | Missed } }
positional [1, 2]             //= Missed
```

Despite being specified on the type, the optionality belongs to the function itself. A spread parameter can be used to define the optionality on an existing type:

```quiver
'a = [x: 'int, y: 'int]
f = #[...'a, (x), (y)] { [$x, $y] }
f [1, 2]                      //= [1, 2]
```

### Parameter default values

A parameter field may also declare a **default**, and the caller may then omit it
entirely.

```quiver
open = #[(path): '%str, mode: (R | W | A) = R, buffer: 'int = 1024] { $ }
open ["f.txt"]                //= [path: "f.txt", mode: R, buffer: 1024]
open ["f.txt", mode: A]       //= [path: "f.txt", mode: A, buffer: 1024]
```

Similarly to field label optionality, the defaults are defined on the function itself, and the same spread approach can be used to define defaults on an existing type. They can alternatively be set with a `:defaults` annotation.

## Matching

A **match** is used for testing a value, creating bindings, or both. When it succeeds it
evaluates to the value it matched (or to the part of it a [ripple](#ripples-in-patterns) marks); when it fails, it fails its step, which ends the
sequence.

There are two variants of the syntax, which both work the same: `x = ...` and `... ~> =x`:

```quiver
x = 42                        //= 42
42 ~> =x                      //= 42 // match, in a chain
{ 42 ~> =41 }                 //= [] // and it can fail
```

Whether a step ending in a match carries on is the match's say, not its value's, so a match
that succeeds on nil is an ordinary success:

```quiver
{ [] ~> =[]; Matched }        //= Matched
{ 5 ~> =[]; Matched }         //= []
```

### Destructuring

Full or partial destructuring is supported. Partial destructuring happens with parentheses. And an asterisk can be used to assign all named fields to bindings.

```quiver
p = Point[x: 10, y: 20]

Point[x: a, y: b] = p; [a, b]   //= [10, 20] // bind both fields
(x, y) = p; [x, y]              //= [10, 20] // partial: named fields only
Point(x) = p; x                 //= 10 // named partial
* = p; [x, y]                   //= [10, 20] // every named field
Point* = p; [x, y]              //= [10, 20] // ... and require the name
```

A pattern field matches by label when it has one and by position when it does not, so a
positional pattern reads a positionally-built tuple:

```quiver
q = Point[10, 20]
Point[a, b] = q; [a, b]       //= [10, 20]
Point[m, _] = q; m            //= 10 // `_` ignores a field
```

A tuple pattern's name must correspond to the value's: a stated name requires that name,
and a pattern without one matches only an unnamed tuple — which makes `=[]`, the nil
literal, simply the rule's empty case: it never matches a named empty tuple. To
destructure a named tuple without stating its name, use a partial or star pattern.

```quiver
{ [x: a, y: b] = Point[x: 1, y: 2]; a }   //= [] // the name must be stated...
(x, y) = Point[x: 1, y: 2]; [x, y]        //= [1, 2] // ...or left to a partial
```

Literals inside a pattern test rather than bind:

```quiver
{ Point[x: 0, y: n] = Point[x: 0, y: 10] }   //= Point[x: 0, y: 10]
{ Point[x: 0, y: n] = Point[x: 1, y: 10] }   //= []
```

### Testing types and pinning

A type name is always a reference — types are never bound — so `='int` tests. To test
against an existing *value*, prefix it with `^`.

```quiver
42 ~> ='int                   //= 42
{ <01> ~> ='int }             //= []

y = 2
2 ~> =^y                      //= 2
{ 3 ~> =^y }                  //= []
Point[1, 2] ~> =Point[x, ^y]  //= Point[1, 2] // binds x, checks y is 2
```

A type test answers by what the value is known to be where it was built, without looking
inside it. A value built at a wider type fails a narrower test even when its contents would
fit, and a tuple pattern, which tests each field, is the precise alternative:

```quiver
'single = Cons['int, Nil]
xs = %data.decode<'%list<'int>> "Cons[1, Nil]"   // built as a list of any length
{ xs ~> ='single }                               //= []
xs ~> =Cons[_, Nil]                              //= Cons[1, Nil]
Cons[1, Nil] ~> ='single                         //= Cons[1, Nil]
```

A pin's target may be any access path — a field of a variable, or of the enclosing
function's parameter via `$`.

```quiver
p = Point[x: 1, y: 2]
1 ~> =^p.x                    //= 1
same? = #[x: 'int, y: 'int] { $y ~> =^$x }
same? [x: 3, y: 3]            //= 3
```

### Alternation

A parenthesised, `|`-separated list matches if any alternative does. Every alternative
must bind the same variables (if any).

```quiver
[[], 5] ~> =([[], _] | [_, []])   //= [[], 5]
42 ~> =('int | 'bin)              //= 42
```

### Conjunction

A parenthesised, `&`-separated list matches if every part does — the pattern reading of a
type [intersection](#intersections), as alternation is of a union. Its parts may bind
different variables, and a bare name among them binds the whole value at the type the other
parts narrowed it to, wherever it is written:

```quiver
42 ~> =('int & n); n            //= 42
{ [] ~> =('int & n) }           //= [] // nil is not an int, so this fails
Point[x: 1, y: 2] ~> =(Point[x: a, y: _] & p); [a, p]   //= [1, Point[x: 1, y: 2]]
```

This is the idiom for "bind, but fail on the wrong type", which combines with
short-circuiting to propagate.

`&` binds tighter than `|`, as it does in types, so an alternation used as a part is
parenthesised, and each alternative of a union may carry its own binder:

```quiver
9 ~> =((32 | 9 | 10 | 13) & b); b          //= 9
[[], 2] ~> =(([x, []] | [[], x]) & p); p   //= [[], 2]
B[2] ~> =(A[n] & v | B[n] & v); [n, v]     //= [2, B[2]]
```

### Negation

`\` negates a pattern: `\P` matches exactly when `P` does not. It applies to any pattern,
nested anywhere a pattern can be.

```quiver
5 ~> =\[]                         //= 5 // not nil
<01> ~> =\'int                    //= <01>
{ 42 ~> =\42 }                    //= []
A[b: <01>] ~> =A[b: \'int]         //= A[b: <01>]
2 ~> =\(0 | 1)                    //= 2
```

When a negation matches, its pattern didn't, so there is nothing to bind inside one. A
binder outside it is fine, and a negated type narrows to what is left: `(\[] & x)` binds `x`
and fails if the value is nil, without having to name the value's type.

```quiver
succ = #('int | []) { (\[] & n) = $; %num.add [n, 1] }
succ 4                        //= 5
succ []                       //= []
5 ~> =\x                      //! cannot bind
```

### Ripples in patterns

A match evaluates to the whole value it matched, so after destructuring, the chain still
carries the whole. A ripple in a pattern marks the part that should flow on instead:

```quiver
[42] ~> =[~]                          //= 42
Ok[41] ~> =Ok[~] ~> %num.add [~, 1]   //= 42
[x: 1, y: 2] ~> =(y: ~)               //= 2
```

A ripple is typed at its position, so the chain after it is checked against what was
there. Like a repeated binder, a repeated ripple requires the values at each to be equal:

```quiver
[1, [1, 2], 3] ~> =[~, [~, 'int], _]   //= 1
{ [1, [2, 2], 3] ~> =[~, [~, _], _] }  //= []
```

A ripple follows the binding rules: every alternative of an alternation must have one or
none may, and a negation may not contain one. A bare `~` is the whole value, so it is how an
alternative passes its value on unchanged:

```quiver
unwrap = #(Ok['int] | Other) { =(Ok[~] | ~) }
unwrap Ok[3]                  //= 3
unwrap Other                  //= Other
```

### Where a match may appear

A match may stand anywhere a term can: mid-chain, in a tuple field, as a call's argument.
Wherever it stands, a match that fails fails its **step**, dropping whatever the step had
built so far, so everything after the match — the rest of the chain, the tuple's later
fields — runs only when it matched, and may rely on its bindings.

```quiver
42 ~> =42 ~> %num.add [~, 1]     //= 43
{ 42 ~> =41 ~> %num.add [~, 1] } //= []
f = #('int | 'bin) { [$ ~> =('int & n), %num.add [n, 1]] }
f 5                              //= [5, 6]
f <01>                           //= []
```

A block contains the failure, being nil when it fails, which is how to keep a match's
outcome as data:

```quiver
x = 5
[int?: x ~> { ='int }, bin?: x ~> { ='bin }]   //= [int?: 5, bin?: []]
```

## Branching

A block, `{ … }`, introduces a scope and holds branches. Branches are separated by `|`,
and every branch starts from the block's input, so a value piped into a block is shared by
all of them.

```quiver
B[42] ~> { =A[a] => 1 | =B[b] => 2 }   //= 2
```

If a branch's sequence fails, control moves to the next branch; if there are none left, the
block is nil. Branches are therefore fallbacks, tried in order:

```quiver
label = #'int {
  | =0; "zero"    // the match fails unless the input is 0 ...
  | "other"       // ... and control moves to the next branch
}
0 ~> label ~      //= "zero"
1 ~> label ~      //= "other"
```

### Condition and consequence

`cond => body` is a branch that commits: if `cond` succeeds, `body` runs and the block is
done, even if `body` itself fails. If `cond` fails, control moves on as usual.

```quiver
sign = #'int {
  | =0 => "zero"
  | %num.gt? [~, 0] => "positive"
  | "negative"
}
0 ~> sign ~                     //= "zero"
5 ~> sign ~                     //= "positive"
-5 ~> sign ~                    //= "negative"
```

The consequence starts from the condition's value, as a branch without one evaluates to it.
A match evaluates to what it matched, so after one the consequence starts from the block's
input as before — unless a [ripple](#ripples-in-patterns) picks out part of it. A condition
that finds something hands it on, with nil ruled out:

```quiver
Lit[5] ~> { | =Lit[~] => %num.add [~, 1] | 0 }     //= 6

ages = %dict{ "ada" => 36 }
older = #'%str { ages ~> { | %dict.get [~, $] => %num.add [~, 1] | 0 } }
older "ada"                     //= 37
older "bob"                     //= 0
```

Every step of the consequence starts from that value, as the steps of any sequence start
from its input.

A condition is a *sequence*, so a step separator adds a guard: the match is one step and
the guard the next, and the boundary short-circuits, so the guard sees the pattern's
bindings only when it matched.

```quiver
'shape = Square[side: 'int] | Circle[radius: 'int]

size = #'shape {
  | =Square[side: s]; %num.gt? [s, 10] => "large"
  | "small"
}
Square[side: 20] ~> size ~      //= "large"
Square[side: 2] ~> size ~       //= "small"
Circle[radius: 1] ~> size ~     //= "small"
```

The condition's value is its last step's, so after a guard the consequence would start from
the guard's answer. To pass the block's input on instead, end the condition with a `~` step
— or, if the input may be nil (which fails a plain step), with a match, which succeeds on it:

```quiver
5 ~> { | %num.gt? [~, 3] => [~, 1] | No }      //= [Ok, 1] // the guard's answer
5 ~> { | %num.gt? [~, 3]; ~ => [~, 1] | No }   //= [5, 1]
[] ~> { | =[]; =_ => [~, 1] | No }             //= [[], 1]
```

To test that a pattern does *not* match, [negate](#negation) it:

```quiver
not_square? = #'shape { =\Square() }
Circle[radius: 1] ~> not_square? ~   //= Circle[radius: 1]
Square[side: 1] ~> not_square? ~     //= []
```

### Scope

Bindings made in a block are local to it, and are cleared between branches.

```quiver
x = 42
{ x = 5 }
x                             //= 42
```

## Types

Most types are inferred. Only function parameters, type aliases and the occasional
assertion are written; results, locals and intermediate values are worked out.

A type name carries a leading `'`, which is what distinguishes it from a variable: `'int`
is a type, `int` a variable. Tuple names need no prefix, as their capital letter already
sets them apart.

There are three primitive types: `'int`, `'bin`, and `'ref` (unique opaque identifiers,
covered under [Refs](#refs)). Tuples compose them, and functions, processes and resources
are each their own kind, with their own notation — `#'int -> 'bin`, `@'int`, `+File`.

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
origin Point[x: 0, y: 0]      //= [0, 0]
```

An alias is a step of a sequence like any other, so it can be declared inside a block or
a function body, where it is scoped to that body and can name the enclosing function's
type parameters. Aliases are positional: a definition must precede its uses.

```quiver
h = #<'t>'t {
  'pair = ['t, 't]
  [$, $] ~> =('pair & p)
  p
}
h 3                           //= [3, 3]
```

### Unions

`|` joins alternatives, and matching is how one is taken apart.

```quiver
'bool = True | False
'shape = Circle[radius: 'int] | Rectangle[width: 'int, height: 'int]
```

Nil is an ordinary member, so an optional `'t` is written `'t | []`.

### Partial types

A partial type constrains some fields and says nothing about the rest. It is written in
parentheses, and every field must be named.

```quiver
'positioned = (x: 'int, y: 'int)

nudge = #'positioned { %num.add [$x, 1] }
nudge Point[x: 1, y: 2, z: 3]   //= 2
```

`()` matches any tuple, and `Point()` any tuple named `Point`.

A last entry `*'t` states a **rest type**, which every field the partial does not list
must have — positional fields included. The listed fields keep their own types:

```quiver
'row = (id: 'int, *'%str)
label = #'row { $id }
label [id: 7, "a", name: "b"]   //= 7
label [id: 7, 8]                //! Type mismatch // 8 is not a string
```

`(*_)` constrains nothing, so it is `()` again.

### The top type

`_` is the type of every value — the type-level reading of the `_` that ignores a value in
a pattern. It is written where a type does not matter, most often the result of a callback
that is only tested for nil:

```quiver
keep? = #[(x): 'int, (pred): #'int -> _] { | $pred $x => Kept | Dropped }
keep? [3, #{ %num.gt? [$, 2] }]   //= Kept
keep? [3, #{ "any answer" }]      //= Kept
keep? [3, #{ [] }]                //= Dropped
```

Nothing is known about a `_` value, so it cannot be used as anything narrower. It can be
passed on, or matched, which is how it is narrowed back to something usable. A tuple
pattern tests for its own shape:

```quiver
describe = #_ {
  | =('int & n) => %num.add [n, 1]
  | =Point[x: ('int & x), y: _] => x
  | 0
}
describe 5                        //= 6
describe Point[x: 1, y: 2]        //= 1
describe <01>                     //= 0

id = #_ { $ }
id 5 ~> %num.add [~, 1]           //! Type mismatch
```

Nil is a member, so a `_` result short-circuits a sequence like any fallible one. `_`
absorbs any union it is part of, and leaves any type it is intersected with unchanged.

### Recursive types

`^` refers back to the nearest enclosing **boundary**, a union or a function type (a tuple is
not one). `^1`, `^2` and so on count further outward from there, so `^` is `^0`.

```quiver
'list<'t> = Nil | Cons['t, ^]
'tree<'t> = Leaf['t] | Node[^, ^]
```

In a nested type, a reference counts the boundaries around it where it is written:

```quiver
'json = Null | 'int | Array[(Nil | Cons[^1, ^])]
render = #'json { Ok }
Array[Cons[1, Cons[Null, Nil]]] ~> render ~   //= Ok
```

Here the list's own `Nil | Cons[…]` is nearest, so `^` makes the tail a list. `^1` is one
further out, `'json`, so an element may be any JSON value. Because a reference counts from
where it stands, a type keeps its meaning when it is moved or pulled out into an alias of its
own. Naming a boundary the type does not have is a compile error.

A function type counts too: the thunk below answers a pair whose tail is another thunk, with
the result's union nearest and the function one further out.

```quiver
'thunk = #[] -> (['int, ^1] | [])
```

A union member that is `^` itself would name its own union, adding nothing, so it is a
compile error. An optional recursive field reaches past its `( … | [])`:

```quiver
'bintree = Leaf | Node[left: (^1 | []), right: (^1 | [])]
f = #'bintree { $ }
f Node[left: Leaf, right: []]   //= Node[left: Leaf, right: []]
```

A function literal's written parameter and result sit inside its own function type, so a
`^` in a parameter's field names the function being defined. That is how a function is handed
itself: it cannot name its own binding, and `^` in a body is only ever a tail call, so
recursion elsewhere passes the function along. The body sees the parameter at the function's
type, so the result must be written.

```quiver
countdown = #[(self): ^, (n): 'int] -> '%list<'int> {
  | $n ~> =0 => Nil
  | Cons[$n, $self [$self, %num.sub [$n, 1]]]
}
countdown [countdown, 3]   //= Cons[3, Cons[2, Cons[1, Nil]]]
```

### Generics

Type parameters are declared in angle brackets, on an alias or a function, and are
inferred from usage.

```quiver
'pair<'a, 'b> = Pair[first: 'a, second: 'b]

id = #<'t>'t { $ }
id 42                         //= 42
id "hi"                       //= "hi"
```

They can also be pinned explicitly, with the argument list glued to the name. This is a
checked assertion rather than a hint: it narrows what the use site sees, and a mismatch is
a compile error.

```quiver
id<'int> 42                   //= 42
f = id<'int>; f 7             //= 7
id<'int> "hi"                 //! Type mismatch
```

A prefix may be given and the rest left inferred. A builtin can be *type-consuming*, in
that its behaviour depends on the type argument itself, and must then always be
instantiated where it is applied — `%data.decode<'t>` is one.

Inside its declaration, a type parameter is one unknown type. Its values can be passed on,
matched, or used wherever `'t` itself is expected, but not as anything narrower:

```quiver
inc = #<'t>'t { %num.add [$, 1] }   //! Type mismatch // 't might not be a number
```

#### Bounds

A parameter may state an upper bound, `'t: 'bound`. Every instantiation must fit it, inferred
or written, and inside the declaration the parameter's values can be used as the bound's. The
parameter still stands for the caller's own type, so a result of type `'t` keeps everything
the bound says nothing about:

```quiver
'dated = (at: 'int)
latest = #<'t: 'dated>['t, 't] { | %num.gt? [$0.at, $1.at] => $0 | $1 }

latest [Post[at: 3, title: "a"], Post[at: 5, title: "b"]] ~> .title   //= "b"
latest [Post[title: "a"], Post[title: "b"]]   //! bounded by (at: 'int)
```

A bound may name the parameters declared before it. An alias's parameters can be bounded
too, and a generic that applies a bounded alias to one of its own parameters must bound the
parameter at least as tightly — `%dict`'s keys are data, so a function generic in them says so:

```quiver
first_key = #<'k: '%data, 'v>'%dict<'k, 'v> { %dict.keys $ ~> %list.head }
first_key %dict{ "a" => 1 }   //= "a"
```

A field update (`$[..., x: 0]`) builds the bound's shape rather than `'t`: whatever the
caller's `'t` holds in that field may be narrower than what is written there. A spread copies
fields by their layout, so the bound must be a concrete tuple rather than a partial:

```quiver
reset = #<'t: Point[x: 'int, y: 'int]>'t { $[..., x: 0] }
reset Point[x: 3, y: 4]       //= Point[x: 0, y: 4]
touch = #<'t: (at: 'int)>'t { $[..., at: 0] }   //! no fixed layout
```

### Spreads

Types compose with `...` the way values do, and later fields override earlier ones.

```quiver
'entity = [id: 'int, created_at: 'int]
'post = Post[...'entity, title: '%str]

make = #'post { $title }
make Post[id: 1, created_at: 0, title: "hi"]   //= "hi"
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

echo = #'rw { $read <01> }
echo [read: #'bin { Ok }, write: #'bin { Ok }]   //= Ok
```

Two partials meet in a partial constraining both field sets, and a field both constrain
takes the intersection of the two constraints. A partial meeting a concrete tuple keeps
the tuple, with the constrained fields narrowed. Disjoint members intersect to nothing, so
`'int & 'bin` is uninhabited and matches no value.

In matching, each member is checked separately:

```quiver
[x: 1, y: 2] ~> =((x: 'int) & (y: 'int) & v); v.x   //= 1
```

### Data notation

**Quiver data notation** is the literal syntax restricted to data: integers, binaries and
tuples, plus the `"…"` sugar. The `%data` module is its codec.

```quiver
%data.encode Point[x: 1, y: "hi"]              //= "Point[x: 1, y: \"hi\"]"
```

```quiver
'msg = Ping | Pong['int]
%data.decode<'msg> "Pong[3]"                   //= Pong[3]
%data.decode<'msg> "Nope"                      //= []
```

`decode` is driven by the expected type: names in the text resolve only against that
type's members, so decoding can never produce a shape the program does not already
contain. Anything malformed or mismatched answers nil, like a failed match.

Strings do not interpolate in data — a `{` is literal, and `encode` escapes it so encoded
text also reads as literal. Values with identity (refs, pids, functions, resources) have
no notation at all. The values that do are the type `'%data` — `'int | 'bin | (*^)`, the
integers, binaries, and tuples whose fields are data, by a [rest type](#partial-types).
`encode` takes one, as do `%dict` and `%registry` keys, and it is the [bound](#bounds) for a
parameter that must be data:

```quiver
%ref [] ~> %data.encode ~     //! Type mismatch
key = #<'k: '%data>'k { %data.encode $ }
key Point[x: 1]               //= "Point[x: 1]"
```

## Modules

A module is a `.qv` file whose value, usually a tuple of functions, is what importing it
gives you. `%name` imports it.

```quiver
num = %num
num.add [1, 2]                //= 3
```

```quiver
(add, mul) = %num             // just these two
add [1, 2]                    //= 3
```

```quiver
* = %num                      // every named export
mul [3, 4]                    //= 12
```

A member can also be reached inline, without binding anything, which is the style used
throughout this guide:

```quiver
%num.add [1, 2]               //= 3
```

Modules are evaluated at compile time, and the result is what gets imported. That
evaluation must be deterministic and produce identity-free values, so minting refs and
reading the clock are rejected there. Both belong in the module's functions, which run
when the caller calls them. A program's own top level has no such restriction, as it runs
at startup with the full runtime.

### Types from modules

`'%mod` is a module's **default type**, its nameless definition, and `'%mod.name` a named
one. A module's type namespace is its top-level aliases; one declared inside a function
body is private to the module.

```quiver
'strings = '%list<'%str>
count = #'strings { %list.count $ }
count Cons["a", Cons["b", Nil]]   //= 2
```

Inside the module that defines it, a module's own default type is a bare `'`, or `'<'t>`
when it takes type parameters. This is how `%list` and `%dict` refer to themselves
(`' = Nil | Cons['t, ^]` is declared `'<'t> = …`).

### Writing one

A module is a sequence like any other, so it is written as a run of definitions ending in
the value it exports:

```quiver ignore
// shapes.qv
' = Circle[radius: 'int] | Rect[width: 'int, height: 'int]   // the default type

'boxed = Boxed[']                                            // a named type, over `'`

[
  area: #' {
    :doc "The shape's area."
    | =Circle[radius: r] => r ~> %num.pow [~, 2] ~> %num.mul [%num.pi, ~]
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
a = @worker 21                // the init argument, after the function
b = 21 ~> @worker             // or piped in
[!a, !b]                      //= [42, 42]
```

The init is written like a call's argument, and for the same reason: a nilary root
function is entered with nil, and that nil is spelled out as `@f []`.

The spawned function is always glued to the `@`. `@'int { … }` is shorthand for spawning a
function literal, `@[] { … }` for a nilary one, `@<'t>'t { … }` for a generic one, and
`@~` spawns a function that is itself the flowing value (`f ~> @~ []`). A root function's
parameter is the process's state, so it is written rather than inferred, and there is no
`@{ … }`.

A bare `@`, with nothing glued to it, is the **current process** — what `$` is to the
parameter and `~` to the flowing value.

### Sending, receiving, awaiting

Send by applying the pid to the message, `p x`, as a function is applied to its argument.
Receive with `!`, whose parameter type names the message type. Await a process's result
with `!` on the pid.

```quiver
p = @[] { 42 } []
!p                            //= 42
```

```quiver
adder = #[] { !#['int, 'int] ~> %num.add ~ }
q = @adder []
q [3, 4]; !q                  //= 7
```

A receive shapes the process's message type, and sending is checked against it: the
message must fit the target's send grant, and a pid that might be one of several
processes accepts only what every one of them takes. Sending is asynchronous, answering
`Ok` without waiting, and sending to self is just `@ x`.

Awaiting is never lethal, so its result is fallible: `'r | []` for a process returning
`'r`. A crashed process answers nil carrying a `:crash` annotation rather than propagating
the error, so an ordinary branch recovers from it.

```quiver
crashed = @[] { __panic__ "boom" } []
r = !crashed
r ~> {
  | =('int & v) => v                                       // completed
  | r:crash<Panic(message: '%str)> ~> =(message: m) => m   // crashed
  | "no result"                                            // legitimately nil
}   //= "boom"
```

The payload is `Error[pid, message]`, `Panic[pid, message]` or `Killed[reason]`, where the
reason is whatever the killer passed to `%proc.kill`, or nil.

### Sampling state

Every process has an observable **state**: the argument its root function was most
recently entered with, which is the spawn init and then each tail call in the root frame.
`?` samples it, and never waits.

```quiver
'status = Loading | Done['int]
step = #'status { =Loading => 7 ~> ^ Done[~] | =Done[x] => x }
p = Loading ~> @step ~
?p ~> ='status                //= ('status) // Loading, then Done[7]
```

The sample's type is inferred at the spawn site: the root function's own parameter type
(the spawn init) united with the parameter types of its tail calls. There is no runtime
test, as every state write is compile-checked.

### Capabilities in written types

A pid's capabilities are inferred, but a *declared* type grants only what it spells. Each
grant is written as the operation that exercises it, in this order: a glued head type is
the send grant (what applying the pid takes), `!'r` the await grant (what selecting on the
pid yields), and `?'s` the sample grant (what `?` reads). A clause sigil glues to a bare
`@` and takes a space after anything else:

```quiver
'status = Loading | Done['int]
step = #'status { =Loading => 7 ~> ^ Done[~] | =Done[x] => x }
p = Loading ~> @step ~

watch = #(@?'status) { ?$ }         // sample-only
await = #(@!'int) { !$ }            // awaitable
watch p ~> ='status                //= ('status)
await p                            //= 7
```

No parentheses are needed where the type ends at a natural boundary, such as a tuple field
`[room: @'post ?'msgs]` or a type argument `%registry.lookup<@'int !'int>`. A clause always
binds to the nearest sigil-head on its left, so in a function's output position a
clause-bearing process type is parenthesized: in `#'init -> @ !'cmd` the clause is the
function's own.

A function type takes `!` and `?` clauses in that order, where `!'c` is what running it
receives and `?'d` the states beyond its parameter. This is what matters when the function
is to be spawned: `#'config -> 'r !'cmd ?'connected`. The two sides mirror the `!`
expression, in that a function type's clause reads from inside the process (what a select
in the body receives) and a process type's from outside (what a select on the pid yields).

An omitted clause is **not granted**, so `?` on a plain `@'msg` is a compile error. Sends
are contravariant (a process that receives more fits a narrower promise), the await result
is covariant, and state is covariant and strict. A state-less process never satisfies a
stated `?`, which is what keeps the test-free `?p` sound, and a boundary may deliberately
state a coarser state union than the implementation's to keep internal phases private.

### Ownership

`@f` creates a child **owned** by the spawning process. When a process ends, normally or
not, its owned children are torn down with it, cascading down the subtree. Teardown never
travels upward, so a child's death is only ever observed by its parent.

```quiver
p = @[] { !'int } []
%proc.link p       //= Ok // fate-sharing: either dying abnormally kills the other
%proc.detach p     //= Ok // relinquish ownership; p outlives this process
%proc.kill [p]     //= Ok // terminate p and its subtree
```

A kill may give a reason, which its awaiters read from the `Killed` payload. The reason
must be data (no pids, refs, functions or resources), as it outlives the killer.

```quiver
p = @[] { !'int } []
%proc.kill [p, reason: Expired]
r = !p
r:crash<'%proc.crash>   //= Killed[reason: Expired]
```

### The registry

Pids normally travel by hand, captured at the spawn or passed in messages. Processes with
no shared ancestor — another session on a shared environment, a detached service — instead
meet through the **registry**: a per-environment table binding data-value keys to live
processes. A key is any data value, such as `"chat"` or `Worker[shard: 3]`, compared
structurally. A value with identity (a pid, a ref, a function) anywhere in a key is a
runtime error.

A lookup states the process type it expects, and the type argument is required, like
`%data.decode`'s. It is checked at runtime against the registered process with the usual
variance rules, so what a name grants is exactly what the lookup spells. An unbound key or
a failed check answers nil, like any failed match.

```quiver
p = @[] { !'int ~> %num.mul [~, 2] } []
%registry.register [Doubler, p]              //= Ok
%registry.register [Doubler, p]              //= [] // the name is taken
%registry.lookup<@'bin> Doubler              //= [] // wrong message type
%registry.lookup<@'int> Doubler ~> =(@'int & q)
q 21
!p                                           //= 42
%registry.lookup<@'int> Doubler              //= [] // freed when the process ended
```

Names free at termination, whatever the cause, kill and cascade included, so the registry
only ever answers live processes and a restarted service simply re-registers. Anyone who
already looked a pid up is unaffected: a held pid awaits and reads as usual, tombstone
semantics included. `%registry.unregister` removes a binding early.

### Select

`!` generalises to a **select** over several sources, racing them. The general form is
`![sources]`, an ordinary value tuple in which a name is the value it names.

```quiver ignore
![p1, p2, 5000]   // whichever of two processes finishes first, or a 5s timeout
![sock, control]  // socket bytes, or a mailbox message
p1 ~> ![~, 1000]  // in a chain, via `~`
```

Sources are processes (await), functions by reference (receive), integers (a timeout in
milliseconds), and stream resources. Order is the tie-break, so when several are ready at
once the earliest wins. A timed-out select answers nil carrying `:timeout`, so a fallback
can tell a timeout from a crash from a legitimate nil.

The shorthands each select on one source:

| form | means |
| --- | --- |
| `!p`, `!f`, `!%mod.recv` | `![p]` — await, or receive |
| `!'int`, `!Done`, `!(…)` | `![#'int]` — receive a message of that type |
| `!#['int, 'int]` | an unnamed tuple type needs the `#` |
| `![]` | a no-op, returning nil |

A select that loses to anything, a timeout included, leaves its other sources as they
were: a message stays in the mailbox, and an awaited process keeps running. Because a
timeout's nil ends a sequence, waiting for its own sake is `%proc.sleep`, not a bare
`![ms]`.

### Timers and deadlines

`%proc` builds timers from processes, so ownership gives them their semantics: a timer
belongs to whoever set it, dies with them, and is cancelled by killing it.

```quiver
tick = #[] {
  %proc.send_after [@, Tick, after: %time{ 10ms }]   // answers the timer, a process
  %proc.sleep %time{ 20ms }                          //= Ok
  !Tick
}
tick []                                    //= Tick
```

`%proc.expire` gives a process a deadline. A process still running when it passes is
killed with a `Timeout` reason, and a watchdog whose process finishes first simply ends.

```quiver
p = @[] { %proc.sleep %time{ 1s }; Done } []
%proc.expire [p, %time{ 10ms }]
r = !p
r:crash<'%proc.crash>                      //= Killed[reason: Timeout[Duration[10000000]]]
```

`%proc.race` runs thunks in processes of their own and answers the first non-nil result,
killing the others. It answers nil once every racer has failed.

```quiver
%proc.race [%list{ #[] { %proc.sleep %time{ 500ms }; Slow }, #[] { Fast } }]   //= Fast
```

### Filters and handlers

A block written **directly after** a select is a *filter*: it inspects each candidate
message and decides whether to accept it. Nil skips the message, leaving it in the mailbox.
A block joined with `~>` is an ordinary chain step — a *handler* applied to the message
already received.

```quiver
filter = @[] { !'int { =42 => Ok | [] } } []      // waits specifically for 42
filter 1; filter 42
!filter                       //= 42 // the 1 is still in the mailbox
```

```quiver
handler = @[] { !'int ~> { =42 => Ok | [] } } []  // takes any int, then tests it
handler 1
!handler                      //= []
```

A filter may be re-evaluated, so its verdict must be stable. Spawning, sending, nested
selects, host-state reads and ref minting are all rejected inside one.

### Refs

`%ref` is not a record but a single nilary function, so every evaluation of it mints a
fresh unique value of type `'ref`.

```quiver
tag = %ref []
[tag, 42] ~> =[^tag, x]; x    //= 42
```

```quiver
a = %ref []; b = %ref []
{ a ~> =^b }                  //= [] // distinct refs are not equal
```

## Resources and failure

Some operations produce **resources** — opaque handles to external state. A resource type
is written `+Name`: `+File`, `+Dir`, `+TcpSocket`, `+TcpListener`, `+DnsResolver`,
`+ByteStream`.

Every resource is owned by exactly one process, and only its owner may operate on it.
Ownership **moves** when the handle is sent in a message or captured by a spawn, and after
the move the original owner can no longer use it. When a process ends, the resources it
still owns are closed, except those in its result. A result is a message to whoever awaits
it, so those move to the first awaiter, and are closed if nobody ever can: when the
process's owner ends, or once no pid is left to await it by. Sharing therefore means
keeping the handle in one process and sending that process requests, which is what
`%file` does.

### Failure as a value

Whether an I/O operation succeeds depends on the world rather than on its arguments, so no
guard can establish in advance that a connect will not be refused. Failure is therefore a
**value**: the result type includes nil, and a failure is nil carrying an
`IoError[kind, message]` under `:error`. It short-circuits and propagates like any other
nil.

```quiver ignore
sock = %tcp.connect [ip, port]                          // a failure ends the sequence
{ %tcp.connect [ip, port]; Connected | Unreachable }    // ... or a branch catches it
result ~> :error<'%io> ~> =IoError(kind: ConnectionRefused) => retry
```

Failures that are not the world's doing stay runtime errors, because no caller could act
on them: an argument the operation rejects, an operation on a resource the process does
not own, a host with no backend for the effect.

"Found nothing" is a plain nil, as everywhere else, and `%fs.stat` on a missing path
answers nil exactly as `%list.find` does. A *failed* lookup answers nil too, with the
`:error` payload as the only difference, and a caller that does not care may treat them
alike.

Iteration is where the two nils meet: a step that ends the sequence means *exhausted*, and
a step over an I/O-backed source may instead have *failed*. Combinators pass the payload
through untouched, but a consumer that recovers in order to produce a value must check
first, since recovering is exactly what discards the difference. `%iter.fold` does not
check; `%iter.try_fold` and `%list.try_collect` do.

### Streams

Some resources are **streams**: their next event arrives when it arrives, so they can be
select sources. A `+TcpSocket` yields `Data[sock, bytes] | Closed[sock]`, and a
`+TcpListener` yields `Accepted[listener, sock] | Closed[listener]`. A source that is
nothing but bytes-until-a-clean-end, such as an HTTP response body, is a `+ByteStream`,
yielding `Data[stream, bytes] | Closed[stream]` whatever produced it.

```quiver ignore
![sock]                   // the next event: a plain blocking read
![sock, 5000]             // ... with a timeout
![listener, control]      // an accept loop that can also be told to stop
```

Reading a stream is fallible like any other I/O operation. A *failed* read — a reset, a
truncation — answers nil carrying `:error` in place of an event, so a select's type is
`'event | []` and `Closed` alone means the stream arrived whole. An unhandled failure
simply ends the sequence, and a consumer for whom a dead stream is a dead stream catches
the nil and moves on.

Selecting is *pull*: a read is armed only while a select waits, so a busy process lets the
kernel buffer and TCP flow control throttle the sender. Events are chunks of bytes rather
than protocol messages, and framing belongs to the layer above.

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
  | %int.div ~
}

div [7, 2]                    //= 3
div [4, 0]                    //= []
div [4, 0] ~> :error          //= DivisionByZero
div:doc                       //= "Integer division. Fails with :error on a zero divisor."
```

A glued `:key` retrieves the annotation, or nil when absent. Since a failing sequence
short-circuits with the *same* nil, an `:error` payload survives out through calls, while
a recovering branch discards it along with the nil it replaces.

Bare retrieval compiles only where the annotation is statically visible. Inferred paths
keep that knowledge, and explicitly declared types — function parameters, a type tested in
a match (`('t & x)`) — shed it. The **checked form** `x:key<'t>` states the expected shape and is
total on any carrier, answering nil when the key is absent or outside the shape.

```quiver
[x: 1] ~> :nope<'int>         //= []
```

### Contracts

`:pre` and `:post` are checked keys. For a function `#P -> R`, `:pre` is a `#P -> _` run on
the argument, and `:post` a `#[in: P, out: R] -> _` run afterwards; only whether the answer is
nil counts. They are inert in release builds and enforced in debug builds, where a nil answer
aborts.

```quiver
half = #'int {
  :pre #{ %num.gt? [~, 0] }
  :post #{ %num.gt? [$in, $out] }
  %int.div [~, 2]
}
half 10                       //= 5
```

Contracts are read from the callee value, so they follow it through variables and module
members. One shed at a declared boundary is no longer visible, and is not enforced.

### Assertions

`//= P` at the end of a line asserts that the value flowing there matches the pattern `P`.
Like a contract, it is enforced in debug builds — a mismatch aborts, naming the site — and
skipped in release builds. It reads as a comment, and that is deliberate: this guide's own
result markers are assertions, so a document whose examples carry them is checked by
running it.

```quiver
double = #'int { %num.mul [$, 2] }
5 ~> double ~ //= 10
x = double 3  //= 6
x             //= 6
```

An assertion ends its line, so what follows one is either the next step or a `~>`
continuation — which is what lets a chain spread over lines assert on each of them:

```quiver
1                         //= 1
~> %num.add [~, 2]        //= 3
~> %num.mul [~, 3]        //= 9
```

The value flows on unchanged, so an asserted nil still ends its sequence. An assertion ends
at its pattern, and the only thing that may follow it on the line is an ordinary `//`
comment — which is how a step carries a note explaining it, and it is a comment like any
other rather than part of the assertion. Anything else after the pattern is an error, so a
pattern that runs on (`//= Point [x: 1]`) cannot quietly shrink to a weaker one with prose
after it.

```quiver
p = Point[x: 1, y: 2]
p[..., y: 3] //= Point[x: 1, y: 3] // the name is kept
```

The pattern may not bind, since an assertion only observes, and pins and type tests cover
most of what a binder would. A pattern that could never match the value's type is a compile
error, so a stale expectation fails the build even in release mode, where the check itself
costs nothing.

An assertion after a match observes the matched value, on the match's success: a failed
match has left its step, so nothing flows there. To assert that a match fails, contain it in
a block, which is nil when it does: `{ e ~> =P } //= []`.

A `//=` may also open its own line. A leading `//=` continues the line above — it is the
trailing form with a line break — and several stack, each observing the same value. At the
start of a block there is no line to continue, so the assertion instead observes the
block's input (the function's parameter, or the piped value) exactly as a bare `~` step
would. In the REPL, where each entry starts from the previous result, a `//=` entry
asserts the value just computed.

```quiver
double = #'int {
  //= ('int) // observed on entry: the parameter
  %num.mul [~, 2]
}
double 5
//= 10
double 5 ~> %num.add [~, 3] //= 13
```

A check runs only where control reaches it: one inside a function body runs when the
function is *called*, not where it is written, and one in an untaken branch or after a failed
step never runs at all. `quiv test` counts the assertions it saw hold, and reports the rest of
those written as **not run**, rather than as passed.

### Expected failures

`//! text` is the assertion's opposite: the step must *fail*, and the error must mention
`text`. A bare `//!` accepts any failure. Unlike `//=` it is not part of the language —
to the compiler it is an ordinary comment — because the step it marks is often one the
compiler rejects, and rejected code cannot carry a compiled check. It is read by
`quiv test`, so it means something in a document and nothing in a `.qv` file.

```quiver
5 ~> 99                        //! must use the value flowing into it
__panic__ "boom"               //! boom
```

A `//` ends the expected text and starts a note, as after a `//=` pattern. The marker
matters more here, the expectation being prose itself: nothing else would say where the
checked text stops.

```quiver
5 ~> 99   //! must use the value flowing into it // the 99 ignores the 5
```

Either kind of error works: a compile error leaves the session untouched, and a runtime one
kills the process, so the document's session is rebuilt behind it and the chapter goes on.

### Failure provenance

Debug builds — `quiv run`'s default, with `--release` opting out — stamp each
freshly-created nil result with an `origin` annotation naming the site. It travels with
the short-circuiting nil and is shown wherever it surfaces:

```
[]  (match failed at shapes.qv:12:9)
```

Stamping is fresh-only, so a propagating failure keeps its original site, and positional,
so nil used as data is never stamped. Stamps are invisible to the type system, as types
are identical across build modes, so they are read with a checked retrieval
(`x:origin<(line: 'int)>`) that answers nil in a release build.

## Dialects

A module can define a **dialect**: `%mod{ … }` hands the braced text to that module at
compile time, and the module returns Quiver code. This is how markup and literal-heavy
notations get first-class syntax without the language growing it.

```quiver
%list{ 1, 2, 3 }                                      //= Cons[1, Cons[2, Cons[3, Nil]]]
%dict{ "a" => 1, "b" => 2 } ~> %dict.get [~, "a"]     //= 1
%json{ {"a": [1, 2]} } ~> %json.stringify ~           //= "{\"a\":[1,2]}"
%html{ <p class="greeting">hi</p> } ~> %html.render ~ //= "<p class=\"greeting\">hi</p>"
```

The content is arbitrary, being the dialect's grammar rather than Quiver's, but holes are
ordinary Quiver: they are parsed by the host and evaluated in the caller's scope, with the
flowing value as input.

```quiver
name = "world"
%html{ <p>hello {name}</p> } ~> %html.render ~   //= "<p>hello world</p>"
```

The standard library ships `%num{ … }`, `%list{ … }`, `%dict{ … }`, `%json{ … }`,
`%time{ … }`, `%html{ … }` and `%html/live{ … }`. A dialect is an ordinary exported
function, usually built from `%parse` combinators, returning the code IR that `%meta`
defines. `%html` exports its grammar seam so other modules can layer their own attribute
policies over it.

## Built-ins

Built-in functions are named with double underscores. The standard library wraps them, and
that is what programs should use — `%num.add` over `__integer_add__` — but they are
reachable directly.

```quiver
__integer_add__ [3, 4]                       //= 7
[add: __integer_add__] ~> .add ~> ~ [3, 4]   //= 7
```

`__panic__` aborts the process with a message.

## Standard library

| module | |
| --- | --- |
| `%num` | exact arithmetic over integers, rationals, single-radical surds, π, e and logarithms, with exact trigonometry |
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
| `%json` | JSON parsing, rendering, querying and editing, typed `decode<'t>`/`encode<'t>`, plus `%json{ … }` |
| `%parse` | parser combinators over binary input |
| `%meta` | the expression IR a dialect returns |
| `%proc` | process management: `detach`, `kill`, `link`, `track`, and timers: `sleep`, `send_after`, `expire`, `race` |
| `%registry` | the per-environment name registry: `register`, `unregister`, `lookup` |
| `%sup` | supervision: restart strategies over `%proc` |
| `%io` | the failure vocabulary every I/O operation shares |
| `%file` | files, via an owning process |
| `%fs` | the file system: whole-file read and write, stat, list, create, copy, rename, remove, symlinks, permissions, temp dirs |
| `%path` | path values and manipulation |
| `%tcp` | TCP sockets and listeners, and their stream events |
| `%tls` | TLS as an in-place upgrade of a connected socket |
| `%pem` | PEM text to the DER bytes `%tls` takes |
| `%dns` | name resolution |
| `%url` | absolute URLs: parsing, rendering, resolution |
| `%http` | the shared HTTP vocabulary: messages and codecs, no I/O |
| `%http/tcp` | HTTP/1.1 over a socket, with the TLS upgrade `https://` needs |
| `%http/client` | an HTTP client over `http://` and `https://` |
| `%http/server` | an HTTP server, with optional TLS |
| `%http/session` | signed-cookie sessions |
| `%http/websocket` | WebSocket server support |
| `%html` | an HTML grammar, node tree and renderer, plus `%html{ … }` |
| `%html/live` | live views: `%html/live{ … }`, frames and patches |
| `%hash` | cryptographic hashing |
| `%time` | instants, civil dates and times, durations and periods, clocks, and `%time{ … }` |
| `%random` | randomness from the host's entropy source |

Per-function documentation lives in each module's `:doc` annotations, which an editor
surfaces through the language server. Each module also has a reference page under
`std/docs/`, which is that module's test suite as well as its documentation — every
example on it is executed, and its `//=` assertions checked, by `quiv test`.
