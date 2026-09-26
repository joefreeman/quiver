# %parse

Parser combinators over binary input, with position-tracked failures.

A **parser** is a function from a parse state to `[value, state]` — what it read, and the
state to carry on from. It fails to nil carrying an `Expected[offset, message]` payload under
`:error`, so a failed parse short-circuits its sequence like any other nil, and the position
travels with it.

```quiver ignore
'perr = Expected[offset: 'int, message: Str['bin]]
' = P[data: 'bin, pos: 'int, len: 'int, err: ('perr | [])]
'p<'t> = #' -> (['t, '] | [])
```

Parsers are ordinary values. A combinator takes them as arguments and answers a new one, so a
grammar is built by naming its pieces and composing them — no declaration form, no separate
grammar language.

```quiver
comma = %parse.byte [44, "','"]
elems = %parse.sep_by [%parse.int, comma]
list = %parse.between [%parse.byte [91, "'['"], elems, %parse.byte [93, "']'"]]
%parse.run ["[1,2,3]", list]   //= Cons[1, Cons[2, Cons[3, Nil]]]
```

That failure contract is also the one a **dialect** function reports to the compiler, so a
`%mod{ … }` grammar built from these combinators gets positioned compile errors for nothing.

## Running a parser

`run` applies a parser to a whole string and answers its value.

```quiver
%parse.run ["42", %parse.int]   //= 42
%parse.run ["-7", %parse.int]   //= -7
```

The parse must reach the end of the input. Bytes left over are a failure, not a partial
success — `run` is a match against the whole string, not a scan of its front.

```quiver
%parse.run ["12x", %parse.int]   //= []
```

The nil carries the position the parse got to. Annotations are not statically visible through
a declared result type, so the payload is read with a **checked retrieval**, naming the shape
expected:

```quiver
r = %parse.run ["12x", %parse.int]
r:error<'%parse.perr>   //= Expected[offset: 2, message: "end of input"]
```

`'%parse.perr` is `Expected[offset: 'int, message: Str['bin]]`, the offset counted in bytes
from the start of the input.

## Primitive parsers

`byte` matches one exact byte and answers it. The second field describes it in failure
messages, and is written the way it should read there.

```quiver
%parse.run ["(", %parse.byte [40, "'('"]]   //= 40
```

`satisfy` matches one byte a predicate accepts, and yields whatever the predicate answered
for it — so recognising and decoding are one step rather than two.

```quiver
digit = %parse.satisfy [#'int { %num.ge? [$, 48]; %num.le? [$, 57]; %num.sub [$, 48] }, "a digit"]
%parse.run ["7", digit]   //= 7
```

`take_while` takes the longest run of bytes a predicate accepts, as a slice of the input —
raw bytes, not a `Str`. It always succeeds, an empty run being a run.

```quiver
lower? = #'int { %num.ge? [$, 97]; %num.le? [$, 122]; Ok }
%parse.run ["abc", %parse.take_while lower?]   //= <616263>
```

`ws` is the whitespace case of the same idea: zero or more spaces, tabs, newlines and carriage
returns, always succeeding.

```quiver
%parse.run ["  7", %parse.right [%parse.ws, %parse.int]]   //= 7
```

`literal` matches an exact string, answering `Ok` — the bytes are already known, so there is
nothing else to say.

```quiver
%parse.run ["let", %parse.literal ["let", "'let'"]]   //= Ok
```

Three parsers cover the tokens most notations need: `int` reads an optionally signed decimal
integer, `ident` a name, and `quoted` a double-quoted string.

```quiver
%parse.run ["-17", %parse.int]         //= -17
%parse.run ["hello_1", %parse.ident]   //= "hello_1"
%parse.run ["\"hi\"", %parse.quoted]   //= "hi"
```

`ident` is exactly the host language's identifier grammar — a lowercase start, then letters,
digits and underscores, with optional `?` then `!` suffixes.

```quiver
%parse.run ["xY_9z", %parse.ident]    //= "xY_9z"
%parse.run ["valid?", %parse.ident]   //= "valid?"
%parse.run ["go!", %parse.ident]      //= "go!"
%parse.run ["ok?!", %parse.ident]     //= "ok?!"
```

`quoted` recognises `\" \\ \/ \b \f \n \t \r`, and nothing else — there is no `\u`. The
result is a `Str` over whatever bytes those escapes produced, which need not be text.

```quiver
%parse.run ["\"a\\nb\"", %parse.quoted]     //= "a\nb"
%parse.run ["\"a\\b\\f\"", %parse.quoted]   //= Str[<61080c>] // backspace, form feed
```

## Sequencing

`then` runs two parsers in order and pairs their values. `left` and `right` do the same and
keep one side, which is what punctuation calls for.

```quiver
a = %parse.byte [97, "'a'"]
b = %parse.byte [98, "'b'"]
%parse.run ["ab", %parse.then [a, b]]    //= [97, 98]
%parse.run ["ab", %parse.left [a, b]]    //= 97
%parse.run ["ab", %parse.right [a, b]]   //= 98
```

`between` is the bracketing case: three parsers, keeping the middle one's value.

```quiver
open = %parse.byte [40, "'('"]
close = %parse.byte [41, "')'"]
%parse.run ["(7)", %parse.between [open, %parse.int, close]]   //= 7
```

`map` applies a function to a parser's value, which is how a parse tree gets built at all.

```quiver
%parse.run ["21", %parse.map [%parse.int, #{ %num.mul [$, 2] }]]   //= 42
```

## Alternatives

`alt2`, `alt3` and `alt4` try their arguments in order and take the first that succeeds. Only
one value comes out, so every alternative must answer the same type.

```quiver
yes = %parse.map [%parse.literal ["yes", "'yes'"], #{ 1 }]
no = %parse.map [%parse.literal ["no", "'no'"], #{ 0 }]
maybe = %parse.map [%parse.literal ["maybe", "'maybe'"], #{ -1 }]
p = %parse.alt3 [yes, no, maybe]
%parse.run ["yes", p]     //= 1
%parse.run ["no", p]      //= 0
%parse.run ["maybe", p]   //= -1
```

Alternatives of different shapes meet in a union, and the union is inferred — each branch
contributes its own shape to the result type.

```quiver
num = %parse.map [%parse.int, #{ Num[$] }]
txt = %parse.map [%parse.quoted, #{ Text[$] }]
p = %parse.alt2 [num, txt]
%parse.run ["42", p]       //= Num[42]
%parse.run ["\"hi\"", p]   //= Text["hi"]
```

Naming the token type is still worth doing where the grammar has one, and pinning it makes
each branch's mapping explicit:

```quiver
'tok = Num['int] | Text[Str['bin]]
num = %parse.map<'int, 'tok> [%parse.int, #{ Num[$] }]
txt = %parse.map<Str['bin], 'tok> [%parse.quoted, #{ Text[$] }]
p = %parse.alt2<'tok> [num, txt]
%parse.run ["42", p]       //= Num[42]
%parse.run ["\"hi\"", p]   //= Text["hi"]
```

An alternative that fails costs nothing: the state is a value, so a rejected branch simply
leaves the original state to the next one. Backtracking is the absence of mutation, not a
mechanism.

## Repetition

`many0` runs a parser as many times as it will go, collecting a list; zero times is a
result, not a failure.

```quiver
item = %parse.left [%parse.int, %parse.byte [59, "';'"]]
%parse.run ["1;2;", %parse.many0 item]   //= Cons[1, Cons[2, Nil]]
%parse.run ["", %parse.many0 %parse.int]   //= Nil
```

`sep_by` is the separated form: elements interleaved with a separator whose value is
discarded.

```quiver
comma = %parse.byte [44, "','"]
elems = %parse.sep_by [%parse.int, comma]
%parse.run ["1,2", elems]   //= Cons[1, Cons[2, Nil]]
%parse.run ["", elems]      //= Nil
```

A separator not followed by an element is **left unconsumed**, so `sep_by` alone rejects a
trailing comma — the comma is still there when `run` checks for the end of the input.

```quiver
comma = %parse.byte [44, "','"]
elems = %parse.sep_by [%parse.int, comma]
%parse.run ["1,2,", elems]   //= []
```

Because the separator is put back rather than consumed, the caller decides what a trailing one
means. `opt` — which turns a failure into `None` without consuming anything — makes it
optional:

```quiver
comma = %parse.byte [44, "','"]
elems = %parse.sep_by [%parse.int, comma]
%parse.run ["1,2,", %parse.left [elems, %parse.opt comma]]   //= Cons[1, Cons[2, Nil]]
```

`sep_end_by` is that composition, named:

```quiver
comma = %parse.byte [44, "','"]
%parse.run ["1,2,", %parse.sep_end_by [%parse.int, comma]]   //= Cons[1, Cons[2, Nil]]
%parse.run ["1,2", %parse.sep_end_by [%parse.int, comma]]    //= Cons[1, Cons[2, Nil]]
```

`opt` on its own wraps success as `Some` and failure as `None`, so a caller can tell an absent
part from a present one without either being a failure:

```quiver
maybe = %parse.opt %parse.int
%parse.run ["5", maybe]                            //= Some[5]
%parse.run ["", maybe]                             //= None
%parse.run ["5", maybe] ~> { =Some[n] => n | 0 }   //= 5
%parse.run ["", maybe] ~> { =Some[n] => n | 0 }    //= 0
```

## Operators

`chainl` is the precedence-level combinator: one or more operands separated by an operator
parser whose *value is the combining function*, folded left-associatively.

```quiver
minus = %parse.map [%parse.byte [45, "'-'"], #{ %num.sub }]
%parse.run ["10-3-2", %parse.chainl [%parse.int, minus]]   //= 5
```

Left association is the point — `(10 − 3) − 2` is 5, where the right-associated reading would
be 9. One operand and no operator is still a chain:

```quiver
minus = %parse.map [%parse.byte [45, "'-'"], #{ %num.sub }]
%parse.run ["10", %parse.chainl [%parse.int, minus]]   //= 10
```

Alternating operators at one level is an `alt` over the operator parser, so each yields its
own function:

```quiver
plus = %parse.map [%parse.byte [43, "'+'"], #{ %num.add }]
minus = %parse.map [%parse.byte [45, "'-'"], #{ %num.sub }]
op = %parse.alt2 [plus, minus]
%parse.run ["1+2-4+8", %parse.chainl [%parse.int, op]]   //= 7
```

## Recursive grammars

A grammar that nests refers to itself, and a Quiver binding cannot be used before it exists.
`rec` ties the knot: it takes a **builder** — a function of the grammar's own parser and the
state to parse from — and answers the parser that builder describes.

```quiver
value = #['%parse.p<'int>, '%parse] {
  =[value, st]
  parens = %parse.between [%parse.byte [40, "'('"], value, %parse.byte [41, "')'"]]
  core = %parse.alt2 [%parse.int, parens]
  core st
} ~> %parse.rec ~

%parse.run ["7", value]       //= 7
%parse.run ["((7))", value]   //= 7
```

The builder receives the parser it is defining, so `value` inside the body is the same parser
`rec` hands back — and each entry rebuilds the combinators from it, which is what keeps the
recursion finite.

### A worked example: arithmetic

Parentheses, `+` and `-`, evaluated as it parses — `rec` for the nesting, `chainl` for the
associativity, and `map` to turn each operator byte into the function that applies it. The
parser's value type is `%num.add`'s result, `'%num | []`: as a function value it covers every
`'%num`, including the sums it refuses with an `:error`.

```quiver
expr = #['%parse.p<('%num | [])>, '%parse] {
  =[expr, st]
  parens = %parse.between [%parse.byte [40, "'('"], expr, %parse.byte [41, "')'"]]
  atom = %parse.alt2 [%parse.int, parens]
  plus = %parse.map [%parse.byte [43, "'+'"], #{ %num.add }]
  minus = %parse.map [%parse.byte [45, "'-'"], #{ %num.sub }]
  op = %parse.alt2 [plus, minus]
  p = %parse.chainl [atom, op]
  p st
} ~> %parse.rec ~

%parse.run ["1+2+3", expr]      //= 6
%parse.run ["10-3-2", expr]     //= 5
%parse.run ["10-(3+2)", expr]   //= 5
```

## Failures

`fail` makes a positioned failure by hand, and `fail_in` makes one at a state's own position.
They are what a hand-written parser reports with, and what every combinator above is built on.

```quiver
%parse.fail [3, "a digit"] ~> :error<'%parse.perr>   //= Expected[offset: 3, message: "a digit"]
```

Nothing about a parser is privileged: it is a function of the state, and the state is an
ordinary tuple, so one written by hand composes with the combinators exactly as `int` does.

```quiver
any = #'%parse {
  | %num.lt? [$pos, $len] => [%bin.get_byte [$data, $pos], $[..., pos: %num.add [$pos, 1]]]
  | %parse.fail_in [$, "any byte"]
}
%parse.run ["A", any]   //= 65
r = %parse.run ["", any]
r:error<'%parse.perr>   //= Expected[offset: 0, message: "any byte"]
```

`label` replaces a parser's failure message with one of your own, reported at the position
where that parser *started* — which is how a grammar names a construct rather than leaking the
byte-level expectation that happened to fail first.

```quiver
r = %parse.run ["x", %parse.label [%parse.int, "a count"]]
r:error<'%parse.perr>   //= Expected[offset: 0, message: "a count"]
```

### The furthest failure

A failure reports the position that got **furthest** through the input, even across
backtracking. Whenever a combinator turns a failure into a success — an alternative moving on,
a repetition stopping, `sep_by` or `opt` putting input back — the discarded failure is stashed
in the state, and every fresh failure merges itself against the stash. On a tie the fresher,
more local message wins.

This is what makes an error useful. `alt2` over `int` and `quoted` on `"ab` fails, and the
message names the unterminated string at offset 3 — the branch that got somewhere — not the
number that failed immediately at 0.

```quiver
r = %parse.run ["\"ab", %parse.alt2 [%parse.int, %parse.quoted]]
r:error<'%parse.perr>   //= Expected[offset: 3, message: "'\"' (unterminated string)"]
```

The same holds through backtracking. `sep_by` on `"1,2,x"` backtracks over the trailing comma
and succeeds with two elements, and `run` then rejects the leftover input at offset 3 — but the
error surfaced is the element's, at 4:

```quiver
comma = %parse.byte [44, "','"]
r = %parse.run ["1,2,x", %parse.sep_by [%parse.int, comma]]
r:error<'%parse.perr>   //= Expected[offset: 4, message: "a number"]
```

And through `opt`, whose whole job is to discard a failure:

```quiver
ab_cd = %parse.then [%parse.literal ["ab", "'ab'"], %parse.literal ["cd", "'cd'"]]
r = %parse.run ["abx", %parse.opt ab_cd]
r:error<'%parse.perr>   //= Expected[offset: 2, message: "'cd'"]
```

`opt` succeeded with `None` at offset 0 and `run` failed there, so without the stash the
message would be "end of input" at 0 — true, and useless. What the writer wants to know is that
`ab` was followed by something that was not `cd`.

## Termination

A parser that succeeds without consuming input would make a repetition loop forever, so every
repeating combinator recurses only on **progress**. `many0`, `sep_by` and `chainl` each stop
when a round leaves the position where it found it, and the zero-width value is discarded — it
matched nothing, so it contributes nothing.

`sep_by` over two always-succeeding parsers terminates rather than hanging; it consumes
nothing, and `run` then rejects the input it never looked at.

```quiver
%parse.run ["abc", %parse.sep_by [%parse.ws, %parse.ws]]   //= []
```

A `take_while` inside a `many0` is the ordinary case of the same thing: the first round takes
the digits, the second matches empty and stops, and there is no phantom trailing element.

```quiver
digit? = #'int { %num.ge? [$, 48]; %num.le? [$, 57]; Ok }
p = %parse.take_while digit? ~> %parse.many0 ~
%parse.run ["12", p]   //= Cons[<3132>, Nil]
```

`chainl` guards the same way, over the operator and operand together:

```quiver
digit? = #'int { %num.ge? [$, 48]; %num.le? [$, 57]; Ok }
digits = %parse.take_while digit?
op = %parse.map [%parse.ws, #{ %bin.concat }]
%parse.run ["57", %parse.chainl [digits, op]]   //= <3537>
```
