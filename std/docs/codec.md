# %codec

Conversion between types. `%codec.convert<'t> [value, using: [f, g, …]]` converts a value to
`'t` through the functions it is given, each an **edge** from its parameter type to its result
type, together with the structural conversions every type has. It answers `'t | []`.

```quiver
show = #'int -> '%str { %str.from_int $ }
%codec.convert<'%str> [42, using: [show]]   //= "42"
```

It is the half of serialisation that is about types rather than text: a format module parses
text into its own document model (`%json.parse`), and `convert` takes that model to the
program's own types, through the functions a format supplies for its standard shapes and the
ones a program writes for its domain.

## Source type

The source type is the value's static type, or stated as a second type argument. A source
union is converted member by member, so each member needs a route of its own, and one that
turns up without one answers nil:

```quiver
%codec.convert<'int, ('int | '%str)> [7, using: []]        //= 7
%codec.convert<'int, ('int | '%str)> ["7", using: []]      //= []
```

A conversion that no value of the source could make is a compile error:

```quiver
%codec.convert<'int> ["7", using: []]   //! no route from Str['bin] to 'int
```

A value that already belongs to the target passes through, and ahead of the functions, so
none is applied to it. Belonging is tested on the value, so one of a wider type passes when
it happens to fit:

```quiver
show = #'int -> '%str { %str.from_int $ }
%codec.convert<('int | '%str)> [42, using: [show]]   //= 42

name = #'%json { %codec.convert<'%str> [$, using: []] }
name %json{ "Ada" }                                  //= "Ada"
name %json{ 36 }                                     //= []
```

## Functions

A function whose parameter the value fits is applied, and its answer converted on to the
target. Functions are tried in the order given, ahead of the structural conversions.

```quiver
decimal = #'int -> '%str { %str.from_int $ }
hex = #'int -> '%str { %str.from_int [$, radix: 16] }
%codec.convert<'%str> [255, using: [decimal, hex]]   //= "255"
%codec.convert<'%str> [255, using: [hex, decimal]]   //= "ff"
```

A function answering nil has **declined**, and the next alternative is tried. So a function
can be partial, and `%str.parse_int` — nil for text that isn't a number — is an edge as it
stands:

```quiver
%codec.convert<'int, ('int | '%str)> ["42", using: [%str.parse_int]]   //= 42
%codec.convert<'int, ('int | '%str)> ["4x", using: [%str.parse_int]]   //= []

big = #'int -> ('%str | []) { %num.gt? [$, 100] => "big" }
%codec.convert<'%str> [500, using: [big, decimal]]   //= "big"
%codec.convert<'%str> [5, using: [big, decimal]]     //= "5"
```

A function's parameter may be wider than the value, and its result narrower than the target:

```quiver
%codec.convert<('%str | 'int)> [5, using: [#('int | 'bin) -> '%str { "num" }]]   //= 5
%codec.convert<Wrapped[_]> [5, using: [#_ -> Wrapped['int] { Wrapped[1] }]]   //= Wrapped[1]
```

Any function value is an edge, closures included, so a configured edge is an ordinary call:

```quiver
prefixed = #'%str { #'int -> '%str { %str.concat [$$, %str.from_int $] } }
%codec.convert<'%str> [5, using: [prefixed "#"]]   //= "#5"
```

The functions must be a tuple of functions:

```quiver
%codec.convert<'int> [5, using: [1]]   //! must all be functions
```

```quiver
%codec.convert<'int> [5, using: 3]   //! must be a tuple
```

## Structure

A tuple converts to a target tuple with the same name, labels and positions, each field
converted on its own — so a function written for a field's type converts it wherever it
stands:

```quiver
show = #'int -> '%str { %str.from_int $ }
%codec.convert<Point[x: '%str, y: '%str]> [Point[x: 1, y: 2], using: [show]]
//= Point[x: "1", y: "2"]
%codec.convert<['%str, 'int]> [[1, 2], using: [show]]   //= ["1", 2]
%codec.convert<[a: [b: '%str]]> [[a: [b: 3]], using: [show]]   //= [a: [b: "3"]]
```

The name and labels must agree, as a type test's do:

```quiver
%codec.convert<Vec[x: '%str]> [Point[x: 1], using: [show]]   //! no route
```

```quiver
%codec.convert<[x: '%str]> [[y: 1], using: [show]]   //! no route
```

A value reaches a target union through whichever member it can, tried in order:

```quiver
'shape = Circle[radius: '%str] | Square[side: '%str]
%codec.convert<'shape> [Square[side: 4], using: [show]]   //= Square[side: "4"]
%codec.convert<('%str | Point[x: '%str])> [Point[x: 1], using: [show]]   //= Point[x: "1"]
```

Recursive types convert by the same rule, so a list converts element by element:

```quiver
%codec.convert<'%list<'%str>> [%list{ 1, 2, 3 }, using: [show]]
//= Cons["1", Cons["2", Cons["3", Nil]]]
%codec.convert<'%list<'%list<'%str>>> [%list{ %list{ 1 }, Nil }, using: [show]]
//= Cons[Cons["1", Nil], Cons[Nil, Nil]]

'tree<'t> = Leaf['t] | Node[^, ^]
%codec.convert<'tree<'%str>> [Node[Leaf[1], Node[Leaf[2], Leaf[3]]], using: [show]]
//= Node[Leaf["1"], Node[Leaf["2"], Leaf["3"]]]
```

A long list converts without growing the native stack:

```quiver
xs = %range.to 20000 ~> %range.iter ~> %list.collect
%codec.convert<'%list<'%str>> [xs, using: [show]] ~> ='%list<'%str> ~> %list.count
//= 20000
```

Nil inside a value is ordinary data, and fits wherever the target allows it:

```quiver
%codec.convert<[a: ('%str | [])]> [[a: []], using: [show]]   //= [a: []]
%codec.convert<[a: '%str], [a: ('int | [])]> [[a: []], using: [show]]   //= []
```

## Chaining

A route may chain functions. Forward, the answer of one is the input of the next:

```quiver
'tree = Leaf['%str] | Node[^, ^]
show = #'int -> '%str { %str.from_int $ }
leaf = #'%str -> Leaf['%str] { Leaf[$] }
%codec.convert<'tree> [Node[1, 2], using: [show, leaf]]   //= Node[Leaf["1"], Leaf["2"]]
```

Backward, a function whose answer reaches the target is applied to the value once it has been
converted to what the function takes. This is how a domain tuple is reshaped into what an
encoding function expects:

```quiver
'pair = Pair['%str, '%str]
render = #Pair['%str, '%str] -> '%str { %str.concat [$0, %str.concat ["/", $1]] }
%codec.convert<'%str> [Pair[1, 2], using: [render, show]]   //= "1/2"
```

Chains compose freely, so supplying an edge widens what converts — here a string reaches an
integer field because `%str.parse_int` is among the functions:

```quiver
%codec.convert<[n: 'int]> [[n: "12"], using: [%str.parse_int]]   //= [n: 12]
```

Each function is tried aimed at the target first — converting the value to what it takes, so
that its answer lands on the target — and only then applied to the value as it stands, its
answer converted on. A generic function differs between the two: aimed, its type variables
come from the target; applied, from the value.

A function is never applied twice in a chain without a structural step between, so routes
always end, even through a function that answers its own type:

```quiver
inc = #'int -> 'int { %num.add [$, 1] }
%codec.convert<'%str> [5, using: [inc, show]]   //= "6"
%codec.convert<'%str> [5, using: [show, inc]]   //= "5"
```

## Generic functions

A generic function is instantiated against the types it meets, so ordinary std functions
are edges. A dict is a hash map built of tuples, which a structural conversion would walk like
any other — fine for its values, but converting a key that way would leave it filed under the
old key's hash. Its own functions take it to a list of pairs and back, and the list converts
structurally between them, keys included:

```quiver
show = #'int -> '%str { %str.from_int $ }
d = %dict{ 1 => "a", 2 => "b" }
%codec.convert<'%dict<'%str, '%str>> [d, using: [%dict.entries, %dict.from, show]]
~> ='%dict<'%str, '%str> ~> [%dict.get [~, "1"], %dict.get [~, "2"]]
//= ["a", "b"]
```

A generic function can reshape a tuple without caring what its fields hold, leaving the fields
to convert afterwards:

```quiver
'user = User[name: '%str, age: 'int]
from_api = #<'a, 'b>ApiUser[full_name: 'a, years: 'b] -> User[name: 'a, age: 'b] {
  User[name: $full_name, age: $years]
}
%codec.convert<'user> [ApiUser[full_name: "Ada", years: "36"], using: [from_api, %str.parse_int]]
//= User[name: "Ada", age: 36]
```

## Search

The search is depth first: each step keeps the first route that succeeds, and a failure
anywhere inside a route moves on to the next alternative. A route can therefore fail deep in a
value, after a function has answered, and the conversion still succeed another way — which is
what decoding an untagged union needs:

```quiver
'shape = Circle[radius: 'int] | Rect[width: 'int, height: 'int]
pick = #[(fields): '%list<['%str, 'int]>, (key): '%str] -> ('int | []) {
  %list.find [$fields, #{ $0 ~> =^$$key }] ~> =[_, ~]
}
circle = #'%list<['%str, 'int]> -> Circle[radius: ('int | [])] { Circle[radius: pick [$, "radius"]] }
rect = #'%list<['%str, 'int]> -> Rect[width: ('int | []), height: ('int | [])] {
  Rect[width: pick [$, "width"], height: pick [$, "height"]]
}
%codec.convert<'shape> [%list{ ["radius", 1] }, using: [circle, rect]]   //= Circle[radius: 1]
%codec.convert<'shape> [%list{ ["width", 2], ["height", 3] }, using: [circle, rect]]
//= Rect[width: 2, height: 3]
%codec.convert<'shape> [%list{ ["width", 2] }, using: [circle, rect]]   //= []
```

## JSON

`%json` parses text to its document model, and `convert` takes the document to a program's
types. A JSON string or number is a Quiver one, so it passes through as it is; the rest
needs a few edges.

```quiver
'user = User[name: '%str, age: 'int]

decoders = [
  #'%json -> ('%list<'%json> | []) { =Array[~] },
  #'%json -> (User[name: ('%json | []), age: ('%json | [])] | []) {
    =Object[_] => User[name: %json.get [$, "name"], age: %json.get [$, "age"]]
  },
]

doc = %json.parse "{\"name\": \"Ada\", \"age\": 36}"    // '%json | []: the nil has no route
%codec.convert<'user, ('%json | [])> [doc, using: decoders]   //= User[name: "Ada", age: 36]
%codec.convert<'%list<'user>> [%json{ [{"name": "Ada", "age": 36}, {"name": "Bob", "age": 7}] }, using: decoders]
//= Cons[User[name: "Ada", age: 36], Cons[User[name: "Bob", age: 7], Nil]]
%codec.convert<'user> [%json{ {"name": "Ada"} }, using: decoders]   //= []
```

The object function maps keys to labels and nothing more: its fields stay JSON, and convert
on structurally. A missing key leaves nil in its field, so a field that may be missing says
so, and JSON `null` stays `Null`:

```quiver
'profile = Profile[name: '%str, bio: ('%str | Null | [])]
profile = #'%json -> (Profile[name: ('%json | []), bio: ('%json | [])] | []) {
  =Object[_] => Profile[name: %json.get [$, "name"], bio: %json.get [$, "bio"]]
}
%codec.convert<'profile> [%json{ {"name": "A", "bio": "hi"} }, using: [profile]]
//= Profile[name: "A", bio: "hi"]
%codec.convert<'profile> [%json{ {"name": "A", "bio": null} }, using: [profile]]
//= Profile[name: "A", bio: Null]
%codec.convert<'profile> [%json{ {"name": "A"} }, using: [profile]]   //= Profile[name: "A", bio: []]
%codec.convert<'profile> [%json{ {"name": "A", "bio": 1} }, using: [profile]]   //= []
```

A tagged union is a function that reads the tag:

```quiver
'shape = Circle[radius: 'int] | Rect[width: 'int, height: 'int]
shape = #'%json -> (Circle[radius: ('%json | [])] | Rect[width: ('%json | []), height: ('%json | [])] | []) {
  | %json.get [$, "type"] ~> ="circle" => Circle[radius: %json.get [$, "radius"]]
  | %json.get [$, "type"] ~> ="rect" => Rect[width: %json.get [$, "width"], height: %json.get [$, "height"]]
}
%codec.convert<'shape> [%json{ {"type": "circle", "radius": 2} }, using: [shape]]   //= Circle[radius: 2]
%codec.convert<'shape> [%json{ {"type": "hex"} }, using: [shape]]   //= []
```

Encoding runs the other way: the function takes a tuple of JSON fields to an object, and the
domain value is converted into that tuple first. `%json.array` is the list edge as it stands.

```quiver
encoders = [
  %json.array,
  #User[name: '%json, age: '%json] -> '%json { Object[%list{ ["name", $name], ["age", $age] }] },
]
%codec.convert<'%json> [User[name: "Ada", age: 36], using: encoders] ~> ='%json ~> %json.stringify
//= "{\"name\":\"Ada\",\"age\":36}"
%codec.convert<'%json> [%list{ User[name: "Ada", age: 36] }, using: encoders] ~> ='%json ~> %json.stringify
//= "[{\"name\":\"Ada\",\"age\":36}]"
```

## Failure

A conversion with no successful route answers nil, carrying a `'%data.error` under `:error`:
its path leads to the furthest point any route reached — a field's label, or its position
without one — and its message names the type it couldn't reach there.

```quiver
'user = User[name: '%str, age: 'int]
users = %list{ User[name: "a", age: "1"], User[name: "b", age: "x"] }
r = %codec.convert<'%list<'user>> [users, using: [%str.parse_int]]
r   //= []
r:error<'%data.error> ~> =DecodeError(path: ~)   //= Cons[1, Cons[0, Cons["age", Nil]]]
r:error<'%data.error> ~> =DecodeError(message: ~)   //= "no route to 'int"
```

A nil value is a failure arriving — a document that didn't parse — so it answers itself, its
`:error` intact, rather than a conversion failure of its own:

```quiver
"{" ~> %json.parse ~> %codec.convert<'%json> [~, using: %json.edges] ~> :error<Expected(offset: 'int)>
//= Expected(offset: 1)
```

Functions run as often as the search tries them, so they should be free of effects.

## Records, lists and text

Formats share a few neutral shapes: a **record** (`'%codec.record<'d>`, a list of `[key, value]`
entries) for an object, a list for an array, and text. A format's edges take its document to and
from them (`%json.edges` for JSON), and `convert` reads a program's tuples from them, and writes
them as them, by their layout: labelled fields from a record, each from the entry under its label;
positional fields from a list; a symbol (a named tuple without fields) from its name as text; and
a single positional field — a *newtype* — from whatever its content is read from.

```quiver
'user = User[name: '%str, age: 'int]
%codec.convert<'user> [%json{ {"name": "Ada", "age": 36} }, using: %json.edges]
//= User[name: "Ada", age: 36]
%codec.convert<'%json> [User[name: "Ada", age: 36], using: %json.edges] ~> ='%json ~> %json.stringify
//= "{\"name\":\"Ada\",\"age\":36}"

'point = Point['int, 'int]
'id = Id['int]
%codec.convert<'point> [%json{ [3, 4] }, using: %json.edges]   //= Point[3, 4]
%codec.convert<'id> [%json{ 7 }, using: %json.edges]           //= Id[7]
%codec.convert<'%json> [Point[3, 4], using: %json.edges] ~> ='%json ~> %json.stringify
//= "[3,4]"
```

Each field converts on its own, so a type nested inside needs nothing of its own:

```quiver
'item = Item[name: '%str, qty: 'int]
'order = Order[id: 'int, items: '%list<'item>]
doc = %json{ {"id": 7, "items": [{"name": "nut", "qty": 2}, {"name": "bolt", "qty": 5}]} }
%codec.convert<'order> [doc, using: %json.edges]
//= Order[id: 7, items: Cons[Item[name: "nut", qty: 2], Cons[Item[name: "bolt", qty: 5], Nil]]]
```

A field whose type admits nil may be absent, and is left out when written; any other is
required, and a record without it is no `'profile`. JSON's `null` is a value, `Null`, so a field
that may be null says so:

```quiver
'profile = Profile[name: '%str, bio: ('%str | Null | [])]
%codec.convert<'profile> [%json{ {"name": "Ada"} }, using: %json.edges]   //= Profile[name: "Ada", bio: []]
%codec.convert<'profile> [%json{ {"name": "Ada", "bio": null} }, using: %json.edges]
//= Profile[name: "Ada", bio: Null]
%codec.convert<'%json> [Profile[name: "Ada", bio: []], using: %json.edges] ~> ='%json ~> %json.stringify
//= "{\"name\":\"Ada\"}"
%codec.convert<'profile> [%json{ {"bio": "hi"} }, using: %json.edges]   //= []
```

A union's members are tried in order, so with nothing to tell them apart, a member is read by its
shape:

```quiver
'shape = Circle[radius: 'int] | Rect[width: 'int, height: 'int] | Dot
%codec.convert<'shape> [%json{ {"width": 2, "height": 3} }, using: %json.edges]   //= Rect[width: 2, height: 3]
%codec.convert<'shape> [%json{ "Dot" }, using: %json.edges]                        //= Dot
```

A named tuple of a type an edge takes or answers — or holds inside — is the edges' to convert, and
is never read or written this way. That is what keeps a format's own document (JSON's
`Object[…]`, its `True`) from being taken apart as if it were the program's, and a type whose
values hold an invariant their structure doesn't from being built field by field. The same holds
for any tuple a program converts by hand.

The standard library's such types have edges of their own, `%codec.standard`, which a format adds
to its edges. A dict files each entry under its key's hash, so it converts only through its own
functions: as a record when its keys are text, and otherwise as a list of `[key, value]` pairs.

```quiver
d = %codec.convert<'%dict<'%str, 'int>> [%json{ {"a": 1} }, using: %json.edges] ~> ='%dict<'%str, 'int>
%dict.get [d, "a"]   //= 1
%codec.convert<'%json> [d, using: %json.edges] ~> ='%json ~> %json.stringify   //= "{\"a\":1}"
%codec.convert<'%dict<'%str, 'int>> [%json{ [0, "a", 1] }, using: %json.edges]   //= []
```

The format's edges are all a form needs as well. Its document is text, so the edges take a form's
fields to a record of text, and integers to and from text:

```quiver
'signup = Signup[email: '%str, age: 'int]
%codec.convert<'signup> [%list{ ["email", "a@b.c"], ["age", "30"] }, using: %http.form_edges]
//= Signup[email: "a@b.c", age: 30]
%codec.convert<'%http.pairs> [Signup[email: "a@b.c", age: 30], using: %http.form_edges]
//= Cons[["email", "a@b.c"], Cons[["age", "30"], Nil]]
```

## Options

`keys:` writes every label in a convention — `Camel` (`userId`), `Pascal` (`UserId`), `Kebab`
(`user-id`) or `Snake` (`user_id`), each dropping a trailing `?` or `!` — and `shapes:` configures
particular tuples, each by an entry named as the tuple is: its `keys:`, keys of its own for
particular fields (`rename:`), and the values an absent field takes (`default:`).

```quiver
'account = Account[user_id: 'int, admin?: (True | False), plan: '%str]
shapes = [Account[rename: [user_id: "@id"], default: [plan: "free"]]]

%codec.convert<'account> [%json{ {"@id": 1, "admin": true} }, using: %json.edges, keys: Camel, shapes: shapes]
//= Account[user_id: 1, admin?: True, plan: "free"]
%codec.convert<'%json> [Account[user_id: 1, admin?: False, plan: "pro"], using: %json.edges, keys: Camel, shapes: shapes]
~> ='%json ~> %json.stringify
//= "{\"@id\":1,\"admin\":false,\"plan\":\"pro\"}"
```

```quiver
'p = P[ready_to_go?: (True | False)]
%codec.convert<'%json> [P[ready_to_go?: True], using: %json.edges, keys: Kebab] ~> ='%json ~> %json.stringify
//= "{\"ready-to-go\":true}"
%codec.convert<'%json> [P[ready_to_go?: True], using: %json.edges, shapes: [P[keys: Pascal]]]
~> ='%json ~> %json.stringify
//= "{\"ReadyToGo\":true}"
```

### Tags

`tag:` says how a union's variants are told apart — or, given in a member's `shapes:` entry, how
that member is — and `wire:` gives a member the name its tag writes:

| `tag:` | `Circle[radius: 2]` | `Dot` |
| --- | --- | --- |
| `Untagged` (default) | `{"radius": 2}` | `"Dot"` |
| `External` | `{"Circle": {"radius": 2}}` | `"Dot"` |
| `Internal[key: "type"]` | `{"type": "Circle", "radius": 2}` | `{"type": "Dot"}` |
| `Adjacent[tag: "t", content: "c"]` | `{"t": "Circle", "c": {"radius": 2}}` | `{"t": "Dot"}` |

```quiver
'shape = Circle[radius: 'int] | Rect[width: 'int, height: 'int] | Dot
json = %json.edges
tag = Internal[key: "type"]
shapes = [Circle[wire: "circle"], Rect[wire: "rect"], Dot[wire: "dot"]]
%codec.convert<'shape> [%json{ {"type": "rect", "width": 2, "height": 3} }, using: json, tag: tag, shapes: shapes]
//= Rect[width: 2, height: 3]
%codec.convert<'%json, 'shape> [Dot, using: json, tag: tag, shapes: shapes] ~> ='%json ~> %json.stringify
//= "{\"type\":\"dot\"}"
```

A tag tells variants apart, so it is written only where a value is one of several: where the
conversion's type is the union. A value whose own type is narrower — a literal, say — states the
union as the source type, `convert<'%json, 'shape>`, to be written as one of its variants.

A member with positional fields is written as its content, a list, or the value itself for one
field. An internal tag has no room for it, as it puts the tag among the member's keys:

```quiver
'expr = Lit['int] | Pair['int, 'int] | Unit
%codec.convert<'expr> [%json{ {"Pair": [1, 2]} }, using: %json.edges, tag: External]   //= Pair[1, 2]
%codec.convert<'%json, 'expr> [Lit[5], using: %json.edges, tag: External] ~> ='%json ~> %json.stringify
//= "{\"Lit\":5}"
%codec.convert<'%json, 'expr> [Unit, using: %json.edges, tag: External] ~> ='%json ~> %json.stringify
//= "\"Unit\""
```

A tag's content converts like any other value, so it may hold lists, or the union itself:

```quiver
'tree = Leaf['int] | Node[^, ^] | Labelled[name: '%str, children: '%list<^>]
json = %json.edges
tag = Adjacent[tag: "t", content: "c"]
t = Labelled[name: "root", children: %list{ Node[Leaf[1], Leaf[2]] }]
text = %codec.convert<'%json, 'tree> [t, using: json, tag: tag] ~> ='%json ~> %json.stringify
text   //= "{\"t\":\"Labelled\",\"c\":{\"name\":\"root\",\"children\":[{\"t\":\"Node\",\"c\":[{\"t\":\"Leaf\",\"c\":1},{\"t\":\"Leaf\",\"c\":2}]}]}}"
text ~> %json.parse ~> %codec.convert<'tree, ('%json | [])> [~, using: json, tag: tag] ~> =^t
//= ('tree)
```

### Checked where it is written

The options are planned with the conversion, from their types, so a mistake in them is a compile
error at the call:

```quiver
'user = User[name: '%str]
%codec.convert<'user> [%json{ {} }, using: %json.edges, shapes: [User[rename: [nmae: "n"]]]]
//! which User doesn't have
```

```quiver
'account = Account[plan: '%str]
%codec.convert<'account> [%json{ {} }, using: %json.edges, shapes: [Account[default: [plan: 0]]]]
//! defaults Account's `plan` to 'int
```

```quiver
'user = User[user_id: 'int, userId: 'int]
%codec.convert<'user> [%json{ {} }, using: %json.edges, keys: Camel]   //! two fields of User under the key "userId"
```

```quiver
'user = User[name: '%str]
%codec.convert<'user> [%json{ {} }, using: %json.edges, shapes: [Other[keys: Camel]]]
//! configures Other, which the conversion never reads or writes
```

```quiver
'shape = A[x: 'int] | B['int, 'int]
%codec.convert<'shape> [%json{ {} }, using: %json.edges, tag: Internal[key: "type"]]
//! can't tag the positional variant B internally
```

```quiver
'user = User[name: '%str]
%codec.convert<'user> [%json{ {} }, using: %json.edges, keys: Shouty]   //! takes Exact, Camel, Pascal, Kebab or Snake
```

What only the values say — a renamed key, a wire name, a tag's key — is read when the conversion
runs, so two of them that clash aren't caught here.

## Edges in pairs

A type a format or library converts its own way has two edges, which may be given together as
`Edge[out: f, in: g]`: `out` takes a value of the type apart, and is only applied to a value as it
stands; `in` builds one, and is only aimed at a target, its argument first converted to what it
takes. A format's own edges are pairs, which keeps the search from routes no conversion wants: a
decoder aimed at a program's type, or a round trip from one record type through another.

```quiver
digits = Edge[out: #'%str -> ('int | []) { %str.parse_int $ }, in: #'int -> '%str { %str.from_int $ }]
%codec.convert<'int, ('int | '%str)> ["42", using: [digits]]   //= 42
%codec.convert<'%str> [5, using: [digits]]                     //= "5"
```

`in` must build what `out` takes apart, from what `out` answers:

```quiver
%codec.convert<'%str> [5, using: [Edge[out: #'%str -> 'int { 0 }, in: #'int -> 'bin { <> }]]]
//! an Edge's `in` builds what its `out` takes apart
```

Anything else is no pair, and every other entry must be a function:

```quiver
%codec.convert<'%str> [[], using: [Sideways[#[] -> '%str { "x" }]]]   //! bare or paired as Edge[out: f, in: g]
```

```quiver
%codec.convert<'%str> [5, using: [Edge[out: #'int -> '%str { "x" }]]]   //! bare or paired as Edge[out: f, in: g]
```

An edge aimed at the target is planned only when its answer may already be the target. Its
answer is not taken further by an edge applied to it as it stands, which is the only use an `out`
has, so one pair's `in` then another's `out` is no route, though the same functions bare are. A
route may still continue through any edge that can itself be aimed. Without this limit, every
encoder's answer would be planned on through every decoder: a round trip between the program's
types that no conversion wants, and many times the planning.

```quiver
f = #Wrap['int] -> Mid['int] { Mid[$0] }
g = #Mid['int] -> Done['int] { Done[$0] }
%codec.convert<Done['int]> [Wrap[5], using: [f, g]]   //= Done[5]

wrapped = Edge[out: #Mid['int] -> Wrap['int] { Wrap[$0] }, in: f]
done = Edge[out: g, in: #Done['int] -> Mid['int] { Mid[$0] }]
%codec.convert<Done['int]> [Wrap[5], using: [wrapped, done]]   //! no route
```

A bare function is tried both ways. Which way an edge runs is part of the edges' type, so it holds
wherever they are passed — through a function's declared parameter, say:

```quiver
'edges = [Edge[out: #'%str -> ('int | []), in: #'int -> '%str]]
via = #[(value): 'int, (using): 'edges] { %codec.convert<'%str> [$value, using: $using] }
digits = Edge[out: #'%str -> ('int | []) { %str.parse_int $ }, in: #'int -> '%str { %str.from_int $ }]
via [5, [digits]]   //= "5"
```

## A format's edges

A format supplies its edges as one tuple, which a program passes as `using:`, beside its own
edges if it has any — an entry of `using:` that is an unnamed tuple holds more edges:

```quiver
show = #'int -> '%str { %str.from_int $ }
%codec.convert<[n: '%str]> [%json{ {"n": 5} }, using: [%json.edges, [show]]]   //= [n: "5"]
```

`%json.edges` is pairs between JSON's object and a record and between its array and a list, and
the standard library's own (`%codec.standard`). A format of another syntax needs only the same:
a document type, its parser and printer, and the pairs between its document and the neutral
shapes. Its typed `decode` and `encode` are then ordinary functions over its parser, printer and
`convert`, which is how `%json` writes them:

```quiver ignore
decode = #<'t, 'u, 'k, 'g, 'h>[(text): '%str, (using): 'u = [], (keys): 'k = [], …] -> ('t | []) {
  document = parse $text
  %codec.convert<'t> [document, using: [edges, $using], keys: $keys, …]
}
```

Naming `convert` at its own type parameter makes `decode` *type-consuming*, as `convert` is:
`%json.decode<'user>` plans the conversion where it is written.
