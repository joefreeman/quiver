# %json

JSON, as an ordinary Quiver value. `'%json` is the document model — a union of the six JSON
shapes — and everything else in the module is a way in or out of it: a compile-time literal
syntax, a runtime codec, a query/update API, and a typed boundary that converts documents to
and from the program's own types.

```quiver
"\{\"name\": \"ada\", \"tags\": [\"admin\"]}"
~> %json.parse ~
~> %json.get [~, %list{ "tags", 0 }]   //= "admin"
```

Because a `{` in a Quiver string opens an interpolation hole, inline JSON *text* escapes it as
`\{`. Input read from a file or a socket needs no such thing — the escape belongs to the
Quiver literal, not to JSON.

## The document model

`'%json` is a union of exactly what JSON has:

```quiver ignore
' = Null | True | False | '%num.coeff | '%str | Array['%list<^>] | Object['%list<['%str, ^]>]
```

`null`, `true` and `false` are named empty tuples; a number is an exact `'%num.coeff` (an
`'int`, or a `'rational` for a non-integer); a string is a `'%str`. An array wraps a `%list` of
values, and an object wraps a `%list` of `[key, value]` pairs — a **list**, not a map, because
JSON objects are ordered and may repeat a key, and a document that survives a round trip has to
keep both.

So a document is data you can match on directly:

```quiver
%json{ { "a": [1, true] } }   //= Object[Cons[["a", Array[Cons[1, Cons[True, Nil]]]], Nil]]
%json{ [] }                   //= Array[Nil]
%json{ {} }                   //= Object[Nil]
```

Every query and edit also accepts nil, and answers nil for a miss, so lookups chain without a
narrowing step between them. That type — `'%json | []` — is `'%json.opt`.

## Literals

`%json{ … }` is a dialect: the braced text is JSON, read at compile time into a `'%json` value.

```quiver
%json{ [1, 2, 3] }                            //= Array[Cons[1, Cons[2, Cons[3, Nil]]]]
%json{ { "a": 1, "b": true } } ~> %json.stringify ~   //= "{\"a\":1,\"b\":true}"
```

In value position a bare identifier is a **hole**: its span is spliced back into the caller's
scope, and must evaluate to a `'%json` value. `~` is the hole naming the value flowing into the
dialect term.

```quiver
inner = %json{ [1] }
%json{ { "wrapped": inner } } ~> %json.stringify ~   //= "{\"wrapped\":[1]}"
%json{ [1] } ~> %json{ { "v": ~ } } ~> %json.stringify ~   //= "{\"v\":[1]}"
```

The dialect's own number grammar is integer-only; decimals and exponents are the runtime
parser's business.

## Parsing

`parse` reads a `'%str` and answers `'%json`, or nil for malformed input. Scalars first:

```quiver
"null" ~> %json.parse ~      //= Null
"true" ~> %json.parse ~      //= True
"false" ~> %json.parse ~     //= False
"42" ~> %json.parse ~        //= 42
"-17" ~> %json.parse ~       //= -17
"\"hi\"" ~> %json.parse ~    //= "hi"
```

Arrays and objects nest, and an object keeps its keys in the order they were written:

```quiver
"[1, 2, 3]" ~> %json.parse ~   //= Array[Cons[1, Cons[2, Cons[3, Nil]]]]
"[[1], []]" ~> %json.parse ~   //= Array[Cons[Array[Cons[1, Nil]], Cons[Array[Nil], Nil]]]
"\{}" ~> %json.parse ~         //= Object[Nil]
```

```quiver
"\{\"a\": 1, \"b\": true}"
~> %json.parse ~   //= Object[Cons[["a", 1], Cons[["b", True], Nil]]]
```

```quiver
"\{\"a\": [\{\"b\": 1}, 2], \"c\": []}"
~> %json.parse ~
//= Object[Cons[["a", Array[Cons[Object[Cons[["b", 1], Nil]], Cons[2, Nil]]]], Cons[["c", Array[Nil]], Nil]]]
```

Whitespace is free, and a trailing comma before `]` or `}` is tolerated:

```quiver
"  [1, 2, ]  " ~> %json.parse ~     //= Array[Cons[1, Cons[2, Nil]]]
"\{ \"a\": 1, }" ~> %json.parse ~   //= Object[Cons[["a", 1], Nil]]
```

The string escapes are `\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\t` and `\r`. There is no `\u`.

```quiver
"[\"a\\nb\"]" ~> %json.parse ~   //= Array[Cons["a\nb", Nil]]
```

Malformed input answers nil, like a failed match — the sequence ends and the nil carries a
positioned `:error`.

```quiver
"[1, 2" ~> %json.parse ~    //= [] // unterminated array
"tru" ~> %json.parse ~      //= [] // incomplete keyword
"nope" ~> %json.parse ~     //= [] // not a keyword at all
"[1 2]" ~> %json.parse ~    //= [] // missing separator
```

## Numbers are exact

A JSON number parses to an exact `'%num.coeff` — never a float. `3.14` is the rational
`157/50`, and stays that, so nothing is rounded on the way in.

```quiver
"3.14" ~> %json.parse ~     //= 157/50
"-0.5" ~> %json.parse ~     //= -1/2
"2.5e-3" ~> %json.parse ~   //= 1/400
```

A whole-valued number is lowered to a plain `'int`, however it was spelled:

```quiver
"42" ~> %json.parse ~      //= 42
"42.0" ~> %json.parse ~    //= 42
"1.5e3" ~> %json.parse ~   //= 1500
```

Integers are arbitrary precision, so a magnitude that would overflow an `f64` to infinity is
just a number here:

```quiver
"1e30" ~> %json.parse ~   //= 1000000000000000000000000000000
```

```quiver
doc = "1000000000000000000000000000000"
doc ~> %json.parse ~ ~> =('%json & v); v ~> %json.stringify ~   //= ^doc
```

## Rendering

`stringify` writes compact, canonical JSON: no spaces, keys in the document's order.

```quiver
%json{ [1, 2, 3] } ~> %json.stringify ~    //= "[1,2,3]"
%json{ [] } ~> %json.stringify ~           //= "[]"
%json{ { "a": 1, "b": true } } ~> %json.stringify ~   //= "{\"a\":1,\"b\":true}"
%json{ "a\nb" } ~> %json.stringify ~       //= "\"a\\nb\""
```

A whole number is written as digits. A rational whose decimal **terminates** within 12
fractional places is written exactly; anything else is rounded, half away from zero, to 12
places. So JSON-originated data round-trips exactly and only computed fractions round.

```quiver
[314, 100] ~> %num.div ~ ~> =('%json & v); v ~> %json.stringify ~   //= "3.14"
[1, 3] ~> %num.div ~ ~> =('%json & v); v ~> %json.stringify ~       //= "0.333333333333"
[2, 3] ~> %num.div ~ ~> =('%json & v); v ~> %json.stringify ~       //= "0.666666666667"
[-2, 7] ~> %num.div ~ ~> =('%json & v); v ~> %json.stringify ~      //= "-0.285714285714"
```

A magnitude below 12 fractional digits of significance rounds to `0` — and never to `-0`:

```quiver
[1, 10000000000000] ~> %num.div ~ ~> =('%json & v); v ~> %json.stringify ~   //= "0"
```

## Round trips

`parse` is nilable, so its result is narrowed with `=('%json & v)` before `stringify` takes it.
Since `stringify` emits canonical form, re-stringifying a parse of already-canonical text
reproduces it byte for byte:

```quiver
"[1, [2, 3], [], -4]" ~> %json.parse ~ ~> =('%json & v)
v ~> %json.stringify ~   //= "[1,[2,3],[],-4]"
```

```quiver
"\{\"k\": [true, null]}" ~> %json.parse ~ ~> =('%json & v)
v ~> %json.stringify ~   //= "{\"k\":[true,null]}"
```

Decimals originating from JSON survive too — including values `f64` cannot represent, and
exponent forms that normalize to plain decimals:

```quiver
round = #'%str { %json.parse $ ~> =('%json & v); %json.stringify v }
round "3.14"     //= "3.14"
round "-0.5"     //= "-0.5"
round "2.675"    //= "2.675"
round "2.5e-3"   //= "0.0025"
round "0.1"      //= "0.1"
round "1e-12"    //= "0.000000000001"
```

A realistic document — nested objects and arrays, every scalar type, empty containers at depth,
and strings needing quote, backslash and newline escaping — checked with a single pin:

```quiver
doc = "\{\"user\":\{\"name\":\"Ada \\\"L\\\"\",\"age\":36,\"active\":true,\"roles\":[\"admin\",\"dev\"],\"manager\":null},\"scores\":[10,-5,0],\"empty_obj\":\{},\"empty_arr\":[],\"path\":\"a\\\\b\\nc\"}"
doc ~> %json.parse ~ ~> =('%json & v); v ~> %json.stringify ~   //= ^doc
```

## Reading

`get` takes a **key**: an object key (`'%str`), a 0-based array index (`'int`), or a path — a
list of either, applied outermost first.

```quiver
%json{ { "a": 1, "b": [10, 20] } } ~> %json.get [~, "a"]   //= 1
%json{ { "b": [10, 20] } } ~> %json.get [~, "b"] ~> %json.get [~, 1]   //= 20
%json{ { "users": [{ "name": "ada" }] } } ~> %json.get [~, %list{ "users", 0, "name" }]   //= "ada"
```

The empty path names the value itself:

```quiver
doc = %json{ { "a": 1 } }
%json.get [doc, %list{}]   //= ^doc
```

Every miss is nil, whether the key is absent, the kind is wrong, or the index is out of range:

```quiver
%json{ { "a": 1 } } ~> %json.get [~, "z"]   //= [] // no such key
%json{ [1, 2] } ~> %json.get [~, "a"]       //= [] // a key applied to an array
%json{ { "a": 1 } } ~> %json.get [~, 0]     //= [] // an index applied to an object
%json{ [1, 2] } ~> %json.get [~, 5]         //= [] // out of range
%json{ [1, 2] } ~> %json.get [~, -1]        //= [] // no negative indices
%json{ { "a": 1 } } ~> %json.get [~, %list{ "z", "x" }]   //= [] // a missing intermediate
```

Because nil is also *accepted*, a miss can be piped onward and answers nil again — so a deep
lookup is one pipeline with no narrowing in the middle, and the provenance stamp on the first
miss survives to the end:

```quiver
%json{ { "a": 1 } } ~> %json.get [~, "z"] ~> %json.get [~, "x"]   //= []
```

Only `parse` can introduce duplicate keys. `get` answers the last binding, matching what
mainstream parsers keep:

```quiver
"\{\"a\": 1, \"a\": 2}" ~> %json.parse ~ ~> %json.get [~, "a"]   //= 2
```

## Writing

`set` replaces what a key names. A present object key is rewritten in place, keeping the
document's order; an absent one is appended.

```quiver
%json{ { "a": 1, "b": 2 } } ~> %json.set [~, "a", 9] ~> =('%json & v)
v ~> %json.stringify ~   //= "{\"a\":9,\"b\":2}"
```

```quiver
%json{ { "a": 1 } } ~> %json.set [~, "c", 3] ~> =('%json & v)
v ~> %json.stringify ~   //= "{\"a\":1,\"c\":3}"
```

Duplicates collapse: the first occurrence is rewritten, the rest dropped.

```quiver
"\{\"a\": 1, \"b\": 2, \"a\": 3}" ~> %json.parse ~ ~> %json.set [~, "a", 9] ~> =('%json & v)
v ~> %json.stringify ~   //= "{\"a\":9,\"b\":2}"
```

An index sets an array element, and a path rebuilds every level around the leaf:

```quiver
%json{ [1, 2, 3] } ~> %json.set [~, 1, 9] ~> =('%json & v)
v ~> %json.stringify ~   //= "[1,9,3]"
```

```quiver
%json{ { "tags": [1, 2] } } ~> %json.set [~, %list{ "tags", 0 }, 9] ~> =('%json & v)
v ~> %json.stringify ~   //= "{\"tags\":[9,2]}"
```

The empty path replaces the whole value:

```quiver
%json{ { "a": 1 } } ~> %json.set [~, %list{}, 5]   //= 5
```

There is no vivification: a path through a container that isn't there is a miss, not a reason
to create one. An out-of-range index is a miss too, and a nil *replacement* propagates rather
than being embedded as `null`.

```quiver
%json{ [1, 2] } ~> %json.set [~, 5, 9]   //= []
%json{ { "a": 1 } } ~> %json.set [~, %list{ "z", "x" }, 9]   //= []
%json{ { "a": 1 } } ~> %json.set [~, "a", %json.get [%json{ {} }, "z"]]   //= []
```

## Updating

`update` is `get` then `set`: `f` is applied to what the key names.

```quiver
%json{ { "n": 2 } } ~> %json.update [~, "n", #{ =('int & i); %num.mul [i, 10] }] ~> %json.get [~, "n"]   //= 20
```

```quiver
%json{ { "a": { "n": [5, 7] } } }
~> %json.update [~, %list{ "a", "n", 1 }, #{ =('int & i); %num.mul [i, 10] }] ~> =('%json & v)
v ~> %json.stringify ~   //= "{\"a\":{\"n\":[5,70]}}"
```

An absent target is nil, and so is `f` answering nil:

```quiver
%json{ { "n": 2 } } ~> %json.update [~, "z", #{ ~ }]   //= []
%json{ { "n": 2 } } ~> %json.update [~, %list{ "z", "x" }, #{ ~ }]   //= []
%json{ { "n": 2 } } ~> %json.update [~, "n", #{ [] }]   //= []
```

## Deleting

`delete` removes what a key names. An array element deletion shifts the rest down; an object
key deletion removes *every* occurrence of it.

```quiver
%json{ { "a": 1, "b": 2 } } ~> %json.delete [~, "a"] ~> =('%json & v)
v ~> %json.stringify ~   //= "{\"b\":2}"
```

```quiver
%json{ [1, 2, 3] } ~> %json.delete [~, 1] ~> =('%json & v)
v ~> %json.stringify ~   //= "[1,3]"
```

```quiver
%json{ { "a": [1, 2] } } ~> %json.delete [~, %list{ "a", 0 }] ~> =('%json & v)
v ~> %json.stringify ~   //= "{\"a\":[2]}"
```

```quiver
"\{\"a\": 1, \"b\": 2, \"a\": 3}" ~> %json.parse ~ ~> %json.delete [~, "a"] ~> =('%json & v)
v ~> %json.stringify ~   //= "{\"b\":2}"
```

Deletion is idempotent — an absent key, an out-of-range index or a missing intermediate leaves
the document unchanged — but still kind-strict, so an object key applied to an array is nil.

```quiver
doc = %json{ { "a": 1 } }
%json.delete [doc, "z"]   //= ^doc
%json.delete [doc, %list{ "z", "x" }]   //= ^doc
```

```quiver
doc = %json{ [1] }
%json.delete [doc, 5]   //= ^doc
%json{ [1] } ~> %json.delete [~, "a"]   //= []
```

The empty path names the whole value, so deleting it leaves nothing:

```quiver
%json{ { "a": 1 } } ~> %json.delete [~, %list{}]   //= []
```

## Merging

`merge` is a shallow merge of two objects: the first's keys in its order, with the second's
values winning, then the second's remaining pairs in its order.

```quiver
[%json{ { "a": 1, "b": 2 } }, %json{ { "b": 20, "c": 30 } }] ~> %json.merge ~ ~> =('%json & v)
v ~> %json.stringify ~   //= "{\"a\":1,\"b\":20,\"c\":30}"
```

Anything but two objects is nil:

```quiver
[%json{ [1] }, %json{ {} }] ~> %json.merge ~   //= []
```

## Dictionaries and lists

`to_dict` reads an object's pairs into a `%dict`, which drops the ordering (and duplicates,
later ones winning, as `get` does) in exchange for hashed lookup.

```quiver
%json{ { "a": 1 } } ~> %json.to_dict ~ ~> =('%dict<'%str, '%json> & d)
%dict.get [d, "a"]   //= 1
```

```quiver
"\{\"a\": 1, \"a\": 2}" ~> %json.parse ~ ~> %json.to_dict ~ ~> =('%dict<'%str, '%json> & d)
%dict.get [d, "a"]   //= 2
```

```quiver
%json{ [1] } ~> %json.to_dict ~   //= [] // an array is not an object
```

`object` builds one back, from either a pair list or a dict, and `array` wraps a list of
values:

```quiver
%json.object %list{ ["x", 1] } ~> %json.stringify ~   //= "{\"x\":1}"
%json.object %dict{ "y" => 2 } ~> %json.get [~, "y"]   //= 2
%json.array %list{ 1, 2 } ~> %json.stringify ~   //= "[1,2]"
```

A dict's entries arrive in its own hash order, so `Object → %dict → Object` preserves the
entries but not their order:

```quiver
%json{ { "m": { "x": 7 } } } ~> %json.to_dict ~ ~> =('%dict<'%str, '%json> & d)
%json.object d ~> %json.get [~, %list{ "m", "x" }]   //= 7
```

## Typed decoding

`decode<'t>` converts a document into an ordinary typed value. Like `%data.decode`, it is
*type-consuming*: the expected type drives the conversion and must be given explicitly where it
is applied. Anything that does not fit is nil, like a failed match.

```quiver
%json{ 42 } ~> %json.decode<'int> ~                //= 42
%json{ "hi" } ~> %json.decode<'%str> ~             //= "hi"
%json{ true } ~> %json.decode<(True | False)> ~    //= True
%json{ "hi" } ~> %json.decode<'int> ~              //= []
"3.5" ~> %json.parse ~ ~> %json.decode<'int> ~     //= [] // not a whole number
"3.5" ~> %json.parse ~ ~> %json.decode<'%num.coeff> ~   //= 7/2
```

An object becomes a tuple with labelled fields. Keys match by label, order-free; extra keys are
ignored; the tuple's name is Quiver's business and has no JSON counterpart.

```quiver
"\{\"age\": 36, \"name\": \"ada\", \"x\": true}"
~> %json.parse ~
~> %json.decode<User[name: '%str, age: 'int]> ~   //= User[name: "ada", age: 36]
```

A field type that admits nil makes its key **optional**, and collapses the two ways JSON has of
saying "nothing" — absent, and `null` — into the same nil. A key with no such type is required,
and its absence is a mismatch:

```quiver
"\{\"a\": 1}" ~> %json.parse ~ ~> %json.decode<[a: 'int, b: '%str | []]> ~   //= [a: 1, b: []]
"\{\"a\": 1, \"b\": null}" ~> %json.parse ~ ~> %json.decode<[a: 'int, b: '%str | []]> ~   //= [a: 1, b: []]
"\{\"name\": \"ada\"}" ~> %json.parse ~ ~> %json.decode<[name: '%str, age: 'int]> ~   //= []
```

Arrays become `%list`s, and nesting composes. One element failing fails the whole array — there
is no partial result.

```quiver
%json{ [1, 2, 3] } ~> %json.decode<'%list<'int>> ~   //= Cons[1, Cons[2, Cons[3, Nil]]]
%json{ { "users": [{ "name": "ada" }] } } ~> %json.decode<[users: '%list<[name: '%str]>]> ~
//= [users: Cons[[name: "ada"], Nil]]
%json{ [1, "x"] } ~> %json.decode<'%list<'int>> ~   //= []
```

A union decodes by ordered choice, so objects discriminate structurally between its members:

```quiver
'shape = Circle[radius: 'int] | Rect[width: 'int, height: 'int]
%json{ "x" } ~> %json.decode<('int | '%str)> ~   //= "x"
"\{\"width\": 2, \"height\": 3}" ~> %json.parse ~ ~> %json.decode<'shape> ~   //= Rect[width: 2, height: 3]
"\{\"radius\": 5}" ~> %json.parse ~ ~> %json.decode<'shape> ~   //= Circle[radius: 5]
```

A recursive type is followed as far as the document goes:

```quiver
'tree = Leaf[value: 'int] | Node[left: ^, right: ^]
"\{\"left\": \{\"value\": 1}, \"right\": \{\"value\": 2}}"
~> %json.parse ~
~> %json.decode<'tree> ~   //= Node[left: Leaf[value: 1], right: Leaf[value: 2]]
```

A `'%json`-typed part is not converted at all: the raw subtree passes through verbatim, `null`
included. That is how a schema keeps an opaque region.

```quiver
doc = %json{ { "a": [1, null] } }
%json.decode<'%json> doc   //= ^doc
```

```quiver
%json{ { "meta": { "x": [true] } } } ~> %json.decode<[meta: '%json]> ~ ~> =[meta: m]
m ~> %json.stringify ~   //= "{\"x\":[true]}"
```

`decode` accepts nil like the query functions, so a lookup and a decode are one pipeline:

```quiver
%json{ { "a": 1 } } ~> %json.get [~, "zzz"] ~> %json.decode<'int> ~   //= []
```

## Typed encoding

`encode<'t>` is the inverse: a value of the stated type becomes a document, which `stringify`
then writes.

```quiver
%json.encode<[name: '%str, age: 'int]> [name: "ada", age: 36] ~> %json.stringify ~
//= "{\"name\":\"ada\",\"age\":36}"
```

A tuple's name is dropped — JSON has nowhere to put it — and a nil optional field is omitted
rather than written as `null`:

```quiver
%json.encode<Point[x: 'int, y: 'int]> Point[x: 1, y: 2] ~> %json.stringify ~   //= "{\"x\":1,\"y\":2}"
%json.encode<[a: 'int, b: '%str | []]> [a: 1, b: []] ~> %json.stringify ~      //= "{\"a\":1}"
```

A list becomes an array. Inside one, where an element's own type admits nil, a nil *is* `null` —
there is no field to omit.

```quiver
%json.encode<'%list<'int>> %list{ 1, 2, 3 } ~> %json.stringify ~        //= "[1,2,3]"
%json.encode<'%list<'int | []>> %list{ 1, [] } ~> %json.stringify ~     //= "[1,null]"
%json.encode<(True | False)> False ~> %json.stringify ~                 //= "false"
```

Exact rationals pass through as `'%num.coeff`; how they are written is `stringify`'s decision,
not `encode`'s:

```quiver
%num.div [1, 2] ~> =('%num.coeff & h)
h ~> %json.encode<'%num.coeff> ~ ~> %json.stringify ~   //= "0.5"
```

A `'%json`-typed part passes through whole, as on the way in:

```quiver
doc = %json{ { "a": [1, true, null] } }
%json.encode<'%json> doc   //= ^doc
```

A value with no JSON form at all — a function, process, ref, resource, binary, dict, or an
unlabelled tuple — is a runtime error rather than a nil, because no caller could act on it:

```quiver
%json.encode<#'int -> 'int> #'int { $ }   //! cannot encode as JSON: the value does not fit the stated type's mapping
```

## Typed round trips

`encode<'t>` then `decode<'t>` is the identity, directly:

```quiver
'user = [name: '%str, age: 'int, email: '%str | []]
u = [name: "ada", age: 36, email: []]
%json.encode<'user> u ~> %json.decode<'user> ~   //= ^u
```

and through text, which is the whole path a program actually uses:

```quiver
'tree = Leaf[value: 'int] | Node[left: ^, right: ^]
t = Node[left: Leaf[value: 1], right: Node[left: Leaf[value: 2], right: Leaf[value: 3]]]
%json.encode<'tree> t ~> %json.stringify ~ ~> %json.parse ~ ~> %json.decode<'tree> ~   //= ^t
```
