# %data

**Quiver data notation** is the language's own literal syntax, restricted to data: integers,
binaries, and tuples with their names and field labels, plus the `"…"` sugar for `Str` values.
`%data` is its codec — `encode` writes a value out, `decode` reads one back.

```quiver
%data.encode Point[x: 1, y: "hi"]              //= "Point[x: 1, y: \"hi\"]"
%data.decode<Point[x: 'int, y: Str['bin]]> "Point[x: 1, y: \"hi\"]"   //= Point[x: 1, y: "hi"]
```

The two are not symmetric. `encode` needs nothing but the value. `decode` is *type-consuming*:
the expected type drives the parse, and names in the text resolve only against that type's
members — so decoding can never produce a shape the program does not already contain. Anything
malformed, mismatched, or left over answers nil, exactly like a failed match.

## Encoding

`encode` walks a value and writes its literal form.

```quiver
%data.encode -42                              //= "-42"
%data.encode 123456789012345678901234567890   //= "123456789012345678901234567890"
%data.encode <0a1b>                           //= "<0a1b>"
%data.encode []                               //= "[]"
%data.encode Ok                               //= "Ok"
%data.encode Point[x: 1, y: [2, Blue]]        //= "Point[x: 1, y: [2, Blue]]"
```

Text that a string literal can carry uses the string sugar, with the language's escapes. `{` is
among them, so encoded text reads back as *literal* code: nothing in it interpolates.

```quiver
%data.encode "a\"b\\c\{d\ne"   //= "\"a\\\"b\\\\c\\{d\\ne\""
```

Bytes no string literal could carry fall back to the ordinary tuple form — a `Str` is only ever
sugar for one:

```quiver
%data.encode Str[<ff00>]   //= "Str[<ff00>]"
```

Annotations are data *about* a value, not part of it, so the notation does not see them:

```quiver
P[x: 1] ~> { :note "hi" } ~> %data.encode ~   //= "P[x: 1]"
```

## What has no notation

A function, a process, a ref or a resource means something only inside the program that holds
it; there is no text that could denote one. Encoding one is a runtime error rather than a
silent placeholder, because a caller has nothing to do about it either way.

```quiver
f = #'int { $ }
%data.encode f              //! cannot encode a function: %data notation carries data only
%ref [] ~> %data.encode ~   //! cannot encode a ref: %data notation carries data only
```

## Decoding

`decode<'t>` parses text at the type it is given, and the type must be written at the point the
member is applied.

```quiver
%data.decode<'int> "  -42  "                          //= -42
%data.decode<'int> "123456789012345678901234567890"   //= 123456789012345678901234567890
%data.decode<'bin> "<0a1b>"                           //= <0a1b>
%data.decode<Str['bin]> "\"hi\""                      //= "hi"
```

The string sugar is a synonym, not a format, so the tuple spelling decodes to the same value:

```quiver
%data.decode<Str['bin]> "Str[<6869>]"   //= "hi"
```

Leaving the type argument off is a compile error, not a guess:

```quiver
"5" ~> %data.decode ~   //! consumes its type argument
```

## Tuples, labels and layout

A field's label is optional in the text, but must match the expected shape when written.
Whitespace and a trailing comma are as free as they are in code.

```quiver
%data.decode<P[x: 'int, y: 'int]> "P[x: 1, y: 2]"   //= P[x: 1, y: 2]
%data.decode<P[x: 'int, y: 'int]> "P[ 1,\n  2, ]"   //= P[x: 1, y: 2]
%data.decode<P[x: 'int]> "P[y: 1]"                  //= [] // the label is not this shape's
```

## Binaries

Because the notation *is* the literal syntax, a hand-written binary may group its digits under
the same rules as source: whole-byte groups, separated by whitespace but never padded. `encode`
never emits grouping, so this only ever matters for text a person wrote.

```quiver
%data.decode<'bin> "<6a09e667 bb67ae85>"   //= <6a09e667bb67ae85>
%data.decode<'bin> "<>"                    //= <>
%data.decode<'bin> "<0a1 b2c>"             //= [] // groups are whole bytes
%data.decode<'bin> "< 0a1b>"               //= [] // a separator pads nothing
%data.decode<'bin> "<0a1b >"               //= []
```

A line break both separates groups and lets the brackets sit apart from the digits, so a table
written across lines reads back like the source literal it mirrors:

```quiver
%data.decode<'bin> "<\n  0a1b 2c3d\n  4e5f 6071\n>"   //= <0a1b2c3d4e5f6071>
%data.decode<'bin> "<\n>"                             //= <>
```

## Unions and recursion

A union decodes by ordered choice, with a prefix-lookahead rule: a bare named-empty tuple is
only ever read as one when no `[` is glued to it.

```quiver
'u = Ok | Ok['int]
%data.decode<('int | 'bin)> "<0a>"   //= <0a>
%data.decode<'u> "Ok[5]"             //= Ok[5]
%data.decode<'u> "Ok"                //= Ok
```

A recursive type is followed as deep as the text goes:

```quiver
'list = Nil | Cons['int, ^]
'tree = Leaf['int] | Node[^, ^]
%data.decode<'list> "Cons[1, Cons[2, Nil]]"   //= Cons[1, Cons[2, Nil]]
%data.decode<'tree> "Node[Leaf[1], Node[Leaf[2], Leaf[3]]]"   //= Node[Leaf[1], Node[Leaf[2], Leaf[3]]]
```

Recursion through a *sibling* member works the same way — `'%json` is the natural witness, since
an `Array` sitting inside an `Object`'s value is reached only by following the pair list back up
to the root union:

```quiver
%data.decode<'%json> "Object[Cons[[\"a\", Array[Cons[1, Nil]]], Nil]]"
//= Object[Cons[["a", Array[Cons[1, Nil]]], Nil]]
```

## What fails

Every kind of mismatch is the same nil: a name the expected type has no member for, the wrong
arity, malformed bytes, input left over at the end, a raw newline inside a string, or an
expected type too partial to build a value from.

```quiver
%data.decode<(A | B)> "C"          //= [] // no such member
%data.decode<P['int]> "P[1, 2]"    //= [] // arity
%data.decode<'bin> "<0a1>"         //= [] // odd hex
%data.decode<'int> "4 2"           //= [] // trailing input
%data.decode<Str['bin]> "\"a\nb\""   //= [] // a string literal ends at the line
%data.decode<(x: 'int)> "[x: 1]"   //= [] // a partial type has no layout to construct
```

A nil *member* is the one case where success and failure are the same answer — by design, and
exactly as anywhere else nil is used both as a value and as a verdict. Keep decoded unions
nil-free if the difference matters.

```quiver
%data.decode<('int | [])> "[]"   //= []
```

## Round trips

`encode` then `decode` at the value's own type is the identity, across every shape the notation
covers at once — a bignum, an escaped string, and a recursive list:

```quiver
'pt = Point[x: 'int, y: Str['bin], z: (Nil | Cons['int, ^])]
v = Point[x: -12345678901234567890123, y: "a\"b\\c\{d\ne", z: Cons[1, Cons[2, Nil]]]
%data.encode v ~> %data.decode<'pt> ~   //= ^v
```

## Instantiation rides the value

`decode<'t>` is an ordinary value once instantiated, so the type argument travels with it: bind
it, pass it around, and call it later.

```quiver
d = %data.decode<('int | Quit)>
d "Quit"   //= Quit
d "5"      //= 5
```

## Decoded values are ordinary values

What comes back is nil-admitting, and matched like anything else. Sibling patterns over a
decoded union all stay reachable, and a failed decode simply falls through to the last branch:

```quiver
'wire = Submit | Toggle['int] | Del['int]
f = #(Ev['wire] | []) {
  $ ~> {
    | =Ev[Submit] => S
    | =Ev[Toggle[id]] => T[id]
    | =Ev[Del[id]] => D[id]
    | Missed
  }
}
%data.decode<Ev['wire]> "Ev[Submit]" ~> f ~      //= S
%data.decode<Ev['wire]> "Ev[Toggle[1]]" ~> f ~   //= T[1]
%data.decode<Ev['wire]> "Ev[Del[7]]" ~> f ~      //= D[7]
%data.decode<Ev['wire]> "garbage" ~> f ~         //= Missed
```
