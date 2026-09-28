# %str

A string is `Str['bin]` — UTF-8 bytes wrapped in a one-field tuple. That is the whole of it:
the language has no string type, only the literal syntax `"…"` that builds this ordinary
value, and `%str` is the module of operations over it.

```quiver
"hello"                       //= Str[<68656c6c6f>]
Str[<68656c6c6f>]             //= "hello"
```

Because the sugar and the tuple are the same value, a string literal is a pattern like any
other, `%str.bytes` is just field access, and nothing about a string needs the module to
exist.

```quiver
%str.bytes "hello"            //= <68656c6c6f>
%str.bytes ""                 //= <>
%str.bytes "🚀"               //= <f09f9a80>
```

Operations divide by what they count. Most work on the bytes, which is all that searching,
splitting and trimming need, since no UTF-8 character's bytes can occur inside another's.
`length`, `slice`, `index_of`, `last_index_of`, the padding functions and `iter` work on
**characters**.

## Interpolation

A `{ … }` hole in a literal interpolates. The hole is an ordinary expression, evaluated in
the surrounding scope, and it must produce a string — anything else is a compile error, so
there is no implicit stringification.

```quiver
name = "world"
"hello {name}"                //= "hello world"
```

```quiver
n = 5
"count {n}"                   //! expected Str, found 'int
```

Holes may appear anywhere, repeatedly, and with no text between them. `\{` is a literal
brace, and a literal without holes is just a literal.

```quiver
a = "X"; b = "Y"
"[{a}-{b}]"                   //= "[X-Y]"
"{a}{b}"                      //= "XY"
"a \{ b"                      //= "a { b"
"plain"                       //= "plain"
```

A hole holds a full expression, so a call chain can sit inside one:

```quiver
"got: {%str.concat ["a", "b"]}"          //= "got: ab"
"1 + 1 = {%str.from_int 2}"              //= "1 + 1 = 2"
```

Like a tuple field, a hole receives the value flowing into the literal, so `~` is available
and is copied into each hole:

```quiver
"world" ~> "hello, {~}"                  //= "hello, world"
"x" ~> "{~}-{~}"                         //= "x-x"
Person[name: "Bo"] ~> "hi {~.name}"      //= "hi Bo"
```

## Multi-line literals

A `"""`-delimited literal spans lines. The closing delimiter's indentation sets a margin
stripped from every line, and the newline before it is not part of the value. Holes work as
they do in a single-line literal.

```quiver
name = "ada"
"""
hi {name}
bye
"""                           //= "hi ada\nbye"
```

The two delimiters are surface forms of one value, so a multi-line literal and the
single-line literal with the same content are equal:

```quiver
s = "a\nb"
"""
a
b
"""                           //= ^s
```

```quiver
x = "v"
s = "a v"
"""
a {x}
"""                           //= ^s
```

```quiver
"""
\{ {%str.concat ["p", "q"]}
"""                           //= "{ pq"
```

The flowing value reaches a multi-line literal's holes too:

```quiver
"ada" ~> """
hi {~}
"""                           //= "hi ada"
```

## Matching

A string literal in a pattern tests for that exact string, since it is exactly a tuple
literal.

```quiver
role = "admin"
role ~> ="admin"              //= "admin"
```

```quiver
role = "guest"
role ~> { ="admin" }          //= []
```

## Emptiness and concatenation

```quiver
%str.empty? ""                //= Ok
%str.empty? "hello"           //= []
%str.empty? "x"               //= []
```

`concat` joins two strings; either may be empty, and multi-byte characters are carried
through untouched.

```quiver
%str.concat ["hello", " "]         //= "hello "
%str.concat ["hello", " world"]    //= "hello world"
%str.concat ["", "hello"]          //= "hello"
%str.concat ["hello", ""]          //= "hello"
%str.concat ["🚀", "!"]            //= "🚀!"
```

## Prefixes, suffixes and substrings

`starts_with?`, `ends_with?` and `contains?` answer verdicts. A string starts with, ends
with and contains itself, and every string contains the empty string.

```quiver
%str.starts_with? ["hello world", "hello"]   //= Ok
%str.starts_with? ["hello world", "world"]   //= []
%str.starts_with? ["hello", "hello"]         //= Ok
%str.starts_with? ["hello", ""]              //= Ok
%str.starts_with? ["hi", "hello"]            //= []
%str.starts_with? ["🚀 rocket", "🚀"]        //= Ok
```

```quiver
%str.ends_with? ["hello world", "world"]     //= Ok
%str.ends_with? ["hello world", "hello"]     //= []
%str.ends_with? ["hello", "hello"]           //= Ok
%str.ends_with? ["hello", ""]                //= Ok
%str.ends_with? ["hi", "hello"]              //= []
%str.ends_with? ["rocket 🚀", "🚀"]          //= Ok
```

```quiver
%str.contains? ["hello world", "world"]      //= Ok
%str.contains? ["hello world", "hello"]      //= Ok
%str.contains? ["hello world", "o w"]        //= Ok
%str.contains? ["hello world", "xyz"]        //= []
%str.contains? ["hello", ""]                 //= Ok
%str.contains? ["hi", "hello"]               //= []
%str.contains? ["hello", "hello"]            //= Ok
%str.contains? ["🚀 rocket 🌙", "rocket"]    //= Ok
%str.contains? ["🚀 rocket 🌙", "🌙"]        //= Ok
```

`strip_prefix` and `strip_suffix` answer what is left once the affix is removed, or nil when
it is not there, so testing and cutting are one step:

```quiver
%str.strip_prefix ["v1.2", "v"]              //= "1.2"
%str.strip_prefix ["1.2", "v"]               //= []
%str.strip_prefix ["hello", ""]              //= "hello"
%str.strip_prefix ["hi", "hello"]            //= []
%str.strip_suffix ["notes.txt", ".txt"]      //= "notes"
%str.strip_suffix ["notes.md", ".txt"]       //= []
%str.strip_suffix ["🚀 rocket 🚀", " 🚀"]    //= "🚀 rocket"
```

```quiver
route = #'%str {
  | %str.strip_prefix [~, "/users/"] => "user {~}"
  | "not found"
}
route "/users/ada"                           //= "user ada"
route "/posts/1"                             //= "not found"
```

`index_of` answers *where*, as a character index, or nil when the substring is absent. The
empty string is found at 0.

```quiver
%str.index_of ["hello world", "hello"]       //= 0
%str.index_of ["hello world", "o"]           //= 4 // the first occurrence
%str.index_of ["hello world", "world"]       //= 6
%str.index_of ["hello world", "xyz"]         //= []
%str.index_of ["hello", ""]                  //= 0
%str.index_of ["🚀 rocket 🌙", "rocket"]     //= 2 // characters, not bytes
%str.index_of ["🚀 rocket 🌙", "🌙"]         //= 9
```

`last_index_of` searches from the other end. The empty string is found at the very end.

```quiver
%str.last_index_of ["hello world", "o"]      //= 7
%str.last_index_of ["hello world", "hello"]  //= 0
%str.last_index_of ["hello world", "xyz"]    //= []
%str.last_index_of ["aaa", "aa"]             //= 1 // overlapping occurrences count
%str.last_index_of ["hello", ""]             //= 5
%str.last_index_of ["🚀a🚀a", "a"]           //= 3
```

## Splitting and joining

`split` yields an **iterator** of the pieces, so nothing is materialised until it is
consumed. A separator that never occurs yields the whole string as one piece, and a
separator at either end yields the empty piece beside it — the pieces always number one more
than the separators found.

```quiver
%str.split ["hello,world,test", ","] ~> %list.collect ~
//= Cons["hello", Cons["world", Cons["test", Nil]]]
```

```quiver
%str.split ["a::b::c", "::"] ~> %list.collect ~   //= Cons["a", Cons["b", Cons["c", Nil]]]
%str.split ["hello", ","] ~> %list.collect ~      //= Cons["hello", Nil]
%str.split ["", ","] ~> %list.collect ~           //= Cons["", Nil]
%str.split [",hello,", ","] ~> %list.collect ~    //= Cons["", Cons["hello", Cons["", Nil]]]
```

Being an iterator, it is a value that flows through a chain, and a consumer that needs one
piece does not pay for the rest:

```quiver
parts = %str.split ["1,2,3", ","]
%str.join [parts, " + "]                          //= "1 + 2 + 3"
%str.split ["a,b,c", ","] ~> %iter.nth [~, 0]     //= "a"
```

An empty separator would match between every pair of characters, which is never what a
split means, so it is rejected. `%str.iter` is how to take a string apart by character.

```quiver
%str.split ["abc", ""] ~> %list.collect ~         //! Cannot split on an empty separator
```

`split_once` splits at the first occurrence only, answering the two sides as a pair, or nil
when the separator is absent. A pair is what a caller usually wants, and destructures in
place:

```quiver
%str.split_once ["key=value", "="]                //= ["key", "value"]
%str.split_once ["a=b=c", "="]                    //= ["a", "b=c"]
%str.split_once ["=x", "="]                       //= ["", "x"]
%str.split_once ["novalue", "="]                  //= []
%str.split_once ["Host: example.com", ": "] ~> =[name, value]; value   //= "example.com"
```

`split_last` splits at the last occurrence instead:

```quiver
%str.split_last ["user@mail@example.com", "@"]    //= ["user@mail", "example.com"]
%str.split_last ["archive.tar.gz", "."]           //= ["archive.tar", "gz"]
%str.split_last ["README", "."]                   //= []
```

`lines` splits on line feeds, dropping a carriage return before each, so text with either
line ending reads the same. A final line feed ends the last line rather than starting an
empty one, and the empty string has no lines.

```quiver
%str.lines "one\ntwo\nthree" ~> %list.collect ~    //= Cons["one", Cons["two", Cons["three", Nil]]]
%str.lines "one\r\ntwo\r\n" ~> %list.collect ~    //= Cons["one", Cons["two", Nil]]
%str.lines "a\n\nb" ~> %list.collect ~             //= Cons["a", Cons["", Cons["b", Nil]]]
%str.lines "\n" ~> %list.collect ~                 //= Cons["", Nil]
%str.lines "" ~> %list.collect ~                   //= Nil
```

`join` is the inverse of `split`, placing a separator between each of an iterator's strings.

```quiver
%list{ "one", "two", "three" } ~> %list.iter ~ ~> %str.join [~, ", "]   //= "one, two, three"
%list{ "only" } ~> %list.iter ~ ~> %str.join [~, ", "]                  //= "only"
Nil ~> %list.iter ~ ~> %str.join [~, ", "]                              //= ""
```

## Replacing

`replace` replaces every occurrence of a substring, scanning from the left, so occurrences
never overlap. As with `split`, the substring must not be empty.

```quiver
%str.replace ["a-b-c", "-", "+"]             //= "a+b+c"
%str.replace ["hello", "l", ""]              //= "heo"
%str.replace ["aaa", "aa", "b"]              //= "ba"
%str.replace ["hello", "x", "y"]             //= "hello"
%str.replace ["🚀 to 🌙", "🚀", "rocket"]     //= "rocket to 🌙"
%str.replace ["abc", "", "-"]                //! Cannot replace an empty string
```

## Whitespace and case

`trim` removes whitespace from both ends, and `trim_start` and `trim_end` from one. Only
ASCII whitespace counts — space, tab, line feed, vertical tab, form feed and carriage return.

```quiver
%str.trim "  hello world \n"         //= "hello world"
%str.trim_start "  hello  "          //= "hello  "
%str.trim_end "  hello  "            //= "  hello"
%str.trim "\t\r\n"                  //= ""
%str.trim ""                         //= ""
%str.trim " 🚀 "                     //= "🚀"
```

`ascii_lower` and `ascii_upper` change the case of ASCII letters and leave every other
character as it is. That is the case-insensitivity of protocols — header names, schemes,
hex digits — rather than of natural language, which needs locale-aware rules.

```quiver
%str.ascii_lower "Content-Type"      //= "content-type"
%str.ascii_upper "utf-8"             //= "UTF-8"
%str.ascii_lower "ÉCOLE"             //= "École" // É is not ASCII
```

## Repeating and padding

```quiver
%str.repeat ["ab", 3]                //= "ababab"
%str.repeat ["ab", 0]                //= ""
%str.repeat ["-", 5]                 //= "-----"
```

`pad_start` and `pad_end` widen a string to a width in characters, with a space or a given
one-character fill. A string already that wide is unchanged.

```quiver
%str.pad_start ["7", 3, fill: "0"]   //= "007"
%str.pad_start ["42", 5]             //= "   42"
%str.pad_end ["ab", 4, fill: "."]    //= "ab.."
%str.pad_start ["hello", 3]          //= "hello"
%str.pad_end ["é", 3, fill: "·"]     //= "é··"
%str.pad_start ["1", 3, fill: "ab"]  //! Padding must be a single character
```

## Characters

`length` counts characters, not bytes, so a multi-byte character counts once.

```quiver
%str.length "hello"           //= 5
%str.length ""                //= 0
%str.length "a"               //= 1
%str.length "🚀"              //= 1
%str.length "🚀🌙"            //= 2
%str.length "café"            //= 4
%str.length "日本語"          //= 3
%str.length "hello 🚀 world 🌙"   //= 15
```

`slice` takes character indices, from `start` inclusive to `end` exclusive, so it can never
cut a character in half. An index past the end simply stops at the end.

```quiver
%str.slice ["hello", 0, 5]    //= "hello"
%str.slice ["hello", 1, 4]    //= "ell"
%str.slice ["hello", 0, 1]    //= "h"
%str.slice ["hello", 4, 5]    //= "o"
%str.slice ["hello", 2, 2]    //= ""
%str.slice ["hello", 0, 9]    //= "hello"
%str.slice ["hello", 9, 12]   //= ""
%str.slice ["hello", 3, 1]    //= "" // an end before the start
%str.slice ["hello", -2, 2]   //= "he" // a start before the beginning
%str.slice ["héllo", -2, 2]   //= "hé"
```

```quiver
%str.slice ["🚀🌙⭐", 0, 1]           //= "🚀"
%str.slice ["🚀🌙⭐", 1, 2]           //= "🌙"
%str.slice ["🚀🌙⭐", 0, 3]           //= "🚀🌙⭐"
%str.slice ["hello 🚀 world", 0, 6]   //= "hello "
%str.slice ["hello 🚀 world", 6, 7]   //= "🚀"
%str.slice ["hello 🚀 world", 8, 13]  //= "world"
```

`iter` is the lazy character view and `collect` its inverse. Each character arrives as its
Unicode codepoint, whatever its encoded width — `"é"` is 233 (U+00E9) and `"🚀"` is 128640
(U+1F680), not the numbers their UTF-8 bytes happen to spell. So a codepoint from `iter`
compares against a codepoint from anywhere else.

```quiver
"hello" ~> %str.iter ~ ~> %list.collect ~   //= Cons[104, Cons[101, Cons[108, Cons[108, Cons[111, Nil]]]]]
"" ~> %str.iter ~ ~> %list.collect ~        //= Nil
"é" ~> %str.iter ~ ~> %list.collect ~       //= Cons[233, Nil] // two bytes, one codepoint
"€" ~> %str.iter ~ ~> %list.collect ~       //= Cons[8364, Nil] // three
"🚀" ~> %str.iter ~ ~> %list.collect ~      //= Cons[128640, Nil] // four
"hi🚀" ~> %str.iter ~ ~> %list.collect ~    //= Cons[104, Cons[105, Cons[128640, Nil]]]
```

`collect` encodes each codepoint back to UTF-8, picking the width from its magnitude:

```quiver
%list{ 104, 101, 108, 108, 111 } ~> %list.iter ~ ~> %str.collect ~   //= "hello"
Nil ~> %list.iter ~ ~> %str.collect ~                                //= ""
%list{ 128640 } ~> %list.iter ~ ~> %str.collect ~                    //= "🚀"
%list{ 72, 233, 8364, 128640 } ~> %list.iter ~ ~> %str.collect ~     //= "Hé€🚀"
```

The two round-trip, and any `%iter` operation may sit between them:

```quiver
"hello world 🚀" ~> %str.iter ~ ~> %str.collect ~                  //= "hello world 🚀"
"hello" ~> %str.iter ~ ~> %iter.take [~, 3] ~> %str.collect ~      //= "hel"
"hello" ~> %str.iter ~ ~> %iter.drop [~, 2] ~> %str.collect ~      //= "llo"
```

## Bytes that are not UTF-8

A string is just a tuple, so nothing stops its bytes from being malformed UTF-8 — a truncated
read, a stray byte, a surrogate. Nothing checks for it either: operations on bytes carry such
bytes through untouched, and operations on characters apply one rule to them. A character is a
byte that is not a continuation byte (`10xxxxxx`), together with the continuation bytes after
it; one that is not a well-formed encoding reads as U+FFFD, the replacement character.

```quiver
%str.iter Str[<41806142>] ~> %list.collect ~   //= Cons[65533, Cons[97, Cons[66, Nil]]] // A plus a stray byte is one bad character
%str.iter Str[<e282>] ~> %list.collect ~       //= Cons[65533, Nil] // truncated
%str.iter Str[<eda080>] ~> %list.collect ~     //= Cons[65533, Nil] // a UTF-16 surrogate
%str.iter Str[<c0af>] ~> %list.collect ~       //= Cons[65533, Nil] // an overlong "/"
%str.length Str[<41806142>]                    //= 3
```

Continuation bytes with nothing before them are not a character at all, so `length` and
`iter` skip them alike. `slice` cuts at character boundaries without decoding, so whatever lies
between them is kept exactly:

```quiver
%str.iter Str[<8041>] ~> %list.collect ~       //= Cons[65, Nil]
%str.slice [Str[<e28241ff>], 1, 3] ~> .0       //= <41ff>
```

`collect` is the other direction, and there a number that is no codepoint — negative, a
surrogate, or past U+10FFFF — is a mistake in the program rather than in some data, so it fails:

```quiver
%list{ 55296 } ~> %list.iter ~ ~> %str.collect ~   //! Not a Unicode code point: 55296
```

## Integers

`from_int` renders an integer in decimal, and `parse_int` reads one back. Since a hole must
hold a string, `from_int` is what puts a number into an interpolated literal.

```quiver
%str.from_int 0               //= "0"
%str.from_int 42              //= "42"
%str.from_int -42             //= "-42"
```

`parse_int` is a match, not a scan: it answers nil unless the *whole* string is an
optionally-signed run of decimal digits.

```quiver
%str.parse_int "42"           //= 42
%str.parse_int "-42"          //= -42
%str.parse_int "007"          //= 7
%str.parse_int ""             //= []
%str.parse_int "12a"          //= []
%str.parse_int "-"            //= []
%str.parse_int " 1"           //= [] // no surrounding whitespace is allowed
```

The pair round-trips, and the arbitrary precision of `'int` survives it:

```quiver
%str.from_int 123456789012345678901234567890 ~> %str.parse_int ~
//= 123456789012345678901234567890
```

Either takes a radix from 2 to 36 in place of decimal, given alongside the value. Digits past 9
are letters: `from_int` writes them lowercase, and `parse_int` reads either case.

```quiver
%str.from_int [255, radix: 16]        //= "ff"
%str.from_int [-5, radix: 2]          //= "-101"
%str.parse_int ["FF", radix: 16]      //= 255
%str.parse_int ["-1a", radix: 16]     //= -26
%str.parse_int ["12", radix: 2]       //= [] // 2 is not a binary digit
%str.from_int [18446744073709551616, radix: 36] ~> %str.parse_int [~, radix: 36]
//= 18446744073709551616
```

A radix outside that range is a mistake in the program rather than in the text, so it fails:

```quiver
%str.from_int [5, radix: 37]          //! Radix must be between 2 and 36
```
