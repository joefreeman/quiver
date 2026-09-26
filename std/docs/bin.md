# %bin

Operations over `'bin`, the primitive byte string. A binary is immutable, so every operation
here answers a new one; concatenation shares structure rather than copying, so building a
buffer by repeated `concat` is cheap.

A binary literal is hex digits between angle brackets, and `<>` is the empty binary. The
module's own functions take and return exactly that.

```quiver
<68656c6c6f>                  //= ('bin)
%bin.length <68656c6c6f>      //= 5
```

A string is a `Str`-wrapped binary and nothing more, so `.0` is how you reach the bytes of
one, and `Str[…]` is how you go back.

```quiver
"hello" ~> .0                 //= <68656c6c6f>
Str[<68656c6c6f>]             //= "hello"
```

Indices are byte offsets from the front, counting from zero. Where an operation is given one
past the end it is a runtime error, not a nil: a bad index is the caller's mistake, not
something the world decided.

```quiver
%bin.get_byte [<ff>, 5]       //! Not enough bits
%bin.slice [<68656c>, 0, 99]  //! Index out of bounds
```

## Construction and length

`new` builds a zero-filled binary of a given length, and `length` reads one back.

```quiver
%bin.new 5                    //= <0000000000>
%bin.new 0                    //= <>
%bin.new 5 ~> %bin.length ~   //= 5
%bin.length <>                //= 0
```

`concat` joins two binaries. It is O(1) — the result is a rope over the two operands, not a
copy — and the empty binary is its identity.

```quiver
%bin.concat [<68656c>, <6c6f>]   //= <68656c6c6f>
%bin.concat [<>, <ff>]           //= <ff>
%bin.concat [<ff>, <>]           //= <ff>
```

```quiver
a = %bin.new 2
b = %bin.new 3
%bin.concat [a, b]            //= <0000000000>
```

Nothing else in the module can tell a rope from a flat binary; the sharing is an
implementation detail of `concat` alone.

```quiver
%bin.concat [<6162>, <0a63>] ~> %bin.length ~   //= 4
```

## Slicing and searching

`slice` takes a start index (inclusive) and an end index (exclusive) — so the length of the
result is the difference, and an empty range is the empty binary. `<68656c6c6f>` is `"hello"`:

```quiver
%bin.slice [<68656c6c6f>, 0, 3]   //= <68656c> // "hel"
%bin.slice [<68656c6c6f>, 2, 5]   //= <6c6c6f> // "llo"
%bin.slice [<68656c6c6f>, 1, 4]   //= <656c6c> // "ell"
%bin.slice [<68656c6c6f>, 2, 2]   //= <>
%bin.slice [<68656c6c6f>, 0, 5]   //= <68656c6c6f>
```

`index` finds the first occurrence of a byte at or after an offset. It answers nil when there
is none — an ordinary "found nothing", so it ends the sequence like any other.

```quiver
%bin.index [<68656c6c6f>, 108, 0]   //= 2 // the first 'l'
%bin.index [<68656c6c6f>, 108, 3]   //= 3 // the offset skips it; the second 'l'
%bin.index [<68656c6c6f>, 111, 0]   //= 4 // 'o'
%bin.index [<68656c6c6f>, 122, 0]   //= [] // no 'z'
%bin.index [<68656c6c6f>, 104, 5]   //= [] // the offset is past the end
```

The search crosses a concatenation boundary, since the rope is invisible to it:

```quiver
%bin.concat [<6162>, <0a63>] ~> %bin.index [~, 10, 0]   //= 2
```

## Bytes and bits

`get_byte` and `set_byte` address whole bytes, with a value from 0 to 255.

```quiver
%bin.get_byte [<68656c6c6f>, 0]      //= 104 // <68>
%bin.get_byte [<68656c6c6f>, 1]      //= 101 // <65>
%bin.get_byte [<68656c6c6f>, 4]      //= 111 // <6f>
%bin.set_byte [<00000000>, 0, 255]   //= <ff000000>
%bin.set_byte [<00000000>, 2, 170]   //= <0000aa00>
```

A buffer allocated by `new` is written the same way, and the operations compose — the
allocate/write/join sequence is how a wire frame gets built:

```quiver
b = %bin.new 3
%bin.set_byte [b, 1, 255]     //= <00ff00>

a = %bin.new 2
c = %bin.set_byte [a, 0, 255] ~> %bin.set_byte [~, 1, 170]
%bin.concat [<ff00>, c]       //= <ff00ffaa>
```

`get_bit` and `set_bit` address single bits, **most significant first**: bit 0 is the top bit
of byte 0, bit 7 the bottom bit of byte 0, bit 8 the top bit of byte 1. That is the numbering
a wire format uses, so a bitmap read this way matches the way it is written down.

```quiver
%bin.get_bit [<80>, 0]         //= 1 // <80> is 1000_0000
%bin.get_bit [<80>, 7]         //= 0
%bin.get_bit [<ff>, 3]         //= 1
%bin.get_bit [<ff00>, 8]       //= 0 // the first bit of the second byte
```

```quiver
%bin.set_bit [<00>, 0, 1]      //= <80>
%bin.set_bit [<00>, 7, 1]      //= <01>
%bin.set_bit [<ff>, 0, 0]      //= <7f>
%bin.set_bit [<0000>, 8, 1]    //= <0080>
```

Setting bits in sequence builds a bitmap of the kind a hash-array-mapped trie keeps — bits 0,
3 and 7 of one byte are `1001_0001`:

```quiver
bitmap = %bin.new 1
%bin.set_bit [bitmap, 0, 1] ~> %bin.set_bit [~, 3, 1] ~> %bin.set_bit [~, 7, 1]   //= <91>
```

A byte value out of range is rejected rather than truncated:

```quiver
%bin.set_byte [<00>, 0, 300]   //! does not fit
```

## Appending numbers

`append` writes a non-negative integer onto the end of a binary as a given number of
big-endian bytes, from 1 to 8. This is how a wire format's length prefixes and fixed-width
fields are laid down.

```quiver
%bin.append [<68656c>, 108, 1]     //= <68656c6c>
%bin.append [<>, 1751477356, 4]    //= <68656c6c> // the same four bytes, in one go
```

The width is stated, not inferred, so a small value still occupies its full field:

```quiver
%bin.append [<>, 1, 4]             //= <00000001>
%bin.append [<>, 0, 8]             //= <0000000000000000>
```

Encoding UTF-8 by hand shows the pattern — 'A' is one byte, 'é' two, '€' three:

```quiver
%bin.append [<>, 65, 1]
~> %bin.append [~, 50089, 2]
~> %bin.append [~, 14844588, 3]   //= <41c3a9e282ac> // "Aé€"
```

A value that does not fit its stated width, or a negative one, is a runtime error:

```quiver
%bin.append [<>, 256, 1]           //! does not fit
%bin.append [<>, -1, 1]            //! cannot be negative
```

## Bitwise operations

`not` complements every bit, and preserves the length.

```quiver
%bin.not <00>                 //= <ff>
%bin.not <ff>                 //= <00>
%bin.not <f0>                 //= <0f>
```

`and`, `or` and `xor` combine two binaries byte by byte, aligned at the **front**.

```quiver
%bin.and [<ff>, <f0>]         //= <f0>
%bin.and [<aa>, <cc>]         //= <88>
%bin.or [<f0>, <0f>]          //= <ff>
%bin.or [<aa>, <55>]          //= <ff>
%bin.xor [<ff>, <f0>]         //= <0f>
%bin.xor [<aa>, <aa>]         //= <00>
```

Unequal lengths are where they differ. `and` truncates to the shorter operand — a byte with
no partner cannot be set in the result anyway. `or` and `xor` extend to the longer, treating
the missing bytes as zero, which is likewise what leaves the result unchanged.

```quiver
%bin.and [<ffff>, <f0>]       //= <f0>
%bin.or [<f0f0>, <0f>]        //= <fff0>
%bin.xor [<ffff>, <f0>]       //= <0fff>
```

`shift` moves the bits by a count — left when positive, right when negative — with zeros
shifted in and the length preserved, so bits shifted off the end are lost. Byte 0 is the most
significant, so a left shift moves bits toward the front.

```quiver
%bin.shift [<01>, 1]          //= <02>
%bin.shift [<0f>, 4]          //= <f0>
%bin.shift [<02>, -1]         //= <01>
%bin.shift [<f0>, -4]         //= <0f>
%bin.shift [<ff>, 0]          //= <ff>
```

```quiver
%bin.shift [<0102>, 8]        //= <0200>
%bin.shift [<0102>, -8]       //= <0001>
%bin.shift [<ff>, 100]        //= <00> // shifted out entirely
```

Chained, the two families read as one expression — `¬(a ∧ b)`:

```quiver
%bin.and [<aa>, <ff>] ~> %bin.not ~   //= <55>
```

## Hex

`to_hex` renders a binary as lowercase hex text, two characters per byte. `from_hex` reads it
back, accepting either case.

```quiver
%bin.to_hex <deadbeef>        //= "deadbeef"
%bin.to_hex <>                //= ""
%bin.from_hex "DeadBEEF"      //= <deadbeef>
%bin.from_hex "deadbeef"      //= <deadbeef>
```

Text that is not hex is nil rather than an error — it is input, so failing to parse it is a
value the caller can branch on.

```quiver
%bin.from_hex "abc"           //= [] // odd length
%bin.from_hex "zz"            //= [] // not hex digits
```

This is the readable form of a digest, and is what a hash is usually compared in:

```quiver
d = "abc" ~> .0 ~> %hash.sha256 ~
%bin.to_hex d                           //= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
%bin.to_hex d ~> %bin.from_hex ~        //= &d // the round trip is exact
```

## Base64

`to_base64` and `from_base64` are RFC 4648's standard alphabet, with padding. The RFC's own
test vectors:

```quiver
%bin.to_base64 <>                  //= ""
"f" ~> .0 ~> %bin.to_base64 ~      //= "Zg=="
"fo" ~> .0 ~> %bin.to_base64 ~     //= "Zm8="
"foo" ~> .0 ~> %bin.to_base64 ~    //= "Zm9v"
"foob" ~> .0 ~> %bin.to_base64 ~   //= "Zm9vYg=="
"fooba" ~> .0 ~> %bin.to_base64 ~  //= "Zm9vYmE="
"foobar" ~> .0 ~> %bin.to_base64 ~ //= "Zm9vYmFy"
```

Decoding round-trips them:

```quiver
%bin.from_base64 "Zm9vYmFy" ~> Str[~]   //= "foobar"
%bin.from_base64 "Zg==" ~> Str[~]       //= "f"
%bin.from_base64 "Zm9vYmE=" ~> Str[~]   //= "fooba"
%bin.from_base64 "" ~> Str[~]           //= ""
```

Padding is required, and it may only appear in the final quantum — so a malformed string is
nil, and a decoder cannot be talked into accepting two concatenated messages as one.

```quiver
%bin.from_base64 "ba!d"       //= [] // characters outside the alphabet
%bin.from_base64 "abcde"      //= [] // length not a multiple of four
%bin.from_base64 "Zg==Zm9v"   //= [] // padding before the final quantum
```
