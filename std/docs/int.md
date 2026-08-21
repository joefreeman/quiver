# %int

Operations defined only over the integers. Anything with an analogue across the other kinds of
number — `add`, `sub`, `mul`, exact `div`, the comparisons — lives in `%num` and works over
rationals and surds too. What is here has no such analogue: truncating quotient and modulo,
integer square root, and the bitwise primitives.

```quiver
%int.div [17, 5]              //= 3   where %num.div, being exact, would answer 17/5
```

## Quotient and remainder

`div` is the quotient truncated toward zero and `mod` the remainder left over, so that
`a = div·b + mod`.

```quiver
%int.div [20, 4]              //= 5
%int.div [17, 5]              //= 3
%int.mod [17, 5]              //= 2
```

Truncation is toward zero rather than toward negative infinity, and the remainder takes the
*dividend's* sign — which is what makes the identity hold in all four sign combinations.

```quiver
%int.div [-17, 5]             //= -3
%int.mod [-17, 5]             //= -2
%int.div [17, -5]             //= -3
%int.mod [17, -5]             //= 2
%int.div [-17, -5]            //= 3
%int.mod [-17, -5]            //= -2
```

Both are exact at any size, since `'int` is arbitrary precision:

```quiver
%int.div [1000000000000000000000000, 7]   //= 142857142857142857142857
%int.mod [1000000000000000000000000, 7]   //= 1
```

A zero divisor is nil rather than an abort, so it ends the sequence like any other failure —
and an ordinary branch recovers from it:

```quiver
%int.div [10, 0]              //= []
%int.mod [10, 0]              //= []
{ %int.mod [10, 0] | -1 }     //= -1
```

## Integer square root

`sqrt` is the square root rounded down: the largest `r` with `r² ≤ n`.

```quiver
%int.sqrt 16                  //= 4
%int.sqrt 17                  //= 4
%int.sqrt 24                  //= 4
%int.sqrt 25                  //= 5
%int.sqrt 0                   //= 0
%int.sqrt 1000000             //= 1000
```

A negative input has no integer root, so it answers nil, and a branch catches it:

```quiver
%int.sqrt -4                  //= []
-4 ~> { %int.sqrt ~ | 0 }     //= 0
```

Rounding down is the whole difference from `%num.sqrt`, which stays exact by answering a surd:

```quiver
%int.sqrt 8                   //= 2
%num.sqrt 8                   //= Surd[0, 2, 2]   2√2
```

## Bitwise operations

`and`, `or`, `xor` and `not` are the bitwise operations, over the two's-complement
representation of the integer.

```quiver
%int.and [255, 240]           //= 240
%int.and [170, 204]           //= 136
%int.or [240, 15]             //= 255
%int.or [170, 85]             //= 255
%int.or [0, 0]                //= 0
%int.xor [255, 240]           //= 15
%int.xor [170, 170]           //= 0
%int.xor [123, 0]             //= 123
```

`not` complements every bit, which in two's complement is `-n - 1`:

```quiver
%int.not 0                    //= -1
%int.not -1                   //= 0
%int.not 1                    //= -2
```

A negative operand is the bit pattern it denotes, so `-1` is all ones and `-2` is all ones but
the last:

```quiver
%int.and [-1, 255]            //= 255
%int.or [-2, 1]               //= -1
%int.xor [-1, -1]             //= 0
```

`popcount` is the number of set bits:

```quiver
%int.popcount 0               //= 0
%int.popcount 1               //= 1
%int.popcount 255             //= 8
```

That representation is 64 bits wide, which is where the bitwise operations part company with
the rest of the language: `-1` has sixty-four set bits, not endlessly many, and an operand too
large for the window is a runtime error rather than a wider answer.

```quiver
%int.popcount -1              //= 64
```

```quiver
%num.mul [1000000000000, 1000000000000] ~> %int.and [~, 1]   //! does not fit in a 64-bit value
```

## Shifting

`shift` takes a value and a bit count: positive shifts left, negative shifts right, zero is
the identity.

```quiver
%int.shift [1, 1]             //= 2
%int.shift [15, 4]            //= 240
%int.shift [1, 8]             //= 256
%int.shift [2, -1]            //= 1
%int.shift [240, -4]          //= 15
%int.shift [256, -8]          //= 1
%int.shift [123, 0]           //= 123
%int.shift [0, 0]             //= 0
```

The right shift is *arithmetic*: it sign-extends, so a negative value stays negative and the
shift is a division rounding toward negative infinity.

```quiver
%int.shift [-8, -1]           //= -4
%int.shift [-16, -2]          //= -4
%int.shift [-1, -1]           //= -1
```

Shifting by 64 or more moves every bit out of the window. Leftward that is always zero;
rightward it is whatever the sign bit says — zero, or `-1`.

```quiver
%int.shift [123, 64]          //= 0
%int.shift [123, 100]         //= 0
%int.shift [123, -64]         //= 0
%int.shift [-123, -64]        //= -1
```

## Worked example: reading a bit field

A hash array mapped trie addresses each level with a five-bit slice of a hash: shift the slice
down to the bottom, then mask off the rest. `0x12345678` is 305419896.

```quiver
chunk = #[hash: 'int, depth: 'int] {
  %num.mul [$depth, 5] ~> %num.neg ~ ~> %int.shift [$hash, ~] ~> %int.and [~, 31]
}
chunk [hash: 305419896, depth: 0]   //= 24
chunk [hash: 305419896, depth: 1]   //= 19
chunk [hash: 305419896, depth: 2]   //= 21
```

The operations compose in a chain like any others — here masking to a byte, setting the low
nibble, complementing, and shifting the result up:

```quiver
%int.and [255, 240] ~> %int.or [~, 15] ~> %int.not ~ ~> %int.shift [~, 8]   //= -65536
```
