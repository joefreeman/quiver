# %vec

Packed numeric vectors. A vector is a flat byte buffer of fixed-width integer lanes, tagged with
a lane type and a **scale** drawn from the [`%num`](num.md) tower. The logical value of lane *i*
is `storedᵢ × scale`, so integer lanes carry fractional values exactly — fixed point, with no
floating point anywhere.

```quiver
%vec.of [I32, 1/2, %list{ 1, 2, 3 }] ~> %vec.sum ~   //= 3 // stored [1,2,3] at ½ is [½, 1, 1½]
```

Every operation is exact. Where a result cannot be represented — a lane that overflows its
width, two vectors whose lane types differ — the answer is nil, and the sequence ends as it
would for any other failure.

## The representation

A vector is an ordinary tuple: `Vec[dtype, scale, data]`. Nothing is hidden, so it can be
matched, spread and read like any other value.

```quiver
%vec.of [I32, 1, %list{ 1, 2, 3 }]
//= Vec[dtype: I32, scale: 1, data: <010000000200000003000000>]
```

`data` is little-endian two's complement, and `dtype` fixes the lane width: `I32` is four bytes,
`I64` is eight. Signed lanes need no special handling.

```quiver
%vec.of [I64, 1, %list{ 5, -7 }] ~> %vec.sum ~   //= -2
```

Because the buffer is an ordinary binary, it can be built by hand — which is how `fill`
broadcasts, tiling one lane rather than materialising the repetition:

```quiver
%vec.of [I32, 1, %list{ 7 }] ~> =('%vec.vec & unit)
[unit.data, 3] ~> __binary_repeat__ ~ ~> __vector_sum__ [~, 4]   //= 21
```

The kernels do assume what the constructors guarantee: a buffer that is a whole number of lanes.
A **ragged** buffer is a violated invariant rather than a runtime condition — only reachable by
forging a `Vec` by hand — so it aborts rather than answering nil.

```quiver
Vec[dtype: I32, scale: 1, data: <0102>] ~> %vec.sum ~   //! Ragged vector buffer
```

## Building

`new` makes an empty vector, and `push` appends one **stored** lane.

```quiver
%vec.new [I32, 1]                          //= Vec[dtype: I32, scale: 1, data: <>]
%vec.new [I32, 1] ~> %vec.push [~, 5] ~> %vec.push [~, 6] ~> %vec.sum ~   //= 11
```

`of` does the same from a list of stored lanes in one step. Its fields are optional labels, so
they may be written either way:

```quiver
%vec.of [I32, 1, %list{ 1, 2, 3 }] ~> %vec.sum ~                       //= 6
%vec.of [dtype: I32, scale: 1, lanes: %list{ 1, 2, 3 }] ~> %vec.sum ~   //= 6
```

`fill` is a vector of a given length with every lane the same, stored compactly until an
operation realises it:

```quiver
%vec.fill [I32, 1, 100, 3] ~> %vec.sum ~   //= 300
%vec.fill [I32, 1, 100, 3] ~> %vec.len ~   //= 3
```

A lane that does not fit the dtype is nil, rather than wrapping:

```quiver
%vec.new [I32, 1] ~> %vec.push [~, 2147483648]   //= []
%vec.of [I32, 1, %list{ 9999999999 }]            //= []
```

## Reading

`len` counts lanes, `scale` reads the scale, and `get` reads one lane as an exact value.

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
a ~> %vec.len ~        //= 3
a ~> %vec.scale ~      //= 1
[a, 0] ~> %vec.get ~   //= 1
[a, 2] ~> %vec.get ~   //= 3
```

An index outside the vector is nil, as a missing thing is everywhere else:

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
[a, 3] ~> %vec.get ~    //= []
[a, -1] ~> %vec.get ~   //= []
```

`sum` and `dot` are exact reductions: `sum` is `(Σ stored) × scale`, `dot` is
`(Σ aᵢbᵢ) × sₐ × s_b`. An empty vector sums to zero.

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
a ~> %vec.sum ~        //= 6
[a, a] ~> %vec.dot ~   //= 14
%vec.new [I32, 1] ~> %vec.sum ~   //= 0
```

`dot` needs matching lane types and lengths; anything else is nil.

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
[a, %vec.fill [I32, 1, 2, 2]] ~> %vec.dot ~   //= []
```

Unlike `%num`, where the kind follows the operands, `%vec` canonicalises: a result whose value is
integral comes back as an `'int`, whatever the scale was. That is what keeps the assertions above
readable, and what makes scale equality — which `add` relies on — a structural test.

```quiver
%vec.of [I32, 1/2, %list{ 1, 2, 3 }] ~> %vec.sum ~   //= 3 // 6 × ½, not 3/1
%vec.of [I32, 4/2, %list{ 1 }] ~> %vec.scale ~       //= 2 // the scale itself, too
```

## Lane-wise arithmetic

`add`, `sub` and `mul` combine two vectors lane by lane.

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
[a, %vec.fill [I32, 1, 10, 3]] ~> %vec.add ~ ~> %vec.sum ~   //= 36 // [11,12,13]
[a, a] ~> %vec.sub ~ ~> %vec.sum ~                           //= 0
[a, a] ~> %vec.mul ~ ~> %vec.sum ~                           //= 14 // [1,4,9]
```

The lane types must agree — an `I32` and an `I64` vector cannot be combined — and a lane that
overflows its width is nil rather than a wrapped value.

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
[a, %vec.of [I64, 1, %list{ 1, 2, 3 }]] ~> %vec.add ~   //= []
```

```quiver
hi = %vec.of [I32, 1, %list{ 2147483647 }]
[hi, %vec.fill [I32, 1, 1, 1]] ~> %vec.add ~   //= []
```

## Scale

The scale is what makes a buffer of integers a vector of exact fractions. It is metadata: `sum`
and `get` apply it, and nothing about the buffer changes.

```quiver
h = %vec.of [I32, 1/2, %list{ 1, 2, 3 }]   //= Vec(data: <010000000200000003000000>)
[h, 2] ~> %vec.get ~   //= 3/2 // stored 3, at scale ½
h ~> %vec.sum ~        //= 3
```

`scale_by` multiplies the whole vector by a number, exactly, by adjusting the scale alone — no
lane is touched and no overflow is possible.

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
[a, 1/2] ~> %vec.scale_by ~ ~> %vec.scale ~   //= 1/2
[a, 1/2] ~> %vec.scale_by ~ ~> %vec.sum ~     //= 3
```

`mul` combines scales the way the values do: the result's scale is the product, and the lanes
just multiply.

```quiver
h = %vec.of [I32, 1/2, %list{ 2, 2 }]
[h, h] ~> %vec.mul ~ ~> %vec.scale ~   //= 1/4 // ½ × ½
[h, h] ~> %vec.mul ~ ~> %vec.sum ~     //= 2 // logical [1,1] · [1,1]
```

`add` and `sub` cannot: two lanes only add if they mean the same unit. So unequal scales are
first **reconciled** to the largest scale both are exact multiples of — the gcd of the two — and
the buffers rescaled to match. Equal scales skip all of it.

```quiver
p = %vec.fill [I32, 1/2, 1, 1]
q = %vec.fill [I32, 1/3, 1, 1]
[p, q] ~> %vec.add ~ ~> %vec.scale ~   //= 1/6 // ½ and ⅓ are 3 and 2 sixths
[p, q] ~> %vec.add ~ ~> %vec.sum ~     //= 5/6
```

```quiver
[%vec.fill [I32, 1, 1, 1], %vec.fill [I32, 1/4, 1, 1]] ~> %vec.sub ~ ~> %vec.sum ~   //= 3/4
```

Rescaling multiplies lanes, so it can overflow — in which case, as ever, the answer is nil.

## Building from values

The constructors above take *stored* lanes. Three more take the values themselves and work out
the lanes.

`of_exact` divides each value by a scale you choose, and requires the result to be an exact
integer.

```quiver
%vec.of_exact [I32, 1/2, %list{ 1/2, 3/2 }] ~> %vec.sum ~   //= 2 // stored [1, 3]
%vec.of_exact [I32, 1/2, %list{ 1/3 }]                      //= [] // ⅓ is no multiple of ½
```

`of_round` takes the same arguments but rounds to the nearest multiple of the scale — lossy, and
so never nil for a representable magnitude. Halves round away from zero.

```quiver
%vec.of_round [I32, 1/100, %list{ 333/1000, 1/2 }] ~> %vec.sum ~   //= 83/100
```

```quiver
%vec.of_round [I32, 1, %list{ -3/2, 3/2 }] ~> =('%vec.vec & r)
[r, 0] ~> %vec.get ~   //= -2
[r, 1] ~> %vec.get ~   //= 2
```

`of_values` picks the scale itself: the finest one that represents every value exactly, `1/lcm`
of the denominators. It is the constructor to reach for when the values are what you have and
the layout is not your concern.

```quiver
%vec.of_values [I32, %list{ 1/2, 1/3 }] ~> =('%vec.vec & v)
v ~> %vec.scale ~   //= 1/6 // 1/lcm(2, 3)
v ~> %vec.sum ~     //= 5/6
```

A scale that fine can push a lane past the dtype, which is nil like any other overflow. An
irrational value has no denominator at all, so no scale holds it: that is nil too, carrying the
`:error` from `%num.denom`.

```quiver
%vec.of_values [I32, %list{ %num.pi, 1 }] ~> :error<'%num.error>   //= OutOfDomain
```

## Comparison and filtering

`lt`, `eq` and `gt` compare lane-wise and answer a `Mask` — one byte per lane, non-zero where the
predicate holds.

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
b = %vec.fill [I32, 1, 2, 3]
[a, b] ~> %vec.lt ~   //= Mask[<010000>]
[a, b] ~> %vec.eq ~   //= Mask[<000100>]
[a, b] ~> %vec.gt ~   //= Mask[<000001>]
```

`filter` keeps the lanes a mask selects, preserving dtype and scale.

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
b = %vec.fill [I32, 1, 2, 3]
[a, b] ~> %vec.lt ~ ~> %vec.filter [a, ~] ~> %vec.sum ~   //= 1
[a, b] ~> %vec.gt ~ ~> %vec.filter [a, ~] ~> %vec.sum ~   //= 3
[a, b] ~> %vec.eq ~ ~> %vec.filter [a, ~] ~> %vec.sum ~   //= 2
```

Selecting every lane, or none, is not a special case — an empty vector is a vector.

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
[a, %vec.fill [I32, 1, 10, 3]] ~> %vec.lt ~ ~> %vec.filter [a, ~] ~> %vec.len ~   //= 3
[a, %vec.fill [I32, 1, 0, 3]] ~> %vec.lt ~ ~> %vec.filter [a, ~] ~> %vec.len ~    //= 0
[a, %vec.fill [I32, 1, 0, 3]] ~> %vec.lt ~ ~> %vec.filter [a, ~] ~> %vec.sum ~    //= 0
```

The comparison is between *logical* values, so it reconciles scales first exactly as `add` does.
Here `c` is stored `[4,4,4]` at scale ½ — logical `[2,2,2]` — and the raw lanes would compare the
other way round:

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
c = %vec.fill [I32, 1/2, 4, 3]
[a, c] ~> %vec.lt ~   //= Mask[<010000>]
[a, c] ~> %vec.lt ~ ~> %vec.filter [a, ~] ~> %vec.sum ~   //= 1
```

Mismatched lengths or lane types have no mask to give, and a mask whose length does not match the
vector cannot filter it:

```quiver
a = %vec.of [I32, 1, %list{ 1, 2, 3 }]
[a, %vec.fill [I32, 1, 2, 2]] ~> %vec.lt ~   //= [] // three lanes against two
[a, %vec.fill [I64, 1, 2, 3]] ~> %vec.lt ~   //= [] // I32 against I64
[a, Mask[<0101>]] ~> %vec.filter ~           //= []
```

## Failure propagates

Every exported operation accepts nil in place of any vector, number, mask or index argument, and
answers nil — the same convention `%num`'s tower follows. A fallible pipeline therefore needs no
narrowing at each step: check once at the end.

```quiver
%vec.sum []   //= []
%vec.of [I32, 1, %list{ 5000000000 }] ~> %vec.len ~   //= [] // the overflow surfaces here
```

Only `dtype` stays strict, because it is always a literal and never computed.
