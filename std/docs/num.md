# %num

Arithmetic over three kinds of number, all exact:

- an **integer**, `'int`, of arbitrary precision;
- an exact **rational**, `Rational['int, 'int]`, always in canonical form — denominator
  positive, terms coprime;
- a single-radical **surd**, `Surd[a, b, n]`, denoting `a + b·√n`, where `a` and `b` are
  integers or rationals.

Every operation is closed over these and never approximates. Where a result cannot be
expressed — a negative square root, arithmetic across two different radicals — the answer is
nil, and the sequence ends as it would for any other failure.

```quiver
%num.add [0.1, 0.2]           //= 3/10 // exact, unlike binary floating point
```

## Numeric literals

A decimal or fraction literal is sugar for a reduced `Rational`. Reduction happens at compile
time, so `0.25` and `1/4` are the same value.

```quiver
1.5                           //= 3/2
0.25                          //= 1/4
0.30                          //= 3/10
1/3                           //= 1/3
2/4                           //= 1/2
```

Negation is part of the literal:

```quiver
-1.5                          //= -3/2
-2/4                          //= -1/2
```

An integer-valued literal reduces but does **not** become an `'int`. `4/2` is the rational
`2/1`, which is a different value from the integer `2` — same number, different kind.

```quiver
4/2                           //= 2/1
6/3                           //= 2/1
1.0                           //= 1/1
-9/3                          //= -3/1
```

A numeric literal never disturbs positional field access, even though both use `.`:

```quiver
x = [10, 20]; x.0             //= 10
x = [[1, 99], 20]; x.0.1      //= 99
```

## Arithmetic

Integers are closed under `add`, `sub` and `mul`: integer in, integer out.

```quiver
%num.add [2, 3]               //= 5
%num.sub [5, 2]               //= 3
%num.mul [6, 7]               //= 42
```

A rational operand makes the result rational, whichever side it is on:

```quiver
%num.add [1/2, 3]             //= 7/2
%num.add [3, 1/2]             //= 7/2
%num.add [1/3, 1/6]           //= 1/2
%num.mul [2/3, 3/4]           //= 1/2
```

A rational result is never lowered back to an integer, even when its value is integral —
the kind follows the operands, not the answer.

```quiver
%num.add [1/2, 1/2]           //= 1/1
%num.sub [3/2, 1/2]           //= 1/1
```

`neg` and `abs` preserve the kind:

```quiver
%num.neg 3/4                  //= -3/4
%num.neg 5                    //= -5
%num.abs -3/4                 //= 3/4
%num.abs 3/4                  //= 3/4
%num.abs 42                   //= 42
%num.abs -42                  //= 42
%num.abs 0                    //= 0
```

## Division

`div` is exact division, so it always answers a rational — dividing two integers included.
For truncating integer division, use `%int.div`.

```quiver
%num.div [1/2, 3/4]           //= 2/3
%num.div [1, 2]               //= 1/2
%num.div [6, 3]               //= 2/1
```

A zero divisor answers nil:

```quiver
%num.div [1/2, 0]             //= []
%num.div [1, 0]               //= []
```

## Comparison

The predicates are polymorphic across the kinds, and compare by **value**, so an integer and
a rational denoting the same number are equal despite their different representations.

```quiver
%num.eq? [1/2, 2/4]           //= Ok
%num.eq? [1/2, 1/3]           //= []
%num.eq? [2, 2]               //= Ok
%num.eq? [2, 4/2]             //= Ok // same number, different kinds
```

```quiver
%num.lt? [1/3, 1/2]           //= Ok
%num.lt? [1/2, 1/3]           //= []
%num.le? [1/2, 1/2]           //= Ok
%num.gt? [1/2, 1/3]           //= Ok
%num.ge? [3, 2]               //= Ok
%num.ge? [2, 3]               //= []
```

## Conversions and accessors

`to_int` truncates toward zero. `numer` and `denom` read a rational's terms, and treat an
integer as itself over one.

```quiver
%num.to_int 7/2               //= 3
%num.to_int -7/2              //= -3
%num.to_int 5                 //= 5
%num.numer 3/4                //= 3
%num.denom 3/4                //= 4
%num.numer 5                  //= 5
%num.denom 5                  //= 1
```

## Matching numeric literals

A literal in a pattern tests, as any other literal does — and because the literal is a
reduced rational, so is the test.

```quiver
%num.add [0.1, 0.2] ~> =0.3   //= Ok
2/4 ~> =1/2                   //= Ok
%num.add [0.1, 0.2] ~> =0.4   //= []
```

Kind is part of the match: `4/2` is a rational, so it matches `=2/1` and not `=2`.

```quiver
4/2 ~> =2/1                   //= Ok
4/2 ~> =2                     //= []
```

## Surds

`sqrt` answers the exact square root. When the root is rational it simply is one; otherwise
the result is a surd with its square factor extracted.

```quiver
%num.sqrt 4                   //= 2
%num.sqrt 9                   //= 3
%num.sqrt 0                   //= 0
%num.sqrt 1/4                 //= 1/2
```

A surd is an ordinary `Surd[a, b, n]` tuple denoting `a + b·√n`, so it can be matched like
any other value. It *renders* in mathematical notation — `√2`, `2√2`, `1 + √2` — which is a
display convention, not a second representation.

```quiver
%num.sqrt 2                   //= Surd[0, 1, 2] // √2
%num.sqrt 8                   //= Surd[0, 2, 2] // 2√2, the square factor pulled out
%num.sqrt 12                  //= Surd[0, 2, 3] // 2√3
%num.sqrt 1/2                 //= Surd[0, 1/2, 2] // (1/2)√2
```

Two roots have no answer in this domain, and are nil: a negative one, and the root of a surd
(denesting is out of scope).

```quiver
%num.sqrt -1                  //= []
%num.sqrt 2 ~> %num.sqrt ~    //= []
```

### Arithmetic over ℚ(√n)

Within one radical, arithmetic is closed and exact. Like terms collect:

```quiver
s = %num.sqrt 2
%num.add [s, s]               //= Surd[0, 2, 2] // √2 + √2 = 2√2
%num.add [s, %num.sqrt 8]     //= Surd[0, 3, 2] // √2 + 2√2 = 3√2
%num.add [1, s]               //= Surd[1, 1, 2] // 1 + √2
```

A result whose radical part cancels collapses back to a bare rational or integer:

```quiver
s = %num.sqrt 2
x = %num.add [1, s]
%num.sub [x, s]               //= 1 // (1 + √2) − √2
%num.mul [s, s]               //= 2 // √2 · √2
```

Multiplying conjugates, and dividing — which rationalises the denominator:

```quiver
s = %num.sqrt 2
a = %num.add [1, s]
b = %num.add [1, %num.neg s]
%num.mul [a, b]               //= -1 // (1 + √2)(1 − √2)
%num.div [1, s]               //= Surd[0, 1/2, 2] // 1/√2 = (1/2)√2
```

`neg` and `abs` behave as they do for the other kinds:

```quiver
%num.sqrt 2 ~> %num.neg ~                  //= Surd[0, -1, 2] // −√2
%num.sqrt 2 ~> %num.neg ~ ~> %num.abs ~    //= Surd[0, 1, 2] // √2
```

Two *different* radicals live in different fields, and arithmetic across them is unsupported
rather than approximated — so it answers nil:

```quiver
a = %num.sqrt 2
b = %num.sqrt 3
%num.mul [a, b]               //= []
%num.add [a, b]               //= []
%num.eq? [a, b]               //= []
```

### Ordering and equality

Surds order against rationals exactly, so a comparison brackets the irrational value without
ever approximating it. √2 = 1.41421356…:

```quiver
s = %num.sqrt 2
%num.gt? [s, 1]               //= Ok
%num.lt? [s, 1]               //= []
%num.lt? [s, 3/2]             //= Ok
%num.lt? [s, 141/100]         //= [] // √2 > 1.41
%num.lt? [s, 142/100]         //= Ok // √2 < 1.42
%num.lt? [%num.neg s, 0]      //= Ok
```

Equality is by value, so construction order does not matter:

```quiver
s = %num.sqrt 2
%num.eq? [s, s]                                  //= Ok
%num.eq? [%num.add [1, s], %num.add [s, 1]]      //= Ok
```

`to_int` truncates toward zero here too — √2 ≈ 1.414, 1 + √2 ≈ 2.414, 5√2 ≈ 7.07,
−3 + √2 ≈ −1.586, √(1/2) ≈ 0.707:

```quiver
s = %num.sqrt 2
%num.to_int s                                //= 1
%num.neg s ~> %num.to_int ~                  //= -1
%num.add [1, s] ~> %num.to_int ~             //= 2
%num.mul [s, 5] ~> %num.to_int ~             //= 7
%num.add [%num.neg 3, s] ~> %num.to_int ~    //= -1
%num.sqrt 1/2 ~> %num.to_int ~               //= 0
```

### Worked example: the golden ratio

φ = (1 + √5)/2, exactly. `sqrt` and `div` are both fallible, so each is bound before reuse —
binding narrows the nil away, and a failure would end the sequence at that step.

```quiver
r = %num.sqrt 5
s = %num.add [1, r]
phi = %num.div [s, 2]         //= Surd[1/2, 1/2, 5] // 1/2 + (1/2)√5
```

It sits between consecutive Fibonacci ratios, 8/5 and 13/8:

```quiver
r = %num.sqrt 5
s = %num.add [1, r]
phi = %num.div [s, 2]
%num.gt? [phi, 8/5]           //= Ok
%num.lt? [phi, 13/8]          //= Ok
```

And it satisfies φ² = φ + 1 — checked exactly, which floating point could not do:

```quiver
r = %num.sqrt 5
s = %num.add [1, r]
phi = %num.div [s, 2]
%num.eq? [%num.mul [phi, phi], %num.add [phi, 1]]   //= Ok
```
