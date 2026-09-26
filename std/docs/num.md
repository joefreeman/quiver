# %num

Arithmetic over five kinds of number, all exact:

- an **integer**, `'int`, of arbitrary precision;
- an exact **rational**, `Rational['int, 'int]`, always in canonical form — denominator
  positive, terms coprime;
- a single-radical **surd**, `Surd[a, b, n]`, denoting `a + b·√n`, where `a` and `b` are
  integers or rationals;
- a **transcendental polynomial**, `Tx[g, terms]`, denoting `Σ c·gᵏ` over the `[k, c]`
  terms, where the generator `g` is `Pi` (π) or `E[d]` (e^(1/d)) — `2π`, `180/π`, `e^(1/2)`;
- a **logarithmic form**, `Log[a, terms]`, denoting `a + Σ c·ln p` over the `[p, c]` terms,
  each `p` prime — `ln 12 = 2·ln 2 + ln 3`.

Every operation is closed over these and never approximates. Where a result cannot be
expressed — a negative square root, arithmetic across two different radicals, or between π
and e — the answer is nil, and the sequence ends as it would for any other failure. The nil
carries the [reason](#failures) as its `:error`. Ordering, on the other hand, is decided for
any two numbers, whatever their kinds.

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

A zero divisor answers nil, carrying `DivisionByZero` as its `:error`:

```quiver
%num.div [1/2, 0]             //= []
%num.div [1, 0] ~> :error     //= DivisionByZero
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
%num.add [0.1, 0.2] ~> =0.3       //= 3/10
2/4 ~> =1/2                       //= 1/2
%num.add [0.1, 0.2] ~> { =0.4 }   //= []
```

Kind is part of the match: `4/2` is a rational, so it matches `=2/1` and not `=2`.

```quiver
4/2 ~> =2/1                   //= 2/1
4/2 ~> { =2 }                 //= []
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
```

They still compare, though: two surds over distinct radicals can never be equal, so bounding
both ever more tightly is certain to tell them apart.

```quiver
a = %num.sqrt 2
b = %num.sqrt 3
%num.eq? [a, b]               //= []
%num.lt? [a, b]               //= Ok
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

## π, e and logarithms

### π

`%num.pi` is π, exactly. Arithmetic with it builds polynomials in π — powers may be negative,
so dividing by π stays exact too:

```quiver
(pi) = %num
pi                             //= Tx[Pi, Cons[[1, 1], Nil]] // π
%num.mul [2, pi]               //= Tx[Pi, Cons[[1, 2], Nil]] // 2π
%num.mul [pi, pi]              //= Tx[Pi, Cons[[2, 1], Nil]] // π²
%num.div [180, pi]             //= Tx[Pi, Cons[[-1, 180], Nil]] // 180π⁻¹, degrees per radian
%num.add [1, pi]               //= Tx[Pi, Cons[[0, 1], Cons[[1, 1], Nil]]] // 1 + π
%num.sub [pi, pi]              //= 0
```

π is transcendental, so no two different polynomials in it are equal: equality is by value,
and a polynomial with nothing of π left collapses back to its coefficient.

```quiver
(pi) = %num
tau = %num.mul [2, pi]
%num.div [tau, 2] ~> %num.eq? [~, pi]   //= Ok
%num.div [tau, pi]                      //= 2
```

Only a single term `c·πᵏ` divides within the representation; anything else answers nil:

```quiver
%num.add [1, %num.pi] ~> %num.div [1, ~]   //= []
```

### Exponentials

`exp` raises e to a power. Any rational power is exact — e^(p/q) is a power of e^(1/q) — and
powers of e multiply by adding exponents:

```quiver
%num.e                                  //= Tx[E[1], Cons[[1, 1], Nil]] // e
%num.exp 0                              //= 1
%num.exp 2                              //= Tx[E[1], Cons[[2, 1], Nil]] // e²
%num.exp 1/2                            //= Tx[E[2], Cons[[1, 1], Nil]] // e^(1/2)
%num.mul [%num.exp 1/2, %num.exp 1/3]   //= Tx[E[6], Cons[[5, 1], Nil]] // e^(5/6)
%num.mul [%num.exp 1/2, %num.exp 1/2]   //= Tx[E[1], Cons[[1, 1], Nil]] // e
%num.sqrt %num.e                        //= Tx[E[2], Cons[[1, 1], Nil]] // √e
%num.mul [2, %num.e] ~> %num.sqrt ~     //= [] // √2·√e, a surd times a power of e
```

π and e are two separate families, and whether they are algebraically independent is an open
problem — so arithmetic mixing them answers nil, as mixed radicals do:

```quiver
%num.add [%num.pi, %num.e]    //= []
```

### Logarithms

`ln` of a positive rational factorises it, and answers the sum of logarithms of primes:

```quiver
%num.ln 12                    //= Log[0, Cons[[2, 2], Cons[[3, 1], Nil]]] // 2 ln 2 + ln 3
%num.ln 3/4                   //= Log[0, Cons[[2, -2], Cons[[3, 1], Nil]]] // −2 ln 2 + ln 3
%num.ln 1                     //= 0
%num.ln -1                    //= []
```

Factorising takes a bounded amount of work, enough to find a prime factor up to about 10¹¹.
A number whose smallest prime factor is larger may not be factorised, and `ln` then answers
nil:

```quiver
%num.ln 18446744073709551617          //= Log[0, Cons[[274177, 1], Cons[[67280421310721, 1], Nil]]] // 2⁶⁴ + 1
%num.ln 340282366920938463463374607431768211457 ~> :error<'%num.error>   //= Unrepresentable // 2¹²⁸ + 1
```

The logarithms of primes are linearly independent over the algebraic numbers (Baker's
theorem), so sums of them add, and scale by coefficients, exactly — including a quotient of
proportional forms. A product of two logarithms is outside the representation:

```quiver
%num.add [%num.ln 2, %num.ln 3] ~> %num.eq? [~, %num.ln 6]   //= Ok
%num.div [%num.ln 8, %num.ln 2]                              //= 3
%num.mul [%num.ln 2, %num.ln 3]                              //= []
```

`exp` and `ln` invert each other wherever the result is representable:

```quiver
%num.exp 3/4 ~> %num.ln ~                        //= 3/4
%num.ln 12 ~> %num.exp ~                         //= 12
%num.ln 2 ~> %num.mul [1/2, ~] ~> %num.exp ~     //= Surd[0, 1, 2] // √2
%num.ln %num.e                                   //= 1
```

A half coefficient on a logarithm makes a square root, which is only representable with no
rational term alongside, since e√2 would mix a surd with a power of e:

```quiver
%num.ln 1000000000039 ~> %num.mul [1/2, ~] ~> %num.exp ~             //= Surd[0, 1, 1000000000039]
%num.ln 2 ~> %num.mul [1/2, ~] ~> %num.add [1, ~] ~> %num.exp ~      //= [] // e√2
```

### Powers

`pow` raises a number to a power. An integer power is exact whenever the products are, so it
takes rationals, surds and polynomials in π or e alike, and a negative power is the
reciprocal:

```quiver
%num.pow [2, 10]                        //= 1024
%num.pow [2/3, -2]                      //= 9/4
%num.pow [%num.sqrt 2, 3]               //= Surd[0, 2, 2] // 2√2
%num.pow [%num.add [1, %num.pi], 2]     //= Tx[Pi, Cons[[0, 1], Cons[[1, 2], Cons[[2, 1], Nil]]]] // 1 + 2π + π²
%num.pow [%num.pi, -1]                  //= Tx[Pi, Cons[[-1, 1], Nil]] // 1/π
%num.pow [%num.ln 2, 2]                 //= [] // a product of logarithms
%num.pow [0, -1]                        //= []
```

A rational power of a rational is exact when the result is rational or a single surd: when,
factorised, every prime's exponent comes out an integer or a half. A negative base takes only
an odd denominator, whose root is real:

```quiver
%num.pow [8, 2/3]                       //= 4
%num.pow [9/4, -1/2]                    //= 2/3
%num.pow [4, 1/4]                       //= Surd[0, 1, 2] // √2
%num.pow [2, 3/2]                       //= Surd[0, 2, 2] // 2√2
%num.pow [-8, 1/3]                      //= -2
%num.pow [-4, 1/2]                      //= []
%num.pow [2, 1/3]                       //= [] // a cube root
```

A monomial in π or e follows its coefficient, as `sqrt` does, and a power of e takes any
exponent `exp` does, logarithms included:

```quiver
%num.pow [%num.e, 1/2]                                     //= Tx[E[2], Cons[[1, 1], Nil]] // √e
%num.mul [%num.pi, %num.pi] ~> %num.mul [4, ~] ~> %num.pow [~, 3/2]   //= Tx[Pi, Cons[[3, 8], Nil]] // 8π³
%num.pow [%num.e, %num.ln 2]                               //= 2
%num.pow [2, %num.pi]                                      //= []
```

### Ordering across fields

Every pair of numbers is ordered, exactly, whatever their kinds. Where the difference is
representable its sign decides; otherwise both values are bounded — each series behind the
bounds counts its own truncation error, so the bounds are proven rather than estimated — until
the bounds separate. π = 3.14159…, e = 2.71828…:

```quiver
(pi, e) = %num
%num.lt? [pi, 22/7]            //= Ok
%num.gt? [pi, 333/106]         //= Ok
%num.lt? [e, pi]               //= Ok
%num.lt? [%num.sqrt 9, pi]     //= Ok
%num.lt? [pi, %num.sqrt 10]    //= Ok
%num.lt? [%num.ln 23, pi]      //= Ok // 3.1355 < 3.1416
%num.max [pi, e]               //= Tx[Pi, Cons[[1, 1], Nil]]
```

Separation always happens against a rational or a surd, and within one family, because the two
values can't be equal. Between two *different* transcendental families — π against e, or
either against a logarithm — no theorem rules equality out, so refinement stops at a precision
budget of 65536 bits. A pair still unseparated there is a runtime error rather than a guess:
it would take a coincidence no one has ever found.

A power of π or e beyond the budget, such as e raised to a trillion, is too large (or too
small) to bound, so it is sized from its exponent instead. A sum of such powers is ordered when
one term, or the powers near the top together, outweigh the rest; otherwise, ordering it is a
runtime error.

```quiver
big = %num.exp 1000000000000
%num.gt? [big, %num.pi]                                       //= Ok
%num.exp -1000000000000 ~> %num.sign ~                        //= 1
%num.exp -1000000000000 ~> %num.lt? [~, 1/1000]               //= Ok
%num.sub [big, %num.exp 999999999999] ~> %num.gt? [~, 1]      //= Ok // eᴺ − eᴺ⁻¹
```

```quiver
c = %num.exp 40000 ~> %num.to_int ~   // ⌊e⁴⁰⁰⁰⁰⌋, so c² is about e⁸⁰⁰⁰⁰
%num.exp 80000 ~> %num.sub [~, %num.mul [c, c]] ~> %num.sign ~   //! none of which outweighs the rest
```

`to_int`, `floor`, `ceil` and `round` refine the same way:

```quiver
(pi) = %num
%num.floor pi                           //= 3
%num.ceil pi                            //= 4
%num.neg pi ~> %num.to_int ~            //= -3
%num.mul [pi, 100] ~> %num.round ~      //= 314
%num.exp 10 ~> %num.to_int ~            //= 22026
%num.ln 1000 ~> %num.floor ~            //= 6
```

A power beyond the budget truncates only when it is below 1. Any other answers nil, since its
integer part could have more bits than the budget:

```quiver
%num.exp -1000000000000 ~> %num.floor ~                                //= 0
%num.exp -1000000000000 ~> %num.neg ~ ~> %num.floor ~                  //= -1
%num.exp 1000000000000 ~> %num.to_int ~ ~> :error<'%num.error>         //= Unrepresentable
```

## Trigonometry

`sin`, `cos` and `tan` take a rational multiple of π, and are exact where the value is
rational or a single surd. By Niven's theorem the only rational sines at rational multiples
of π are 0, ±1/2 and ±1; the rest of the table is surds. All three functions take every
multiple of 30° and 45°. Of the other multiples of 18°, `sin` takes 18° and 54° (and their
reflections, such as 126° and 198°) and `cos` takes 36° and 72°, the complementary angles;
the rest need nested radicals. `tan` takes every multiple of 15° and 22.5°, but no other
multiple of 18°.

```quiver
(pi) = %num
%num.div [pi, 6] ~> %num.sin ~          //= 1/2
%num.div [pi, 4] ~> %num.sin ~          //= Surd[0, 1/2, 2] // (1/2)√2
%num.div [pi, 3] ~> %num.cos ~          //= 1/2
%num.div [pi, 5] ~> %num.cos ~          //= Surd[1/4, 1/4, 5] // (1 + √5)/4
%num.div [pi, 10] ~> %num.sin ~         //= Surd[-1/4, 1/4, 5] // (√5 − 1)/4
%num.mul [pi, 7/6] ~> %num.sin ~        //= -1/2
%num.sin pi                             //= 0
%num.div [pi, 12] ~> %num.tan ~         //= Surd[2, -1, 3] // 2 − √3
%num.div [pi, 8] ~> %num.tan ~          //= Surd[-1, 1, 2] // √2 − 1
```

Anything else answers nil: an angle whose value needs nested or several radicals, a pole of
`tan`, and an argument that isn't a rational multiple of π at all.

```quiver
(pi) = %num
%num.div [pi, 12] ~> %num.cos ~         //= [] // (√6 + √2)/4: two radicals
%num.div [pi, 5] ~> %num.sin ~          //= [] // sin 36° = √(10 − 2√5)/4: nested
%num.div [pi, 10] ~> %num.cos ~         //= [] // cos 18° = √(10 + 2√5)/4: nested
%num.div [pi, 10] ~> %num.tan ~         //= [] // tan 18° = √(25 − 10√5)/5: nested
%num.div [pi, 7] ~> %num.sin ~          //= [] // a cubic irrational
%num.div [pi, 2] ~> %num.tan ~          //= []
%num.sin 1                              //= []
```

The inverse functions read the same tables backwards, answering a multiple of π — `asin` in
[−π/2, π/2], `acos` in [0, π], `atan` in (−π/2, π/2):

```quiver
%num.asin 1/2                          //= Tx[Pi, Cons[[1, 1/6], Nil]] // π/6
%num.acos -1                           //= Tx[Pi, Cons[[1, 1], Nil]] // π
%num.acos 1                            //= 0
%num.atan 1                            //= Tx[Pi, Cons[[1, 1/4], Nil]] // π/4
%num.sqrt 3 ~> %num.atan ~             //= Tx[Pi, Cons[[1, 1/3], Nil]] // π/3
%num.atan 2                            //= []
```

`atan2` takes a point `[y, x]` and answers its angle in (−π, π]:

```quiver
%num.atan2 [1, -1]                     //= Tx[Pi, Cons[[1, 3/4], Nil]] // 3π/4
%num.atan2 [-1, -1]                    //= Tx[Pi, Cons[[1, -3/4], Nil]] // −3π/4
%num.atan2 [0, -1]                     //= Tx[Pi, Cons[[1, 1], Nil]] // π
%num.atan2 [1, 0]                      //= Tx[Pi, Cons[[1, 1/2], Nil]] // π/2
%num.atan2 [0, 0]                      //= []
```

## Approximation

`approx` answers the simplest rational — smallest denominator — within a given distance of a
number, for when an exact value has to meet something that only takes rationals. A rational is
simplified the same way, which tidies one with an unwieldy denominator.

```quiver
(pi) = %num
%num.approx [pi, within: 1/100]              //= 22/7
%num.approx [pi, within: 1/1000000]          //= 355/113
%num.approx [pi, within: 1]                  //= 3
%num.approx [%num.sqrt 2, within: 1/100000]  //= 577/408
%num.approx [3333/10000, within: 1/100]      //= 1/3
%num.approx [3/7, within: 1/10]              //= 1/2
%num.approx [pi, within: 0]                  //= []
```

## The `%num{ … }` dialect

`%num{ … }` writes arithmetic infix. `+`, `-`, `*`, `/` and `^` are `add`, `sub`, `mul`,
`div` and `pow`, with the usual precedence: `^` binds tightest, and to the right.

```quiver
%num{ 1 + 2 * 3 }             //= 7
%num{ 2 ^ 3 ^ 2 }             //= 512 // 2⁹
%num{ -2 ^ 2 }                //= -4 // −(2²)
%num{ 2 ^ -1 }                //= 1/2
%num{ -(1 + 2) * 4 }          //= -12
```

A decimal literal is the same rational as outside the dialect, while `/` is always a
division. So a fraction binds as one:

```quiver
%num{ 0.5 + 1 }               //= 3/2
%num{ 1/3 ^ 2 }               //= 1/9 // 1/(3²)
%num{ (1/3) ^ 2 }             //= 1/9
%num{ 4 / 2 }                 //= 2/1 // `div` answers a rational
```

`pi` (or `π`) and `e` are the constants. Any other operand is a Quiver term, such as a
variable, a field or `~`, evaluated in the surrounding scope:

```quiver
r = 3
%num{ pi * r ^ 2 }            //= Tx[Pi, Cons[[1, 9], Nil]] // 9π
%num{ e ^ (1/2) }             //= Tx[E[2], Cons[[1, 1], Nil]] // √e
[x: 10, y: 4] ~> %num{ ~.x + ~.y / 2 }   //= 12/1
```

## Failures

A nil answered for a non-nil input carries its reason as an `:error` annotation, one of the
`'%num.error` tags:

| tag | when |
| --- | --- |
| `DivisionByZero` | a zero divisor, or zero to a negative power |
| `OutOfDomain` | an input the function has no real answer for: `√−1`, `ln 0`, `tan` at a pole, `asin 2`, `atan2` at the origin, the numerator of π |
| `Unrepresentable` | a result that exists, but not as one of the five kinds: `√2 · √3`, `π + e`, `sin 1` |

```quiver
(pi) = %num
%num.sqrt -1 ~> :error<'%num.error>                        //= OutOfDomain
%num.div [pi, 2] ~> %num.tan ~ ~> :error<'%num.error>      //= OutOfDomain
%num.sin 1 ~> :error<'%num.error>                          //= Unrepresentable
%num.add [pi, %num.e] ~> :error<'%num.error>               //= Unrepresentable
%num.mul [2, %num.e] ~> %num.sqrt ~ ~> :error<'%num.error>  //= Unrepresentable
```

A nil operand is answered as itself, so the first failure's reason survives the rest of a
calculation:

```quiver
%num.div [1, 0] ~> %num.add [~, 1] ~> %num.sqrt ~ ~> :error<'%num.error>   //= DivisionByZero
```
