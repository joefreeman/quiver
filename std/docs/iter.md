# %iter

A lazy sequence. An iterator wraps a thunk: pull it and you get `[value, next]` — the element,
and the iterator that follows it — or nil when there is nothing left.

```quiver ignore
'thunk<'t> = #[] -> (['t, ^1] | [])
'<'t> = Iter['thunk<'t>]
```

The thunk type is private to the module. Iterators come from `unfold` or from a source module —
`%list.iter`, `%range.iter`, `%dict.iter`, `%str.split` — and are consumed by `fold`, `count`,
`%list.collect` and the rest, so nothing outside ever has to name it.

Nothing is computed until a consumer pulls, and a stage holds only the element in flight. So a
pipeline of several combinators builds no intermediate list between them, and a source may be
endless as long as something downstream stops.

```quiver
%list.iter %list{ 1, 2, 3, 4 }
~> %iter.filter [~, #{ %num.gt? [$, 1]; $ }]
~> %iter.map [~, #{ %num.mul [$, 10] }]
~> %list.collect ~            //= Cons[20, Cons[30, Cons[40, Nil]]]
```

## Sources

Most iterators come from a module that has elements to offer.

```quiver
%list.iter %list{ 1, 2, 3 } ~> %list.collect ~    //= Cons[1, Cons[2, Cons[3, Nil]]]
%range.to 3 ~> %range.iter ~ ~> %list.collect ~   //= Cons[0, Cons[1, Cons[2, Nil]]]
%str.split ["a,b,c", ","] ~> %list.collect ~      //= Cons["a", Cons["b", Cons["c", Nil]]]
```

`repeat` yields one value forever, and `cycle` repeats an iterator's elements forever. Both are
endless, so both need bounding.

```quiver
%iter.repeat 42 ~> %iter.take [~, 3] ~> %list.collect ~   //= Cons[42, Cons[42, Cons[42, Nil]]]
%list.iter %list{ 1, 2 } ~> %iter.cycle ~ ~> %iter.take [~, 5] ~> %list.collect ~
//= Cons[1, Cons[2, Cons[1, Cons[2, Cons[1, Nil]]]]]
```

`unfold` is the general constructor, and what every other source is built from: a seed state,
and a step function from the state to `[value, next_state]`.

```quiver
%iter.unfold [1, #{ [$, %num.mul [$, 2]] }] ~> %iter.take [~, 5] ~> %list.collect ~
//= Cons[1, Cons[2, Cons[4, Cons[8, Cons[16, Nil]]]]]
```

A step that answers nil ends the sequence, which is how a bounded source stops:

```quiver
%iter.unfold [1, #{ %num.lt? [$, 20]; [$, %num.mul [$, 2]] }] ~> %list.collect ~
//= Cons[1, Cons[2, Cons[4, Cons[8, Cons[16, Nil]]]]]
```

## Sinks

An iterator is not something to look at; something has to run it. `%list.collect` materialises
the elements, `fold` reduces them, and `count` counts them.

```quiver
xs = %list.iter %list{ 1, 2, 3 }
%list.collect xs              //= Cons[1, Cons[2, Cons[3, Nil]]]
%iter.fold [xs, 0, %num.add]  //= 6
%iter.count xs                //= 3
```

`fold` threads an accumulator through a function of `[accumulator, element]`, and neither the
accumulator nor the result need resemble the elements:

```quiver
%list.iter %list{ "a", "b", "c" } ~> %iter.fold [~, "", #{ %str.concat [$0, $1] }]   //= "abc"
```

`nth` indexes, zero-based, and answers nil past the end — a plain "found nothing". A negative
index is before the start, and finds nothing too.

```quiver
xs = %list.iter %list{ 1, 2, 3 }
%iter.nth [xs, 0]             //= 1
%iter.nth [xs, 1]             //= 2
%iter.nth [xs, 5]             //= []
%iter.nth [xs, -1]            //= []
```

An empty iterator folds to its initial accumulator and counts zero.

```quiver
empty = %list.iter %list{ }
%iter.count empty             //= 0
%iter.fold [empty, 7, %num.add]   //= 7
```

The thunks an iterator is made of are ordinary pure functions, so running one does not use it
up: the same iterator answers the same elements every time. Nothing is memoised either — the
work is simply done again.

```quiver
xs = %range.to 4 ~> %range.iter ~
%iter.count xs                //= 4
%iter.count xs                //= 4
```

## Transforming

`map` applies a function to each element as it passes.

```quiver
%list.iter %list{ 1, 2, 3 } ~> %iter.map [~, #{ %num.add [$, 1] }] ~> %list.collect ~
//= Cons[2, Cons[3, Cons[4, Nil]]]
```

A **predicate** — what `filter`, `find`, `find_index`, `any?`, `all?`, `take_while` and
`drop_while` take — is a function from an element to that element or to nil. That is why the
body below ends with `$`: the test gates the step, and the element itself is what flows out.

```quiver
even? = #'int { %int.mod [$, 2] ~> =0; $ }
%list.iter %list{ 1, 2, 3, 4 } ~> %iter.filter [~, even?] ~> %list.collect ~
//= Cons[2, Cons[4, Nil]]
```

`flat_map` maps each element to an iterator and runs them one after another.

```quiver
%list.iter %list{ 1, 2, 3 } ~> %iter.flat_map [~, #{ %list.iter %list{ $, $ } }] ~> %list.collect ~
//= Cons[1, Cons[1, Cons[2, Cons[2, Cons[3, Cons[3, Nil]]]]]]
```

## Slicing

`take` and `drop` cut at a count, and neither complains about one the source cannot honour — a
short iterator is taken whole, dropping past the end leaves nothing, and a count below zero
counts as zero.

```quiver
xs = %list.iter %list{ 1, 2, 3, 4 }
%iter.take [xs, 3] ~> %list.collect ~   //= Cons[1, Cons[2, Cons[3, Nil]]]
%iter.drop [xs, 2] ~> %list.collect ~   //= Cons[3, Cons[4, Nil]]
%iter.take [xs, 9] ~> %list.collect ~   //= Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]]
%iter.drop [xs, 9] ~> %list.collect ~   //= Nil
%iter.take [xs, -1] ~> %list.collect ~  //= Nil
%iter.drop [xs, -1] ~> %list.collect ~  //= Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]]
```

That holds on an endless source too, so a computed count that comes out negative still returns:

```quiver
%iter.repeat 1 ~> %iter.take [~, -3] ~> %list.collect ~   //= Nil
```

`take` is also the thing standing between an endless source and a program that never returns.

`take_while` and `drop_while` cut at the first element a predicate rejects. They are two halves
of one split, so what one keeps the other discards.

```quiver
xs = %list.iter %list{ 1, 2, 3, 4 }
before3? = #'int { | =3 => [] | $ }
%iter.take_while [xs, before3?] ~> %list.collect ~   //= Cons[1, Cons[2, Nil]]
%iter.drop_while [xs, before3?] ~> %list.collect ~   //= Cons[3, Cons[4, Nil]]
```

The cut is at the *first* rejection, not at every one: `drop_while` stops testing once it has
started yielding.

```quiver
%list.iter %list{ 1, 5, 2 } ~> %iter.drop_while [~, #{ %num.lt? [$, 3]; $ }] ~> %list.collect ~
//= Cons[5, Cons[2, Nil]]
```

## Combining

`chain` runs one iterator after another. `zip` pairs them off, and stops as soon as either runs
out.

```quiver
xs = %list.iter %list{ 1, 2, 3 }
ys = %list.iter %list{ 4, 5 }
%iter.chain [xs, ys] ~> %list.collect ~   //= Cons[1, Cons[2, Cons[3, Cons[4, Cons[5, Nil]]]]]
%iter.zip [xs, ys] ~> %list.collect ~     //= Cons[[1, 4], Cons[[2, 5], Nil]]
```

`enumerate` pairs each element with its zero-based index — a `zip` against a counting range,
done for you.

```quiver
%list.iter %list{ 2, 3, 4 } ~> %iter.enumerate ~ ~> %list.collect ~
//= Cons[[0, 2], Cons[[1, 3], Cons[[2, 4], Nil]]]
```

`intersperse` puts a separator between consecutive elements — between them only, so one element
is unchanged and none stays none.

```quiver
%list.iter %list{ 1, 2, 3 } ~> %iter.intersperse [~, 0] ~> %list.collect ~
//= Cons[1, Cons[0, Cons[2, Cons[0, Cons[3, Nil]]]]]
%list.iter %list{ 42 } ~> %iter.intersperse [~, 0] ~> %list.collect ~   //= Cons[42, Nil]
%list.iter %list{ } ~> %iter.intersperse [~, 0] ~> %list.collect ~      //= Nil
```

## Searching

`find` answers the first element a predicate accepts and `find_index` its position; `any?` and
`all?` answer only `Ok` or nil.

```quiver
xs = %list.iter %list{ 10, 20, 30 }
%iter.find [xs, #{ %num.gt? [$, 15]; $ }]   //= 20
%iter.find [xs, #{ %num.gt? [$, 99]; $ }]   //= []
```

```quiver
even? = #'int { %int.mod [$, 2] ~> =0; $ }
%list.iter %list{ 1, 2, 3 } ~> %iter.find_index [~, even?]   //= 1
%list.iter %list{ 1, 3 } ~> %iter.find_index [~, even?]      //= []
%list.iter %list{ 1, 2, 3 } ~> %iter.any? [~, even?]         //= Ok
%list.iter %list{ 1, 3 } ~> %iter.any? [~, even?]            //= []
%list.iter %list{ 2, 4, 6 } ~> %iter.all? [~, even?]         //= Ok
%list.iter %list{ 1, 3 } ~> %iter.all? [~, even?]            //= []
```

Over an empty iterator they take the usual vacuous readings: nothing satisfies the predicate,
and everything does.

```quiver
even? = #'int { %int.mod [$, 2] ~> =0; $ }
%list.iter %list{ } ~> %iter.any? [~, even?]   //= []
%list.iter %list{ } ~> %iter.all? [~, even?]   //= Ok
```

Each of them stops at the first decisive element — `find`, `find_index` and `any?` at the first
hit, `all?` at the first miss — so none pulls further than it must, and each can be run against
an endless source.

```quiver
%range.from 2 ~> %range.iter ~ ~> %iter.find [~, #{ %num.gt? [$, 5]; $ }]   //= 6
%iter.repeat 7 ~> %iter.any? [~, #{ =7; $ }]                                //= Ok
```

## Laziness

Only what a consumer demands is computed. Mapping over an endless range and taking three does
three multiplications, not infinitely many:

```quiver
%range.from 1 ~> %range.iter ~
~> %iter.map [~, #{ %num.mul [$, $] }]
~> %iter.take [~, 3]
~> %list.collect ~            //= Cons[1, Cons[4, Cons[9, Nil]]]
```

Because each stage holds only the element in flight, the depth of a pipeline costs nothing in
memory, and skipping costs nothing in stack: `filter` advancing past a long run of rejected
elements tail-calls its way forward rather than nesting.

```quiver
%range.to 2000 ~> %range.iter ~
~> %iter.filter [~, #{ =1999; $ }]
~> %iter.nth [~, 0]           //= 1999
```

Two thousand is small enough to prove nothing. Fifty thousand consecutive skips is past the
depth a nesting implementation could reach, so this answering at all is the claim:

```quiver
%range.to 50000 ~> %range.iter ~
~> %iter.filter [~, #'int { =49999 }]
~> %iter.nth [~, 0]           //= 49999
```

## Predicates and the element type

A predicate may declare a fallible result — `#'t -> ('t | [])` — and the `[]` it uses to reject
must not leak into the *element* type: `filter` yields the elements that passed, so a consumer
demanding the exact element type still fits. These consumers are written strictly, so they
compile only if that holds.

```quiver
pred = #'int -> ('int | []) { %num.gt? [$, 1]; $ }
strict = #'%iter<'int> { %iter.count ~ }
Cons[1, Cons[2, Nil]] ~> %list.iter ~ ~> %iter.filter [~, pred] ~> strict ~   //= 1
```

The same holds when the element is a tuple, where the rejected `[]` sits alongside a member it
could be confused with:

```quiver
'lst<'t> = Nil | Cons['t, ^]
keep? = #[Str['bin], 'int] -> ([Str['bin], 'int] | []) { $ }
from_it = #'%iter<[Str['bin], 'int]> {
  %iter.fold [~, Nil, #['lst<[Str['bin], 'int]>, [Str['bin], 'int]] { Cons[$1, $0] }]
}
Cons[["a", 1], Nil] ~> %list.iter ~ ~> %iter.filter [~, keep?] ~> from_it ~   //= Cons[["a", 1], Nil]
```

## Failure and exhaustion

A pull that ends the sequence means *exhausted* — but for a source backed by I/O it may instead
mean the walk **failed**, a nil carrying `:error`. `fold` cannot tell them apart, because
recovering to the accumulator is how it terminates at all, so a broken walk would read as a
completed one and answer a short result. `try_fold` checks before recovering and propagates the
failure nil instead, payload intact; `%list.try_collect` draws the same distinction for
collecting.

Over a pure source — `%list`, `%range`, `%dict` — no failure is possible, and each pair agrees.

```quiver
%list.iter %list{ 1, 2, 3 } ~> %iter.try_fold [~, 0, %num.add]   //= 6
%list.iter %list{ 1, 2, 3 } ~> %list.try_collect ~               //= Cons[1, Cons[2, Cons[3, Nil]]]
```

That is why they are siblings rather than a change to `fold`: a pure fold's result stays exactly
the accumulator's type, with no nil for the caller to narrow away.

## Eager counterparts

Most of these combinators have an eager twin in `%list` that answers a list. Each is literally
this one with `%list.iter` in front and `%list.collect` behind, so several stages are better
written once through `%iter` — the eager spelling materialises a list per stage.

```quiver
xs = %list{ 1, 2, 3, 4 }
%list.map [xs, #{ %num.mul [$, 10] }]   //= Cons[10, Cons[20, Cons[30, Cons[40, Nil]]]]
%list.iter xs ~> %iter.map [~, #{ %num.mul [$, 10] }] ~> %list.collect ~
//= Cons[10, Cons[20, Cons[30, Cons[40, Nil]]]]
```

Collecting is not always the end of the road: `%str.join` consumes an iterator directly, so a
rendering pipeline needs no list at all.

```quiver
%range.to 4 ~> %range.iter ~
~> %iter.map [~, #{ %str.from_int $ }]
~> %str.join [~, ", "]        //= "0, 1, 2, 3"
```
