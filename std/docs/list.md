# %list

A singly-linked list: `Nil` for the empty one, `Cons[head, tail]` for everything else.

```quiver ignore
'<'t> = Nil | Cons['t, ^]
```

That is the whole representation — an ordinary recursive union of ordinary tuples, so a list
is written, matched and rendered like any other value, and the module is a library over it
rather than an abstraction hiding it.

```quiver
%list{ 1, 2, 3 }              //= Cons[1, Cons[2, Cons[3, Nil]]]
```

Lists are immutable, so a tail is shared rather than copied: `prepend` is a single new cell in
front of a list that is otherwise untouched. Working at the *front* is what the shape is for;
anything that has to reach the end — `append`, `count`, `nth` — walks the whole chain.

The sequence operations here are eager: each answers a list. Their lazy counterparts are in
`%iter`, and each of these is in fact that one with `%list.iter` in front and `%list.collect`
behind — so a pipeline of several is better written once through `%iter`, building no
intermediate list per stage.

## Building a list

`new` answers the empty list, and `prepend` and `append` add one element.

```quiver
%list.new []                            //= Nil
%list.prepend [%list.new [], 10]        //= Cons[10, Nil]
%list.prepend [%list{ 2, 3 }, 1]        //= Cons[1, Cons[2, Cons[3, Nil]]]
%list.append [%list{ 1, 2 }, 3]         //= Cons[1, Cons[2, Cons[3, Nil]]]
```

Their fields are labelled `list` and `item`, and both labels are optional — so the positional
spelling above and the labelled one below are the same call.

```quiver
%list.prepend [list: %list{ 2 }, item: 1]   //= Cons[1, Cons[2, Nil]]
```

Writing the cells out by hand is always available, and for anything longer the `%list{ … }`
dialect does it: the elements are comma-separated and the braces close the chain with `Nil`.

```quiver
Cons[1, Cons[2, Nil]]         //= Cons[1, Cons[2, Nil]]
%list{ 1, 2 }                 //= Cons[1, Cons[2, Nil]]
%list{ }                      //= Nil
```

An element is an ordinary Quiver expression evaluated in the surrounding scope, not a literal —
a variable, a call, a tuple, the flowing value:

```quiver
n = 5
%list{ n, %num.add [n, 1] }   //= Cons[5, Cons[6, Nil]]
%list{ [1, 2], "three" }      //= Cons[[1, 2], Cons["three", Nil]]
7 ~> %list{ ~, ~ }            //= Cons[7, Cons[7, Nil]]
```

## Taking one apart

`head` and `tail` read the first cell; `empty?` and `count` describe the whole list.

```quiver
xs = %list{ 1, 2, 3 }
%list.head xs                 //= 1
%list.tail xs                 //= Cons[2, Cons[3, _]]
%list.count xs                //= 3
%list.empty? xs               //= []
%list.empty? Nil              //= Ok
%list.count Nil               //= 0
```

An empty list has neither a head nor a tail, so both answer nil — which ends the sequence, as
a failed step does anywhere.

```quiver
%list.head Nil                //= []
%list.tail Nil                //= []
```

That nil is part of `tail`'s type, so a bare binding carries it and the result cannot be
passed straight back to a list operation. Ascribing the list type is the idiom: it narrows the
nil away, and a genuinely empty list ends the sequence at that step instead.

```quiver
%list.tail %list{ 1, 2, 3 } ~> =('%list<'int> & rest)
%list.head rest               //= 2
%list.count rest              //= 2
```

Matching the cells directly is often simpler than either, and needs nothing from the module:

```quiver
%list{ 1, 2, 3 } ~> =Cons[first, Cons[second, _]]; [first, second]   //= [1, 2]
```

## Reversing

```quiver
%list.reverse %list{ 1, 2, 3 }   //= Cons[3, Cons[2, Cons[1, Nil]]]
%list.reverse %list{ 10 }        //= Cons[10, Nil]
%list.reverse Nil                //= Nil
```

## Transforming

`map` applies a function to every element; the result's element type is the function's.

```quiver
%list.map [%list{ 1, 2, 3 }, #{ %num.mul [$, 10] }]   //= Cons[10, Cons[20, Cons[30, Nil]]]
```

A **predicate** — what `filter`, `find`, `any?` and the rest take — is a function from an
element to that element or to nil. That is why the body below ends with `$`: `%num.gt?`
answers `Ok`, which gates the step, and the element itself is what flows out.

```quiver
%list.filter [%list{ 1, 2, 3 }, #{ %num.gt? [$, 1]; $ }]   //= Cons[2, Cons[3, Nil]]
```

`flat_map` maps each element to a *list* and concatenates the results:

```quiver
%list.flat_map [%list{ 1, 2 }, #{ %list{ $, $ } }]   //= Cons[1, Cons[1, Cons[2, Cons[2, Nil]]]]
```

## Slicing

`take` and `drop` cut at a count, and neither complains about one the list cannot honour — a
short list is taken whole, and dropping past the end leaves nothing.

```quiver
xs = %list{ 1, 2, 3 }
%list.take [xs, 2]            //= Cons[1, Cons[2, Nil]]
%list.take [xs, 5]            //= Cons[1, Cons[2, Cons[3, Nil]]]
%list.take [xs, 0]            //= Nil
%list.drop [xs, 1]            //= Cons[2, Cons[3, Nil]]
%list.drop [xs, 5]            //= Nil
```

`take_while` and `drop_while` cut at the first element a predicate rejects. They are two
halves of one split, so what one keeps the other discards.

```quiver
xs = %list{ 1, 2, 3 }
%list.take_while [xs, #{ %num.lt? [$, 3]; $ }]   //= Cons[1, Cons[2, Nil]]
%list.drop_while [xs, #{ %num.lt? [$, 3]; $ }]   //= Cons[3, Nil]
```

The cut is at the *first* rejection, not at every one: `drop_while` stops testing once it has
started yielding.

```quiver
%list.drop_while [%list{ 1, 5, 2 }, #{ %num.lt? [$, 3]; $ }]   //= Cons[5, Cons[2, Nil]]
```

## Combining

`chain` concatenates two lists, `zip` pairs them off, and `zip` stops as soon as either runs
out.

```quiver
%list.chain [%list{ 1, 2 }, %list{ 3 }]             //= Cons[1, Cons[2, Cons[3, Nil]]]
%list.zip [%list{ 1, 2 }, %list{ 10, 20, 30 }]      //= Cons[[1, 10], Cons[[2, 20], Nil]]
```

`enumerate` pairs each element with its zero-based index:

```quiver
%list.enumerate %list{ 10, 20 }   //= Cons[[0, 10], Cons[[1, 20], Nil]]
```

`intersperse` puts a separator between consecutive elements — between them only, so a list of
one is unchanged and an empty list stays empty.

```quiver
%list.intersperse [%list{ 1, 2, 3 }, 0]   //= Cons[1, Cons[0, Cons[2, Cons[0, Cons[3, Nil]]]]]
%list.intersperse [%list{ 42 }, 0]        //= Cons[42, Nil]
%list.intersperse [Nil, 0]                //= Nil
```

## Reducing

`fold` walks the list, threading an accumulator through a function of `[accumulator, element]`.

```quiver
%list.fold [%list{ 1, 2, 3 }, 0, #{ %num.add [$0, $1] }]   //= 6
%list.fold [%list{ 1, 2, 3 }, init: 0, f: %num.add]        //= 6
```

The accumulator need not be a number, nor the same kind of thing as the elements:

```quiver
%list.fold [%list{ "a", "b", "c" }, "", #{ %str.concat [$0, $1] }]   //= "abc"
```

A list makes a fine accumulator too, and folding with `Cons` is exactly a reverse — which is
how the operations in this module are built, each being a fold with a step in front of it.

```quiver
built = %list.fold [%list{ 1, 2, 3 }, Nil, #{ Cons[$1, $0] }]
%list.head built              //= 3
%list.count built             //= 3
```

`count` and `nth` index into the list. `nth` is zero-based, and answers nil when the list is
too short — which is a plain "found nothing", the same nil `find` gives.

```quiver
xs = %list{ 1, 2, 3 }
%list.count xs                //= 3
%list.nth [xs, 0]             //= 1
%list.nth [xs, 1]             //= 2
%list.nth [xs, 5]             //= []
```

## Searching

`find` answers the first element a predicate accepts and `find_index` its position; `any?` and
`all?` answer only `Ok` or nil.

```quiver
xs = %list{ 1, 2, 3 }
%list.find [xs, #{ %num.gt? [$, 1]; $ }]         //= 2
%list.find [xs, #{ %num.gt? [$, 9]; $ }]         //= []
%list.find_index [xs, #{ %num.gt? [$, 1]; $ }]   //= 1
%list.find_index [xs, #{ %num.gt? [$, 9]; $ }]   //= []
```

```quiver
xs = %list{ 1, 2, 3 }
%list.any? [xs, #{ %num.gt? [$, 2]; $ }]   //= Ok
%list.any? [xs, #{ %num.gt? [$, 9]; $ }]   //= []
%list.all? [xs, #{ %num.gt? [$, 0]; $ }]   //= Ok
%list.all? [xs, #{ %num.gt? [$, 1]; $ }]   //= []
```

Over an empty list they take the usual vacuous readings: nothing satisfies the predicate, and
everything does. The predicate needs its parameter type written out here, because a bare `Nil`
says nothing about what the elements would have been for `#{ … }` to infer from.

```quiver
%list.any? [Nil, #'int { %num.gt? [$, 0]; $ }]   //= []
%list.all? [Nil, #'int { %num.gt? [$, 0]; $ }]   //= Ok
%list.find [Nil, #'int { %num.gt? [$, 0]; $ }]   //= []
```

`find` and `any?` stop at the first hit and `all?` at the first miss, so none of them walks
further than it must.

## Crossing to iterators

`iter` and `collect` are the bridge to `%iter`, and are inverse over a finite list.

```quiver
%list.iter %list{ 1, 2, 3 } ~> %list.collect ~   //= Cons[1, Cons[2, Cons[3, Nil]]]
```

Going through them once is how to compose several stages without materialising a list between
each:

```quiver
%list.iter %list{ 1, 2, 3, 4 }
~> %iter.filter [~, #{ %num.gt? [$, 1]; $ }]
~> %iter.map [~, #{ %num.mul [$, 10] }]
~> %list.collect ~            //= Cons[20, Cons[30, Cons[40, Nil]]]
```

It is also what makes an infinite iterator usable: bound it first, then collect.

```quiver
%iter.repeat 7 ~> %iter.take [~, 3] ~> %list.collect ~   //= Cons[7, Cons[7, Cons[7, Nil]]]
```

`collect` reads exhaustion as the end of the list, which is right for every pure source but
wrong for one backed by I/O, where the walk may instead have *failed* — a broken walk would
quietly answer a short list. `try_collect` checks first, and answers nil carrying the
failure's `:error` instead. Over a pure source the two agree.

```quiver
%list.iter %list{ 1, 2, 3 } ~> %list.try_collect ~   //= Cons[1, Cons[2, Cons[3, Nil]]]
```
