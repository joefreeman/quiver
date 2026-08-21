# %range

An integer arithmetic sequence, described by three numbers: where it starts, where it stops —
exclusive — and how far each step moves. `None` in the stop position means there is no upper
bound. That is the whole type, and it is an ordinary tuple:

```quiver ignore
' = Range['int, ('int | None), 'int]
```

A range is a *description*, not a sequence. Nothing is walked until `iter` turns it into a lazy
`%iter` iterator, and from there every combinator in that module applies.

```quiver
%range.to 5 ~> %range.iter ~ ~> %list.collect ~
//= Cons[0, Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]]]
```

## Describing a range

Four constructors, differing only in what they leave implicit.

```quiver
%range.to 5                   //= Range[0, 5, 1]       0 up to 5
%range.between [3, 8]         //= Range[3, 8, 1]
%range.from 10                //= Range[10, None, 1]   no upper bound
%range.new [0, 10, 2]         //= Range[0, 10, 2]
```

`between` and `new` label their fields, and every label is optional, so a call may name them or
give them positionally.

```quiver
%range.between [from: 3, to: 8]         //= Range[3, 8, 1]
%range.new [from: 0, to: 10, step: 2]   //= Range[0, 10, 2]
```

Since the value is nothing but that tuple, writing one out by hand is the same thing, and one
can be taken apart by an ordinary match.

```quiver
Range[0, 3, 1] ~> %range.iter ~ ~> %list.collect ~   //= Cons[0, Cons[1, Cons[2, Nil]]]
%range.new [2, 9, 3] ~> =Range[from, to, step]; [from, to, step]   //= [2, 9, 3]
```

## Walking one

`iter` yields the start, then each stepped value, stopping *before* the stop.

```quiver
%range.between [3, 8] ~> %range.iter ~ ~> %list.collect ~
//= Cons[3, Cons[4, Cons[5, Cons[6, Cons[7, Nil]]]]]
```

The stop is a bound rather than a landmark: a step that would overshoot ends the walk, so the
stop need never be reached exactly.

```quiver
%range.new [0, 10, 2] ~> %range.iter ~ ~> %list.collect ~
//= Cons[0, Cons[2, Cons[4, Cons[6, Cons[8, Nil]]]]]
%range.new [0, 9, 2] ~> %range.iter ~ ~> %list.collect ~
//= Cons[0, Cons[2, Cons[4, Cons[6, Cons[8, Nil]]]]]
```

A negative step counts down, and the stop is exclusive there too — below the start rather than
above it.

```quiver
%range.new [10, 5, -1] ~> %range.iter ~ ~> %list.collect ~
//= Cons[10, Cons[9, Cons[8, Cons[7, Cons[6, Nil]]]]]
%range.new [10, 0, -3] ~> %range.iter ~ ~> %list.collect ~
//= Cons[10, Cons[7, Cons[4, Cons[1, Nil]]]]
```

## Empty ranges

A range whose start has already passed its stop in the step's direction yields nothing. That is
an ordinary empty walk rather than a failure, so it collects to `Nil` like any other.

```quiver
%range.new [5, 5, 1] ~> %range.iter ~ ~> %list.collect ~     //= Nil
%range.to 0 ~> %range.iter ~ ~> %list.collect ~              //= Nil
%range.between [8, 3] ~> %range.iter ~ ~> %list.collect ~    //= Nil   ascending, start past stop
%range.new [5, 10, -1] ~> %range.iter ~ ~> %list.collect ~   //= Nil   descending, likewise
```

## Unbounded ranges

`None` as the stop means the walk never ends; `from` is the shorthand for that with a step of 1.
Such an iterator has to be bounded downstream — by `take`, `take_while`, `find`, or anything
else that stops early — since collecting it whole would not return.

```quiver
%range.from 10 ~> %range.iter ~ ~> %iter.take [~, 5] ~> %list.collect ~
//= Cons[10, Cons[11, Cons[12, Cons[13, Cons[14, Nil]]]]]
%range.new [0, None, 3] ~> %range.iter ~ ~> %iter.take [~, 4] ~> %list.collect ~
//= Cons[0, Cons[3, Cons[6, Cons[9, Nil]]]]
%range.new [0, None, -2] ~> %range.iter ~ ~> %iter.take [~, 3] ~> %list.collect ~
//= Cons[0, Cons[-2, Cons[-4, Nil]]]
```

A zero step never advances, so the start is never passed and the stop can never be crossed: the
range repeats its start forever, whether or not one was given.

```quiver
%range.new [7, 10, 0] ~> %range.iter ~ ~> %iter.take [~, 3] ~> %list.collect ~
//= Cons[7, Cons[7, Cons[7, Nil]]]
```

## In a pipeline

A range is the usual source for `%iter`, and stands where another language would write a
counted loop.

```quiver
%range.to 5 ~> %range.iter ~
~> %iter.map [~, #{ %num.mul [$, $] }]
~> %list.collect ~            //= Cons[0, Cons[1, Cons[4, Cons[9, Cons[16, Nil]]]]]
```

```quiver
%range.between [1, 101] ~> %range.iter ~ ~> %iter.fold [~, 0, %num.add]   //= 5050
```

Nothing but the elements demanded is ever computed, so an unbounded range in front of a
short-circuiting consumer costs only the steps it takes to answer:

```quiver
%range.from 2 ~> %range.iter ~ ~> %iter.find [~, #{ %num.gt? [$, 5]; $ }]   //= 6
```

Zipped against another iterator, an unbounded range is how an index gets attached to something
that has none — which is what `%iter.enumerate` does internally.

```quiver
letters = %list.iter %list{ "a", "b", "c" }
%range.from 1 ~> %range.iter ~ ~> %iter.zip [~, letters] ~> %list.collect ~
//= Cons[[1, "a"], Cons[[2, "b"], Cons[[3, "c"], Nil]]]
```
