# %dict

A persistent hash map from **data values** to values of any type. It is an immutable,
structurally-shared 32-way hash-array-mapped trie: `put` and `remove` answer a new dict and
share everything the change did not touch, so the dict a caller holds never moves under it.

```quiver
%dict{ "a" => 1, "b" => 2 } ~> %dict.get [~, "b"]   //= 2
```

A missing key answers nil, and the sequence ends as it would for any other failure — which
also means nil is not a useful value to store, since `get` could not tell it from absence.

## Keys

A key is any **data value**: an integer, a binary, or a tuple of them, nested freely.
Strings are tuples (`Str[<…>]`), so they are keys like any other.

```quiver
%dict.new [] ~> %dict.put [~, 42, 7] ~> %dict.get [~, 42]           //= 7
%dict.new [] ~> %dict.put [~, <01>, 7] ~> %dict.get [~, <01>]       //= 7
%dict.new [] ~> %dict.put [~, "a", 7] ~> %dict.get [~, "a"]         //= 7
```

Tuple keys compare structurally — by contents, not by identity — so a key can be rebuilt at
the lookup site, and a tuple differing anywhere is a different key.

```quiver
d = %dict.new [] ~> %dict.put [~, Point[x: 1, y: 2], 9]
%dict.get [d, Point[x: 1, y: 2]]   //= 9
%dict.get [d, Point[x: 1, y: 3]]   //= []
```

```quiver
d = %dict.new [] ~> %dict.put [~, A[B[1, "x"], <0a>], 5]
%dict.get [d, A[B[1, "x"], <0a>]]   //= 5
```

Keys of different kinds never coincide, even where they share bytes or digits: the integer
`1`, the string `"1"` and the binary `<31>` are three distinct keys in one dict, and the
string `"A"` — which is `Str[<41>]` — is not the binary `<41>`.

```quiver
d = %dict.new [] ~> %dict.put [~, 1, 10] ~> %dict.put [~, "1", 20] ~> %dict.put [~, <31>, 30]
%dict.count d           //= 3
%dict.get [d, 1]        //= 10
```

```quiver
d = %dict.new [] ~> %dict.put [~, "A", 1] ~> %dict.put [~, <41>, 2]
%dict.get [d, "A"]      //= 1
%dict.get [d, <41>]     //= 2
```

Negation is part of the value, not a sign on a shared magnitude:

```quiver
%dict.new [] ~> %dict.put [~, -3, 1] ~> %dict.get [~, 3]   //= []
```

A key is hashed by encoding it as `%data` notation and hashing that text, so anything
`%data` cannot encode — a function, a process, a ref, a resource — is
a runtime error when used as a key, not a silently wrong lookup.

```quiver
%dict.new [] ~> %dict.put [~, __integer_add__, 1]   //! cannot encode a builtin
```

## Building a dict

`new` makes an empty dict and `put` adds one entry at a time.

```quiver
%dict.new [] ~> %dict.get [~, "a"]                                  //= []
%dict.new [] ~> %dict.put [~, "a", 1] ~> %dict.get [~, "a"]         //= 1
```

`from` builds one from a list of `[key, value]` pairs, and `%dict{ … }` is the dialect that
writes the same thing directly. Both take later entries as winning over earlier ones.

```quiver
%list{ ["a", 1], ["b", 2] } ~> %dict.from ~ ~> %dict.get [~, "b"]   //= 2
%dict{ "a" => 1, "b" => 2 } ~> %dict.get [~, "b"]                   //= 2
%dict{} ~> %dict.count ~                                            //= 0
```

```quiver
%list{ ["k", 1], ["k", 2] } ~> %dict.from ~ ~> %dict.get [~, "k"]   //= 2
%dict{ "k" => 1, "k" => 2 } ~> %dict.get [~, "k"]                   //= 2
```

In the dialect, a key and a value are each an ordinary Quiver expression evaluated in the
caller's scope, so bindings and computed keys work as they would anywhere else.

```quiver
k = "computed"
%dict{ k => %num.add [1, 2], Point[1, 2] => "tuple key" }
~> %dict.get [~, "computed"]                                        //= 3
```

## Lookup

`get` answers the stored value or nil. `has?` answers the verdict alone, which is what to
use when the value might itself be falsey.

```quiver
d = %dict{ "a" => 1 }
%dict.get [d, "a"]      //= 1
%dict.get [d, "z"]      //= []
%dict.has? [d, "a"]     //= Ok
%dict.has? [d, "q"]     //= []
```

## Adding and removing

`put` replaces an existing mapping rather than adding a second one, so the count is
unchanged when the key was already present.

```quiver
d = %dict{ "a" => 1 }
%dict.put [d, "a", 99] ~> %dict.get [~, "a"]   //= 99
%dict.put [d, "a", 99] ~> %dict.count ~        //= 1
```

`remove` drops a key, leaves the others alone, and is a no-op for a key that is not there.

```quiver
d = %dict{ "a" => 1, "b" => 2 }
%dict.remove [d, "b"] ~> %dict.get [~, "b"]    //= []
%dict.remove [d, "b"] ~> %dict.get [~, "a"]    //= 1
%dict.remove [d, "z"] ~> %dict.count ~         //= 2
```

Every operation answers a *new* dict; the one it was derived from is untouched, so a
discarded update leaves no trace.

```quiver
d = %dict.new [] ~> %dict.put [~, "a", 1]
%dict.put [d, "a", 99]
%dict.get [d, "a"]      //= 1
```

Each step of a chain — or of a REPL session — carries the whole dict forward, so a run of
updates reads as a pipeline:

```quiver
%dict.new []
%dict.put [~, "a", 1]
%dict.put [~, "b", 2]
%dict.put [~, "c", 3]
%dict.remove [~, "a"]
%dict.get [~, "c"]      //= 3
```

## Merging

`merge` adds every entry of the second dict to the first. On a key both hold, the second
wins.

```quiver
a = %dict{ "k" => 1, "x" => 1 }
b = %dict{ "k" => 9, "y" => 2 }
%dict.merge [a, b] ~> %dict.get [~, "k"]     //= 9
%dict.merge [a, b] ~> %dict.get [~, "x"]     //= 1
%dict.merge [a, b] ~> %dict.get [~, "y"]     //= 2
%dict.merge [a, b] ~> %dict.count ~          //= 3
```

## Entries, keys and values

`entries`, `keys` and `values` answer lists, and `count` the number of entries. The order is
**unspecified** — it follows the trie's shape, which follows the hashes — so a program that
needs an order must impose one.

```quiver
d = %dict{ "a" => 1, "b" => 2, "c" => 3 }
%dict.count d                                //= 3
%dict.entries d ~> %list.count ~             //= 3
%dict.keys d ~> %list.count ~                //= 3
%dict.values d ~> %list.fold [~, 0, %num.add]   //= 6
```

## Iterating and collecting

`iter` is the lazy view of the same entries and `collect` its inverse, so the two round-trip
through any `%iter` pipeline.

```quiver
d = %dict{ "a" => 1, "b" => 2, "c" => 3 }
%dict.iter d ~> %iter.count ~                                        //= 3
%dict.iter d ~> %iter.map [~, #{ $1 }] ~> %iter.fold [~, 0, %num.add]   //= 6
%dict.iter d ~> %dict.collect ~ ~> %dict.get [~, "b"]                //= 2
%dict.iter d ~> %dict.collect ~ ~> %dict.count ~                     //= 3
```

## Sequence operations

The eager combinators take the dict and answer a dict (or a verdict), each entry arriving as
a `[key, value]` pair. They are `iter`/`collect` composed with the corresponding `%iter`
operation, so their order is the iterator's — unspecified.

`map` transforms whole entries, keys included, which is why duplicate result keys collapse.

```quiver
d = %dict{ "a" => 1, "b" => 2, "c" => 3 }
%dict.map [d, #{ [$0, %num.mul [$1, 10]] }] ~> %dict.get [~, "b"]   //= 20
%dict.map [d, #{ [$0, %num.mul [$1, 10]] }] ~> %dict.count ~        //= 3
```

`filter` keeps the entries its predicate answers non-nil for — the idiom is to test, then
hand back `$`.

```quiver
d = %dict{ "a" => 1, "b" => 2, "c" => 3 }
%dict.filter [d, #{ %num.le? [$1, 2]; $ }] ~> %dict.count ~         //= 2
%dict.filter [d, #{ %num.le? [$1, 2]; $ }] ~> %dict.get [~, "a"]    //= 1
%dict.filter [d, #{ %num.le? [$1, 2]; $ }] ~> %dict.get [~, "c"]    //= []
```

`fold` reduces to a single value, the accumulator first and the entry second.

```quiver
%dict{ "a" => 1, "b" => 2, "c" => 3 } ~> %dict.fold [~, 0, #{ %num.add [$0, $1.1] }]   //= 6
```

`find` answers a matching entry — the whole pair — or nil; `any?` and `all?` answer verdicts.

```quiver
d = %dict{ "a" => 1, "b" => 2, "c" => 3 }
%dict.find [d, #{ $1 ~> =2; $ }]              //= ["b", 2]
%dict.find [d, #{ $1 ~> =99; $ }]             //= []
%dict.any? [d, #{ $1 ~> =3; $ }]              //= Ok
%dict.any? [d, #{ $1 ~> =99; $ }]             //= []
%dict.all? [d, #{ %num.gt? [$1, 0]; $ }]      //= Ok
%dict.all? [d, #{ %num.gt? [$1, 1]; $ }]      //= []
```

## Deeper tries

A dict of a few entries fits in one level. Past that the trie grows: a 5-bit slice of each
key's hash picks a slot per level, and a slot holding two keys becomes a node of its own.
None of that is visible from outside — the operations behave identically at any depth, and a
`put` or `remove` still rebuilds only the path it touched.

```quiver
d = %dict{
  "alpha" => 1,   "bravo" => 2,  "charlie" => 3, "delta" => 4,
  "echo" => 5,    "foxtrot" => 6, "golf" => 7,   "hotel" => 8,
  "india" => 9,   "juliet" => 10, "kilo" => 11,  "lima" => 12,
}
%dict.count d                    //= 12
%dict.get [d, "alpha"]           //= 1
%dict.get [d, "golf"]            //= 7
%dict.get [d, "lima"]            //= 12
%dict.has? [d, "delta"]          //= Ok
%dict.has? [d, "kilo"]           //= Ok
%dict.has? [d, "mike"]           //= []
```

```quiver
d = %dict{
  "alpha" => 1,   "bravo" => 2,  "charlie" => 3, "delta" => 4,
  "echo" => 5,    "foxtrot" => 6, "golf" => 7,   "hotel" => 8,
  "india" => 9,   "juliet" => 10, "kilo" => 11,  "lima" => 12,
}
%dict.remove [d, "echo"] ~> %dict.count ~             //= 11
%dict.remove [d, "echo"] ~> %dict.get [~, "echo"]     //= []
%dict.remove [d, "echo"] ~> %dict.get [~, "india"]    //= 9
%dict.iter d ~> %iter.count ~                         //= 12
%dict.iter d ~> %iter.map [~, #{ $1 }] ~> %iter.fold [~, 0, %num.add]   //= 78
%dict.iter d ~> %dict.collect ~ ~> %dict.count ~      //= 12
%dict.iter d ~> %dict.collect ~ ~> %dict.get [~, "echo"]   //= 5
```

## Canonical form

A dict's structure depends only on its contents. Removal collapses what insertion would
never have built — an emptied node back to nothing, a lone surviving entry hoisted up to
where its node was — so two dicts holding the same entries are the *same value*, whatever
sequence of operations produced them. That is what makes an ordinary pin (`=&`) a working
equality test.

Insertion order does not matter:

```quiver
a = %dict.new [] ~> %dict.put [~, "alpha", 1] ~> %dict.put [~, "bravo", 2] ~> %dict.put [~, "charlie", 3]
b = %dict.new [] ~> %dict.put [~, "charlie", 3] ~> %dict.put [~, "bravo", 2] ~> %dict.put [~, "alpha", 1]
a ~> =&b     //= Ok
```

Nor does a detour through an entry that was later removed:

```quiver
a = %dict{ "alpha" => 1, "bravo" => 2, "charlie" => 3, "delta" => 4 } ~> %dict.remove [~, "charlie"]
b = %dict{ "alpha" => 1, "bravo" => 2, "delta" => 4 }
a ~> =&b     //= Ok
```

The trie is an ordinary value, so its nodes can be matched directly. A one-entry dict is a
bare `Leaf[hash, key, value]`, and removing one of two entries under a node yields exactly
that shape rather than a node with a single child.

```quiver
%dict{ "a" => 1, "b" => 2 } ~> %dict.remove [~, "b"]   //= Leaf[1637994474, "a", 1]
```

## Hash collisions

Keys are placed by a 32-bit hash of their `%data` text. Two distinct keys can hash alike, and
those land together in a `Collision` node holding a bucket of entries, which is searched by
structural key equality. The pair below was found by search — it is the shape of a bucket,
not something a program would write.

```quiver
d = %dict{ <86f15dabd8> => 1, <86f166b4b6> => 2 }
d   //= Collision[1102851308, Cons[[<86f15dabd8>, 1], Cons[[<86f166b4b6>, 2], Nil]]]
```

A bucket behaves like the rest of the dict. Both keys are retrievable, they count as two,
and overwriting one leaves the other alone:

```quiver
d = %dict{ <86f15dabd8> => 1, <86f166b4b6> => 2 }
%dict.get [d, <86f15dabd8>]                              //= 1
%dict.get [d, <86f166b4b6>]                              //= 2
%dict.count d                                            //= 2
%dict.put [d, <86f15dabd8>, 9] ~> %dict.get [~, <86f15dabd8>]   //= 9
%dict.put [d, <86f15dabd8>, 9] ~> %dict.get [~, <86f166b4b6>]   //= 2
%dict.put [d, <86f15dabd8>, 9] ~> %dict.count ~          //= 2
```

Removing from a bucket is the same story, and removing a key that is not in it changes
nothing:

```quiver
d = %dict{ <86f15dabd8> => 1, <86f166b4b6> => 2 }
%dict.remove [d, <86f15dabd8>] ~> %dict.get [~, <86f166b4b6>]   //= 2
%dict.remove [d, <86f15dabd8>] ~> %dict.get [~, <86f15dabd8>]   //= []
%dict.remove [d, <86f15dabd8>] ~> %dict.count ~                 //= 1
%dict.remove [d, "zz"] ~> %dict.count ~                         //= 2
```

Collapse applies here too: a bucket down to one entry becomes a `Leaf`, and an emptied one
becomes an empty dict, which is reusable like any other.

```quiver
d = %dict{ <86f15dabd8> => 1, <86f166b4b6> => 2 }
%dict.remove [d, <86f15dabd8>]   //= Leaf[1102851308, <86f166b4b6>, 2]
```

```quiver
d = %dict{ <86f15dabd8> => 1, <86f166b4b6> => 2 }
e = %dict.remove [d, <86f15dabd8>] ~> %dict.remove [~, <86f166b4b6>]
%dict.count e                                   //= 0
%dict.put [e, "c", 5] ~> %dict.get [~, "c"]     //= 5
```

Because collapse is canonical, a bucket that loses an entry is indistinguishable from a dict
that never held it:

```quiver
a = %dict{ <86f15dabd8> => 1, <86f166b4b6> => 2 } ~> %dict.remove [~, <86f15dabd8>]
b = %dict{ <86f166b4b6> => 2 }
a ~> =&b     //= Ok
```
