# %file

An open file, held by a process of its own. `open` spawns a process that owns the descriptor,
and the value it answers is a handle on that process: every read and write is a request to
it. The handle is ordinary data, so it can be kept, passed around and sent to other
processes, which all share the one open file.

```quiver ignore
' = File[@'command]
```

For a whole file read or written in one go, `%fs.read` and `%fs.write` are simpler: they need
no process, and open and close the file around the call. Each chapter below works in a
directory from `%fs.temp`.

## Opening

`open` takes a path and a `mode`:

| mode | reads | writes | if missing | if present |
| --- | --- | --- | --- | --- |
| `R` (default) | yes | | fails | |
| `W` | | yes | created | truncated |
| `A` | | at the end | created | kept |
| `RW` | yes | yes | created | kept |
| `New` | | yes | created | fails |

```quiver
d = %fs.temp [] ~> =\[]
p = %path.join [d, %path.parse "a.txt"] ~> =\[]
f = %file.open [p, mode: W] ~> =\[]
%file.write [f, 0, "hello" ~> .0]                //= 5
%file.close f                                    //= Ok
```

A failed open answers nil carrying `:error`:

```quiver
{ %file.open [%path.join [d, %path.parse "nope"] ~> =\[]] } ~> :error<'%io> ~> =IoError(kind: ~)
//= NotFound
{ %file.open [p, mode: New] } ~> :error<'%io> ~> =IoError(kind: ~)
//= AlreadyExists
%fs.remove [d, recursive?: Ok]                   //= Ok
```

## Reading

`read` takes an offset and a length, and answers up to that many bytes from there — fewer at
the end of the file, and none past it. `read_all` answers everything from the start.

```quiver
d = %fs.temp [] ~> =\[]
p = %path.join [d, %path.parse "a.txt"] ~> =\[]
%fs.write [p, "hello world" ~> .0]               //= Ok
f = %file.open [p] ~> =\[]
%file.read [f, 0, 5] ~> Str[~]                   //= "hello"
%file.read [f, 6, 100] ~> Str[~]                 //= "world"
%file.read [f, 11, 100]                          //= <>
%file.read_all f ~> Str[~]                       //= "hello world"
%file.close f                                    //= Ok
```

`lines` iterates a file's lines lazily, without their newlines. A final line without one is
still a line, and a trailing newline does not add an empty one.

```quiver
%fs.write [p, "one\ntwo\nthree" ~> .0]           //= Ok
f = %file.open [p] ~> =\[]
%file.lines f ~> %list.collect ~                 //= Cons["one", Cons["two", Cons["three", Nil]]]
%file.close f                                    //= Ok
%fs.remove [d, recursive?: Ok]                   //= Ok
```

## Writing

`write` writes bytes at an offset and answers how many it wrote. `flush` waits until what has
been written is durably on disk.

```quiver
d = %fs.temp [] ~> =\[]
p = %path.join [d, %path.parse "a.txt"] ~> =\[]
f = %file.open [p, mode: W] ~> =\[]
%file.write [f, 0, "hello world" ~> .0]          //= 11
%file.write [f, 6, "there" ~> .0]                //= 5
%file.flush f                                    //= Ok
%file.close f                                    //= Ok
%fs.read [p] ~> Str[~]                           //= "hello there"
```

In `A` mode every write goes to the end of the file, whatever the offset:

```quiver
f = %file.open [p, mode: A] ~> =\[]
%file.write [f, 0, "!" ~> .0]                    //= 1
%file.close f                                    //= Ok
%fs.read [p] ~> Str[~]                           //= "hello there!"
```

`RW` reads and writes the one file, keeping what is already there:

```quiver
f = %file.open [p, mode: RW] ~> =\[]
%file.write [f, 0, "J" ~> .0]                    //= 1
%file.read_all f ~> Str[~]                       //= "Jello there!"
%file.close f                                    //= Ok
%fs.remove [d, recursive?: Ok]                   //= Ok
```

## Sharing

The handle can be sent to other processes, and their requests are served one at a time by
the file's process, so none of them sees a half-finished write.

```quiver
d = %fs.temp [] ~> =\[]
p = %path.join [d, %path.parse "log"] ~> =\[]
f = %file.open [p, mode: A] ~> =\[]
writer = #'%file { %file.write [$, 0, "x" ~> .0] }
a = @writer f
b = @writer f
[!a, !b]                                         //= [1, 1]
%file.close f                                    //= Ok
%fs.read [p] ~> Str[~]                           //= "xx"
%fs.remove [d, recursive?: Ok]                   //= Ok
```
