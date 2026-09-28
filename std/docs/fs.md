# %fs

The file system: reading and writing whole files, looking paths up, listing directories, and
creating, moving and removing what they name. Every path argument is a `'%path` value, and
every path answered is one, so `%path` does the manipulation and `%fs` the touching. `%file`
is the long-lived handle, for a file read piecemeal or shared between processes.

```quiver ignore
'kind  = File | Dir | Symlink | Other
'entry = Entry[name: '%str, path: '%path, kind: 'kind]
'stat  = Stat[kind: 'kind, size: 'int, modified: '%time.instant, perm: 'int]
```

Each chapter below works in a directory of its own from `temp`, and removes it at the end.

## Temporary directories

`temp` creates a fresh, empty directory under the host's temp dir and answers its path. It is
readable only by its owner, and nothing removes it automatically: that is the caller's job.

```quiver
d = %fs.temp [] ~> =\[]
%fs.dir? [d]                              //= Ok
%fs.list [d] ~> %iter.count ~             //= 0
%fs.stat [d] ~> =Stat(perm: ~)            //= 448 // 0o700
%fs.remove [d]                            //= Ok
{ %fs.exists? [d] }                       //= []
```

Two calls never answer the same directory:

```quiver
a = %fs.temp [] ~> =\[]
b = %fs.temp [] ~> =\[]
{ a ~> =^b }                              //= []
%fs.remove [a]; %fs.remove [b]            //= Ok
```

## Reading and writing

`read` answers a whole file's contents, and `write` replaces them, creating the file if it is
not there. Both work in the calling process, opening and closing the file around the call.

```quiver
d = %fs.temp [] ~> =\[]
f = %path.join [d, %path.parse "notes.txt"] ~> =\[]
%fs.write [f, "hello" ~> .0]                   //= Ok
%fs.read [f] ~> Str[~]                         //= "hello"
%fs.write [f, "bye" ~> .0]                     //= Ok
%fs.read [f] ~> Str[~]                         //= "bye"
```

`mode` chooses how an existing file is treated: `W` (the default) replaces it, `A` appends to
it, and `New` refuses it, failing with `AlreadyExists` — the way to claim a name without
racing another writer.

```quiver
%fs.write [f, "!" ~> .0, mode: A]              //= Ok
%fs.read [f] ~> Str[~]                         //= "bye!"
{ %fs.write [f, <>, mode: New] } ~> :error<'%io> ~> =IoError(kind: ~)   //= AlreadyExists
g = %path.join [d, %path.parse "fresh.txt"] ~> =\[]
%fs.write [g, <>, mode: New]                   //= Ok
%fs.read [g]                                   //= <>
```

A path that is not there, or is not a file, fails as a nil carrying `:error`:

```quiver
{ %fs.read [d] } ~> :error<'%io> ~> =IoError(kind: ~)                                        //= IsADirectory
{ %fs.read [%path.join [d, %path.parse "nope"] ~> =\[]] } ~> :error<'%io> ~> =IoError(kind: ~) //= NotFound
%fs.remove [d, recursive?: Ok]                 //= Ok
```

## Where am I

`cwd` answers the host's working directory, and `canonical` the absolute path the kernel
resolves a path to — every symlink followed and every `..` applied. A relative path is taken
from the working directory, so the two agree about `.`:

```quiver
here = %fs.cwd [] ~> =\[]
%path.absolute? here                                //= Ok
%fs.canonical [%path.parse "."] ~> =^here           //= ('%path)
```

`%path` normalises `..` lexically, before the kernel ever sees the path, so `a/b/..` is `a`
even where `b` is a symlink to somewhere else. That is what makes `%path.join`'s escape check
work by construction; `canonical` is the kernel's view, when that is what is wanted.

## Looking a path up

`stat` describes a path, and answers plain nil when there is nothing there — the ordinary
"found nothing" of a lookup, like `%dict.get`. `exists?`, `file?` and `dir?` are the same
lookup asked as a question.

```quiver
d = %fs.temp [] ~> =\[]
%fs.create_dir [%path.join [d, %path.parse "sub"] ~> =\[]]   //= Ok
%fs.stat [d] ~> =Stat(kind: ~)                                //= Dir
%fs.dir? [%path.join [d, %path.parse "sub"] ~> =\[]]          //= Ok
{ %fs.file? [d] }                                             //= []
missing = %path.join [d, %path.parse "nope"] ~> =\[]
{ %fs.stat [missing] }                                        //= []
{ %fs.exists? [missing] }                                     //= []
%fs.remove [d, recursive?: Ok]                                //= Ok
```

A lookup that *fails* — as opposed to finding nothing — answers nil too, but carrying an
`:error`, so a caller that cares can tell the two apart:

```quiver
{ %fs.stat [%path.parse "/nonexistent-quiver-path"] } ~> :error<'%io>   //= []
```

## Listing a directory

`list` iterates a directory's entries lazily, in the order the file system gives them,
without `.` and `..`. Each entry carries its name, its full path and its kind — the entry's
own kind, so a symlink lists as `Symlink` whatever it points to.

```quiver
d = %fs.temp [] ~> =\[]
%fs.create_dir [%path.join [d, %path.parse "one"] ~> =\[]]
%fs.create_dir [%path.join [d, %path.parse "two"] ~> =\[]]
%fs.list [d] ~> %iter.count ~                                  //= 2
%fs.list [d] ~> %iter.map [~, #'%fs.entry { $kind }] ~> %list.collect ~
//= Cons[Dir, Cons[Dir, Nil]]
%fs.remove [d, recursive?: Ok]                                 //= Ok
```

A listing that fails part-way ends its iterator with the failure rather than a quiet end, so
collect with `%list.try_collect` to see it. Listing what is not there fails straight away:

```quiver
d = %fs.temp [] ~> =\[]
%fs.list [d] ~> %list.try_collect ~                            //= Nil
{ %fs.list [%path.parse "/nonexistent-quiver-path"] ~> %list.try_collect ~ }
~> :error<'%io> ~> =IoError(kind: ~)                           //= NotFound
%fs.remove [d]                                                 //= Ok
```

## Walking a tree

`walk` iterates everything under a directory, lazily and depth-first: each directory's entry
comes before its contents, and its contents are finished before its next sibling. The
directory itself is not among the entries, as with `list`.

```quiver
d = %fs.temp [] ~> =\[]
%fs.create_dir [%path.join [d, %path.parse "a/b"] ~> =\[], all?: Ok]           //= Ok
%fs.write [%path.join [d, %path.parse "a/b/c.txt"] ~> =\[], <>]                //= Ok
names = #'%fs.entry { $name }
%fs.walk [d] ~> %iter.map [~, names] ~> %list.collect ~
//= Cons["a", Cons["b", Cons["c.txt", Nil]]]
%fs.walk [d] ~> %iter.filter [~, #'%fs.entry { =(kind: File) }] ~> %iter.map [~, #'%fs.entry { $path }] ~> %list.collect ~
~> =Cons[~, Nil] ~> %path.basename ~                                            //= "c.txt"
```

A symlink is yielded but not entered, so a walk stays inside the tree it was given. With
`follow?`, a link to a directory is entered too — unless it leads back to a directory the walk
is already inside, which would otherwise go round forever.

```quiver
outside = %fs.temp [] ~> =\[]
%fs.write [%path.join [outside, %path.parse "far.txt"] ~> =\[], <>]            //= Ok
%fs.symlink [link: %path.join [d, %path.parse "a/out"] ~> =\[], target: outside] //= Ok
%fs.symlink [link: %path.join [d, %path.parse "a/loop"] ~> =\[], target: d]     //= Ok
%fs.walk [d] ~> %iter.count ~                                                   //= 5
%fs.walk [d, follow?: Ok] ~> %iter.count ~                                      //= 6 // far.txt, and loop is not entered
```

As with `list`, a failure — a directory that cannot be read, say — ends the walk carrying its
`:error`, which `%list.try_collect` keeps:

```quiver
{ %fs.walk [%path.parse "/nonexistent-quiver-path"] ~> %list.try_collect ~ }
~> :error<'%io> ~> =IoError(kind: ~)                                            //= NotFound
%fs.remove [d, recursive?: Ok]                                                  //= Ok
%fs.remove [outside, recursive?: Ok]                                            //= Ok
```

## Creating directories

`create_dir` makes one directory, whose parent must exist, and fails with `AlreadyExists` if
something is already there. `all?` is `mkdir -p`: missing parents are made too, and an
existing directory is fine.

```quiver
d = %fs.temp [] ~> =\[]
deep = %path.join [d, %path.parse "a/b/c"] ~> =\[]
{ %fs.create_dir [deep] } ~> :error<'%io> ~> =IoError(kind: ~)   //= NotFound
%fs.create_dir [deep, all?: Ok]                                   //= Ok
%fs.dir? [deep]                                                   //= Ok
%fs.create_dir [deep, all?: Ok]                                   //= Ok // already there
{ %fs.create_dir [deep] } ~> :error<'%io> ~> =IoError(kind: ~)   //= AlreadyExists
%fs.remove [d, recursive?: Ok]                                    //= Ok
```

## Copying and renaming

`copy` copies a regular file's contents and permission bits. It will not overwrite: an
existing destination fails with `AlreadyExists` unless `replace?` is set.

```quiver
d = %fs.temp [] ~> =\[]
a = %path.join [d, %path.parse "a.txt"] ~> =\[]
b = %path.join [d, %path.parse "b.txt"] ~> =\[]
%fs.write [a, "hello" ~> .0]                  //= Ok
%fs.copy [a, b]                                //= Ok
%fs.stat [b] ~> =Stat(size: ~)                 //= 5
{ %fs.copy [a, b] } ~> :error<'%io> ~> =IoError(kind: ~)   //= AlreadyExists
%fs.copy [a, b, replace?: Ok]                  //= Ok
```

Only files are copied — a directory fails with `IsADirectory`:

```quiver
{ %fs.copy [d, b, replace?: Ok] } ~> :error<'%io> ~> =IoError(kind: ~)   //= IsADirectory
```

`rename` moves a path atomically, replacing a file already at the destination. Across file
systems it fails with `CrossesDevices`, where a copy and a remove is the fallback.

```quiver
c = %path.join [d, %path.parse "c.txt"] ~> =\[]
%fs.rename [from: a, to: c]                    //= Ok
{ %fs.exists? [a] }                            //= []
%fs.stat [c] ~> =Stat(size: ~)                 //= 5
%fs.rename [c, b]                              //= Ok // b is replaced
%fs.list [d] ~> %iter.count ~                  //= 1
%fs.remove [d, recursive?: Ok]                 //= Ok
```

## Symlinks

`symlink` makes a link, and its arguments are labelled, since which of the two comes first is
a well-known trap. The target is stored as written: a relative one is resolved from the link's
directory whenever the link is followed. `read_link` answers it back.

```quiver
d = %fs.temp [] ~> =\[]
target = %path.join [d, %path.parse "real"] ~> =\[]
link = %path.join [d, %path.parse "link"] ~> =\[]
%fs.create_dir [target]                                    //= Ok
%fs.symlink [link: link, target: %path.parse "real"]       //= Ok
%fs.read_link [link]                                       //= Path["real", Relative[0]]
%fs.link? [link]                                           //= Ok
```

Lookups follow a link to what it names unless `follow?` is nil, when they describe the link
itself. `link?` never follows, and `list` describes entries as they are.

```quiver
%fs.dir? [link]                                            //= Ok
{ %fs.dir? [link, follow?: []] }                           //= []
%fs.stat [link, follow?: []] ~> =Stat(kind: ~)             //= Symlink
real = %fs.canonical [target]
%fs.canonical [link] ~> =^real                             //= ('%path)
```

A dangling link exists only when not followed:

```quiver
%fs.remove [target]                                        //= Ok
{ %fs.exists? [link] }                                     //= []
%fs.exists? [link, follow?: []]                            //= Ok
{ %fs.read_link [target] } ~> :error<'%io> ~> =IoError(kind: ~)   //= NotFound
%fs.remove [d, recursive?: Ok]                             //= Ok
```

## Permissions

`perm` is the permission bits of a path's mode — `0o644` is `420` — and `set_perm` sets them,
following a symlink.

```quiver
d = %fs.temp [] ~> =\[]
%fs.set_perm [d, 493]                          //= Ok // 0o755
%fs.stat [d] ~> =Stat(perm: ~)                 //= 493
%fs.remove [d]                                 //= Ok
```

## Removing

`remove` removes a file, a symlink or an empty directory. A directory with anything in it
fails with `DirectoryNotEmpty` unless `recursive?` is set. A symlink is removed rather than
followed, and a recursive removal never descends through one.

```quiver
d = %fs.temp [] ~> =\[]
outside = %fs.temp [] ~> =\[]
%fs.create_dir [%path.join [d, %path.parse "sub"] ~> =\[]]                    //= Ok
%fs.symlink [link: %path.join [d, %path.parse "out"] ~> =\[], target: outside] //= Ok
{ %fs.remove [d] } ~> :error<'%io> ~> =IoError(kind: ~)                       //= DirectoryNotEmpty
%fs.remove [d, recursive?: Ok]                                                 //= Ok
{ %fs.exists? [d] }                                                            //= []
%fs.dir? [outside]                                                             //= Ok // untouched
```

Removing what is not there fails with `NotFound`. There is no `force?` option, since
containing the failure in a block is the idempotent form:

```quiver
{ %fs.remove [d] } ~> :error<'%io> ~> =IoError(kind: ~)   //= NotFound
{ %fs.remove [d] }                                        //= []
%fs.remove [outside]                                      //= Ok
```
