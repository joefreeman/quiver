# %path

Filesystem paths as values. A path is not a string here: it is a small recursive tuple, built
so that the structure answers the questions a program actually asks — is this absolute, what
is its parent, does joining these two escape the root — without re-scanning text each time.

```quiver ignore
' = Root | Relative['int] | Path['%str, ^]
```

There are three shapes. `Root` is `/`. `Relative[n]` is a starting point `n` levels above the
current directory — `Relative[0]` is `.`, `Relative[2]` is `../..`. `Path[segment, parent]`
is a named segment under one of those, so a path is built **outward-in**: the last segment is
outermost, and its parent is the rest.

```quiver
%path.parse "/foo/bar"        //= Path["bar", Path["foo", Root]]
%path.parse "a/b"             //= Path["b", Path["a", Relative[0]]]
```

Nothing here touches the filesystem — `%path` is pure manipulation of the value. Asking
whether a path exists is `%fs`'s job.

## Parsing

`parse` splits a slash-separated string and normalises it as it goes. A leading `/` makes the
path absolute; anything else is relative to `.`.

```quiver
%path.parse "/"               //= Root
%path.parse "/foo"            //= Path["foo", Root]
%path.parse "/foo/bar"        //= Path["bar", Path["foo", Root]]
%path.parse "foo"             //= Path["foo", Relative[0]]
%path.parse "foo/bar"         //= Path["bar", Path["foo", Relative[0]]]
```

A `.` segment names the directory it is already in, so it contributes nothing and is dropped.

```quiver
%path.parse "foo/./bar"       //= Path["bar", Path["foo", Relative[0]]]
%path.parse "/foo/./bar"      //= Path["bar", Path["foo", Root]]
%path.parse "./foo"           //= Path["foo", Relative[0]]
```

Empty segments are dropped for the same reason, so a doubled or trailing slash makes no
difference:

```quiver
%path.parse "a//b"            //= Path["b", Path["a", Relative[0]]]
%path.parse "/foo/"           //= Path["foo", Root]
```

A `..` segment goes up one level. When there is a segment above to cancel, it cancels it —
which is why normalisation happens here rather than being deferred:

```quiver
%path.parse "foo/../bar"      //= Path["bar", Relative[0]]
%path.parse "/foo/../bar"     //= Path["bar", Root]
%path.parse "foo/bar/../baz"  //= Path["baz", Path["foo", Relative[0]]]
```

With nothing above to cancel, a relative path records the ascent in its `Relative` count.
That is what the count is for: `../foo` is not the same place as `foo`, and the value has to
say so.

```quiver
%path.parse "../foo"          //= Path["foo", Relative[1]]
%path.parse "../../foo"       //= Path["foo", Relative[2]]
%path.parse "foo/bar/../.."   //= Relative[0]
```

An absolute path has nothing above the root, so `..` there is absorbed. `/..` is `/`, as it
is in every filesystem.

```quiver
%path.parse "/../foo"         //= Path["foo", Root]
```

## Rendering

`to_string` is `parse`'s inverse over normalised paths, so what comes back is the canonical
spelling rather than the input's.

```quiver
%path.parse "/" ~> %path.to_string ~            //= "/"
%path.parse "/foo/bar" ~> %path.to_string ~     //= "/foo/bar"
%path.parse "foo/bar" ~> %path.to_string ~      //= "foo/bar"
%path.parse "/foo/./bar/" ~> %path.to_string ~  //= "/foo/bar"
```

A bare `Relative[n]` renders as the dots it stands for:

```quiver
%path.to_string Relative[0]   //= "."
%path.to_string Relative[1]   //= ".."
%path.to_string Relative[2]   //= "../.."
```

`to_string` also accepts nil and propagates it, so a failed `join` flows through a rendering
pipeline as nil instead of becoming a type error:

```quiver
%path.to_string []            //= []
```

## Segments and parents

`basename` is the final segment. A path that has none — a root, or a dots-only relative
path — answers nil.

```quiver
%path.parse "/foo/bar" ~> %path.basename ~   //= "bar"
%path.parse "/foo" ~> %path.basename ~       //= "foo"
%path.parse "foo" ~> %path.basename ~        //= "foo"
%path.basename Root                          //= []
%path.basename Relative[0]                   //= []
```

`parent` drops the final segment, which is just reading the second field of `Path`.

```quiver
%path.parse "/foo/bar" ~> %path.parent ~   //= Path["foo", Root]
%path.parse "/foo" ~> %path.parent ~       //= Root
%path.parse "foo" ~> %path.parent ~        //= Relative[0]
```

At a starting point there is no segment left to drop, so `parent` says what is above it. The
root is its own parent, and a relative path's ascent count goes up by one:

```quiver
%path.parse "/" ~> %path.parent ~   //= Root
%path.parent Relative[0]            //= Relative[1]
%path.parent Relative[1]            //= Relative[2]
```

Walking up terminates, since every chain of parents ends at `Root` or grows a `Relative`:

```quiver
%path.parse "/a/b/c" ~> %path.parent ~ ~> %path.parent ~ ~> %path.parent ~   //= Root
```

## Absolute and relative

The two predicates ask which starting point a path descends from. They are exclusive and
total: every path is one or the other.

```quiver
%path.parse "/" ~> %path.absolute? ~         //= Ok
%path.parse "/foo/bar" ~> %path.absolute? ~  //= Ok
%path.parse "foo/bar" ~> %path.absolute? ~   //= []
%path.absolute? Relative[0]                  //= []
```

```quiver
%path.parse "foo/bar" ~> %path.relative? ~   //= Ok
%path.relative? Relative[2]                  //= Ok
%path.parse "/" ~> %path.relative? ~         //= []
%path.parse "/foo" ~> %path.relative? ~      //= []
```

Being predicates, they gate a step in the usual way:

```quiver
p = %path.parse "/etc/hosts"
%path.absolute? p; %path.basename p          //= "hosts"
```

## Joining

`join [path, with]` extends the first path by the second. The second must be relative — an
absolute path is a destination, not an extension, so joining one is nil rather than silently
replacing the first.

```quiver
[%path.parse "/foo", %path.parse "bar"] ~> %path.join ~   //= Path["bar", Path["foo", Root]]
[%path.parse "/foo", %path.parse "bar"] ~> %path.join ~ ~> %path.to_string ~     //= "/foo/bar"
[%path.parse "/foo", %path.parse "bar/baz"] ~> %path.join ~ ~> %path.to_string ~ //= "/foo/bar/baz"
[%path.parse "foo", %path.parse "bar"] ~> %path.join ~ ~> %path.to_string ~      //= "foo/bar"
[%path.parse "/foo", %path.parse "/bar"] ~> %path.join ~                         //= []
```

Joining `Relative[0]` — `.` — leaves the path alone, so it is the identity of the operation:

```quiver
[%path.parse "/foo", Relative[0]] ~> %path.join ~ ~> %path.to_string ~   //= "/foo"
```

A `Relative[n]` with `n` above zero ascends, cancelling that many segments of the first path:

```quiver
[%path.parse "/foo/bar", Relative[1]] ~> %path.join ~ ~> %path.to_string ~   //= "/foo"
[%path.parse "/foo", Relative[1]] ~> %path.join ~ ~> %path.to_string ~       //= "/"
[%path.parse "a/b", Relative[1]] ~> %path.join ~ ~> %path.to_string ~        //= "a"
```

Ascending past the root is where the structure earns its keep. There is no `..` above `/` to
represent, so the join has no answer and is nil — a traversal attempt is caught by
construction rather than by inspecting the rendered string afterwards.

```quiver
[%path.parse "/foo", Relative[2]] ~> %path.join ~   //= []
```

The nil propagates through `to_string`, so a whole pipeline can be written straight and
checked once at the end:

```quiver
[%path.parse "/srv", %path.parse "../../etc/passwd"] ~> %path.join ~ ~> %path.to_string ~   //= []
[%path.parse "/srv", %path.parse "app/../static/x.css"] ~> %path.join ~ ~> %path.to_string ~
//= "/srv/static/x.css"
```

A relative starting point has no such floor — ascending simply raises the count:

```quiver
[Relative[1], Relative[2]] ~> %path.join ~   //= Relative[3]
```
