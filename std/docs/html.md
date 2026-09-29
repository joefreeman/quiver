# %html

An HTML dialect and a renderer. `%html{ … }` hands its brace content to the module at compile
time, which parses it into a `'%html` node tree; `render` serializes that tree to a string.

```quiver
%html{ <p class="greeting">hi</p> } ~> %html.render ~   //= "<p class=\"greeting\">hi</p>"
```

The tree is an ordinary value — `Raw`, `Text`, `Element` and `Fragment` tuples — so a template
is data before it is text. It can be matched, held in a variable, returned from a function, and
spliced into another template.

Two rules run through everything below. **Static template text is trusted**: it is what the
author wrote, and it is emitted verbatim. **Hole values are data**: they are escaped at render,
because they are not. Unescaped dynamic markup is the explicit opt-in `raw`.

## Elements

An element renders as itself, children and all.

```quiver
%html{ <p>hi</p> } ~> %html.render ~   //= "<p>hi</p>"
%html{ <div><span>a</span><span>b</span></div> } ~> %html.render ~
//= "<div><span>a</span><span>b</span></div>"
```

The void elements (`br`, `img`, `input`, `link`, `meta`, `hr`, …) take no close tag. `/>`
self-closes any element, and a non-void one renders expanded — an empty `<div/>` in the
template is `<div></div>` on the wire, which is what a browser would make of it anyway.

```quiver
%html{ <img src="a.png"><br><div/> } ~> %html.render ~
//= "<img src=\"a.png\"><br><div></div>"
```

Comments are for the template's reader, so they are dropped rather than emitted.

```quiver
%html{ <div><!-- note -->x</div> } ~> %html.render ~   //= "<div>x</div>"
```

The doctype is not part of the template; it comes from `page`, which renders a tree as a whole
document.

```quiver
%html{ <p>hi</p> } ~> %html.page ~   //= "<!doctype html><p>hi</p>"
```

## Attributes

An attribute is written one of three ways: bare (a boolean), `name="literal"` (verbatim
bytes), or `name={ … }` (a hole — see below). Names may carry `-`, `:` and `_`, so custom and
namespaced attributes need nothing special.

```quiver
%html{ <a href="/x" data-k="v" hidden>go</a> } ~> %html.render ~
//= "<a href=\"/x\" data-k=\"v\" hidden>go</a>"
```

## Fragments and whitespace

A template need not have a single root. Several roots render in order — the tree is a
`Fragment`, which is a node like any other and splices wherever a node is expected.

```quiver
%html{ <li>a</li><li>b</li> } ~> %html.render ~   //= "<li>a</li><li>b</li>"
```

Whitespace-only text at the root's *edges* is trimmed, so a template laid out over several
lines still yields the single root it looks like. Interior whitespace is the author's and
survives verbatim.

```quiver
%html{
  <p>hi</p>
} ~> %html.render ~   //= "<p>hi</p>"
```

```quiver
%html{ <ul>
  <li>a</li>
</ul> } ~> %html.render ~   //= "<ul>\n  <li>a</li>\n</ul>"
```

## Holes

A `{ … }` in child position is a **hole**: ordinary Quiver, parsed by the host and evaluated in
the caller's scope. Its value is normalized into a node, so what may go in a hole is exactly
what `child` accepts — a node, a string, an integer, a list of nodes, or nil.

A string is escaped, because it is data:

```quiver
name = "<b>&\"x"
%html{ <p>{name}</p> } ~> %html.render ~   //= "<p>&lt;b&gt;&amp;\"x</p>"
```

Static text is not, because it is the author's:

```quiver
%html{ <p>&amp; &#123;</p> } ~> %html.render ~   //= "<p>&amp; &#123;</p>"
```

That asymmetry is the whole safety story, and `raw` is how an author opts a *value* into the
trusted side.

```quiver
%html{ <div>{ "<b>x</b>" ~> %html.raw ~ }</div> } ~> %html.render ~   //= "<div><b>x</b></div>"
```

An integer renders as its digits:

```quiver
%html{ <p>{ 42 }</p> } ~> %html.render ~   //= "<p>42</p>"
```

A hole is a block, so it holds branches and steps — and a hole that evaluates to nil renders
nothing. That is the conditional idiom: no separate `if` construct, just a sequence that ends.

```quiver
no = []
%html{ <p>x{ no => "yes" }y</p> } ~> %html.render ~   //= "<p>xy</p>"
```

```quiver
ok? = Ok
%html{ <p>{ ok? => "yes" }</p> } ~> %html.render ~   //= "<p>yes</p>"
```

`~` inside a hole is the value flowing into the dialect term, which is what makes a template a
step in a chain rather than a thing that needs its data named first.

```quiver
[name: "Ada"] ~> %html{ <p>{~.name}</p> } ~> %html.render ~   //= "<p>Ada</p>"
```

Because `{` always opens a hole, a literal brace in text is written as an entity — `&#123;`
and `&#125;`.

## Attribute holes

`name={ … }` puts a hole in attribute-value position. Its value is normalized by `attr_value`:
a string renders as the value (escaped for the quoted context, so the quote itself becomes
`&quot;`), an integer as its digits, `Ok` renders the attribute **bare**, and nil **omits** it
entirely.

```quiver
c = "a\"b"
%html{ <div class={c} data-n={ 7 }>x</div> } ~> %html.render ~
//= "<div class=\"a&quot;b\" data-n=\"7\">x</div>"
```

```quiver
%html{ <input disabled={ Ok } value={ [] }> } ~> %html.render ~   //= "<input disabled>"
```

So a boolean attribute is a hole answering `Ok` or nil, and an optional attribute is a hole
answering a string or nil — no special syntax for either.

## Lists and components

A hole holding a list of nodes splices as a fragment, which is how repetition works.

```quiver
items = %list.map [%list{ "a", "b" }, #'%str { %html{ <li>{$}</li> } }]
%html{ <ul>{items}</ul> } ~> %html.render ~   //= "<ul><li>a</li><li>b</li></ul>"
```

A component is just a function from props to a tree; there is no component protocol to learn.
Children are passed as an ordinary field whose value is another `%html{ … }`. A tree's typed
attribute values are rendered as `%data` notation, so a component generic in them bounds its
parameter by `'%data`.

```quiver
card = #<'e: '%data>[title: '%str, children: '%html<'e>] {
  %html{ <section><h2>{$title}</h2>{$children}</section> }
}
%html{ <main>{ [title: "T", children: %html{ <p>b</p> }] ~> card ~ }</main> } ~> %html.render ~
//= "<main><section><h2>T</h2><p>b</p></section></main>"
```

Capitalized tags are reserved for a future component syntax, so `<Card/>` is rejected rather
than treated as an unknown element — see below.

## The node tree

The dialect answers a value, not a string, so a template can be inspected and matched like any
other data. Static text is `Raw`, an escaped hole value is `Text`, and an element carries its
tag, its attributes and its children as lists.

```quiver
%html{ <div id="x">hi</div> }   //= Element(tag: "div")
%html{ <div id="x">hi</div> }   //= Element(attrs: Cons[["id", "x"], Nil])
%html{ <br> }                   //= Element[tag: "br", attrs: Nil, children: Nil]
%html{ <li>a</li><li>b</li> }   //= Fragment[_]
```

Children are a list, so they are counted and walked like any other:

```quiver
%html{ <div><span>a</span><span>b</span></div> } ~> =Element(children: cs); %list.count cs
//= 2
```

Nodes can also be built directly, without the dialect: `text` makes an escaped text node and
`raw` a verbatim one.

```quiver
"a<b" ~> %html.text ~                     //= Text["a<b"]
"a<b" ~> %html.text ~ ~> %html.render ~   //= "a&lt;b"
"a<b" ~> %html.raw ~ ~> %html.render ~    //= "a<b"
```

The two escapers are exported for the same reason — a caller assembling markup by hand needs
the module's rules, not its own. Text content escapes `&`, `<` and `>`; an attribute value
escapes `&` and the quote.

```quiver
%html.escape_text "a<b>&c"    //= "a&lt;b&gt;&amp;c"
%html.escape_attr "a\"b&c"    //= "a&quot;b&amp;c"
```

Every expansion also carries a `:template` annotation — the pre-rendered static byte runs and
the normalized hole values they interleave with. It is invisible to matching, equality and
`render`; it exists so a layer above (`%html/live`) can diff two renderings of the same
template by their holes alone.

## What the dialect rejects

The dialect runs at compile time, so a malformed template is a compile error with a position,
not a runtime surprise.

A close tag must match the element it closes:

```quiver
%html{ <div><span></div> }   //! '</span>'
```

A capitalized tag is not an unknown element — the name space is reserved:

```quiver
%html{ <Card/> }   //! capitalized tags are reserved for components
```

Inline `<script>` and `<style>` bodies are not supported; link external assets instead:

```quiver
%html{ <script>x</script> }   //! inline script/style is not supported
```

An attribute may not be given twice, since only one of the two could survive:

```quiver
%html{ <div id="a" id="b"></div> }   //! a unique attribute name
```

A hole is type-checked where it splices, against the union `child` accepts — so a value that
is not a node, a string, an integer, a node list or nil is a type error naming the offending
value.

```quiver
%html{ <p>{ [1, 2] }</p> }   //! ['int, 'int] does not fit
```

## The policy seam

`%html` owns *syntax and mechanism*; a dialect layered on it owns *vocabulary*. The three
exported stages are the seam: `parse` answers the parse tree, `rewrite_attrs` applies an
attribute policy to it, and `expand` emits the tree expression a dialect returns.

A policy is a function over one written attribute, and may rename it, drop it, replace it with
several, or give it an arbitrary value expression — so a layered dialect decides what
`on:click` means without `%html` knowing the name. `%html`'s own policy claims nothing, which
is why an attribute modifier bracket (`name[ … ]`, meaningful only to a layer that reads it) is
rejected here. `%html/live` is this seam's one caller in the standard library.
