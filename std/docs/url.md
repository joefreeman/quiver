# %url

Absolute URLs: parsing, rendering, and reference resolution. A URL is a value with six named
fields, and every one of them is concrete once parsed — there are no absent components to
special-case downstream.

```quiver ignore
' = Url[scheme: '%str, host: '%str, port: 'int, path: '%str, query: '%str, fragment: '%str]
```

Nothing here is HTTP-specific: `ws://` and `wss://` want the same shape, so the module knows
nothing about requests or headers, and performs no I/O.

```quiver
%url.parse "https://example.com/index.html"
//= Url(scheme: "https", host: "example.com", path: "/index.html")
```

The grammar accepted is deliberately narrow — `scheme://host[:port][/path][?query][#fragment]`,
which is what a client needs. There is no `user:pass@host` userinfo (obsolete, and a
credential-leaking hazard), and no percent-decoding: the pieces go back onto the wire as
written, and a query is decoded with `%http.form_decode` when a program wants its fields.

## Parsing

`parse` splits a string into all six components at once. Nothing is dropped and nothing is
inferred except the port.

```quiver
%url.parse "http://example.com/a/b?x=1&y=2#frag"
//= Url[scheme: "http", host: "example.com", port: 80, path: "/a/b", query: "x=1&y=2", fragment: "frag"]
```

The delimiters belong to the syntax, not the values: `query` is what follows `?` and
`fragment` what follows `#`, neither carrying its introducer. When a component is absent the
field is the empty string, and `path` is at least `"/"`.

```quiver
%url.parse "http://e.com"        //= Url(path: "/", query: "", fragment: "")
%url.parse "http://e.com?q=1"    //= Url(path: "/", query: "q=1", fragment: "")
%url.parse "http://e.com/a#top"  //= Url(path: "/a", query: "", fragment: "top")
%url.parse "http://e.com/a#"     //= Url(path: "/a", fragment: "")
```

Because every field is a plain string, ordinary destructuring reaches them:

```quiver
%url.parse "https://example.com:8443/v1/items?page=2" ~> =('%url & u)
[u.scheme, u.host, u.port]    //= ["https", "example.com", 8443]
u.path                        //= "/v1/items"
u.query                       //= "page=2"
```

The parse works on bytes, not characters, since a URL is a wire format — so a multi-byte
character in a path is carried through untouched rather than being re-indexed as text.

```quiver
%url.parse "http://e.com/caf%C3%A9"   //= Url(path: "/caf%C3%A9")
```

## Ports and normalisation

A port is always concrete. An absent one is filled in from the scheme, so a client never has
to ask "and what does this scheme default to".

```quiver
%url.parse "https://example.com"   //= Url(scheme: "https", port: 443)
%url.parse "http://example.com"    //= Url(scheme: "http", port: 80)
%url.parse "ws://h/socket"         //= Url(scheme: "ws", port: 80)
%url.parse "wss://h/socket"        //= Url(scheme: "wss", port: 443)
```

A stated port wins, and a scheme with no known default gets 0 — which is the module saying
"there is no default", not a guess.

```quiver
%url.parse "https://example.com:8080/p"   //= Url(port: 8080)
%url.parse "ftp://h/x"                    //= Url(scheme: "ftp", port: 0)
```

A scheme is case-insensitive on the wire, so it is folded to lower case at the parse and
compared thereafter as an ordinary string. The host is left as written.

```quiver
%url.parse "HTTP://example.com:8080/p"   //= Url(scheme: "http", host: "example.com", port: 8080)
%url.parse "HtTpS://E.com/"              //= Url(scheme: "https", host: "E.com")
```

An IPv6 literal keeps its brackets, and the colons inside them are part of the address rather
than the port separator — which is exactly why the brackets exist.

```quiver
%url.parse "https://[::1]:9000/v6"   //= Url(host: "[::1]", port: 9000)
%url.parse "https://[::1]/v6"        //= Url(host: "[::1]", port: 443)
```

## Malformed input

A URL that does not parse is nil, like any other failed match, so a caller either branches on
it or lets the sequence end.

```quiver
%url.parse "not a url"           //= [] // no scheme separator
%url.parse "://example.com"      //= [] // no scheme
%url.parse "http:///p"           //= [] // no host
%url.parse "http:/example.com"   //= [] // one slash, so no authority
%url.parse "http://h:notaport/"  //= [] // the port is not a number
```

Since it is nil, a guard is a step boundary and nothing more:

```quiver
{ %url.parse "http://e.com/" ~> ='%url => Fetchable | Rejected }   //= Fetchable
{ %url.parse "nonsense" ~> ='%url => Fetchable | Rejected }        //= Rejected
```

## Rendering

`format` is `parse`'s inverse. It omits a port that is the scheme's default, so a URL that
was parsed with an explicit `:443` comes back canonical rather than as written.

```quiver
%url.parse "https://e.com:443/p" ~> =('%url & u)
%url.format u                  //= "https://e.com/p"
```

```quiver
%url.parse "http://e.com:8080/p?q#f" ~> =('%url & u)
%url.format u                  //= "http://e.com:8080/p?q#f"
```

`authority` is the `host` or `host:port` half of that — what a `Host` header carries, under
the same default-port rule.

```quiver
%url.parse "http://e.com/a" ~> =('%url & u)
%url.authority u               //= "e.com"
```

```quiver
%url.parse "http://e.com:8080/a" ~> =('%url & u)
%url.authority u               //= "e.com:8080"
```

`target` is the request-target: path and query, and never the scheme, host or fragment. A
fragment is a client-side concern and is not sent, which is why the split lives here rather
than in the caller.

```quiver
%url.parse "http://e.com/a?x=1#frag" ~> =('%url & u)
%url.target u                  //= "/a?x=1"
```

```quiver
%url.parse "http://e.com/a" ~> =('%url & u)
%url.target u                  //= "/a"
```

Together they are the two halves an HTTP request line and `Host` header need:

```quiver
%url.parse "https://api.example.com/v1/items?page=2" ~> =('%url & u)
"GET {%url.target u} — Host: {%url.authority u}"
//= "GET /v1/items?page=2 — Host: api.example.com"
```

## Resolving references

`resolve [base, ref]` interprets a reference against a base URL, the way a redirect's
`Location` is interpreted. The reference is a string, since that is what arrives on the wire,
and the result is a URL — or nil when what it names is not well-formed.

```quiver
%url.parse "http://example.com/a/b?x=1#f" ~> =('%url & base)
r = #'%str { %url.resolve [base, $] ~> =('%url & u); %url.format u }
```

An absolute reference stands alone and the base contributes nothing:

```quiver
r "https://other.org/z"        //= "https://other.org/z"
```

A scheme-relative reference inherits only the scheme — this is how a redirect moves to
another host while staying on the protocol the client arrived on:

```quiver
r "//other.org/z"              //= "http://other.org/z"
```

An absolute-path reference inherits the authority, and replaces path, query and fragment
wholesale:

```quiver
r "/z?q=2"                     //= "http://example.com/z?q=2"
```

A fragment-only reference re-fragments the base and leaves everything else — including the
query — in place:

```quiver
r "#top"                       //= "http://example.com/a/b?x=1#top"
```

Anything else is relative to the base path's *directory* — everything up to and including its
last `/`. So `c/d` against `/a/b` is `/a/c/d`: `b` is a document, not a directory, and is
replaced.

```quiver
r "c/d"                        //= "http://example.com/a/c/d"
```

The empty reference is the base itself — a self-reference, which is what an empty `Location`
or `href=""` means:

```quiver
r ""                           //= "http://example.com/a/b?x=1#f"
```

A reference that composes into something that is not a well-formed URL is nil, exactly as a
bad `parse` is:

```quiver
%url.resolve [base, "//"]      //= [] // scheme-relative, but with no host
```
