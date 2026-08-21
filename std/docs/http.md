# %http

The shared HTTP vocabulary: the message records, and the codecs between them and bytes.

It is pure data — nothing here opens a socket, and there is no client and no server in it.
`%http/server` builds a connection pump over these codecs and `%http/client` a request path,
but both speak the same records, so a handler is an ordinary function that can be called
directly.

```quiver ignore
'method = GET | HEAD | POST | PUT | DELETE | PATCH | OPTIONS | Other['%str]
'pairs = '%list<['%str, '%str]>
'segments = '%list<'%str>

' = Request[method: 'method, target: '%str, path: 'segments, query: 'pairs,
            version: '%str, headers: 'pairs, body: 'bin]
'response = Response[status: 'int, headers: 'pairs, body: 'bin]
```

Headers, query parameters, form fields and cookies all share one shape: an ordered list of
`[name, value]` pairs, duplicates preserved. `Set-Cookie` twice is two cookies and a repeated
form field is two entries, so nothing is quietly collapsed into a map — `header` looks a name
up case-insensitively, `get` exactly.

The codecs work on raw bytes, because that is what a socket delivers. `"…" ~> .0` is a
string's bytes and `Str[…]` puts them back.

## Parsing a request

Reads arrive in whatever sizes the network chooses, so `parse_request` is incremental: it
reads *one* request out of a buffer and answers `Incomplete` (read more), `Bad[status,
reason]` (send that status and close), or the request together with the bytes left over.

```quiver
"GET /posts/7 HTTP/1.1\r\nHost: x\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[r, rest]
r.method    //= GET
r.path      //= Cons["posts", Cons["7", Nil]]
r.version   //= "HTTP/1.1"
rest        //= <>
```

A head that has not finished arriving, and a head that has but whose body has not, are both
`Incomplete` — the caller reads more and asks again.

```quiver
"GET / HTTP/1.1\r\nHost:" ~> .0 ~> %http.parse_request ~   //= Incomplete
"POST /x HTTP/1.1\r\nContent-Length: 5\r\n\r\nhe" ~> .0 ~> %http.parse_request ~   //= Incomplete
```

`Content-Length` delimits the body. Bytes beyond it are the start of the *next* request on a
keep-alive connection, and come back as the leftover rather than being buffered out of sight.

```quiver
"POST /x HTTP/1.1\r\nContent-Length: 5\r\n\r\nhelloGET /" ~> .0 ~> %http.parse_request ~ ~> =[r, rest]
r.body   //= <68656c6c6f>   "hello"
rest     //= <474554202f>   "GET /"
```

### What it rejects

A `Bad` carries the status to answer with and a reason for the log. Malformed input is
reported rather than guessed at.

```quiver
"nonsense\r\n\r\n" ~> .0 ~> %http.parse_request ~
//= Bad[status: 400, reason: "malformed request line"]
"POST /x HTTP/1.1\r\nContent-Length: nope\r\n\r\n" ~> .0 ~> %http.parse_request ~
//= Bad[status: 400, reason: "invalid content-length"]
```

A chunked request body is refused outright, with 501:

```quiver
"POST /x HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n" ~> .0 ~> %http.parse_request ~
//= Bad[status: 501, reason: "transfer encodings are not supported"]
```

That is a decision a server can make and a client cannot: a client does not dictate how the
server frames its answer, so `parse_response` below does decode chunked.

### Size limits

Hardening here is size caps rather than timeouts, so the parser fails fast on a buffer that is
growing without ever completing. `parse_request` uses 8 KiB of head and 1 MiB of body;
`parse_request_with` states both.

```quiver
"GET /aaaaaaaaaaaaaaaa HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request_with [~, 8, 100]
//= Bad[status: 431, reason: "request head too large"]
"POST /x HTTP/1.1\r\nContent-Length: 200\r\n\r\n" ~> .0 ~> %http.parse_request_with [~, 8192, 100]
//= Bad[status: 413, reason: "request body too large"]
```

The head cap is checked *before* the head completes — as in the 431 above, where no
`\r\n\r\n` has arrived at all — so a peer that never finishes its head is cut off rather than
accumulated.

## The request target

The target is kept verbatim, and the decoded `path` and `query` are derived from it. Segments
and query values are percent-decoded, and in a query `+` is a space.

```quiver
"GET /a%20b?x=1&msg=hi+there HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[r, _]
r.target   //= "/a%20b?x=1&msg=hi+there"
r.path     //= Cons["a b", Nil]
r.query    //= Cons[["x", "1"], Cons[["msg", "hi there"], Nil]]
```

Paths are normalised the one way that has no information in it: the leading `/` goes, and a
single trailing empty segment drops, so `/posts/` and `/posts` are the same route and `/` is
the empty list. Interior empty segments are kept, since those the writer meant.

```quiver
"GET /posts/ HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[r, _]
r.path   //= Cons["posts", Nil]
"GET / HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[r, _]
r.path   //= Nil
```

`split_target` is that decomposition on its own, for deriving a request from a new URL
without going through the wire format:

```quiver
%http.split_target "/a/b?x=1"   //= [Cons["a", Cons["b", Nil]], Cons[["x", "1"], Nil]]
%http.split_target "/"          //= [Nil, Nil]
%http.split_target "/a//b"      //= [Cons["a", Cons["", Cons["b", Nil]]], Nil]
```

## Headers

Header names are case-insensitive on the wire, so `header` compares them that way. A missing
header is nil, like any other lookup that found nothing, and a duplicated one answers the
first.

```quiver
"GET / HTTP/1.1\r\nX-Thing: One\r\nx-thing: Two\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[r, _]
%http.header [r.headers, "X-THING"]   //= "One"
%http.header [r.headers, "missing"]   //= []
```

`get` is the exact-match sibling, for the pair lists whose names are data rather than protocol
— query parameters, form fields, cookies.

```quiver
q = %list{ ["x", "1"], ["X", "2"] }
%http.get [q, "x"]   //= "1"
%http.get [q, "X"]   //= "2"
```

`lower` is the case fold both are built on, exposed because anything comparing header names
needs it.

```quiver
%http.lower "Content-Type"   //= "content-type"
```

A header *block* — the CRLF-separated lines, with no blank line after them — converts both
ways. This is the unit a host hands over when it has already done the framing itself, and
`%http` is the one place that knows the grammar.

```quiver
"host: x\r\ncontent-length: 0\r\n" ~> .0 ~> %http.headers_of ~
//= Cons[["host", "x"], Cons[["content-length", "0"], Nil]]
%http.headers_to_bytes %list{ ["host", "x"] } ~> Str[~]   //= "host: x\r\n"
```

## Forms

`form` decodes a request's body as `application/x-www-form-urlencoded`, and `form_decode` does
the same for any buffer — a query string, say, since the two grammars are one.

```quiver
"POST /x HTTP/1.1\r\nContent-Length: 23\r\n\r\na=1&b=hi+x&c=%2Fpath%2F" ~> .0 ~> %http.parse_request ~ ~> =[r, _]
%http.form r   //= Cons[["a", "1"], Cons[["b", "hi x"], Cons[["c", "/path/"], Nil]]]
```

Duplicates survive in order, and a bare name is that name with an empty value. `get` answers
the first, which is what a caller that expects one field wants; a caller that wants all of
them has the list.

```quiver
f = "k=1&k=2&bare" ~> .0 ~> %http.form_decode ~
f                    //= Cons[["k", "1"], Cons[["k", "2"], Cons[["bare", ""], Nil]]]
%http.get [f, "k"]   //= "1"
```

`form_encode` is the inverse. Spaces become `+`, and everything outside the unreserved set is
percent-escaped — including the `&` and `=` that would otherwise be read as structure.

```quiver
%list{ ["a b", "c&d=e"], ["k", "v"] } ~> %http.form_encode ~ ~> Str[~]   //= "a+b=c%26d%3De&k=v"
%list{ ["a b", "c&d=e"] } ~> %http.form_encode ~ ~> %http.form_decode ~   //= Cons[["a b", "c&d=e"], Nil]
```

## Cookies

`cookies` reads the request's `Cookie` headers into the same pair list. Values are verbatim
bytes: cookie values are not percent-encoded by the protocol, so decoding one would corrupt a
value that happens to contain a `%`.

```quiver
"GET / HTTP/1.1\r\nCookie: a=1; session=abc.def; b=x%20y\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[r, _]
%http.cookies r   //= Cons[["a", "1"], Cons[["session", "abc.def"], Cons[["b", "x%20y"], Nil]]]
```

No `Cookie` header means no cookies, and a segment without an `=` is skipped rather than
failing the request.

```quiver
"GET / HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[r, _]
%http.cookies r   //= Nil
```

`set_cookie` builds the header pair for the other direction. The attributes are plain strings,
so the module needs no vocabulary for them and none can be misspelled into silence.

```quiver
%http.set_cookie ["s", "v"]   //= ["set-cookie", "s=v"]
%http.set_cookie ["s", "v", %list{ "Path=/", "HttpOnly" }]
//= ["set-cookie", "s=v; Path=/; HttpOnly"]
```

## Responses

A response is `Response[status, headers, body]` — no version and no reason phrase, since the
phrase is decoration and the version is HTTP/1.1.

`serialize_response` writes it, supplying `content-length` when the headers do not:

```quiver
Response[status: 404, headers: Nil, body: "gone" ~> .0] ~> %http.serialize_response ~ ~> Str[~]
//= "HTTP/1.1 404 Not Found\r\ncontent-length: 4\r\n\r\ngone"
```

A stated one is left alone, which is what lets a caller frame a body itself:

```quiver
Response[status: 200, headers: %list{ ["content-length", "0"] }, body: <>] ~> %http.serialize_response ~ ~> Str[~]
//= "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n"
```

`reason` is the phrase table it uses, and answers the empty string for a code it does not
list — an unknown status is still a valid one.

```quiver
%http.reason 200   //= "OK"
%http.reason 404   //= "Not Found"
%http.reason 599   //= ""
```

## Building a request

`request` builds one, deriving `path` and `query` from the target exactly as the server's
parser would. That is the point of a single vocabulary: a client-built request and a parsed
one are the *same value*, so `request` → `serialize_request` → `parse_request` is a round
trip.

```quiver
req = %http.request [
  method: POST,
  target: "/submit?a=1",
  headers: %list{ ["host", "x"] },
  body: "hello" ~> .0,
]
%http.serialize_request req ~> %http.parse_request ~ ~> =[r, _]
r.method     //= POST
r.target     //= "/submit?a=1"
r.path       //= Cons["submit", Nil]
r.query      //= Cons[["a", "1"], Nil]
Str[r.body]  //= "hello"
```

`headers` and `body` default to empty, and `serialize_request` adds `content-length` only when
there is a body and no length already stated — a bodyless GET carries neither.

```quiver
%http.request [method: GET, target: "/"] ~> %http.serialize_request ~ ~> Str[~]
//= "GET / HTTP/1.1\r\n\r\n"
```

```quiver
%http.request [
  method: PUT,
  target: "/x",
  headers: %list{ ["content-length", "99"] },
  body: "hi" ~> .0,
] ~> %http.serialize_request ~ ~> Str[~]   //= "PUT /x HTTP/1.1\r\ncontent-length: 99\r\n\r\nhi"
```

`Host` is the caller's to supply: this module never sees the authority, only the target.

`method_name` writes a method back out, including one the type does not name.

```quiver
%http.method_name POST                //= "POST"
%http.method_name Other["PROPFIND"]   //= "PROPFIND"
```

## Parsing a response

`parse_response` is the mirror of `parse_request`, and a harder job — a server frames its
answer however it likes, and HTTP/1.1 servers chunk routinely. It takes an `eof` flag saying
the connection has closed, and answers `Incomplete`, `Bad[reason]`, or the response plus the
leftover bytes.

With `Content-Length`, the body is exactly that many bytes and the rest is the next response
on the connection:

```quiver
"HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello!!" ~> .0 ~> %http.parse_response [~, []] ~> =[resp, rest]
resp.status      //= 200
Str[resp.body]   //= "hello"
Str[rest]        //= "!!"
```

Chunked bodies are decoded and rejoined, chunk extensions ignored and the trailer section
skipped:

```quiver
"HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n5;x=1\r\nhello\r\n6\r\n world\r\n0\r\nx-t: 1\r\n\r\nNEXT"
~> .0 ~> %http.parse_response [~, []] ~> =[resp, rest]
Str[resp.body]   //= "hello world"
Str[rest]        //= "NEXT"
```

A status that carries no body — 204, 304, and the 1xx range — has none however it is framed:

```quiver
"HTTP/1.1 204 No Content\r\n\r\n" ~> .0 ~> %http.parse_response [~, []] ~> =[resp, rest]
resp.status   //= 204
resp.body     //= <>
rest          //= <>
```

### Why `eof` is an argument

A response with neither framing header runs until the connection closes, and no amount of
buffer inspection can tell that from a body still arriving. So it is `Incomplete` until the
caller — the only party that knows — says the peer hung up.

```quiver
"HTTP/1.1 200 OK\r\n\r\nbody bytes" ~> .0 ~> %http.parse_response [~, []]   //= Incomplete
```

```quiver
"HTTP/1.1 200 OK\r\n\r\nbody bytes" ~> .0 ~> %http.parse_response [~, Ok] ~> =[resp, _]
Str[resp.body]   //= "body bytes"
```

A short read is `Incomplete` too, never a truncated body — the difference between "not yet"
and "not ever" is never guessed at.

```quiver
"HTTP/1.1 200 OK\r\ncontent-length: 10\r\n\r\nshort" ~> .0 ~> %http.parse_response [~, []]   //= Incomplete
"HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n5\r\nhel" ~> .0 ~> %http.parse_response [~, []]   //= Incomplete
```

What is malformed, though, is reported:

```quiver
"XYZ\r\n\r\n" ~> .0 ~> %http.parse_response [~, []]
//= Bad[reason: "malformed status line"]
"HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\nzz\r\n" ~> .0 ~> %http.parse_response [~, []]
//= Bad[reason: "malformed chunk size"]
"HTTP/1.1 200 OK\r\ncontent-length: nope\r\n\r\n" ~> .0 ~> %http.parse_response [~, []]
//= Bad[reason: "invalid content-length"]
```

`parse_response_with` states the caps, which default to 8 KiB of head and 8 MiB of body.

### Incremental in earnest

Being incremental means every prefix of a response is `Incomplete` and only the whole thing
parses — feeding the buffer one byte at a time must never produce a `Bad`, and must produce
the response exactly once.

```quiver
full = "HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n" ~> .0
n = %bin.length full
step = #[(i): 'int, (seen): 'int] {
  | %num.gt? [$i, n] => $seen
  | {
    r = %http.parse_response [%bin.slice [full, 0, $i], []]
    seen2 = {
      | r ~> =[Response(body: b), _]; Str[b] ~> ="hello" => %num.add [$seen, 1]
      | r ~> =Incomplete => $seen
      | -1000
    }
    ^ [%num.add [$i, 1], seen2]
  }
}
step [0, 0]   //= 1
```

## Handlers are pure functions

Since nothing in this module performs I/O, a handler is an ordinary function from a request to
a response. It needs no server to run: build a request with the same parser the server uses,
call it, and match the answer.

```quiver
handler = #'%http {
  [$method, $path] ~> {
    | =[GET, Cons["greet", Cons[name, Nil]]] => Response[status: 200, headers: Nil, body: name.0]
    | Response[status: 404, headers: Nil, body: <>]
  }
}

"GET /greet/ada HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[req, _]
req ~> handler ~ ~> =Response(status: s, body: b)
s        //= 200
Str[b]   //= "ada"
```

```quiver
handler = #'%http {
  [$method, $path] ~> {
    | =[GET, Cons["greet", Cons[name, Nil]]] => Response[status: 200, headers: Nil, body: name.0]
    | Response[status: 404, headers: Nil, body: <>]
  }
}

"GET /nope HTTP/1.1\r\n\r\n" ~> .0 ~> %http.parse_request ~ ~> =[req, _]
req ~> handler ~ ~> .status   //= 404
```
