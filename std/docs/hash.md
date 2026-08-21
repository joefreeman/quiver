# %hash

Cryptographic hashing: SHA-256 and HMAC-SHA256, plus SHA-1 for legacy protocol
compatibility. Every function takes bytes and answers bytes — a digest is a `'bin`, not text —
so `%bin.to_hex` is what turns one into the form a person reads.

The module is pure Quiver over the integer and binary builtins, with no host capability
involved: hashing is a computation, so it is available wherever the language is, and is
deterministic.

```quiver
"abc" ~> .0 ~> %hash.sha256 ~ ~> %bin.length ~   //= 32
```

## SHA-256

`sha256` answers the 32-byte FIPS 180-4 digest of the bytes it is given. The three examples
below are the standard vectors: the empty message, `"abc"`, and a 56-byte message that
spills into a second 64-byte block.

```quiver
"" ~> .0 ~> %hash.sha256 ~ ~> %bin.to_hex ~
//= "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
```

```quiver
"abc" ~> .0 ~> %hash.sha256 ~ ~> %bin.to_hex ~
//= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
```

```quiver
"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" ~> .0 ~> %hash.sha256 ~ ~> %bin.to_hex ~
//= "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
```

The message is bytes, not characters, so anything with a byte representation can be hashed —
a binary literal is hashed exactly as the string that carries the same bytes.

```quiver
a = %hash.sha256 <616263>
b = "abc" ~> .0 ~> %hash.sha256 ~
a ~> =&b                      //= Ok
```

The digest is the whole message's, so a one-bit change to the input relates the two outputs
not at all:

```quiver
a = "abc" ~> .0 ~> %hash.sha256 ~
b = "abd" ~> .0 ~> %hash.sha256 ~
a ~> =&b                      //= []
%bin.length a                 //= 32
%bin.length b                 //= 32
```

## HMAC-SHA256

`hmac_sha256` is RFC 2104's keyed hash over SHA-256 — the signing primitive `%http/session`
uses for cookies. It takes `[key, msg]`, both binaries, and answers 32 bytes.

RFC 4231's first case, a 20-byte key of `<0b>` repeated:

```quiver
[<0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b>, "Hi There" ~> .0] ~> %hash.hmac_sha256 ~ ~> %bin.to_hex ~
//= "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
```

A key shorter than the 64-byte block is zero-padded up to it, which is why a four-byte key
needs no special handling by the caller:

```quiver
["Jefe" ~> .0, "what do ya want for nothing?" ~> .0] ~> %hash.hmac_sha256 ~ ~> %bin.to_hex ~
//= "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
```

A key *longer* than the block is hashed first and the digest used in its place. RFC 4231's
sixth case uses 131 bytes of `<aa>`:

```quiver
key = %range.to 131 ~> %range.iter ~ ~> %iter.fold [~, init: <>, f: #{ %bin.append [$0, 170, 1] }]
%bin.length key               //= 131
[key, "Test Using Larger Than Block-Size Key - Hash Key First" ~> .0] ~> %hash.hmac_sha256 ~ ~> %bin.to_hex ~
//= "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
```

The key is what makes the tag unforgeable, so changing it changes everything — the message
alone does not determine the output:

```quiver
msg = "Hi There" ~> .0
a = %hash.hmac_sha256 [<0b0b0b0b>, msg]
b = %hash.hmac_sha256 [<0b0b0b0c>, msg]
a ~> =&b                      //= []
```

Verifying a tag is comparing it with a freshly computed one, which is an ordinary pin:

```quiver
tag = %hash.hmac_sha256 ["secret" ~> .0, "payload" ~> .0]
%hash.hmac_sha256 ["secret" ~> .0, "payload" ~> .0] ~> =&tag   //= Ok
%hash.hmac_sha256 ["secret" ~> .0, "payl0ad" ~> .0] ~> =&tag   //= []
```

## SHA-1

`sha1` answers the 20-byte FIPS 180-4 digest. It is here for protocol compatibility — the
WebSocket handshake specifies it — and not for new designs: SHA-1 is collision-broken and
must not be relied on for integrity or signatures. Use `sha256`.

```quiver
"" ~> .0 ~> %hash.sha1 ~ ~> %bin.to_hex ~
//= "da39a3ee5e6b4b0d3255bfef95601890afd80709"
```

```quiver
"abc" ~> .0 ~> %hash.sha1 ~ ~> %bin.to_hex ~
//= "a9993e364706816aba3e25717850c26c9cd0d89d"
```

```quiver
"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" ~> .0 ~> %hash.sha1 ~ ~> %bin.to_hex ~
//= "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
```

It shares SHA-256's padding and differs in state width — five 32-bit words rather than
eight, which is the whole of the 20 bytes against 32:

```quiver
"abc" ~> .0 ~> %hash.sha1 ~ ~> %bin.length ~     //= 20
"abc" ~> .0 ~> %hash.sha256 ~ ~> %bin.length ~   //= 32
```

The WebSocket handshake is the canonical use: the client's key, concatenated with a fixed
GUID, hashed, and base64-encoded into the `Sec-WebSocket-Accept` header.

```quiver
key = "dGhlIHNhbXBsZSBub25jZQ=="
guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
%bin.concat [key.0, guid.0] ~> %hash.sha1 ~ ~> %bin.to_base64 ~   //= "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
```
