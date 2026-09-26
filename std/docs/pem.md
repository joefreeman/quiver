# %pem

PEM (RFC 7468) is how certificate tooling writes DER: base64 armored between
`-----BEGIN <LABEL>-----` and `-----END <LABEL>-----` lines. The TLS builtins take DER alone,
so this module is the bridge — a certbot `fullchain.pem` or an openssl key in, the bytes
`%tls` wants out. It is text-and-bytes work with nothing the host has to provide, so it is
written in Quiver like any other library.

Real certificates run to dozens of base64 lines. The examples here use four-byte bodies in
their place; nothing in the module looks inside the DER, so a stand-in behaves exactly as a
certificate would.

```quiver
text = """
  -----BEGIN CERTIFICATE-----
  3q2+7w==
  -----END CERTIFICATE-----
  """
%pem.blocks text              //= Cons[[label: "CERTIFICATE", der: <deadbeef>], Nil]
```

## Blocks

`blocks` answers every block in the text as `[label, der]`, in file order.

```quiver
text = """
  -----BEGIN CERTIFICATE-----
  3q2+7w==
  -----END CERTIFICATE-----
  -----BEGIN PRIVATE KEY-----
  BAUG
  -----END PRIVATE KEY-----
  """
%pem.blocks text
//= Cons[[label: "CERTIFICATE", der: <deadbeef>], Cons[[label: "PRIVATE KEY", der: <040506>], Nil]]
```

A body is wrapped — at 64 columns, conventionally — and the lines are joined before being
decoded, so where the writer broke them makes no difference.

```quiver
text = """
  -----BEGIN CERTIFICATE-----
  3q2+
  7w==
  -----END CERTIFICATE-----
  """
%pem.blocks text              //= Cons[[label: "CERTIFICATE", der: <deadbeef>], Nil]
```

Anything outside a block is ignored — blank lines, comments, and the human-readable dump
`openssl x509 -text` writes above the armor.

```quiver
text = """
  subject=CN = localhost
  not a pem line

  -----BEGIN CERTIFICATE-----
  3q2+7w==
  -----END CERTIFICATE-----
  trailing note
  """
%pem.blocks text              //= Cons[[label: "CERTIFICATE", der: <deadbeef>], Nil]
```

Text with no block at all is the empty list. That is "found nothing", not a failure, and is
the same nil-free answer an empty file gives.

```quiver
%pem.blocks "just some text"  //= Nil
%pem.blocks ""                //= Nil
```

## Line endings

A file written on Windows arrives CRLF. One trailing carriage return is stripped from each
line, so the two spellings decode identically.

```quiver
crlf = "-----BEGIN CERTIFICATE-----\r\n3q2+7w==\r\n-----END CERTIFICATE-----\r\n"
%pem.blocks crlf              //= Cons[[label: "CERTIFICATE", der: <deadbeef>], Nil]
%pem.certificates crlf        //= <deadbeef>
```

## Certificates

`certificates` keeps the `CERTIFICATE` blocks and concatenates their DER — which is exactly
the form `%tls.accept`'s `cert` and `%tls.attach`'s `roots` take. A `fullchain.pem` is the
leaf followed by its issuers, and the concatenation preserves that order.

```quiver
chain = """
  -----BEGIN CERTIFICATE-----
  3q2+7w==
  -----END CERTIFICATE-----
  -----BEGIN CERTIFICATE-----
  AQID
  -----END CERTIFICATE-----
  """
%pem.certificates chain       //= <deadbeef010203>
```

Blocks with any other label are skipped, so a combined certificate-and-key file needs no
splitting first.

```quiver
combined = """
  -----BEGIN PRIVATE KEY-----
  BAUG
  -----END PRIVATE KEY-----
  -----BEGIN CERTIFICATE-----
  3q2+7w==
  -----END CERTIFICATE-----
  """
%pem.certificates combined    //= <deadbeef>
```

At least one certificate is required. Text holding none answers nil rather than the empty
binary — an empty chain is never what the caller meant, and nil ends the sequence where the
empty binary would travel on into a handshake.

```quiver
%pem.certificates "just some text"   //= []
```

```quiver
text = """
  -----BEGIN PRIVATE KEY-----
  BAUG
  -----END PRIVATE KEY-----
  """
%pem.certificates text        //= []
```

## Keys

`key` answers the first `PRIVATE KEY` block's DER, which is PKCS#8 — the form `%tls.accept`'s
`key` takes.

```quiver
text = """
  -----BEGIN PRIVATE KEY-----
  BAUG
  -----END PRIVATE KEY-----
  """
%pem.key text                 //= <040506>
%pem.key "just some text"     //= []
```

A legacy `EC PRIVATE KEY` or `RSA PRIVATE KEY` block is SEC1 or PKCS#1, not PKCS#8. Handing
its DER to TLS would fail opaquely somewhere below, so `key` deliberately does not answer one;
convert the file first, with `openssl pkcs8 -topk8 -nocrypt`. The block is still an ordinary
block — it is `key` that is selective, not the reader.

```quiver
text = """
  -----BEGIN EC PRIVATE KEY-----
  BAUG
  -----END EC PRIVATE KEY-----
  """
%pem.key text                 //= []
%pem.blocks text              //= Cons[[label: "EC PRIVATE KEY", der: <040506>], Nil]
```

## Malformed text

A block that never ends, an `END` whose label does not match its `BEGIN`, or a body that is
not base64 poisons the **whole** text: every function answers nil, rather than handing back
the blocks it managed to read first. Half a certificate file is not something to proceed on.

```quiver
unterminated = """
  -----BEGIN CERTIFICATE-----
  3q2+7w==
  """
%pem.blocks unterminated      //= []
```

```quiver
mismatched = """
  -----BEGIN CERTIFICATE-----
  3q2+7w==
  -----END PRIVATE KEY-----
  """
%pem.blocks mismatched        //= []
```

```quiver
corrupted = """
  -----BEGIN CERTIFICATE-----
  3q2+7w!=
  -----END CERTIFICATE-----
  """
%pem.blocks corrupted         //= []
```

A sound block beside a broken one does not survive it, and `certificates` and `key` inherit
the verdict:

```quiver
text = """
  -----BEGIN CERTIFICATE-----
  3q2+7w==
  -----END CERTIFICATE-----
  -----BEGIN CERTIFICATE-----
  !!!!
  -----END CERTIFICATE-----
  """
%pem.blocks text              //= []
%pem.certificates text        //= []
```

That nil short-circuits like any other, so a caller that reads a file and hands the text
straight on never has to test it:

```quiver ignore
%pem.certificates cert_text ~> =('bin & cert)
%pem.key key_text ~> =('bin & key)
%tls.accept [socket: s, cert: cert, key: key]
```

The client side is the same shape: `certificates` over a CA bundle gives the `roots` that
`%tls.attach` verifies a server against.

```quiver ignore
%pem.certificates ca_text ~> =('bin & roots)
%tls.attach [socket: s, hostname: "example.com", roots: roots]
```
