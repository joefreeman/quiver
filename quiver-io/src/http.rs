//! The native `http_request` implementation: the whole HTTP exchange as one mediated
//! effect, performed by the backend over its own sockets.
//!
//! This module owns the protocol-shaped pieces — URL parsing, request serialization,
//! response-head parsing and body framing — as pure functions of bytes, so the backend's
//! driver stays io-only and the protocol logic is testable without a socket. The exchange
//! is deliberately minimal HTTP/1.1: one request, `connection: close`, no transparent
//! decompression, and redirects are *not* followed — the mediating host may follow them
//! itself (a browser's `fetch` does), so a caller that wants uniform following does it
//! above this layer, where `%http/client` already has the loop.

use crate::effects::NativeEffect;
use crate::util::{binary_bytes, expect_tuple};
use quiver_core::builtins::{BuiltinContext, BuiltinRegistry, Completion};
use quiver_core::error::Error;
use quiver_core::value::Value;

/// http_request([method, url, headers, body]) -> [status, headers, body: \ByteStream]
/// Parks the process while the backend performs the exchange.
pub fn builtin_http_request(
    value: &Value,
    ctx: &mut BuiltinContext<NativeEffect>,
) -> Result<Completion<NativeEffect>, Error> {
    let fields = expect_tuple(value, 4)?.to_vec();
    Ok(Completion::Effect(NativeEffect::HttpRequest {
        method: binary_bytes(&fields[0], ctx)?,
        url: binary_bytes(&fields[1], ctx)?,
        headers: binary_bytes(&fields[2], ctx)?,
        body: binary_bytes(&fields[3], ctx)?,
    }))
}

/// Attach the native `http_request` implementation.
pub fn attach_http_builtins(registry: &mut BuiltinRegistry<NativeEffect>) {
    registry.attach_implementation("http_request", builtin_http_request);
}

/// An absolute `http://` or `https://` URL, reduced to what one exchange needs.
#[derive(Debug, Clone, PartialEq)]
pub struct HttpUrl {
    pub https: bool,
    /// The bare host: a name, or an IP literal without its brackets.
    pub host: String,
    pub port: u16,
    /// The request target: path plus query, `/` at minimum.
    pub target: String,
}

impl HttpUrl {
    /// The `Host` header value: the host (IPv6 re-bracketed), with the port when it is
    /// not the scheme's default.
    pub fn authority(&self) -> String {
        let host = if self.host.contains(':') {
            format!("[{}]", self.host)
        } else {
            self.host.clone()
        };
        let default = if self.https { 443 } else { 80 };
        if self.port == default {
            host
        } else {
            format!("{host}:{}", self.port)
        }
    }
}

/// Parse an absolute URL. Strict and small: `http`/`https` only, no userinfo, fragment
/// discarded. Anything else is an argument error — the caller (or `%url`) validates first.
pub fn parse_url(url: &[u8]) -> Result<HttpUrl, String> {
    let url = std::str::from_utf8(url).map_err(|_| "url is not UTF-8".to_string())?;
    let (https, rest) = if let Some(rest) = strip_prefix_ignore_case(url, "http://") {
        (false, rest)
    } else if let Some(rest) = strip_prefix_ignore_case(url, "https://") {
        (true, rest)
    } else {
        return Err(format!("not an http(s) url: {url}"));
    };
    let (authority, target) = match rest.find(['/', '?', '#']) {
        Some(index) if rest.as_bytes()[index] == b'/' => (&rest[..index], &rest[index..]),
        Some(index) => (&rest[..index], &rest[index..]),
        None => (rest, ""),
    };
    if authority.contains('@') {
        return Err("userinfo in urls is not supported".to_string());
    }
    let (host, port) = if let Some(inner) = authority.strip_prefix('[') {
        // An IPv6 literal: the closing bracket ends the host, an optional `:port` follows.
        let (host, after) = inner
            .split_once(']')
            .ok_or_else(|| format!("unterminated IPv6 literal in {url}"))?;
        let port = match after.strip_prefix(':') {
            Some(port) => Some(port),
            None if after.is_empty() => None,
            None => return Err(format!("malformed authority in {url}")),
        };
        (host, port)
    } else {
        match authority.rsplit_once(':') {
            Some((host, port)) => (host, Some(port)),
            None => (authority, None),
        }
    };
    if host.is_empty() {
        return Err(format!("no host in {url}"));
    }
    let port = match port {
        Some(port) => port
            .parse::<u16>()
            .map_err(|_| format!("invalid port in {url}"))?,
        None if https => 443,
        None => 80,
    };
    // The fragment never crosses the wire; an empty path is the root. A `?`-led target
    // keeps its implied `/`.
    let target = target.split('#').next().unwrap_or("");
    let target = match target {
        "" => "/".to_string(),
        t if t.starts_with('?') => format!("/{t}"),
        t => t.to_string(),
    };
    Ok(HttpUrl {
        https,
        host: host.to_string(),
        port,
        target,
    })
}

fn strip_prefix_ignore_case<'a>(text: &'a str, prefix: &str) -> Option<&'a str> {
    text.len()
        .checked_sub(prefix.len())
        .and_then(|_| text.get(..prefix.len()))
        .filter(|head| head.eq_ignore_ascii_case(prefix))
        .map(|_| &text[prefix.len()..])
}

/// Whether the caller's raw header block already carries `name` (case-insensitive).
fn block_has_header(block: &[u8], name: &str) -> bool {
    block.split(|&b| b == b'\n').any(|line| {
        let line = line.strip_suffix(b"\r").unwrap_or(line);
        match line.iter().position(|&b| b == b':') {
            Some(colon) => line[..colon].eq_ignore_ascii_case(name.as_bytes()),
            None => false,
        }
    })
}

/// Serialize the request. The caller's header block is passed through as-is; the headers
/// the exchange itself depends on are added only where the caller didn't: `host` (the
/// URL's authority), `content-length` (the body's), `connection: close` (this is a
/// one-exchange transport) and `accept-encoding: identity` (nothing here decompresses).
pub fn serialize_request(url: &HttpUrl, method: &[u8], headers: &[u8], body: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(headers.len() + body.len() + 128);
    out.extend_from_slice(method);
    out.push(b' ');
    out.extend_from_slice(url.target.as_bytes());
    out.extend_from_slice(b" HTTP/1.1\r\n");
    if !block_has_header(headers, "host") {
        out.extend_from_slice(format!("host: {}\r\n", url.authority()).as_bytes());
    }
    for line in headers.split(|&b| b == b'\n') {
        let line = line.strip_suffix(b"\r").unwrap_or(line);
        if !line.is_empty() {
            out.extend_from_slice(line);
            out.extend_from_slice(b"\r\n");
        }
    }
    let is_bodyless_method =
        method.eq_ignore_ascii_case(b"GET") || method.eq_ignore_ascii_case(b"HEAD");
    if !block_has_header(headers, "content-length")
        && !block_has_header(headers, "transfer-encoding")
        && (!body.is_empty() || !is_bodyless_method)
    {
        out.extend_from_slice(format!("content-length: {}\r\n", body.len()).as_bytes());
    }
    if !block_has_header(headers, "connection") {
        out.extend_from_slice(b"connection: close\r\n");
    }
    if !block_has_header(headers, "accept-encoding") {
        out.extend_from_slice(b"accept-encoding: identity\r\n");
    }
    out.extend_from_slice(b"\r\n");
    out.extend_from_slice(body);
    out
}

/// A parsed response head: the status, the raw header block (`name: value\r\n` lines, the
/// shape `%http` parses and the web host produces), and where the body bytes begin.
#[derive(Debug, PartialEq)]
pub struct ResponseHead {
    pub status: u16,
    pub headers: Vec<u8>,
    pub body_start: usize,
}

/// Parse a response head out of `bytes`: `None` while it is still incomplete, an error if
/// what has arrived already cannot be a response.
pub fn parse_response_head(bytes: &[u8]) -> Result<Option<ResponseHead>, String> {
    let Some(head_end) = find_subslice(bytes, b"\r\n\r\n") else {
        // Nothing that starts like a status line never becomes a response, however much
        // more arrives.
        if bytes.len() >= 5 && !bytes[..5].eq_ignore_ascii_case(b"HTTP/") {
            return Err("malformed response: not HTTP".to_string());
        }
        return Ok(None);
    };
    let status_end = find_subslice(bytes, b"\r\n").expect("head end implies a status line end");
    let status_line = &bytes[..status_end];
    // `HTTP/1.x <code> <reason>` — the code is the three digits after the first space.
    let mut parts = status_line.splitn(3, |&b| b == b' ');
    let version = parts.next().unwrap_or_default();
    let code = parts.next().unwrap_or_default();
    if !version.starts_with(b"HTTP/") {
        return Err("malformed response: not HTTP".to_string());
    }
    let status = std::str::from_utf8(code)
        .ok()
        .and_then(|code| code.parse::<u16>().ok())
        .ok_or_else(|| "malformed response: bad status code".to_string())?;
    Ok(Some(ResponseHead {
        status,
        headers: bytes[status_end + 2..head_end + 2].to_vec(),
        body_start: head_end + 4,
    }))
}

fn find_subslice(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    haystack
        .windows(needle.len())
        .position(|window| window == needle)
}

/// The value of the named header in a raw block, if present (case-insensitive name,
/// trimmed value).
fn header_value<'a>(block: &'a [u8], name: &str) -> Option<&'a [u8]> {
    for line in block.split(|&b| b == b'\n') {
        let line = line.strip_suffix(b"\r").unwrap_or(line);
        if let Some(colon) = line.iter().position(|&b| b == b':')
            && line[..colon].eq_ignore_ascii_case(name.as_bytes())
        {
            let mut value = &line[colon + 1..];
            while value.first() == Some(&b' ') || value.first() == Some(&b'\t') {
                value = &value[1..];
            }
            while value.last() == Some(&b' ') || value.last() == Some(&b'\t') {
                value = &value[..value.len() - 1];
            }
            return Some(value);
        }
    }
    None
}

/// How a response body is delimited, decided by the head — and then a running decoder of
/// the bytes that follow it.
#[derive(Debug)]
pub enum Framing {
    /// `content-length`: exactly this many bytes remain.
    Length(u64),
    /// `transfer-encoding: chunked`.
    Chunked(ChunkDecoder),
    /// Neither: the body runs to the end of the connection.
    Eof,
    /// No body at all (HEAD, 1xx, 204, 304) — or a delimited body fully delivered.
    Done,
}

/// The framing a response head declares for its body.
pub fn response_framing(method: &[u8], head: &ResponseHead) -> Result<Framing, String> {
    if method.eq_ignore_ascii_case(b"HEAD")
        || head.status / 100 == 1
        || head.status == 204
        || head.status == 304
    {
        return Ok(Framing::Done);
    }
    if let Some(value) = header_value(&head.headers, "transfer-encoding") {
        if value
            .split(|&b| b == b',')
            .any(|token| token.trim_ascii().eq_ignore_ascii_case(b"chunked"))
        {
            return Ok(Framing::Chunked(ChunkDecoder::new()));
        }
        return Err(format!(
            "unsupported transfer-encoding: {}",
            String::from_utf8_lossy(value)
        ));
    }
    match header_value(&head.headers, "content-length") {
        Some(value) => {
            let length = std::str::from_utf8(value)
                .ok()
                .and_then(|value| value.parse::<u64>().ok())
                .ok_or_else(|| "malformed content-length".to_string())?;
            Ok(if length == 0 {
                Framing::Done
            } else {
                Framing::Length(length)
            })
        }
        None => Ok(Framing::Eof),
    }
}

impl Framing {
    /// Decode as much of `input` as the framing allows, consuming what it takes and
    /// answering the body bytes it yields. `Done` afterwards means the body is complete
    /// (trailing bytes past a delimited body are left unconsumed and ignored).
    pub fn decode(&mut self, input: &mut Vec<u8>) -> Result<Vec<u8>, String> {
        match self {
            Framing::Done => Ok(Vec::new()),
            Framing::Eof => Ok(std::mem::take(input)),
            Framing::Length(remaining) => {
                let take = (*remaining).min(input.len() as u64) as usize;
                let out: Vec<u8> = input.drain(..take).collect();
                *remaining -= take as u64;
                if *remaining == 0 {
                    *self = Framing::Done;
                }
                Ok(out)
            }
            Framing::Chunked(decoder) => {
                let (out, complete) = decoder.decode(input)?;
                if complete {
                    *self = Framing::Done;
                }
                Ok(out)
            }
        }
    }

    /// Whether the body is fully delivered.
    pub fn is_complete(&self) -> bool {
        matches!(self, Framing::Done)
    }

    /// What the end of the connection means mid-body: completion for an EOF-delimited
    /// body, truncation for a delimited one.
    pub fn on_eof(&mut self) -> Result<(), String> {
        match self {
            Framing::Done => Ok(()),
            Framing::Eof => {
                *self = Framing::Done;
                Ok(())
            }
            Framing::Length(_) | Framing::Chunked(_) => {
                Err("connection closed mid-body".to_string())
            }
        }
    }
}

/// A `transfer-encoding: chunked` decoder: size lines in hex (extensions ignored), chunk
/// data, a blank size ending the body, trailers discarded.
#[derive(Debug)]
pub struct ChunkDecoder {
    phase: ChunkPhase,
}

#[derive(Debug)]
enum ChunkPhase {
    /// Expecting a `<hex-size>[;ext]\r\n` line.
    Size,
    /// Inside a chunk's data.
    Data {
        remaining: u64,
    },
    /// Expecting the `\r\n` that closes a chunk's data.
    DataEnd,
    /// Past the zero-size chunk: trailer lines until a blank one.
    Trailers,
    Done,
}

impl ChunkDecoder {
    fn new() -> Self {
        ChunkDecoder {
            phase: ChunkPhase::Size,
        }
    }

    fn decode(&mut self, input: &mut Vec<u8>) -> Result<(Vec<u8>, bool), String> {
        let mut out = Vec::new();
        loop {
            match &mut self.phase {
                ChunkPhase::Size => {
                    let Some(line_end) = find_subslice(input, b"\r\n") else {
                        if input.len() > 1024 {
                            return Err("malformed chunk size".to_string());
                        }
                        break;
                    };
                    let line: Vec<u8> = input.drain(..line_end + 2).collect();
                    let digits = line[..line_end]
                        .split(|&b| b == b';')
                        .next()
                        .unwrap_or_default();
                    let size = std::str::from_utf8(digits)
                        .ok()
                        .and_then(|digits| u64::from_str_radix(digits.trim(), 16).ok())
                        .ok_or_else(|| "malformed chunk size".to_string())?;
                    self.phase = if size == 0 {
                        ChunkPhase::Trailers
                    } else {
                        ChunkPhase::Data { remaining: size }
                    };
                }
                ChunkPhase::Data { remaining } => {
                    if input.is_empty() {
                        break;
                    }
                    let take = (*remaining).min(input.len() as u64) as usize;
                    out.extend(input.drain(..take));
                    *remaining -= take as u64;
                    if *remaining == 0 {
                        self.phase = ChunkPhase::DataEnd;
                    }
                }
                ChunkPhase::DataEnd => {
                    if input.len() < 2 {
                        break;
                    }
                    if &input[..2] != b"\r\n" {
                        return Err("malformed chunk: missing CRLF".to_string());
                    }
                    input.drain(..2);
                    self.phase = ChunkPhase::Size;
                }
                ChunkPhase::Trailers => {
                    let Some(line_end) = find_subslice(input, b"\r\n") else {
                        break;
                    };
                    let blank = line_end == 0;
                    input.drain(..line_end + 2);
                    if blank {
                        self.phase = ChunkPhase::Done;
                    }
                }
                ChunkPhase::Done => return Ok((out, true)),
            }
        }
        Ok((out, false))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn url(text: &str) -> HttpUrl {
        parse_url(text.as_bytes()).unwrap()
    }

    #[test]
    fn urls_parse_to_their_parts() {
        assert_eq!(
            url("http://example.com/a?x=1"),
            HttpUrl {
                https: false,
                host: "example.com".into(),
                port: 80,
                target: "/a?x=1".into()
            }
        );
        assert_eq!(url("https://example.com").target, "/");
        assert_eq!(url("https://example.com").port, 443);
        assert_eq!(url("HTTP://e.com:8080").port, 8080);
        assert_eq!(url("http://e.com?x=1").target, "/?x=1");
        assert_eq!(url("http://e.com/a#frag").target, "/a");
        assert_eq!(url("http://[::1]:8080/x").host, "::1");
        assert_eq!(url("http://[::1]:8080/x").authority(), "[::1]:8080");
        assert_eq!(url("http://e.com/").authority(), "e.com");
        assert_eq!(url("http://e.com:81/").authority(), "e.com:81");
        assert!(parse_url(b"ftp://e.com/").is_err());
        assert!(parse_url(b"not a url").is_err());
        assert!(parse_url(b"http://user@e.com/").is_err());
    }

    #[test]
    fn requests_serialize_with_the_exchange_headers_added() {
        let text = String::from_utf8(serialize_request(
            &url("http://example.com/a"),
            b"GET",
            b"x-custom: 1\r\n",
            b"",
        ))
        .unwrap();
        assert!(text.starts_with("GET /a HTTP/1.1\r\n"));
        assert!(text.contains("host: example.com\r\n"));
        assert!(text.contains("x-custom: 1\r\n"));
        assert!(text.contains("connection: close\r\n"));
        assert!(!text.contains("content-length"), "{text}");
        assert!(text.ends_with("\r\n\r\n"));

        let post = String::from_utf8(serialize_request(
            &url("http://example.com/a"),
            b"POST",
            b"",
            b"hello",
        ))
        .unwrap();
        assert!(post.contains("content-length: 5\r\n"));
        assert!(post.ends_with("\r\n\r\nhello"));

        // A caller-supplied header suppresses the added one.
        let custom_host = String::from_utf8(serialize_request(
            &url("http://example.com/a"),
            b"GET",
            b"Host: other.example\r\nConnection: keep-alive\r\n",
            b"",
        ))
        .unwrap();
        assert_eq!(custom_host.matches("ost:").count(), 1);
        assert!(!custom_host.contains("connection: close"));
    }

    #[test]
    fn response_heads_parse_incrementally() {
        assert_eq!(parse_response_head(b"HTTP/1.1 200").unwrap(), None);
        assert!(parse_response_head(b"SSH-2.0-OpenSSH").is_err());
        let head = parse_response_head(b"HTTP/1.1 404 Not Found\r\na: 1\r\nb: 2\r\n\r\nrest")
            .unwrap()
            .unwrap();
        assert_eq!(head.status, 404);
        assert_eq!(head.headers, b"a: 1\r\nb: 2\r\n");
        assert_eq!(head.body_start, 38);
        let bare = parse_response_head(b"HTTP/1.1 204 No Content\r\n\r\n")
            .unwrap()
            .unwrap();
        assert_eq!(bare.headers, b"");
    }

    fn framing_for(status: u16, headers: &[u8]) -> Framing {
        response_framing(
            b"GET",
            &ResponseHead {
                status,
                headers: headers.to_vec(),
                body_start: 0,
            },
        )
        .unwrap()
    }

    #[test]
    fn framing_follows_the_head() {
        assert!(matches!(
            framing_for(200, b"content-length: 5\r\n"),
            Framing::Length(5)
        ));
        assert!(matches!(
            framing_for(200, b"Transfer-Encoding: chunked\r\n"),
            Framing::Chunked(_)
        ));
        assert!(matches!(framing_for(200, b""), Framing::Eof));
        assert!(matches!(
            framing_for(200, b"content-length: 0\r\n"),
            Framing::Done
        ));
        assert!(matches!(framing_for(204, b""), Framing::Done));
        assert!(matches!(framing_for(304, b""), Framing::Done));
        let head = ResponseHead {
            status: 200,
            headers: b"content-length: 5\r\n".to_vec(),
            body_start: 0,
        };
        assert!(matches!(
            response_framing(b"HEAD", &head).unwrap(),
            Framing::Done
        ));
    }

    #[test]
    fn length_framing_counts_down() {
        let mut framing = framing_for(200, b"content-length: 5\r\n");
        let mut input = b"hel".to_vec();
        assert_eq!(framing.decode(&mut input).unwrap(), b"hel");
        assert!(!framing.is_complete());
        let mut input = b"lo and more".to_vec();
        assert_eq!(framing.decode(&mut input).unwrap(), b"lo");
        assert!(framing.is_complete());
        assert!(framing.on_eof().is_ok());
    }

    #[test]
    fn eof_framing_passes_bytes_until_the_end() {
        let mut framing = framing_for(200, b"");
        let mut input = b"abc".to_vec();
        assert_eq!(framing.decode(&mut input).unwrap(), b"abc");
        assert!(!framing.is_complete());
        assert!(framing.on_eof().is_ok());
        assert!(framing.is_complete());
    }

    #[test]
    fn truncation_is_an_error_not_an_end() {
        let mut framing = framing_for(200, b"content-length: 5\r\n");
        framing.decode(&mut b"ab".to_vec()).unwrap();
        assert!(framing.on_eof().is_err());
    }

    #[test]
    fn chunked_bodies_decode_across_arbitrary_splits() {
        let wire = b"5\r\nhello\r\n6\r\n world\r\n0\r\nx-trailer: 1\r\n\r\n";
        // Feed byte by byte: every split point must assemble identically.
        let mut framing = framing_for(200, b"Transfer-Encoding: chunked\r\n");
        let mut input = Vec::new();
        let mut body = Vec::new();
        for &byte in wire.iter() {
            input.push(byte);
            body.extend(framing.decode(&mut input).unwrap());
        }
        assert_eq!(body, b"hello world");
        assert!(framing.is_complete());
    }

    #[test]
    fn chunk_extensions_and_bare_terminators_decode() {
        let mut framing = framing_for(200, b"transfer-encoding: chunked\r\n");
        let mut input = b"3;ext=1\r\nabc\r\n0\r\n\r\ntrailing-garbage".to_vec();
        assert_eq!(framing.decode(&mut input).unwrap(), b"abc");
        assert!(framing.is_complete());
    }

    #[test]
    fn malformed_chunks_are_errors() {
        let mut framing = framing_for(200, b"transfer-encoding: chunked\r\n");
        assert!(framing.decode(&mut b"zz\r\n".to_vec()).is_err());
        let mut framing = framing_for(200, b"transfer-encoding: chunked\r\n");
        assert!(framing.decode(&mut b"3\r\nabcXY".to_vec()).is_err());
    }
}
