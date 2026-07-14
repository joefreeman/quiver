mod common;
use common::*;

// `%hash` (std/hash.qv): SHA-256, HMAC-SHA256, and SHA-1, pure Quiver — all
// deterministic, pinned to the standard test vectors.

#[test]
fn test_sha256_nist_vectors() {
    // FIPS 180-4 / NIST examples: empty, "abc", and a two-block message.
    quiver()
        .evaluate(r#""" ~> .0 ~> %hash.sha256 ~> %bin.to_hex"#)
        .expect(r#""e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855""#);
    quiver()
        .evaluate(r#""abc" ~> .0 ~> %hash.sha256 ~> %bin.to_hex"#)
        .expect(r#""ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad""#);
    quiver()
        .evaluate(
            r#""abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" ~> .0 ~> %hash.sha256 ~> %bin.to_hex"#,
        )
        .expect(r#""248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1""#);
}

#[test]
fn test_hmac_sha256_rfc4231_vectors() {
    // RFC 4231 test case 1: key = 20 × 0x0b, data = "Hi There".
    quiver()
        .evaluate(
            r#"[0x0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b, "Hi There" ~> .0] ~> %hash.hmac_sha256 ~> %bin.to_hex"#,
        )
        .expect(r#""b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7""#);
    // Test case 2: short key ("Jefe").
    quiver()
        .evaluate(r#"["Jefe" ~> .0, "what do ya want for nothing?" ~> .0] ~> %hash.hmac_sha256 ~> %bin.to_hex"#)
        .expect(r#""5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843""#);
    // A key longer than the block size is hashed first (RFC 4231 case 6, truncated key
    // form: 131 × 0xaa, data "Test Using Larger Than Block-Size Key - Hash Key First").
    quiver()
        .evaluate(
            r#"rep = #['bin, 'int, 'int] {
                 =[acc, b, n]
                 { | n ~> =0 => acc | [[acc, b, 1] ~> %bin.append, b, [n, 1] ~> __integer_subtract__] ~> ^ }
               };
               key = [0x, 170, 131] ~> rep;
               [key, "Test Using Larger Than Block-Size Key - Hash Key First" ~> .0] ~> %hash.hmac_sha256 ~> %bin.to_hex"#,
        )
        .expect(r#""60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54""#);
}

#[test]
fn test_bin_hex_codecs() {
    quiver()
        .evaluate(r#"0xdeadbeef ~> %bin.to_hex"#)
        .expect(r#""deadbeef""#);
    quiver()
        .evaluate(r#""DeadBEEF" ~> %bin.from_hex"#)
        .expect("0xdeadbeef");
    quiver().evaluate(r#""abc" ~> %bin.from_hex"#).expect("[]"); // odd length
    quiver().evaluate(r#""zz" ~> %bin.from_hex"#).expect("[]"); // non-hex
}

#[test]
fn test_sha1_fips_vectors() {
    // FIPS 180-4 examples: empty, "abc", and a two-block message.
    quiver()
        .evaluate(r#""" ~> .0 ~> %hash.sha1 ~> %bin.to_hex"#)
        .expect(r#""da39a3ee5e6b4b0d3255bfef95601890afd80709""#);
    quiver()
        .evaluate(r#""abc" ~> .0 ~> %hash.sha1 ~> %bin.to_hex"#)
        .expect(r#""a9993e364706816aba3e25717850c26c9cd0d89d""#);
    quiver()
        .evaluate(
            r#""abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" ~> .0 ~> %hash.sha1 ~> %bin.to_hex"#,
        )
        .expect(r#""84983e441c3bd26ebaae4aa1f95129e5e54670f1""#);
}
