use crate::common::*;

#[test]
fn test_new() {
    quiver().evaluate("5 ~> %bin.new ~").expect("<0000000000>");
    quiver().evaluate("0 ~> %bin.new ~").expect("<>");
}

#[test]
fn test_length() {
    quiver()
        .evaluate("5 ~> %bin.new ~ ~> %bin.length ~")
        .expect("5");
    quiver()
        .evaluate("<68656c6c6f> ~> %bin.length ~")
        .expect("5");
    quiver()
        .evaluate("0 ~> %bin.new ~ ~> %bin.length ~")
        .expect("0");
}

#[test]
fn test_concat() {
    quiver()
        .evaluate("[<68656c>, <6c6f>] ~> %bin.concat ~")
        .expect("<68656c6c6f>");

    quiver()
        .evaluate(
            r#"
            a = 2 ~> %bin.new ~;
            b = 3 ~> %bin.new ~;
            [a, b] ~> %bin.concat ~
            "#,
        )
        .expect("<0000000000>");

    quiver()
        .evaluate("[<>, <ff>] ~> %bin.concat ~")
        .expect("<ff>");
}

#[test]
fn test_and() {
    quiver()
        .evaluate("[<ff>, <f0>] ~> %bin.and ~")
        .expect("<f0>");
    quiver()
        .evaluate("[<aa>, <cc>] ~> %bin.and ~")
        .expect("<88>");
}

#[test]
fn test_or() {
    quiver()
        .evaluate("[<f0>, <0f>] ~> %bin.or ~")
        .expect("<ff>");
    quiver()
        .evaluate("[<aa>, <55>] ~> %bin.or ~")
        .expect("<ff>");
}

#[test]
fn test_xor() {
    quiver()
        .evaluate("[<ff>, <f0>] ~> %bin.xor ~")
        .expect("<0f>");
    quiver()
        .evaluate("[<aa>, <aa>] ~> %bin.xor ~")
        .expect("<00>");
}

#[test]
fn test_not() {
    quiver().evaluate("<00> ~> %bin.not ~").expect("<ff>");
    quiver().evaluate("<ff> ~> %bin.not ~").expect("<00>");
    quiver().evaluate("<f0> ~> %bin.not ~").expect("<0f>");
}

#[test]
fn test_shift() {
    // Test left shift (positive)
    quiver()
        .evaluate("[<01>, 1] ~> %bin.shift ~")
        .expect("<02>");
    quiver()
        .evaluate("[<0f>, 4] ~> %bin.shift ~")
        .expect("<f0>");

    // Test right shift (negative)
    quiver()
        .evaluate("[<02>, -1] ~> %bin.shift ~")
        .expect("<01>");
    quiver()
        .evaluate("[<f0>, -4] ~> %bin.shift ~")
        .expect("<0f>");

    // Test zero shift
    quiver()
        .evaluate("[<ff>, 0] ~> %bin.shift ~")
        .expect("<ff>");
}

#[test]
fn test_get_byte() {
    quiver()
        .evaluate("[<68656c6c6f>, 0] ~> %bin.get_byte ~")
        .expect("104"); // <68> = 104
    quiver()
        .evaluate("[<68656c6c6f>, 1] ~> %bin.get_byte ~")
        .expect("101"); // <65> = 101
    quiver()
        .evaluate("[<68656c6c6f>, 4] ~> %bin.get_byte ~")
        .expect("111"); // <6f> = 111
}

#[test]
fn test_get_bit() {
    quiver().evaluate("[<80>, 0] ~> %bin.get_bit ~").expect("1"); // MSB of <80> = 10000000
    quiver().evaluate("[<80>, 7] ~> %bin.get_bit ~").expect("0"); // LSB of <80>
    quiver().evaluate("[<ff>, 3] ~> %bin.get_bit ~").expect("1"); // Bit 3 of <FF>

    // Test bit across byte boundary
    quiver()
        .evaluate("[<ff00>, 8] ~> %bin.get_bit ~")
        .expect("0"); // First bit of second byte (<00>)
}

#[test]
fn test_set_byte() {
    quiver()
        .evaluate("[<00000000>, 0, 255] ~> %bin.set_byte ~")
        .expect("<ff000000>");
    quiver()
        .evaluate("[<00000000>, 2, 170] ~> %bin.set_byte ~")
        .expect("<0000aa00>");

    quiver()
        .evaluate(
            r#"
            b = 3 ~> %bin.new ~;
            [b, 1, 255] ~> %bin.set_byte ~
            "#,
        )
        .expect("<00ff00>");
}

#[test]
fn test_set_bit() {
    quiver()
        .evaluate("[<00>, 0, 1] ~> %bin.set_bit ~")
        .expect("<80>"); // Set MSB
    quiver()
        .evaluate("[<00>, 7, 1] ~> %bin.set_bit ~")
        .expect("<01>"); // Set LSB
    quiver()
        .evaluate("[<ff>, 0, 0] ~> %bin.set_bit ~")
        .expect("<7f>"); // Clear MSB

    // Test bit across byte boundary
    quiver()
        .evaluate("[<0000>, 8, 1] ~> %bin.set_bit ~")
        .expect("<0080>"); // Set first bit of second byte
}

#[test]
fn test_slice() {
    quiver()
        .evaluate("[<68656c6c6f>, 0, 3] ~> %bin.slice ~")
        .expect("<68656c>"); // "hel"
    quiver()
        .evaluate("[<68656c6c6f>, 2, 5] ~> %bin.slice ~")
        .expect("<6c6c6f>"); // "llo"
    quiver()
        .evaluate("[<68656c6c6f>, 1, 4] ~> %bin.slice ~")
        .expect("<656c6c>"); // "ell"

    // Test empty slice
    quiver()
        .evaluate("[<68656c6c6f>, 2, 2] ~> %bin.slice ~")
        .expect("<>");

    // Test full slice
    quiver()
        .evaluate("[<68656c6c6f>, 0, 5] ~> %bin.slice ~")
        .expect("<68656c6c6f>");
}

#[test]
fn test_chained_operations() {
    // Test combining multiple operations
    quiver()
        .evaluate(
            r#"
            a = 2 ~> %bin.new ~;
            b = [a, 0, 255] ~> %bin.set_byte ~;
            c = [b, 1, 170] ~> %bin.set_byte ~;
            [<ff00>, c] ~> %bin.concat ~
            "#,
        )
        .expect("<ff00ffaa>");

    // Test bitwise operations chain
    quiver()
        .evaluate(
            r#"
            a = <aa>;
            b = <ff>;
            [a, b] ~> %bin.and ~ ~> %bin.not ~
            "#,
        )
        .expect("<55>");
}

#[test]
fn test_bit_manipulation_pattern() {
    // Simulate HAMT-like bit operations
    quiver()
        .evaluate(
            r#"
            bitmap = 1 ~> %bin.new ~;
            step1 = [bitmap, 0, 1] ~> %bin.set_bit ~;
            step2 = [step1, 3, 1] ~> %bin.set_bit ~;
            step3 = [step2, 7, 1] ~> %bin.set_bit ~;
            step3
            "#,
        )
        .expect("<91>"); // 10010001 in binary
}

#[test]
fn test_append() {
    // Append single byte
    quiver()
        .evaluate("[<68656c>, 108, 1] ~> %bin.append ~")
        .expect("<68656c6c>");

    // Append multi-byte value
    quiver()
        .evaluate("[<>, 1751477356, 4] ~> %bin.append ~")
        .expect("<68656c6c>");

    // Build string by appending characters (UTF-8)
    quiver()
        .evaluate(
            r#"
            step1 = [<>, 65, 1] ~> %bin.append ~;
            step2 = [step1, 50089, 2] ~> %bin.append ~;
            step3 = [step2, 14844588, 3] ~> %bin.append ~;
            step3
            "#,
        )
        .expect("<41c3a9e282ac>"); // "Aé€"
}

#[test]
fn test_index() {
    // 'hello' = 68 65 6c 6c 6f; find 'l' (<6c>) and 'o' (<6f>).
    quiver()
        .evaluate("[<68656c6c6f>, 108, 0] ~> %bin.index ~")
        .expect("2");
    // Search respects the offset: the second 'l' is at index 3.
    quiver()
        .evaluate("[<68656c6c6f>, 108, 3] ~> %bin.index ~")
        .expect("3");
    quiver()
        .evaluate("[<68656c6c6f>, 111, 0] ~> %bin.index ~")
        .expect("4");
    // Absent byte yields nil.
    quiver()
        .evaluate("[<68656c6c6f>, 122, 0] ~> %bin.index ~")
        .expect("[]");
    // Offset past the end yields nil.
    quiver()
        .evaluate("[<68656c6c6f>, 104, 5] ~> %bin.index ~")
        .expect("[]");
}

#[test]
fn test_index_across_concat() {
    // Concatenation builds a rope; search must cross the boundary. '6162' ++ '0a63' -> ab\nc.
    quiver()
        .evaluate("[<6162>, <0a63>] ~> %bin.concat ~ ~> [~, 10, 0] ~> %bin.index ~")
        .expect("2");
}

#[test]
fn test_base64_rfc4648_vectors() {
    quiver()
        .evaluate(
            r#"["" ~> .0 ~> %bin.to_base64 ~, "f" ~> .0 ~> %bin.to_base64 ~, "fo" ~> .0 ~> %bin.to_base64 ~, "foo" ~> .0 ~> %bin.to_base64 ~, "foob" ~> .0 ~> %bin.to_base64 ~, "fooba" ~> .0 ~> %bin.to_base64 ~, "foobar" ~> .0 ~> %bin.to_base64 ~]"#,
        )
        .expect(r#"["", "Zg==", "Zm8=", "Zm9v", "Zm9vYg==", "Zm9vYmE=", "Zm9vYmFy"]"#);
}

#[test]
fn test_base64_decode_round_trip_and_rejects() {
    quiver()
        .evaluate(
            r#"["Zm9vYmFy" ~> %bin.from_base64 ~ ~> Str[~], "Zg==" ~> %bin.from_base64 ~ ~> Str[~], "Zm9vYmE=" ~> %bin.from_base64 ~ ~> Str[~]]"#,
        )
        .expect(r#"["foobar", "f", "fooba"]"#);
    // Malformed: bad characters, bad length, and padding before the final quantum.
    quiver()
        .evaluate(r#"{ "ba!d" ~> %bin.from_base64 ~ | Rejected }"#)
        .expect("Rejected");
    quiver()
        .evaluate(r#"{ "abcde" ~> %bin.from_base64 ~ | Rejected }"#)
        .expect("Rejected");
    quiver()
        .evaluate(r#"{ "Zg==Zm9v" ~> %bin.from_base64 ~ | Rejected }"#)
        .expect("Rejected");
}
