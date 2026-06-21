//! Conversions between Quiver's byte-based [`SourceSpan`] and LSP's UTF-16
//! line/character positions.
//!
//! LSP positions are 0-based and count UTF-16 code units within a line, so we convert from
//! the byte `offset` (the source of truth) against the document text rather than relying on
//! `SourceSpan`'s byte/char `column`.

use crate::documents::LineIndex;
use quiver_compiler::parser::SourceSpan;
use tower_lsp::lsp_types::{Position, Range};

pub fn offset_to_position(text: &str, index: &LineIndex, offset: usize) -> Position {
    let offset = offset.min(text.len());
    let line = index.line_at(offset);
    let line_start = index.line_start(line);
    let character: u32 = text[line_start..offset]
        .chars()
        .map(|c| c.len_utf16() as u32)
        .sum();
    Position {
        line: line as u32,
        character,
    }
}

pub fn position_to_offset(text: &str, index: &LineIndex, pos: Position) -> usize {
    let line_start = index.line_start(pos.line as usize);
    let mut offset = line_start;
    let mut utf16 = 0u32;
    for c in text[line_start..].chars() {
        if utf16 >= pos.character || c == '\n' {
            break;
        }
        utf16 += c.len_utf16() as u32;
        offset += c.len_utf8();
    }
    offset
}

pub fn span_to_range(text: &str, index: &LineIndex, span: SourceSpan) -> Range {
    // Ensure a non-empty range so the diagnostic/highlight is visible.
    let end_offset = span.offset + span.length.max(1);
    Range {
        start: offset_to_position(text, index, span.offset),
        end: offset_to_position(text, index, end_offset),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offset_and_position_round_trip() {
        let text = "ab\ncde\n";
        let index = LineIndex::new(text);
        // 'd' is at byte offset 4 → line 1, char 1.
        let pos = offset_to_position(text, &index, 4);
        assert_eq!(pos, Position::new(1, 1));
        assert_eq!(position_to_offset(text, &index, pos), 4);
    }

    #[test]
    fn position_columns_count_utf16_units() {
        // "é" is 2 UTF-8 bytes but 1 UTF-16 unit; "𝄞" is 4 bytes, 2 UTF-16 units.
        let text = "é𝄞x";
        let index = LineIndex::new(text);
        let x_offset = text.find('x').unwrap();
        let pos = offset_to_position(text, &index, x_offset);
        assert_eq!(pos, Position::new(0, 3)); // 1 (é) + 2 (𝄞)
        assert_eq!(position_to_offset(text, &index, pos), x_offset);
    }
}
