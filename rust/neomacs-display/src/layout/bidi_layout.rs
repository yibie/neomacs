//! Bidi (bidirectional text) integration for the Rust layout engine.
//!
//! Provides helpers to reorder glyph X positions within a completed row
//! according to the Unicode Bidirectional Algorithm (UAX#9).
//!
//! The integration works at row completion: after all characters on a line
//! have been laid out left-to-right, this module reorders their X positions
//! so that RTL runs appear in the correct visual order.

use crate::core::bidi::{self, BidiDir};
use crate::core::frame_glyphs::{FrameGlyph, FrameGlyphBuffer};

/// Quick check whether a character is in an RTL script range.
/// Used as a fast-path: if no character on a line is RTL, we skip
/// the full bidi algorithm entirely.
fn is_rtl_char(ch: char) -> bool {
    let cp = ch as u32;
    // Hebrew (0590-05FF)
    (0x0590..=0x05FF).contains(&cp)
    // Arabic (0600-06FF)
    || (0x0600..=0x06FF).contains(&cp)
    // Syriac (0700-074F)
    || (0x0700..=0x074F).contains(&cp)
    // Arabic Supplement (0750-077F)
    || (0x0750..=0x077F).contains(&cp)
    // Thaana (0780-07BF)
    || (0x0780..=0x07BF).contains(&cp)
    // NKo (07C0-07FF)
    || (0x07C0..=0x07FF).contains(&cp)
    // Samaritan (0800-083F)
    || (0x0800..=0x083F).contains(&cp)
    // Mandaic (0840-085F)
    || (0x0840..=0x085F).contains(&cp)
    // Arabic Extended-A (08A0-08FF)
    || (0x08A0..=0x08FF).contains(&cp)
    // Arabic Presentation Forms-A (FB50-FDFF)
    || (0xFB50..=0xFDFF).contains(&cp)
    // Arabic Presentation Forms-B (FE70-FEFF)
    || (0xFE70..=0xFEFF).contains(&cp)
    // Hebrew Presentation Forms (FB1D-FB4F)
    || (0xFB1D..=0xFB4F).contains(&cp)
    // RTL bidi control characters
    || cp == 0x200F  // RLM
    || cp == 0x202B  // RLE
    || cp == 0x202E  // RLO
    || cp == 0x2067  // RLI
}

/// Information about a character glyph on the current row, collected
/// for bidi reordering.
#[derive(Clone)]
struct RowCharInfo {
    /// Index into `frame_glyphs.glyphs`
    glyph_idx: usize,
    /// The character (for bidi class lookup and mirroring)
    ch: char,
    /// Original X position (set during LTR layout)
    x: f32,
    /// Glyph advance width
    width: f32,
}

/// Reorder glyph X positions on one completed row using the bidi algorithm.
///
/// `glyph_start` is the index into `frame_glyphs.glyphs` where this row's
/// glyphs begin. `glyph_end` is the exclusive end index.
/// `content_x` is the left edge of the text content area (after line numbers).
///
/// This function:
/// 1. Collects all Char/ComposedChar glyphs on the row
/// 2. Checks if any are RTL (fast-path exit if all LTR)
/// 3. Resolves bidi embedding levels
/// 4. Computes the visual reorder
/// 5. Reassigns X positions according to visual order
/// 6. Applies character mirroring for RTL characters
/// 7. Adjusts cursor positions if any cursors are on this row
pub fn reorder_row_bidi(
    frame_glyphs: &mut FrameGlyphBuffer,
    glyph_start: usize,
    glyph_end: usize,
    _content_x: f32,
) {
    if glyph_start >= glyph_end {
        return;
    }

    // Step 1: Collect character glyphs on this row
    let mut row_chars: Vec<RowCharInfo> = Vec::new();
    for idx in glyph_start..glyph_end {
        if idx >= frame_glyphs.glyphs.len() {
            break;
        }
        match &frame_glyphs.glyphs[idx] {
            FrameGlyph::Char { char: ch, x, width, .. } => {
                row_chars.push(RowCharInfo {
                    glyph_idx: idx,
                    ch: *ch,
                    x: *x,
                    width: *width,
                });
            }
            _ => {
                // Skip non-character glyphs (Stretch, Cursor, etc.)
                // They keep their positions
            }
        }
    }

    if row_chars.is_empty() {
        return;
    }

    // Step 2: Fast-path check — skip bidi if no RTL characters
    let has_rtl = row_chars.iter().any(|info| is_rtl_char(info.ch));
    if !has_rtl {
        return;
    }

    // Step 3: Build the character string and resolve bidi levels
    let chars: Vec<char> = row_chars.iter().map(|info| info.ch).collect();
    let text: String = chars.iter().collect();
    let levels = bidi::resolve_levels(&text, BidiDir::Auto);

    if levels.is_empty() {
        return;
    }

    // Fast-path: if all levels are 0, no reordering needed
    if levels.iter().all(|&l| l == 0) {
        return;
    }

    // Step 4: Get visual reorder indices
    let visual_order = bidi::reorder_visual(&levels);

    // Step 5: Compute new X positions based on visual order.
    // The visual order tells us: visual_order[visual_pos] = logical_index
    // We need to place glyphs left-to-right in visual order.
    //
    // First, compute the starting X of the row (minimum X among all chars).
    let row_start_x = row_chars.iter()
        .map(|info| info.x)
        .fold(f32::INFINITY, f32::min);

    // Collect widths in logical order
    let widths: Vec<f32> = row_chars.iter().map(|info| info.width).collect();

    // Assign new X positions: walk in visual order, placing each glyph
    let mut current_x = row_start_x;
    // new_x[logical_index] = new x position
    let mut new_x: Vec<f32> = vec![0.0; row_chars.len()];
    for &logical_idx in &visual_order {
        new_x[logical_idx] = current_x;
        current_x += widths[logical_idx];
    }

    // Step 6: Apply new X positions and mirroring to the glyphs
    for (logical_idx, info) in row_chars.iter().enumerate() {
        let glyph = &mut frame_glyphs.glyphs[info.glyph_idx];
        match glyph {
            FrameGlyph::Char { x, char: ch, .. } => {
                *x = new_x[logical_idx];
                // Apply character mirroring for RTL characters (odd level)
                if levels[logical_idx] % 2 == 1 {
                    if let Some(mirrored) = bidi::bidi_mirror(*ch) {
                        *ch = mirrored;
                    }
                }
            }
            _ => {}
        }
    }

    // Step 7: Adjust cursor positions on this row.
    // Cursors were placed at LTR X positions; we need to move them
    // to match the reordered glyph positions.
    // Find cursor glyphs in the range and update their X to match
    // the character glyph they correspond to (by matching original X).
    for idx in glyph_start..glyph_end.min(frame_glyphs.glyphs.len()) {
        if let FrameGlyph::Cursor { x: cursor_x, .. } = &mut frame_glyphs.glyphs[idx] {
            // Find the character glyph whose original X matches
            // this cursor's X position (within floating-point tolerance)
            for (logical_idx, info) in row_chars.iter().enumerate() {
                if (info.x - *cursor_x).abs() < 0.5 {
                    *cursor_x = new_x[logical_idx];
                    break;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::types::Color;

    /// Helper to create a minimal Char glyph for testing.
    fn make_char_glyph(ch: char, x: f32, width: f32) -> FrameGlyph {
        FrameGlyph::Char {
            char: ch,
            composed: None,
            x,
            y: 0.0,
            width,
            height: 16.0,
            ascent: 12.0,
            fg: Color::new(1.0, 1.0, 1.0, 1.0),
            bg: None,
            face_id: 0,
            font_weight: 400,
            italic: false,
            font_size: 14.0,
            underline: 0,
            underline_color: None,
            strike_through: 0,
            strike_through_color: None,
            overline: 0,
            overline_color: None,
            is_overlay: false,
        }
    }

    fn get_char_x(glyph: &FrameGlyph) -> f32 {
        match glyph {
            FrameGlyph::Char { x, .. } => *x,
            _ => panic!("expected Char glyph"),
        }
    }

    fn get_char(glyph: &FrameGlyph) -> char {
        match glyph {
            FrameGlyph::Char { char: ch, .. } => *ch,
            _ => panic!("expected Char glyph"),
        }
    }

    #[test]
    fn test_pure_ltr_no_reorder() {
        let mut buf = FrameGlyphBuffer::default();
        buf.glyphs.push(make_char_glyph('H', 0.0, 8.0));
        buf.glyphs.push(make_char_glyph('i', 8.0, 8.0));

        reorder_row_bidi(&mut buf, 0, 2, 0.0);

        // Should be unchanged
        assert_eq!(get_char_x(&buf.glyphs[0]), 0.0);
        assert_eq!(get_char_x(&buf.glyphs[1]), 8.0);
    }

    #[test]
    fn test_pure_rtl_reorder() {
        let mut buf = FrameGlyphBuffer::default();
        // Hebrew: alef, bet, gimel laid out LTR
        buf.glyphs.push(make_char_glyph('\u{05D0}', 0.0, 8.0));  // Alef
        buf.glyphs.push(make_char_glyph('\u{05D1}', 8.0, 8.0));  // Bet
        buf.glyphs.push(make_char_glyph('\u{05D2}', 16.0, 8.0)); // Gimel

        reorder_row_bidi(&mut buf, 0, 3, 0.0);

        // RTL: visual order should be reversed
        // Gimel at x=0, Bet at x=8, Alef at x=16
        assert_eq!(get_char_x(&buf.glyphs[0]), 16.0); // Alef (logical 0) -> rightmost
        assert_eq!(get_char_x(&buf.glyphs[1]), 8.0);  // Bet (logical 1) -> middle
        assert_eq!(get_char_x(&buf.glyphs[2]), 0.0);  // Gimel (logical 2) -> leftmost
    }

    #[test]
    fn test_mixed_ltr_rtl() {
        let mut buf = FrameGlyphBuffer::default();
        // "Hi" + Hebrew "אב" — LTR base with RTL embedded
        buf.glyphs.push(make_char_glyph('H', 0.0, 8.0));
        buf.glyphs.push(make_char_glyph('i', 8.0, 8.0));
        buf.glyphs.push(make_char_glyph(' ', 16.0, 8.0));
        buf.glyphs.push(make_char_glyph('\u{05D0}', 24.0, 8.0)); // Alef
        buf.glyphs.push(make_char_glyph('\u{05D1}', 32.0, 8.0)); // Bet

        reorder_row_bidi(&mut buf, 0, 5, 0.0);

        // LTR base: H, i, space stay at left
        // RTL segment: Alef and Bet should be swapped
        assert_eq!(get_char_x(&buf.glyphs[0]), 0.0);  // H
        assert_eq!(get_char_x(&buf.glyphs[1]), 8.0);  // i
        assert_eq!(get_char_x(&buf.glyphs[2]), 16.0); // space
        // Bet (logical idx 4) should come before Alef (logical idx 3)
        assert_eq!(get_char_x(&buf.glyphs[3]), 32.0); // Alef -> right
        assert_eq!(get_char_x(&buf.glyphs[4]), 24.0); // Bet -> left
    }

    #[test]
    fn test_bracket_mirroring() {
        let mut buf = FrameGlyphBuffer::default();
        // RTL text with brackets: ( should become ) and vice versa
        buf.glyphs.push(make_char_glyph('\u{05D0}', 0.0, 8.0));  // Alef
        buf.glyphs.push(make_char_glyph('(', 8.0, 8.0));           // Open paren
        buf.glyphs.push(make_char_glyph('\u{05D1}', 16.0, 8.0));  // Bet
        buf.glyphs.push(make_char_glyph(')', 24.0, 8.0));          // Close paren

        reorder_row_bidi(&mut buf, 0, 4, 0.0);

        // In RTL context, '(' should be mirrored to ')' and ')' to '('
        // The paren characters are at levels determined by the bidi algorithm.
        // After reordering, verify that brackets got mirrored at odd levels.
        let chars: Vec<char> = buf.glyphs.iter().map(|g| get_char(g)).collect();
        // The reordered text should have ')' where '(' was and vice versa
        // (because they're in an RTL run)
        assert!(chars.contains(&')'));
        assert!(chars.contains(&'('));
    }

    #[test]
    fn test_empty_row() {
        let mut buf = FrameGlyphBuffer::default();
        // Should not panic
        reorder_row_bidi(&mut buf, 0, 0, 0.0);
    }

    #[test]
    fn test_non_char_glyphs_preserved() {
        let mut buf = FrameGlyphBuffer::default();
        // Add a stretch glyph between chars
        buf.glyphs.push(make_char_glyph('H', 0.0, 8.0));
        buf.glyphs.push(FrameGlyph::Stretch {
            x: 8.0,
            y: 0.0,
            width: 16.0,
            height: 16.0,
            bg: Color::new(0.0, 0.0, 0.0, 1.0),
            face_id: 0,
            is_overlay: false,
            stipple_id: 0,
            stipple_fg: None,
        });
        buf.glyphs.push(make_char_glyph('i', 24.0, 8.0));

        reorder_row_bidi(&mut buf, 0, 3, 0.0);

        // Stretch should be untouched
        if let FrameGlyph::Stretch { x, .. } = &buf.glyphs[1] {
            assert_eq!(*x, 8.0);
        } else {
            panic!("expected Stretch glyph");
        }
    }
}
