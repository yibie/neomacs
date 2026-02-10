//! Status line types and rendering for the Rust layout engine.
//!
//! Handles mode-line, header-line, and tab-line: type definitions,
//! face run parsing, and rendering into FrameGlyphBuffer.

use crate::core::frame_glyphs::FrameGlyphBuffer;
use crate::core::types::Color;
use super::unicode::decode_utf8;
use super::types::*;
use super::emacs_ffi::*;
use super::engine::LayoutEngine;

/// Which kind of status line to render.
pub(crate) enum StatusLineKind {
    ModeLine,
    HeaderLine,
    TabLine,
}

/// A face run within an overlay/display string: byte offset + fg/bg colors.
pub(crate) struct OverlayFaceRun {
    pub byte_offset: u16,
    pub fg: u32,
    pub bg: u32,
}

/// Parse face runs appended after text in a buffer.
/// Runs are stored as 10-byte records: u16 byte_offset + u32 fg + u32 bg.
pub(crate) fn parse_overlay_face_runs(buf: &[u8], text_len: usize, nruns: i32) -> Vec<OverlayFaceRun> {
    let mut runs = Vec::with_capacity(nruns as usize);
    let runs_start = text_len;
    for ri in 0..nruns as usize {
        let off = runs_start + ri * 10;
        if off + 10 <= buf.len() {
            let byte_offset = u16::from_ne_bytes([buf[off], buf[off + 1]]);
            let fg = u32::from_ne_bytes([buf[off + 2], buf[off + 3], buf[off + 4], buf[off + 5]]);
            let bg = u32::from_ne_bytes([buf[off + 6], buf[off + 7], buf[off + 8], buf[off + 9]]);
            runs.push(OverlayFaceRun { byte_offset, fg, bg });
        }
    }
    runs
}

/// Apply the face run covering the current byte index.
/// Returns the updated current_run index.
pub(crate) fn apply_overlay_face_run(
    runs: &[OverlayFaceRun],
    byte_idx: usize,
    current_run: usize,
    frame_glyphs: &mut FrameGlyphBuffer,
) -> usize {
    let mut cr = current_run;
    // Advance to the correct run
    while cr + 1 < runs.len() && byte_idx >= runs[cr + 1].byte_offset as usize {
        cr += 1;
    }
    if byte_idx >= runs[cr].byte_offset as usize {
        let run = &runs[cr];
        if run.fg != 0 || run.bg != 0 {
            let rfg = Color::from_pixel(run.fg);
            let rbg = Color::from_pixel(run.bg);
            frame_glyphs.set_face(0, rfg, Some(rbg), false, false, 0, None, 0, None, 0, None);
        }
        // Pre-advance if next run starts at next byte
        if cr + 1 < runs.len() && byte_idx + 1 >= runs[cr + 1].byte_offset as usize {
            cr += 1;
        }
    }
    cr
}

impl LayoutEngine {
    /// Render a status line (mode-line, header-line, or tab-line).
    pub(crate) unsafe fn render_status_line(
        &mut self,
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        char_w: f32,
        ascent: f32,
        wp: &WindowParamsFFI,
        frame: EmacsFrame,
        frame_glyphs: &mut FrameGlyphBuffer,
        kind: StatusLineKind,
    ) {
        let mut line_face = FaceDataFFI::default();
        let buf_size = 1024usize;
        let mut line_buf = vec![0u8; buf_size];

        let bytes = match kind {
            StatusLineKind::TabLine => neomacs_layout_tab_line_text(
                wp.window_ptr,
                std::ptr::null_mut(),
                line_buf.as_mut_ptr(),
                buf_size as i64,
                &mut line_face,
            ),
            StatusLineKind::HeaderLine => neomacs_layout_header_line_text(
                wp.window_ptr,
                std::ptr::null_mut(),
                line_buf.as_mut_ptr(),
                buf_size as i64,
                &mut line_face,
            ),
            StatusLineKind::ModeLine => neomacs_layout_mode_line_text(
                wp.window_ptr,
                std::ptr::null_mut(),
                line_buf.as_mut_ptr(),
                buf_size as i64,
                &mut line_face,
            ),
        };

        // Apply face
        self.apply_face(&line_face, frame, frame_glyphs);
        let bg = Color::from_pixel(line_face.bg);
        let default_fg = Color::from_pixel(line_face.fg);

        // Draw background
        Self::add_stretch_for_face(&line_face, frame_glyphs, x, y, width, height, bg, line_face.face_id, true);

        if bytes <= 0 {
            return;
        }

        // Extract text length and face run count from packed return value
        let text_len = (bytes & 0xFFFFFFFF) as usize;
        let nruns = (bytes >> 32) as usize;

        let text = &line_buf[..text_len];

        // Parse face runs: each run is 10 bytes (u16 byte_offset, u32 fg, u32 bg)
        // stored after text data
        struct FaceRun {
            byte_offset: u16,
            fg: u32,
            bg: u32,
        }
        let mut face_runs: Vec<FaceRun> = Vec::with_capacity(nruns);
        if nruns > 0 {
            let runs_start = text_len;
            for i in 0..nruns {
                let off = runs_start + i * 10;
                if off + 10 <= line_buf.len() {
                    let byte_offset = u16::from_ne_bytes([
                        line_buf[off], line_buf[off + 1],
                    ]);
                    let fg = u32::from_ne_bytes([
                        line_buf[off + 2], line_buf[off + 3],
                        line_buf[off + 4], line_buf[off + 5],
                    ]);
                    let bg_val = u32::from_ne_bytes([
                        line_buf[off + 6], line_buf[off + 7],
                        line_buf[off + 8], line_buf[off + 9],
                    ]);
                    face_runs.push(FaceRun {
                        byte_offset,
                        fg,
                        bg: bg_val,
                    });
                }
            }
        }

        // Render text with face runs
        let mut sl_x_offset: f32 = 0.0;
        let mut byte_idx = 0usize;
        let mut current_run = 0usize;

        while byte_idx < text.len() && sl_x_offset < width {
            // Check if we need to switch face for this byte position
            if current_run < face_runs.len() {
                let next_run = if current_run + 1 < face_runs.len() {
                    current_run + 1
                } else {
                    face_runs.len()
                };
                // Move to the correct run for this byte position
                while next_run < face_runs.len()
                    && byte_idx >= face_runs[next_run].byte_offset as usize
                {
                    // Already past this run, skip
                    break;
                }
                if byte_idx >= face_runs[current_run].byte_offset as usize {
                    // Check if next run starts here or we're in current run
                    if current_run + 1 < face_runs.len()
                        && byte_idx >= face_runs[current_run + 1].byte_offset as usize
                    {
                        current_run += 1;
                    }
                    let run = &face_runs[current_run];
                    if run.fg != 0 || run.bg != 0 {
                        let run_fg = Color::from_pixel(run.fg);
                        let run_bg = Color::from_pixel(run.bg);
                        frame_glyphs.set_face(
                            line_face.face_id, run_fg, Some(run_bg),
                            false, false, 0, None, 0, None, 0, None,
                        );
                    }
                }
            }

            let (ch, ch_len) = decode_utf8(&text[byte_idx..]);
            byte_idx += ch_len;

            if ch == '\n' || ch == '\r' {
                continue;
            }

            let gx = x + sl_x_offset;
            frame_glyphs.add_char(ch, gx, y, char_w, height, ascent, true);
            sl_x_offset += char_w;
        }

        // Restore default mode-line face
        frame_glyphs.set_face(
            line_face.face_id, default_fg, Some(bg),
            false, false, 0, None, 0, None, 0, None,
        );

        // Fill remaining with background
        if sl_x_offset < width {
            let gx = x + sl_x_offset;
            let remaining = width - sl_x_offset;
            Self::add_stretch_for_face(&line_face, frame_glyphs, gx, y, remaining, height, bg, line_face.face_id, true);
        }

        // Box borders are rendered by the renderer's box span detection
        // (supports both sharp and SDF rounded corners).
    }
}
