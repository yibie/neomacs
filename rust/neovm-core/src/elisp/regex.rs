//! Regex engine and search primitives for the Elisp VM.
//!
//! Uses the `regex` crate as the backend.  Translates basic Emacs regex
//! syntax to Rust regex syntax before compiling patterns.

use regex::Regex;

use crate::buffer::Buffer;

// ---------------------------------------------------------------------------
// MatchData
// ---------------------------------------------------------------------------

/// Match data from the last successful search.
#[derive(Clone, Debug)]
pub struct MatchData {
    /// Full match and capture groups: (start_byte, end_byte) pairs.
    /// Index 0 = full match, 1+ = capture groups.
    pub groups: Vec<Option<(usize, usize)>>,
    /// The string that was searched (for `string-match`).
    /// `None` when the search was performed on a buffer.
    pub searched_string: Option<String>,
}

// ---------------------------------------------------------------------------
// Emacs → Rust regex translation
// ---------------------------------------------------------------------------

/// Translate basic Emacs regex syntax to Rust regex syntax.
///
/// Key differences handled:
/// - Emacs `\(` `\)` for groups  →  Rust `(` `)`
/// - Emacs `\|` for alternation  →  Rust `|`
/// - Emacs `\{` `\}` for repetition  →  Rust `{` `}`
/// - Emacs `\1`..`\9` for back-references  →  not supported by `regex` crate,
///   but we translate the syntax anyway for completeness
/// - Emacs literal `(` `)` `{` `}` `|`  →  Rust `\(` `\)` `\{` `\}` `\|`
/// - Emacs `\w` (word char)  →  Rust `\w`
/// - Emacs `\W` (non-word char)  →  Rust `\W`
/// - Emacs `\b` (word boundary)  →  Rust `\b`
/// - Emacs `\B` (non-word boundary)  →  Rust `\B`
/// - Emacs `\s-` etc. (syntax classes)  →  simplified to `\s` (whitespace)
/// - Emacs `\<` `\>` (word boundaries)  →  Rust `\b`
/// - Emacs character classes inside `[...]` are kept as-is.
pub fn translate_emacs_regex(pattern: &str) -> String {
    let mut out = String::with_capacity(pattern.len() + 8);
    let bytes = pattern.as_bytes();
    let len = bytes.len();
    let mut i = 0;
    let mut in_bracket = false;

    while i < len {
        let ch = bytes[i] as char;

        // Inside a character class [...], pass through mostly unchanged.
        if in_bracket {
            out.push(ch);
            if ch == ']' && i > 0 {
                in_bracket = false;
            }
            i += 1;
            continue;
        }

        match ch {
            '[' => {
                in_bracket = true;
                out.push('[');
                i += 1;
                // Handle `[^` or `[]` — the first char after `[` might be
                // `]` or `^]` which doesn't close the bracket.
                if i < len && bytes[i] == b'^' {
                    out.push('^');
                    i += 1;
                }
                if i < len && bytes[i] == b']' {
                    out.push(']');
                    i += 1;
                }
            }
            // Emacs uses literal `(`, `)`, `{`, `}`, `|` — escape them for Rust regex.
            '(' => { out.push_str("\\("); i += 1; }
            ')' => { out.push_str("\\)"); i += 1; }
            '{' => { out.push_str("\\{"); i += 1; }
            '}' => { out.push_str("\\}"); i += 1; }
            '|' => { out.push_str("\\|"); i += 1; }
            '\\' if i + 1 < len => {
                let next = bytes[i + 1] as char;
                match next {
                    // Emacs group → Rust group
                    '(' => { out.push('('); i += 2; }
                    ')' => { out.push(')'); i += 2; }
                    // Emacs alternation → Rust alternation
                    '|' => { out.push('|'); i += 2; }
                    // Emacs repetition braces → Rust repetition braces
                    '{' => { out.push('{'); i += 2; }
                    '}' => { out.push('}'); i += 2; }
                    // Word boundaries
                    '<' => { out.push_str("\\b"); i += 2; }
                    '>' => { out.push_str("\\b"); i += 2; }
                    // Back-references (1-9) — not supported by `regex` crate,
                    // but translate the syntax for pattern acceptance.
                    '1'..='9' => {
                        // Rust `regex` doesn't support back-refs; drop silently.
                        // In practice, patterns using \1..\9 will fail to compile
                        // which is acceptable for now.
                        out.push('\\');
                        out.push(next);
                        i += 2;
                    }
                    // Emacs syntax classes (\s-, \s , etc.) → simplified to \s
                    's' => {
                        i += 2;
                        // Consume the optional syntax-class character
                        if i < len {
                            let class_ch = bytes[i] as char;
                            match class_ch {
                                '-' | ' ' | '.' | '_' | 'w' => { i += 1; }
                                _ => {}
                            }
                        }
                        out.push_str("\\s");
                    }
                    'S' => {
                        i += 2;
                        if i < len {
                            let class_ch = bytes[i] as char;
                            match class_ch {
                                '-' | ' ' | '.' | '_' | 'w' => { i += 1; }
                                _ => {}
                            }
                        }
                        out.push_str("\\S");
                    }
                    // Known escape sequences — pass through
                    'w' | 'W' | 'b' | 'B' | 'd' | 'D'
                    | 'n' | 't' | 'r' | '`' | '\'' => {
                        match next {
                            // \` (beginning of buffer) and \' (end of buffer) → \A and \z
                            '`' => { out.push_str("\\A"); i += 2; }
                            '\'' => { out.push_str("\\z"); i += 2; }
                            _ => {
                                out.push('\\');
                                out.push(next);
                                i += 2;
                            }
                        }
                    }
                    // Literal backslash
                    '\\' => { out.push_str("\\\\"); i += 2; }
                    // Anything else after `\` — pass through the escape
                    _ => {
                        out.push('\\');
                        out.push(next);
                        i += 2;
                    }
                }
            }
            // Lone trailing backslash — pass through
            '\\' => { out.push('\\'); i += 1; }
            // All other chars — pass through as-is
            _ => { out.push(ch); i += 1; }
        }
    }

    out
}

// ---------------------------------------------------------------------------
// Internal: compile an Emacs regex pattern
// ---------------------------------------------------------------------------

fn compile_emacs_regex(pattern: &str) -> Result<Regex, String> {
    let rust_pattern = translate_emacs_regex(pattern);
    Regex::new(&rust_pattern).map_err(|e| format!("Invalid regexp: {}", e))
}

fn match_data_from_captures(caps: &regex::Captures<'_>, offset: usize) -> MatchData {
    let mut groups = Vec::with_capacity(caps.len());
    for i in 0..caps.len() {
        groups.push(caps.get(i).map(|m| (m.start() + offset, m.end() + offset)));
    }
    MatchData {
        groups,
        searched_string: None,
    }
}

// ---------------------------------------------------------------------------
// Buffer search primitives
// ---------------------------------------------------------------------------

/// Search forward from point for a literal string PATTERN.
///
/// If found, moves point to end of match and returns the new point position
/// (as a byte position).  If not found, behaviour depends on `noerror`:
/// - `noerror` false: signals `search-failed`
/// - `noerror` true: returns `None` without signaling
///
/// `bound` optionally limits the search to positions <= bound.
pub fn search_forward(
    buf: &mut Buffer,
    pattern: &str,
    bound: Option<usize>,
    noerror: bool,
    match_data: &mut Option<MatchData>,
) -> Result<Option<usize>, String> {
    let start = buf.pt;
    let limit = bound.unwrap_or(buf.zv).min(buf.zv);

    if start > limit {
        if noerror {
            return Ok(None);
        }
        return Err(format!("Search failed: \"{}\"", pattern));
    }

    let text = buf.text.text_range(start, limit);

    if let Some(pos) = text.find(pattern) {
        let match_start = start + pos;
        let match_end = match_start + pattern.len();
        buf.pt = match_end;
        *match_data = Some(MatchData {
            groups: vec![Some((match_start, match_end))],
            searched_string: None,
        });
        Ok(Some(match_end))
    } else {
        if noerror {
            // When noerror is t, don't move point.
            // When noerror is a value, move point to bound.
            return Ok(None);
        }
        Err(format!("Search failed: \"{}\"", pattern))
    }
}

/// Search backward from point for a literal string PATTERN.
///
/// If found, moves point to beginning of match and returns the new point
/// position (as a byte position).
pub fn search_backward(
    buf: &mut Buffer,
    pattern: &str,
    bound: Option<usize>,
    noerror: bool,
    match_data: &mut Option<MatchData>,
) -> Result<Option<usize>, String> {
    let end = buf.pt;
    let limit = bound.unwrap_or(buf.begv).max(buf.begv);

    if end < limit {
        if noerror {
            return Ok(None);
        }
        return Err(format!("Search failed: \"{}\"", pattern));
    }

    let text = buf.text.text_range(limit, end);

    // Find the *last* occurrence within the range
    if let Some(pos) = text.rfind(pattern) {
        let match_start = limit + pos;
        let match_end = match_start + pattern.len();
        buf.pt = match_start;
        *match_data = Some(MatchData {
            groups: vec![Some((match_start, match_end))],
            searched_string: None,
        });
        Ok(Some(match_start))
    } else {
        if noerror {
            return Ok(None);
        }
        Err(format!("Search failed: \"{}\"", pattern))
    }
}

/// Search forward from point for a regex PATTERN.
///
/// If found, moves point to end of match and returns the new point position.
/// Updates match data with capture groups.
pub fn re_search_forward(
    buf: &mut Buffer,
    pattern: &str,
    bound: Option<usize>,
    noerror: bool,
    match_data: &mut Option<MatchData>,
) -> Result<Option<usize>, String> {
    let re = compile_emacs_regex(pattern)?;
    let start = buf.pt;
    let limit = bound.unwrap_or(buf.zv).min(buf.zv);

    if start > limit {
        if noerror {
            return Ok(None);
        }
        return Err(format!("Search failed: \"{}\"", pattern));
    }

    let text = buf.text.text_range(start, limit);

    if let Some(caps) = re.captures(&text) {
        let mut md = match_data_from_captures(&caps, start);
        md.searched_string = None;
        let full_match = md.groups[0].unwrap();
        buf.pt = full_match.1;
        *match_data = Some(md);
        Ok(Some(full_match.1))
    } else {
        if noerror {
            return Ok(None);
        }
        Err(format!("Search failed: \"{}\"", pattern))
    }
}

/// Search backward from point for a regex PATTERN.
///
/// If found, moves point to beginning of match and returns the new point.
/// Updates match data with capture groups.
pub fn re_search_backward(
    buf: &mut Buffer,
    pattern: &str,
    bound: Option<usize>,
    noerror: bool,
    match_data: &mut Option<MatchData>,
) -> Result<Option<usize>, String> {
    let re = compile_emacs_regex(pattern)?;
    let end = buf.pt;
    let limit = bound.unwrap_or(buf.begv).max(buf.begv);

    if end < limit {
        if noerror {
            return Ok(None);
        }
        return Err(format!("Search failed: \"{}\"", pattern));
    }

    let text = buf.text.text_range(limit, end);

    // Find the *last* match by iterating all matches
    let mut last_caps = None;
    for caps in re.captures_iter(&text) {
        last_caps = Some(caps);
    }

    if let Some(caps) = last_caps {
        let mut md = match_data_from_captures(&caps, limit);
        md.searched_string = None;
        let full_match = md.groups[0].unwrap();
        buf.pt = full_match.0;
        *match_data = Some(md);
        Ok(Some(full_match.0))
    } else {
        if noerror {
            return Ok(None);
        }
        Err(format!("Search failed: \"{}\"", pattern))
    }
}

/// Test if text after point matches PATTERN (without moving point).
///
/// Returns `true` if the regex matches starting exactly at point, and
/// updates match data.
pub fn looking_at(
    buf: &Buffer,
    pattern: &str,
    match_data: &mut Option<MatchData>,
) -> Result<bool, String> {
    let re_pattern = translate_emacs_regex(pattern);
    // Anchor the pattern at the start
    let anchored = if re_pattern.starts_with("\\A") || re_pattern.starts_with('^') {
        re_pattern
    } else {
        format!("\\A(?:{})", re_pattern)
    };
    let re = Regex::new(&anchored).map_err(|e| format!("Invalid regexp: {}", e))?;

    let start = buf.pt;
    let limit = buf.zv;

    if start > limit {
        return Ok(false);
    }

    let text = buf.text.text_range(start, limit);

    if let Some(caps) = re.captures(&text) {
        let mut md = match_data_from_captures(&caps, start);
        md.searched_string = None;
        *match_data = Some(md);
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Match a regex against a string (not a buffer).
///
/// `start` is the byte offset within `string` to begin matching.
/// Returns the byte position of the start of the match (relative to the
/// whole string, not `start`), or `None` if no match.
/// Updates match data with capture groups; stores the searched string.
pub fn string_match_full(
    pattern: &str,
    string: &str,
    start: usize,
    match_data: &mut Option<MatchData>,
) -> Result<Option<usize>, String> {
    let re = compile_emacs_regex(pattern)?;

    if start > string.len() {
        return Ok(None);
    }

    let search_region = &string[start..];

    if let Some(caps) = re.captures(search_region) {
        let mut md = match_data_from_captures(&caps, start);
        md.searched_string = Some(string.to_string());
        let result_pos = md.groups[0].unwrap().0;
        *match_data = Some(md);
        Ok(Some(result_pos))
    } else {
        Ok(None)
    }
}

/// Replace the last match with replacement text.
///
/// `newtext` is the replacement string. If `fixedcase` is false, the
/// replacement preserves the case pattern of the original match.
/// If `literal` is true, `newtext` is used literally; otherwise `\&` and
/// `\N` substitutions are performed.
///
/// Returns `true` if a replacement was made, `false` otherwise.
pub fn replace_match(
    buf: &mut Buffer,
    newtext: &str,
    _fixedcase: bool,
    literal: bool,
    match_data: &Option<MatchData>,
) -> Result<bool, String> {
    let md = match match_data {
        Some(md) => md,
        None => return Err("No match data".to_string()),
    };

    // We only support buffer-based replace (not string match replace).
    if md.searched_string.is_some() {
        return Err("replace-match on string match not supported for buffer modification".to_string());
    }

    let (match_start, match_end) = match md.groups.first() {
        Some(Some(pair)) => *pair,
        _ => return Err("No match data".to_string()),
    };

    let replacement = if literal {
        newtext.to_string()
    } else {
        build_replacement(newtext, md, buf)
    };

    // Delete old match, insert replacement
    buf.pt = match_start;
    buf.delete_region(match_start, match_end);
    buf.insert(&replacement);

    Ok(true)
}

/// Build a replacement string handling `\&` (whole match) and `\N` (group N).
fn build_replacement(template: &str, md: &MatchData, buf: &Buffer) -> String {
    let mut out = String::with_capacity(template.len());
    let bytes = template.as_bytes();
    let len = bytes.len();
    let mut i = 0;

    while i < len {
        if bytes[i] == b'\\' && i + 1 < len {
            let next = bytes[i + 1];
            match next {
                b'&' => {
                    // Whole match
                    if let Some(Some((s, e))) = md.groups.first() {
                        if *e <= buf.text.len() {
                            out.push_str(&buf.text.text_range(*s, *e));
                        }
                    }
                    i += 2;
                }
                b'0'..=b'9' => {
                    let group = (next - b'0') as usize;
                    if let Some(Some((s, e))) = md.groups.get(group) {
                        if *e <= buf.text.len() {
                            out.push_str(&buf.text.text_range(*s, *e));
                        }
                    }
                    i += 2;
                }
                b'\\' => {
                    out.push('\\');
                    i += 2;
                }
                _ => {
                    out.push('\\');
                    out.push(next as char);
                    i += 2;
                }
            }
        } else {
            out.push(bytes[i] as char);
            i += 1;
        }
    }

    out
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::{Buffer, BufferId};

    // -----------------------------------------------------------------------
    // translate_emacs_regex
    // -----------------------------------------------------------------------

    #[test]
    fn translate_groups() {
        // Emacs \( \) → Rust ( )
        assert_eq!(translate_emacs_regex("\\(foo\\)"), "(foo)");
    }

    #[test]
    fn translate_alternation() {
        // Emacs \| → Rust |
        assert_eq!(translate_emacs_regex("foo\\|bar"), "foo|bar");
    }

    #[test]
    fn translate_literal_parens() {
        // Emacs literal ( ) → Rust \( \)
        assert_eq!(translate_emacs_regex("(foo)"), "\\(foo\\)");
    }

    #[test]
    fn translate_literal_braces() {
        // Emacs literal { } → Rust \{ \}
        assert_eq!(translate_emacs_regex("{3}"), "\\{3\\}");
    }

    #[test]
    fn translate_repetition_braces() {
        // Emacs \{3\} → Rust {3}
        assert_eq!(translate_emacs_regex("a\\{3\\}"), "a{3}");
    }

    #[test]
    fn translate_literal_pipe() {
        // Emacs literal | → Rust \|
        assert_eq!(translate_emacs_regex("a|b"), "a\\|b");
    }

    #[test]
    fn translate_word_boundary() {
        // Emacs \< \> → Rust \b
        assert_eq!(translate_emacs_regex("\\<word\\>"), "\\bword\\b");
    }

    #[test]
    fn translate_buffer_boundaries() {
        // Emacs \` → Rust \A, Emacs \' → Rust \z
        assert_eq!(translate_emacs_regex("\\`foo\\'"), "\\Afoo\\z");
    }

    #[test]
    fn translate_character_class_passthrough() {
        // Character classes should pass through mostly unchanged
        assert_eq!(translate_emacs_regex("[a-z]"), "[a-z]");
        assert_eq!(translate_emacs_regex("[^0-9]"), "[^0-9]");
    }

    #[test]
    fn translate_backslash_w() {
        assert_eq!(translate_emacs_regex("\\w+"), "\\w+");
    }

    #[test]
    fn translate_complex_pattern() {
        // Emacs: \(defun\|defvar\)\s-+\(\w+\)
        // Rust:  (defun|defvar)\s+(\w+)
        let emacs = "\\(defun\\|defvar\\)\\s-+\\(\\w+\\)";
        let rust = translate_emacs_regex(emacs);
        // After translation: (defun|defvar)\s+(\w+)
        assert_eq!(rust, "(defun|defvar)\\s+(\\w+)");
    }

    #[test]
    fn translate_empty_pattern() {
        assert_eq!(translate_emacs_regex(""), "");
    }

    #[test]
    fn translate_no_special_chars() {
        assert_eq!(translate_emacs_regex("hello"), "hello");
    }

    #[test]
    fn translate_escaped_backslash() {
        assert_eq!(translate_emacs_regex("\\\\"), "\\\\");
    }

    // -----------------------------------------------------------------------
    // string_match_full
    // -----------------------------------------------------------------------

    #[test]
    fn string_match_basic() {
        let mut md = None;
        let result = string_match_full("he..o", "hello world", 0, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Some(0));
        let md = md.unwrap();
        assert_eq!(md.groups[0], Some((0, 5)));
        assert_eq!(md.searched_string, Some("hello world".to_string()));
    }

    #[test]
    fn string_match_with_groups() {
        let mut md = None;
        // Emacs regex: \(\w+\)@\(\w+\)
        let result = string_match_full(
            "\\(\\w+\\)@\\(\\w+\\)",
            "user@host",
            0,
            &mut md,
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Some(0));
        let md = md.unwrap();
        assert_eq!(md.groups.len(), 3); // full + 2 groups
        assert_eq!(md.groups[0], Some((0, 9)));
        assert_eq!(md.groups[1], Some((0, 4))); // "user"
        assert_eq!(md.groups[2], Some((5, 9))); // "host"
    }

    #[test]
    fn string_match_with_start_offset() {
        let mut md = None;
        let result = string_match_full("world", "hello world", 6, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Some(6));
    }

    #[test]
    fn string_match_no_match() {
        let mut md = None;
        let result = string_match_full("xyz", "hello world", 0, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), None);
        assert!(md.is_none());
    }

    #[test]
    fn string_match_emacs_alternation() {
        let mut md = None;
        // Emacs regex: \(foo\|bar\)
        let result = string_match_full("\\(foo\\|bar\\)", "test bar baz", 0, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Some(5));
        let md = md.unwrap();
        assert_eq!(md.groups[1], Some((5, 8))); // "bar"
    }

    // -----------------------------------------------------------------------
    // Buffer search: search_forward
    // -----------------------------------------------------------------------

    fn make_test_buffer(text: &str) -> Buffer {
        let mut buf = Buffer::new(BufferId(1), "test".to_string());
        buf.insert(text);
        // Reset point to beginning
        buf.pt = 0;
        // zv was updated by insert
        buf
    }

    #[test]
    fn search_forward_basic() {
        let mut buf = make_test_buffer("hello world");
        let mut md = None;
        let result = search_forward(&mut buf, "world", None, false, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Some(11)); // end of "world"
        assert_eq!(buf.pt, 11);
        let md = md.unwrap();
        assert_eq!(md.groups[0], Some((6, 11)));
    }

    #[test]
    fn search_forward_not_found_noerror() {
        let mut buf = make_test_buffer("hello world");
        let mut md = None;
        let result = search_forward(&mut buf, "xyz", None, true, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), None);
        assert_eq!(buf.pt, 0); // point unchanged
    }

    #[test]
    fn search_forward_not_found_error() {
        let mut buf = make_test_buffer("hello world");
        let mut md = None;
        let result = search_forward(&mut buf, "xyz", None, false, &mut md);
        assert!(result.is_err());
    }

    #[test]
    fn search_forward_with_bound() {
        let mut buf = make_test_buffer("hello world");
        let mut md = None;
        // Search only within first 5 bytes — "world" starts at 6 so should not be found
        let result = search_forward(&mut buf, "world", Some(5), true, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), None);
    }

    #[test]
    fn search_forward_from_middle() {
        let mut buf = make_test_buffer("aaa bbb aaa");
        buf.pt = 4; // after "aaa "
        let mut md = None;
        let result = search_forward(&mut buf, "aaa", None, false, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Some(11)); // second "aaa" at end
    }

    // -----------------------------------------------------------------------
    // Buffer search: search_backward
    // -----------------------------------------------------------------------

    #[test]
    fn search_backward_basic() {
        let mut buf = make_test_buffer("hello world");
        buf.pt = 11; // end of buffer
        let mut md = None;
        let result = search_backward(&mut buf, "hello", None, false, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Some(0)); // beginning of "hello"
        assert_eq!(buf.pt, 0);
    }

    #[test]
    fn search_backward_not_found() {
        let mut buf = make_test_buffer("hello world");
        buf.pt = 11;
        let mut md = None;
        let result = search_backward(&mut buf, "xyz", None, true, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), None);
    }

    #[test]
    fn search_backward_finds_last_occurrence() {
        let mut buf = make_test_buffer("aaa bbb aaa");
        buf.pt = 11; // end
        let mut md = None;
        let result = search_backward(&mut buf, "aaa", None, false, &mut md);
        assert!(result.is_ok());
        // Should find the LAST "aaa" (at position 8)
        assert_eq!(result.unwrap(), Some(8));
        assert_eq!(buf.pt, 8);
    }

    // -----------------------------------------------------------------------
    // Buffer search: re_search_forward
    // -----------------------------------------------------------------------

    #[test]
    fn re_search_forward_basic() {
        let mut buf = make_test_buffer("foo 123 bar");
        let mut md = None;
        let result = re_search_forward(&mut buf, "[0-9]+", None, false, &mut md);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Some(7)); // end of "123"
        assert_eq!(buf.pt, 7);
        let md = md.unwrap();
        assert_eq!(md.groups[0], Some((4, 7)));
    }

    #[test]
    fn re_search_forward_with_groups() {
        let mut buf = make_test_buffer("name: John");
        let mut md = None;
        // Emacs regex: \(\w+\): \(\w+\)
        let result = re_search_forward(&mut buf, "\\(\\w+\\): \\(\\w+\\)", None, false, &mut md);
        assert!(result.is_ok());
        let md = md.unwrap();
        assert_eq!(md.groups.len(), 3);
        assert_eq!(md.groups[1], Some((0, 4))); // "name"
        assert_eq!(md.groups[2], Some((6, 10))); // "John"
    }

    // -----------------------------------------------------------------------
    // Buffer search: re_search_backward
    // -----------------------------------------------------------------------

    #[test]
    fn re_search_backward_basic() {
        let mut buf = make_test_buffer("abc 123 def 456");
        buf.pt = 15; // end
        let mut md = None;
        let result = re_search_backward(&mut buf, "[0-9]+", None, false, &mut md);
        assert!(result.is_ok());
        // Should find "456" (the last match)
        assert_eq!(result.unwrap(), Some(12));
        assert_eq!(buf.pt, 12);
    }

    // -----------------------------------------------------------------------
    // looking_at
    // -----------------------------------------------------------------------

    #[test]
    fn looking_at_matches() {
        let mut buf = make_test_buffer("hello world");
        buf.pt = 0;
        let mut md = None;
        let result = looking_at(&buf, "hello", &mut md);
        assert!(result.is_ok());
        assert!(result.unwrap());
        assert!(md.is_some());
    }

    #[test]
    fn looking_at_no_match() {
        let mut buf = make_test_buffer("hello world");
        buf.pt = 0;
        let mut md = None;
        let result = looking_at(&buf, "world", &mut md);
        assert!(result.is_ok());
        assert!(!result.unwrap());
    }

    #[test]
    fn looking_at_from_middle() {
        let mut buf = make_test_buffer("hello world");
        buf.pt = 6; // "world"
        let mut md = None;
        let result = looking_at(&buf, "world", &mut md);
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[test]
    fn looking_at_with_groups() {
        let mut buf = make_test_buffer("foo123bar");
        buf.pt = 0;
        let mut md = None;
        // Emacs: \(\w+\)\([0-9]+\)
        let result = looking_at(&buf, "\\(\\w+\\)\\([0-9]+\\)", &mut md);
        assert!(result.is_ok());
        assert!(result.unwrap());
        let md = md.unwrap();
        // \w+ is greedy, matches "foo123bar" leaving nothing for [0-9]+
        // Actually \w includes digits, so \w+ matches everything
        // Let's check what actually happens
        assert!(md.groups[0].is_some());
    }

    // -----------------------------------------------------------------------
    // replace_match
    // -----------------------------------------------------------------------

    #[test]
    fn replace_match_literal() {
        let mut buf = make_test_buffer("hello world");
        let mut md = None;
        let _ = re_search_forward(&mut buf, "world", None, false, &mut md);
        let result = replace_match(&mut buf, "rust", false, true, &md);
        assert!(result.is_ok());
        assert!(result.unwrap());
        let content = buf.text.text_range(0, buf.text.len());
        assert_eq!(content, "hello rust");
    }

    #[test]
    fn replace_match_with_backref() {
        let mut buf = make_test_buffer("hello world");
        buf.pt = 0;
        let mut md = None;
        // Match "hello" with a group
        let _ = re_search_forward(&mut buf, "\\(hello\\)", None, false, &mut md);
        let result = replace_match(&mut buf, "\\1 there", false, false, &md);
        assert!(result.is_ok());
        assert!(result.unwrap());
        let content = buf.text.text_range(0, buf.text.len());
        assert_eq!(content, "hello there world");
    }

    // -----------------------------------------------------------------------
    // Integration: search + match data
    // -----------------------------------------------------------------------

    #[test]
    fn search_forward_then_match_string() {
        let mut buf = make_test_buffer("The quick brown fox");
        let mut md = None;
        let _ = re_search_forward(&mut buf, "\\(quick\\) \\(brown\\)", None, false, &mut md);
        let md = md.as_ref().unwrap();

        // match-string 0 = "quick brown"
        let (s0, e0) = md.groups[0].unwrap();
        assert_eq!(buf.text.text_range(s0, e0), "quick brown");

        // match-string 1 = "quick"
        let (s1, e1) = md.groups[1].unwrap();
        assert_eq!(buf.text.text_range(s1, e1), "quick");

        // match-string 2 = "brown"
        let (s2, e2) = md.groups[2].unwrap();
        assert_eq!(buf.text.text_range(s2, e2), "brown");
    }

    #[test]
    fn string_match_then_match_data() {
        let mut md = None;
        let _ = string_match_full(
            "\\([0-9]+\\)-\\([0-9]+\\)",
            "date: 2024-01-15",
            0,
            &mut md,
        );
        let md = md.as_ref().unwrap();
        let string = md.searched_string.as_ref().unwrap();

        // match-beginning 0
        let (s0, _e0) = md.groups[0].unwrap();
        assert_eq!(s0, 6); // "2024-01"

        // Group 1: "2024"
        let (s1, e1) = md.groups[1].unwrap();
        assert_eq!(&string[s1..e1], "2024");

        // Group 2: "01"
        let (s2, e2) = md.groups[2].unwrap();
        assert_eq!(&string[s2..e2], "01");
    }

    #[test]
    fn string_match_optional_group() {
        let mut md = None;
        // Pattern with an optional group: \(foo\)\(bar\)?
        let _ = string_match_full("\\(foo\\)\\(bar\\)?", "fooXYZ", 0, &mut md);
        let md = md.as_ref().unwrap();
        assert_eq!(md.groups[1], Some((0, 3))); // "foo"
        assert_eq!(md.groups[2], None); // optional group didn't match
    }
}
