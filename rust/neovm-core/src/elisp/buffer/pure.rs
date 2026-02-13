use super::args::{expect_args, expect_max_args, expect_min_args, expect_string};
use super::{EvalResult, Value};

/// (get-file-buffer FILENAME) -> nil
///
/// Find the buffer visiting file FILENAME.
/// Stub: always returns nil (no file-visiting tracking yet).
pub(crate) fn builtin_get_file_buffer(args: Vec<Value>) -> EvalResult {
    expect_args("get-file-buffer", &args, 1)?;
    let _filename = expect_string(&args[0])?;
    Ok(Value::Nil)
}

/// (make-indirect-buffer BASE-BUFFER NAME &optional CLONE) -> nil
///
/// Create an indirect buffer sharing the text of BASE-BUFFER.
/// Stub: signals an error (indirect buffers not supported).
pub(crate) fn builtin_make_indirect_buffer(args: Vec<Value>) -> EvalResult {
    expect_min_args("make-indirect-buffer", &args, 2)?;
    expect_max_args("make-indirect-buffer", &args, 3)?;
    // Stub: indirect buffers not supported
    Ok(Value::Nil)
}

/// (buffer-base-buffer &optional BUFFER) -> nil
///
/// Return the base buffer of BUFFER if it is indirect, or nil.
/// Since we do not support indirect buffers, always returns nil.
pub(crate) fn builtin_buffer_base_buffer(args: Vec<Value>) -> EvalResult {
    expect_max_args("buffer-base-buffer", &args, 1)?;
    Ok(Value::Nil)
}

/// (overlay-lists) -> (nil . nil)
///
/// Return a pair of lists of overlays before and after point.
/// Stub: returns (nil . nil).
pub(crate) fn builtin_overlay_lists(args: Vec<Value>) -> EvalResult {
    expect_args("overlay-lists", &args, 0)?;
    Ok(Value::cons(Value::Nil, Value::Nil))
}

/// (overlayp OBJECT) -> nil
///
/// Return t if OBJECT is an overlay, nil otherwise.
/// Stub: always returns nil.
pub(crate) fn builtin_overlayp(args: Vec<Value>) -> EvalResult {
    expect_args("overlayp", &args, 1)?;
    Ok(Value::Nil)
}

/// (make-overlay BEG END &optional BUFFER FRONT-ADVANCE REAR-ADVANCE) -> nil
///
/// Create a new overlay in BUFFER from BEG to END.
/// Stub: returns nil.
pub(crate) fn builtin_make_overlay(args: Vec<Value>) -> EvalResult {
    expect_min_args("make-overlay", &args, 2)?;
    expect_max_args("make-overlay", &args, 5)?;
    Ok(Value::Nil)
}

/// (delete-overlay OVERLAY) -> nil
///
/// Delete the overlay OVERLAY from its buffer.
/// Stub: returns nil.
pub(crate) fn builtin_delete_overlay(args: Vec<Value>) -> EvalResult {
    expect_args("delete-overlay", &args, 1)?;
    Ok(Value::Nil)
}

/// (overlay-start OVERLAY) -> nil
///
/// Return the start position of OVERLAY.
/// Stub: returns nil.
pub(crate) fn builtin_overlay_start(args: Vec<Value>) -> EvalResult {
    expect_args("overlay-start", &args, 1)?;
    Ok(Value::Nil)
}

/// (overlay-end OVERLAY) -> nil
///
/// Return the end position of OVERLAY.
/// Stub: returns nil.
pub(crate) fn builtin_overlay_end(args: Vec<Value>) -> EvalResult {
    expect_args("overlay-end", &args, 1)?;
    Ok(Value::Nil)
}

/// (overlay-buffer OVERLAY) -> nil
///
/// Return the buffer OVERLAY belongs to.
/// Stub: returns nil.
pub(crate) fn builtin_overlay_buffer(args: Vec<Value>) -> EvalResult {
    expect_args("overlay-buffer", &args, 1)?;
    Ok(Value::Nil)
}

/// (overlay-get OVERLAY PROP) -> nil
///
/// Return the value of property PROP on OVERLAY.
/// Stub: returns nil.
pub(crate) fn builtin_overlay_get(args: Vec<Value>) -> EvalResult {
    expect_args("overlay-get", &args, 2)?;
    Ok(Value::Nil)
}

/// (overlay-put OVERLAY PROP VALUE) -> nil
///
/// Set property PROP on OVERLAY to VALUE.
/// Stub: returns nil.
pub(crate) fn builtin_overlay_put(args: Vec<Value>) -> EvalResult {
    expect_args("overlay-put", &args, 3)?;
    Ok(Value::Nil)
}

/// (overlays-at POS &optional SORTED) -> nil
///
/// Return a list of overlays at position POS.
/// Stub: returns nil (empty list).
pub(crate) fn builtin_overlays_at(args: Vec<Value>) -> EvalResult {
    expect_min_args("overlays-at", &args, 1)?;
    expect_max_args("overlays-at", &args, 2)?;
    Ok(Value::Nil)
}

/// (overlays-in BEG END) -> nil
///
/// Return a list of overlays between BEG and END.
/// Stub: returns nil (empty list).
pub(crate) fn builtin_overlays_in(args: Vec<Value>) -> EvalResult {
    expect_args("overlays-in", &args, 2)?;
    Ok(Value::Nil)
}
