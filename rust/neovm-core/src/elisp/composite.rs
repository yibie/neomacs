//! Composition builtins (complex script rendering).
//!
//! In real Emacs, the composition system handles combining characters,
//! ligatures, and complex script shaping.  Most of this work is done by the
//! display engine (and in Neomacs, by the Rust layout engine), so here we
//! provide stubs that satisfy Elisp code which queries or manipulates
//! compositions at the Lisp level.

use super::error::{signal, EvalResult, Flow};
use super::value::*;

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

fn expect_range_args(name: &str, args: &[Value], min: usize, max: usize) -> Result<(), Flow> {
    if args.len() < min || args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_args(name: &str, args: &[Value], n: usize) -> Result<(), Flow> {
    if args.len() != n {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_max_args(name: &str, args: &[Value], max: usize) -> Result<(), Flow> {
    if args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// `(compose-region-internal START END &optional COMPONENTS MODIFICATION-FUNC)`
///
/// Compose text in the current buffer between START and END.
/// COMPONENTS, if given, is a vector or string describing the composition.
/// MODIFICATION-FUNC, if non-nil, is called when the composition is modified.
///
/// Stub: composition is handled by the display/layout engine; return nil.
pub(crate) fn builtin_compose_region_internal(args: Vec<Value>) -> EvalResult {
    expect_range_args("compose-region-internal", &args, 2, 4)?;
    // START and END should be integers, but since this is a stub we just
    // validate arity and return nil.
    Ok(Value::Nil)
}

/// `(compose-string-internal STRING START END &optional COMPONENTS MODIFICATION-FUNC)`
///
/// Compose text in STRING between indices START and END.
/// Returns STRING (possibly with composition properties attached).
///
/// Stub: return STRING unchanged.
pub(crate) fn builtin_compose_string_internal(args: Vec<Value>) -> EvalResult {
    expect_range_args("compose-string-internal", &args, 3, 5)?;
    // Return the string argument unchanged.
    Ok(args[0].clone())
}

/// `(find-composition-internal POS LIMIT STRING DETAIL-P)`
///
/// Find a composition at or near position POS.
/// Returns a list describing the composition, or nil if none found.
///
/// Stub: no compositions exist, always return nil.
pub(crate) fn builtin_find_composition_internal(args: Vec<Value>) -> EvalResult {
    expect_args("find-composition-internal", &args, 4)?;
    Ok(Value::Nil)
}

/// `(composition-get-gstring FROM TO FONT-OBJECT STRING)`
///
/// Return a gstring (grapheme cluster string) for composing characters
/// between FROM and TO with FONT-OBJECT in STRING.
///
/// Stub: return nil (let the display engine handle shaping).
pub(crate) fn builtin_composition_get_gstring(args: Vec<Value>) -> EvalResult {
    expect_args("composition-get-gstring", &args, 4)?;
    Ok(Value::Nil)
}

/// `(clear-composition-cache)`
///
/// Clear the internal composition cache.
///
/// Stub: no cache to clear, return nil.
pub(crate) fn builtin_clear_composition_cache(args: Vec<Value>) -> EvalResult {
    expect_max_args("clear-composition-cache", &args, 0)?;
    Ok(Value::Nil)
}

/// `(composition-sort-rules RULES)`
///
/// Sort composition rules by priority.
/// Returns RULES unchanged (stub).
pub(crate) fn builtin_composition_sort_rules(args: Vec<Value>) -> EvalResult {
    expect_args("composition-sort-rules", &args, 1)?;
    Ok(args[0].clone())
}

/// `(auto-composition-mode &optional ARG)`
///
/// Toggle auto-composition mode.  In real Emacs this is a minor mode that
/// controls whether automatic character composition is performed.
///
/// Stub: return nil.  The actual variable `auto-composition-mode` should be
/// set via `defvar`/`defcustom` in Lisp; this function entry exists only to
/// satisfy code that calls it as a function.
pub(crate) fn builtin_auto_composition_mode(args: Vec<Value>) -> EvalResult {
    expect_max_args("auto-composition-mode", &args, 1)?;
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compose_region_internal_min_args() {
        let result = builtin_compose_region_internal(vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn compose_region_internal_max_args() {
        let result = builtin_compose_region_internal(vec![
            Value::Int(1),
            Value::Int(10),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn compose_region_internal_too_few_args() {
        let result = builtin_compose_region_internal(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn compose_region_internal_too_many_args() {
        let result = builtin_compose_region_internal(vec![
            Value::Int(1),
            Value::Int(10),
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn compose_string_internal_returns_string() {
        let s = Value::string("hello");
        let result =
            builtin_compose_string_internal(vec![s.clone(), Value::Int(0), Value::Int(5)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("hello"));
    }

    #[test]
    fn compose_string_internal_with_optional_args() {
        let s = Value::string("hello");
        let result = builtin_compose_string_internal(vec![
            s.clone(),
            Value::Int(0),
            Value::Int(5),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("hello"));
    }

    #[test]
    fn compose_string_internal_too_few_args() {
        let result =
            builtin_compose_string_internal(vec![Value::string("hi"), Value::Int(0)]);
        assert!(result.is_err());
    }

    #[test]
    fn find_composition_internal_returns_nil() {
        let result = builtin_find_composition_internal(vec![
            Value::Int(1),
            Value::Int(100),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn find_composition_internal_wrong_arity() {
        let result = builtin_find_composition_internal(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn composition_get_gstring_returns_nil() {
        let result = builtin_composition_get_gstring(vec![
            Value::Int(0),
            Value::Int(5),
            Value::Nil,
            Value::string("hello"),
        ]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn composition_get_gstring_wrong_arity() {
        let result = builtin_composition_get_gstring(vec![Value::Int(0)]);
        assert!(result.is_err());
    }

    #[test]
    fn clear_composition_cache_no_args() {
        let result = builtin_clear_composition_cache(vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn clear_composition_cache_too_many_args() {
        let result = builtin_clear_composition_cache(vec![Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn composition_sort_rules_returns_input() {
        let rules = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_composition_sort_rules(vec![rules.clone()]);
        assert!(result.is_ok());
        // Should return the same list object.
        let returned = result.unwrap();
        assert!(returned.is_list());
    }

    #[test]
    fn composition_sort_rules_wrong_arity() {
        let result = builtin_composition_sort_rules(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn auto_composition_mode_no_args() {
        let result = builtin_auto_composition_mode(vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn auto_composition_mode_with_arg() {
        let result = builtin_auto_composition_mode(vec![Value::Int(1)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn auto_composition_mode_too_many_args() {
        let result = builtin_auto_composition_mode(vec![Value::Int(1), Value::Int(2)]);
        assert!(result.is_err());
    }
}
