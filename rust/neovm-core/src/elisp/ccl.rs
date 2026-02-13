//! Code Conversion Language (CCL) stubs.
//!
//! CCL is a low-level bytecode language for efficient character/text conversion.
//! This implementation provides stubs for basic CCL operations:
//! - `ccl-program-p` — check if object is a CCL program (always returns nil)
//! - `ccl-execute` — execute CCL program on status vector (stub, returns nil)
//! - `ccl-execute-on-string` — execute CCL program on string (stub, returns string unchanged)
//! - `register-ccl-program` — register a CCL program (stub, returns nil)
//! - `register-code-conversion-map` — register a code conversion map (stub, returns nil)
//!
//! Since the Elisp interpreter doesn't implement the full CCL runtime,
//! all operations are no-ops that satisfy the API contract.

use super::error::{signal, EvalResult, Flow};
use super::value::*;

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

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

fn expect_min_args(name: &str, args: &[Value], min: usize) -> Result<(), Flow> {
    if args.len() < min {
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

/// (ccl-program-p OBJECT) -> nil
/// Returns nil since we don't support CCL programs.
pub(crate) fn builtin_ccl_program_p(args: Vec<Value>) -> EvalResult {
    expect_args("ccl-program-p", &args, 1)?;
    // We don't have a CCL program type, so always return nil
    Ok(Value::Nil)
}

/// (ccl-execute CCL-PROGRAM STATUS) -> nil
/// Stub: doesn't actually execute CCL bytecode.
pub(crate) fn builtin_ccl_execute(args: Vec<Value>) -> EvalResult {
    expect_args("ccl-execute", &args, 2)?;
    // Argument validation: CCL-PROGRAM should be a CCL program (we have none)
    // STATUS should be a vector (we don't validate, just accept anything)
    // Since we don't support CCL programs, this is a no-op that returns nil
    Ok(Value::Nil)
}

/// (ccl-execute-on-string CCL-PROGRAM STATUS STRING &optional CONTINUE UNIBYTE-P) -> STRING
/// Stub: returns STRING unchanged without processing.
pub(crate) fn builtin_ccl_execute_on_string(args: Vec<Value>) -> EvalResult {
    expect_min_args("ccl-execute-on-string", &args, 3)?;
    // Arguments:
    //   0: CCL-PROGRAM (we don't use)
    //   1: STATUS vector (we don't use)
    //   2: STRING (return this unchanged)
    //   3: CONTINUE (optional, we don't use)
    //   4: UNIBYTE-P (optional, we don't use)

    // Extract and return the string argument unchanged
    match &args[2] {
        Value::Str(s) => Ok(Value::Str(s.clone())),
        Value::Nil => Ok(Value::Nil), // Allow nil as empty string
        other => {
            // Type error: STRING must be a string or nil
            Err(signal("wrong-type-argument", vec![Value::symbol("stringp"), other.clone()]))
        }
    }
}

/// (register-ccl-program NAME CCL-PROG) -> nil
/// Stub: accepts and discards the CCL program registration.
pub(crate) fn builtin_register_ccl_program(args: Vec<Value>) -> EvalResult {
    expect_args("register-ccl-program", &args, 2)?;
    // Arguments:
    //   0: NAME (symbol name for the program)
    //   1: CCL-PROG (the program definition)
    // We accept both but don't store anything since we don't support CCL
    Ok(Value::Nil)
}

/// (register-code-conversion-map SYMBOL MAP) -> nil
/// Stub: accepts and discards the code conversion map.
pub(crate) fn builtin_register_code_conversion_map(args: Vec<Value>) -> EvalResult {
    expect_args("register-code-conversion-map", &args, 2)?;
    // Arguments:
    //   0: SYMBOL (name for the conversion map)
    //   1: MAP (the conversion map definition, typically a char-table)
    // We accept both but don't store anything since we don't support CCL
    Ok(Value::Nil)
}
