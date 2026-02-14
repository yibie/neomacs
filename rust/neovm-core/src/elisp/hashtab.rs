//! Extended hash-table and obarray builtins.
//!
//! Supplements the basic hash-table operations in `builtins.rs` with:
//! - `hash-table-keys`, `hash-table-values`, `maphash`
//! - `hash-table-test`, `hash-table-size`, `hash-table-rehash-size`,
//!   `hash-table-rehash-threshold`, `hash-table-weakness`
//! - `copy-hash-table`
//! - `mapatoms`, `unintern`

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

fn validate_optional_obarray_arg(args: &[Value]) -> Result<(), Flow> {
    if let Some(obarray) = args.get(1) {
        if !obarray.is_nil() && !matches!(obarray, Value::Vector(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("obarrayp"), obarray.clone()],
            ));
        }
    }
    Ok(())
}

/// Convert a `HashKey` back into a `Value`.
fn hash_key_to_value(key: &HashKey) -> Value {
    match key {
        HashKey::Nil => Value::Nil,
        HashKey::True => Value::True,
        HashKey::Int(n) => Value::Int(*n),
        HashKey::Float(bits) => Value::Float(f64::from_bits(*bits)),
        HashKey::Symbol(s) => Value::symbol(s.clone()),
        HashKey::Keyword(s) => Value::Keyword(s.clone()),
        HashKey::Str(s) => Value::string(s.clone()),
        HashKey::Char(c) => Value::Char(*c),
        HashKey::Ptr(_) => Value::Nil, // can't reconstruct from pointer
    }
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// (hash-table-keys TABLE) -> list of keys
pub(crate) fn builtin_hash_table_keys(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-keys", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let keys: Vec<Value> = table.data.keys().map(hash_key_to_value).collect();
            Ok(Value::list(keys))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-values TABLE) -> list of values
pub(crate) fn builtin_hash_table_values(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-values", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let vals: Vec<Value> = table.data.values().cloned().collect();
            Ok(Value::list(vals))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-test TABLE) -> symbol
pub(crate) fn builtin_hash_table_test(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-test", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let sym = match table.test {
                HashTableTest::Eq => "eq",
                HashTableTest::Eql => "eql",
                HashTableTest::Equal => "equal",
            };
            Ok(Value::symbol(sym))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-size TABLE) -> integer
pub(crate) fn builtin_hash_table_size(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-size", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            Ok(Value::Int(table.size))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-rehash-size TABLE) -> float (stub: 1.5)
pub(crate) fn builtin_hash_table_rehash_size(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-rehash-size", &args, 1)?;
    match &args[0] {
        Value::HashTable(_) => Ok(Value::Float(1.5)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-rehash-threshold TABLE) -> float (stub: 0.8125)
pub(crate) fn builtin_hash_table_rehash_threshold(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-rehash-threshold", &args, 1)?;
    match &args[0] {
        Value::HashTable(_) => Ok(Value::Float(0.8125)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-weakness TABLE) -> nil | symbol
pub(crate) fn builtin_hash_table_weakness(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-weakness", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            Ok(match table.weakness {
                None => Value::Nil,
                Some(HashTableWeakness::Key) => Value::symbol("key"),
                Some(HashTableWeakness::Value) => Value::symbol("value"),
                Some(HashTableWeakness::KeyOrValue) => Value::symbol("key-or-value"),
                Some(HashTableWeakness::KeyAndValue) => Value::symbol("key-and-value"),
            })
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (copy-hash-table TABLE) -> new hash table with same entries
pub(crate) fn builtin_copy_hash_table(args: Vec<Value>) -> EvalResult {
    expect_args("copy-hash-table", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let new_table = table.clone();
            Ok(Value::HashTable(std::sync::Arc::new(
                std::sync::Mutex::new(new_table),
            )))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// Eval-dependent builtins
// ---------------------------------------------------------------------------

/// (maphash FUNCTION TABLE) — call FUNCTION with each (KEY VALUE) pair.
pub(crate) fn builtin_maphash(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("maphash", &args, 2)?;
    let func = args[0].clone();
    let entries: Vec<(Value, Value)> = match &args[1] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            table
                .data
                .iter()
                .map(|(k, v)| (hash_key_to_value(k), v.clone()))
                .collect()
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("hash-table-p"), other.clone()],
            ));
        }
    };
    for (key, val) in entries {
        eval.apply(func.clone(), vec![key, val])?;
    }
    Ok(Value::Nil)
}

/// (mapatoms FUNCTION &optional OBARRAY) — call FUNCTION with each interned symbol.
pub(crate) fn builtin_mapatoms(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("mapatoms", &args, 1)?;
    expect_max_args("mapatoms", &args, 2)?;
    validate_optional_obarray_arg(&args)?;
    let func = args[0].clone();
    // Collect symbol names to avoid borrowing obarray during eval
    let symbols: Vec<String> = eval
        .obarray
        .all_symbols()
        .iter()
        .map(|s| s.to_string())
        .collect();
    for sym in symbols {
        eval.apply(func.clone(), vec![Value::symbol(sym)])?;
    }
    Ok(Value::Nil)
}

/// (unintern NAME &optional OBARRAY) — remove symbol from obarray.
pub(crate) fn builtin_unintern(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("unintern", &args, 1)?;
    expect_max_args("unintern", &args, 2)?;
    validate_optional_obarray_arg(&args)?;
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        Value::Str(s) => (**s).clone(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };
    let removed = eval.obarray.unintern(&name);
    Ok(if removed { Value::True } else { Value::Nil })
}
