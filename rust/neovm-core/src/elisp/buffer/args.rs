use super::super::eval::Evaluator;
use super::{signal, BufferId, Flow, Value};

pub(super) fn expect_args(name: &str, args: &[Value], n: usize) -> Result<(), Flow> {
    if args.len() != n {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

pub(super) fn expect_min_args(name: &str, args: &[Value], min: usize) -> Result<(), Flow> {
    if args.len() < min {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

pub(super) fn expect_max_args(name: &str, args: &[Value], max: usize) -> Result<(), Flow> {
    if args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

pub(super) fn expect_string(val: &Value) -> Result<String, Flow> {
    match val {
        Value::Str(s) => Ok((**s).clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

pub(super) fn expect_buffer_id(value: &Value) -> Result<BufferId, Flow> {
    match value {
        Value::Buffer(id) => Ok(*id),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("bufferp"), other.clone()],
        )),
    }
}

/// Resolve a BUFFER-OR-NAME argument to a BufferId.
/// Accepts Value::Buffer, Value::Str (looked up by name), or Value::Nil/absent
/// (uses current buffer).
pub(super) fn resolve_buffer_arg(eval: &Evaluator, arg: Option<&Value>) -> Result<BufferId, Flow> {
    match arg {
        None | Some(Value::Nil) => eval
            .buffers
            .current_buffer()
            .map(|b| b.id)
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")])),
        Some(Value::Buffer(id)) => {
            if eval.buffers.get(*id).is_some() {
                Ok(*id)
            } else {
                Err(signal("error", vec![Value::string("No such live buffer")]))
            }
        }
        Some(Value::Str(s)) => eval
            .buffers
            .find_buffer_by_name(s)
            .ok_or_else(|| signal("error", vec![Value::string(format!("No buffer named {s}"))])),
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

/// Like resolve_buffer_arg but also accepts a string for BUFFER-OR-NAME and
/// returns None (instead of error) when the buffer is not found.
pub(super) fn resolve_buffer_or_name_opt(
    eval: &Evaluator,
    arg: Option<&Value>,
) -> Option<BufferId> {
    match arg {
        None | Some(Value::Nil) => eval.buffers.current_buffer().map(|b| b.id),
        Some(Value::Buffer(id)) => {
            if eval.buffers.get(*id).is_some() {
                Some(*id)
            } else {
                None
            }
        }
        Some(Value::Str(s)) => eval.buffers.find_buffer_by_name(s),
        _ => None,
    }
}
