//! Error and signal types for the evaluator.

use std::error::Error;
use std::fmt::{self, Display, Formatter};

use super::value::Value;

/// Public-facing evaluation error.
#[derive(Clone, Debug)]
pub enum EvalError {
    Signal { symbol: String, data: Vec<Value> },
    UncaughtThrow { tag: Value, value: Value },
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Signal { symbol, data } => {
                write!(
                    f,
                    "signal {} {}",
                    symbol,
                    super::print::print_value(&Value::list(data.clone()))
                )
            }
            Self::UncaughtThrow { tag, value } => write!(
                f,
                "uncaught throw tag={} value={}",
                super::print::print_value(tag),
                super::print::print_value(value),
            ),
        }
    }
}

impl Error for EvalError {}

/// Internal non-local control flow.
#[derive(Clone, Debug)]
pub(crate) enum Flow {
    Signal(SignalData),
    Throw { tag: Value, value: Value },
}

#[derive(Clone, Debug)]
pub(crate) struct SignalData {
    pub symbol: String,
    pub data: Vec<Value>,
    /// Original cdr payload when a signal uses non-list data.
    pub raw_data: Option<Value>,
}

pub(crate) type EvalResult = Result<Value, Flow>;

/// Create a signal flow.
pub(crate) fn signal(symbol: &str, data: Vec<Value>) -> Flow {
    Flow::Signal(SignalData {
        symbol: symbol.to_string(),
        data,
        raw_data: None,
    })
}

/// Create a signal where DATA is used as the raw cdr payload.
///
/// This preserves dotted signal data shapes such as `(foo . 1)`.
pub(crate) fn signal_with_data(symbol: &str, data: Value) -> Flow {
    let normalized = super::value::list_to_vec(&data).unwrap_or_else(|| vec![data.clone()]);
    Flow::Signal(SignalData {
        symbol: symbol.to_string(),
        data: normalized,
        raw_data: Some(data),
    })
}

/// Convert internal flow to public EvalError.
pub(crate) fn map_flow(flow: Flow) -> EvalError {
    match flow {
        Flow::Signal(sig) => EvalError::Signal {
            symbol: sig.symbol,
            data: sig.data,
        },
        Flow::Throw { tag, value } => EvalError::UncaughtThrow { tag, value },
    }
}

/// Check if a condition-case pattern matches a signal symbol.
pub(crate) fn signal_matches(pattern: &super::expr::Expr, symbol: &str) -> bool {
    use super::expr::Expr;
    match pattern {
        Expr::Symbol(name) => name == symbol || name == "error" || name == "t",
        Expr::List(items) => items.iter().any(|item| signal_matches(item, symbol)),
        _ => false,
    }
}

/// Build the binding value for condition-case variable: (symbol . data)
pub(crate) fn make_signal_binding_value(sig: &SignalData) -> Value {
    if let Some(raw) = &sig.raw_data {
        return Value::cons(Value::symbol(sig.symbol.clone()), raw.clone());
    }
    let mut values = Vec::with_capacity(sig.data.len() + 1);
    values.push(Value::symbol(sig.symbol.clone()));
    values.extend(sig.data.clone());
    Value::list(values)
}

/// Format an eval result for the compat test harness (TSV output).
pub fn format_eval_result(result: &Result<Value, EvalError>) -> String {
    match result {
        Ok(value) => format!("OK {}", super::print::print_value(value)),
        Err(EvalError::Signal { symbol, data }) => {
            let payload = if data.is_empty() {
                "nil".to_string()
            } else {
                super::print::print_value(&Value::list(data.clone()))
            };
            format!("ERR ({} {})", symbol, payload)
        }
        Err(EvalError::UncaughtThrow { tag, value }) => {
            format!(
                "ERR (no-catch ({} {}))",
                super::print::print_value(tag),
                super::print::print_value(value),
            )
        }
    }
}

/// Render a value with evaluator-context-aware opaque handle formatting.
pub fn print_value_with_eval(eval: &super::eval::Evaluator, value: &Value) -> String {
    if let Some(handle) = super::display::print_terminal_handle(value) {
        return handle;
    }
    if let Some(id) = eval.threads.thread_id_from_handle(value) {
        return format!("#<thread {id}>");
    }
    if let Some(id) = eval.threads.mutex_id_from_handle(value) {
        return format!("#<mutex {id}>");
    }
    if let Some(id) = eval.threads.condition_variable_id_from_handle(value) {
        return format!("#<condvar {id}>");
    }
    super::print::print_value(value)
}

fn print_data_payload_with_eval(eval: &super::eval::Evaluator, data: &[Value]) -> String {
    if data.is_empty() {
        "nil".to_string()
    } else {
        let parts = data
            .iter()
            .map(|v| print_value_with_eval(eval, v))
            .collect::<Vec<_>>();
        format!("({})", parts.join(" "))
    }
}

/// Format an eval result for harnesses that have evaluator context and need
/// opaque handle rendering for thread/mutex/condvar/terminal values.
pub fn format_eval_result_with_eval(
    eval: &super::eval::Evaluator,
    result: &Result<Value, EvalError>,
) -> String {
    match result {
        Ok(value) => format!("OK {}", print_value_with_eval(eval, value)),
        Err(EvalError::Signal { symbol, data }) => {
            let payload = print_data_payload_with_eval(eval, data);
            format!("ERR ({} {})", symbol, payload)
        }
        Err(EvalError::UncaughtThrow { tag, value }) => {
            format!(
                "ERR (no-catch ({} {}))",
                print_value_with_eval(eval, tag),
                print_value_with_eval(eval, value),
            )
        }
    }
}
