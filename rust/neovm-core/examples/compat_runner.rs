use neovm_core::elisp::{format_eval_result, parse_forms, print_expr, Evaluator};
use std::fs;

fn main() {
    let Some(path) = std::env::args().nth(1) else {
        eprintln!("usage: compat_runner <forms-file>");
        std::process::exit(2);
    };

    let source = match fs::read_to_string(&path) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("failed to read {path}: {err}");
            std::process::exit(2);
        }
    };

    let forms = match parse_forms(&source) {
        Ok(forms) => forms,
        Err(err) => {
            eprintln!("failed to parse forms: {err}");
            std::process::exit(2);
        }
    };

    let mut evaluator = Evaluator::new();
    for (index, form) in forms.iter().enumerate() {
        let result = evaluator.eval_expr(form);
        println!("{}\t{}\t{}", index + 1, print_expr(form), format_eval_result(&result));
    }
}
