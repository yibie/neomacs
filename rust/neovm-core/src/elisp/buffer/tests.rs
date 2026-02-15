use super::*;

// =======================================================================
// Helper: create an evaluator (comes with *scratch* buffer pre-created)
// =======================================================================

fn new_eval() -> super::super::eval::Evaluator {
    super::super::eval::Evaluator::new()
}

// =======================================================================
// Eval-dependent builtins
// =======================================================================

// ----- current-buffer -----

#[test]
fn current_buffer_returns_scratch() {
    let mut eval = new_eval();
    let result = builtin_current_buffer(&mut eval, vec![]).unwrap();
    match result {
        Value::Buffer(_) => {} // OK
        other => panic!("expected buffer, got {:?}", other),
    }
}

// ----- set-buffer -----

#[test]
fn set_buffer_by_name() {
    let mut eval = new_eval();
    let id = eval.buffers.create_buffer("test-buf");
    let result = builtin_set_buffer(&mut eval, vec![Value::string("test-buf")]).unwrap();
    assert!(matches!(result, Value::Buffer(bid) if bid == id));
    assert_eq!(eval.buffers.current_buffer().unwrap().id, id);
}

#[test]
fn set_buffer_by_value() {
    let mut eval = new_eval();
    let id = eval.buffers.create_buffer("test-buf");
    let result = builtin_set_buffer(&mut eval, vec![Value::Buffer(id)]).unwrap();
    assert!(matches!(result, Value::Buffer(bid) if bid == id));
}

#[test]
fn set_buffer_nonexistent_name_errors() {
    let mut eval = new_eval();
    let result = builtin_set_buffer(&mut eval, vec![Value::string("nonexistent")]);
    assert!(result.is_err());
}

#[test]
fn set_buffer_wrong_type_errors() {
    let mut eval = new_eval();
    let result = builtin_set_buffer(&mut eval, vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn set_buffer_wrong_arg_count() {
    let mut eval = new_eval();
    let result = builtin_set_buffer(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- get-buffer -----

#[test]
fn get_buffer_by_name_found() {
    let mut eval = new_eval();
    let result = builtin_get_buffer(&mut eval, vec![Value::string("*scratch*")]).unwrap();
    assert!(matches!(result, Value::Buffer(_)));
}

#[test]
fn get_buffer_by_name_not_found() {
    let mut eval = new_eval();
    let result = builtin_get_buffer(&mut eval, vec![Value::string("nonexistent")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn get_buffer_by_buffer_value() {
    let mut eval = new_eval();
    let id = eval.buffers.create_buffer("buf");
    let result = builtin_get_buffer(&mut eval, vec![Value::Buffer(id)]).unwrap();
    assert!(matches!(result, Value::Buffer(bid) if bid == id));
}

#[test]
fn get_buffer_dead_buffer() {
    let mut eval = new_eval();
    let id = eval.buffers.create_buffer("doomed");
    eval.buffers.kill_buffer(id);
    let result = builtin_get_buffer(&mut eval, vec![Value::Buffer(id)]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn get_buffer_wrong_type_returns_nil() {
    let mut eval = new_eval();
    let result = builtin_get_buffer(&mut eval, vec![Value::Int(42)]).unwrap();
    assert!(result.is_nil());
}

// ----- get-buffer-create -----

#[test]
fn get_buffer_create_existing() {
    let mut eval = new_eval();
    let scratch_id = eval.buffers.find_buffer_by_name("*scratch*").unwrap();
    let result = builtin_get_buffer_create(&mut eval, vec![Value::string("*scratch*")]).unwrap();
    assert!(matches!(result, Value::Buffer(id) if id == scratch_id));
}

#[test]
fn get_buffer_create_new() {
    let mut eval = new_eval();
    let result = builtin_get_buffer_create(&mut eval, vec![Value::string("new-buf")]).unwrap();
    match result {
        Value::Buffer(id) => {
            assert_eq!(eval.buffers.get(id).unwrap().name, "new-buf");
        }
        other => panic!("expected buffer, got {:?}", other),
    }
}

#[test]
fn get_buffer_create_with_inhibit_hooks() {
    let mut eval = new_eval();
    let result =
        builtin_get_buffer_create(&mut eval, vec![Value::string("new-buf"), Value::True]).unwrap();
    assert!(matches!(result, Value::Buffer(_)));
}

#[test]
fn get_buffer_create_wrong_args() {
    let mut eval = new_eval();
    let result = builtin_get_buffer_create(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- generate-new-buffer-name -----

#[test]
fn generate_new_buffer_name_free() {
    let mut eval = new_eval();
    let result =
        builtin_generate_new_buffer_name(&mut eval, vec![Value::string("unique-name")]).unwrap();
    assert_eq!(result.as_str().unwrap(), "unique-name");
}

#[test]
fn generate_new_buffer_name_taken() {
    let mut eval = new_eval();
    let result =
        builtin_generate_new_buffer_name(&mut eval, vec![Value::string("*scratch*")]).unwrap();
    assert_eq!(result.as_str().unwrap(), "*scratch*<2>");
}

#[test]
fn generate_new_buffer_name_with_ignore() {
    let mut eval = new_eval();
    // *scratch* exists but IGNORE matches it, so the base name is free.
    let result = builtin_generate_new_buffer_name(
        &mut eval,
        vec![Value::string("*scratch*"), Value::string("*scratch*")],
    )
    .unwrap();
    assert_eq!(result.as_str().unwrap(), "*scratch*");
}

#[test]
fn generate_new_buffer_name_wrong_args() {
    let mut eval = new_eval();
    let result = builtin_generate_new_buffer_name(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- buffer-list -----

#[test]
fn buffer_list_includes_scratch() {
    let mut eval = new_eval();
    let result = builtin_buffer_list(&mut eval, vec![]).unwrap();
    let items = list_to_vec(&result).unwrap();
    assert!(!items.is_empty());
    // At least *scratch* should be there.
    assert!(items.iter().any(|v| matches!(v, Value::Buffer(_))));
}

#[test]
fn buffer_list_grows_with_new_buffers() {
    let mut eval = new_eval();
    let before = list_to_vec(&builtin_buffer_list(&mut eval, vec![]).unwrap())
        .unwrap()
        .len();
    eval.buffers.create_buffer("extra");
    let after = list_to_vec(&builtin_buffer_list(&mut eval, vec![]).unwrap())
        .unwrap()
        .len();
    assert_eq!(after, before + 1);
}

#[test]
fn buffer_list_ignores_frame_arg() {
    let mut eval = new_eval();
    let result = builtin_buffer_list(&mut eval, vec![Value::Nil]).unwrap();
    assert!(list_to_vec(&result).is_some());
}

// ----- buffer-name -----

#[test]
fn buffer_name_current() {
    let mut eval = new_eval();
    let result = builtin_buffer_name(&mut eval, vec![]).unwrap();
    assert_eq!(result.as_str().unwrap(), "*scratch*");
}

#[test]
fn buffer_name_specific_buffer() {
    let mut eval = new_eval();
    let id = eval.buffers.create_buffer("named-buf");
    let result = builtin_buffer_name(&mut eval, vec![Value::Buffer(id)]).unwrap();
    assert_eq!(result.as_str().unwrap(), "named-buf");
}

#[test]
fn buffer_name_nil_means_current() {
    let mut eval = new_eval();
    let result = builtin_buffer_name(&mut eval, vec![Value::Nil]).unwrap();
    assert_eq!(result.as_str().unwrap(), "*scratch*");
}

// ----- buffer-file-name -----

#[test]
fn buffer_file_name_none() {
    let mut eval = new_eval();
    let result = builtin_buffer_file_name(&mut eval, vec![]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn buffer_file_name_with_file() {
    let mut eval = new_eval();
    let id = eval.buffers.create_buffer("foo.el");
    eval.buffers.get_mut(id).unwrap().file_name = Some("/home/user/foo.el".to_string());
    let result = builtin_buffer_file_name(&mut eval, vec![Value::Buffer(id)]).unwrap();
    assert_eq!(result.as_str().unwrap(), "/home/user/foo.el");
}

// ----- buffer-modified-p -----

#[test]
fn buffer_modified_p_unmodified() {
    let mut eval = new_eval();
    let result = builtin_buffer_modified_p(&mut eval, vec![]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn buffer_modified_p_after_modification() {
    let mut eval = new_eval();
    eval.buffers.current_buffer_mut().unwrap().insert("hello");
    let result = builtin_buffer_modified_p(&mut eval, vec![]).unwrap();
    assert!(result.is_truthy());
}

// ----- set-buffer-modified-p -----

#[test]
fn set_buffer_modified_p_set_true() {
    let mut eval = new_eval();
    let result = builtin_set_buffer_modified_p(&mut eval, vec![Value::True]).unwrap();
    assert!(result.is_truthy());
    assert!(eval.buffers.current_buffer().unwrap().is_modified());
}

#[test]
fn set_buffer_modified_p_set_nil() {
    let mut eval = new_eval();
    eval.buffers.current_buffer_mut().unwrap().insert("x");
    assert!(eval.buffers.current_buffer().unwrap().is_modified());
    builtin_set_buffer_modified_p(&mut eval, vec![Value::Nil]).unwrap();
    assert!(!eval.buffers.current_buffer().unwrap().is_modified());
}

#[test]
fn set_buffer_modified_p_wrong_args() {
    let mut eval = new_eval();
    let result = builtin_set_buffer_modified_p(&mut eval, vec![]);
    assert!(result.is_err());
}

// ----- kill-buffer -----

#[test]
fn kill_buffer_by_buffer_value() {
    let mut eval = new_eval();
    let id = eval.buffers.create_buffer("doomed");
    let result = builtin_kill_buffer(&mut eval, vec![Value::Buffer(id)]).unwrap();
    assert!(result.is_truthy());
    assert!(eval.buffers.get(id).is_none());
}

#[test]
fn kill_buffer_by_name() {
    let mut eval = new_eval();
    eval.buffers.create_buffer("doomed");
    let result = builtin_kill_buffer(&mut eval, vec![Value::string("doomed")]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn kill_buffer_nonexistent() {
    let mut eval = new_eval();
    let result = builtin_kill_buffer(&mut eval, vec![Value::string("nope")]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn kill_buffer_default_current() {
    let mut eval = new_eval();
    let cur_id = eval.buffers.current_buffer().unwrap().id;
    let result = builtin_kill_buffer(&mut eval, vec![]).unwrap();
    assert!(result.is_truthy());
    assert!(eval.buffers.get(cur_id).is_none());
}

#[test]
fn kill_buffer_already_dead() {
    let mut eval = new_eval();
    let id = eval.buffers.create_buffer("doomed");
    eval.buffers.kill_buffer(id);
    let result = builtin_kill_buffer(&mut eval, vec![Value::Buffer(id)]).unwrap();
    assert!(result.is_nil());
}

// ----- erase-buffer -----

#[test]
fn erase_buffer_clears_content() {
    let mut eval = new_eval();
    eval.buffers
        .current_buffer_mut()
        .unwrap()
        .insert("hello world");
    assert!(eval.buffers.current_buffer().unwrap().text.len() > 0);
    let result = builtin_erase_buffer(&mut eval, vec![]).unwrap();
    assert!(result.is_nil());
    assert_eq!(eval.buffers.current_buffer().unwrap().text.len(), 0);
}

#[test]
fn erase_buffer_empty_buffer() {
    let mut eval = new_eval();
    let result = builtin_erase_buffer(&mut eval, vec![]).unwrap();
    assert!(result.is_nil());
}

// ----- buffer-size -----

#[test]
fn buffer_size_empty() {
    let mut eval = new_eval();
    let result = builtin_buffer_size(&mut eval, vec![]).unwrap();
    assert!(matches!(result, Value::Int(0)));
}

#[test]
fn buffer_size_with_content() {
    let mut eval = new_eval();
    eval.buffers.current_buffer_mut().unwrap().insert("hello");
    let result = builtin_buffer_size(&mut eval, vec![]).unwrap();
    assert!(matches!(result, Value::Int(5)));
}

#[test]
fn buffer_size_specific_buffer() {
    let mut eval = new_eval();
    let id = eval.buffers.create_buffer("sized");
    eval.buffers.get_mut(id).unwrap().insert("abc");
    let result = builtin_buffer_size(&mut eval, vec![Value::Buffer(id)]).unwrap();
    assert!(matches!(result, Value::Int(3)));
}

#[test]
fn buffer_size_multibyte() {
    let mut eval = new_eval();
    // Chinese chars: 3 bytes each in UTF-8, but 1 character each.
    eval.buffers
        .current_buffer_mut()
        .unwrap()
        .insert("\u{4f60}\u{597d}"); // 2 characters
    let result = builtin_buffer_size(&mut eval, vec![]).unwrap();
    assert!(matches!(result, Value::Int(2)));
}

// =======================================================================
// Integration tests
// =======================================================================

#[test]
fn erase_and_check_size() {
    let mut eval = new_eval();
    eval.buffers
        .current_buffer_mut()
        .unwrap()
        .insert("some content");
    let size_before = builtin_buffer_size(&mut eval, vec![]).unwrap();
    assert!(matches!(size_before, Value::Int(n) if n > 0));

    builtin_erase_buffer(&mut eval, vec![]).unwrap();

    let size_after = builtin_buffer_size(&mut eval, vec![]).unwrap();
    assert!(matches!(size_after, Value::Int(0)));
}

#[test]
fn modified_flag_lifecycle() {
    let mut eval = new_eval();

    // Initially not modified.
    let m1 = builtin_buffer_modified_p(&mut eval, vec![]).unwrap();
    assert!(m1.is_nil());

    // Set modified.
    builtin_set_buffer_modified_p(&mut eval, vec![Value::True]).unwrap();
    let m2 = builtin_buffer_modified_p(&mut eval, vec![]).unwrap();
    assert!(m2.is_truthy());

    // Clear modified.
    builtin_set_buffer_modified_p(&mut eval, vec![Value::Nil]).unwrap();
    let m3 = builtin_buffer_modified_p(&mut eval, vec![]).unwrap();
    assert!(m3.is_nil());
}

#[test]
fn generate_name_and_create() {
    let mut eval = new_eval();
    // *scratch* exists, so generate-new-buffer-name should produce <2>.
    let name =
        builtin_generate_new_buffer_name(&mut eval, vec![Value::string("*scratch*")]).unwrap();
    let name_str = name.as_str().unwrap().to_string();
    assert_ne!(name_str, "*scratch*");

    // Create a buffer with that name.
    let buf = builtin_get_buffer_create(&mut eval, vec![Value::string(&name_str)]).unwrap();
    assert!(matches!(buf, Value::Buffer(_)));

    // It should appear in the buffer list.
    let list = builtin_buffer_list(&mut eval, vec![]).unwrap();
    let items = list_to_vec(&list).unwrap();
    assert!(items.len() >= 2);
}

#[test]
fn buffer_file_name_lifecycle() {
    let mut eval = new_eval();
    let id = eval.buffers.create_buffer("file-buf");

    // Initially no file name.
    let fn1 = builtin_buffer_file_name(&mut eval, vec![Value::Buffer(id)]).unwrap();
    assert!(fn1.is_nil());

    // Set file name.
    eval.buffers.get_mut(id).unwrap().file_name = Some("/tmp/test.el".to_string());
    let fn2 = builtin_buffer_file_name(&mut eval, vec![Value::Buffer(id)]).unwrap();
    assert_eq!(fn2.as_str().unwrap(), "/tmp/test.el");
}
