;;; oracle_eval.el --- GNU Emacs oracle evaluator for NeoVM compatibility -*- lexical-binding: nil; -*-

(let ((forms-file (getenv "NEOVM_FORMS_FILE")))
  (unless (and forms-file (file-readable-p forms-file))
    (princ "ERR (:invalid-input \"NEOVM_FORMS_FILE missing or unreadable\")")
    (terpri)
    (kill-emacs 2))

  (with-temp-buffer
    (insert-file-contents forms-file)
    (goto-char (point-min))

    (let ((index 0)
          form)
      (condition-case nil
          (while t
            (setq form (read (current-buffer)))
            (setq index (1+ index))
            (princ (number-to-string index))
            (princ "\t")
            (prin1 form)
            (princ "\t")
            (condition-case err
                (let ((value (eval form nil)))
                  (princ "OK ")
                  (prin1 value))
              (error
               (princ "ERR ")
               (prin1 (list (car err) (cdr err)))))
            (terpri))
        (end-of-file nil)))))
