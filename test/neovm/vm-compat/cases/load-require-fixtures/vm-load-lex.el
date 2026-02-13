;; -*- lexical-binding: t; -*-
(let ((x 7))
  (setq vm-load-lex-fn (lambda () x)))
(setq vm-load-lex-lexical lexical-binding)
(provide 'vm-load-lex)
