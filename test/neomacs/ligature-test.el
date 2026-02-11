;;; ligature-test.el --- Test font ligature rendering -*- lexical-binding: t -*-

;; Test whether programming font ligatures render correctly in neomacs.
;; Uses JetBrains Mono which has extensive ligature support.
;; Usage: ./src/emacs -Q -l test/neomacs/ligature-test.el
;;
;; Common ligature sequences in programming fonts:
;;   ->  =>  !=  ==  ===  >=  <=  <>  |>  <|  >>  <<
;;   ::  ..  ...  //  /*  */  ;;  --  ++  **  ~~  %%
;;   www  <!--  -->  |||  &&  ||  ?:  ?.  ..=  =>>

;;; Code:

;; Set font to JetBrains Mono (has ligatures)
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 140)

(defun ligature-test--insert-section (title)
  "Insert section TITLE."
  (insert "\n")
  (let ((start (point)))
    (insert (format "=== %s ===\n" title))
    (put-text-property start (point) 'face '(:weight bold :foreground "gold"))))

(defun ligature-test--insert-pair (label text)
  "Insert LABEL and ligature TEXT sample."
  (insert (format "  %-30s  " label))
  (let ((start (point)))
    (insert text)
    (put-text-property start (point) 'face '(:foreground "cyan" :height 1.5)))
  (insert "\n"))

(defun ligature-test ()
  "Create ligature test buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Ligature Test*")))
    (switch-to-buffer buf)
    (erase-buffer)

    (let ((start (point)))
      (insert "FONT LIGATURE RENDERING TEST\n")
      (put-text-property start (point) 'face '(:weight bold :height 1.8 :foreground "cyan")))
    (insert (format "Font: %s\n" (face-attribute 'default :family)))
    (insert (format "Window system: %s\n" window-system))
    (insert (make-string 70 ?-) "\n")
    (insert "\n")
    (insert "If ligatures work, multi-character sequences below should render as\n")
    (insert "SINGLE connected glyphs, not separate characters.\n")

    ;; Arrow ligatures
    (ligature-test--insert-section "ARROWS")
    (ligature-test--insert-pair "right arrow:"        "->")
    (ligature-test--insert-pair "fat arrow:"          "=>")
    (ligature-test--insert-pair "left arrow:"         "<-")
    (ligature-test--insert-pair "left fat arrow:"     "<=")
    (ligature-test--insert-pair "bidirectional:"      "<->")
    (ligature-test--insert-pair "fat bidirectional:"  "<=>")
    (ligature-test--insert-pair "long right arrow:"   "-->")
    (ligature-test--insert-pair "long left arrow:"    "<--")
    (ligature-test--insert-pair "pipe forward:"       "|>")
    (ligature-test--insert-pair "pipe backward:"      "<|")

    ;; Comparison ligatures
    (ligature-test--insert-section "COMPARISON / EQUALITY")
    (ligature-test--insert-pair "not equal:"          "!=")
    (ligature-test--insert-pair "not identical:"      "!==")
    (ligature-test--insert-pair "equal:"              "==")
    (ligature-test--insert-pair "identical:"          "===")
    (ligature-test--insert-pair "greater-equal:"      ">=")
    (ligature-test--insert-pair "less-equal:"         "<=")
    (ligature-test--insert-pair "diamond:"            "<>")
    (ligature-test--insert-pair "spaceship:"          "<=>")

    ;; Logical ligatures
    (ligature-test--insert-section "LOGICAL OPERATORS")
    (ligature-test--insert-pair "and:"                "&&")
    (ligature-test--insert-pair "or:"                 "||")
    (ligature-test--insert-pair "triple or:"          "|||")
    (ligature-test--insert-pair "ternary:"            "?:")
    (ligature-test--insert-pair "null coalesce:"      "??")
    (ligature-test--insert-pair "optional chain:"     "?.")

    ;; Shift / stream ligatures
    (ligature-test--insert-section "SHIFT / STREAM")
    (ligature-test--insert-pair "left shift:"         "<<")
    (ligature-test--insert-pair "right shift:"        ">>")
    (ligature-test--insert-pair "heredoc:"            "<<<")
    (ligature-test--insert-pair "right shift assign:" ">>=")
    (ligature-test--insert-pair "left shift assign:"  "<<=")

    ;; Scope / type ligatures
    (ligature-test--insert-section "SCOPE / TYPE")
    (ligature-test--insert-pair "scope:"              "::")
    (ligature-test--insert-pair "assign type:"        ":=")
    (ligature-test--insert-pair "range:"              "..")
    (ligature-test--insert-pair "spread:"             "...")
    (ligature-test--insert-pair "range inclusive:"     "..=")

    ;; Comment ligatures
    (ligature-test--insert-section "COMMENTS / STRINGS")
    (ligature-test--insert-pair "line comment:"       "//")
    (ligature-test--insert-pair "block comment open:" "/*")
    (ligature-test--insert-pair "block comment close:" "*/")
    (ligature-test--insert-pair "HTML comment open:"  "<!--")
    (ligature-test--insert-pair "HTML comment close:" "-->")
    (ligature-test--insert-pair "hash-bang:"          "#!")
    (ligature-test--insert-pair "string escape:"      "\\n")

    ;; Arithmetic ligatures
    (ligature-test--insert-section "ARITHMETIC / MISC")
    (ligature-test--insert-pair "increment:"          "++")
    (ligature-test--insert-pair "decrement:"          "--")
    (ligature-test--insert-pair "exponent:"           "**")
    (ligature-test--insert-pair "tilde-tilde:"        "~~")
    (ligature-test--insert-pair "plus-equal:"         "+=")
    (ligature-test--insert-pair "minus-equal:"        "-=")
    (ligature-test--insert-pair "multiply-equal:"     "*=")
    (ligature-test--insert-pair "divide-equal:"       "/=")

    ;; Haskell / functional
    (ligature-test--insert-section "HASKELL / FUNCTIONAL")
    (ligature-test--insert-pair "bind:"               ">>="
    )
    (ligature-test--insert-pair "apply:"              "<*>")
    (ligature-test--insert-pair "fmap:"               "<$>")
    (ligature-test--insert-pair "alternative:"        "<|>")
    (ligature-test--insert-pair "monad:"              ">=>")
    (ligature-test--insert-pair "kleisli:"            "<=<")
    (ligature-test--insert-pair "type constraint:"    "=>")

    ;; Real code samples
    (ligature-test--insert-section "REAL CODE SAMPLES")
    (insert "\n")
    (let ((start (point)))
      (insert "  // Rust\n")
      (insert "  fn main() -> Result<(), Box<dyn Error>> {\n")
      (insert "      let x = vec![1, 2, 3];\n")
      (insert "      let y = x.iter().map(|&n| n != 0 && n >= 2).collect::<Vec<_>>();\n")
      (insert "      x == y || x <= y;\n")
      (insert "      println!(\"result: {:?}\", y);\n")
      (insert "  }\n")
      (insert "\n")
      (insert "  // Haskell\n")
      (insert "  main :: IO ()\n")
      (insert "  main = getLine >>= putStrLn . map toUpper\n")
      (insert "  compose = (.) . (.)\n")
      (insert "\n")
      (insert "  // JavaScript\n")
      (insert "  const fn = (x) => x !== null ? x?.value ?? 0 : -1;\n")
      (insert "  const cmp = a === b || a >= c && a <= d;\n")
      (put-text-property start (point) 'face '(:family "JetBrains Mono")))

    (insert "\n")
    (ligature-test--insert-section "VERDICT")
    (insert "\n")
    (insert "  If you see connected/merged glyphs for sequences like -> => != ==\n")
    (insert "  then ligatures ARE working.\n")
    (insert "\n")
    (insert "  If -> looks like two separate chars '-' and '>' then ligatures\n")
    (insert "  are NOT working (character-by-character rendering).\n")

    (goto-char (point-min))
    (setq buffer-read-only t)
    (message "Ligature test ready. Check if multi-char sequences render as single glyphs.")))

(ligature-test)

;;; ligature-test.el ends here
