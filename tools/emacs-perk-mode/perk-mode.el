 ;;; perk-mode.el --- Major mode for Perk language -*- lexical-binding: t; -*-

(defvar perk-font-lock-keywords
  (let* (
         ;; Keywords
         (perk-keywords
          '("let" "import" "open" "extern" "archetype" "model" "struct"
            "private" "public" "fun" "for" "while" "do" "if" "then" "else"
            "return" "summon" "banish" "make" "cast" "of" "just" "nothing"))

         ;; Types
         (perk-types
          '("int" "void" "uint8_t" "uint16_t" "uint32_t" "uint64_t"
            "float" "double" "bool" "char"))

         ;; Embedded C markers
         (perk-c-markers
          '("BEGIN_C" "END_C"))

         ;; Regex helpers
         (perk-keywords-regexp (regexp-opt perk-keywords 'symbols))
         (perk-types-regexp    (regexp-opt perk-types 'symbols))
         (perk-c-markers-regexp (regexp-opt perk-c-markers 'symbols))
         )

    `(
      ;; Keywords
      (,perk-keywords-regexp . font-lock-keyword-face)

      ;; Types
      (,perk-types-regexp . font-lock-type-face)

      ;; Embedded C markers
      (,perk-c-markers-regexp . font-lock-preprocessor-face)

      ;; Function name after 'fun'
      ("\\_<fun\\_>[ \t]+\\(\\_<[A-Za-z0-9_]+\\_>\\)"
       1 font-lock-function-name-face)

      ;; Variable name after 'let' and before ':'
      ("\\_<let\\_>[ \t]+\\(\\_<[A-Za-z0-9_]+\\_>\\)[ \t]*:"
       1 font-lock-variable-name-face)

      ;; TODO inside comments
      ("\\_<TODO\\_>" . font-lock-warning-face)

      ;; Hex numbers
      ("\\_<0x[0-9A-Fa-f]+\\_>" . font-lock-constant-face)

      ;; Octal numbers
      ("\\_<0o[0-7]+\\_>" . font-lock-constant-face)

      ;; Binary numbers
      ("\\_<0b[01]+\\_>" . font-lock-constant-face)

      ;; Floating point numbers
      ("\\_<[0-9]+\\.[0-9]+\\_>" . font-lock-constant-face)

      ;; Integer numbers
      ("\\_<[0-9]+\\_>" . font-lock-constant-face)
      )))

;; Syntax table
(defvar perk-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; C++-style comments //
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)

    ;; Underscore as word constituent
    (modify-syntax-entry ?_ "w" st)

    st))

;;;###autoload
(define-derived-mode perk-mode prog-mode "Perk"
  "Major mode for editing Perk language files."
  :syntax-table perk-mode-syntax-table
  (setq-local font-lock-defaults '(perk-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.perk\\'" . perk-mode))

(provide 'perk-mode)
;;; perk-mode.el ends here

