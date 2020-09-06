;;; tablegen-mode.el --- Major mode for TableGen description files (part of LLVM project)

;; Maintainer:  The LLVM team, http://llvm.org/

;;; Commentary:
;; A major mode for TableGen description files in LLVM.

(require 'comint)
(require 'custom)
(require 'prog-mode)
(require 'ansi-color)

;; Create mode-specific tables.
;;; Code:

(defvar td-decorators-face 'td-decorators-face
  "Face method decorators.")
(make-face 'td-decorators-face)

(defvar tablegen-font-lock-keywords
  (let ((kw (regexp-opt '("class" "defm" "def" "field" "include" "in"
                         "let" "multiclass" "foreach" "if" "then" "else"
                         "defvar" "defset")
                        'words))
        (type-kw (regexp-opt '("bit" "bits" "code" "dag" "int" "list" "string")
                             'words))
        )
    (list
     ;; Comments
;;     '("\/\/" . font-lock-comment-face)
     ;; Strings
     '("\"[^\"]+\"" . font-lock-string-face)
     ;; Hex constants
     '("\\<0x[0-9A-Fa-f]+\\>" . font-lock-preprocessor-face)
     ;; Binary constants
     '("\\<0b[01]+\\>" . font-lock-preprocessor-face)
     ;; Integer literals
     '("\\<[-]?[0-9]+\\>" . font-lock-preprocessor-face)
     ;; Floating point constants
     '("\\<[-+]?[0-9]+\.[0-9]*\([eE][-+]?[0-9]+\)?\\>" . font-lock-preprocessor-face)

     '("^[ \t]*\\(@.+\\)" 1 'td-decorators-face)
     ;; Keywords
     kw
     ;; Type keywords
     type-kw
     ))
  "Additional expressions to highlight in TableGen mode.")
(put 'tablegen-mode 'font-lock-defaults '(tablegen-font-lock-keywords))

;; ---------------------- Syntax table ---------------------------

(defvar tablegen-mode-syntax-table nil
  "Syntax table used in `tablegen-mode' buffers.")
(when (not tablegen-mode-syntax-table)
  (setq tablegen-mode-syntax-table (make-syntax-table))
  ;; whitespace (` ')
  (modify-syntax-entry ?\   " "      tablegen-mode-syntax-table)
  (modify-syntax-entry ?\t  " "      tablegen-mode-syntax-table)
  (modify-syntax-entry ?\r  " "      tablegen-mode-syntax-table)
  (modify-syntax-entry ?\n  " "      tablegen-mode-syntax-table)
  (modify-syntax-entry ?\f  " "      tablegen-mode-syntax-table)
  ;; word constituents (`w')
  (modify-syntax-entry ?\%  "w"      tablegen-mode-syntax-table)
  (modify-syntax-entry ?\_  "w"      tablegen-mode-syntax-table)
  ;; comments
  (modify-syntax-entry ?/   ". 124b" tablegen-mode-syntax-table)
  (modify-syntax-entry ?*   ". 23"   tablegen-mode-syntax-table)
  (modify-syntax-entry ?\n  "> b"    tablegen-mode-syntax-table)

  (modify-syntax-entry ?\"  "\""     tablegen-mode-syntax-table)
  )

;; --------------------- Abbrev table -----------------------------

(defvar tablegen-mode-abbrev-table (make-abbrev-table)
  "Abbrev table used while in TableGen mode.")
(defvar tablegen-mode-hook nil)

(toggle-debug-on-error)

(defun tablegen-variables ()
  "Major mode for editing TableGen description files.
\\{tablegen-mode-map}
  Runs `tablegen-mode-hook' on startup."
  ;; (kill-all-local-variables)
  ;; (use-local-map tablegen-mode-map)      ; Provides the local keymap.
  (make-local-variable 'font-lock-defaults)
  (setq major-mode 'tablegen-mode        ; This is how describe-mode
					;   finds the doc string to print.
	mode-name             "TableGen" ; This name goes into the modeline.
	font-lock-defaults    `(tablegen-font-lock-keywords)
	require-final-newline t
        )

  (set-syntax-table tablegen-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (setq indent-tabs-mode nil)
  (run-hooks 'tablegen-mode-hook))       ; Finally, this permits the user to
					;   customize the mode with a hook.

;;;###autoload
(define-derived-mode tablegen-mode prog-mode "TableGen"
  "Major mode for editing TableGen description files.
\\{tablegen-mode-map}
  Runs `tablegen-mode-hook' on startup."
  :group 'tablegen-mode
  :syntax-table tablegen-mode-syntax-table
  :abbrev-table tablegen-mode-abbrev-table

  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local electric-indent-chars '(?\n ?{ ?} ?\[ ?\] ?\( ?\)))

  (tablegen-variables))


;; Associate .td files with tablegen-mode
;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.td\\'")  'tablegen-mode))

(provide 'tablegen-mode)

;;; tablegen-mode.el ends here
