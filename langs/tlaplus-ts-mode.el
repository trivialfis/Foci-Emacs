;; tlaplus-ts-mode --- Summary -*- lexical-binding: t -*-
;;;
;;; Copyright 2024 Jiamingy <jm.yuan@outlook.com>
;;;
;;; Commentary:
;;; Code:

(require 'flycheck)
(require 'treesit)

(defcustom tla+/toolbox-jar ""
  "The path to the toolbox's jar file."
  :type 'string
  :group 'tlaplus)

(defcustom tla+/apalache-jar ""
  "The path to the apalache's jar file."
  :type 'string
  :group 'tlaplus)

(use-package comint
  :defer t
  :commands
  comint-check-proc
  make-comint-in-buffer)


(defvar-local tla+/comint-buffer "*Tla+*")

;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
(defun run-tla+-repl ()
  "Run the TLA+ REPL."
  (interactive)
  (let* ((tla+/arguments `("-cp" ,tla+/toolbox-jar "tlc2.REPL"))
	 (tla+-program "java")
	 (buffer (get-buffer-create tla+/comint-buffer))
	 (proc-alive (comint-check-proc buffer)))
    ;; (process (get-buffer-process buffer))
    (unless proc-alive
      (with-current-buffer buffer
	(apply 'make-comint-in-buffer "Tla+" buffer
	       tla+-program nil tla+/arguments)))
    (when buffer
      (pop-to-buffer buffer))))

(define-derived-mode tlaplus-ts-mode prog-mode "TLA+[ts]"
  :syntax tlaplus-ts-mode-syntax-table
  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'tlaplus)
    (treesit-parser-create 'tlaplus)
    (tlaplus-ts-setup)))

(defun tla+/flycheck-error-filter (errors)
  "Remove duplicated ERRORS."
  (seq-remove
   (lambda (err)
     (when-let (msg (flycheck-error-message err))
       (message msg)
       (string-match-p "Parsing error: ***" msg)))
   errors))

(flycheck-define-checker tla+/apalache
  "TLA+ checker using apalache."
  :command ("java" "-jar" (eval tla+/apalache-jar) "parse" source)
  :error-patterns
  ;; Encountered "Init" at line 4, column 1 and token "Apalache"
  ((error line-start (message) "at line " line ", column " column (one-or-more not-newline) line-end)
   ;; line 12, col 15 to line 12, col 17 of module BinSearch0
   (error line-start
	  (message)
	  (one-or-more "\n")
	  ;; Actual error
	  line-start "line " line ", col " column " to line " end-line ", col " end-column (one-or-more not-newline)
	  line-end)
   )
  :modes (tla-mode tlaplus-ts-mode)
  ;; Parsing error: *** Errors: 2
  :error-filter tla+/flycheck-error-filter
  :predicate flycheck-buffer-nonempty-p)

(defvar tlaplus-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "()1" table)  ; (* comment starter
    (modify-syntax-entry ?\) ")(4" table)  ; *) comment ender
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?\\ "_ 1" table)  ; \* comment starter, also \A, \in, etc
    (modify-syntax-entry ?* ". 23c" table) ; (* or *) or \*
    (modify-syntax-entry ?\n "> c" table)  ; \* comment ender
    (modify-syntax-entry ?_ "_" table)     ; x_y
    (modify-syntax-entry ?' "_" table)     ; x'
    table))

(defvar tlaplus-ts-mode--keywords
  '("ACTION"
    "ASSUME"
    "ASSUMPTION"
    "AXIOM"
    "BY"
    "CASE"
    "CHOOSE"
    "CONSTANT"
    "CONSTANTS"
    "COROLLARY"
    "DEF"
    "DEFINE"
    "DEFS"
    "ELSE"
    "EXCEPT"
    "EXTENDS"
    "HAVE"
    "HIDE"
    "IF"
    "IN"
    "INSTANCE"
    "LAMBDA"
    "LEMMA"
    "LET"
    "LOCAL"
    "MODULE"
    "NEW"
    "OBVIOUS"
    "OMITTED"
    "ONLY"
    "OTHER"
    "PICK"
    "PROOF"
    "PROPOSITION"
    "PROVE"
    "QED"
    "RECURSIVE"
    "SF_"
    "STATE"
    "SUFFICES"
    "TAKE"
    "TEMPORAL"
    "THEN"
    "THEOREM"
    "USE"
    "VARIABLE"
    "VARIABLES"
    "WF_"
    "WITH"
    "WITNESS")
  "Tla+ keywords for tree-sitter font-locking.")

(defun tla+/set-comment-syntax-vars ()
  (setq-local comment-start "\\*")
  (setq-local comment-end "")
  (setq-local comment-use-syntax t)
  (setq-local comment-start-skip "\\(\\\\\\*\\)\\s *"))


(defvar tlaplus-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'tlaplus
   :feature 'bracket
   '((["(" ")" "[" "]" "]_" "{" "}"]) @font-lock-bracket-face)

   :language 'tlaplus
   :feature 'comment
   '((comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)

   :language 'tlaplus
   :feature 'keyword
   `([,@tlaplus-ts-mode--keywords] @font-lock-keyword-face)

   :language 'tlaplus
   :feature 'case-box
   '((case_box) @font-lock-keyword-face)

   :language 'tlaplus
   :feature 'string
   '((string) @font-lock-string-face)))

(defun tlaplus-ts-setup()
  "Setup treesit for tlaplus-ts-mode."
  (setq-local treesit-font-lock-settings tlaplus-ts-mode--font-lock-settings)
  (font-lock-add-keywords 'tlaplus-ts-mode
			  '(("UNCHANGED" . 'font-lock-keyword-face)
			    ("DOMAIN" . 'font-lock-keyword-face)
			    ("SUBSET" . 'font-lock-keyword-face)
			    ("@" . 'font-lock-builtin-face)
			    ("\\\\A" . 'font-lock-builtin-face)
			    ("\\\\cup" . 'font-lock-builtin-face)
			    ("\\\\div" . 'font-lock-builtin-face)
			    ("\\\\E" . 'font-lock-builtin-face)
			    ("\\\\in" . 'font-lock-builtin-face)))
  ;; (font-lock-add-keywords 'c-mode
  ;; 			  '(("\\<\\(FIXME\\):" 1 'font-lock-warning-face prepend)
  ;; 			    ("\\<\\(and\\|or\\|not\\)\\>" . 'font-lock-keyword-face)))
  (setq-local treesit-font-lock-feature-list
              '((bracket)
		(comment)
                (keyword string)
		(case-box)
		(string)))
  ;; (setq-local treesit--indent-verbose t)
  ;; (setq-local treesit-simple-indent-rules
  ;; 	      `((tlaplus
  ;; 		 ((parent-is "let_in") parent 2))
  ;; 		(tlaplus
  ;; 		 ((parent-is "operator_definition") parent 2))
  ;; 		(tlaplus
  ;; 		 ((parent-is "constant_declaration") parent 2))))
  (tla+/set-comment-syntax-vars)
  (treesit-major-mode-setup))

(provide 'tlaplus-ts-mode)
;;; tlaplus-ts-mode.el ends here
