;;; alloy-trivialfis.el --- Configuration for Alloy 6 -*- lexical-binding: t -*-
;;;
;;; Copyright 2026 Jiamingy <jm.yuan@outlook.com>
;;;
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package programming-trivialfis)

(defvar treesit-simple-indent-rules)

(defvar trivialfis/alloy--indent-rules
  '((alloy
     ;; Closing delimiters must be tested before container rules; otherwise
     ;; fact/pred/fun block closing braces indent as body expressions.
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ;; Top-level: no indent
     ((parent-is "source_file") column-0 0)
     ;; `sig_body' starts at the first field, so anchor sibling fields to the
     ;; enclosing signature instead of to the previous field.
     ((parent-is "sig_body") grand-parent alloy-ts-mode-indent-offset)
     ;; Inside blocks/braces: indent
     ((parent-is "block_expression") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "set_comprehension") parent-bol alloy-ts-mode-indent-offset)
     ;; Parameters
     ((parent-is "decl_list") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "expr_list") parent-bol alloy-ts-mode-indent-offset)
     ;; General continuation
     ((parent-is "sig_decl") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "pred_decl") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "fun_decl") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "fact_decl") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "assert_decl") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "command") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "enum_decl") parent-bol alloy-ts-mode-indent-offset)
     ;; Default: no change
     (no-node parent-bol 0)))
  "Local indentation rules for `alloy-ts-mode'.")

(defun trivialfis/alloy--configure-indentation ()
  "Install local tree-sitter indentation rules for Alloy."
  (setq-local treesit-simple-indent-rules trivialfis/alloy--indent-rules))

(use-package alloy-ts-mode
  :defer t
  :commands alloy-ts-mode
  :hook (alloy-ts-mode . trivialfis/alloy--configure-indentation))

(use-package lsp-trivialfis
  :autoload trivialfis/lsp)

(use-package lsp-mode
  :defer t
  :commands lsp lsp-deferred
  :config (trivialfis/lsp)
  :autoload lsp-find-references)

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references))

(use-package alloy-lsp
  :defer t
  :commands alloy-lsp-ensure
  :custom
  ;; Use the nix-installed `alloy6' stdio LSP rather than letting alloy-lsp.el
  ;; auto-download the upstream JAR into the package's source dir.
  (alloy-lsp-server-command '("alloy6" "lsp"))
  :config
  ;; alloy-lsp.el registers an LSP client but never publishes a languageId,
  ;; so lsp-mode warns "Unable to calculate the languageId" on every .als open.
  (add-to-list 'lsp-language-id-configuration '(alloy-ts-mode . "alloy"))
  (add-to-list 'lsp-language-id-configuration '(alloy-mode    . "alloy")))

(defun trivialfis/alloy ()
  "Configuration for Alloy 6 buffers."
  (alloy-ts-mode)
  (lsp-ui-mode)
  (alloy-lsp-ensure))

(provide 'alloy-trivialfis)
;;; alloy-trivialfis.el ends here
