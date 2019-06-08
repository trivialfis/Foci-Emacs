;;; java-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'company-lsp)
(require 'programming-trivialfis)

(use-package lsp-trivialfis)
(use-package lsp-java)
(require 'lsp-ui)

(defun trivialfis/java ()
  "Java configuration."
  ;; Don't know why prog mode isn't loaded.
  (c-set-style "gnu")
  (setq lsp-before-save-edits nil)
  (trivialfis/programming-init)
  (flycheck-mode 1)
  ;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (push 'company-lsp company-backends)
  (trivialfis/lsp)
  ;; (lsp)
  ;; (lsp-ui-mode)
  )

(provide 'java-trivialfis)
;;; java-trivialfis.el ends here
