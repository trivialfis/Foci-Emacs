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
  (trivialfis/lsp)
  (lsp)
  (lsp-ui-mode))

(provide 'java-trivialfis)
;;; java-trivialfis.el ends here
