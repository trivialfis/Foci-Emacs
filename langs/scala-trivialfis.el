;;; scala-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(use-package programming-trivialfis)
(use-package lsp-trivialfis)
(use-package company-lsp)
(require 'scala-mode)

;; Super slow at initialization.
(defun trivialfis/scala ()
  "Configuration for scala."
  (trivialfis/programming-init)
  (trivialfis/lsp)
  (lsp)
  (lsp-ui-mode)
  (flycheck-mode 1)
  (yas-minor-mode))

(provide 'scala-trivialfis)
;;; scala-trivialfis.el ends here
