;;; scala-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(use-package programming-trivialfis)
(require 'lsp-trivialfis)
(use-package company-lsp)

;; Super slow at initialization.
(defun trivialfis/scala ()
  "Configuration for scala."
  (trivialfis/lsp)
  (trivialfis/programming-init)
  (lsp)
  (lsp-ui-mode)
  (flycheck-mode 1))

(provide 'scala-trivialfis)
;;; scala-trivialfis.el ends here
