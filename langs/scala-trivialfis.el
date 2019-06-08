;;; scala-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(use-package programming-trivialfis)
(require 'lsp-trivialfis)

;; Super slow at initialization.
(defun trivialfis/scala ()
  "Configuration for scala."
  (trivialfis/lsp)
  (lsp)
  (lsp-ui-mode))

(provide 'scala-trivialfis)
;;; scala-trivialfis.el ends here
