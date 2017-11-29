;;; go-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'lsp-mode)
(require 'lsp-go)

(defun trivialfis/go ()
  "Go configuration."
  (lsp-go-enable))

(provide 'go-trivialfis)
;;; go-trivialfis.el ends here
