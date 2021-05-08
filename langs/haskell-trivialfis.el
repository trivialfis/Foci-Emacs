;;; haskell-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(use-package lsp)
(use-package lsp-trivialfis)

(defun trivialfis/haskell ()
  "Configuration for haskell."
  (trivialfis/lsp)
  (lsp)
  (lsp-ui-mode))

(provide 'haskell-trivialfis)
;;; haskell-trivialfis.el ends here
