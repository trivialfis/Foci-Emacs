;;; haskell-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :defer t
  :commands lsp
  :autoload
  lsp-find-references)
(use-package lsp-trivialfis
  :defer t
  :autoload trivialfis/lsp)

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references)
  (trivialfis/lsp))

(defun trivialfis/haskell ()
  "Configuration for haskell."
  (lsp)
  (lsp-ui-mode))

(provide 'haskell-trivialfis)
;;; haskell-trivialfis.el ends here
