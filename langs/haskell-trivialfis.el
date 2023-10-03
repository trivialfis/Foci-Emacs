;;; haskell-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package lsp-mode
  :defer t
  :commands lsp
  :autoload lsp-find-references)
(use-package lsp-trivialfis
  :defer t
  :autoload trivialfis/lsp)
(use-package lsp-haskell
  :defer t
  :config
  (setq lsp-haskell-server-args `("-l" ,lsp-haskell-server-log-file "-j" ,(number-to-string (num-processors)))))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references)
  (trivialfis/lsp))

(use-package yassnippet
  :autoload yas-minor-mode)
(use-package company)

(defun trivialfis/haskell ()
  "Configuration for haskell."
  (lsp)
  (lsp-ui-mode)
  (yas-minor-mode))

(provide 'haskell-trivialfis)
;;; haskell-trivialfis.el ends here
