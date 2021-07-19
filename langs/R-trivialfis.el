;;; R-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'ess)
(require 'lsp-trivialfis)
(use-package lsp)
(use-package ess-site)

;; Not quite usable yet.
(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection '("R" "--slave" "-e" "languageserver::run()"))
  :major-modes '(ess-r-mode inferior-ess-r-mode)
  :server-id 'lsp-R))

(defun trivialfis/R ()
  "R configuration."
  (setq ess-indent-offset 2
	ess-indent-level 2)
  (flymake-mode 0)
  (flycheck-mode 1)
  ;; (lsp)
  ;; (lsp-ui-mode)
  )
(provide 'R-trivialfis)
;;; R-trivialfis.el ends here
