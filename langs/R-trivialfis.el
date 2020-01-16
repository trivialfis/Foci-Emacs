;;; R-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'ess)
(require 'lsp-trivialfis)
(use-package lsp)

;; Not quite usable yet.
(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection '("R" "--slave" "-e" "languageserver::run()"))
  :major-modes '(ess-r-mode inferior-ess-r-mode)
  :server-id 'lsp-R))

(defun trivialfis/R ()
  "R configuration."
  (set-default ess-indent-offset 2)
  (toggle-debug-on-error)
  ;; (lsp)
  ;; (lsp-ui-mode)
  )
(provide 'R-trivialfis)
;;; R-trivialfis.el ends here
