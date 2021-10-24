;;; prolog-trivialfis.el --- Summary
;;; Commentary:
;;; Code:
(require 'programming-trivialfis)
(require 'lsp-trivialfis)
(require 'lsp)

;; https://github.com/jamesnvc/lsp_server
(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection (list "swipl"
                              "-g" "use_module(library(lsp_server))."
                              "-g" "lsp_server:main"
                              "-t" "halt"
                              "--" "stdio"))
  :major-modes '(prolog-mode)
  :priority 1
  :multi-root t
  :server-id 'prolog-ls))

(defun trivialfis/prolog ()
  "Prolog configuration."
  (trivialfis/lsp)
  (lsp)
  (lsp-ui-mode))

(provide 'prolog-trivialfis)
;;; prolog-trivialfis.el ends here
