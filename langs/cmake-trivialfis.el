;;; cmake-trivialfis.el --- Summary
;;; Commentary:
;;; Code:
(require 'lsp)
(require 'lsp-trivialfis)

(push '(cmake-mode . "cmake")
      lsp-language-id-configuration)

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection (list "lsp-cmaked"))
  :major-modes '(cmake-mode)
  :priority 1
  :multi-root t
  :server-id 'cmake-lsp))

(defun trivialfis/cmake ()
  "CMake configuration."
  (trivialfis/lsp)
  ;; (lsp)
  ;; (lsp-ui-mode)
  )

(provide 'cmake-trivialfis)
;;; cmake-trivialfis.el ends here
