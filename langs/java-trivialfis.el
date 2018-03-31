;;; java-trivialfis.el --- Summary
;;; Commentary:
;;; Code:
(require 'lsp-java)
(require 'lsp-mode)
(require 'programming-trivialfis)

(defun trivialfis/java ()
  "Java configuration."
  ;; Don't know why prog mode isn't loaded.
  (trivialfis/programming-init)
  (lsp-java-enable))

(provide 'java-trivialfis)
;;; java-trivialfis.el ends here
