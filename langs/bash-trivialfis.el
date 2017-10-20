;;; bash-trivialfis --- Summary
;;; Commentary:
;;; Code:
(require 'company)
(require 'company-shell)

(defun trivialfis/bash()
  (require 'company-shell)
  (add-to-list 'company-backends 'company-shell)
  (setq company-shell-use-help-arg t)
  (flycheck-mode 1)
  )
(provide 'bash-trivialfis)
;;; bash-trivialfis.el ends here
