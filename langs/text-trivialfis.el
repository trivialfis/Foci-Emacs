;;; text-trivialfis --- Summary
;;; Commentary:
;;; Code:
(require 'company)
(defun trivialfis/text ()
  "Configuration for normal text."
  (flyspell-mode 1)
  (add-to-list 'company-backends 'company-ispell))
(provide 'text-trivialfis)
;;; text-trivialfis.el ends here
