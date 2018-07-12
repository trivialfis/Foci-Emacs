;;; makefile-trivialfis.el --- Summary
;;; Commentary:
;;; Code:
(require 'make-mode)

(defun trivialfis/makefile ()
  "Makefile configuration."
  (local-set-key (kbd "C-M-n") 'makefile-next-dependency)
  (local-set-key (kbd "C-M-p") 'makefile-previous-dependency)
  (local-set-key (kbd "M-n") 'highlight-symbol-next)
  (local-set-key (kbd "M-p") 'highlight-symbol-prev))

(provide 'makefile-trivialfis)
;;; makefile-trivialfis.el ends here
