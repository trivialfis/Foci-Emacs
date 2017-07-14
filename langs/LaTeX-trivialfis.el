;;; latex-trivialfis --- Summary
;;; Commentary:
;;; Code:

(require 'tex)
(require 'latex)
(require 'company)
(defun trivialfis/LaTeX()
  "Somehow the company-latex-commands conflicts with the other two latex backends"
  ;; (auto-make-header)
  (LaTeX-math-mode)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-latex-commands)
  (flycheck-mode 1)
  )
(provide 'latex-trivialfis)
;;; latex-trivialfis ends here
