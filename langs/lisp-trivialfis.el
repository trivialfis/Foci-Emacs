;;; lisp-trivialfis --- Summary
;;; Commentary:
;;; Code:
(require 'company)
(require 'slime)
(require 'slime-company)

(defun trivialfis/lisp ()
  "Configuration for clisp."
  (setq inferior-lisp-program "sbcl")
  (setq slime-company-completion 'fuzzy)
  (add-to-list 'slime-contribs 'slime-fancy)
  (add-to-list 'slime-contribs 'slime-company)
  (add-to-list 'company-backends 'company-slime)

  (flycheck-mode 1)

  (slime-setup)
  (slime-mode 1))
(provide 'lisp-trivialfis)
;;; lisp-trivialfis ends here
