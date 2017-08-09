;;; lisp-trivialfis --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'company)
(require 'slime)
(require 'slime-company)

(defun trivialfis/lisp ()
  "Basic configuration for Lisp."
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-company-completion 'fuzzy
	slime-contribs '(slime-fancy
			 slime-company
			 slime-scratch
			 slime-editing-commands))
  (add-to-list 'company-backends 'company-slime)

  (flycheck-mode 1)
  
  (define-key slime-mode-map (kbd "C-c h o") 'slime-documentation-lookup)
  (define-key slime-mode-map (kbd "C-c C-b") 'slime-eval-buffer)
  (define-key slime-mode-map (kbd "C-c C-a") 'slime-compile-file)
  (slime-setup)
  (slime-mode 1)
  (unless (slime-connected-p)
    (slime)))

(provide 'lisp-trivialfis)
;;; lisp-trivialfis ends here
