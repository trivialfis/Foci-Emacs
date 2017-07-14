;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package s
  :commands (s-chop-suffix))

(defun trivialfis/elisp()
  (flycheck-mode 1)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (when (eq (buffer-size) 0)
    (insert ";;; " (buffer-name) " --- Summary\n"
	    ";;; Commentary:\n"
	    ";;; Code:"
	    "\n\n\n"
	    "(provide '" (s-chop-suffix ".el" (buffer-name)) ")\n"
	    ";;; " (buffer-name) " ends here"))

  (local-set-key (kbd "C-c C-a") 'byte-compile-file)
  (local-set-key (kbd "C-c C-b") 'eval-buffer)
  (aggressive-indent-mode))

;; Semantic mode doesn't play well with elisp
;; (autoload 'semantic-add-system-include "semantic")
;; (semantic-add-system-include "~/.emacs.d/elpa/" 'emacs-lisp-mode)
;; (require 'srefactor-lisp)
;; (autoload 'srefactor-lisp-format-sexp "srefactor-lisp" t)
;; (autoload 'srefactor-lisp-one-line "srefactor-lisp" t)
;; (autoload 'srefactor-lisp-format-defun "srefactor-lisp" t)
;; (autoload 'srefactor-lisp-format-buffer "srefactor-lisp" t)

;; (local-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
;; (local-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
;; (local-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
;; (local-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)
;; (custom-semantic-mode 'emacs-lisp-mode)

;;; elisp-trivialfis.el ends here
