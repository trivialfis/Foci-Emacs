;;; text-trivialfis --- Configuration for normal text file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'company)
(eval-when-compile			; Get rid of the free reference
  (defvar flyspell-mode-map))

(defun trivialfis/_text ()
  "Configuration for normal text."
  (flyspell-mode 1)
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)
  (add-to-list 'company-backends 'company-ispell))

(defun trivialfis/text ()
  "Guard for trivialfis/_text."
  (when (or (eq major-mode 'text-mode)
	    (eq major-mode 'org-mode)
	    (eq major-mode 'markdown-mode))
    (trivialfis/_text)))

(provide 'text-trivialfis)
;;; text-trivialfis.el ends here
