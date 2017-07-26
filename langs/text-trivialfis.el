;;; text-trivialfis --- Configuration for normal text file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'company)
(eval-when-compile			; Get rid of the free reference
  (defvar flyspell-mode-map))

(defun trivialfis/text ()
  "Configuration for normal text."
  (flyspell-mode 1)
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)
  (add-to-list 'company-backends 'company-ispell))

(provide 'text-trivialfis)
;;; text-trivialfis.el ends here
