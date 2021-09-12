;;; rust-trivialfis.el --- Summary
;;;
;;; Copyright © 2016-2018 Fis Trivial <ybbs.daans@hotmail.com>
;;;
;;; This file is part of Foci-Emacs.
;;;
;;; Foci-Emacs is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Foci-Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Foci-Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;

;;; Commentary:
;; Notes:
;; Explain errors¶
;; Flycheck also has the ability to display explanations for errors, provided the error checker is capable of producing these explanations.
;; Currently, only the rust and rust-cargo checkers produce explanations.
;; C-c ! e
;; M-x flycheck-explain-error-at-point

;; Display an explanation for the first explainable error at point.

;;; Code:

(require 'programming-trivialfis)
(require 'rust-mode)
(require 'racer)
(require 'company)
(require 'flycheck)
(use-package lsp-trivialfis)

(defun trivialfis/rust-compile ()
  "Compile rust code using cargo."
  (interactive)
  (let ((compile-buf
	 (get-buffer-create "*Compile log*")))
    (set-buffer compile-buf)
    (goto-char (point-max))
    (insert (concat
	     (shell-command-to-string "cargo build")
	     "\n\n"))
    (display-buffer compile-buf)))

(defun trivialfis/rust ()
  "Common configuration for rust mode."
  ;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  ;; (add-hook 'racer-mode-hook 'eldoc-mode)
  ;; (add-hook 'racer-mode-hook 'company-mode)

  ;; (setq company-backends '((company-capf
  ;; 			    company-dabbrev-code
  ;; 			    company-yasnippet
  ;; 			    company-files
  ;; 			    company-keywords)))
  ;; (flycheck-mode 1)
  ;; (racer-mode 1)

  ;; (define-key rust-mode-map (kbd "C-c C-a") #'trivialfis/rust-compile)
  ;; (setq company-tooltip-align-annotations t)
  (trivialfis/lsp)
  (lsp)
  (lsp-ui-mode)
  )

(provide 'rust-trivialfis)
;;; rust-trivialfis.el ends here
