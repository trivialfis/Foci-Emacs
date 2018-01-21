;;; lisp-trivialfis --- Summary -*- lexical-binding: t -*-
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
