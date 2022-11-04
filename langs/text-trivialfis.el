;;; text-trivialfis --- Configuration for normal text file -*- lexical-binding: t -*-
;;;
;;; Copyright © 2016-2018 Fis Trivial <jm.yuan@outlook.com>
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
(require 'langtool)

(eval-when-compile			; Get rid of the free reference
  (require 'use-package)
  (defvar flyspell-mode-map))

(defvar accepted-mode-list '(text-mode org-mode markdown-mode mu4e-compose-mode))

(defun trivialfis/check-buffer-on-save ()
  "Use language tool to check text mode buffer on save."
  (interactive)
  (use-package langtool
    :init
    (add-hook 'after-save-hook #'(lambda ()
  				   (when (memq major-mode accepted-mode-list)
  				     (unless langtool-buffer-process
  				       (langtool-check-buffer)))))
    :config
    (setq-local
     langtool-language-tool-jar "~/.emacs.d/LanguageTool-4.3/languagetool-commandline.jar"
     langtool-default-language "en-US"))
  :demand t)

(defun trivialfis/_text ()
  "Configuration for normal text."
  (flyspell-mode 1)
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-at-point)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ispell)
  (setq require-final-newline 'nil))

(defun trivialfis/cjk ()
  "Enable CJK input method."
  (interactive)
  (use-package pyim
    :commands pyim-basedict-enable)
  (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
  (setq default-input-method "pyim")
  (flyspell-mode 'f))

(defun trivialfis/text ()
  "Guard for trivialfis/_text."
  (interactive)
  (when (memq major-mode accepted-mode-list)
    (trivialfis/_text)))

(provide 'text-trivialfis)
;;; text-trivialfis.el ends here
