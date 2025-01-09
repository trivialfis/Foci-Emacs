;;; text-trivialfis --- Configuration for lsp-mode -*- lexical-binding: t -*-
;;;
;;; Copyright © 2018 Fis Trivial <jm.yuan@outlook.com>
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

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package which-key
  :defer t
  :commands which-key-mode)
(use-package lsp-mode
  :defer t
  :commands lsp
  :autoload
  lsp-find-references
  lsp-package-path
  :config
  (message "use lsp-mode"))
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  '(trivialfis/global-set-keys
    '(
      ("M-n"          .           highlight-symbol-next)
      ("M-p"          .           highlight-symbol-prev)
      ))
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references))
(use-package programming-trivialfis)

(defun trivialfis/lsp ()
  "Configure lsp-mode."
  (setq-default
   lsp-prefer-flymake 'nil
   lsp-enable-on-type-formatting 'nil
   lsp-enable-snippet 'nil
   lsp-enable-semantic-highlighting t
   lsp-ui-peek-enable 't
   lsp-ui-doc-position 'top
   lsp-ui-doc-delay 1
   lsp-ui-doc-use-webkit t
   lsp-ui-doc-max-width 90
   lsp-inline-completion-enable 't
   lsp-keep-workspace-alive nil
   lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (which-key-mode))

(provide 'lsp-trivialfis)
;;; lsp-trivialfis.el ends here
