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

(use-package lsp-mode
  :defer t
  :commands lsp)
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config '(trivialfis/global-set-keys
	    '(
	      ("M-n"          .           highlight-symbol-next)
	      ("M-p"          .           highlight-symbol-prev)
	      )))
(use-package company-lsp)
(use-package programming-trivialfis)

(defun trivialfis/lsp ()
  "Configure lsp-mode."
  (setq-default
   lsp-prefer-flymake 'nil
   company-transformers nil
   company-lsp-async t
   lsp-enable-on-type-formatting 'nil
   company-lsp-cache-candidates nil
   lsp-enable-snippet 'nil
   lsp-enable-semantic-highlighting t
   ;; lsp-semantic-highlighting :deferred
   ))

(provide 'lsp-trivialfis)
;;; lsp-trivialfis.el ends here
