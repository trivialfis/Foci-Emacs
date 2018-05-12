;;; scheme-trivialfis.el --- Summary
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

(require 'geiser)
(require 'geiser-mode)
(require 'geiser-repl)
(require 'geiser-guile)
(require 'geiser-compile)

(defun trivialfis/scheme ()
  "Run Geiser."
  ;; (setq geiser-active-implementations '(guile racket chicken))
  (setq geiser-active-implementations '(guile))
  (setq geiser-repl-query-on-kill-p nil)

  (define-key geiser-mode-map (kbd "C-c C-a") 'geiser-compile-current-buffer)
  (define-key geiser-mode-map (kbd "C-c C-k") 'geiser-mode-switch-to-repl-and-enter)

  (eval-after-load 'geiser
    (save-window-excursion
      (run-geiser 'guile)))

  (add-to-list 'geiser-guile-load-path "~/.config/guix/latest"))

(provide 'scheme-trivialfis)
;;; scheme-trivialfis.el ends here
