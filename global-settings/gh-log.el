;;; gh-log.el --- Utilities for handling GH logs.
;;;
;;; Copyright © 2025 Jiamingy <jm.yuan@outlook.com>
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

(defun trivialfis/keep-pr-num ()
  "Remove the log message but keep the PR number."
  (interactive)
  (move-beginning-of-line nil)
  (let ((ppos (search-forward "(")))
    (move-beginning-of-line nil)
    (let ((curp (point)))
      (kill-region curp ppos)))
  (move-end-of-line nil)
  (backward-delete-char 1)
  (insert ", ")
  (forward-line)
  (move-beginning-of-line nil))

(use-package move-text
  :defer t
  :commands
  (move-text-up move-text-down))

(defvar trivialfis/gh-log-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c p") 'move-text-up)
    (define-key map (kbd "C-c n") 'move-text-down)
    (define-key map (kbd "C-c r") 'trivialfis/keep-pr-num)
    map))

(define-minor-mode trivialfis/gh-log-mode "GitHub logs mode."
  :init-value nil
  :lighter "gh-log"
  :keymap trivialfis/gh-log-mode-map)

(provide 'gh-log)
;;; gh-log.el ends here
