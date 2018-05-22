;;; misc-trivialfis.el --- Misc functions.
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

(defun trivialfis/goto-pos ()
  "Go to position.
If you want to go to the middle, enter 50. 50 means 50% of the buffer."
  (interactive)
  (let ((inhibit-message t)
	(current (line-number-at-pos (point)))
	(pos (string-to-number (read-from-minibuffer "Pos: ")))
	(max-line (save-excursion
		    (progn
		      (goto-char (buffer-end 1))
		      (line-number-at-pos (point))))))
    (catch 'wrong-pos
      (if (or (> pos 100)
	      (< pos 0))
	  (throw 'wrong-pos "Wrong position")
	(forward-line (- (truncate (* (/ (float pos) 100)
				      max-line))
			 current))))))
(defun trivialfis/pop-frame ()
  "Pop up a new frame and close the current window."
  (interactive)
  (if (eq (selected-window) (next-window))
      (make-frame)
    (progn
      (make-frame)
      (delete-window))))
(defun trivialfis/close-frame ()
  "Close frame or kill Emacs."
  (interactive)
  (if (eq (next-frame) (selected-frame))
      (save-buffers-kill-terminal)
    (delete-frame)))

(defun trivialfis/local-set-keys (key-commands)
  "Set multiple local bindings with KEY-COMMANDS list."
  (let ((local-map (current-local-map)))
    (dolist (kc key-commands)
      (define-key local-map
	(kbd (car kc))
	(cdr kc)))))

(defun until-success (args)
  "Run until one of ARGS succeed.
ARGS: A quoted list containing all functions to be tried."
  (if (equal args 'nil)
      'nil
    (if (not (apply (list (car args))))
  	(until-success (cdr args))
      't)))

(provide 'misc-trivialfis)
;;; misc-trivialfis.el ends here
