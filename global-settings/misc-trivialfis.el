;;; misc-trivialfis.el --- Summary
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


(provide 'misc-trivialfis)
;;; misc-trivialfis.el ends here
