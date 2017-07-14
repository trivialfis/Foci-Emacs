;;; ansi-color-mode.el --- Summary
;;; Commentary:
;;
;; The `trivialfis/ansi-color-apply-on-window' is actually
;; a modified version of `ansi-color-apply-on-region'.
;;
;;; Code:
(require 'ansi-color)

(defvar last-modified-tick (buffer-chars-modified-tick)
  "The last time when modification to buffer happens.")

(defun trivialfis/ansi-color-apply-on-region (begin end)
  "Translates SGR control sequences into overlays or extents.
Delete all other control sequences without processing them.

SGR control sequences are applied by calling the function
specified by `ansi-color-apply-face-function'.  The default
function sets foreground and background colors to the text
between BEGIN and END, using overlays.  The colors used are given
in `ansi-color-faces-vector' and `ansi-color-names-vector'.  See
`ansi-color-apply-sequence' for details.

Every call to this function will set and use the buffer-local
variable `ansi-color-context-region' to save position and current
ansi codes.  This information will be used for the next call to
`ansi-color-apply-on-region'.  Specifically, it will override
BEGIN, the start of the region and set the face with which to
start.  Set `ansi-color-context-region' to nil if you don't want
this."
  (let* ((codes (car ansi-color-context-region))
	 (start-marker (or (cadr ansi-color-context-region)
			   (copy-marker begin)))
	 (end-marker (copy-marker end))
	 escape-sequence)
    ;; First, eliminate unrecognized ANSI control sequences.
    (save-excursion
      (goto-char start-marker)
      (while (re-search-forward ansi-color-drop-regexp end-marker t)
	;; (replace-match "")
	(put-text-property (match-beginning 0) (match-end 0) 'invisible t)))
    (save-excursion
      (goto-char start-marker)
      ;; Find the next SGR sequence.
      (while (re-search-forward ansi-color-regexp end-marker t)
	;; Colorize the old block from start to end using old face.
	(funcall ansi-color-apply-face-function
		 start-marker (match-beginning 0)
		 (ansi-color--find-face codes))
        ;; store escape sequence and new start position
        (setq escape-sequence (match-string 1)
	      start-marker (copy-marker (match-end 0)))
	;; hide the escape sequence
	(put-text-property (match-beginning 0) (match-end 0) 'invisible t)
	;; Update the list of ansi codes.
	(setq codes (ansi-color-apply-sequence escape-sequence codes)))
      ;; search for the possible start of a new escape sequence
      (if (re-search-forward "\033" end-marker t)
	  (progn
	    ;; if the rest of the region should have a face, put it there
	    (funcall ansi-color-apply-face-function
		     start-marker (point) (ansi-color--find-face codes))
	    ;; save codes and point
	    (setq ansi-color-context-region
		  (list codes (copy-marker (match-beginning 0)))))
	;; if the rest of the region should have a face, put it there
	(funcall ansi-color-apply-face-function
		 start-marker end-marker (ansi-color--find-face codes))
	(setq ansi-color-context-region (if codes (list codes)))))))

(defun ansi-color-apply-on-window (win start)
  "Apply ansi convert on WIN beginning at START."
  (let ((modified (buffer-modified-p))
	(end (window-end win)))
    (trivialfis/ansi-color-apply-on-region start end)
    (set-buffer-modified-p modified)))

(defun ansi-color-apply-on-line ()
  "Apply ansi convert on current line."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eq tick last-modified-tick)
      (trivialfis/ansi-color-apply-on-region (line-beginning-position)
					     (line-end-position))
      (setf last-modified-tick (buffer-chars-modified-tick)))))

(defun delete-skip-invisible-char (n &optional killflag)
  "Skip any invisible characters when trying to delete.
N and optional parameter KILLFLAG are same as `delete-char'"
  (while (and (null (eq 0 (1- (point))))
	      (get-char-property (1- (point)) 'invisible))
    (backward-char 1))
  (delete-char (- n) killflag))

(defun ansi-delete-backward-char (n &optional killflag)
  "Delete the previous N visible characters.
Optional KILLFLAG, if non-nil, kill instead of delete.
For detailed description, please refer to `delete-backward-char'"
  (declare (interactive-only delete-char))
  (interactive "p\nP")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (cond ((and (use-region-p)
	      delete-active-region
	      (= n 1))
	 ;; If a region is active, kill or delete it.
	 (if (eq delete-active-region 'kill)
	     (kill-region (region-beginning) (region-end) 'region)
           (funcall region-extract-function 'delete-only)))
	;; In Overwrite mode, maybe untabify while deleting
	((null (or (null overwrite-mode)
		   (<= n 0)
		   (memq (char-before) '(?\t ?\n))
		   (eobp)
		   (eq (char-after) ?\n)))
	 (let ((ocol (current-column)))
	   (delete-skip-invisible-char n killflag)
	   (save-excursion
	     (insert-char ?\s (- ocol (current-column)) nil))))
	;; Otherwise, do simple deletion.
	(t
	 (delete-skip-invisible-char n killflag))))

(define-minor-mode ansi-color-mode "Fundamental ANSI color mode"
  "Fundamental mode that use ANSI color code to set text face"
  :init-value nil
  :lighter "ANSI"
  :keymap '(([backspace] . ansi-delete-backward-char))
  (ansi-color-apply-on-window
   (selected-window)
   (window-start (selected-window)))
  (add-hook 'window-scroll-functions 'ansi-color-apply-on-window)
  (add-hook 'post-command-hook 'ansi-color-apply-on-line))

(provide 'ansi-color-mode)
;;; ansi-color-mode.el ends here
