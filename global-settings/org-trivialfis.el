;;; org-trivialfis --- Summary
;;; Commentary:
;;; Code:
;; (require 'outline)
(require 'org)
(require 'org-faces)
(require 'org-bullets)
(require 'misc-trivialfis)
(defun trivialfis/org-insert-src ()
  "Insert src block at point."
  (interactive)
  (insert "#+BEGIN_SRC\n\n#+END_SRC")
  (forward-line -1))

(defun trivialfis/org-region-src (begin end)
  "Make region between BEGIN and END as src block."
  (interactive "r")
  (goto-char end)
  (if (eq (line-beginning-position)
	  end)
      (insert "#+END_SRC\n")
    (insert "\n#+END_SRC\n"))
  (goto-char begin)
  (if (eq (line-beginning-position)
	  begin)
      (insert "#+BEGIN_SRC \n")
    (insert "\n#+BEGIN_SRC \n"))
  (backward-char))

(defun trivialfis/org-insert-example ()
  "INsert example block at point."
  (interactive)
  (insert "#+BEGIN_EXAMPLE\n\n#+END_EXAMPLE")
  (forward-line -1))

(defun trivialfis/org-region-example (begin end)
  "Make region between BEGIN and END as src block."
  (interactive "r")
  (goto-char end)
  (if (eq (line-beginning-position)
	  end)
      (insert "#+END_EXAMPLE\n")
    (insert "\n#+END_EXAMPLE\n"))
  (goto-char begin)
  (if (eq (line-beginning-position)
	  begin)
      (insert "#+BEGIN_EXAMPLE \n")
    (insert "\n#+BEGIN_EXAMPLE \n"))
  (backward-char))

(defun trivialfis/org-init()
  "Run before org mode initialization"
  (setf org-startup-truncated nil
	org-startup-folded nil)
  (setf org-agenda-files '("~/Workspace/note/reading_list/reading-progress.org"))

  ;; fontify code in code blocks
  (setq org-src-fontify-natively t)

  ;; Set the header appearence
  (let ((level 1.3)
  	(org-level '(org-level-1
  		     org-level-2
  		     org-level-3
  		     )))
    (dolist (face org-level)
      (set-face-attribute face nil :weight 'semi-bold :height level)
      (setf level (- level 0.1)))))

(defun trivialfis/org-post()
  "Run after org mode initialization"
  (setf org-bullets-bullet-list '("◉" "○" "■" "◆")
	org-image-actual-width nil)
  ;; (org-indent-mode 1)
  (org-bullets-mode 1)
  (toc-org-enable)
  (org-display-inline-images)

  (trivialfis/local-set-keys
   '(
     ("C-c s"   .  trivialfis/org-insert-src)
     ("C-c e"   .  trivialfis/org-insert-example)
     ("C-c r s" .  trivialfis/org-region-src)
     ("C-c r e" .  trivialfis/org-region-example)
     )
   ))

(provide 'org-trivialfis)
;;; org-trivialfis ends here
