;;; org-trivialfis --- Summary
;;; Commentary:
;;; Code:
;; (require 'outline)
(require 'org)
(require 'org-faces)
(require 'org-bullets)

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
  (org-indent-mode 1)
  (org-bullets-mode 1)
  (toc-org-enable)
  (org-display-inline-images))
(provide 'org-trivialfis)
;;; org-trivialfis ends here
