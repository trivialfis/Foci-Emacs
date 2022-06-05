;;; org-trivialfis --- Org configuration.
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

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))


(defun trivialfis/org-init()
  "Run before org mode initialization."
  (setf org-startup-truncated nil
	org-startup-folded nil)

  ;; fontify code in code blocks
  (setq org-src-fontify-natively t
	org-log-done 'time)

  ;; Set the header appearence
  (let ((level 1.3)
  	(org-level '(org-level-1
  		     org-level-2
  		     org-level-3
  		     )))
    (dolist (face org-level)
      (set-face-attribute face nil :weight 'semi-bold :height level)
      (setf level (- level 0.1))))
  (company-posframe-mode 1)

  (let* ((agenda-file "~/.emacs.d/misc/org-agenda.el")
	 (account-string (if (file-exists-p agenda-file)
			     (with-temp-buffer
			       (insert-file-contents agenda-file)
			       (buffer-string))
			   (prog2
			       (message "Failed to find the agenda file.")
			       nil)))
	 (evaluated (if account-string
			(eval (car (read-from-string account-string)))
		      nil)))
    (setq org-agenda-files evaluated))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo))

(defun trivialfis/org-post()
  "Run after org mode initialization."
  (setf org-bullets-bullet-list '("◉" "○" "■" "◆")
	org-image-actual-width nil)
  ;; (org-indent-mode 1)
  (org-bullets-mode 1)
  (toc-org-enable)
  (org-display-inline-images)
  (setq org-preview-latex-default-process 'dvisvgm)
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (trivialfis/local-set-keys
   '(
     ("C-c s"   .  trivialfis/org-insert-src)
     ("C-c e"   .  trivialfis/org-insert-example)
     ("C-c r s" .  trivialfis/org-region-src)
     ("C-c r e" .  trivialfis/org-region-example)
     )
   )
  (autoload 'fpg-mode "fpg-mode" "\
Minor mode for using org and mu4e to manage projects (Foci playground).

\(fn)" t nil))

(setq org-directory (concat (getenv "HOME") "/Workspace/roam/"))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

(provide 'org-trivialfis)
;;; org-trivialfis ends here
