;;; package --- Summary
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

(use-package s
  :commands (s-chop-suffix))

(require 'window-purpose)
(require 'helm-xref)

;; (toggle-debug-on-error)
(defvar info-buffers '("*Flycheck errors*" "*Compile-Log*"))
(defun get-info-buffers ()
  "Get windows display info related buffers."
  (let ((buf-list (buffer-list))
	(win-list '()))
    (dolist (name info-buffers)
      (when (memq name buf-list)
	(let ((win (get-buffer-window name)))
	  (when win
	    (add-to-list win win-list)
	    ))))))

(defun helm-display-buffer-at-info (buffer)
  "Display helm buffer BUFFER."
  (let ((windows (get-info-buffers)))
    (if (listp windows)
	(progn
	  (message "a list of windows")
	  (select-window (car windows))
	  (switch-to-buffer buffer t t))
      (if windows
	  (progn
	    (message "single window")
	    (select-window windows)
	    (switch-to-buffer t t))
	(funcall #'helm-default-display-buffer buffer)))))

(defun my-helm-display-buffer-at-bottom (buffer)
  "BUFFER."
  (let ((window (or
		 (purpose-display-reuse-window-purpose buffer nil)
		 (purpose-display-reuse-window-buffer buffer nil)
		 (purpose-display-at-bottom buffer nil 0.25))))
    (message "My helm display at bottom.")
    (if window
        (progn
          (select-window window)
          ;; don't know why, but it doesn't work without `switch-to-buffer'
          (switch-to-buffer buffer t t))
      ;; in case the above methods weren't successful, fallback to default
      ;; helm display function
      (funcall #'helm-default-display-buffer buffer))))


(defun trivialfis/purpose ()
  "Common window-purpose configuration."
  (define-key purpose-mode-map (kbd "C-x b") nil)
  (define-key purpose-mode-map (kbd "C-x C-f") nil)
  (add-to-list 'purpose-user-name-purposes '("*Flycheck errors*" . general-info))
  (add-to-list 'purpose-user-name-purposes '("*Compile-Log*" . general-info))
  (purpose-compile-user-configuration)
  ;; (setq helm-display-function #'helm-display-buffer-at-info)
  (purpose-mode 1))

(defun trivialfis/elisp()
  "Emacs Lisp configuration."
  (flycheck-mode 1)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (when (eq (buffer-size) 0)
    (insert ";;; " (buffer-name) " --- Summary\n"
	    ";;; Commentary:\n"
	    ";;; Code:"
	    "\n\n\n"
	    "(provide '" (s-chop-suffix ".el" (buffer-name)) ")\n"
	    ";;; " (buffer-name) " ends here"))

  (local-set-key (kbd "C-c C-a") 'byte-compile-file)
  (local-set-key (kbd "C-c C-b") 'eval-buffer)
  (aggressive-indent-mode)
  (trivialfis/purpose))
;;; elisp-trivialfis.el ends here
