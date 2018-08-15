;;; python-trivialfis --- Python configuration -*- lexical-binding: t -*-
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

(require 'elpy)
(require 'company)
(require 'helm-xref)
(require 'f)

(use-package repl-trivialfis
  :commands (trivialfis/comint-send-input))

(defun trivialfis/shebang-p ()
  "Detect whether python command is declared in shebang."
  (save-window-excursion
    (goto-char (point-min))
    (save-match-data
      (if (search-forward "#!" (line-end-position) t 1)
	  (progn
	    (goto-char (point-min))
	    (search-forward-regexp "python[2|3]" (line-end-position) t 1))
	'nil))))

(defun trivialfis/python-from-shebang ()
  "Get python command."
  (message "Python from shebang.")
  (save-window-excursion
    (goto-char 0)
    (let* ((has-command-p (search-forward-regexp "python[2|3]"))
	   (start (if has-command-p
		      (match-beginning 0)
		    nil))
	   (end (if has-command-p
		    (match-end 0)
		  nil)))
      (buffer-substring start end))))

(defun trivialfis/filename-python-p ()
  "Whether filename contain python version."
  (save-match-data
    (let ((filename (buffer-file-name)))
      (string-match "python[2|3]" filename))))

(defun trivialfis/python-from-filename ()
  "Get python command from `w/buffer-file-name'."
  (message "Find from filename.")
  (save-match-data
    (let* ((file-name (buffer-file-name))
	   (start (string-match "python[2|3]" file-name))
	   (end (if start
		    (match-end 0)
		  nil)))
      (if end
	  (substring file-name start end)
	nil))))

(defun foundp (str)
  "Whether STR returned by `which' contain python."
  (not (equal (car (split-string str)) "which:")))

(defun trivialfis/python-from-which ()
  "Get python path by which shell command."
  (message "Python from `which'")
  (let* ((which (shell-command-to-string "which python"))
	 (which-python3 (shell-command-to-string "which python3"))
	 (which-python2 (shell-command-to-string "which python2"))
	 (has-python-p (foundp which))
	 (has-python3-p (foundp which-python3))
	 (has-python2-p (foundp which-python2)))
    (cond
     (has-python-p (car (reverse (split-string which))))
     (has-python3-p (car (reverse (split-string which-python3))))
     (has-python2-p (car (reverse (split-string which-python2))))
     (t "python"))))

(defun trivialfis/activate-virtualenv ()
  "Find and activate virtualenv."
  (let ((env-path (f-traverse-upwards
		   (lambda (path)
		     (or (equal path (f-expand "~"))
			 (f-exists? (f-join path "bin/activate")))))))
    (if (and env-path
	     (not (equal env-path (f-expand "~"))))
	(progn
	  (pyvenv-activate env-path)
	  't)
      'nil)))

(defun trivialfis/get-command-from-shell ()
  "Used after activating virtualenv."
  (message "Python from shell(virtual envs).")
  (let* ((raw-version (shell-command-to-string "python --version"))
	 (version-index (string-match "[2|3]" raw-version))
	 (version (substring-no-properties
		   raw-version version-index (+ 1 version-index))))
    (concat "python" version)))

(defun trivialfis/determine-python ()
  "Get python path."
  (cond ((trivialfis/activate-virtualenv) (trivialfis/get-command-from-shell))
	((trivialfis/shebang-p) (trivialfis/python-from-shebang))
	((trivialfis/filename-python-p) (trivialfis/python-from-filename))
	(t (trivialfis/python-from-which))))

(defun trivialfis/elpy-setup()
  "Elpy configuration."
  (setq ffip-prefer-ido-mode t)
  (flycheck-mode 1)
  ;; Replace flymake with flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (with-eval-after-load 'elpy
    (let ((command (trivialfis/determine-python)))
      (message (format "Python command: %s" command))
      (setq elpy-rpc-python-command command
	    python-shell-interpreter command))
    ;; ipython makes use of xterm ansi code.
    ;; (elpy-use-ipython)
    (setq elpy-rpc-timeout 3)
    (add-to-list 'company-backends 'elpy-company-backend)
    (elpy-mode 1)))

(defun trivialfis/clear-python ()
  "Clear the python environment."
  (interactive)
  (python-shell-send-string
   "
import sys
this = sys.modules[__name__]
for n in dir():
    if (n[0] != '_' and n[-1] != '_' and
     type(getattr(this, n)) is not type(this)):
        delattr(this, n)")
  (comint-clear-buffer))

(defun trivialfis/eval-file()
  "Eval the default buffer by sending file.
This can make use of __name__ == '__main__'."
  (interactive)
  (let ((path (buffer-file-name)))
    ;; (kill-process "Python")
    ;; (kill-buffer "*Python*")
    (run-python)
    (python-shell-send-file path)))

(defun trivialfis/python()
  "Python configuration."
  (local-set-key (kbd "C-c C-a") 'trivialfis/eval-file)
  (define-key elpy-mode-map (kbd "<C-return>") 'nil)
  ;; (local-set-key (kbd "C-c k") 'trivialfis/restart-python)
  (trivialfis/elpy-setup)
  (add-hook 'inferior-python-mode-hook
	    #'(lambda ()
		(local-set-key (kbd "C-c k") 'trivialfis/clear-python)
		(fset 'comint-send-input 'trivialfis/comint-send-input)
		))
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(provide 'python-trivialfis)
;;; python-trivialfis.el ends here
