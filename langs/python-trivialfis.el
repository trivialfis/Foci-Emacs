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
(require 'python)
(require 'flycheck)

(use-package lsp-trivialfis)
(use-package lsp)
(use-package lsp-pyls)


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
     (has-python3-p (car (reverse (split-string which-python3))))
     (has-python-p (car (reverse (split-string which))))
     (has-python2-p (car (reverse (split-string which-python2))))
     (t "python"))))

(require 'conda)

(defvar conda-env-name 'nil)

(defun trivialfis/activate-conda-env ()
  "Activate conda env if there's any."
  (setq-default conda-env-home-directory "~/.anaconda/"
		conda-anaconda-home "~/.anaconda/")
  ;; FIXME: How to use local variables without being asked for safety for each opened
  ;; buffer?

  ;; (hack-dir-local-variables)
  ;; (print dir-local-variables-alist)
  (let ((env-path (f-traverse-upwards
		   (lambda (path)
		     (or (equal path (f-expand "~"))
			 (f-exists? (f-join path "environment.el")))))))
    (if (and env-path
	     (not (equal env-path (f-expand "~"))))
	(progn
	  (load-file (f-join env-path "environment.el"))
	  (conda-env-activate env-name)
	  't)
      'nil)))

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
  (cond
   ((trivialfis/activate-conda-env) (trivialfis/python-from-which))
   ((trivialfis/activate-virtualenv) (trivialfis/get-command-from-shell))
   ((trivialfis/shebang-p) (trivialfis/python-from-shebang))
   ((trivialfis/filename-python-p) (trivialfis/python-from-filename))
   (t (trivialfis/python-from-which))))

(defun trivialfis/elpy-setup()
  "Elpy configuration."
  (define-key elpy-mode-map (kbd "<C-return>") 'nil)
  (setq flycheck-disabled-checkers '(python-pylint))
  (flycheck-mode 1)
  ;; Replace flymake with flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (with-eval-after-load 'elpy
    (let ((command (trivialfis/determine-python)))
      (message (format "Python command: %s" command))
      (setq elpy-rpc-virtualenv-path 'current)
      (setq elpy-rpc-python-command command
	    python-shell-interpreter command))
    ;; ipython makes use of xterm ansi code.
    ;; (elpy-use-ipython)
    (setq elpy-rpc-timeout 3
	  elpy-rpc-ignored-buffer-size (* 102400 10))
    (add-to-list 'company-backends 'elpy-company-backend)
    (elpy-enable)
    (elpy-mode 1)))

(defun trivialfis/python-lsp-setup()
  "Setup for Python lsp mode."
  (trivialfis/lsp)
  (let* ((_ (trivialfis/determine-python)) ; activate env if presented
	 (python-which (shell-command-to-string "which python"))
	 (python-bin (f-dirname python-which)))
    (setq lsp-pyls-server-command (f-join python-bin "pyls")))

  (lsp)
  (lsp-ui-mode)
  (setq forward-sexp-function 'nil))

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
  (setq python-shell-completion-native-disabled-interpreters
	(cons "python3" python-shell-completion-native-disabled-interpreters))
  (trivialfis/elpy-setup)
  ;; (trivialfis/python-lsp-setup)
  (setq python-indent-def-block-scale 1)
  (add-hook 'inferior-python-mode-hook
	    #'(lambda ()
		(local-set-key (kbd "C-c k") 'trivialfis/clear-python)
		(fset 'comint-send-input 'trivialfis/comint-send-input)
		)))

(provide 'python-trivialfis)
;;; python-trivialfis.el ends here
