;;; python-trivialfis --- Python configuration -*- lexical-binding: t -*-
;;;
;;; Copyright © 2016-2024 Jiamingy <jm.yuan@outlook.com>
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

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package conda
  :custom
  (conda-anaconda-home  "~/.anaconda"))

(use-package lsp-trivialfis
  :defer t
  :autoload trivialfis/lsp)

(use-package lsp-mode
  :defer t
  :commands lsp
  :autoload
  lsp-register-client
  make-lsp-client
  lsp-find-references
  lsp-tramp-connection
  :config
  (setq-local lsp-client-packages '(lsp-pylsp))
  (trivialfis/lsp)
  (use-package lsp-pylsp))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references))


(use-package repl-trivialfis
  :commands (trivialfis/comint-send-input))

(use-package condaenv
  :commands
  trivialfis/python-from-which
  trivialfis/find-activate-conda-env)

(cl-defstruct
    (python-env
     (:constructor new-python--env))
  path
  from)

(defun new-python-env-rec (funcs)
  "Find python environment.  `FUNCS' is a list of functions to be called."
  (if funcs
      (let* ((pair (car funcs))
	     (func (car pair))
	     (sym (cdr pair))
	     (venv (funcall func)))
	(if venv
	    (cons venv sym)
	  (new-python-env-rec (cdr funcs))))
    (cons nil nil)))

(defun trivialfis/find-activate-virtualenv ()
  "Find and activate virtualenv."
  (let ((env-path (f-traverse-upwards
		   (lambda (path)
		     (or (equal path (f-expand "~"))
			 (f-exists? (f-join path "bin/activate")))))))
    (if (and env-path
	     (not (equal env-path (f-expand "~"))))
	(progn
	  (pyvenv-activate env-path)
	  (f-join env-path "bin" "python"))
      'nil)))

(cl-defun new-python-env ()
  "Create a new Python env struct."
  (let* ((options '((trivialfis/find-activate-virtualenv . virtual-env)
		    (trivialfis/find-activate-conda-env . conda-env)
		    (trivialfis/filename-python-p . file-name)
		    (trivialfis/python-from-shebang . shebang)))
	 (env (new-python-env-rec options)))
    (if (car env)
	(progn
	  (new-python--env :path (car env) :from (cdr env)))
      (progn
	(let ((from-which (trivialfis/python-from-which)))
	  (new-python--env :path from-which :from 'which))))))


(defun trivialfis/python-from-shebang ()
  "Get python command."
  (save-window-excursion
    (goto-char 0)
    (let* ((has-command-p (search-forward-regexp "python[2|3]*" 256 't))
	   (start (if has-command-p
		      (match-beginning 0)
		    nil))
	   (end (if has-command-p
		    (match-end 0)
		  nil)))
      (if has-command-p
	  (buffer-substring start end)
	nil))))

(defun trivialfis/filename-python-p ()
  "Whether filename contain python version."
  (save-match-data
    (let ((filename (buffer-file-name)))
      (if filename
	  (string-match "python[2|3]" filename)
	nil))))

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

(defun trivialfis/get-command-from-shell ()
  "Used after activating virtualenv."
  (message "Python from shell(virtual envs).")
  (if (file-remote-p default-directory)
      nil
    (let* ((raw-version (shell-command-to-string "python --version"))
	   (version-index (string-match "[2|3]" raw-version))
	   (version (substring-no-properties
		     raw-version version-index (+ 1 version-index))))
      (concat "python" version))))


(defvar current-env 'nil)

(defun trivialfis/elpy-setup(venv)
  "Elpy configuration.  VENV is the virtual env to be activated."
  (define-key elpy-mode-map (kbd "<C-return>") 'nil)
  (setq-local
   flycheck-flake8-maximum-line-length 88 ; black
   fill-column 88
   ;; way too slow
   flycheck-disabled-checkers '(python-pylint))

  (unless current-env
    (setq current-env venv)
    ;; Set the mypy cache for flycheck to a global directory to avoid polluting the source
    ;; dir.
    (let* ((command (python-env-path current-env))
	   (remove-/ (substring command 1)) ; remove the root / to make f-join happy
	   (path (f-join (f-expand "~/.cache/mypy/") remove-/)))
      ;; The final path is ~/.cache/mypy/<Python command path>
      (if (and (or (not (f-exists? path)) (f-dir? path))
	       ;; I haven't tested on Windows.
	       (not (string= system-type "windows-nt")))
	  (setq flycheck-python-mypy-cache-dir path))))

  (add-hook 'xref-after-jump-hook
	    #'(lambda ()
		;; mypy is not happy about top level modules.
		(let ((foundpath (f-traverse-upwards
				  (lambda (path)
				    (or (equal path (f-expand "~"))
					(f-exists? (f-join path "site-packages")))))))
		  (if (f-exists? (f-join foundpath "site-packages"))
		      (setq-local flycheck-disabled-checkers '(python-mypy python-pylint))))))

  ;; Replace flymake with flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (with-eval-after-load 'elpy
    (let ((command (python-env-path current-env)))
      (message (format "Python command: %s" command))
      (setq
       elpy-rpc-virtualenv-path 'current
       elpy-rpc-python-command command
       python-shell-interpreter command
       elpy-formatter 'black))
    ;; ipython makes use of xterm ansi code.
    ;; (elpy-use-ipython)
    (setq elpy-rpc-timeout 3
	  elpy-rpc-ignored-buffer-size (* 102400 10))
    (add-to-list 'company-backends 'elpy-company-backend)
    (elpy-enable)
    (elpy-mode 1))
  (flycheck-mode 1))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                  :major-modes '(python-mode)
                  :remote? t
                  :server-id 'pylsp-remote))


(defun trivialfis/python-lsp-setup(venv)
  "Setup for Python lsp mode.  `VENV' is the virtual environment."
  (if (not current-env)
      (setq current-env venv))

  (let ((max-line-length 88)
	(pydoc-ignore ["D205" "D400"]))
    (if (file-remote-p default-directory)
	;; Tramp, find virtualenv or conda env
	(progn
	  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
	  (let* ((venv-path (python-env-path current-env))
		 (conda-path (python-env-path current-env)))
	    (cond
	     ((eq (python-env-from current-env) 'virtual-env)
	      (progn
		(message "Remote virtualenv path: %s, %s" current-env (buffer-file-name))
		(add-to-list 'tramp-remote-path (f-join (f-dirname venv-path)))))
	     ((eq (python-env-from current-env) 'conda-env)
	      (progn
		(message "Remote conda path: %s, filename: %s" conda-path (buffer-file-name))
		(add-to-list 'tramp-remote-path (f-join (f-dirname venv-path)))))))
	  (setq-local lsp-pylsp-plugins-flake8-max-line-length max-line-length
		      lsp-pylsp-plugins-pydocstyle-ignore pydoc-ignore))
      ;; local file
      (let* ((command (python-env-path current-env))
	     (dir (if command (f-dirname command) 'nil)))
	(if dir
	    (setq-local lsp-pylsp-server-command (f-join dir "pylsp")
			lsp-pylsp-plugins-flake8-max-line-length max-line-length
			lsp-pylsp-plugins-pydocstyle-ignore pydoc-ignore
			lsp-pylsp-plugins-isort-enabled t
			lsp-pylsp-plugins-black-enabled t)))))


  (lsp)
  (lsp-ui-mode)
  (flycheck-mode 1)
  (setq-local forward-sexp-function 'nil
	      fill-column 88))

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

(defun trivialfis/isort()
  "Run Python isort."
  (interactive)
  (unless (equal major-mode 'python-mode)
    (error "Not in python-mode"))
  (let ((isort "isort"))
    (unless (executable-find isort)
      (error "Executable isort is not found"))
    (let ((*isort* (get-buffer-create (concat "*" isort "*"))))
      (call-process isort nil *isort* nil "--profile=black" (buffer-file-name))
      (revert-buffer-quick))))

(defun trivialfis/elpy-format-buffer()
  "Format buffer."
  (interactive)
  (elpy-format-code)
  (trivialfis/isort))

(defun trivialfis/python()
  "Python configuration."
  (local-set-key (kbd "C-c C-a") 'trivialfis/eval-file)
  (setq python-shell-completion-native-disabled-interpreters
	(cons "python3" python-shell-completion-native-disabled-interpreters))
  ;; Use elpy on local but lsp on remote.
  ;; fixme: this limits emacs to a single project.
  (let ((venv (if current-env current-env (new-python-env))))
    (if (or
	 (eq (python-env-from venv) 'conda-env)
	 (eq (python-env-from venv) 'virtual-env))
	(progn
	  (message "Found a virtual environment: venv: %s, file: %s" venv (buffer-file-name))
	  ;; Stick to elpy for now
	  (if (file-remote-p default-directory)
	      (trivialfis/python-lsp-setup venv)
	    (trivialfis/elpy-setup venv)))))

  (setq python-indent-def-block-scale 1)
  (add-hook 'inferior-python-mode-hook
	    #'(lambda ()
		(local-set-key (kbd "C-c k") 'trivialfis/clear-python)
		(fset 'comint-send-input 'trivialfis/comint-send-input)
		)))

(provide 'python-trivialfis)
;;; python-trivialfis.el ends here
