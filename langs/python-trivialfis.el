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

(use-package lsp-trivialfis
  :defer t
  :autoload trivialfis/lsp)

(use-package tramp
  :defer t
  :autoload tramp-dissect-file-name tramp-file-name-localname)

(use-package lsp-mode
  :defer t
  :commands lsp
  :autoload
  lsp-find-references
  :config
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
  (let* ((options '((trivialfis/find-activate-conda-env . conda-env)
		    (trivialfis/find-activate-virtualenv . virtual-env)
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
  (local-set-key (kbd "C-c l = =") #'trivialfis/ruff-format-buffer)
  (setq-local
   flycheck-flake8-maximum-line-length 88 ; black
   fill-column 88)
  ;; pylint is way too slow, pycompile couldn't find path on Windows
  (if (string= system-type "windows-nt")
      (setq-local flycheck-disabled-checkers '(python-pylint python-pycompile))
    (setq-local flycheck-disabled-checkers '(python-pylint)))

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
		(let ((major-mode (with-current-buffer (current-buffer)
				    major-mode)))
		  (if (eq major-mode 'python-mode)
		      ;; mypy is not happy about top level modules.
		      (let ((foundpath (f-traverse-upwards
					(lambda (path)
					  (or (equal path (f-expand "~"))
					      (f-exists? (f-join path "site-packages")))))))
			(if (f-exists? (f-join foundpath "site-packages"))
			    (setq-local flycheck-disabled-checkers '(python-mypy python-pylint))))))))

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

(defun trivialfis/get--local-path (path)
  "Get local path from a tramp PATH."
  (tramp-file-name-localname
   (tramp-dissect-file-name path)))

(defun trivialfis/python-lsp-setup(venv)
  "Setup for Python lsp mode.  `VENV' is the virtual environment."
  (if (not current-env)
      (setq current-env venv))

  (if (file-remote-p default-directory)
      ;; Tramp, find virtualenv or conda env
      (progn
	;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
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
	      (add-to-list 'tramp-remote-path
			   (trivialfis/get--local-path (f-join (f-dirname venv-path)))))))
	  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))
    ;; local file
    (let* ((command (python-env-path current-env))
	   (dir (if command (f-dirname command) 'nil)))
      (if dir
	  (setq-local lsp-pylsp-server-command (f-join dir "pylsp")))))

  (let ((max-line-length 88)
	(pydoc-ignore ["D205" "D213" "D400" "D401" "D415"]))
    (setq-default
     lsp-pylsp-plugins-ruff-line-length max-line-length
     lsp-pylsp-plugins-flake8-max-line-length max-line-length
     lsp-pylsp-plugins-pycodestyle-max-line-length max-line-length
     lsp-pylsp-plugins-pydocstyle-ignore pydoc-ignore
     lsp-pylsp-plugins-isort-enabled t
     lsp-pylsp-plugins-black-enabled t))

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
  "Run Python isort.

Find pyproject.toml if available, call isort and replace the
buffer content with formatted result."
  (interactive)
  (unless (equal major-mode 'python-mode)
    (error "Not in python-mode"))
  (let* ((isort "isort")
	 (pyproject-rel (locate-dominating-file default-directory "pyproject.toml"))
	 (name (f-filename (buffer-file-name)))
	 (pyproject (if pyproject-rel (f-expand pyproject-rel) nil)))
    (unless (executable-find isort)
      (error "Executable isort is not found"))

    (let ((*isort* (get-buffer-create (concat "*" isort "*")))
	  (src (if pyproject (format "--src=%s" pyproject) ""))
	  (config (if pyproject (format "--settings-path=%s" pyproject) "--profile=black"))
	  (temp-file (make-temp-file
		      "isort-emacs"	; PREFIX
		      nil		; DIR-FLAG
		      name
		      (buffer-substring-no-properties (point-min) (point-max)))))
      (call-process
       isort				; PROGRAM
       nil				; INFILE
       *isort*				; DESTINATION
       nil				; DISPLAY
       ;; args
       temp-file
       src
       config)

      (let ((current-point (point)))
	(goto-char (point-max))
	(let ((pmax (point)))
	  (insert-file-contents temp-file)  ; append to the end of the buffer
	  (delete-region (point-min) pmax)) ; remove the old content
	(goto-char current-point))	    ; go back to the previous point
      (delete-file temp-file))))

(defun trivialfis/elpy-format-buffer()
  "Format buffer."
  (interactive)
  (elpy-format-code)
  (trivialfis/isort))

(defvar-local trivialfis/--ruff-pyproject-path nil
  "Cached path to pyproject.toml for the current buffer.")

(defun trivialfis/--ruff-run (args label)
  "Pipe the current buffer through ruff with ARGS and replace contents.

LABEL is used in error messages to identify the ruff sub-command.
The entire buffer is always sent via stdin so that ruff has full
context.  On success the buffer is updated with
`replace-buffer-contents' to preserve markers and point."
  (let ((output-buf (generate-new-buffer " *ruff-format*"))
        (stderr-file (make-temp-file "ruff-stderr")))
    (unwind-protect
        (let ((exit-code (apply #'call-process-region
                                (point-min) (point-max)
                                "ruff"
                                nil
                                (list output-buf stderr-file)
                                nil
                                args)))
          (if (zerop exit-code)
              (replace-buffer-contents output-buf)
            (user-error "%s failed (exit %d): %s"
                        label exit-code
                        (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (string-trim (buffer-string))))))
      (kill-buffer output-buf)
      (delete-file stderr-file))))

(defun trivialfis/--find-pyproject-dir ()
  "Find the directory containing pyproject.toml.

Return the cached path if available.  Otherwise search upward from
the project root (via `project-current') or `default-directory'.
The result is cached in the buffer-local variable
`trivialfis/--ruff-pyproject-path'."
  (or trivialfis/--ruff-pyproject-path
      (let ((search-dir (if-let ((proj (project-current)))
                            (project-root proj)
                          default-directory)))
        (when-let ((dir (locate-dominating-file
                         search-dir "pyproject.toml")))
          (setq trivialfis/--ruff-pyproject-path (expand-file-name dir))
          trivialfis/--ruff-pyproject-path))))

(defun trivialfis/ruff-format-buffer ()
  "Format the current buffer or the active region using ruff.

Sort imports with `ruff check --select I --fix', then format code
with `ruff format'.  When a region is active, only format that
region via --range (import sorting still applies to the whole
buffer).  Buffer contents are piped through stdin/stdout and
replaced using `replace-buffer-contents' to preserve markers and
point."
  (interactive)
  (unless (executable-find "ruff")
    (user-error "The `ruff' formatter is not accessible"))
  (let* ((pyproject-dir (trivialfis/--find-pyproject-dir))
         (config-args
          (when pyproject-dir
            (list "--config"
                  (expand-file-name "pyproject.toml" pyproject-dir))))
         (range-args
          (when (use-region-p)
            (let ((start-line (line-number-at-pos (region-beginning)))
                  (end-line (line-number-at-pos (region-end))))
              (list "--range" (format "%d-%d" start-line end-line)))))
         (filename (or (buffer-file-name) "stdin")))
    ;; 1. Sort imports (whole buffer)
    (trivialfis/--ruff-run
     (append (list "check" "--select" "I" "--fix"
                   "--stdin-filename" filename)
             config-args
             (list "-"))
     "ruff isort")
    ;; 2. Format code (respects --range when region is active)
    (trivialfis/--ruff-run
     (append (list "format" "--stdin-filename" filename)
             config-args
             range-args
             (list "-"))
     "ruff format")))

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
