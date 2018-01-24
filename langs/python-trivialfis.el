;;; python-trivialfis --- Python configuration
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
;; (require 'lsp-mode)
;; (require 'lsp-python)

(defun trivialfis/python-from-shebang ()
  "Get python command.
Given COMMAND-START, the start position of python[2|3], return python[2|3]."
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

(defun trivialfis/shebang-p ()
  "Detect whether python command is declared in shebang."
  (save-window-excursion
    (goto-char (point-min))
    (save-match-data
      (search-forward "#!" (line-end-position) t 1))))

(defun trivialfis/activate-virtualenv ()
  "Find and activate virtualenv."
  (let ((env-path (f-traverse-upwards
		   (lambda (path)
		     (or (equal path (f-expand "~"))
			 (f-exists? (f-join path "bin/activate")))))))
    (when (and env-path
	       (not (equal env-path (f-expand "~"))))
      (pyvenv-activate env-path))))

(defun trivialfis/determine-python ()
  "Get python path."
  (trivialfis/activate-virtualenv)
  (cond (pyvenv-activate "python")
	((trivialfis/shebang-p) (trivialfis/python-from-shebang))
	(t "python")))

(defun trivialfis/elpy-setup()
  "Elpy configuration."
  (setq ffip-prefer-ido-mode t)
  (flycheck-mode 1)
  ;; Replace flymake with flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (with-eval-after-load 'elpy
    (let ((command (trivialfis/determine-python)))
      (setq elpy-rpc-python-command command
	    python-shell-interpreter command))
    ;; ipython makes use of xterm ansi code.
    ;; (elpy-use-ipython)
    (setq elpy-rpc-timeout 3)
    (add-to-list 'company-backends 'elpy-company-backend)
    (elpy-mode 1)))

(defun trivialfis/eval-file()
  "Eval the default buffer by sending file.
This can make use of __name__ == '__main__'."
  (interactive)
  (let ((path (buffer-file-name)))
    (run-python)
    (python-shell-send-file path)))

(defun trivialfis/python()
  "Python configuration."
  ;; lsp is not ready.
  ;; (lsp-python-enable)
  (local-set-key (kbd "C-c C-a") 'trivialfis/eval-file)
  (trivialfis/elpy-setup)
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(provide 'python-trivialfis)
;;; python-trivialfis.el ends here
