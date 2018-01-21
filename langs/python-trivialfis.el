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

(defun trivialfis/activate-virtualenv ()
  "Find and activate virtualenv."
  (let ((env-path (f-traverse-upwards
		   (lambda (path)
		     (or (equal path (f-expand "~"))
			 (f-exists? (f-join path "bin/activate")))))))
    (when (and env-path
	       (not (equal env-path (f-expand "~"))))
      (pyvenv-activate env-path))))

(defun trivialfis/elpy-setup()
  "Elpy configuration."
  (setq ffip-prefer-ido-mode t)
  (flycheck-mode 1)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) ;; Replace flymake with flycheck
  (elpy-mode 1)
  (with-eval-after-load 'elpy
    ;; FIXME: Find virtualenv then decide python version.
    (setq elpy-rpc-python-command "python3"
  	  python-shell-interpreter "python3"
	  elpy-rpc-backend "jedi")
    ;; ipython makes use of xterm ansi code.
    ;; (elpy-use-ipython)
    (add-to-list 'company-backends 'elpy-company-backend))
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

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
  (trivialfis/activate-virtualenv))

(provide 'python-trivialfis)
;;; python-trivialfis.el ends here
