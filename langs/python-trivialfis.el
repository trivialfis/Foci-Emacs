;;; python-trivialfis --- Python configuration
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
  (flycheck-mode 1)
  (elpy-enable)
  (elpy-mode 1)
  (with-eval-after-load 'elpy
    (setq elpy-rpc-python-command "python3"
  	  python-shell-interpreter "python3"
	  elpy-rpc-backend "jedi")
    ;; (elpy-use-ipython)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) ;; Replace flymake with flycheck
    (add-to-list 'company-backends 'elpy-company-backend))
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(defun trivialfis/python()
  "Python configuration."
  ;; lsp is not ready.
  ;; (lsp-mode)
  (trivialfis/elpy-setup)
  (trivialfis/activate-virtualenv)
  )

(provide 'python-trivialfis)
;;; python-trivialfis.el ends here
