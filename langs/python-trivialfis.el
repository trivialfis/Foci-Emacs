;;; python-trivialfis --- Summary
;;; Commentary:
;;; Code:
;; (require 'elpy)
(require 'company)
(require 'programming-trivialfis)
(require 'elpy)
(require 'lsp-mode)
(require 'lsp-common)
;; (require 'python)

(defun lsp-client()
  "Not ready to use. Emacs hangs."
  (lsp-define-stdio-client 'python-mode "python" 'stdio
			   (lsp-make-traverser #'(lambda (dir)
						   (directory-files
						    dir
						    nil
						    "\\(__init__\\|setup\\)\\.py")))
			   "Python Language Server"
			   '("pyls"))
  (lsp-mode))

(defun elpy-setup()
  "Elpy configuration."
  (flycheck-mode 1)
  (elpy-mode)
  (with-eval-after-load 'elpy
    (setq elpy-rpc-python-command "python3"
  	  python-shell-interpreter "python3")
    ;; (elpy-use-ipython)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) ;; Replace flymake with flycheck
    ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
    (add-to-list 'company-backends 'elpy-company-backend)))

(defun trivialfis/python()
  (trivialfis/programming-init)
  ;; (lsp-client)
  (elpy-setup)
  )

(provide 'python-trivialfis)
;;; python-trivialfis.el ends here
