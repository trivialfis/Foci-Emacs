;;; R-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'ess)
(require 'lsp-trivialfis)

(eval-when-compile
  (require 'use-package))

(use-package lsp)
(use-package ess-site)
(use-package f
  :autoload f-dirname)
(use-package condaenv
  :autoload trivialfis/find-activate-conda-env)

(defun trivialfis/ess ()
  "Configuration for ess."
  (setq-local ess-style 'RStudio
	      ess-indent-offset 2)
  (flymake-mode 0)
  (flycheck-mode 1))

(defun trivialfis/R ()
  "R configuration."
  (trivialfis/ess)
  ;; Fixme: R has its own venv package.
  (let ((condapy (trivialfis/find-activate-conda-env)))
    (if condapy
	(progn
	  ;; https://github.com/REditorSupport/languageserver
	  (trivialfis/lsp)
	  (lsp)))))

(provide 'R-trivialfis)
;;; R-trivialfis.el ends here
