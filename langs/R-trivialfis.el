;;; R-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'ess)
(require 'lsp-trivialfis)
(use-package lsp)
(use-package ess-site)

(defun trivialfis/ess ()
  "Configuration for ess."
  (setq ess-style 'RStudio)
  (flymake-mode 0)
  (flycheck-mode 1))

(defun trivialfis/R ()
  "R configuration."
  ;; https://github.com/REditorSupport/languageserver
  (trivialfis/lsp)
  (lsp))
(provide 'R-trivialfis)
;;; R-trivialfis.el ends here
