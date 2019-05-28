;;; julia-trivialfis.el --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'programming-trivialfis)
(require 'lsp-julia)
(use-package lsp-trivialfis)

(defun trivialfis/julia ()
  "Julia configuration."
  (trivialfis/lsp)
  ;; It doesn't work.
  ;; (lsp)
  ;; (lsp-ui-mode)
  )

(provide 'julia-trivialfis)
;;; julia-trivialfis.el ends here
