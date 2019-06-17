;;; julia-trivialfis.el --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'programming-trivialfis)
(require 'lsp-trivialfis)
(require 'lsp-julia)
(use-package lsp-trivialfis)

(defun trivialfis/julia ()
  "Julia configuration."
  (trivialfis/lsp)
  ;; As Jun-17 2019, all Julia dependencies are built from source to
  ;; get it work.
  (lsp)
  (lsp-ui-mode))

(provide 'julia-trivialfis)
;;; julia-trivialfis.el ends here
