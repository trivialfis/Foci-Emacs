;;; sml-mode.el --- Summary
;;; Commentary:
;;; Code:

(use-package sml-mode)
(use-package repl-trivialfis
  :commands (trivialfis/comint-send-input))

(defun trivialfis/inferior-ml ()
  "Inferior SML configuration."
  #'(lambda ()
      (fset 'comint-send-input 'trivialfis/comint-send-input)
      ))

(defun trivialfis/sml()
  "SML configuration."
  (add-hook 'inferior-sml-model 'trivialfis/inferior-ml))

(provide 'sml-trivialfis)
;;; sml-trivialfis.el ends here
