;;; java-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'programming-trivialfis)
(require 'cc-mode)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package lsp-trivialfis
  :defer t
  :autoload trivialfis/lsp)
(use-package lsp-mode
  :defer t
  :commands lsp
  :config
  (trivialfis/lsp))
(use-package lsp-java
  :config
  (message "load lsp java")
  (setq lsp-java-autobuild-enabled nil))
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)

(defun trivialfis/java ()
  "Java configuration."
  ;; Don't know why prog mode isn't loaded.
  (trivialfis/programming-init)
  ;; (c-set-style "gnu")
  (setq lsp-before-save-edits nil
	c-basic-offset 2
	indent-tabs-mode nil)
  (flycheck-mode 1)
  (lsp)
  (lsp-ui-mode))

(provide 'java-trivialfis)
;;; java-trivialfis.el ends here
