;;; java-trivialfis.el --- Summary
;;; Commentary:
;;; Code:
(require 'lsp-java)
(require 'lsp-mode)
;; (require 'company-lsp)
(require 'lsp-ui)
(require 'programming-trivialfis)

(defun trivialfis/java ()
  "Java configuration."
  ;; Don't know why prog mode isn't loaded.
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (c-set-style "gnu")
  (setq lsp-java--workspace-folders (list "/home/fis/Workspace/bitwar"))
  (trivialfis/programming-init)
  (flycheck-mode 1)
  ;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; (push 'company-lsp company-backends)
  (lsp-java-enable))

(provide 'java-trivialfis)
;;; java-trivialfis.el ends here
