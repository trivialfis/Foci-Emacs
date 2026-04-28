;;; alloy-trivialfis.el --- Configuration for Alloy 6 -*- lexical-binding: t -*-
;;;
;;; Copyright 2026 Jiamingy <jm.yuan@outlook.com>
;;;
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package programming-trivialfis)

(use-package alloy-ts-mode
  :defer t
  :commands alloy-ts-mode)

(use-package lsp-trivialfis
  :autoload trivialfis/lsp)

(use-package lsp-mode
  :defer t
  :commands lsp lsp-deferred
  :config (trivialfis/lsp)
  :autoload lsp-find-references)

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references))

(use-package alloy-lsp
  :defer t
  :commands alloy-lsp-ensure
  :custom
  ;; Use the nix-installed `alloy6' stdio LSP rather than letting alloy-lsp.el
  ;; auto-download the upstream JAR into the package's source dir.
  (alloy-lsp-server-command '("alloy6" "lsp"))
  :config
  ;; alloy-lsp.el registers an LSP client but never publishes a languageId,
  ;; so lsp-mode warns "Unable to calculate the languageId" on every .als open.
  (add-to-list 'lsp-language-id-configuration '(alloy-ts-mode . "alloy"))
  (add-to-list 'lsp-language-id-configuration '(alloy-mode    . "alloy")))

(defun trivialfis/alloy ()
  "Configuration for Alloy 6 buffers."
  (alloy-ts-mode)
  (lsp-ui-mode)
  (alloy-lsp-ensure))

(provide 'alloy-trivialfis)
;;; alloy-trivialfis.el ends here
