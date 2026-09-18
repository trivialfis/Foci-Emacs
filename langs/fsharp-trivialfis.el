;;; fsharp-trivialfis.el --- Configuration for F# -*- lexical-binding: t -*-
;;;
;;; Copyright 2026 Jiamingy <jm.yuan@outlook.com>
;;;
;;; Commentary:
;; Use FsAutoComplete installed with dotnet for F# sources and signatures.
;;; Code:

(eval-when-compile
  (require 'use-package)
  ;; Make the LSP client struct accessors available to the byte compiler.
  (require 'lsp-mode))

(use-package programming-trivialfis)

(use-package lsp-trivialfis
  :autoload trivialfis/lsp)

(use-package lsp-mode
  :defer t
  :commands lsp-deferred
  :config (trivialfis/lsp)
  :autoload lsp-find-references)

(use-package lsp-fsharp
  :demand t
  :custom
  ;; Launch the globally installed dotnet tool by its full path.
  (lsp-fsharp-use-dotnet-tool-for-fsac nil)
  (lsp-fsharp-server-install-dir (expand-file-name "~/.dotnet/tools/"))
  :config
  ;; lsp-fsharp already supports fsharp-ts-mode, but not its sibling for .fsi.
  (add-to-list 'lsp-language-id-configuration '(fsharp-ts-signature-mode . "fsharp"))
  (cl-pushnew 'fsharp-ts-signature-mode
              (lsp--client-major-modes (gethash 'fsac lsp-clients))))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references))

(use-package yasnippet
  :commands yas-minor-mode)

(defun trivialfis/fsharp ()
  "Configure F# buffers with FsAutoComplete and the shared LSP UI."
  (setq-local indent-tabs-mode nil
              lsp-enable-dap-auto-configure nil)
  (lsp-deferred)
  (lsp-ui-mode 1)
  (yas-minor-mode 1))

(provide 'fsharp-trivialfis)
;;; fsharp-trivialfis.el ends here
