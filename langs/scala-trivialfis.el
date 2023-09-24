;;; scala-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package programming-trivialfis)
;; Add metals backend for lsp-mode
(use-package scala-mode)
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-trivialfis
  :autoload trivialfis/lsp)
(use-package lsp-mode
  :defer t
  :commands lsp
  :config (trivialfis/lsp)
  :autoload lsp-find-references)
(use-package lsp-metals
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            "-J-Dmetals.icons=unicode"))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-metals-enable-semantic-highlighting t)
  (setq lsp-semantic-tokens-enable t
	lsp-semantic-tokens-apply-modifiers' nil))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references))

;; Super slow at initialization.
(defun trivialfis/scala ()
  "Configuration for scala."
  (trivialfis/lsp)
  (lsp)
  (lsp-ui-mode)
  ;; (flycheck-mode 1)
  ;; (yas-minor-mode)
  )

(provide 'scala-trivialfis)
;;; scala-trivialfis.el ends here
