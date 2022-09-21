;;; scala-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package programming-trivialfis)
(use-package lsp-trivialfis)
(use-package company-lsp)
;; Add metals backend for lsp-mode
(require 'scala-mode)
(use-package lsp-metals)
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
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;;       (setq gc-cons-threshold 100000000) ;; 100mb
  ;;       (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;;       (setq lsp-idle-delay 0.500)
  ;;       (setq lsp-log-io nil)
  ;;       (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil))

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
