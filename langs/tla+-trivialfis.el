;; tla+-mode --- Summary -*- lexical-binding: t -*-
;;;
;;; Copyright 2024 Jiamingy <jm.yuan@outlook.com>
;;;
;;; Commentary:
;;; Code:
(use-package yasnippet
  :ensure t
  :defines yas-snippet-dir
  :hook ((snippet-mode) . yas-minor-mode-on)
  :init (setq yas-snippet-dir "~/.emacs.d/snippets"))

(use-package tlaplus-ts-mode
  :defer t
  :custom
  (tla+/toolbox-jar "/home/jiamingy/workspace/pluscal/toolbox/tla2tools.jar")
  (tla+/apalache-jar "/home/jiamingy/workspace/pluscal/apalache/lib/apalache.jar")
  :commands
  tlaplus-ts-mode)

(defvar-local tla+/apalache-bin  "/home/jiamingy/workspace/pluscal/apalache/bin/apalache-mc")

(use-package flycheck
  :commands flycheck-mode)

(defun trivialfis/tla+ ()
  "Basic configuration for TLA+."
  (tlaplus-ts-mode)
  (setq-local flycheck-checker 'tla+/apalache)
  (flycheck-mode))

;;; tla+-mode.el ends here
