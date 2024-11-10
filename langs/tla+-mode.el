;; tla+-mode --- Summary -*- lexical-binding: t -*-
;;;
;;; Copyright 2024 Jiamingy <jm.yuan@outlook.com>
;;;
;;; Commentary:
;;; Code:
(use-package comint
  :defer t
  :commands
  comint-check-proc
  make-comint-in-buffer)
(use-package tla-pcal-mode
  :defer t
  :commands
  tla-pcal-mode)

(use-package yasnippet
  :ensure t
  :defines yas-snippet-dir
  :hook ((snippet-mode) . yas-minor-mode-on)
  :init (setq yas-snippet-dir "~/.emacs.d/snippets"))

(defvar-local tla+/toolbox-jar "/home/jiamingy/workspace/pluscal/toolbox/tla2tools.jar")

(defvar-local tla+/arguments `("-cp" ,tla+/toolbox-jar "tlc2.REPL"))
(defvar-local tla+/comint-buffer "*Tla+*")

;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
(defun run-tla+-repl ()
  "Run the TLA+ REPL."
  (interactive)
  (let* ((tla+-program "java")
	 (buffer (get-buffer-create tla+/comint-buffer))
	 (proc-alive (comint-check-proc buffer)))
    ;; (process (get-buffer-process buffer))
    (unless proc-alive
      (with-current-buffer buffer
	(apply 'make-comint-in-buffer "Tla+" buffer
	       tla+-program nil tla+/arguments)))
    (when buffer
      (pop-to-buffer buffer))))

(defvar-local tla+/apalache-bin  "/home/jiamingy/workspace/pluscal/apalache/bin/apalache-mc")
(defvar-local tla+/apalache-jar  "/home/jiamingy/workspace/pluscal/apalache/lib/apalache.jar")

;; (use-package programming-trivialfis
;;   :commands flycheck-mode)
(require 'flycheck)

(flycheck-define-checker tla+/apalache
  "TLA+ checker using apalache."
  :command ("java" "-jar" "/home/jiamingy/workspace/pluscal/apalache/lib/apalache.jar" "parse" source)
  :error-patterns
  ;; Encountered "Init" at line 4, column 1 and token "Apalache"
  ((error line-start (message) "at line " line ", column " column (one-or-more not-newline) line-end))
  :modes (tla-mode)
  :predicate flycheck-buffer-nonempty-p)

(defun trivialfis/tla+ ()
  "Basic configuration for TLA+."
  (tla-pcal-mode)
  (setq-local flycheck-checker 'tla+/apalache)
  (flycheck-mode))

;;; tla+-mode.el ends here
