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
  :defer
  :commands
  tla-pcal-mode)

(defvar-local tla+/toolbox-jar "/home/jiamingy/workspace/pluscal/toolbox/tla2tools.jar")

(defvar-local tla+/arguments `("-cp" ,tla+/toolbox-jar "tlc2.REPL"))
(defvar-local tla+/comint-buffer "*Tla+*")

;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
(defun run-tla+-repl ()
  "Run the TLA+ REPL."
  (interactive)
  (let* ((tla+-program "java")
	 (buffer (get-buffer-create tla+/comint-buffer))
	 (proc-alive (comint-check-proc buffer))
	 (process (get-buffer-process buffer)))
    (unless proc-alive
      (with-current-buffer buffer
	(apply 'make-comint-in-buffer "Tla+" buffer
	       tla+-program nil tla+/arguments)))
    (when buffer
      (pop-to-buffer buffer))))


(defun trivialfis/tla+ ()
  "Basic configuration for TLA+."
  (tla-pcal-mode))

;;; tla+-mode.el ends here
