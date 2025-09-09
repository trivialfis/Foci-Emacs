;;; llm-trivialfis.el --- llm related functions.  -*- lexical-binding: t -*-
;;;
;;; Copyright © 2025 Jiamingy <jm.yuan@outlook.com>
;;;
;;; This file is part of Foci-Emacs.
;;;
;;; Foci-Emacs is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Foci-Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Foci-Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; Commentary:
;;; Code:

(use-package gptel
  :defer t
  :autoload gptel-make-openai
  :commands (gptel))
(use-package f
  :defer t
  :commands f-join)

(defun trivialfis/shell-to-string (command)
  "Execute COMMAND and return its output as string."
  (with-temp-buffer
    (let ((status (shell-command command (current-buffer))))
      (if (eq status 1)
	  (string-trim (buffer-string))
	nil))))

(defun trivialfis/get-key(name)
  "Get key with NAME."
  (let* ((keynum (trivialfis/shell-to-string (concat "keyctl request2 user " name " @u") ))
	 (key (if keynum
		  (trivialfis/shell-to-string (concat "keyctl print " keynum))
		nil)))
    (if (not key)
	(error "Failed to load the key")
      key)))

(defun trivialfis/get-dft-host()
  "Get the default host for LLM endpoint."
  (let ((host (getenv "DEFAULT_LLM_HOST")))
    (if host
	host
      (error "No available host"))))

(defconst DFT-ENDPOINT "/openai/deployments/claude-3-7-sonnet-20250219")

(defun trivialfis/make-gptel-claude ()
  "Make a gptel backend using claude."
  (interactive)
  (let ((key (trivialfis/get-key "perflab"))
	(host (trivialfis/get-dft-host)))
    (setq gptel-backend (gptel-make-openai "Claude"
			  :protocol "https"
			  :host host
			  :endpoint (concat DFT-ENDPOINT "/chat/completions")
			  :stream t
			  :key key
			  :models '(claude-3.7)))))

(defun trivialfis/aidermacs-minor-mode ()
  "Enable aidermacs."
  (interactive)
  (use-package aidermacs
    :defer t
    :bind (("C-c a" . aidermacs-transient-menu))
    :commands
    aidermacs-transient-menu
    :config
    (setq aidermacs-default-model "openai/claude-3-7"
	  aidermacs-architect-model "openai/claude-3-7"
	  ;; Editor model for code generation (initially defaults to
	  ;; aidermacs-default-model)
	  aidermacs-editor-model "openai/claude-3-7"
	  aidermacs-default-chat-mode 'architect
	  aidermacs-backend 'vterm
	  aidermacs-extra-args '("--analytics-disable" "--no-gitignore" "--cache-prompts" "--watch-files"))
    (let* ((host (concat "https://" (trivialfis/get-dft-host)))
	   (api-base (concat host DFT-ENDPOINT)))
      (setenv "AIDER_OPENAI_API_BASE" api-base))
    (setenv "AIDER_OPENAI_API_KEY" (trivialfis/get-key "perflab"))))

;; aider --analytics-disable --model=openai/claude-3-7 --env-file ./.env  ./CMakeLists.txt

(use-package condaenv
  :autoload trivialfis/find-activate-conda-env
  :defer t)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :commands
  claude-code-ide
  :config
  (message "enable claude code ide")
  (trivialfis/find-activate-conda-env)
  (claude-code-ide-emacs-tools-setup))

(provide 'llm-trivialfis)
;;; llm-trivialfis.el ends here
