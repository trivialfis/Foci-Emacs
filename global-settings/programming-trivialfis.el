;;; programming-trivialfis.el --- Configurations hooked to prog-mode.
;;;
;;; Copyright 2016-2024 Jiamingy <ybbs.daans@hotmail.com>
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
;;
;;  Configuration for prog mode.
;;
;;; Code:

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package semantic/idle
  :commands (semantic-mode
	     semanticdb-enable-gnu-global-databases)
  :defer t
  :config (message "Semantic loaded."))

(use-package flycheck
  :init (declare-function flycheck-display-error-messages-unless-error-list
			  "Show messages of ERRORS unless the error list is visible.")
  :defer t
  :commands (flycheck-mode)
  :config
  (message "Flycheck loaded.")
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.23)))
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list)
  (if (and (memq 'idle-change flycheck-check-syntax-automatically)
	   (file-remote-p default-directory))
      (delete 'idle-change flycheck-check-syntax-automatically)))

(require 'helm-xref)

(use-package window-purpose
  :defer t
  :commands purpose-mode
  :autoload purpose-compile-user-configuration
  :config (progn
	    (define-key purpose-mode-map (kbd "C-x b") nil)
	    (define-key purpose-mode-map (kbd "C-x C-f") nil)
	    (add-to-list 'purpose-user-regexp-purposes '("\\*Man.*" . Man-page))
	    (purpose-compile-user-configuration)
	    (message "Purpose loaded.")))

(defun trivialfis/semantic (MODE)
  "Custom semantic mode.
MODE: the major programming mode"
  (let ((semantic-submodes '(global-semantic-decoration-mode
			     global-semantic-idle-local-symbol-highlight-mode
			     global-semantic-highlight-func-mode
			     global-semanticdb-minor-mode
			     global-semantic-mru-bookmark-mode
			     ;; global-semantic-idle-summary-mode
			     global-semantic-stickyfunc-mode
			     )))
    (setq semantic-default-submodes (append semantic-default-submodes semantic-submodes)
	  semantic-idle-scheduler-idle-time 1)
    (semanticdb-enable-gnu-global-databases 'MODE)
    (semantic-mode 1)))

(defun trivialfis/programming-init ()
  "Added before any programming mode configuration."
  (add-hook 'eldoc-mode-hook #'(lambda ()
				 (setf eldoc-idle-delay 0.5)))
  (hs-minor-mode 1)

  (local-set-key (kbd "TAB") 'company-indent-or-complete-common)
  (setq-local company-tooltip-align-annotations t)
  ;; Quickhelp
  (autoload 'trivialfis/company-quickhelp "quickhelp-trivialfis")
  (add-hook 'company-mode-hook 'trivialfis/company-quickhelp)
  (use-package hl-line
    :config
    (hl-line-mode 1)
    (set-face-foreground 'highlight nil)))

(autoload 'auto-update-file-header "header2")
(defun trivialfis/programming-post ()
  "Added after any programming mode configuration.
Enabled by specific programming language."
  (auto-update-file-header))


(use-package copilot
  :defer t
  :config
  (setq copilot--base-dir (expand-file-name "~/.emacs.d/copilot.el"))
  (company-box-mode)
  (define-key (current-global-map) (kbd "C-<tab>") 'copilot-accept-completion)
  :commands
  copilot-mode)

(use-package company-box
  :defer t
  :commands
  company-box-mode)

(provide 'programming-trivialfis)
;;; programming-trivialfis.el ends here
