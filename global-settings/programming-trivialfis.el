;;; programming-trivialfis.el --- Configurations hooked to prog-mode.
;;;
;;; Copyright © 2016-2018 Fis Trivial <ybbs.daans@hotmail.com>
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

(use-package ecb
  :init (setf ecb-fix-window-size 'width
	      ;; ecb-minor-mode-text "Ecb"
	      ecb-windows-width 0.18)
  :defer t
  :commands (ecb-activate)
  :config (message "ECB loaded."))

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
  :config (message "Flycheck loaded."))

(require 'helm-xref)

(defun trivialfis/flycheck ()
  "Configurate flycheck."
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.23)))
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list))

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
  (trivialfis/flycheck)
  (hs-minor-mode 1)

  (local-set-key (kbd "TAB") 'company-indent-or-complete-common)
  ;; Quickhelp
  (autoload 'trivialfis/company-quickhelp "quickhelp-trivialfis")
  (add-hook 'company-mode-hook 'trivialfis/company-quickhelp)
  (add-hook 'company-mode-hook #'(lambda ()
				   (require 'color)
				   (let ((bg (face-attribute 'default :background)))
				     (custom-set-faces
				      `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
				      `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
				      `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
				      `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
				      `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))))

(autoload 'auto-update-file-header "header2")
(defun trivialfis/programming-post ()
  "Added after any programming mode configuration.
Enabled by specific programming language."
  (auto-update-file-header))

(provide 'programming-trivialfis)
;;; programming-trivialfis.el ends here
