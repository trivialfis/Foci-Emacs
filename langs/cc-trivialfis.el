;; cc-trivialfis --- Summary
;;;
;;; Copyright © 2016-2018 Fis Trivial <jm.yuan@outlook.com>
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

(require 'misc-trivialfis)
(require 'cc-mode)
(require 'google-c-style)

(use-package cmake-ide
  :defer t
  :commands (cide--locate-project-dir cmake-ide-setup))

(use-package window-purpose
  :defer t
  :commands purpose-mode
  :config (progn
	    (define-key purpose-mode-map (kbd "C-x b") nil)
	    (define-key purpose-mode-map (kbd "C-x C-f") nil)
	    (add-to-list 'purpose-user-regexp-purposes '("\\*Man.*" . Man-page))
	    (purpose-compile-user-configuration)
	    (message "Purpose loaded.")))

(use-package company-c-headers)

(use-package company-clang
  :defer t
  :commands company-clang
  :config (message "company-clang loaded"))

(use-package flycheck
  :defer t
  :commands flycheck-mode flycheck-select-checker)

(use-package lsp
  :defer t
  :commands lsp)

(defun trivialfis/company-clang ()
  "Company clang configuration."
  (setq company-backends (delete 'company-semantic company-backends))
  (if (equal major-mode 'c-mode)
      (setq company-clang-arguments '("-std=c++14"))
    (setq company-clang-arguments '("-std=gnu11")))

  (require 'company-c-headers)
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-backends 'company-clang))

(use-package rtags
  :commands rtags-start-process-unless-running
  :config (progn
	    (message "Rtags loaded")
	    (use-package company-rtags)))

(use-package helm-gtags
  :commands (helm-gtags-dwim
	     helm-gtags-find-rtag)
  :config (message "helm-gtags loaded"))

(defvar-local cc-current-backend 'nil
  "Current backend for C++")

(defun trivialfis/use-rtags ()
  "Rtags configuration.
Used only for nevigation."
  (rtags-start-process-unless-running)
  ;; (setq rtags-autostart-diagnostics t)
  ;; (rtags-diagnostics)
  ;; (add-to-list 'company-backends 'company-rtags)
  (setq rtags-completions-enabled 1)
  (setq rtags-display-result-backend 'helm)
  (trivialfis/local-set-keys
   '(
     ("M-."     . (lambda (arg) (interactive "P")
		    (until-success
		     '(rtags-find-symbol-at-point
		       rtags-find-symbol
		       helm-gtags-dwim))))
     ("M-?"     .  (lambda (arg) (interactive "P")
		     (until-success
		      '(rtags-find-references-at-point
			rtags-find-references
			helm-gtags-find-tag-from-here))))
     ("M-,"     .  rtags-location-stack-back)
     ("C-,"   .    rtags-location-stack-forward)
     ("C-c r r" .  rtags-rename-symbolrtags-next-match)
     )
   ))

(defun trivialfis/use-irony ()
  "Irony mode configuration."
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-irony-c-headers)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'flycheck-mode-hook #'(lambda ()
				    (flycheck-irony-setup)
				    (flycheck-select-checker 'irony)))
  (when (or (eq major-mode 'c-mode)	; Prevent from being loaded by c derived mode
  	    (eq major-mode 'c++-mode)
	    (eq major-mode 'cuda-mode))
    (ignore-errors
      (irony-mode 1)))
  (setq cc-current-backend 'irony)
  (message "Use irony mode as backend."))

(defun trivialfis/cc-base-srefactor ()
  "Configuration for refactor."
  (trivialfis/local-set-keys
   '(
     ("M-RET"   .  srefactor-refactor-at-point)
     ("C-c t"   .  senator-fold-tag-toggle)
     ("M-?"     .  semantic-symref)
     ("M-."     .  semantic-ia-fast-jump)
     ("C-,"     .  semantic-mrub-switch-tags)))
  (eval-after-load 'helm-trivialfis
    (local-set-key (kbd "C-h ,") 'helm-semantic-or-imenu)))

(defun trivialfis/cc-semantic ()
  "Use semantic mode as cc backend."
  (trivialfis/semantic 'c++-mode)
  (trivialfis/cc-base-srefactor)
  (setq cc-current-backend 'cc-semantic)
  (message "Use semantic mode as backend."))

(defun trivialfis/c-semantic ()
  "Use semantic mode as c backend."
  (trivialfis/semantic 'c-mode)
  (trivialfis/cc-base-srefactor)
  (setq cc-current-backend 'c-semantic)
  (message "Use semantic mode as backend."))

(defvar original-project nil
  "A global variable to keep the directory for the CMake project before file jump.")
(defvar-local current-project nil
  "A local variable to keep the directory for current CMake project.")

(defun trivialfis/use-cide ()
  "Set up rtags and CMake-ide if CMakeLists.txt is presented.
Otherwise do nothing.
When jumping around headers, keep the CMake project as the original one.
If the newly opened file belongs to a new project, then change the current
project to the new project."
  (interactive)
  (let ((project-dir (cide--locate-project-dir)))
    (if project-dir
	(setq current-project project-dir
	      original-project project-dir)
      (setq current-project original-project))
    (when current-project
      (setq cmake-ide-build-dir (concat current-project "build")))
    (trivialfis/use-rtags)
    (trivialfis/local-set-keys
     '(("C-c C-a" .  cmake-ide-compile)))
    (cmake-ide-setup)))


(defun trivialfis/cquery ()
  "Cquery configuration."
  (require 'cquery)
  (require 'company-lsp)
  (eval-when-compile
    (require 'cquery)
    (require 'company-lsp))
  (setq
   cquery-executable (expand-file-name "~/.guix-profile/bin/cquery")
   company-transformers nil
   company-lsp-async t
   company-lsp-cache-candidates nil
   cquery-extra-init-params '(:completion (:detailedLabel t))
   cquery-sem-highlight-method 'font-lock
   ;; cquery-sem-highlight-method 'overlay
   )
  ;; Hack around to avoid storing indexes in sub-directories
  (setf cquery-cache-dir-function
	#'(lambda (proj-dir)
	    (expand-file-name (cquery--get-root))))
  (set-buffer-multibyte nil)
  (add-to-list 'company-backends 'company-lsp)

  (setq cc-current-backend 'cquery)
  ;; (cquery-use-default-rainbow-sem-highlight)
  (lsp)
  (lsp-ui-mode)
  (flycheck-mode 1)
  (flymake-mode 0))

(defun trivialfis/ccls ()
  "Ccls configuration."
  (require 'ccls)
  (eval-when-compile
    (require 'ccls))
  (setq
   company-transformers nil
   company-lsp-async t
   company-lsp-cache-candidates nil
   ccls-executable "~/.guix-profile/bin/ccls")
  (defvar ccls-project-root-matchers '("compile_commands.json"))
  (setq cc-current-backend 'ccl)
  (lsp)
  (lsp-ui-mode)
  (flymake-mode 0)
  (flycheck-mode 1))

(defun trivialfis/cc-base ()
  "Common configuration for c and c++ mode."
  ;; Company mode
  (setf company-backends '())
  (setq-default indent-tabs-mode 'nil)
  (add-to-list 'company-backends 'company-keywords)

  (let ((cdb-file (locate-dominating-file "." "compile_commands.json")))
    (if (or cdb-file buffer-read-only (equal cc-current-backend 'ccls))
	;; (trivialfis/cquery)
	(trivialfis/ccls)
      (progn
	;; (trivialfis/use-irony)
	(trivialfis/company-clang))))

  (defconst trivialfis/cc-style
    '("gnu"
      (c-offsets-alist . ((innamespace . [0])))))
  (setq c-auto-newline nil)

  (c-add-style "trivialfis/cc-style" trivialfis/cc-style)
  (c-add-style "google-c-style" google-c-style)

  (c-set-style "google-c-style")

  (trivialfis/local-set-keys
   '(
     ;; Disaster
     ("C-c d a" . disaster)
     ;; Clang formating
     ("C-c f b" . clang-format-buffer)
     ("C-c f r" . clang-format-region)
     ("C-c C-f" . find-file-in-project)
     )
   ))

(defun trivialfis/c++ ()
  "Custom C++ mode."
  (trivialfis/cc-base)
  (if (equal cc-current-backend 'irony)
      (progn
	(setq flycheck-clang-language-standard "gnu++14"
  	      flycheck-gcc-language-standard "gnu++14"
  	      irony-additional-clang-options '("-std=c++14"))
	(flycheck-mode 1))))

(defun trivialfis/c ()
  "Custom c mode."
  (trivialfis/cc-base)
  (if (equal cc-current-backend 'irony)
      (progn
	(setq flycheck-clang-language-standard "-std=gnu11"
	      flycheck-gcc-language-standard "-std=gnu11")
	(flycheck-mode 1))))

(provide 'cc-trivialfis)
;;; cc-trivialfis.el ends here
