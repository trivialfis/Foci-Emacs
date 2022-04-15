;; cc-trivialfis --- Summary -*- lexical-binding: t -*-
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
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

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

(use-package dap-mode
  :defer t
  :commands dap-mode
  :config (progn
	    (setq dap-auto-configure-features '(sessions locals controls tooltip))
	    (dap-ui-mode)
	    (use-package dap-lldb)))

(use-package company-c-headers)

(use-package company-clang
  :defer t
  :commands company-clang
  :config (message "company-clang loaded"))

(use-package flycheck
  :defer t
  :commands flycheck-mode flycheck-select-checker)

(use-package lsp-trivialfis)

(defun trivialfis/company-clang ()
  "Company clang configuration."
  (setq company-backends (delete 'company-semantic company-backends))
  (if (equal major-mode 'c++-mode)
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

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection 'lsp-clients--clangd-command)
                  :major-modes '(c++-mode)
                  :remote? t
                  :server-id 'clangd-remote))

(defun trivialfis/clangd ()
  "Clangd configuration."
  (trivialfis/lsp)
  (require 'lsp-ui)
  (setq-default lsp-clients-clangd-executable "clangd"
		lsp-clients-clangd-args '("--header-insertion=never")
		lsp-ui-peek-enable 't
		lsp-ui-doc-position 'top
		lsp-ui-doc-delay 1
		lsp-ui-doc-use-webkit t
		lsp-ui-doc-max-width 90)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references)
  (lsp)
  (lsp-ui-mode)
  (flycheck-mode 1))

(defun trivialfis/ccls ()
  "Configuration for ccls."
  (trivialfis/lsp)
  (lsp)
  (lsp-ui-mode)
  (flycheck-mode 1))

(defun trivialfis/cc-base ()
  "Common configuration for c and c++ mode."
  ;; Company mode
  (setq-local company-backends '())
  (setq-default indent-tabs-mode 'nil)
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-capf)

  (let ((cdb-file (locate-dominating-file "." "compile_commands.json")))
    (if (or cdb-file buffer-read-only)
	(trivialfis/clangd)
      ;; (trivialfis/ccls)
      (progn
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
  (trivialfis/cc-base))

(defun trivialfis/c ()
  "Custom c mode."
  (trivialfis/cc-base))

(provide 'cc-trivialfis)
;;; cc-trivialfis.el ends here
