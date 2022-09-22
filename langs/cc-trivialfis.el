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


(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package company-box
  :defer t
  :hook (company-mode . company-box-mode))

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

(use-package helm-gtags
  :commands (helm-gtags-dwim
	     helm-gtags-find-rtag)
  :config (message "helm-gtags loaded"))

(defvar-local cc-current-backend 'nil
  "Current backend for C++")

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
