;; cc-trivialfis --- Summary -*- lexical-binding: t -*-
;;;
;;; Copyright 2016-2024 Jiamingy <jm.yuan@outlook.com>
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

(use-package dap-mode
  :defer t
  :commands dap-mode dap-ui-mode
  :config (progn
	    (setq dap-auto-configure-features '(sessions locals controls tooltip))
	    (dap-ui-mode)
	    (use-package dap-lldb)))

(use-package lsp-trivialfis
  :defer t
  :autoload trivialfis/lsp)

(use-package programming-trivialfis
  :defer t
  :autoload flycheck-mode)

;; This one is loaded, then the global lsp configuration. Multiple files can reuse the
;; same clangd session, regardless how a file is loaded.
(use-package lsp-mode
  :defer t
  :commands lsp
  :autoload
  lsp-find-references
  lsp-package-path
  lsp-clients-executable-find
  :config
  (message "Use lsp with clangd.")
  (trivialfis/lsp)
  (use-package lsp-clangd
    :autoload lsp-clients--clangd-command lsp-clangd-find-other-file))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references))

(use-package dash
  :autoload -first -map)

(defun lsp-clients-clangd-command (remote)
  "Find clangd executable.

REMOTE is for `executable-find'.

Modified from `lsp-clients--clangd-command'."
  (or (lsp-package-path 'clangd)
      (-first #'(lambda (command)
		  (executable-find command remote))
	      (-map (lambda (version)
		      (concat "clangd" version))
		    ;; Prefer `clangd` without a version number appended.
		    (cl-list* "" (-map
				  (lambda (vernum) (format "-%d" vernum))
				  (number-sequence 22 14 -1)))))
      (lsp-clients-executable-find "xcodebuild" "-find-executable" "clangd")
      (lsp-clients-executable-find "xcrun" "--find" "clangd")))

(defun trivialfis/clangd ()
  "Clangd configuration."
  (let ((clangd (lsp-clients-clangd-command (file-remote-p default-directory))))
    (message "found clangd: %s" clangd)
    (setq-default lsp-clients-clangd-executable clangd
		  lsp-clients-clangd-args '("--header-insertion=never")
		  lsp-json-use-lists t))
  (lsp)
  (lsp-ui-mode)
  (flycheck-mode 1))

(defun trivialfis/ff-find-other-file ()
  "Bundles find other file with xref and clangd."
  (interactive)
  (xref-push-marker-stack)
  (condition-case _
      (if lsp-mode
	  (lsp-clangd-find-other-file)
	(ff-find-other-file))
    (user-error
     (ff-find-other-file))))

(defun trivialfis/cc-base ()
  "Common configuration for c and c++ mode."
  ;; Company mode
  (setq-local company-backends '()
	      indent-tabs-mode 'nil)
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-capf)

  (let ((cdb-file (locate-dominating-file "." "compile_commands.json")))
    (if (or cdb-file buffer-read-only)
	(trivialfis/clangd)))

  (defconst trivialfis/cc-style
    '("gnu"
      (c-offsets-alist . ((innamespace . [0])))))
  (setq-local c-auto-newline nil)

  (c-add-style "trivialfis/cc-style" trivialfis/cc-style)
  (c-add-style "google-c-style" google-c-style)

  (c-set-style "google-c-style")
  ;; Handle CUDA header files
  (add-to-list 'cc-other-file-alist
	       '("\\.cu\\'"  (".cuh" ".h")))
  (add-to-list 'cc-other-file-alist
	       '("\\.cuh\\'" (".cu")))

  (trivialfis/local-set-keys
   '(
     ;; Disaster
     ("C-c d a" . disaster)
     ("C-c f f" . trivialfis/ff-find-other-file)
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
