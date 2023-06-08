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

(use-package dap-mode
  :defer t
  :commands dap-mode dap-ui-mode
  :config (progn
	    (setq dap-auto-configure-features '(sessions locals controls tooltip))
	    (dap-ui-mode)
	    (use-package dap-lldb)))

(use-package flycheck
  :defer t
  :commands flycheck-mode)

(use-package lsp-trivialfis
  :defer t
  :autoload trivialfis/lsp)

(use-package lsp-mode
  :defer t
  :commands lsp
  :autoload
  lsp-find-references
  lsp-tramp-connection
  lsp-register-client
  make-lsp-client
  lsp-package-path
  lsp-clients-executable-find
  :config
  (setq-local lsp-client-packages '(lsp-clangd))
  (trivialfis/lsp)
  (use-package lsp-clangd))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-find-references))

(use-package dash
  :autoload -first -map)

(defun lsp-clients-clangd-command ()
  "Find clangd executable.

Modified from `lsp-clients--clangd-command'."
  (if (string= system-type "windows-nt")
      "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\Llvm\\x64\\bin\\clangd.exe"
    (or lsp-clients-clangd-executable lsp-clients--clangd-default-executable
	(or (lsp-package-path 'clangd)
            (-first #'executable-find
                    (-map (lambda (version)
                            (concat "clangd" version))
                          ;; Prefer `clangd` without a version number appended.
                          (cl-list* "" (-map
					(lambda (vernum) (format "-%d" vernum))
					(number-sequence 17 6 -1)))))
            (lsp-clients-executable-find "xcodebuild" "-find-executable" "clangd")
            (lsp-clients-executable-find "xcrun" "--find" "clangd")))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection 'lsp-clients--clangd-command)
                  :major-modes '(c++-mode)
                  :remote? t
                  :server-id 'clangd-remote))

(defun trivialfis/clangd ()
  "Clangd configuration."
  (let ((clangd (lsp-clients-clangd-command)))
    (setq-default lsp-clients--clangd-command clangd
		  lsp-clients-clangd-args '("--header-insertion=never")))
  (lsp)
  (lsp-ui-mode)
  (flycheck-mode 1))

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

  (trivialfis/local-set-keys
   '(
     ;; Disaster
     ("C-c d a" . disaster)
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
