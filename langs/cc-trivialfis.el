;;; cc-trivialfis --- Summary
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
;;; Code:

(require 'misc-trivialfis)
(require 'cc-mode)
(require 'cmake-ide)

(use-package window-purpose
  :defer t
  :commands purpose-mode
  :config (progn
	    (setq purpose-preferred-prompt 'helm)
	    (define-key purpose-mode-map (kbd "C-x b") nil)
	    (define-key purpose-mode-map (kbd "C-x C-f") nil)
	    (add-to-list 'purpose-user-regexp-purposes '("\\*Man.*" . Man-page))
	    (purpose-compile-user-configuration)
	    (message "Purpose loaded.")))

;; (require 'flycheck)			; For language standard

;; (use-package company-clang
;;   :commands (trivialfis/company-clang))
;; (use-package company-c-headers)


;; (defun trivialfis/company-clang ()
;;   "Company clang configuration."
;;   (require 'company-clang)
;;   (require 'company-c-headers)
;;   (setq company-backends (delete 'company-semantic company-backends))
;;   (setq company-clang-arguments '("-std=c++14"))
;;   (require 'company-c-headers)
;;   (add-to-list 'company-c-headers-path-system "/usr/include/c++/6.3.1/")  ; Add c++ headers to company
;;   (add-to-list 'company-backends 'company-c-headers))

;; (use-package programming-trivialfis
;;   :commands trivialfis/semantic
;;   :config (message "Semantic loaded"))

(use-package cc-pkg-trivialfis
  :commands mumbo-find-library
  :config (message "cc-pkg loaded"))

(use-package rtags
  :commands rtags-start-process-unless-running
  :config (progn
	    (message "Rtags loaded")
	    (use-package company-rtags)))

(defun trivialfis/rtags ()
  "Rtags configuration.
Used only for nevigation."
  (rtags-start-process-unless-running)

  ;; (setq rtags-autostart-diagnostics t)
  ;; (rtags-diagnostics)
  (setq rtags-completions-enabled 1)
  (add-to-list 'company-backends 'company-rtags)
  (setq rtags-display-result-backend 'helm)
  (trivialfis/local-set-keys
   '(
     ("M-." . (lambda (arg) (interactive "P")
		(if arg
		    (rtags-find-symbol))
		(if (not (rtags-find-symbol-at-point))
     		    (helm-gtags-dwim))))
     ("M-?"     .  (lambda () (interactive)
		     (if (not (rtags-find-references-at-point))
			 (helm-gtags-find-rtag))))
     ("M-,"     .  rtags-location-stack-back)
     ("C-,"   .    rtags-location-stack-forward)
     ("C-c r r" .  rtags-rename-symbolrtags-next-match)
     ))
  ;; (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
  )

(defun trivialfis/irony ()
  "Irony mode configuration."
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-irony-c-headers)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
  (when (or (eq major-mode 'c-mode)	; Prevent from being loaded by c derived mode
  	    (eq major-mode 'c++-mode))
    (ignore-errors
      (irony-mode 1))))


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

(defvar original-project nil
  "A global variable to keep the directory for the CMake project before file jump.")
(defvar-local current-project nil
  "A local variable to keep the directory for current CMake project.")

(defun setup-ide ()
  "Set up rtags and CMake-ide if CMakeLists.txt is presented.
Otherwise do nothing.
When jumping around headers, keep the CMake project as the original one.
If the newly opened file belongs to a new project, then change the current
project to the new project."
  (let ((project-dir (cmake-ide--locate-project-dir)))
    ;; (message project-dir)
    (if project-dir
	(setq current-project project-dir
	      original-project project-dir)
      (setq current-project original-project))
    (when current-project
      (setq cmake-ide-build-dir (concat current-project "build")))
    (trivialfis/rtags)
    (cmake-ide-setup)))

(defun trivialfis/cc-base ()
  "Common configuration for c and c++ mode."
  ;; Company mode
  (setf company-backends '())
  (add-to-list 'company-backends 'company-keywords)
  (trivialfis/irony)
  (trivialfis/rtags)
  ;; (setup-ide)

  (defconst trivialfis/cc-style
    '("gnu"
      (c-offsets-alist . ((innamespace . [0])))))
  (c-add-style "trivialfis/cc-style" trivialfis/cc-style)
  (setq c-auto-newline nil)
  (c-set-style "trivialfis/cc-style")

  (trivialfis/local-set-keys
   '(
     ;; Disaster
     ("C-c d a" . disaster)
     ;; Clang formating
     ("C-c f b" . clang-format-buffer)
     ("C-c f r" . clang-format-region)
     ("C-c C-f" . find-file-in-project)

     ("C-c C-a" .  cmake-ide-compile)
     ))
  (flycheck-mode 1))


(defun trivialfis/c++ ()
  "Custom C++ mode."
  (setf irony-additional-clang-options '("-std=c++14" "-cc1"))
  ;; (setf flycheck-clang-language-standard "c++14")
  ;; (trivialfis/semantic 'c++-mode)
  (trivialfis/cc-base))

(defun trivialfis/c ()
  "Custom c mode."
  ;; (trivialfis/semantic 'c-mode)
  (trivialfis/cc-base)
  )

(provide 'c++-trivialfis)
;;; cc-trivialfis.el ends here
