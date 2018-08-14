;;; cuda-trivialfis.el --- Summary
;;; Commentary:
;;;
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
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
;;; Code:

(require 'flycheck)
(require 'cc-trivialfis)
(require 'cl-lib)
(require 'cl-seq)
(require 'google-c-style)
(require 'cuda-mode)

(defun clang-wrapper--get-pos ()
  "Get current position."
  (save-excursion
    (format "%d %d"
	    (line-number-at-pos)
	    (1+ (length
		 (encode-coding-region
		  (line-beginning-position)
		  (point)
		  'utf-8
		  t))))))

(defun execute-complete (args)
  "Execute clang-complete, ARGS."
  (let* ((complete-str (string-join (list "complete" (buffer-file-name)
					  (clang-wrapper--get-pos)) " " ))
	 (cmd (string-join (list "~/Workspace/clang-wrapper/clang-complete" complete-str) " "))
	 (raw-str (shell-command-to-string cmd))
	 (candidates-raw (split-string raw-str))
	 (candidates (remove-if-not
		      (lambda (c)
			(and (stringp c)
			     (not (string-prefix-p "[" c))
			     (not (string-prefix-p ":" c))
			     (not (string-prefix-p "COMPLETION" c))))
		      candidates-raw)))
    candidates))

(defun clang-wrapper--get-candidates ()
  (let* ((args (list "complete" (buffer-file-name)
		     (clang-wrapper--get-pos)))
	 (candidates
	  (execute-complete args)))
    candidates))

(defun clang-wrapper-backend (command &optional arg &rest ignored)
  "A simple company backend."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'clang-wrapper-backend))
    (init)
    (prefix (and (eq major-mode 'cuda-mode)
		 (company-grab-symbol)))
    (candidates (clang-wrapper--get-candidates))))

(defun trivialfis/clang-wrapper ()
  "Use a clang wrapper which redirects clang output to Emacs."
  (setq
   flycheck-clang-language-standard "c++14"
   flycheck-clang-include-path (list "/usr/local/cuda/include"))

  (defvar cuda-path "/usr/local/cuda-9.2")

  (flycheck-define-checker clang-wrapper
    "A C/C++ syntax checker using Clang."
    :command ("~/Workspace/clang-wrapper/clang-check"
	      (eval (buffer-file-name)))
    :standard-input nil
    :error-patterns
    ((error line-start
            (message "In file included from") " " (or "<stdin>" (file-name))
            ":" line ":" line-end)
     (info line-start (or "<stdin>" (file-name)) ":" line ":" column
           ": note: " (optional (message)) line-end)
     (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
              ": warning: " (optional (message)) line-end)
     (error line-start (or "<stdin>" (file-name)) ":" line ":" column
            ": " (or "fatal error" "error") ": " (optional (message)) line-end))
    :error-filter
    (lambda (errors)
      (let ((errors (flycheck-sanitize-errors errors)))
	(dolist (err errors)
          ;; Clang will output empty messages for #error/#warning pragmas without
          ;; messages.  We fill these empty errors with a dummy message to get
          ;; them past our error filtering
          (setf (flycheck-error-message err)
		(or (flycheck-error-message err) "no message")))
	(flycheck-fold-include-levels errors "In file included from")))
    :modes (cuda-mode)
    :predicate flycheck-buffer-saved-p
    :next-checkers ((warning . c/c++-cppcheck))))


(defun trivialfis/cuda-base ()
  "Base configuration for customized cuda mode."
  (trivialfis/clang-wrapper)
  (flycheck-select-checker 'clang-wrapper)
  (c-add-style "google-c-style" google-c-style)
  (c-set-style "google-c-style")
  (eval-and-compile
    (require 'company-irony-c-headers))
  (setq c-auto-newline nil
	irony-additional-clang-options '("-std=c++14")
	company-irony-c-headers--modes (cons 'cuda-mode
					     company-irony-c-headers--modes))
  ;; (add-to-list 'company-backends 'clang-wrapper-backend)
  (trivialfis/company-clang)
  (trivialfis/use-rtags)
  ;; (trivialfis/irony)
  (flycheck-mode 1))


(defun trivialfis/cuda ()
  "Custom CUDA mode."
  ;; (trivialfis/cuda-base)
  (trivialfis/c++))

(provide 'cuda-trivailfis)
;;; cuda-trivialfis.el ends here
