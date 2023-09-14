;;; latex-trivialfis --- Configuration for LaTeX -*- lexical-binding: t -*-
;;;
;;; Copyright © 2016-2019 Fis Trivial <jm.yuan@outlook.com>
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

(require 'tex)
(require 'latex)
(require 'company)
(require 'company-posframe)
(require 'text-trivialfis)
(require 'f)
(use-package lsp
  :commands lsp)
(require 'subr-x)			; string-join

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package flycheck
  :defer t
  :commands flycheck-mode)
(use-package async
  :defer t
  :commands async-start)

(defun trivialfis/f--loop-files (files target-file)
  "Check each file in FILES for TARGET-FILE."
  (let ((home-dir (f-full "~")))
    (catch 'found
      (if files
	  (dolist (fpath files)
	    (let ((fname (downcase (f-filename fpath))))
	      (if (string-equal fname target-file)
		  (throw 'found 't))
	      (if (string-equal fpath home-dir)
		  (throw 'found 'f))))
	'f))))

(defun trivialfis/find-makefile()
  "Find make file."
  (let* ((makefile "makefile")
	 (found-path
	  (f-traverse-upwards
	   #'(lambda (path)
	       (let ((dir (if (f-directory? path)
			      path
			    (f-dirname path))))
		 (let ((files (if dir (f-files dir) 'nil)))
		   (trivialfis/f--loop-files files makefile))))
	   (f-dirname (buffer-file-name)))))
    (if found-path
	(f-join found-path "makefile")
      'nil)))

(defun trivialfis/LaTeX()
  "Configure LaTeX mode."
  (LaTeX-math-mode)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-latex-commands)
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Compile log*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.23)))

  (let* ((makefile (trivialfis/find-makefile))
	 (filename (buffer-file-name))
	 (makefile-dir (f-dirname makefile)))

    (if (and makefile
	     (s-ends-with? ".tex" filename))
	(progn
	  (message (string-join `("Found makefile: " ,makefile)))
	  (message "Use C-c C-a to call make.")
	  (local-set-key
	   (kbd "C-c C-a")
	   #'(lambda ()
	       (interactive)
	       (async-start
		#'(lambda ()
		    (let ((command (if makefile-dir
				       (string-join `("make -C " ,makefile-dir))
				     "make")))
		      (concat
		       (shell-command-to-string command)
		       "\n\n")))
		#'(lambda (result)
		    (let ((compile-buf
			   (get-buffer-create "*Compile log*")))
		      (set-buffer compile-buf)
		      (goto-char (point-max))
		      (insert result)
		      (display-buffer compile-buf)))))))))
  (flycheck-mode 1)
  (trivialfis/_text)
  (company-posframe-mode))

(provide 'latex-trivialfis)
;;; latex-trivialfis ends here
