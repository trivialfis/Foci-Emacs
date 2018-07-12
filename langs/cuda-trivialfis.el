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

(defun trivialfis/cuda ()
  "Custom CUDA mode."
  (trivialfis/cc-base)

  (eval-and-compile
    (require 'company-irony-c-headers))

  (setq flycheck-clang-language-standard "cuda"
	flycheck-clang-args '("-nocudalib")
	irony-additional-clang-options '("-nocudalib")
	company-irony-c-headers--modes (cons 'cuda-mode
					     company-irony-c-headers--modes))

  (setq flycheck-clang-include-path '("/usr/local/cuda/include"))
  (defvar cuda-gpu-arch "sm_50")

  (flycheck-define-checker cuda-clang
    "A C/C++ syntax checker using Clang.

See URL `http://clang.llvm.org/'."
    :command ("clang++"
              "-fsyntax-only"
              "-fno-color-diagnostics"    ; Do not include color codes in output
              "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                        ; location
              "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
              "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
              (option "-std=" flycheck-clang-language-standard concat)
	      (option "--cuda-gpu-arch=" cuda-gpu-arch concat)
              (option-flag "-pedantic" flycheck-clang-pedantic)
              (option-flag "-pedantic-errors" flycheck-clang-pedantic-errors)
              (option "-stdlib=" flycheck-clang-standard-library concat)
              (option-flag "-fms-extensions" flycheck-clang-ms-extensions)
              (option-flag "-fno-exceptions" flycheck-clang-no-exceptions)
              (option-flag "-fno-rtti" flycheck-clang-no-rtti)
              (option-flag "-fblocks" flycheck-clang-blocks)
              (option-list "-include" flycheck-clang-includes)
              (option-list "-W" flycheck-clang-warnings concat)
              (option-list "-D" flycheck-clang-definitions concat)
              (option-list "-I" flycheck-clang-include-path)
              (eval flycheck-clang-args)
              "-x" (eval
                    (pcase major-mode
                      (`cuda-mode "cuda")))
              ;; Read from standard input
              "-")
    :standard-input t
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
    :modes (c-mode c++-mode cuda-mode)
    :next-checkers ((warning . c/c++-cppcheck)))

  (flycheck-select-checker 'cuda-clang)
  (flycheck-mode 1))

(provide 'cuda-trivailfis)
;;; cuda-trivialfis.el ends here
