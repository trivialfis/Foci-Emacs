;;; cmake-trivialfis.el --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'lsp)
(require 'lsp-trivialfis)

(push '(cmake-mode . "cmake")
      lsp-language-id-configuration)

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection (list "lsp-cmaked"))
  :major-modes '(cmake-mode)
  :priority 1
  :multi-root t
  :server-id 'cmake-lsp))

(use-package company-cmake
  :defines company-cmake-executable)

(defun trivialfis/cmake ()
  "CMake configuration."
  ;; ctags -e -R --languages=CMake
  ;; (require 'company-cmake)

  ;; Locate CMake
  (if (and (not company-cmake-executable)
	   (string= system-type "windows-nt"))
      (let* ((bin-path "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\Common7\\IDE\\CommonExtensions\\Microsoft\\CMake\\CMake\\bin")
	     (path (locate-file "cmake" (list bin-path) exec-suffixes 'executable)))
	(if path
	    (setq company-cmake-executable path)))))

(provide 'cmake-trivialfis)
;;; cmake-trivialfis.el ends here
