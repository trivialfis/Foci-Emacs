;;; cc-trivialfis --- Summary
;;; Commentary:

;;; Code:
(require 'programming-trivialfis)	; For semantic
(require 'flycheck)			; For language standard
(require 'cc-mode)
(require 's)				; It's gonna be loaded anyway
(require 'company-clang)
;; (require 'company-c-headers)

(use-package f
  :commands (f-traverse-upwards
	     f-exists?
	     f-expand
	     f-read-text
	     f-write-text
	     f-join
	     f-append-text)
  :config (message "f loaded"))

(use-package semantic
  :commands (semantic-add-system-include
	     semantic-force-refresh))

(defvar c/c++-packages-list '(irony company-clang flycheck semantic)
  "This variable defines a list for packages needed to be configurated.")

(defun config-irony-clang-cdb(cflags)
  "Find existing .clang_complete file or create a new one"
  (let ((cdb (f-traverse-upwards
	      (lambda (path)
		(f-exists? (f-expand ".clang_complete" path))) ".")))
    (if cdb
	(progn
	  (let* ((old-cdb (f-read-text (f-join cdb ".clang_complete")))
		 (new-cdb (s-split "\n" (s-chomp old-cdb))))
	    (dolist (flag cflags)
	      (add-to-list
	       'new-cdb (s-replace-all '(("\n" . "") (" " . "")) flag)))
	    (f-write-text
	     (s-join "\n" new-cdb) 'utf-8 (f-join cdb ".clang_complete"))))
      (progn
	(dolist (flag cflags)
	  (f-append-text
	   (s-append "\n" (s-chomp flag)) 'utf-8 ".clang_complete"))))))


(defun config-libraries(lib-name)
  "Configurate c++ libraries for flycheck-clang, company-clang, semantic, irony"
  (let* ((cflags-raw (shell-command-to-string (concat "pkg-config --libs --cflags " lib-name)))
	 (cflags (s-split " " cflags-raw)))
    (if (featurep 'irony)
	(config-irony-clang-cdb cflags))
    (dolist (pc c/c++-packages-list)
      (if (featurep pc)
	  (dolist (flag cflags)
	    (cond ((equal pc 'company-clang)
		   (add-to-list 'company-clang-arguments flag))
		  ((equal pc 'flycheck) (add-to-list 'flycheck-clang-args flag))
		  ((equal pc 'semantic)
		   (if (s-starts-with? "-I" flag)
		       (semantic-add-system-include (s-replace "-I" "" flag) 'c++-mode)))
		  ((equal pc 'irony) ())
		  (t (message "Packages %s not supported by config-libraries" pc)))))))
  (flycheck-buffer)
  (semantic-force-refresh))


(defun add-guile()
  (interactive)
  (config-libraries "guile-2.0"))

(defun add-gtkmm()
  (interactive)
  (config-libraries "gtkmm-3.0"))

(defun trivialfis/cc-base ()
  "Common configuration for c and c++ mode."
  ;; Company mode
  (setf company-backends '())
  (add-to-list 'company-backends 'company-keywords)

  ;; Irony
  (add-hook 'irony-mode-hook #'irony-eldoc)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-irony-c-headers)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (when (or (eq major-mode 'c-mode)	; Prevent from being loaded by c derived mode
	    (eq major-mode 'c++-mode))
    (irony-mode 1))

  ;; Disaster
  (define-key c-mode-base-map (kbd "C-c d a") 'disaster)

  ;; Clang formating
  (define-key c-mode-base-map (kbd "C-c f b") 'clang-format-buffer)
  (define-key c-mode-base-map (kbd "C-c f r") 'clang-format-region)

  ;; Refactor
  (define-key c-mode-base-map (kbd "M-RET")   'srefactor-refactor-at-point)
  (define-key c-mode-base-map (kbd "C-c t")   'senator-fold-tag-toggle)

  (flycheck-mode 1))


(defun trivialfis/c++ ()
  "Custom C++ mode."
  ;; Company clang
  ;; (setq company-backends (delete 'company-semantic company-backends))
  ;; (setq company-clang-arguments '("-std=c++14"))
  ;; (require 'company-c-headers)
  ;; (add-to-list 'company-c-headers-path-system "/usr/include/c++/6.3.1/")  ; Add c++ headers to company
  ;; (add-to-list 'company-backends 'company-c-headers)

  (setf irony-additional-clang-options '("-std=c++14" "-cc1"))
  (setf flycheck-clang-language-standard "c++14")
  (trivialfis/semantic 'c++-mode)
  (trivialfis/cc-base))

(defun trivialfis/c ()
  "Custom c mode."
  (trivialfis/semantic 'c-mode)
  (trivialfis/cc-base))

(provide 'c++-trivialfis)
;;; cc-trivialfis.el ends here
