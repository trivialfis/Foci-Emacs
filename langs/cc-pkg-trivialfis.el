;;; cc-pkg-trivialfis.el --- Summary
;;; Commentary:
;;; Code:
(require 'programming-trivialfis)
(require 'flycheck)
(require 'company-clang)
(require 'helm)
(require 'f)
(require 's)


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
  (let* ((lib-name (car (split-string lib-name " " t)))
	 (cflags (s-split
		  " " (shell-command-to-string
		       (concat "pkg-config --libs --cflags " lib-name)))))
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


(defun helm-source-pkg ()
  "Return a `helm' source for libraries from pkg-config."
  (helm-build-sync-source "PKG-CONFIG"
    :candidates
    (lambda ()
      (split-string
       (shell-command-to-string "pkg-config --list-all")
       "\n"))
    :candidate-number-limit 500
    :candidate-transformer
    (lambda (candidates)
      (sort candidates #'string-lessp))
    :nomark t
    :action '(("Add" . config-libraries))))

(defun mumbo-find-library ()
  "Find library by name."
  (interactive)
  (if (featurep 'helm)
      (helm :sources (helm-source-pkg)
	    :buffer "*helm pkg*")))

(provide 'cc-pkg-trivialfis)
;;; cc-pkg-trivialfis.el ends here
