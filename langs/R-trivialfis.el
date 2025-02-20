;;; R-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ess-r-mode)

(use-package condaenv
  :defer t
  :autoload trivialfis/find-activate-conda-env)
(use-package lsp-trivialfis
  :autoload trivialfis/lsp)
(use-package lsp-ui-mode
  :defer t
  :commands lsp-ui-mode)
(use-package files
  :defer t
  :autoload locate-dominating-file)
(use-package f
  :autoload f-join)

;; https://github.com/REditorSupport/languageserver
;; install.packages("languageserver", Ncpus=parallel::detectCores())
(use-package lsp-mode
  :defer t
  :commands lsp
  :config
  ;; check renv first, then anaconda.
  (let ((profile (locate-dominating-file "." ".Rprofile")))
    (if (and profile
	     ;; Make sure the `.Rprfoile' under the home directory is not used
	     (not (string=
		   (file-name-as-directory (expand-file-name profile))
		   (file-name-as-directory (getenv "HOME")))))
	(let* ((activate-path (f-join profile "renv" "activate.R"))
	       ;; Otherwise renv will bootstrap at current working directory
	       (chdir (format "setwd('%s')" profile))
	       (activate (format "source('%s')" activate-path))
	       (server-run "languageserver::run()")
	       (run (format "%s;%s;%s" chdir activate server-run)))
	  (trivialfis/lsp)
	  (message "renv activate script: %s" activate)
	  (setq-local lsp-clients-r-server-command `("R" "--slave" "-e" ,run))
	  (lsp-ui-mode 1))
      (let ((condapy (trivialfis/find-activate-conda-env)))
	(if condapy
	    (progn
	      (trivialfis/lsp)
	      (lsp-ui-mode 1))))))
  (use-package lsp-r))


(defun trivialfis/ess ()
  "Configuration for ess."
  (setq-local ess-style 'RStudio
	      ess-indent-offset 2)
  (flymake-mode 0)
  (flycheck-mode 1))

(defun trivialfis/R ()
  "R configuration."
  (trivialfis/ess)
  (lsp))

(provide 'R-trivialfis)
;;; R-trivialfis.el ends here
