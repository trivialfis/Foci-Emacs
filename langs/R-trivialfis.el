;;; R-trivialfis.el --- Summary  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(eval-when-compile
  (require 'use-package))

(use-package ess-r-mode)

(use-package pixienv
  :defer t
  :autoload trivialfis/find-activate-pixi-r)
(use-package lsp-trivialfis
  :autoload trivialfis/lsp)
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)
(use-package flycheck
  :defer t
  :commands flycheck-mode)
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
  (use-package lsp-r))

(defun trivialfis/R--renv-root ()
  "Return the current project's renv root, or nil."
  (when-let ((profile (locate-dominating-file "." ".Rprofile")))
    ;; Do not treat the user's home-level .Rprofile as a project renv.
    (unless (string=
             (file-name-as-directory (expand-file-name profile))
             (file-name-as-directory (expand-file-name (getenv "HOME"))))
      profile)))

(defun trivialfis/R--configure-environment ()
  "Configure the R command for renv or the project Pixi environment."
  (if-let ((profile (trivialfis/R--renv-root)))
      (let* ((activate-path (f-join profile "renv" "activate.R"))
             ;; Otherwise renv bootstraps in `default-directory'.
             (chdir (format "setwd('%s')" profile))
             (activate (format "source('%s')" activate-path))
             (server-run "languageserver::run()")
             (run (format "%s;%s;%s" chdir activate server-run)))
        (message "renv activate script: %s" activate)
        (setq-local lsp-clients-r-server-command
                    `("R" "--slave" "-e" ,run)))
    (when-let ((r-command (trivialfis/find-activate-pixi-r)))
      (setq-local inferior-ess-r-program r-command
                  lsp-clients-r-server-command
                  `(,r-command "--slave" "-e" "languageserver::run()")))))

(defun trivialfis/ess ()
  "Configuration for ess."
  (setq-local ess-style 'RStudio
	      ess-indent-offset 2)
  (flymake-mode 0)
  (flycheck-mode 1))

(defun trivialfis/R ()
  "R configuration."
  (trivialfis/ess)
  (require 'lsp-mode)
  (trivialfis/R--configure-environment)
  (trivialfis/lsp)
  (lsp-ui-mode 1)
  (lsp))

(provide 'R-trivialfis)
;;; R-trivialfis.el ends here
