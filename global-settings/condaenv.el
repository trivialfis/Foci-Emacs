;;; condaenv.el --- Summary  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(defun trivialfis/conda-get-home()
  (if (string= system-type "windows-nt")
      "C:\\Users\\jiamingy\\AppData\\Local\\miniforge3"
    "~/.anaconda"))

(use-package conda
  :custom
  (conda-anaconda-home (trivialfis/conda-get-home))
  :autoload conda--env-dir-is-valid conda-env-default-location
  :defer t
  :config
  (if (string= system-type "windows-nt")
      (let* ((conda-home (trivialfis/conda-get-home))
	     (bin-path (f-join conda-home conda-env-executables-dir)))
	;; Set an internal variable in conda.el .
	(setq conda--executable-path (locate-file "conda.exe" (list bin-path) exec-suffixes 'executable)))))

(use-package f
  :autoload f-join
  :defer t)
(use-package dash
  :autoload -filter
  :defer t)

(defun trivialfis/make-tramp-path (path)
  "Turn PATH into a remote path if working a on remote buffer."
  (if (file-remote-p default-directory)
      (tramp-make-tramp-file-name (tramp-dissect-file-name default-directory) path)
    path))

;; Got this from the conda.el package, modified to work with tramp.
(defun trivialfis/conda-env-name-to-dir (name)
  "Translate NAME to the directory where the environment is located."
  (if (and (string= name "base")
           (conda--env-dir-is-valid conda-anaconda-home))
      (file-name-as-directory (expand-file-name conda-anaconda-home))
    (let* ((default-location (trivialfis/make-tramp-path  (file-name-as-directory (conda-env-default-location))))
           (initial-possibilities (list name (concat default-location name)))
           (possibilities (if (boundp 'venv-location)
                              (if (stringp venv-location)
                                  (cons venv-location initial-possibilities)
                                (nconc venv-location initial-possibilities))
                            initial-possibilities))
           (matches (-filter 'conda--env-dir-is-valid possibilities)))
      (if (> (length matches) 0)
          (file-name-as-directory (expand-file-name (car matches)))
        (progn
	  (display-warning 'conda (format "Environment %s not found." name) :warning)
	  nil)))))
(provide 'condaenv)
;;; condaenv.el ends here
