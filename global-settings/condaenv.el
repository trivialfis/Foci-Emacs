;;; condaenv.el --- Summary  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package conda
  :custom
  (conda-anaconda-home  "~/.anaconda")
  :autoload conda-env-activate-path conda--env-dir-is-valid conda-env-default-location
  :defer t)
(use-package f
  :autoload f-read-text f-join
  :defer t)
(use-package dash
  :autoload -filter
  :defer t)

(defun foundp (str)
  "Whether STR returned by `which' contain python."
  (not (equal (car (split-string str)) "which:")))

(defun trivialfis/python-from-which ()
  "Get python path by which shell command."
  (message "Python from `which'")
  (let* ((which (shell-command-to-string "which python"))
	 (which-python3 (shell-command-to-string "which python3"))
	 (which-python2 (shell-command-to-string "which python2"))
	 (has-python-p (foundp which))
	 (has-python3-p (foundp which-python3))
	 (has-python2-p (foundp which-python2)))
    (cond
     (has-python3-p (car (reverse (split-string which-python3))))
     (has-python-p (car (reverse (split-string which))))
     (has-python2-p (car (reverse (split-string which-python2))))
     (t "python"))))


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



(defun trivialfis/find-activate-conda-env ()
  "Activate conda env if there's any."
  (setq-default conda-env-home-directory "~/.anaconda/"
		conda-anaconda-home "~/.anaconda/")
  (let* ((hook ".conda-env.json")
	 (path (locate-dominating-file "." hook))
	 (project-file (if path (concat path hook) 'nil))
	 (json-str (if project-file (f-read-text project-file) 'nil))
	 (config (if json-str (json-parse-string json-str) 'nil))
	 (project-name (if config (gethash "project-name" config) 'nil))
	 (dirpath (if project-name (trivialfis/conda-env-name-to-dir project-name) 'nil)))
    (if (and project-name dirpath)
	(progn
	  ;; Caller should handle the tramp path.
	  (if (not (file-remote-p dirpath))
	      (conda-env-activate-path dirpath))
	  (message (format "Anaconda project name: %s\n" project-name))
	  (f-join (trivialfis/conda-env-name-to-dir project-name) "bin" "python3"))
      'nil)))

(provide 'condaenv)
;;; condaenv.el ends here
