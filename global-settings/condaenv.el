;;; condaenv.el --- Summary
;;; Commentary:
;;; Code:
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(use-package conda
  :custom
  (conda-anaconda-home  "~/.anaconda")
  :autoload conda-env-activate
  :defer t)
(use-package f
  :autoload f-read-text
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

(defun trivialfis/find-activate-conda-env ()
  "Activate conda env if there's any."
  (setq-default conda-env-home-directory "~/.anaconda/"
		conda-anaconda-home "~/.anaconda/")
  ;; FIXME: How to use local variables without being asked for safety for each opened
  ;; buffer?

  ;; (hack-dir-local-variables)
  ;; (print dir-local-variables-alist)
  (let* ((hook ".conda-env.json")
	 (path (locate-dominating-file "." hook))
	 (project-file (if path (concat path hook) 'nil))
	 (json-str (if project-file (f-read-text project-file) 'nil))
	 (config (if json-str (json-parse-string json-str) 'nil))
	 (project-name (if config (gethash "project-name" config) 'nil)))
    (if project-name
	(progn
	  (conda-env-activate project-name)
	  (message (format "Anaconda project name: %s\n" project-name))
	  (trivialfis/python-from-which))
      'nil)))

(provide 'condaenv)
;;; condaenv.el ends here
