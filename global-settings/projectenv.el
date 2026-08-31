;;; projectenv.el --- Project-local configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'toml)

(defconst trivialfis/project-config-file "foci.toml"
  "Name of the project-local Emacs configuration file.")

(defun trivialfis/project-config (&optional directory)
  "Read the project configuration above DIRECTORY.

Return a cons cell (CONFIG-FILE . DATA), where DATA is the alist
returned by `toml:read-from-file'.  Return nil when no project file
exists."
  (when-let ((root (locate-dominating-file
                    (or directory default-directory)
                    trivialfis/project-config-file)))
    (let ((file (expand-file-name trivialfis/project-config-file root)))
      (let ((inhibit-message t))
        (cons file (toml:read-from-file file))))))

(defun trivialfis/project-config-get (table &rest keys)
  "Return the value below KEYS in TOML alist TABLE.

Return nil when any key is absent."
  (let ((value table))
    (while (and keys (listp value))
      (setq value (alist-get (pop keys) value nil nil #'string=)))
    (and (null keys) value)))

(provide 'projectenv)
;;; projectenv.el ends here
