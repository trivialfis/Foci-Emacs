;;; paradox-trivialfis --- Configuration for paradox. -*- lexical-binding: t -*-
;;; Commentary:
;;; code:
(require 'paradox)
(require 'paradox-github)
(require 'archives-trivialfis)

(defun trivialfis/get-token ()
  "Get token from file."
  (let ((token-path "~/.emacs.d/global-settings/github-token"))
    (if (file-exists-p token-path)
	(with-temp-buffer
	  (insert-file-contents token-path)
	  (buffer-string))
      nil)))

(defun trivialfis/package ()
  "Configuration for packages using paradox."
  (setq paradox-github-token (trivialfis/get-token))
  (package-initialize))
(trivialfis/package)

(provide 'paradox-trivialfis)
;;; paradox-trivialfis.el ends here
