;;; paradox-trivialfis --- Configuration for paradox
;;; Commentary:
;;; code:
(require 'paradox)
(require 'paradox-github)
(require 'archives-trivialfis)

(defun get-token ()
  "Get token from file."
  (let ((token-path "~/.emacs.d/global-settings/github-token"))
    (if (file-exists-p token-path)
	(with-temp-buffer
	  (insert-file-contents token-path)
	  (buffer-string))
      nil)))

(setq paradox-github-token (get-token))

(package-initialize)

(provide 'paradox-trivialfis)
;;; paradox-trivialfis.el ends here
