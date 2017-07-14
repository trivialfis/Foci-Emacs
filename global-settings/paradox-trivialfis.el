;;; paradox-trivialfis --- Configuration for paradox
;;; Commentary:
;;; code:
(require 'paradox)
(require 'paradox-github)
(defun get-token ()
  "Get token from file."
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/global-settings/github-token")
    (buffer-string)))

(setf package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")  ; tsinghua's mirror
			 ("org"   . "https://elpa.emacs-china.org/org/")
			 ("gnu"   . "https://elpa.emacs-china.org/gnu/")
			 )
      paradox-github-token (get-token))
(package-initialize)

(provide 'paradox-trivialfis)
;;; paradox-trivialfis.el ends here
