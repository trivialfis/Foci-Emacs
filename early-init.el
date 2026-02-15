(custom-set-variables
 '(custom-enabled-themes (quote (misterioso))))
(custom-set-faces `(default ((t (:background "gray13")))))

(setq initial-frame-alist '((tool-bar-lines . 0)
			    (width . 128) ; chars
			    (height . 32) ; lines
			    (left . 120)
			    (top . 80)))

;; Set default directory to home on Windows
;; Must be set in early init, no idea why.
(when (eq system-type 'windows-nt)
  (cd (getenv "USERPROFILE")))

;; Override the internal function `internal-macroexpand-for-load' to see where the error
;; occurs.
