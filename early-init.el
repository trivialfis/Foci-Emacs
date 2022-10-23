(custom-set-variables
 '(custom-enabled-themes (quote (misterioso))))

(setq initial-frame-alist '((tool-bar-lines . 0)
			    (width . 128) ; chars
			    (height . 32) ; lines
			    (left . 120)
			    (top . 80)))

;; Override the internal function `internal-macroexpand-for-load' to see where the error
;; occurs.
