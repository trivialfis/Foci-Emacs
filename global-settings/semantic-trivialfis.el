;;; semantic-trivialfis --- Common setting for semantic mode
;;; Commentary:
;;; Code:

(require 'semantic/idle)
(require 'srefactor)

(defun trivialfis/semantic (MODE)
  "Custom semantic mode.
MODE: the major programming mode"
  (let ((semantic-submodes '(global-semantic-decoration-mode
			     global-semantic-idle-local-symbol-highlight-mode
			     global-semantic-highlight-func-mode
			     global-semanticdb-minor-mode
			     global-semantic-mru-bookmark-mode
			     ;; global-semantic-idle-summary-mode
			     global-semantic-stickyfunc-mode
			     )))
    (setq semantic-default-submodes (append semantic-default-submodes semantic-submodes)
	  semantic-idle-scheduler-idle-time 1)
    (semanticdb-enable-gnu-global-databases 'MODE)
    (set-face-attribute 'semantic-idle-symbol-highlight nil :background "gold" :foreground "yellow")
    (local-set-key "\C-cr" 'semantic-symref)
    (local-set-key "\C-cj" 'semantic-ia-fast-jump)
    (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
    (semantic-mode 1)))

(provide 'semantic-trivialfis)
;;; semantic-trivialfis.el ends here
