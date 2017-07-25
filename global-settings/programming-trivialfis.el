;;; programming-trivialfis.el --- Configurations hooked to prog-mode.
;;; Commentary:
;;; Code:
(use-package ecb
  :commands (ecb-activate)
  :init (setf ecb-fix-window-size 'width
	      ;; ecb-minor-mode-text "Ecb"
	      ecb-windows-width 0.18))

(use-package semantic/idle
  :commands (semantic-mode
	     semanticdb-enable-gnu-global-databases))

(defun trivialfis/flycheck ()
  "Configurate flycheck."
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.23))))

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
    ;; (set-face-attribute 'semantic-idle-symbol-highlight nil :background "gold" :foreground "yellow")
    ;; (local-set-key "M-," 'semantic-symref)
    ;; (local-set-key "C-." 'semantic-ia-fast-jump)
    ;; (local-set-key "C-," 'semantic-mrub-switch-tags)
    ;; (eval-after-load 'helm
    ;;   (local-set-key (kbd "C-.") 'helm-semantic-or-imenu))
    (semantic-mode 1)))

(defun trivialfis/programming-init ()
  "Added before any programming mode configuration."
  (add-hook 'eldoc-mode-hook #'(lambda ()
				 (setf eldoc-idle-delay 0)))
  (col-highlight-set-interval 2)
  (setq col-highlight-overlay-priority 0)
  (toggle-highlight-column-when-idle 1)
  (trivialfis/flycheck))


(defun trivialfis/programming-post ()
  "Added after any programming mode configuration.")

(provide 'programming-trivialfis)
;;; programming-trivialfis.el ends here
