;;; programming-trivialfis.el --- Configurations hooked to prog-mode.
;;; Commentary:
;;
;;  Configuration for prog mode.
;;
;;; Code:

(use-package ecb
  :init (setf ecb-fix-window-size 'width
	      ;; ecb-minor-mode-text "Ecb"
	      ecb-windows-width 0.18)
  :commands (ecb-activate)
  :config (message "ECB loaded."))

(use-package semantic/idle
  :commands (semantic-mode
	     semanticdb-enable-gnu-global-databases)
  :config (message "Semantic loaded."))

(use-package flycheck
  :init (declare-function flycheck-display-error-messages-unless-error-list
			  "Show messages of ERRORS unless the error list is visible.")
  :commands (flycheck-mode)
  :config (message "Flycheck loaded."))

(defun trivialfis/flycheck ()
  "Configurate flycheck."
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.23)))
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list))

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
  (trivialfis/flycheck)
  (hs-minor-mode 1))


(defun trivialfis/programming-post ()
  "Added after any programming mode configuration.")

(provide 'programming-trivialfis)
;;; programming-trivialfis.el ends here
