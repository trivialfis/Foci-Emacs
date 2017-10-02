;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package s
  :commands (s-chop-suffix))

(require 'window-purpose)
(require 'helm-xref)

;; (toggle-debug-on-error)
(defvar info-buffers '("*Flycheck errors*" "*Compile-Log*"))
(defun get-info-buffers ()
  "Get windows display info related buffers."
  (let ((buf-list (buffer-list))
	(win-list '()))
    (dolist (name info-buffers)
      (when (memq name buf-list)
	(let ((win (get-buffer-window name)))
	  (when win
	    (add-to-list win win-list)
	    ))))))

(defun helm-display-buffer-at-info (buffer)
  "Display helm buffer BUFFER."
  (let ((windows (get-info-buffers)))
    (if (listp windows)
	(progn
	  (message "a list of windows")
	  (select-window (car windows))
	  (switch-to-buffer buffer t t))
      (if windows
	  (progn
	    (message "single window")
	    (select-window windows)
	    (switch-to-buffer t t))
	(funcall #'helm-default-display-buffer buffer)))))

(defun my-helm-display-buffer-at-bottom (buffer)
  "BUFFER."
  (let ((window (or
		 (purpose-display-reuse-window-purpose buffer nil)
		 (purpose-display-reuse-window-buffer buffer nil)
		 (purpose-display-at-bottom buffer nil 0.25))))
    (message "My helm display at bottom.")
    (if window
        (progn
          (select-window window)
          ;; don't know why, but it doesn't work without `switch-to-buffer'
          (switch-to-buffer buffer t t))
      ;; in case the above methods weren't successful, fallback to default
      ;; helm display function
      (funcall #'helm-default-display-buffer buffer))))


(defun trivialfis/purpose ()
  "Common window-purpose configuration."
  (setq purpose-preferred-prompt 'helm)
  (define-key purpose-mode-map (kbd "C-x b") nil)
  (define-key purpose-mode-map (kbd "C-x C-f") nil)
  (add-to-list 'purpose-user-name-purposes '("*Flycheck errors*" . general-info))
  (add-to-list 'purpose-user-name-purposes '("*Compile-Log*" . general-info))
  (purpose-compile-user-configuration)
  ;; (setq helm-display-function #'helm-display-buffer-at-info)
  (purpose-mode 1))

(defun trivialfis/elisp()
  (flycheck-mode 1)
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (when (eq (buffer-size) 0)
    (insert ";;; " (buffer-name) " --- Summary\n"
	    ";;; Commentary:\n"
	    ";;; Code:"
	    "\n\n\n"
	    "(provide '" (s-chop-suffix ".el" (buffer-name)) ")\n"
	    ";;; " (buffer-name) " ends here"))

  (local-set-key (kbd "C-c C-a") 'byte-compile-file)
  (local-set-key (kbd "C-c C-b") 'eval-buffer)
  (aggressive-indent-mode)
  (trivialfis/purpose))


;; Semantic mode doesn't play well with elisp
;; (autoload 'semantic-add-system-include "semantic")
;; (semantic-add-system-include "~/.emacs.d/elpa/" 'emacs-lisp-mode)
;; (require 'srefactor-lisp)
;; (autoload 'srefactor-lisp-format-sexp "srefactor-lisp" t)
;; (autoload 'srefactor-lisp-one-line "srefactor-lisp" t)
;; (autoload 'srefactor-lisp-format-defun "srefactor-lisp" t)
;; (autoload 'srefactor-lisp-format-buffer "srefactor-lisp" t)

;; (local-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
;; (local-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
;; (local-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
;; (local-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)
;; (custom-semantic-mode 'emacs-lisp-mode)

;;; elisp-trivialfis.el ends here
