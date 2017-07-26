;;; helm-trivialfis --- Summary
;;; Commentary:
;;; Code:

(require 'helm)
(require 'helm-files)
(require 'helm-config)
(require 'helm-net)
(require 'spaceline-segments)

(defun trivialfis/helm ()
  "Helm mode configuration."
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (define-key helm-map (kbd "C-n") 'next-history-element)
  (define-key helm-map (kbd "C-p") 'previous-history-element)
  (define-key helm-map (kbd "M-n") 'helm-next-line)
  (define-key helm-map (kbd "M-p") 'helm-previous-line)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line        nil
	helm-autoresize-max-height            0
	helm-autoresize-min-height            20
	helm-buffers-fuzzy-matching           t
	helm-recentf-fuzzy-match              t)

  (helm-autoresize-mode 1)

  ;; (defun helm-hide-minibuffer-maybe ()
  ;;   "Hide minibuffer in Helm session if we use the header line as input field."
  ;;   (when (with-helm-buffer helm-echo-input-in-header-line)
  ;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
  ;; 	(overlay-put ov 'window (selected-window))
  ;; 	(overlay-put ov 'face
  ;; 		     (let ((bg-color (face-background 'default nil)))
  ;; 		       `(:background ,bg-color :foreground ,bg-color)))
  ;; 	(setq-local cursor-type nil))))

  ;; (add-hook 'helm-minibuffer-set-up-hook
  ;; 	    'helm-hide-minibuffer-maybe)
  ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

  (spaceline-toggle-helm-buffer-id-on)
  (spaceline-toggle-helm-number-on)
  (spaceline-toggle-helm-help-on)

  (helm-mode 1))


(trivialfis/helm)
(provide 'helm-trivialfis)
;;; helm-trivialfis.el ends here
