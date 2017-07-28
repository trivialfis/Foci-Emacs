;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global settings---------------------------------------------------------------------------------
(load-theme 'atom-dark t)
(setq initial-major-mode 'fundamental-mode  ; Prevents loading emacs lisp mode automatically
      inhibit-startup-screen t
      frame-title-format '((:eval (if (buffer-file-name) ; Set the frame title to display file path and name
				      (abbreviate-file-name
				       (buffer-file-name))
				    "%b")))
      package-enable-at-startup nil
      visible-bell t)			; Remove the beep

;; (setq-default display-line-numbers t)
;; (modify-all-frames-parameters '((scroll-bar-width . 8)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.50")
 '(package-selected-packages
   (quote
    (hideshowvis xterm-color flyspell-correct-helm org-dashboard toc-org vala-mode flycheck-irony company-rtags flycheck-rtags helm-rtags rtags toml-mode rust-mode package-lint font-lock-studio langtool org-bullets company-irony company-irony-c-headers irony irony-eldoc projectile cpputils-cmake clang-format aggressive-indent spaceline disaster paradox helm org markdown-mode slime vline cmake-mode go-mode all-the-icons-dired use-package company-math font-lock+ col-highlight winum powerline spacemacs-theme atom-dark-theme company-go highlight-symbol header2 company-quickhelp company-auctex auctex company-c-headers srefactor company-php company-shell fill-column-indicator ecb magit elpy flycheck company)))
 '(paradox-automatically-star t)
 '(paradox-github-token t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/global-settings/")
(add-to-list 'load-path "~/.emacs.d/langs/")

(use-package misc-trivialfis
  :commands (trivialfis/pop-frame
	     trivialfis/close-frame
	     trivialfis/goto-pos
	     trivialfis/local-set-keys)
  :config (message "Misc loaded."))

;; ANSI
(autoload 'ansi-color-mode "stupid-color-mode"
  :interactive t
  "Fundamental ANSI color mode")
(autoload 'trivial-ansi-mode "trivial-ansi"
  :interactive t
  "Minor mode for handling ANSI 24bit color sequences")


(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t		;Don't delink hardlinks
      version-control t			;Use version numbers on backups
      delete-old-versions t		;Automatically delete excess backups
      kept-new-versions 5		;How many of the newest versions to keep
      kept-old-versions 1		;How many of the old versions to keep
      )


(tool-bar-mode 0)			;Remove tool-bar
(scroll-bar-mode 0)
(show-paren-mode 1)			;Show matching paras
(global-linum-mode 1)
(electric-pair-mode 1)


;; Paradox
(let ((packages-autoload '(paradox-list-packages
			   paradox-upgrade-packages
			   package-list-packages
			   package-list-packages-no-fetch
			   package-install)))
  (dolist (x packages-autoload)
    (autoload x "paradox-trivialfis" :interactive t)))

;; Spaceline theme
(require 'spaceline-config)
(setq powerline-height 25
      ;; powerline-default-separator 'wave
      spaceline-window-numbers-unicode t)
;; (spaceline-toggle-battery-off)
;; (spaceline-toggle-anzu-off)
(spaceline-helm-mode)
(spaceline-emacs-theme)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(require 'winum)
(setq winum-auto-setup-mode-line nil)
(set-face-attribute 'winum-face nil :weight 'bold)
(winum-mode)


(defun load-helm ()
  "Load up the helm settings and remove this function from hook."
  (unless (eq this-command 'trivialfis/close-frame)
    (load "helm-trivialfis" :nomessage t)
    (remove-hook 'pre-command-hook 'load-helm)))
(add-hook 'pre-command-hook 'load-helm)


;; Fill indicator mode
(autoload 'fci-mode "fill-col")


;; highlight current line
(global-hl-line-mode 1)
(set-face-foreground 'highlight nil) ; keep the syntex highlighting


;; Highlight symbol at point
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0)
(add-hook 'after-change-major-mode-hook 'highlight-symbol-mode)


;; Auto insert and update header info
;; (autoload 'auto-make-header "header2")
;; (autoload 'auto-update-file-header "header2")
;; (add-hook 'write-file-hooks 'auto-update-file-header)


;; company mode
(require 'company-dabbrev)
(setq company-dabbrev-downcase 0
      company-idle-delay 0)
(add-hook 'after-init-hook 'global-company-mode)
;; Quickhelp
(autoload 'company-quickhelp-setup "company-quickhelp-c")
(add-hook 'company-mode-hook 'company-quickhelp-setup)


;; org mode
(let ((org-init-func '(trivialfis/org-init
		       trivialfis/org-post))
      (org-config-file "org-trivialfis"))
  (dolist (func org-init-func)
    (autoload func org-config-file))
  (add-hook 'org-load-hook 'trivialfis/org-init)
  (add-hook 'org-mode-hook 'trivialfis/org-post))


(global-unset-key (kbd "C-h C-o"))

(defun trivialfis/global-set-keys (key-commands)
  "Global-set-key with list.
KEY-COMMANDS: A list containing one or more (key command)"
  (dolist (kc key-commands)
    (define-key (current-global-map)
      (kbd (car kc))
      (cdr kc))))

(trivialfis/global-set-keys
 '(

   ;; Misc
   ("C-c d w"      .      delete-trailing-whitespace)
   ("C-x C-b"      .                         ibuffer)
   ("C-c i"        .                        fci-mode)
   ("C-c g"        .             trivialfis/goto-pos)

   ;; Paradox
   ("C-c C-l"      .           paradox-list-packages)
   ("C-c C-u"               paradox-upgrade-packages)

   ;; Highlight symbols
   ("M-n"          .           highlight-symbol-next)
   ("M-p"          .           highlight-symbol-prev)

   ;; Helm
   ("M-x"          .                        helm-M-x)
   ("C-x b"        .                       helm-mini)
   ("C-x C-f"      .                 helm-find-files)

   ;; Frames
   ("C-x 4"        .            trivialfis/pop-frame)
   ("C-x C-c"      .          trivialfis/close-frame)

   ))

;; End global settings--------------------------------------------------------------------------


;; Language settings----------------------------------------------------------------------------

;; Text mode
(autoload 'trivialfis/text "text-trivialfis")
(add-hook 'text-mode-hook 'trivialfis/text)

;; Prog mode
(autoload 'trivialfis/programming-init "programming-trivialfis")
(add-hook 'prog-mode-hook 'trivialfis/programming-init)

;; Clisp
(autoload 'trivialfis/lisp "lisp-trivialfis")
(add-hook 'lisp-mode-hook 'trivialfis/lisp)

;; LaTeX mode
(autoload 'trivialfis/LaTeX "LaTeX-trivialfis")
(add-hook 'LaTeX-mode-hook 'trivialfis/LaTeX)

;; C++ mode
(autoload 'trivialfis/c++ "cc-trivialfis")
(add-hook 'c++-mode-hook 'trivialfis/c++)

;; C mode
(autoload 'trivialfis/c "cc-trivialfis")
(add-hook 'c-mode-hook 'trivialfis/c)

;; rust mode
(autoload 'trivialfis/rust "rust-trivialfis")
(add-hook 'rust-mode-hook 'trivialfis/rust)

;; javascript  mode
(autoload 'trivialfis/js "js-trivialfis")
(add-hook 'js-mode-hook 'trivialfis/js)

;; Elisp mode
(autoload 'trivialfis/elisp "elisp-trivialfis")
(add-hook 'emacs-lisp-mode-hook 'trivialfis/elisp)

;; Python mode
(autoload 'trivialfis/python "python-trivialfis")
(add-hook 'python-mode-hook 'trivialfis/python)

;; php mode
(autoload 'trivialfis/php "php-trivialfis")
(add-hook 'php-mode-hook 'trivialfis/php)

;; bash mode
(autoload 'trivialfis/bash "bash-trivialfis")
(add-hook 'sh-mode-hook 'trivialfis/bash)

;; End language settings------------------------------------------------------------------------

(provide '.init.el)
;;; init.el ends here
