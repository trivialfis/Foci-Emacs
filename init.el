;;; package --- Summary  -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Copyright © 2015-2020 Fis Trivial <jm.yuan@outlook.com>
;;;
;;; This file is part of Foci-Emacs.
;;;
;;; Foci-Emacs is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Foci-Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Foci-Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; Code:

;; Global settings------------------------------------------------------------
(setq initial-major-mode 'fundamental-mode  ; Prevents loading emacs lisp mode automatically
      inhibit-startup-screen t
      ;; Set the frame title to display file path and name
      frame-title-format '((:eval (if (buffer-file-name)
				      (abbreviate-file-name
				       (buffer-file-name))
				    "%b")))
      package-enable-at-startup nil
      visible-bell t			; Remove the beep
      )
(set-face-attribute 'default nil :height 90)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(put 'narrow-to-region 'disabled nil)
(if (> emacs-major-version 25)
    (global-display-line-numbers-mode t)
  (global-linum-mode t))
;; (modify-all-frames-parameters '((scroll-bar-width . 8)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(auth-source-save-behavior nil)
 '(ecb-options-version "2.50")
 '(ediff-split-window-function 'split-window-sensibly)
 '(package-selected-packages
   '(visual-fill-column org-roam groovy-mode lsp-metals kotlin-mode minimap nginx-mode htmlize lsp-haskell conda polymode lsp-mode cask company-math flyspell-correct-helm paradox paredit scala-mode winum racer sml-mode dockerfile-mode powershell lsp-ui-sideline dap-mode lua-mode realgud pyim github-review json-mode let-alist company-box lsp-origami guix company-posframe forge bbdb lsp-java cython-mode mu4e-alert ess xterm-color yaml-mode markdown-toc racket-mode ninja-mode cuda-mode rtags lsp-ui company-lsp foci-org-dashboard nix-mode slime mu4e-jump-to-list sr-speedbar debbugs flycheck-haskell sage-shell-mode meson-mode csv-mode slime-company cargo ox-gfm flycheck-rust cmake-ide rainbow-mode opencl-mode window-purpose helm-xref helm-gtags header2 geiser toc-org vala-mode toml-mode rust-mode font-lock-studio langtool org-bullets projectile aggressive-indent disaster markdown-mode vline cmake-mode go-mode all-the-icons-dired font-lock+ powerline atom-dark-theme company-go highlight-symbol company-quickhelp company-auctex auctex company-c-headers srefactor company-php company-shell fill-column-indicator ecb elpy flycheck company))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; set font for emoji
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))
 ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
 )

(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
(if (file-remote-p default-directory)
    (setq vc-handled-backends 'nil))
;; (toggle-debug-on-error)
;; (toggle-debug-on-quit)

(add-to-list 'load-path "~/.emacs.d/global-settings/")
(add-to-list 'load-path "~/.emacs.d/langs/")

(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(add-hook 'after-init-hook
	  #'(lambda ()
	      (message "Loading time: %s."
		       (float-time
			(time-subtract after-init-time before-init-time)))))

(let ((packages-autoload '(trivialfis/pop-frame
			   trivialfis/close-frame
			   trivialfis/goto-pos
			   trivialfis/local-set-keys
			   trivialfis/nav-indent-shift-right
			   trivialfis/nav-indent-shift-left
			   trivialfis/screenshot
			   trivialfis/vterm
			   trivialfis/remove-blank-lines
			   trivialfis/swap-windows-horizontal)))
  (dolist (x packages-autoload)
    (autoload x "misc-trivialfis" :interactive t)))

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
(setq-default fill-column 90)	 ;can be modified by `set-fill-column'

(show-paren-mode 1)			;Show matching paras
(electric-pair-mode 1)

;; Paradox
;; The following commands don't work, use paradox instead.
;; package-list-packages
;; package-list-packages-no-fetch
;; package-install
(let ((packages-autoload '(paradox-list-packages
			   paradox-upgrade-packages)))
  (dolist (x packages-autoload)
    (autoload x "paradox-trivialfis" :interactive t)))

(require 'powerline)
(powerline-center-theme)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(require 'winum)
(setq winum-auto-setup-mode-line nil
      winum-scope 'frame-local)
(define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
(define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
(define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
(define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
(define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
(define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
(set-face-attribute 'winum-face nil :weight 'bold)
(winum-mode)

;; FIXME: If the first time invoke of helm is via xref-helm, these functions won't be loaded.
(use-package helm-trivialfis
  :defer t
  :commands (trivialfis/replace-completion-region
	     trivialfis/replace-completing-read
	     trivialfis/replace-read-file))
(let ((helm-commands '(helm-M-x
		       helm-find-files
		       helm-buffers-list
		       helm-occur)))
  (dolist (x helm-commands)
    (autoload x "helm-trivialfis" :interactive t)))

;; These functions will be removed in helm-trivialfis
(add-function :override completing-read-function
	      #'trivialfis/replace-completing-read)
(add-function :override read-file-name-function
	      #'trivialfis/replace-read-file)
(add-function :override completion-in-region-function
	      #'trivialfis/replace-completion-region)

;; Fill indicator mode
(autoload 'fci-mode "fill-col")


;; highlight current line
(global-hl-line-mode 1)
(set-face-foreground 'highlight nil) ; keep the syntax highlighting


;; Highlight symbol at point
;; (require 'highlight-symbol)
;; (setq highlight-symbol-idle-delay 0)
;; (add-hook 'after-change-major-mode-hook 'highlight-symbol-mode)


;; Auto insert and update header info
;; (autoload 'auto-make-header "header2")
;; (autoload 'auto-update-file-header "header2")
;; (add-hook 'write-file-hooks 'auto-update-file-header)

;; company mode
(require 'company-dabbrev)
(setq company-dabbrev-downcase 0
      company-async-timeout 10
      company-idle-delay 0)
(add-hook 'after-init-hook 'global-company-mode)


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
   ("C-c o"        .      previous-multiframe-window)
   ("C-c C-w"      .      (lambda ()
			    (interactive)
			    (if (region-active-p)
				(delete-region (region-beginning)
					       (region-end))
			      (delete-char 1))))
   ("C-x C-g"      .        (lambda ()(interactive))) ; regret
   ("C-c C-g"      .        (lambda ()(interactive))) ; regret
   ("M-<left>"   .  trivialfis/nav-indent-shift-left)
   ("M-<right>"  . trivialfis/nav-indent-shift-right)
   ("C-x g"        .                    magit-status)

   ;; Paradox
   ("C-c p l"      .           paradox-list-packages)
   ("C-c C-u"               paradox-upgrade-packages)

   ;; Highlight symbols
   ("M-n"          .           highlight-symbol-next)
   ("M-p"          .           highlight-symbol-prev)

   ;; Helm
   ("M-x"          .                        helm-M-x)
   ("C-x b"        .               helm-buffers-list)
   ("C-x C-f"      .                 helm-find-files)
   ("M-s o"        .                      helm-occur)

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

;; Scheme
(autoload 'trivialfis/scheme "scheme-trivialfis")
(add-hook 'scheme-mode-hook 'trivialfis/scheme)

;; LaTeX mode
(autoload 'trivialfis/LaTeX "LaTeX-trivialfis")
(add-hook 'LaTeX-mode-hook 'trivialfis/LaTeX)

;; C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(autoload 'trivialfis/c++ "cc-trivialfis")
(add-hook 'c++-mode-hook 'trivialfis/c++)

;; Cuda mode
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode))
(autoload 'trivialfis/cuda "cuda-trivialfis")
(add-hook 'cuda-mode-hook 'trivialfis/cuda)

;; OpenCL mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode))
(add-to-list 'auto-mode-alist '("\\.clh\\'" . opencl-mode))
(add-hook 'opencl-mode-hook 'trivialfis/c)

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

;; Go mode
(autoload 'trivialfis/go "go-trivialfis")
(add-hook 'go-mode-hook 'trivialfis/go)

;; php mode
(autoload 'trivialfis/php "php-trivialfis")
(add-hook 'php-mode-hook 'trivialfis/php)

;; bash mode
(autoload 'trivialfis/bash "bash-trivialfis")
(add-hook 'sh-mode-hook 'trivialfis/bash)

;; java mode
(autoload 'trivialfis/java "java-trivialfis")
(add-hook 'java-mode-hook 'trivialfis/java)

;; julia mode
(autoload 'trivialfis/julia "julia-trivialfis")
(add-hook 'julia-mode-hook 'trivialfis/julia)

(autoload 'trivialfis/R "R-trivialfis")
(add-hook 'ess-r-mode-hook 'trivialfis/R)

;; scala mode
(autoload 'trivialfis/scala "scala-trivialfis")
(add-hook 'scala-mode-hook 'trivialfis/scala)

;; makefile mode
(autoload 'trivialfis/makefile "makefile-trivialfis")
(add-hook 'makefile-gmake-mode-hook 'trivialfis/makefile)

;; groovy mode
(autoload 'trivialfis/groovy "groovy-trivialfis")
(add-hook 'groovy-mode-hook 'trivialfis/groovy)

;; prolog mode
(autoload 'trivialfis/prolog "prolog-trivialfis")
(add-hook 'prolog-mode-hook 'trivialfis/prolog)

;; cmake mode
(autoload 'trivialfis/cmake "cmake-trivialfis")
(add-hook 'cmake-mode-hook 'trivialfis/cmake)

;; xml-mode
(add-hook 'nxml-mode-hook
	  #'(lambda ()
	      (message "Loading")
	      (autoload 'trivialfis/xml-format-region "xml-trivialfis"
		:interactive t)))

(autoload 'mlir-mode "mlir-mode")
(add-to-list 'auto-mode-alist '("\\.mlir\\'" . mlir-mode))

(autoload 'tla-pcal-mode "tla-pcal-mode")
(add-to-list 'auto-mode-alist '("\\.tla\\'" . tla-pcal-mode))

(autoload 'tablegen-mode "tablegen-mode")
(add-to-list 'auto-mode-alist '("\\.td\\'" . tablegen-mode))

(autoload 'trivialfis/sml "sml-trivialfis")
(add-hook 'sml-mode 'trivialfis/sml)

(autoload 'trivialfis/haskell "haskell-trivialfis")
(add-hook 'haskell-mode-hook 'trivialfis/haskell)

(autoload 'trivialfis/gnus "gnus-trivialfis" :interactive t)
(autoload 'trivialfis/mu4e "mu4e-trivialfis" :interactive t)

(autoload 'trivialfis/markdown "markdown-trivialfis")
(add-hook 'markdown-mode-hook 'trivialfis/markdown)

(add-hook 'magit-diff-mode-hook
	  #'(lambda ()
	      (setq magit-diff-refine-hunk 'all
		    magit-diff-refine-ignore-whitespace 'nil)))
(add-hook 'magit-status-mode-hook
	  #'(lambda ()
	      (transient-append-suffix 'magit-log "-A"
		'("-m" "No merges" "--no-merges"))))


;; End language settings------------------------------------------------------------------------

(eval-when-compile
  (with-temp-buffer
    (insert (format ";;; Bootstrap
;;; This file is generated by init.el at compile time.
;;;
;;; Copyright © 2015-2019 Fis Trivial <jm.yuan@outlook.com>
;;;
;;; This file is part of Foci-Emacs.
;;;
;;; Foci-Emacs is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Foci-Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Foci-Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;

(load-file \"~/.emacs.d/global-settings/archives-trivialfis.el\")
(setq package-selected-packages
      '%s)
(package-install-selected-packages)

(load-file \"~/.emacs.d/init.el\")\n" package-selected-packages))

    (add-file-local-variable 'flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp))
    (write-file "selected-packages.el" nil))
  (let ((tempf "~/.emacs.d/selected-packages.el~"))
    (when (file-exists-p tempf)
      (delete-file tempf))))

(provide 'init.el)
;;; init.el ends here
