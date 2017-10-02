;;; archives-trivialfis.el --- Record package archives URL
;;; Commentary:
;;; Code:

(setq package-archives '(
			 ;; tsinghua's mirror
			 ;; ("melpa"     .   "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ;; ("org"       .   "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
			 ;; ("marmalade" .   "https://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")
			 ;; ("gnu"       .   "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")

			 ;; Emacs China's mirror
			 ("melpa"     .    "https://elpa.emacs-china.org/melpa/")
			 ("org"       . "https://elpa.emacs-china.org/org/")
			 ("gnu"       . "https://elpa.emacs-china.org/gnu/")
			 ("marmalade" . "https://elpa.emacs-china.org/marmalade/")
			 ))

(provide 'archives-trivialfis)
;;; archives-trivialfis.el ends here
