;;; archives-trivialfis.el --- Record package archives URL
;;; Commentary:
;;;
;;; Copyright © 2016-2018 Fis Trivial <ybbs.daans@hotmail.com>
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

;; "https://elpa.emacs-china.org/melpa/"
(defvar melpa-repo-url "https://melpa.org/packages/")

(setq package-archives `(
			 ;; tsinghua's mirror
			 ;; ("melpa"     .   "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ;; ("org"       .   "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
			 ;; ("marmalade" .   "https://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")
			 ;; ("gnu"       .   "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")

			 ;; Emacs China's mirror
			 ("melpa"     . ,melpa-repo-url)
			 ("org"       . "https://elpa.emacs-china.org/org/")
			 ("gnu"       . "https://elpa.emacs-china.org/gnu/")
			 ("marmalade" . "https://elpa.emacs-china.org/marmalade/")
			 ))

(provide 'archives-trivialfis)
;;; archives-trivialfis.el ends here
