;;; archives-trivialfis.el --- Record package archives URLs
;;; Commentary:
;;;
;;; Copyright © 2016-2019 Fis Trivial <jm.yuan@outlook.com>
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

(defvar elpa-official
  '(
    ("melpa"     . "https://melpa.org/packages/")
    ("gnu"       . "https://elpa.gnu.org/packages/")
    ))
(defvar elpa-china
  '(
    ("melpa"     . "https://elpa.emacs-china.org/melpa/")
    ("gnu"       . "https://elpa.emacs-china.org/org/")
    ))
(defvar elpa-tuna
  '(
    ("melpa"     .   "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("gnu"       .   "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ))
(setq-default package-archives elpa-official)

(provide 'archives-trivialfis)
;;; archives-trivialfis.el ends here
