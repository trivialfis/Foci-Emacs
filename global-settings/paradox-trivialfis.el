;;; paradox-trivialfis --- Configuration for paradox. -*- lexical-binding: t -*-
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
;;; Commentary:
;;; code:

(require 'paradox)
(require 'paradox-github)
(require 'archives-trivialfis)

(defun trivialfis/get-token ()
  "Get token from file."
  (let ((token-path "~/.emacs.d/misc/github-token"))
    (if (file-exists-p token-path)
	(with-temp-buffer
	  (insert-file-contents token-path)
	  (buffer-string))
      nil)))

(defun trivialfis/package ()
  "Configuration for packages using paradox."
  (if (trivialfis/get-token)
      (setq  paradox-automatically-star t
	     paradox-github-token t
	     paradox-github-token (trivialfis/get-token)))
  (package-initialize))
(trivialfis/package)

(provide 'paradox-trivialfis)
;;; paradox-trivialfis.el ends here
