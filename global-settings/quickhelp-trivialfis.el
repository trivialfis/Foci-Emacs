;;; Package --- Summary
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
;;; Code:

(require 'company-quickhelp)

(defun trivialfis/company-quickhelp()
  "Configuration for company quickhelp."
  (setq
   company-quickhelp-color-background "#4d4d4d"
   company-quickhelp-delay 1
   company-quickhelp-max-lines nil)

  (company-quickhelp-mode 1))

(provide 'quickhelp-trivialfis.el)
;;; quickhelp-trivialfis ends here
