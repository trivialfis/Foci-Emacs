;;; llm-trivialfis.el --- llm related functions.
;;;
;;; Copyright © 2025 Jiamingy <jm.yuan@outlook.com>
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

(use-package gptel
  :defer t
  :autoload gptel-make-openai
  :commands (gptel))
(use-package f
  :defer t
  :commands f-join)

(defun trivialfis/shell-to-string (command)
  "Execute COMMAND and return its output as string."
  (with-temp-buffer
    (let ((status (shell-command command (current-buffer))))
      (if (eq status 1)
	  (string-trim (buffer-string))
	nil))))

(provide 'llm-trivialfis)
;;; llm-trivialfis.el ends here
