;;; trivial-ansi.el --- Basic ANSI 24 bit color viewing.
;;; 
;;; Copyright © 2015-2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; All rights reserved
;;; 
;;; This file is part of Trimacs.
;;; 
;;; Trimacs is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; Trimacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Trimacs.  If not, see <http://www.gnu.org/licenses/>.
;;;

;; Version: 0.1
;; Author: ybbs.daans@hotmail.com
;; Keywords: ANSI

;;; Commentary:
;;
;; This package provides a minor mode to view 24 bit ANSI color text.
;; 
;; Usage:
;;
;; M-x RET trivial-ansi-mode RET
;;
;; Then the color codes should be translated into proper text faces
;; automatically.
;;

;;; License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Code:

(defun trivial-ansi-get-rgb (code)
  "Retrun RGB string from CODE.
CODE is an ANSI color escape sequence."
  (save-match-data
    (let* ((start (+ 2 (string-match "2;" code)))
	   (end (string-match "m" code))
	   (rgb-dec (split-string (substring code start end) ";"))
	   (rgb-hex-str ""))
      (dolist (val rgb-dec)
	(setq rgb-hex-str
	      (concat rgb-hex-str
		      (let ((hex (format "%x" (string-to-number val))))
			(if (< (string-width hex) 2)
			    (concat "0" hex)
			  hex)))))
      (concat "#" rgb-hex-str))))


(defun trivial-ansi-hide-prefix ()
  "Hide the color sequences."
  (let ((beg (match-beginning 1))
	(end (match-end 1)))
    (save-match-data
      (put-text-property beg end 'invisible t)
      (put-text-property beg end 'intangible t))))

(defun trivial-ansi-hide-reset ()
  "Hide the reset sequence."
  (let ((beg (match-beginning 4))
	(end (match-end 4)))
    (save-match-data
      (put-text-property beg end 'invisible t)
      (put-text-property beg end 'intangible t))))


(defun trivial-ansi-colorize-content ()
  "Colorize text around color sequences and reset sequence."
  (save-match-data
    (let* ((code (split-string
		  (match-string-no-properties 1)
		  "" t))
	   result)
      (dolist (c code)
	(let ((color (trivial-ansi-get-rgb c)))
	  (if (string-equal
	       (car (split-string c ";"))
	       "[38")
	      (setq result (append `(:foreground ,color) result))
	    (setq result (append `(:background ,color) result)))))
      result)))


(defvar trivial-ansi-keyword
  '(("\\(\\(\\[[34]8;2;[0-9;]+[0-9]m\\)+\\)\\(.*?\\)\\(\\[0m\\)"
     . ((1 (trivial-ansi-hide-prefix) t)
	(3 (trivial-ansi-colorize-content) t)
	(4 (trivial-ansi-hide-reset) t))))
  "Keyword for font lock mode.")
(make-variable-buffer-local 'trivial-ansi-keyword)

(defun trivial-ansi-turn-on ()
  "Turn on trivial-ansi-mode."
  (add-to-list 'font-lock-extra-managed-props 'invisible)
  (add-to-list 'font-lock-extra-managed-props 'intangible)
  (font-lock-add-keywords nil
			  trivial-ansi-keyword
			  t))

(defun trivial-ansi-turn-off ()
  "Turn off trivial-ansi-mode."
  (font-lock-remove-keywords nil
			     trivial-ansi-keyword))

;;;###autoload
(define-minor-mode trivial-ansi-mode
  "Minor mode for handling ANSI 24bit color sequences"
  :init-value nil
  :lighter "ANSI"
  (if trivial-ansi-mode
      (trivial-ansi-turn-on)
    (trivial-ansi-turn-off)))

(provide 'trivial-ansi)
;;; trivial-ansi.el ends here
