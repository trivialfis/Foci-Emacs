;;; fpg-mode.el --- Use org-mode to manage projects -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
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

(require 'mail-trivialfis)
(require 'subr-x)
(require 'map)
(require 'mu4e)
(require 'org)

(defvar *project-template*
  '("* "
    "** Home Page: \n"
    "** Mailing List:\n\n"
    "** Synopsis:\n\n"
    "** Description:\n\n"
    "** Source dir: "))

;; It's really hard to find suitable keys with org-mode around.
(defvar fpg-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c f u") 'fpg-update-unread-at-line)
    (define-key map (kbd "C-c f U") 'fpg-update-lists)
    (define-key map (kbd "C-c f i q") 'fpg-insert-list)
    (define-key map (kbd "C-c f i i") 'fpg-insert-list-id)
    (define-key map (kbd "C-c f i m") 'fpg-insert-message-id)
    map))

(easy-menu-define fpg-mode-menu fpg-mode-map
  "Menu for FPG mode"
  '("FPG"
    "---"
    ["Update list at point" fpg-update-unread-at-line]
    ["Update all lists" fpg-update-lists]
    "---"
    ["Insert query" fpg-insert-list]
    ["Insert list id" fpg-insert-list-id]
    ["Insert message id" fpg-insert-message-id]
    ))

(defvar *mu4e-search* "mu4e-headers-search"
  "Mu4e search command.")

(defun fpg-come-back (buffer-name)
  "Come back to BUFFER-NAME."
  (lambda ()
    (interactive)
    (switch-to-buffer buffer-name)
    (fpg-update-unread-at-line)
    (if (buffer-file-name)
	(save-buffer))))

(defun fpg-update-lists-iter ()
  "Internal function for updating lists."
  (let ((cur-point (point))
	(next-point (prog2
			(org-next-link)
			(point))))
    (if (= cur-point next-point)
	nil
      (let ((search-succeed-p (search-forward
			       (string-join `("[[elisp:(" ,*mu4e-search*))
			       (line-end-position) t)))
	(when search-succeed-p
	  (fpg-update-unread-at-line))
	t))))

(defun fpg-update-lists ()
  "Upgrade all mailing lists."
  (interactive)
  (let ((point (point)))
    (goto-char (point-min))
    (cl-loop
     while (fpg-update-lists-iter))
    (goto-char point)))

(defun fpg-get-query-at-line ()
  "Get the query string at current line."
  (beginning-of-line)
  (let ((query-start (search-forward "\""))
	(query-end (progn
		     (end-of-line)
		     (search-backward "\""))))
    (buffer-substring-no-properties query-start query-end)))

(defun fpg-update-unread-at-line()
  "Update the numbers of unread mails with point at current line."
  (interactive)
  (let* ((query-command (fpg-get-query-at-line))
	 (query-unread (string-join
			`("mu find "
			  ,query-command
			  " AND flag:unread | wc -l")))
	 (count (string-trim
		 (shell-command-to-string query-unread)))
	 (no-matches (string-prefix-p "mu: no matches" count)))
    (search-forward "Unread")
    (search-forward-regexp "\([0-9]*\)")
    (if no-matches
	(replace-match "(0)")
      (replace-match (string-join `("(" ,count ")"))))))

(defun fpg-goto-unread ()
  "Go to unread messages view."
  (trivialfis/mu4e-config)
  (let ((query-command (fpg-get-query-at-line)))
    (mu4e-headers-search (string-join `(,query-command
					" AND flag:unread ")))))

(defun fpg-construct-query-link (query desc)
  "Construct link for a mailing list.
QUERY: The query string for mu4e,
DESC: Mailing list description."
  (let* ((list-link (string-join
		     `("+ [[elisp:(" ,*mu4e-search* " \"" ,query "\")]["
		       ,desc "]]")))
	 (unread "Unread(0)")
	 (unread-link (string-join
		       `("[[elisp:(fpg-goto-unread)][" ,unread "]]")))
	 (link-width (string-width desc))
	 (unread-width (string-width unread))
	 (blank-num (- 75 link-width unread-width))
	 (blanks (make-string blank-num ?\xB7)))
    (string-join `("   " ,list-link ,blanks ,unread-link))))

(defun fpg-insert-handle-line-beginning ()
  "Insert new line when needed."
  (when (not (= (line-beginning-position) (point)))
    (end-of-line)
    (insert "\n")))

(defun fpg-insert-list ()
  "Insert mailing list at current line."
  (interactive)
  (let ((query (read-string "Query: "))
	(desc (read-string "List Description: ")))
    (fpg-insert-handle-line-beginning)
    (insert (fpg-construct-query-link query desc))))

(defun fpg-insert-list-id ()
  "Insert mailing list at current line.
Prompt for List-ID and list name"
  (interactive)
  (let ((addr (read-string "List ID: "))
	(name (read-string "List Description: ")))
    (fpg-insert-handle-line-beginning)
    (insert (fpg-construct-query-link (string-join `("list:" ,addr)) name))))

(defun fpg-insert-message-id ()
  "Insert message id for mailing list."
  (interactive)
  (let ((addr (read-string "Message Id: "))
	(name (read-string "List Description: ")))
    (fpg-insert-handle-line-beginning)
    (insert (fpg-construct-query-link (string-join `("msgid:" ,addr)) name))))

(defun fpg-insert-project ()
  "Insert a project template."
  (interactive)
  (let ((name (read-string "Project name: ")))
    (insert (string-join `(,(car *project-template*) ,name "\n"))))
  (cl-mapcar 'insert (cdr *project-template*)))

(defun fpg-turn-on ()
  "Turn fpg-mode on."
  (message "Turn fpg on!")
  (trivialfis/mu4e-config)
  (define-key mu4e-headers-mode-map
    (kbd "q")
    (fpg-come-back (buffer-name)))
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (load-library "mu4e")
  (add-hook 'mu4e-index-updated-hook 'fpg-update-lists)
  (flyspell-mode 0))

(defun fpg-turn-off ()
  "Turn fpg-mode off."
  (message "Turn fpg off!")
  (add-to-list 'org-link-frame-setup '(file . find-file-other-window))
  (remove-hook 'mu4e-index-updated-hook 'fpg-update-lists)
  (flyspell-mode 1))

;;;###autoload
(define-minor-mode fpg-mode
  "Use org mode and mu4e for managing projects. (Foci playground)"
  :init-value nil
  :lighter "Fpg"
  :keymap fpg-mode-map
  (if fpg-mode
      (fpg-turn-on)
    (fpg-turn-off)))

(provide 'fpg-mode)
;;; fpg-mode.el ends here
