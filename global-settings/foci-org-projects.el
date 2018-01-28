;;; foci-org-projects.el --- Use org-mode to manage projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'mail-trivialfis)
(require 'subr-x)
(require 'map)
(require 'mu4e)
(require 'org)

(defvar *project-template*
  '("* " "** Home Page: \n" "** Mailing List:\n\n"
    "** Synopsis:\n\n" "** Description:\n\n" "** Source dir: "))

(defvar %mu4e-search "mu4e-headers-search")

(defun project-come-back (buffer-name)
  "Come back to BUFFER-NAME."
  (lambda ()
    (interactive)
    (switch-to-buffer buffer-name)))

(defun goto-list(addr &optional use-msgid)
  "Show mailing list ADDR.
If USE-MSGID is t, then use message-id rather than list-id."
  (if use-msgid
      (mu4e-headers-search (string-join `("msgid:" ,addr)))
    (mu4e-headers-search (string-join `("list:" ,addr)))))

(defun get-query-at-line ()
  "Get the query string at current line."
  (beginning-of-line)
  (let ((query-start (search-forward "\""))
	(query-end (progn
		     (end-of-line)
		     (search-backward "\""))))
    (buffer-substring-no-properties query-start query-end)))

(defun update-unread-at-line()
  "Update the numbers of unread mails with point at current line."
  (interactive)
  (let* ((query-command (get-query-at-line))
	 (count (string-trim
		 (shell-command-to-string (string-join
					   `("mu find flag:unread AND "
					     ,query-command
					     " | wc -l")))))
	 (no-matches (string-prefix-p "mu: no matches" count)))
    (search-forward "Unread")
    (search-forward-regexp "\([0-9]*\)")
    (message (match-string 0))
    (if no-matches
	(replace-match "(0)")
      (replace-match (string-join `("(" ,count ")"))))))

(defun goto-unread ()
  "Go to unread messages view."
  (trivialfis/mu4e-config)
  (let ((query-command (get-query-at-line)))
    (print query-command)
    (mu4e-headers-search (string-join `("flag:unread AND "
					,query-command)))))

(defun construct-query-link (query desc)
  "Construct link for a mailing list.
QUERY: The query string for mu4e,
DESC: Mailing list description."
  (let* ((list-link (string-join
		     `("+ [[elisp:(" ,%mu4e-search " \"" ,query "\")]["
		       ,desc "]]")))
	 (unread "Unread(0)")
	 (unread-link (string-join `("[[elisp:(goto-unread)][" ,unread "]]")))
	 (link-width (string-width desc))
	 (unread-width (string-width unread))
	 (blank-num (- 75 link-width unread-width))
	 (blanks (make-string blank-num ?\xB7)))
    (print blanks)
    (string-join `("   " ,list-link ,blanks ,unread-link))))

(defun insert-list ()
  "Insert mailing list at current line."
  (interactive)
  (let ((query (read-string "Query: "))
	(desc (read-string "List Description: ")))
    (when (not (= (line-beginning-position) (point)))
      (end-of-line)
      (insert "\n"))
    (insert (construct-query-link query desc))))

(defun insert-list-id ()
  "Insert mailing list at current line.
Prompt for List-ID and list name"
  (interactive)
  (let ((addr (read-string "List ID:"))
	(name (read-string "List Description:")))
    (org-indent-line)
    (insert (construct-query-link (string-join `("list:" ,addr)) name))))

(defun insert-message-id ()
  "Insert message id for mailing list."
  (interactive)
  (let ((addr (read-string "Message Id:"))
	(name (read-string "List Description:")))
    (org-indent-line)
    (insert (construct-query-link (string-join `("msgid:" ,addr)) name))))

(defun insert-project ()
  "Insert a project template."
  (interactive)
  (let ((name (read-string "Project name: ")))
    (insert (string-join `(,(car *project-template*) ,name "\n"))))
  (cl-mapcar 'insert (cdr *project-template*)))

(defun foci-update-mails ()
  "Call mu4e for updating mails."
  (interactive)
  (mu4e-update-mail-and-index nil))

(defun foci-project-turn-on ()
  "Turn foci-project-mode on."
  (trivialfis/mu4e-config)
  (define-key mu4e-headers-mode-map
    (kbd "q")
    (project-come-back (buffer-name)))
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (flyspell-mode 0))

(defun foci-project-turn-off ()
  "Turn foci-project-mode off."
  (add-to-list 'org-link-frame-setup '(file . find-file-other-window))
  (flyspell-mode 1))

(define-minor-mode foci-project-mode
  "Minor mode for handling ANSI 24bit color sequences"
  :init-value nil
  :lighter "FP"
  (if foci-project-mode
      (foci-project-turn-on)
    (foci-project-turn-off)))

(provide 'foci-org-projects)
;;; foci-org-projects.el ends here
