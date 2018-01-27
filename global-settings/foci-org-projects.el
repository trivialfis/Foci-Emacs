;;; foci-org-projects.el --- Use org-mode to manage projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'mail-trivialfis)
(require 'subr-x)
(require 'mu4e)
(require 'org)

(defvar %project-template
  "
*
** Home Page:
** Mailing Lists:

** Synopsis:
** Description:

** Source dir:
")

(defun project-come-back (buffer-name)
  "Come back to BUFFER-NAME."
  (lambda ()
    (interactive)
    (switch-to-buffer buffer-name)))

(defun goto-list(addr &optional use-msgid)
  "Show mailing list ADDR.
If USE-MSGID is t, then use message-id rather than list-id."
  (trivialfis/mu4e-config)
  (define-key mu4e-headers-mode-map
    (kbd "q")
    (project-come-back (buffer-name)))
  (if use-msgid
      (mu4e-headers-search (string-join `("msgid:" ,addr)))
    (mu4e-headers-search (string-join `("list:" ,addr)))))

(defun insert-list-id ()
  "Insert mailing list at current line.
Prompt for List-ID and list name"
  (interactive)
  (let ((addr (read-string "List ID:"))
	(name (read-string "List name:")))
    (org-indent-line)
    (insert (string-join
	     `("+ [[elisp:(progn (goto-list \""
	       ,addr "\"))][" ,name "]]")))))

(defun insert-message-id ()
  "Insert message id for mailing list."
  (interactive)
  (let ((addr (read-string "Message Id:"))
	(name (read-string "List name:")))
    (org-indent-line)
    (insert (string-join
	     `("+ [[elisp:(progn (goto-list \""
	       ,addr "\" t))][" ,name "]]")))))

(defun insert-project ()
  "Insert a project template."
  (interactive)
  (insert %project-template))

(defun foci-update-mails ()
  "Call mu4e for updating mails."
  (interactive)
  (mu4e-update-mail-and-index nil))

(defun foci-project-turn-on ()
  "Turn foci-project-mode on."
  (trivialfis/mu4e-config)
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
