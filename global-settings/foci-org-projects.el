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

(defun goto-list(addr)
  "Show mailing list ADDR."
  (trivialfis/mu4e-config)
  (define-key mu4e-headers-mode-map
    (kbd "q")
    (project-come-back (buffer-name)))
  (mu4e-headers-search (string-join `("list:" ,addr))))

(defun insert-list ()
  "Insert mailing list at current line.
Prompt for List-ID and list name"
  (interactive)
  (let ((addr (read-string "List ID:"))
	(name (read-string "List name:")))
    (org-indent-line)
    (insert (string-join
	     `("+ [[elisp:(progn (require 'foci-org-projects)(goto-list\""
	       ,addr "\"))][" ,name "]]")))))

(defun insert-project ()
  "Insert a project template."
  (interactive)
  (insert %project-template))

(defun foci-project-turn-on ()
  "Turn foci-project-mode on."
  (trivialfis/mu4e-config)
  (mu4e-update-mail-and-index nil))

(defun foci-project-turn-off ()
  "Turn foci-project-mode off.")

(define-minor-mode foci-project-mode
  "Minor mode for handling ANSI 24bit color sequences"
  :init-value nil
  :lighter "FP"
  (if foci-project-mode
      (foci-project-turn-on)
    (foci-project-turn-off)))

(provide 'foci-org-projects)
;;; foci-org-projects.el ends here
