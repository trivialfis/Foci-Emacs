;;; mu4e-trivialfis.el --- Summary -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Copyright © 2015-2019 Fis Trivial <ybbs.daans@hotmail.com>
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

(require 'smtpmail)
(require 'smtpmail-async)

(require 'text-trivialfis)

(eval-when-compile
  (require 'highlight-symbol)
  (add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp"))
(add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp")
(require 'mu4e)

(defconst mu4e-default-bookmarks mu4e-bookmarks)

(defun trivialfis/use-param (default optional)
  "Return OPTIONAL if it isn't nil, otherwise return DEFAULT."
  (if optional
      optional
    default))

(defun trivialfis/configure-smtp-server (address &optional server)
  "Try to deduce smtp server address from ADDRESS unless SERVER is not nil."
  (if server
      server
    (cond
     ((string-match-p "outlook.com" address)
      "smtp.office365.com")
     ((string-match-p "hotmail.com" address)
      "smtp-mail.outlook.com")
     ((string-match-p "gmail.com" address)
      "smtp.gmail.com")
     (t nil))))


(defun trivialfis/mu4e-context (name address fullname
				     &optional
				     signature server sent
				     draft trash refile)
  "Create a single mu4e context.
NAME: Account name.
ADDRESS: Account address.
FULLNAME: The one that displayed on title,
SIGNATURE: Mail Signature.

&optional parameters:
SERVER: Remote server address;
SENT:   Directory storing sent mails;
DRAFT:  ~ storing draft mails;
TRASH:  ~ storing deleted mails;
REFILE: ~ storing archived mails."
  (make-mu4e-context
   :name name
   :enter-func
   (lambda ()
     (mu4e-message (concat "Entering " name " context"))
     (add-to-list
      'mu4e-bookmarks
      `(,(concat "(to:" address
		 " or cc:" address
		 ") and not (flag:trashed)")
	"@me" ?a))
     (add-to-list
      'mu4e-bookmarks
      `(,(concat "(to:" address
		 ") and (not flag:trashed)")
	"2me" ?m)))
   :leave-func
   (lambda ()
     (mu4e-message (concat "Leaving " name " context"))
     (setq mu4e-bookmarks mu4e-default-bookmarks))
   ;; we match based on the contact-fields of the message
   :match-func (lambda (msg)
                 (when msg
                   (string-match-p (concat "~/" name) (mu4e-message-field msg :maildir))))
   :vars `( ( user-mail-address      . ,address  )
            ( user-full-name         . ,fullname )
            ( mu4e-compose-signature . ,(trivialfis/use-param
					 "" signature))

	    ( message-send-mail-function . async-smtpmail-send-it )
	    ( send-mail-function . async-smtpmail-send-it )
	    ( smtpmail-smtp-server
	      . ,(trivialfis/configure-smtp-server address server))
	    ( smtpmail-default-smtp-server
	      . ,(trivialfis/configure-smtp-server address server) )
	    ( smtpmail-smtp-service . 587 )
	    ( smtpmail-stream-type  . starttls )
	    ( smtpmail-debug-info   . t )

	    ( mu4e-maildir       . ,(concat "~/.Mail/" name) )
	    ( mu4e-sent-folder   . ,(trivialfis/use-param "/Sent"    sent)  )
	    ( mu4e-drafts-folder . ,(trivialfis/use-param "/Drafts"  draft) )
	    ( mu4e-trash-folder  . ,(trivialfis/use-param "/Deleted" trash) )
	    ( mu4e-refile-folder . ,(trivialfis/use-param "/Archive" refile))

	    (mu4e-get-mail-command . ,(concat "offlineimap -a " name)))))

(defun trivialfis/mu4e-contexts ()
  "Configuration for contexts.
This function tries to read a file with content like this:

'((name-0 address-0 fullname-0)
  (name-1 address-1 fullname-1 signature-1) ...)

Where each sublist contains the arguments for `trivialfis/mu4e-context'."
  (let* ((account-file "~/.emacs.d/misc/mail-accounts.el")
	 (account-string (if (file-exists-p account-file)
			     (with-temp-buffer
			       (insert-file-contents account-file)
			       (buffer-string))
			   (prog2
			       (message "Failed to find the account file.")
			       nil)))
	 (evaluated (if account-string
			(eval (car (read-from-string account-string)))
		      nil))
	 (contexts
	  (cl-mapcar #'(lambda (lst)
			 (apply 'trivialfis/mu4e-context lst))
		     evaluated)))
    (setq mu4e-contexts contexts)
    (setq mu4e-context-policy 'pick-first
	  mu4e-compose-context-policy 'pick-first)))

(defun trivialfis/mu4e-config ()
  "Mu4e mail configuration."

  (trivialfis/mu4e-contexts)
  (let ((dir "~/Downloads/"))
    (when (file-directory-p dir)
      (setq mu4e-attachment-dir dir)))

  (setq mu4e-headers-fields '((:human-date . 12)
			      (:flags . 6)
			      (:mailing-list . 10)
			      (:from . 22)
			      (:message-id . 30)
			      (:subject . nil))
	mu4e-headers-auto-update      t
	mu4e-headers-skip-duplicates  t
	mu4e-use-fancy-chars          t
	mu4e-completing-read-function 'completing-read
	mu4e-headers-include-related  t
	message-kill-buffer-on-exit   t
	mu4e-view-show-images         t
	mu4e-sent-messages-behavior   'delete) ; handled by outlook or gmail.

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (add-hook 'mu4e-compose-mode-hook 'trivialfis/_text))

(defun trivialfis/mu4e ()
  "Call mu4e."
  (interactive)
  (trivialfis/mu4e-config)
  (mu4e)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'notifications)
  (run-with-timer 1 300 #'(lambda ()
			    (unless
				(or (and (buffer-live-p mu4e~update-buffer)
					 (process-live-p
					  (get-buffer-process mu4e~update-buffer)))
				    (equal major-mode 'mu4e-compose-mode))
			      (mu4e-update-mail-and-index t)))))

(provide 'mu4e-trivialfis)
;;; mu4e-trivialfis.el ends here
