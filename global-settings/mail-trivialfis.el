;;; mail-trivialfis.el --- Summary -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Copyright © 2015-2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2012-2018 Sylvain Benner & Contributors
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

(require 'gnus)
(require 'gnus-art)
(require 'nnml)
(require 'nnimap)
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
		 ") and not flag:trashed")
	"@me" ?a))
     (add-to-list
      'mu4e-bookmarks
      `(,(concat "to:" address
		 " and not flag:trashed")
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
	    ( smtpmail-smtp-server .         ,(trivialfis/use-param
					       "smtp-mail.outlook.com" server) )
	    ( smtpmail-default-smtp-server . ,(trivialfis/use-param
					       "smtp-mail.outlook.com" server) )
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
    (setq mu4e-contexts contexts)))

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
	mu4e-view-show-images         t)

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (add-hook 'mu4e-compose-mode-hook 'trivialfis/_text))

(defun trivialfis/mu4e ()
  "Call mu4e."
  (interactive)
  (trivialfis/mu4e-config)
  (mu4e))

(defun trivialfis/smtp4gnus ()
  "Configuration for sending mails."
  (setq	message-send-mail-function 'async-smtpmail-send-it
	send-mail-function 'async-smtpmail-send-it
	smtpmail-smtp-server "smtp-mail.outlook.com"
	smtpmail-default-smtp-server "smtp-mail.outlook.com"
	smtpmail-smtp-service 587
	smtpmail-stream-type 'starttls
	smtpmail-debug-info 't)
  (setq mu4e-sent-messages-behavior 'delete)) ; handled by outlook or gmail.

(defun trivialfis/gnus ()
  "Gnus news reader configuration."
  (interactive)
  (global-hl-line-mode 0)

  (setq gnus-select-method
	'(nnimap "outlook"
		 (nnimap-address "imap-mail.outlook.com")
		 (nnimap-server-port "imaps")
		 (nnimap-stream ssl)
		 (nnimap-split-methods default)
		 (nnir-search-engine imap)))

  (setq nnml-directory "~/.Mail")
  (setq message-directory "~/.Mail")

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  (setq gnus-visible-headers
        "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^List-Id:\\|^Gnus")

  (setq gnus-sorted-header-list
        '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:" "^List-id:"
          "^Subject:" "^Date:" "^Gnus"))
  (setq nnmail-split-methods
	'(("guix.devel" "List-Id:.*\n.*<guix-devel.gnu.org>")
	  ("guix.bugs" "List-Id:.*<bug-guix.gnu.org>")
	  ("guix.patches" "List-Id:.*<guix-patches.gnu.org>")
	  ("guix.help" "To:.*help-guix@gnu.org|Cc:.*help-guix@gnu.org")
	  ("pytorch.issues" "Message-ID: .*pytorch/issue\\(.*\\)")
	  ("pytorch.pr" "Message-ID: .*pytorch/pull\\(.*\\)")
	  ("Inbox" "")))

  (setq-default
   gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
   gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
   gnus-group-line-format "%M%S%p%P%5y:%B %G\n";;"%B%(%g%)"
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
   gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
   gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
   gnus-sum-thread-tree-false-root ""
   gnus-sum-thread-tree-indent " "
   gnus-sum-thread-tree-leaf-with-other "├► "
   gnus-sum-thread-tree-root ""
   gnus-sum-thread-tree-single-leaf "╰► "
   gnus-sum-thread-tree-vertical "│"
   gnus-article-browse-delete-temp t
   gnus-treat-strip-trailing-blank-lines 'last
   gnus-keep-backlog 'nil
   gnus-summary-display-arrow nil ; Don't show that annoying arrow:
   gnus-mime-display-multipart-related-as-mixed t ; Show more MIME-stuff:
   gnus-auto-select-first nil ; Don't get the first article automatically:
   smiley-style 'medium
   gnus-keep-backlog '0)

  (trivialfis/smtp4gnus)

  (gnus)
  (highlight-symbol-mode 0)
  (delq 'after-change-major-mode-hook highlight-symbol-mode))


(provide 'mail-trivialfis)
;;; mail-trivialfis.el ends here
