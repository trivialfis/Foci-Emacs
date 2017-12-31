;;; mail-trivialfis.el --- Summary
;;; Commentary:
;;; Code:
(eval-and-compile
  (add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp/"))
(require 'smtpmail)
(require 'mu4e)

(defun trivialfis/mail ()
  (setq message-send-mail-function 'smtpmail-send-it
	smtpmail-starttls-credentials '(("mail.example.com" 587 nil nil))
	smtpmail-default-smtp-server "mail.example.com"
	smtpmail-smtp-server "mail.example.com"
	smtpmail-smtp-service 587
	smtpmail-debug-info t)


  (setq mu4e-maildir (expand-file-name "~/email/mbsyncmail"))

  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent Items")
  (setq mu4e-trash-folder  "/Trash")
  ;; (setq message-signature-file "~/.emacs.d/.signature") ; put your signature in this file

					; get mail
  (setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/.mbsyncrc work"
	mu4e-html2text-command "w3m -T text/html"
	mu4e-update-interval 120
	mu4e-headers-auto-update t
	mu4e-compose-signature-auto-include nil)

  (setq mu4e-maildir-shortcuts
	'( ("/INBOX"               . ?i)
           ("/Sent Items"   . ?s)
           ("/Trash"       . ?t)
           ("/Drafts"    . ?d)))

  ;; show images
  (setq mu4e-show-images t)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; general emacs mail settings; used when composing e-mail
  ;; the non-mu4e-* stuff is inherited from emacs/message-mode
  (setq mu4e-reply-to-address "me@example.com"
	user-mail-address "me@example.com"
	user-full-name  "Rob Stewart")

  ;; don't save message to Sent Messages, IMAP takes care of this
					; (setq mu4e-sent-messages-behavior 'delete)

  ;; spell check
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (set-fill-column 72)
              (flyspell-mode)))
  )

(provide 'mail-trivialfis)
;;; mail-trivialfis.el ends here
