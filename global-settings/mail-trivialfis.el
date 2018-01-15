;;; mail-trivialfis.el --- Summary
;;; Commentary:
;;; Code:
(require 'gnus)
(require 'nnimap)
(require 'smtpmail)

(defun trivialfis/mail()
  "My mail configuration."
  (interactive)

  (global-hl-line-mode 0)

  (setq gnus-select-method
	'(nnimap "outlook"
		 (nnimap-address "imap-mail.outlook.com")
		 (nnimap-server-port "imaps")
		 (nnimap-stream ssl)))

  (setq smtpmail-smtp-server "smpt-mail.outlook.com"
	smtpmail-smtp-service 587)

  (setq nnimap-split-methods
	'(
	  ;; ("guix")
	  ;; ("guix:devel")
	  ("guix:debug" "List-Id:.*\n.*<guix-devel.gnu.org>")
	  ("guix:patch" "List-Id:.*<guix-patches.gnu.org>")
	  ("guix:help" "^To:.*help-guix@gnu.org|Cc:.*help-guix@gnu.org|List-ID:.*\n.*<guix-devel.gnu.org>")))

  ;; (eval-and-compile
  ;;   (add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp/"))
  (gnus)
  (highlight-symbol-mode 0)
  (delq 'after-change-major-mode-hook highlight-symbol-mode))

(provide 'mail-trivialfis)
;;; mail-trivialfis.el ends here
