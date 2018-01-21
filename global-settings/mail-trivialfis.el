;;; mail-trivialfis.el --- Summary
;;; Commentary:
;;; Code:
(require 'gnus)
(require 'gnus-art)
(require 'nnml)
(require 'nnimap)
(require 'smtpmail)
(eval-when-compile
  (require 'highlight-symbol))

(defun trivialfis/mail()
  "My mail configuration."
  (interactive)

  (global-hl-line-mode 0)

  (setq gnus-select-method
	'(nnimap "outlook"
		 (nnimap-address "imap-mail.outlook.com")
		 (nnimap-server-port "imaps")
		 (nnimap-stream ssl)))

  (setq nnml-directory "~/.Mail")
  (setq message-directory "~/.Mail")

  (setq smtpmail-smtp-server "smpt-mail.outlook.com"
	smtpmail-smtp-service 587
	message-send-mail-function 'smtpmail-send-it
	smtpmail-default-smtp-server "smtp-mail.outlook.com")

  (setq gnus-visible-headers
        "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^List-Id:\\|^Gnus")

  (setq gnus-sorted-header-list
        '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:" "^List-id:"
          "^Subject:" "^Date:" "^Gnus"))
  (setq nnimap-split-methods
	'(
	  ;; ("guix")
	  ;; ("guix:devel")
	  ("guix:debug" "List-Id:.*\n.*<guix-devel.gnu.org>")
	  ("guix:patch" "List-Id:.*<guix-patches.gnu.org>")
	  ("guix:help" "^To:.*help-guix@gnu.org|Cc:.*help-guix@gnu.org|List-ID:.*\n.*<guix-devel.gnu.org>")))

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

  (gnus)
  (highlight-symbol-mode 0)
  (delq 'after-change-major-mode-hook highlight-symbol-mode))

(provide 'mail-trivialfis)
;;; mail-trivialfis.el ends here
