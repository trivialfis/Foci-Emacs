;;; gnus-trivialfis.el --- Summary -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Copyright © 2015-2019 Fis Trivial <ybbs.daans@hotmail.com>
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

(defun trivialfis/smtp4gnus ()
  "Configuration for sending mails."
  (setq	message-send-mail-function 'async-smtpmail-send-it
	send-mail-function 'async-smtpmail-send-it
	smtpmail-smtp-server "smtp-mail.outlook.com"
	smtpmail-default-smtp-server "smtp-mail.outlook.com"
	smtpmail-smtp-service 587
	smtpmail-stream-type 'starttls
	smtpmail-debug-info 't))

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


(provide 'gnus-trivialfis)
;;; gnus-trivialfis.el ends here
