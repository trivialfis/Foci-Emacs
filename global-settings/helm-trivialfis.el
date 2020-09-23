;;; helm-trivialfis --- Summary
;;; Commentary:
;;;
;;; Copyright © 2016-2018 Fis Trivial <ybbs.daans@hotmail.com>
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

(require 'helm)
(require 'helm-mode)
(require 'helm-files)
(require 'helm-config)
(require 'helm-net)
(require 'bind-key)

(defun trivialfis/helm ()
  "Helm mode configuration."
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (define-key helm-map (kbd "C-n") 'next-history-element)
  (define-key helm-map (kbd "C-p") 'previous-history-element)
  (define-key helm-map (kbd "M-n") 'helm-next-line)
  (define-key helm-map (kbd "M-p") 'helm-previous-line)

  (define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "M-p") 'helm-previous-line)
  (define-key helm-find-files-map (kbd "M-n") 'helm-next-line)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  (setq helm-split-window-inside-p            t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line        nil
	helm-autoresize-max-height            0
	helm-autoresize-min-height            32
	helm-buffers-fuzzy-matching           t
	helm-always-two-windows               t
	helm-default-display-buffer-functions '(display-buffer-in-side-window))

  (helm-autoresize-mode 1)
  (helm-mode 1))

(defun trivialfis/replace-completing-read (prompt collection
						  &optional
						  predicate require-match
						  initial-input hist def
						  inherit-input-method)
  "Helper function for lazy loading helm configuration.
Parameters here is the same as `completing-read-function'.
PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
	  INHERIT-INPUT-METHOD"
  (remove-function completing-read-function #'trivialfis/replace-completing-read)
  (helm--completing-read-default prompt collection
				 predicate require-match
				 initial-input hist def
				 inherit-input-method))

(defun trivialfis/replace-completion-region (start end collection
						   &optional predicate)
  "Helper function for lazy loading helm configuration.
START END COLLECTION &OPTIONAL PREDICATE"
  (remove-function completion-in-region-function #'trivialfis/replace-completion-region)
  (helm--completion-in-region start end collection predicate))

(defun trivialfis/replace-read-file
    (prompt &optional dir default-filename mustmatch initial predicate)
  "Helper function for lazy loading helm configuration.
PROMPT &OPTIONAL DIR DEFAULT-FILENAME MUSTMATCH INITIAL PREDICATE"
  (remove-function read-file-name-function #'trivialfis/replace-read-file)
  (helm--generic-read-file-name
   prompt dir default-filename mustmatch initial predicate))

(trivialfis/helm)

(provide 'helm-trivialfis)
;;; helm-trivialfis.el ends here
