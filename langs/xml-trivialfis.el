;;; xml-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

;; https://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun trivialfis/xml-format-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end)))
(provide 'xml-trivialfis)
;;; xml-trivialfis.el ends here
