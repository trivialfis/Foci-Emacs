;;; markdown-trivialfis.el --- Summary
;;; Commentary:
;;; Code:
(require 'markdown-mode)
(defun trivialfis/markdown ()
  "Configuration for markdown mode."
  (interactive)
  (markdown-toggle-fontify-code-blocks-natively))

(provide 'markdown-trivialfis)
;;; markdown-trivialfis.el ends here
