;;; scheme-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'geiser-guile)
(defun trivialfis/scheme ()
  "Run Geiser."
  (save-window-excursion
    (run-geiser 'guile))
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "~/Workspace/guix")))


(provide 'scheme-trivialfis)
;;; scheme-trivialfis.el ends here
