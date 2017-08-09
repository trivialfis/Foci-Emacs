;;; scheme-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(defun trivialfis/scheme ()
  "Run Geiser."
  (save-window-excursion
    (run-geiser 'guile)))


(provide 'scheme-trivialfis)
;;; scheme-trivialfis.el ends here
