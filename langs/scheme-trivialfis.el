;;; scheme-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'geiser)
(require 'geiser-repl)
(require 'geiser-guile)
(defun trivialfis/scheme ()
  "Run Geiser."
  (with-eval-after-load 'geiser
    (setq geiser-active-implementations '(guile)))
  (save-window-excursion
    (run-geiser 'guile))
  ;; (with-eval-after-load 'geiser-repl
  (setq geiser-repl-query-on-kill-p nil)
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "~/Workspace/guix"))
  )


(provide 'scheme-trivialfis)
;;; scheme-trivialfis.el ends here
