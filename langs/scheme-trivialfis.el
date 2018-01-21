;;; scheme-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'geiser)
(require 'geiser-mode)
(require 'geiser-repl)
(require 'geiser-guile)
(require 'geiser-compile)

(defun trivialfis/scheme ()
  "Run Geiser."
  (setq geiser-active-implementations '(guile))
  (setq geiser-repl-query-on-kill-p nil)

  (define-key geiser-mode-map (kbd "C-c C-a") 'geiser-compile-current-buffer)
  (define-key geiser-mode-map (kbd "C-c C-k") 'geiser-mode-switch-to-repl-and-enter)
  (eval-after-load 'geiser
    (save-window-excursion
      (run-geiser 'guile))
    )

  (add-to-list 'geiser-guile-load-path "~/Workspace/guix"))

(provide 'scheme-trivialfis)
;;; scheme-trivialfis.el ends here
