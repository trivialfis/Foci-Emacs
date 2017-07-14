;;; python-trivialfis --- Summary
;;; Commentary:
;;; Code:
(require 'elpy)
(require 'company)
(require 'programming-trivialfis)

(defun trivialfis/python()
  ;; (auto-make-header)
  ;; (fci-mode)
  (trivialfis/programming-init)
  (flycheck-mode 1)
  (elpy-mode)
  (with-eval-after-load 'elpy
    (setq elpy-rpc-python-command "python3")
    (setq python-shell-interpreter "python3")
    (elpy-use-ipython)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) ;; Replace flymake with flycheck
    ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
    (add-to-list 'company-backends 'elpy-company-backend)
    )
  (setq ansi-color-for-comint-mode t)
  ;; (aggressive-indent-mode)
  )
(provide 'python-trivialfis)
;;; python-trivialfis.el ends here
