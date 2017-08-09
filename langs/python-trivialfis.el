;;; python-trivialfis --- Summary
;;; Commentary:
;;; Code:
;; (require 'elpy)
(require 'company)
(require 'programming-trivialfis)
(require 'elpy)
;; (require 'xterm-color)

(defun trivialfis/python()
  ;; (auto-make-header)

  (trivialfis/programming-init)
  (flycheck-mode 1)
  (elpy-mode)
  
  (with-eval-after-load 'elpy
    (setq elpy-rpc-python-command "python3"
	  python-shell-interpreter "python3")
    ;; (elpy-use-ipython)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) ;; Replace flymake with flycheck
    ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
    (add-to-list 'company-backends 'elpy-company-backend)))
(provide 'python-trivialfis)
;;; python-trivialfis.el ends here
