;;; php-trivialfis --- Summary
;;; Commentary:
;;; Code:

(require 'programming-trivialfis)
(require 'company)
(require 'company-php)

(defun trivialfis/php ()
  "Configuration for php."
  (flycheck-mode 1)
  ;; (add-to-list 'company-backends 'company-ac-php-backend)
  (trivialfis/semantic 'php-mode))
(provide 'php-trivialfis)
;;; php-trivialfis.el ends here
