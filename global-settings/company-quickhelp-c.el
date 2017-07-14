;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'company-quickhelp)
(defun company-quickhelp-setup()
  (setq company-quickhelp-color-background "yellow"
	company-quickhelp-color-foreground "black"
	company-quickhelp-delay 1
	company-quickhelp-max-lines nil)
  (company-quickhelp-mode 1)
  )
(provide 'company-quickhelp-c.el)
;;; company-quickhelp-c ends here
