;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'company-quickhelp)

(defun trivialfis/company-quickhelp()
  "Configuration for company quickhelp."
  (setq company-quickhelp-color-background "yellow"
	company-quickhelp-color-foreground "black"
	company-quickhelp-delay 1
	company-quickhelp-max-lines nil)
  (company-quickhelp-mode 1))

(provide 'quickhelp-trivialfis.el)
;;; quickhelp-trivialfis ends here
