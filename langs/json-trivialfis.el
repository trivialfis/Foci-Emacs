;;; json-model.el --- Summary
;;; Commentary:
;;; Code:

(require 'json-mode)

(defun trivialfis/json ()
  "JSON configuration."
  (setq-default js-indent-level 2)
  (indent-tabs-mode nil))

(provide 'json-model)
;;; json-model.el ends here
