;;; rust-trivialfis.el --- Summary
;;; Commentary:
;;; Code:

(require 'programming-trivialfis)
(require 'rust-mode)
(require 'racer)
(require 'company)

(defun trivialfis/rust-compile ()
  "Compile rust code using cargo."
  (interactive)
  (let ((compile-buf
	 (get-buffer-create "*Compile log*")))
    (set-buffer compile-buf)
    (goto-char (point-max))
    (insert (concat
	     (shell-command-to-string "cargo build")
	     "\n\n"))
    (display-buffer compile-buf)))

(defun trivialfis/rust ()
  "Common configuration for rust mode."
  ;; (setq racer-rust-src-path "/home/fis/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
  ;; (setenv "CARGO_HOME" "~/.cargo")
  ;; (setenv "RUST_SRC_PATH" "/home/fis/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
  ;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'racer-mode-hook 'company-mode)
  ;; (flycheck-mode 1)
  (racer-mode 1)
  ;; (add-to-list 'company-backends 'company-racer)

  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (define-key rust-mode-map (kbd "C-c C-a") #'trivialfis/rust-compile)
  (setq company-tooltip-align-annotations t))

(provide 'rust-trivialfis)
;;; rust-trivialfis.el ends here
