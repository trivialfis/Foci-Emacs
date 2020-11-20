;;; nix-elpa.el --- Summary
;;; Commentary:
;;; Code:

(defvar nix-elpa-path nil)

;; Copied from nix site-start.el
(defun nix--profile-paths ()
  "Return a list of all paths in NIX_PROFILES.
The list is ordered from more-specific (the user profile) to the
least specific (the system profile)"
  (reverse (split-string (or (getenv "NIX_PROFILES") ""))))

(defun load-nix-elpa-packages ()
  "Add elpa subdirectories to load path."
  (if (not nix-elpa-path)
      (let* ((subdirectory-sites (lambda (site-lisp)
				   (when (file-exists-p site-lisp)
				     (seq-filter (lambda (f) (file-directory-p (file-truename f)))
						 ;; Returns all files in `site-lisp', excluding `.' and `..'
						 (directory-files site-lisp 'full "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))))
	     (paths (apply #'append
			   (mapcar (lambda (profile-dir)
				     (let ((site-lisp (concat profile-dir "/share/emacs/site-lisp/elpa")))
                                       (cons site-lisp (funcall subdirectory-sites site-lisp))))
				   (nix--profile-paths)))))
	(setq nix-elpa-path paths)
	(setq load-path (append nix-elpa-path load-path)))))

(provide 'nix-elpa)
;;; nix-elpa.el ends here
