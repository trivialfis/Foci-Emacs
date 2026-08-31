;;; pixienv.el --- Pixi environment support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'projectenv)

(defconst trivialfis/pixi-default-manifest "pixi.toml"
  "Default Pixi manifest path, relative to the project file.")

(defconst trivialfis/pixi-default-environment "default"
  "Default Pixi environment name.")

(defun trivialfis/pixi--string-option (table key default)
  "Read string KEY from TOML TABLE, falling back to DEFAULT."
  (let ((entry (assoc-string key table)))
    (if (not entry)
        default
      (let ((value (cdr entry)))
        (unless (and (stringp value) (not (string-empty-p value)))
          (error "Project setting pixi.%s must be a non-empty string" key))
        value))))

(defun trivialfis/pixi--env-config (&optional directory)
  "Read pixi environment config from DIRECTORY or `default-directory'.

Return a plist containing the project file, manifest path, workspace
root, and environment name.  Return nil when the project has no
`pixi' table or its manifest does not exist.

Relative `manifest-path' values are resolved from the directory that
contains `foci.toml'."
  (when-let* ((project (trivialfis/project-config directory))
              (project-file (car project))
              (config (cdr project))
              (pixi (trivialfis/project-config-get config "pixi")))
    (unless (listp pixi)
      (error "Project setting pixi must be a TOML table"))
    (let* ((project-root (file-name-directory project-file))
           (manifest-setting
            (trivialfis/pixi--string-option
             pixi "manifest-path" trivialfis/pixi-default-manifest))
           (manifest-path (expand-file-name manifest-setting project-root))
           (workspace-root
            (if (file-directory-p manifest-path)
                (file-name-as-directory manifest-path)
              (file-name-directory manifest-path)))
           (environment
            (trivialfis/pixi--string-option
             pixi "environment" trivialfis/pixi-default-environment)))
      (when (file-exists-p manifest-path)
        (list :project-file project-file
              :config config
              :manifest-path manifest-path
              :workspace-root workspace-root
              :environment environment)))))

(defun trivialfis/pixi-env-dir (&optional directory)
  "Return pixi environment directory for DIRECTORY, or nil.

The environment is resolved as `.pixi/envs/<environment>' relative to
the configured manifest.  `environment' defaults to `default'."
  (when-let ((config (trivialfis/pixi--env-config directory)))
    (expand-file-name
     (concat ".pixi/envs/" (plist-get config :environment))
     (plist-get config :workspace-root))))

(defun trivialfis/pixi--bin-dir (env-dir)
  "Return the executable directory for ENV-DIR."
  (if (eq system-type 'windows-nt)
      env-dir
    (expand-file-name "bin" env-dir)))

(defun trivialfis/pixi-python-command (&optional directory)
  "Return the pixi Python executable for DIRECTORY, or nil."
  (when-let* ((env-dir (trivialfis/pixi-env-dir directory))
              (_ (file-directory-p env-dir)))
    (let* ((bin-dir (trivialfis/pixi--bin-dir env-dir))
           (candidates (if (eq system-type 'windows-nt)
                           (list (expand-file-name "python.exe" env-dir))
                         (list (expand-file-name "python" bin-dir)
                               (expand-file-name "python3" bin-dir)))))
      (seq-find #'file-executable-p candidates))))

(defun trivialfis/pixi-r-command (&optional directory)
  "Return the pixi R executable for DIRECTORY, or nil."
  (when-let* ((env-dir (trivialfis/pixi-env-dir directory))
              (_ (file-directory-p env-dir)))
    (let ((candidates
           (if (eq system-type 'windows-nt)
               (mapcar (lambda (path) (expand-file-name path env-dir))
                       '("R.exe" "Scripts/R.exe" "Library/bin/R.exe" "bin/R.exe"))
             (list (expand-file-name "bin/R" env-dir)))))
      (seq-find #'file-executable-p candidates))))

(defun trivialfis/pixi--setenv-local (name value)
  "Set environment variable NAME to VALUE in the current buffer."
  (setq-local process-environment
              (cons (format "%s=%s" name value)
                    (cl-remove-if
                     (lambda (entry)
                       (string-prefix-p (concat name "=") entry))
                     process-environment))))

(defun trivialfis/pixi--prepend-path (bin-dir separator)
  "Return PATH with BIN-DIR prepended once, using SEPARATOR."
  (string-join
   (cons bin-dir
         (remove bin-dir
                 (split-string (or (getenv "PATH") "") separator t)))
   separator))

(defun trivialfis/pixi-environment-variables (&optional directory)
  "Return Pixi environment variable overrides for DIRECTORY, or nil."
  (when-let* ((config (trivialfis/pixi--env-config directory))
              (env-dir (trivialfis/pixi-env-dir directory))
              (_ (file-directory-p env-dir)))
    (let* ((env-dir (directory-file-name env-dir))
           (bin-dir (trivialfis/pixi--bin-dir env-dir))
           (separator (if (eq system-type 'windows-nt) ";" ":"))
           (workspace-root
            (directory-file-name (plist-get config :workspace-root)))
           (manifest-path (plist-get config :manifest-path))
           (environment (plist-get config :environment)))
      (list (format "PATH=%s"
                    (trivialfis/pixi--prepend-path bin-dir separator))
            (format "CONDA_PREFIX=%s" env-dir)
            (format "VIRTUAL_ENV=%s" env-dir)
            (format "PIXI_ENVIRONMENT_NAME=%s" environment)
            (format "PIXI_PROJECT_ROOT=%s" workspace-root)
            (format "PIXI_PROJECT_MANIFEST=%s" manifest-path)))))

(defun trivialfis/pixi-activate (&optional directory)
  "Make the configured Pixi environment visible in the current buffer.

Return its environment directory, or nil."
  (when-let* ((env-dir (trivialfis/pixi-env-dir directory))
              (variables (trivialfis/pixi-environment-variables directory)))
    (unless (file-remote-p env-dir)
      (let ((bin-dir (trivialfis/pixi--bin-dir env-dir)))
        (setq-local exec-path (cons bin-dir (remove bin-dir exec-path)))
        (dolist (entry variables)
          (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" entry)
            (trivialfis/pixi--setenv-local
             (match-string 1 entry)
             (match-string 2 entry))))))
    env-dir))

(defun trivialfis/find-activate-pixi-python ()
  "Activate the project Pixi environment for Python.

Return the environment's Python executable on success, or nil."
  (when-let* ((python (trivialfis/pixi-python-command default-directory))
              (env-dir (trivialfis/pixi-activate default-directory)))
    (message "Pixi project env: %s" env-dir)
    python))

(defun trivialfis/find-activate-pixi-r ()
  "Activate the project Pixi environment for R.

Return the environment's R executable on success, or nil."
  (when-let* ((r-command (trivialfis/pixi-r-command default-directory))
              (env-dir (trivialfis/pixi-activate default-directory)))
    (message "Pixi project env: %s" env-dir)
    r-command))

(provide 'pixienv)
;;; pixienv.el ends here
