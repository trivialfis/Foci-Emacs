;;; pixienv.el --- Pixi environment support  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'f)

(defconst trivialfis/pixi-config-file ".conda-env.json"
  "Project-local environment configuration file.")

(defconst trivialfis/pixi-manager-field "environment-manager"
  "JSON field that selects the environment manager.")

(defconst trivialfis/pixi-workspace-file "pixi.toml"
  "File that marks the pixi workspace root.")

(defun trivialfis/pixi--workspace-root (directory config-dir)
  "Return pixi workspace root for DIRECTORY, falling back to CONFIG-DIR."
  (when-let ((root (or (locate-dominating-file
                        (or directory default-directory)
                        trivialfis/pixi-workspace-file)
                       (locate-dominating-file
                        (or directory default-directory)
                        ".pixi")
                       config-dir)))
    (file-name-as-directory (expand-file-name root))))

(defun trivialfis/pixi--env-config (&optional directory)
  "Read pixi environment config from DIRECTORY or `default-directory'.

Return a cons cell (WORKSPACE-ROOT . CONFIG) when `.conda-env.json'
exists and selects pixi via `environment-manager'.  Return nil
otherwise."
  (let* ((config-dir (locate-dominating-file
                      (or directory default-directory)
                      trivialfis/pixi-config-file))
         (config-file (when config-dir
                        (expand-file-name trivialfis/pixi-config-file config-dir)))
         (config (when config-file
                   (json-parse-string (f-read-text config-file)))))
    (when (and config
               (equal (gethash trivialfis/pixi-manager-field config)
                      "pixi"))
      (cons (trivialfis/pixi--workspace-root directory config-dir) config))))

(defun trivialfis/pixi--env-name (config)
  "Return pixi environment name from CONFIG."
  (let ((project-name (gethash "project-name" config)))
    (if (and (stringp project-name)
             (not (string-empty-p project-name)))
        project-name
      "default")))

(defun trivialfis/pixi-env-dir (&optional directory)
  "Return pixi environment directory for DIRECTORY, or nil.

The environment is resolved as `.pixi/envs/<project-name>' from the
project root.  `project-name' defaults to `default'."
  (when-let* ((root-and-config (trivialfis/pixi--env-config directory))
              (root (car root-and-config))
              (config (cdr root-and-config)))
    (expand-file-name
     (f-join ".pixi" "envs" (trivialfis/pixi--env-name config))
     root)))

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

(defun trivialfis/pixi-activate-path (env-dir)
  "Make pixi environment ENV-DIR visible to Emacs subprocesses."
  (let* ((env-dir (directory-file-name env-dir))
         (bin-dir (trivialfis/pixi--bin-dir env-dir))
         (path-separator (if (eq system-type 'windows-nt) ";" ":")))
    (setq-local exec-path (cons bin-dir (remove bin-dir exec-path)))
    (trivialfis/pixi--setenv-local
     "PATH" (trivialfis/pixi--prepend-path bin-dir path-separator))
    (trivialfis/pixi--setenv-local "CONDA_PREFIX" env-dir)
    (trivialfis/pixi--setenv-local "VIRTUAL_ENV" env-dir)
    (trivialfis/pixi--setenv-local "PIXI_ENVIRONMENT_NAME"
                                   (file-name-nondirectory env-dir))
    (when-let* ((root-and-config (trivialfis/pixi--env-config default-directory))
                (project-root (car root-and-config)))
      (trivialfis/pixi--setenv-local "PIXI_PROJECT_ROOT"
                                     (directory-file-name project-root)))))

(defun trivialfis/find-activate-pixi-env ()
  "Activate pixi env if `.conda-env.json' selects pixi.

Return the environment's Python executable on success, or nil."
  (when-let* ((python (trivialfis/pixi-python-command default-directory))
              (env-dir (trivialfis/pixi-env-dir default-directory)))
    (unless (file-remote-p env-dir)
      (trivialfis/pixi-activate-path env-dir))
    (message "Pixi project env: %s" env-dir)
    python))

(provide 'pixienv)
;;; pixienv.el ends here
