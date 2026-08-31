;;; test-pixienv.el --- Tests for Pixi project environments -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Batch:
;;   emacs --batch -L global-settings \
;;         -l global-settings/test-pixienv.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(add-to-list 'load-path (file-name-directory (or load-file-name
                                                  buffer-file-name)))
(require 'pixienv)

(defun pixi-test--write-file (file contents)
  "Write CONTENTS to FILE."
  (with-temp-file file
    (insert contents)))

(ert-deftest pixi-project-environment-variables ()
  (let* ((tmp (make-temp-file "pixi-project" t))
         (workspace (expand-file-name "workspace" tmp))
         (manifest (expand-file-name "pixi.toml" workspace))
         (env-dir (expand-file-name ".pixi/envs/dev" workspace))
         (default-directory tmp)
         (process-environment '("PATH=/usr/bin")))
    (unwind-protect
        (progn
          (make-directory workspace t)
          (make-directory (expand-file-name "bin" env-dir) t)
          (pixi-test--write-file
           (expand-file-name "foci.toml" tmp)
           (concat "[pixi]\n"
                   "manifest-path = \"workspace/pixi.toml\"\n"
                   "environment = \"dev\"\n"))
          (pixi-test--write-file manifest "")
          (should
           (equal
            (trivialfis/pixi-environment-variables)
            (list
             (format "PATH=%s:/usr/bin" (expand-file-name "bin" env-dir))
             (format "CONDA_PREFIX=%s" env-dir)
             (format "VIRTUAL_ENV=%s" env-dir)
             "PIXI_ENVIRONMENT_NAME=dev"
             (format "PIXI_PROJECT_ROOT=%s" workspace)
             (format "PIXI_PROJECT_MANIFEST=%s" manifest)))))
      (delete-directory tmp t))))

(ert-deftest pixi-project-language-commands ()
  (let* ((tmp (make-temp-file "pixi-languages" t))
         (bin-dir (expand-file-name ".pixi/envs/default/bin" tmp))
         (python (expand-file-name "python" bin-dir))
         (r-command (expand-file-name "R" bin-dir))
         (default-directory tmp)
         (process-environment '("PATH=/usr/bin"))
         (exec-path '("/usr/bin")))
    (unwind-protect
        (progn
          (make-directory bin-dir t)
          (pixi-test--write-file
           (expand-file-name "foci.toml" tmp)
           "[pixi]\nenvironment = \"default\"\n")
          (pixi-test--write-file (expand-file-name "pixi.toml" tmp) "")
          (dolist (command (list python r-command))
            (pixi-test--write-file command "#!/bin/sh\n")
            (set-file-modes command #o755))
          (should (equal (trivialfis/find-activate-pixi-python) python))
          (should (equal (trivialfis/find-activate-pixi-r) r-command))
          (should (equal (getenv "CONDA_PREFIX")
                         (expand-file-name ".pixi/envs/default" tmp)))
          (should (member bin-dir exec-path)))
      (delete-directory tmp t))))

(provide 'test-pixienv)
;;; test-pixienv.el ends here
