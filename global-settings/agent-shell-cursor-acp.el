;;; agent-shell-cursor-acp.el --- Custom Cursor ACP backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Alvaro Ramirez
;;
;; Based on agent-shell by Alvaro Ramirez (https://github.com/xenodium/agent-shell)
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)
(require 'map)
(require 'seq)

(use-package agent-shell
  :defer t
  :autoload
  agent-shell-cwd
  agent-shell-make-agent-config
  agent-shell--make-acp-client
  agent-shell--dwim
  agent-shell--indent-string
  agent-shell--update-fragment
  agent-shell-interrupt
  agent-shell-get-config)

(use-package condaenv
  :defer t
  :autoload trivialfis/conda-env-name-to-dir)

(use-package f
  :defer t
  :autoload f-read-text)

;; ---------------------------------------------------------------------------
;;; Customization
;; ---------------------------------------------------------------------------

(defcustom agent-shell-cursor-acp-cli-command
  '("agent" "acp")
  "Command and parameters for the official Cursor ACP agent.
The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-cursor-acp-environment
  nil
  "Environment variables for the Cursor ACP agent."
  :type '(repeat string)
  :group 'agent-shell)

(defvar agent-shell-cursor-acp-whitelisted-commands
  '("pytest" "ninja" "python" "mypy"
    "git show" "git log" "git diff" "git status" "git stash" "git branch --list"
    "gh run view" "gh issue view" "gh pr view"
    "cd" "wc" "head" "tail" "grep" "sed" "ruff" "cat" "ls" "echo" "touch"
    "xargs" "sort" "find" "rg" "which" "sleep"
    "mvn"
    "curl"
    "nvidia-smi"))

;; ---------------------------------------------------------------------------
;;; Agent configuration
;; ---------------------------------------------------------------------------

(defun agent-shell-cursor-acp-make-config ()
  "Create a Cursor ACP agent configuration."
  (agent-shell-make-agent-config
   :identifier 'cursor-acp
   :mode-line-name "Cursor ACP"
   :buffer-name "Cursor ACP"
   :shell-prompt "Cursor ACP> "
   :shell-prompt-regexp "Cursor ACP> "
   :icon-name "cursor.png"
   :welcome-function #'agent-shell-cursor-acp--welcome-message
   :needs-authentication t
   :authenticate-request-maker
   (lambda ()
     (acp-make-authenticate-request :method-id "cursor_login"))
   ;; :default-model-id
   ;; (lambda () "claude-4.6-opus-max-thinking[thinking=true,context=200k,effort=high,fast=false]")
   :client-maker
   (lambda (buffer)
     (agent-shell-cursor-acp-make-client :buffer buffer))
   :install-instructions
   "Install Cursor CLI."))

(defun agent-shell-cursor-acp--conda-env-vars ()
  "Return environment variable overrides for the conda env, or nil.
Searches upward from `default-directory' for a `.conda-env.json' file.
Also loads project-specific ACP command whitelist entries from the
`acp-whitelist' field in that file."
  (when-let* ((path (locate-dominating-file "." ".conda-env.json"))
              (project-file (expand-file-name ".conda-env.json" path))
              (json-str (f-read-text project-file))
              (config (json-parse-string json-str)))
    (when-let* ((acp-whitelist (gethash "acp-whitelist" config))
                (entries (append acp-whitelist nil)))
      (dolist (entry entries)
        (unless (member entry agent-shell-cursor-acp-whitelisted-commands)
          (push entry agent-shell-cursor-acp-whitelisted-commands))))
    (when-let* ((project-name (gethash "project-name" config))
                (dirpath (trivialfis/conda-env-name-to-dir project-name))
                (bin-dir (expand-file-name "bin" dirpath)))
      (message "Cursor ACP: using conda env %s" project-name)
      (list (format "PATH=%s:%s" bin-dir (getenv "PATH"))
            (format "CONDA_PREFIX=%s" (directory-file-name dirpath))
            (format "CONDA_DEFAULT_ENV=%s" project-name)))))

(cl-defun agent-shell-cursor-acp-make-client (&key buffer)
  "Create a Cursor ACP client with BUFFER as context.
When a `.conda-env.json' file is found in the project, the agent
process is started with the conda environment activated."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (let ((conda-vars (with-current-buffer buffer
                      (agent-shell-cursor-acp--conda-env-vars))))
    (agent-shell--make-acp-client
     :command (car agent-shell-cursor-acp-cli-command)
     :command-params (cdr agent-shell-cursor-acp-cli-command)
     :environment-variables (append conda-vars agent-shell-cursor-acp-environment)
     :context-buffer buffer)))

;; ---------------------------------------------------------------------------
;;; Permission responder
;; ---------------------------------------------------------------------------

(defun agent-shell-cursor-acp--find-option (kind options)
  "Find the first option with KIND in OPTIONS."
  (seq-find (lambda (o) (equal (map-elt o :kind) kind)) options))

(defun agent-shell-cursor-acp--approve (respond options)
  "Auto-approve via RESPOND, preferring allow_always from OPTIONS.
Returns non-nil if a suitable option was found and the response was sent."
  (when-let ((choice (or (agent-shell-cursor-acp--find-option "allow_always" options)
                         (agent-shell-cursor-acp--find-option "allow_once" options))))
    (funcall respond (map-elt choice :option-id))
    t))

(defun agent-shell-cursor-acp--strip-quoted-strings (cmd)
  "Replace quoted string contents in CMD to prevent false operator splits.
Handles double-quoted strings (with backslash escapes) and
single-quoted strings."
  (replace-regexp-in-string
   "\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"\\|'[^']*'"
   "\"\"" cmd))

(defun agent-shell-cursor-acp--strip-redirections (cmd)
  "Strip shell I/O redirections from CMD.
Handles 2>&1, N>/path, &>/path, &>>/path, and similar patterns."
  (replace-regexp-in-string
   "[0-9]*>&?[0-9]+\\|&>>[^ \t\n]*\\|&>[^ \t\n]*\\|[0-9]*>>[^ \t\n]*\\|[0-9]*>[^ \t\n]*"
   "" cmd))

(defun agent-shell-cursor-acp--strip-timeout (cmd)
  "Strip a leading `timeout N' wrapper from CMD string."
  (if (string-match "\\`timeout[ \t]+[0-9]+[ \t]+" cmd)
      (substring cmd (match-end 0))
    cmd))

(defun agent-shell-cursor-acp--whitelisted-entry-p (cmd)
  "Return non-nil if CMD matches a whitelisted command."
  (let* ((trimmed (string-trim cmd))
         (res (seq-some
               (lambda (entry)
                 (or (string= trimmed entry)
                     (string-prefix-p (concat entry " ") trimmed)))
               agent-shell-cursor-acp-whitelisted-commands)))
    res))

(defun agent-shell-cursor-acp--whitelisted-command-p (title)
  "Return non-nil if TITLE contain only whitelisted commands.
Handles compound commands (&&, ||, ;, |), `timeout N' prefixes, and
shell redirections."
  (when (and title (not (string-empty-p title)))
    (let* ((bare (string-trim title "`" "`"))
           (no-strings (agent-shell-cursor-acp--strip-quoted-strings bare))
           (cleaned (agent-shell-cursor-acp--strip-redirections no-strings))
           (parts (split-string cleaned "[;&|]+" t "[ \t\n]+"))
           (commands (mapcar (lambda (part)
                               (agent-shell-cursor-acp--strip-timeout
                                (string-trim part)))
                             parts)))
      (message "title %s" title)
      (and commands
           (seq-every-p #'agent-shell-cursor-acp--whitelisted-entry-p
                        commands)))))

(defun agent-shell-cursor-acp--permission-responder (permission)
  "Custom permission policy for Cursor ACP sessions.
Auto-approves safe operations and falls through to the interactive
prompt for everything else.
PERMISSION is an alist with :tool-call, :options, and :respond."
  (let* ((tool-call (map-elt permission :tool-call))
         (options   (map-elt permission :options))
         (respond   (map-elt permission :respond))
         (title     (or (map-elt tool-call :title) ""))
         (kind      (or (map-elt tool-call :kind) "")))
    (cond
     ((or (equal kind "search")
          (let ((case-fold-search t))
            (string-match-p
             "\\bweb[_-]?search\\b\\|\\bweb[_-]?fetch\\b\\|\\bsearch\\b"
             title)))
      (agent-shell-cursor-acp--approve respond options))

     ((agent-shell-cursor-acp--whitelisted-command-p title)
      (let ((response (agent-shell-cursor-acp--approve respond options)))
        (message "response %s" response)
        response))

     (t
      (progn
        (message "not whitelisted: %s" (agent-shell-cursor-acp--whitelisted-command-p title))
        nil)))))

(defun agent-shell-cursor-acp--setup-permissions ()
  "Set up per-buffer permission policy for Cursor ACP sessions."
  (when (eq (map-elt (agent-shell-get-config (current-buffer)) :identifier)
            'cursor-acp)
    (setq-local agent-shell-permission-responder-function
                #'agent-shell-cursor-acp--permission-responder)))

;; ---------------------------------------------------------------------------
;;; Welcome
;; ---------------------------------------------------------------------------

(defun agent-shell-cursor-acp--welcome-message (config)
  "Return Cursor ACP welcome message using CONFIG."
  (let ((art (agent-shell--indent-string
              4 (agent-shell-cursor-acp--ascii-art)))
        (message (string-trim-left
                  (shell-maker-welcome-message config) "\n")))
    (concat "\n\n" art "\n\n" message)))

(defun my/agent-shell-dot-subdir (subdir)
  "Find a global dir for agent shell SUBDIR."
  (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
         (sanitized (replace-regexp-in-string "/" "-" (string-remove-prefix "/" cwd))))
    (expand-file-name subdir (locate-user-emacs-file (concat "agent-shell/" sanitized)))))


(defun agent-shell-cursor-acp--ascii-art ()
  "Cursor ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
  ██████╗ ██╗   ██╗ ██████╗  ███████╗  ██████╗  ██████╗
 ██╔════╝ ██║   ██║ ██╔══██╗ ██╔════╝ ██╔═══██╗ ██╔══██╗
 ██║      ██║   ██║ ██████╔╝ ███████╗ ██║   ██║ ██████╔╝
 ██║      ██║   ██║ ██╔══██╗ ╚════██║ ██║   ██║ ██╔══██╗
 ╚██████╗ ╚██████╔╝ ██║  ██║ ███████║ ╚██████╔╝ ██║  ██║
  ╚═════╝  ╚═════╝  ╚═╝  ╚═╝ ╚══════╝  ╚═════╝  ╚═╝  ╚═╝
" "\n")))
    (propertize text 'font-lock-face
                (if is-dark
                    '(:foreground "#00d4ff" :inherit fixed-pitch)
                  '(:foreground "#0066cc" :inherit fixed-pitch)))))

;; ---------------------------------------------------------------------------
;;; Cursor extension handlers
;; ---------------------------------------------------------------------------

(defun agent-shell-cursor-acp--is-cursor-acp-p (state)
  "Return non-nil if STATE belongs to a cursor-acp session."
  (eq (map-elt (map-elt state :agent-config) :identifier) 'cursor-acp))

(defun agent-shell-cursor-acp--send-ack (state acp-request)
  "Send an empty acknowledgement response for ACP-REQUEST using STATE.
Only sends if the message has an id (i.e. it is a request, not a notification)."
  (when (map-elt acp-request 'id)
    (acp-send-response
     :client (map-elt state :client)
     :response `((:request-id . ,(map-elt acp-request 'id))
                 (:result . nil)))))

(defun agent-shell-cursor-acp--handle-ask-question (state acp-request)
  "Handle cursor/ask_question by prompting with `completing-read'.
STATE is the agent-shell session state.  ACP-REQUEST is the incoming request."
  (let* ((params (map-elt acp-request 'params))
         (question (or (map-nested-elt params '(question))
                       (map-nested-elt params '(text))
                       "Select an option"))
         (raw-options (or (map-nested-elt params '(options)) []))
         (options (append raw-options nil))
         (labels (mapcar
                  (lambda (opt)
                    (or (map-elt opt 'label)
                        (map-elt opt 'text)
                        (map-elt opt 'id)
                        (format "%s" opt)))
                  options))
         (selected-label (completing-read (concat question " ") labels nil t))
         (selected (seq-find
                    (lambda (opt)
                      (equal (or (map-elt opt 'label)
                                 (map-elt opt 'text)
                                 (map-elt opt 'id)
                                 (format "%s" opt))
                             selected-label))
                    options))
         (answer-id (when selected
                      (or (map-elt selected 'id)
                          (map-elt selected 'optionId)
                          selected-label))))
    (agent-shell--update-fragment
     :state state
     :block-id (format "ask-question-%s" (map-elt acp-request 'id))
     :label-left (propertize "Question"
                             'font-lock-face 'font-lock-keyword-face)
     :body (format "%s → %s" question selected-label)
     :create-new t
     :navigation 'never)
    (acp-send-response
     :client (map-elt state :client)
     :response `((:request-id . ,(map-elt acp-request 'id))
                 (:result . ((answer . ,answer-id)))))))

(defun agent-shell-cursor-acp--handle-create-plan (state acp-request)
  "Handle cursor/create_plan by displaying plan and asking for approval.
STATE is the agent-shell session state.  ACP-REQUEST is the incoming request."
  (let* ((params (map-elt acp-request 'params))
         (plan-text (or (map-nested-elt params '(plan))
                        (map-nested-elt params '(text))
                        (map-nested-elt params '(content))
                        (format "%s" params)))
         (plan-display (if (stringp plan-text) plan-text
                         (format "%s" plan-text))))
    (agent-shell--update-fragment
     :state state
     :block-id (format "plan-%s" (map-elt acp-request 'id))
     :label-left (propertize "Proposed Plan"
                             'font-lock-face 'font-lock-doc-markup-face)
     :body plan-display
     :expanded t
     :create-new t)
    (let ((approved (y-or-n-p "Approve this plan? ")))
      (acp-send-response
       :client (map-elt state :client)
       :response `((:request-id . ,(map-elt acp-request 'id))
                   (:result . ((approved . ,(if approved t :json-false)))))))))

(defun agent-shell-cursor-acp--todo-status-icon (status)
  "Return a status icon string for todo STATUS."
  (pcase status
    ("completed"   "✓")
    ("in_progress" "→")
    ("cancelled"   "✗")
    (_             "○")))

(defun agent-shell-cursor-acp--handle-update-todos (state acp-request)
  "Handle cursor/update_todos by displaying todo state.
STATE is the agent-shell session state.  ACP-REQUEST is the incoming message."
  (let* ((params (map-elt acp-request 'params))
         (todos (append (or (map-nested-elt params '(todos)) []) nil))
         (lines (mapcar
                 (lambda (todo)
                   (format "%s %s"
                           (agent-shell-cursor-acp--todo-status-icon
                            (or (map-elt todo 'status) "unknown"))
                           (or (map-elt todo 'content)
                               (map-elt todo 'text)
                               (map-elt todo 'title)
                               "")))
                 todos)))
    (when lines
      (agent-shell--update-fragment
       :state state
       :block-id "cursor-todos"
       :label-left (propertize "Todos"
                               'font-lock-face 'font-lock-type-face)
       :body (string-join lines "\n")
       :expanded t))
    (agent-shell-cursor-acp--send-ack state acp-request)))

(defun agent-shell-cursor-acp--handle-task (state acp-request)
  "Handle cursor/task by displaying task completion.
STATE is the agent-shell session state.  ACP-REQUEST is the incoming message."
  (let* ((params (map-elt acp-request 'params))
         (description (or (map-nested-elt params '(description))
                          (map-nested-elt params '(title))
                          "Subagent task"))
         (status (or (map-nested-elt params '(status)) "completed"))
         (result (map-nested-elt params '(result))))
    (agent-shell--update-fragment
     :state state
     :block-id (format "task-%s" (or (map-elt acp-request 'id)
                                     (random 10000)))
     :label-left (propertize (format "Task %s" status)
                             'font-lock-face 'font-lock-function-name-face)
     :body (if result (format "%s\n%s" description result) description)
     :create-new t
     :navigation 'never)
    (agent-shell-cursor-acp--send-ack state acp-request)))

(defun agent-shell-cursor-acp--handle-generate-image (state acp-request)
  "Handle cursor/generate_image by displaying the generated image.
STATE is the agent-shell session state.  ACP-REQUEST is the incoming message."
  (let* ((params (map-elt acp-request 'params))
         (image-path (or (map-nested-elt params '(path))
                         (map-nested-elt params '(filePath))))
         (format-type (or (map-nested-elt params '(format))
                          (map-nested-elt params '(mimeType))
                          "png"))
         (block-id (format "image-%s" (or (map-elt acp-request 'id)
                                          (random 10000)))))
    (agent-shell--update-fragment
     :state state
     :block-id block-id
     :label-left (propertize "Generated Image"
                             'font-lock-face 'font-lock-string-face)
     :body (if image-path
               (format "[Image saved to %s]" image-path)
             (format "[Image generated (%s)]" format-type))
     :render-body-images t
     :create-new t
     :navigation 'never)
    (agent-shell-cursor-acp--send-ack state acp-request)))

;; ---------------------------------------------------------------------------
;;; Dispatch: intercept cursor/* methods via advice
;; ---------------------------------------------------------------------------

(defun agent-shell-cursor-acp--dispatch-extension (state msg)
  "Dispatch a cursor/* extension method in MSG using STATE.
Return non-nil if the method was handled."
  (let ((method (map-elt msg 'method)))
    (when (and method (string-prefix-p "cursor/" method))
      (pcase method
        ("cursor/ask_question"   (agent-shell-cursor-acp--handle-ask-question state msg) t)
        ("cursor/create_plan"    (agent-shell-cursor-acp--handle-create-plan state msg) t)
        ("cursor/update_todos"   (agent-shell-cursor-acp--handle-update-todos state msg) t)
        ("cursor/task"           (agent-shell-cursor-acp--handle-task state msg) t)
        ("cursor/generate_image" (agent-shell-cursor-acp--handle-generate-image state msg) t)))))

(defun agent-shell-cursor-acp--on-request-advice (orig-fn &rest args)
  "Advice around `agent-shell--on-request' for cursor/* extensions.
ORIG-FN and ARGS are the advised function and its arguments."
  (let ((state (plist-get args :state))
        (acp-request (plist-get args :acp-request)))
    (unless (and (agent-shell-cursor-acp--is-cursor-acp-p state)
                 (agent-shell-cursor-acp--dispatch-extension state acp-request))
      (apply orig-fn args))))

(defun agent-shell-cursor-acp--on-notification-advice (orig-fn &rest args)
  "Advice around `agent-shell--on-notification' for cursor/* extensions.
ORIG-FN and ARGS are the advised function and its arguments."
  (let ((state (plist-get args :state))
        (acp-notification (plist-get args :acp-notification)))
    (unless (and (agent-shell-cursor-acp--is-cursor-acp-p state)
                 (agent-shell-cursor-acp--dispatch-extension state acp-notification))
      (apply orig-fn args))))

;; ---------------------------------------------------------------------------
;;; Integration with agent-shell
;; ---------------------------------------------------------------------------

(with-eval-after-load 'agent-shell
  (setq agent-shell-session-strategy 'new
        agent-shell-show-session-id t
        agent-shell-thought-process-expand-by-default t
        agent-shell-preferred-agent-config 'cursor-acp)

  (setq agent-shell-agent-configs
        (cons (agent-shell-cursor-acp-make-config)
              (seq-remove
               (lambda (config)
                 (eq (map-elt config :identifier) 'cursor-acp))
               agent-shell-agent-configs)))
  (setopt agent-shell-dot-subdir-function #'my/agent-shell-dot-subdir)

  (add-hook 'agent-shell-mode-hook
            #'agent-shell-cursor-acp--setup-permissions)

  (advice-add 'agent-shell--on-request :around
              #'agent-shell-cursor-acp--on-request-advice)
  (advice-add 'agent-shell--on-notification :around
              #'agent-shell-cursor-acp--on-notification-advice))

(provide 'agent-shell-cursor-acp)

;;; agent-shell-cursor-acp.el ends here
