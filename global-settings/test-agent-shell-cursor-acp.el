;;; test-agent-shell-cursor-acp.el --- Tests for command approval -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT tests for the whitelisted-command approval stack.
;;
;; Interactive:
;;   M-x load-file RET global-settings/test-agent-shell-cursor-acp.el RET
;;   M-x ert RET t RET
;;
;; Batch:
;;   emacs --batch -l ert \
;;         -l global-settings/agent-shell-cursor-acp.el \
;;         -l global-settings/test-agent-shell-cursor-acp.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'seq)
(require 'map)

(add-to-list 'load-path (file-name-directory (or load-file-name
                                                  buffer-file-name)))
(require 'agent-shell-cursor-acp)

;; ---------------------------------------------------------------------------
;;; Stripping helpers
;; ---------------------------------------------------------------------------

(ert-deftest acp-strip-redirections ()
  (let ((s #'agent-shell-cursor-acp--strip-redirections))
    (should (equal (string-trim (funcall s "cmd 2>&1"))        "cmd"))
    (should (equal (string-trim (funcall s "cmd >/dev/null"))  "cmd"))
    (should (equal (string-trim (funcall s "cmd 2>/dev/null")) "cmd"))
    (should (equal (string-trim (funcall s "cmd &>/dev/null")) "cmd"))
    (should (equal (string-trim (funcall s "cmd >>/tmp/log"))  "cmd"))
    (should (equal (funcall s "cmd --flag arg") "cmd --flag arg"))
    (should (equal (string-trim (funcall s "ninja -C build/ 2>&1 | tail -5"))
                   "ninja -C build/  | tail -5"))))

(ert-deftest acp-strip-quoted-strings ()
  (let ((s #'agent-shell-cursor-acp--strip-quoted-strings))
    (should (equal (funcall s "grep -E \"Invalid|Error|Failed\" log.txt")
                   "grep -E \"\" log.txt"))
    (should (equal (funcall s "echo \"hello world\"")    "echo \"\""))
    (should (equal (funcall s "grep -E 'foo|bar' f.txt") "grep -E \"\" f.txt"))
    (should (equal (funcall s "echo 'a;b&&c|d'")         "echo \"\""))
    (should (equal (funcall s "echo \"say \\\"hi\\\"\"")  "echo \"\""))
    (should (equal (funcall s "cmd \"a|b\" && cmd2 'c;d'")
                   "cmd \"\" && cmd2 \"\""))
    (should (equal (funcall s "cmd --flag") "cmd --flag"))))

(ert-deftest acp-strip-timeout ()
  (let ((s #'agent-shell-cursor-acp--strip-timeout))
    (should (equal (funcall s "timeout 120 ninja -j4") "ninja -j4"))
    (should (equal (funcall s "timeout 5 cmd")         "cmd"))
    (should (equal (funcall s "ninja -j4")             "ninja -j4"))
    (should (equal (funcall s "timeout cmd")           "timeout cmd"))))

;; ---------------------------------------------------------------------------
;;; whitelisted-entry-p
;; ---------------------------------------------------------------------------

(ert-deftest acp-whitelisted-entry ()
  (let ((ok #'agent-shell-cursor-acp--whitelisted-entry-p))
    (should     (funcall ok "cd"))
    (should     (funcall ok "cd /home/user/project"))
    (should     (funcall ok "wc -l file.txt"))
    (should     (funcall ok "ninja -C build/"))
    (should     (funcall ok "mypy ."))
    (should     (funcall ok "git show HEAD"))
    (should-not (funcall ok "rm -rf /"))
    (should-not (funcall ok "git push origin"))))

;; ---------------------------------------------------------------------------
;;; whitelisted-command-p (integration)
;; ---------------------------------------------------------------------------

(ert-deftest acp-whitelisted-cmd/accept ()
  (let ((ok #'agent-shell-cursor-acp--whitelisted-command-p))
    ;; Simple commands.
    (should (funcall ok "ninja -j4"))
    (should (funcall ok "cd /some/path"))
    (should (funcall ok "git log --oneline -10"))
    ;; Timeout prefix.
    (should (funcall ok "timeout 10 cd /some/path"))
    (should (funcall ok "timeout 120 pytest -s -v tests/test_foo.py"))
    ;; Redirections.
    (should (funcall ok "ninja -C build/ >/tmp/out 2>/tmp/err"))
    (should (funcall ok "ninja -C build/ install 2>&1 | tail -5"))
    ;; Compound commands.
    (should (funcall ok "cd /tmp ; ninja -j4"))
    (should (funcall ok "cd /home/user/project && mypy ."))
    (should (funcall ok "ninja -j4 || tail -1 /tmp/log"))
    ;; Backtick-wrapped.
    (should (funcall ok "`ninja -j4`"))
    (should (funcall ok "`cd /tmp && ninja -j4`"))
    ;; Operators inside quoted strings must not cause false splits.
    (should (funcall ok "grep -E \"Invalid|Error|Failed\" log.txt"))
    (should (funcall ok "grep -E 'foo|bar' file.txt"))
    (should (funcall ok "echo \"a && b || c\""))
    (should (funcall ok "sed 's/old|new/replaced/' file.txt"))))

(ert-deftest acp-whitelisted-cmd/reject ()
  (let ((ok #'agent-shell-cursor-acp--whitelisted-command-p))
    (should-not (funcall ok nil))
    (should-not (funcall ok ""))
    (should-not (funcall ok "``"))
    (should-not (funcall ok "rm -rf /"))
    (should-not (funcall ok "`rm -rf /`"))
    (should-not (funcall ok "rm \"safe|name\" -rf /"))
    ;; One bad segment poisons the whole pipeline.
    (should-not (funcall ok "ninja -j4 | rm -rf /"))))

(provide 'test-agent-shell-cursor-acp)

;;; test-agent-shell-cursor-acp.el ends here
